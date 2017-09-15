#include "Rcpp.h"
#include <gdal_priv.h>
#include <ogr_api.h>
#include <ogr_geometry.h>
#include <ogr_srs_api.h>

#include "stars.h"

using namespace Rcpp;

NumericMatrix CPL_read_gdal_data(Rcpp::List meta, GDALDataset *poDataset, NumericVector nodatavalue);

// [[Rcpp::export]]
List CPL_read_gdal(CharacterVector fname, CharacterVector options, CharacterVector driver,
		bool read_data = true) {
// reads data set metadata, and if read_data is true, adds data array
    GDALDataset  *poDataset;
	poDataset = (GDALDataset *) GDALOpenEx(fname[0], GA_ReadOnly,
		driver.size() ? create_options(driver).data() : NULL,
		options.size() ? create_options(options).data() : NULL,
		NULL);
    if( poDataset == NULL )
        stop("file not found");

	CharacterVector Driver = CharacterVector::create(
        poDataset->GetDriver()->GetDescription(),
        poDataset->GetDriver()->GetMetadataItem( GDAL_DMD_LONGNAME ));

	/*
	if (poDataset->GetRasterCount() == 0)
		stop("zero bands");
	*/

	// geotransform:
	double adfGeoTransform[6];
	CPLErr err = poDataset->GetGeoTransform( adfGeoTransform );
	NumericVector geotransform = NumericVector::create(
		err == CE_None ? adfGeoTransform[0] : NA_REAL,
		err == CE_None ? adfGeoTransform[1] : NA_REAL,
		err == CE_None ? adfGeoTransform[2] : NA_REAL,
		err == CE_None ? adfGeoTransform[3] : NA_REAL,
		err == CE_None ? adfGeoTransform[4] : NA_REAL,
		err == CE_None ? adfGeoTransform[5] : NA_REAL);
/*	 
	// from GDAL tutorial:

	int             nBlockXSize, nBlockYSize;
	int             bGotMin, bGotMax;
	double          adfMinMax[2];
	poBand->GetBlockSize( &nBlockXSize, &nBlockYSize );
	printf( "Block=%dx%d Type=%s, ColorInterp=%s\n",
       		nBlockXSize, nBlockYSize,
        	GDALGetDataTypeName(poBand->GetRasterDataType()),
        	GDALGetColorInterpretationName(
           	poBand->GetColorInterpretation()) ); 

	adfMinMax[0] = poBand->GetMinimum( &bGotMin );
	adfMinMax[1] = poBand->GetMaximum( &bGotMax );
	if( ! (bGotMin && bGotMax) )
    	GDALComputeRasterMinMax((GDALRasterBandH)poBand, TRUE, adfMinMax);
	printf( "Min=%.3fd, Max=%.3f\n", adfMinMax[0], adfMinMax[1] );
	if( poBand->GetOverviewCount() > 0 )
    	printf( "Band has %d overviews.\n", poBand->GetOverviewCount() );
	if( poBand->GetColorTable() != NULL )
    	printf( "Band has a color table with %d entries.\n",
             		poBand->GetColorTable()->GetColorEntryCount() );
*/
	// projection:
	const char *wkt = poDataset->GetProjectionRef();
	CharacterVector proj = NA_STRING;
	CharacterVector p4 = NA_STRING;
	if (*wkt != '\0') {
		proj = CharacterVector::create(wkt);
		// proj4string:
		OGRSpatialReference *sr = new OGRSpatialReference;
		char **ppt = (char **) &wkt;
		sr->importFromWkt(ppt);
		char *proj4; 
		sr->exportToProj4(&proj4); // need to error check?
		p4 = CharacterVector::create(proj4); // need to CPLFree?
		delete sr;
	}

	GDALRasterBand *poBand = NULL;
	NumericVector nodatavalue = NumericVector::create(NA_REAL);
	if (poDataset->GetRasterCount()) {
		poBand = poDataset->GetRasterBand( 1 );
		int set = 0;
		poBand->GetNoDataValue(&set);
		if (set)
			nodatavalue = poBand->GetNoDataValue(NULL);
	}

	CharacterVector items = get_meta_data((GDALDatasetH) poDataset, NA_STRING);
	CharacterVector sub = NA_STRING;
	for (size_t i; i < items.size(); i++) {
		// Rcpp::Rcout << items[i] << std::endl;
		if (items[i] == "SUBDATASETS")
			sub = get_meta_data(poDataset, "SUBDATASETS");
	}

	List ReturnList = List::create(
		_["filename"] = fname,
		_["driver"] = Driver,
		_["cols"] = NumericVector::create(1, poDataset->GetRasterXSize()),
		_["rows"] = NumericVector::create(1, poDataset->GetRasterYSize()),
		_["bands"] = NumericVector::create(1, poDataset->GetRasterCount()),
		_["proj_wkt"] = proj,
		_["proj4string"] = p4,
		_["geotransform"] = geotransform,
        _["datatype"] =	poBand != NULL ? 
			GDALGetDataTypeName(poBand->GetRasterDataType()) : CharacterVector::create(NA_STRING),
		_["sub"] = sub,
		_["meta"] = get_meta_data(poDataset, CharacterVector::create())
	);
	if (read_data)
		ReturnList.attr("data") = CPL_read_gdal_data(ReturnList, poDataset, nodatavalue);

	GDALClose(poDataset);
	return ReturnList;
}

NumericMatrix CPL_read_gdal_data(Rcpp::List meta, GDALDataset *poDataset, NumericVector nodatavalue) {

	IntegerVector x = meta["cols"];
	IntegerVector y = meta["rows"];
	IntegerVector bands = meta["bands"];

	size_t nx = diff(x)[0] + 1, ny = diff(y)[0] + 1, nbands = diff(bands)[0] + 1,
		resample = 1; // still need to find out what resample exactly does, 
		              // and how it interacts with/should change geotransform

	// GDALRasterBand *poBand = poDataset->GetRasterBand( 1 );
	NumericMatrix mat( (nx / resample) * (ny / resample), nbands );

	IntegerVector band_index(nbands);
	for (int j = 0, i = bands(0); i <= bands(1); i++)
		band_index(j++) = i;
	// read the full set of bands:
	CPLErr err = poDataset->RasterIO( GF_Read,
			x(0) - 1, y(0) - 1,
			nx, ny,
			mat.begin(),
			nx / resample, ny / resample,
			GDT_Float64, nbands, band_index.begin(), 0, 0, 0);
	if (err == CE_Failure)
		stop("read failure");

	for (size_t band = bands(0), i = 0; band <= bands(1); band++) { // unlike x & y, band is 1-based
		GDALRasterBand *poBand = poDataset->GetRasterBand( band );
		NumericVector nodatavalue = NumericVector::create(NA_REAL);
		int success = 0, has_scale = 0, has_offset = 0;
		double offset = 0.0, scale = 1.0;
		poBand->GetNoDataValue(&success);
		if (success)
			nodatavalue = poBand->GetNoDataValue(NULL);
		poBand->GetScale(&has_scale);
		if (has_scale)
			scale = poBand->GetScale(NULL);
		poBand->GetOffset(&has_offset);
		if (has_offset)
			offset = poBand->GetOffset(NULL);
		// char *units = poBand->GetUnits();
		if (! NumericVector::is_na(nodatavalue[0]) || has_offset || has_scale) {
			NumericVector nv = mat(_, band - 1);
			for (size_t i = 0; i < nv.size(); i++) {
				if (nv[i] == nodatavalue[0])
					nv[i] = NA_REAL;
				else
					nv[i] = (nv[i] * scale) + offset;
			}
			mat(_, band - 1) = nv;
		}
		checkUserInterrupt();
	}

	// dim:
	IntegerVector dims;
	if (diff(bands)[0] == 0) { // one band:
		dims = IntegerVector::create(nx / resample, ny / resample);
		dims.attr("names") = CharacterVector::create("x", "y");
	} else {
		dims = IntegerVector::create(nx / resample, ny / resample, diff(bands)[0] + 1);
		dims.attr("names") = CharacterVector::create("x", "y", "band");
	}
	mat.attr("dim") = dims;
	return mat;
}
