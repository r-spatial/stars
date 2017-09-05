#include "Rcpp.h"
#include <gdal_priv.h>
#include <ogr_api.h>
#include <ogr_geometry.h>
#include <ogr_srs_api.h>

#include "stars.h"

using namespace Rcpp;

NumericMatrix CPL_read_gdal_data(Rcpp::List meta, GDALDataset *poDataset);

// [[Rcpp::export]]
List CPL_read_gdal(CharacterVector fname, CharacterVector options, CharacterVector driver,
		bool read_data = true) {
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

	// geotransform:
	double adfGeoTransform[6];
	if (poDataset->GetGeoTransform( adfGeoTransform ) != CE_None )
		stop("cannot get geotransform");
	NumericVector geotransform = NumericVector::create(
		adfGeoTransform[0],
		adfGeoTransform[1],
		adfGeoTransform[2],
		adfGeoTransform[3],
		adfGeoTransform[4],
		adfGeoTransform[5]);
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
	CharacterVector proj = CharacterVector::create(wkt);

	// proj4string:
	OGRSpatialReference *sr = new OGRSpatialReference;
	char **ppt = (char **) &wkt;
	sr->importFromWkt(ppt);
	char *proj4; 
	sr->exportToProj4(&proj4); // need to error check?
	CharacterVector p4 = CharacterVector::create(proj4); // need to CPLFree?
	delete sr;


	GDALRasterBand *poBand = poDataset->GetRasterBand( 1 );
	List ReturnList = List::create(
		_["filename"] = fname,
		_["driver"] = Driver,
		_["cols"] = NumericVector::create(1, poDataset->GetRasterXSize()),
		_["rows"] = NumericVector::create(1, poDataset->GetRasterYSize()),
		_["bands"] = NumericVector::create(1, poDataset->GetRasterCount()),
		_["proj_wkt"] = proj,
		_["proj4string"] = p4,
		_["geotransform"] = geotransform,
        _["datatype"] =	GDALGetDataTypeName(poBand->GetRasterDataType())
	);
	if (read_data)
		ReturnList.attr("data") = CPL_read_gdal_data(ReturnList, poDataset);

	GDALClose(poDataset);
	return ReturnList;
}

NumericMatrix CPL_read_gdal_data(Rcpp::List meta, GDALDataset *poDataset) {

	IntegerVector x = meta["cols"];
	IntegerVector y = meta["rows"];
	IntegerVector bands = meta["bands"];

	// GDALRasterBand *poBand = poDataset->GetRasterBand( 1 );
	NumericMatrix mat( diff(x)[0] + 1, (diff(y)[0] + 1) * (diff(bands)[0] + 1) );

	size_t i = 0;
	for (size_t band = bands(0); band <= bands(1); band++) { // unlike x & y, band is 1-based
		GDALRasterBand *poBand = poDataset->GetRasterBand( band );
		for (size_t row = y(0) - 1; row < y(1); row++) {
			std::vector<double> buf( diff(x)[0] + 1 );
			CPLErr err = poBand->RasterIO( GF_Read, x(0) - 1, row, x(1), 1,
                  	buf.data(), buf.size(), 1, GDT_Float64, 0, 0);
			if (err == CE_Failure)
				stop("read failure");
			mat(_, i++) = NumericVector(buf.begin(), buf.end());
		}
	}

	// dim:
	IntegerVector dims = IntegerVector::create(diff(x)[0] + 1, diff(y)[0] + 1,
		diff(bands)[0] + 1);
	dims.attr("names") = CharacterVector::create("x", "y", "band");
	mat.attr("dim") = dims;
	return mat;
}
