#include "Rcpp.h"
#include "gdal_priv.h"

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::NumericMatrix CPL_read_gdal(Rcpp::CharacterVector fname, bool verbose = true)
{
    GDALDataset  *poDataset;
    poDataset = (GDALDataset *) GDALOpen(fname[0], GA_ReadOnly );
    if( poDataset == NULL )
        Rcpp::stop("file not found");

	double adfGeoTransform[6];
	Rcpp::Rcout << "Driver: " << 
        poDataset->GetDriver()->GetDescription() << "/" <<
        poDataset->GetDriver()->GetMetadataItem( GDAL_DMD_LONGNAME ) << std::endl;

	if (poDataset->GetGeoTransform( adfGeoTransform ) != CE_None )
		Rcpp::stop("cannot get geotransform");

	if (verbose) {
		Rcpp::Rcout << "Size is " << 
        poDataset->GetRasterXSize() << " x " <<  poDataset->GetRasterYSize() << " x " <<
        poDataset->GetRasterCount() << std::endl;

		if (poDataset->GetProjectionRef() != NULL)
			Rcpp::Rcout << "Projection is " << poDataset->GetProjectionRef() << std::endl;

		Rcpp::Rcout << "Origin = (" << 
           	adfGeoTransform[0] << "," << adfGeoTransform[3] << ")" << std::endl;
		Rcpp::Rcout << "Pixel Size = (" << 
           	adfGeoTransform[1] << "," << adfGeoTransform[5] << ")" << std::endl;
	}
/*	 
	int             nBlockXSize, nBlockYSize;
	int             bGotMin, bGotMax;
	double          adfMinMax[2];
	poBand->GetBlockSize( &nBlockXSize, &nBlockYSize );
	printf( "Block=%dx%d Type=%s, ColorInterp=%s\n",
       		nBlockXSize, nBlockYSize,
        	GDALGetDataTypeName(poBand->GetRasterDataType()),
        	GDALGetColorInterpretationName(
           	poBand->GetColorInterpretation()) ); 
*/
/*
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
	GDALRasterBand *poBand = poDataset->GetRasterBand( 1 );
	Rcpp::NumericMatrix mat( poBand->GetXSize(), poBand->GetYSize() * poDataset->GetRasterCount());
	size_t i = 0;
	for (size_t band = 0; band < poDataset->GetRasterCount(); band++) {
		poBand = poDataset->GetRasterBand( band + 1 );
		for (size_t row = 0; row < poBand->GetYSize(); row++) {
			std::vector<double> buf( poBand->GetXSize() );
			CPLErr err = poBand->RasterIO( GF_Read, 0, row, poBand->GetXSize(), 1,
                  	buf.data(), buf.size(), 1, GDT_Float64, 0, 0);
			if (err == CE_Failure)
				Rcpp::stop("read failure");
			mat(_, i++) = Rcpp::NumericVector(buf.begin(), buf.end());
		}
	}
	Rcpp::IntegerVector dims(3);
	dims(0) = poBand->GetXSize();
	dims(1) = poBand->GetYSize();
	dims(2) = poDataset->GetRasterCount();
	mat.attr("dim") = dims; // converts to array
	return mat;
}
