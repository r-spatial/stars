#include "cpl_port.h"
#include "gdal.h"

#include "Rcpp.h"

#include "stars.h"

using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector CPL_GetMetadata(CharacterVector obj, CharacterVector domain_item,
		CharacterVector options) {
	
	GDALDatasetH ds = GDALOpenEx(obj[0], GDAL_OF_RASTER | GDAL_OF_READONLY, NULL, NULL, 
		create_options(options).data());
	if (ds == NULL)
		return NA_STRING;
	CharacterVector ret;
	if (domain_item.size() == 0)
		ret = charpp2CV(GDALGetMetadata(ds, NULL));
	else if (domain_item.size() == 1) {
		if (domain_item[0] == NA_STRING)
			 ret = charpp2CV(GDALGetMetadataDomainList(ds));
		else
			 ret = charpp2CV(GDALGetMetadata(ds, domain_item[0]));
	} else if (domain_item.size() == 2) // domain and item
		ret = CharacterVector::create(GDALGetMetadataItem(ds, domain_item[1], domain_item[0]));
	else
		ret = NA_STRING;
	GDALClose(ds);
	return ret;
}

// [[Rcpp::export]]
List CPL_get_crs(CharacterVector obj, CharacterVector options) {
	List ret(4);
	GDALDatasetH ds = GDALOpenEx(obj[0], GDAL_OF_RASTER | GDAL_OF_READONLY, NULL, NULL, 
		create_options(options).data());
	if (ds == NULL)
		return ret;
	ret(0) = GDALGetRasterCount(ds);
	ret(1) = GDALGetProjectionRef(ds);
	double gt[6];
	GDALGetGeoTransform(ds, gt);
	NumericVector gt_r(6);
	for (int i = 0; i < 6; i++)
		gt_r(i) = gt[i];
	ret(2) =  gt_r;
	double gt_inv[6];

	int retval = GDALInvGeoTransform(gt, gt_inv);
	if (retval == FALSE)
		return ret;

	NumericVector gt_r_inv(6);
	for (int i = 0; i < 6; i++)
		gt_r_inv(i) = gt_inv[i];
	ret(3) =  gt_r;
	ret.attr("names") = CharacterVector::create("nbands", "crs", "gt", "gt_inv");
	return ret;
}
