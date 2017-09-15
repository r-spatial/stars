#include "cpl_port.h"
#include "gdal.h"
#include "gdal_priv.h"

#include "Rcpp.h"

#include "stars.h"

using namespace Rcpp;

CharacterVector get_meta_data(GDALDatasetH ds, CharacterVector domain_item) {

	CharacterVector ret;
	if (ds == NULL)
		return NA_STRING;
	if (domain_item.size() == 0)
		ret = charpp2CV(GDALGetMetadata(ds, NULL));
	else if (domain_item.size() == 1) {
		if (CharacterVector::is_na(domain_item[0])) {
			 char **dl = GDALGetMetadataDomainList(ds);
			 ret = charpp2CV(dl);
			 CSLDestroy(dl);
		} else
			 ret = charpp2CV(GDALGetMetadata(ds, domain_item[0]));
	} else if (domain_item.size() == 2) // domain and item
		ret = CharacterVector::create(GDALGetMetadataItem(ds, domain_item[1], domain_item[0]));
	else
		ret = NA_STRING;
	return(ret);
}

// [[Rcpp::export]]
CharacterVector CPL_GetMetadata(CharacterVector obj, CharacterVector domain_item,
		CharacterVector options) {
	
	GDALDatasetH ds = GDALOpenEx(obj[0], GDAL_OF_RASTER | GDAL_OF_READONLY, NULL, NULL, 
		create_options(options).data());
	CharacterVector ret = get_meta_data(ds, domain_item);
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
	NumericVector gt_r_inv(6);
	for (int i = 0; i < 6; i++)
		gt_r_inv(i) = retval ? gt_inv[i] : NA_REAL;
	ret(3) =  gt_r;

	ret.attr("names") = CharacterVector::create("nbands", "crs", "gt", "gt_inv");

	return ret;
}
