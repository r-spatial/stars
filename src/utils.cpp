#include "cpl_port.h"
#include "gdal.h"

#include "gdal_utils.h" // requires 2.1

#if GDAL_VERSION_MAJOR == 2 && GDAL_VERSION_MINOR < 1
# error "Insufficient GDAL version, 2.1 required"
#endif

#include "Rcpp.h"

#include "stars.h"

// [[Rcpp::export]]
Rcpp::CharacterVector CPL_gdalinfo(Rcpp::CharacterVector obj, Rcpp::CharacterVector options) {
	char **options_char = create_options(options).data();
	GDALInfoOptions* opt = GDALInfoOptionsNew(options_char, NULL);
	GDALDatasetH ds = GDALOpen((const char *) obj[0], GA_ReadOnly);
	Rcpp::CharacterVector ret = GDALInfo(ds, opt);
	GDALInfoOptionsFree(opt);
	return ret;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_gdalwarp(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options) {

	int err = 0;
	char **options_char = create_options(options).data();
	GDALWarpAppOptions* opt = GDALWarpAppOptionsNew(options_char, NULL);

	std::vector<GDALDatasetH> src_pt(src.size());
	for (int i; i < src.size(); i++)
		src_pt[i] = GDALOpen((const char *) src[i], GA_ReadOnly);
	GDALDatasetH dst_pt = GDALOpen((const char *) dst[0], GA_Update);
	GDALDatasetH result = GDALWarp(NULL, dst_pt, src.size(), src_pt.data(), opt, &err);
	GDALWarpAppOptionsFree(opt);
	for (int i; i < src.size(); i++)
		GDALClose(src_pt[i]);
	GDALClose(dst_pt);
	if (result != NULL)
		GDALClose(result);
	if (result == NULL || err)
		return false;
	else
		return true;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_gdalrasterize(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options) {

	int err = 0;
	char **options_char = create_options(options).data();
	GDALRasterizeOptions* opt =  GDALRasterizeOptionsNew(options_char, NULL);

	GDALDatasetH src_pt = GDALOpen((const char *) src[0], GA_ReadOnly);
	GDALDatasetH dst_pt = GDALOpen((const char *) dst[0], GA_Update);
	GDALDatasetH result = GDALRasterize(NULL, dst_pt, src_pt, opt, &err);
	GDALRasterizeOptionsFree(opt);
	GDALClose(src_pt);
	GDALClose(dst_pt);
	if (result != NULL)
		GDALClose(result);
	if (result == NULL || err)
		return false;
	else
		return true;
}

// still todo:

// GDALTranslate
// GDALVectorTranslate
// GDALDEMProcessing
// GDALNearBlack
// GDALGrid
// GDALBuildVRT
