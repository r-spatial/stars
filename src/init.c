#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP stars_CPL_add_one(SEXP);
extern SEXP stars_CPL_gdal_cleanup_all();
extern SEXP stars_CPL_gdalinfo(SEXP, SEXP);
extern SEXP stars_CPL_gdal_init();
extern SEXP stars_CPL_gdalrasterize(SEXP, SEXP, SEXP);
extern SEXP stars_CPL_gdal_version(SEXP);
extern SEXP stars_CPL_gdalwarp(SEXP, SEXP, SEXP);
extern SEXP stars_CPL_get_crs(SEXP, SEXP);
extern SEXP stars_CPL_GetMetadata(SEXP, SEXP, SEXP);
extern SEXP stars_CPL_proj_info(SEXP);
extern SEXP stars_CPL_proj_is_valid(SEXP);
extern SEXP stars_CPL_proj_version(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"stars_CPL_add_one",          (DL_FUNC) &stars_CPL_add_one,          1},
    {"stars_CPL_gdal_cleanup_all", (DL_FUNC) &stars_CPL_gdal_cleanup_all, 0},
    {"stars_CPL_gdalinfo",         (DL_FUNC) &stars_CPL_gdalinfo,         2},
    {"stars_CPL_gdal_init",        (DL_FUNC) &stars_CPL_gdal_init,        0},
    {"stars_CPL_gdalrasterize",    (DL_FUNC) &stars_CPL_gdalrasterize,    3},
    {"stars_CPL_gdal_version",     (DL_FUNC) &stars_CPL_gdal_version,     1},
    {"stars_CPL_gdalwarp",         (DL_FUNC) &stars_CPL_gdalwarp,         3},
    {"stars_CPL_get_crs",          (DL_FUNC) &stars_CPL_get_crs,          2},
    {"stars_CPL_GetMetadata",      (DL_FUNC) &stars_CPL_GetMetadata,      3},
    {"stars_CPL_proj_info",        (DL_FUNC) &stars_CPL_proj_info,        1},
    {"stars_CPL_proj_is_valid",    (DL_FUNC) &stars_CPL_proj_is_valid,    1},
    {"stars_CPL_proj_version",     (DL_FUNC) &stars_CPL_proj_version,     1},
    {NULL, NULL, 0}
};

void R_init_stars(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
