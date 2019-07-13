# version 0.3-3

* we can now set and unset the `x` and `y` raster dimensions with `xy` argument in `st_set_dimensions`; #190

* retain `factor` levels with dimension values when set in `st_set_dimensions`; #188

* add conversion from `stars_proxy` to `Raster`: #193

* make plotting of multiple curvilinear grids work

* plot by default no cell borders in case of curvilinear, rotated or sheared grids

* robustify handling of units

* allow `read_ncdf` to ignore bounds

# version 0.3-2

* scale was applied wrongly on multi-band images; #189, this requires sf >= 0.7-5

* `.nc` is now recognized correctly by `write_stars` and will write a NetCDF file; #186 

* `[` subset now works correctly with negative or logical indices; #184, #185

* `NA` values for float32 grids are now correctly detected; #182, this requires sf >= 0.7-5

* cropping of a `stars_proxy` object now works; #179

* `st_apply` can now loop over raster layers; examples in #176

# version 0.3-1

* `st_as_stars.bbox` now has an `ncells` and `pretty` argument, to better choose default raster dimensions

* `geom_stars` now works with `stars_proxy` objects, but needs `downsample` to be set; #21

* `NA` values in Float32 rasters are now read correctly with `read_stars`; #182

* handle bounds, when given, in `read_ncdf`

* provide time parsing (POSIXct, PCICt) for `read_ncdf`; #115

# version 0.3-0

* add `st_area` method to return raster grid cell sizes; #99

* fix `st_warp` with `use_gdal=TRUE`, allowing for multi-band warps

* add `st_get_dimension_values` to get the values of a particular dimension (if meaningful); #100

* allow for setting intervals as dimension values; see examples of `st_dimensions`

* add `st_contour`, and clean up `st_as_sf`; #99

* experimental color table support; https://github.com/r-spatial/mapview/issues/208

* rewrote vignettes, added vignettes; #99

* deprecate `st_write.stars` for `write_stars`; #96

* use "native" R array-factor support

* support for `PCICt` 360- and 365-day calendars; #29

* remove import of `ncdf4` in favour of `RNetCDF`, now in line with practice in `ncmeta` package. Thanks to David Blodgett for motivation and testing (see #87, #94). 

* `st_as_sf` uses date/time column names when appropriate

* allow missing trailing comma's when subsetting: `a[1,,]` and `a[1,]` now do the same.

* move `rlang` to Imports: ; rewrite `[` subset using rlang.

* add conversion to and from `Spatial*` classes, including the gridded ones, taking care of `factor` variables

* depend on sf 0.7-2

* add logz support for log-scale keys to `plot` and `image`

# version 0.2-0

* vignettes now use an external package, `starsdata`, for larger dataset examples

* support `[<-.stars` e.g. to mask out values; support `is.na.stars` to replace NA masks

* support `cut` methods and factor arrays (plot, subset); #56

* add `st_rasterize`, which uses `GDALRasterize` to rasterize an sf object; #13

* `st_as_sf.stars` now uses `GDAL(F)Polygonize` when give a regular or sheared grid grid cells are not points, and returns contour bands using `GDALContourGenerateEx` (requiring GDAL 2.4.0) in case cells are points; #13

* support curvilinear grids; see #54 and the `data_model` vignette

* add vignette about how `stars_proxy` objects work

* `stars_proxy` objects defer processing of `st_apply` jobs until after subsampling; #50

* allow reading sections of a raster, raster at a lower resolution, selected bands; #48

* allow reading vectors (arrays) with more than 2^31 elements; #48

* fold all higher dimensions into the third dimension before coercing to `Raster`; #40

# version 0.1-1

* add meta data reader to `read_stars`

# version 0.1-0

* add `merge` (merge attributes into array dimension) and `split` (split dimension over attributes)

* interface to sf, raster and spacetime

* improve plotting

* handle `logical` arrays in plot

* add `st_apply`, analogous to `apply`

* add cropping/masking when used as x[buf] with buf an `sf`, `sfc` or `bbox` object; masking when `x[buf, crop = FALSE]`

* add Ops (+,-,/ etc) and Math (sqrt, pow, sin etc) methods

* add `dimnames` and `dimnames<-` methods for stars objects

* downsample large grids to only plot pixels actually shown

* can plot rectilinear grids (but will plot rgb images as regular grids)

* `rgb` argument to `image` works 

* `[` array-like subsetting works; first index is attribute selector

# version 0.0

* interface the 9 C++ gdal utils through `gdal_utils()` (now part of `sf`)
