# version 0.2-0

* add elementary support for `cut` methods and factor arrays (plot, subset); #56

* add `st_rasterize`, which uses `GDALRasterize` to rasterize an sf object; #13

* `st_as_sf.stars` now uses `GDAL(F)Polygonize` when give a regular or sheared grid; #13

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
