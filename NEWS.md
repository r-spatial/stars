# version 0.1-0

* handle `logical` arrays in plot

* add `st_apply`, analogous to `apply`

* add cropping/masking when used as x[buf] with buf an `sf`, `sfc` or `bbox` object; masking when `x[buf, crop = FALSE]`

* add Ops (+,-,/ etc) and Math (sqrt, pow, sin etc) methods

* add `dimnames` and `dimnames<-` methods for stars objects

* downsample large grids to only plot pixels actually shown

* can plot rectilinear grids (but will plot rgb images as regular grids)

* `rbg` argument to `image` works 

* `[` array-like subsetting works; first index is attribute selector

# version 0.0

* interface the 9 C++ gdal utils through `gdal_utils()`
