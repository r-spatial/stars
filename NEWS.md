# version 0.6-8

* address difftime issue new in R-devel rev 87670

* `c.stars()` is more strict when combining time sequences; #703

* fix plotting when breaks contain duplicates; #728

* fix `st_as_stars.im()`; #727 and #648, thanks to Barry Rowlingson

# version 0.6-7

* `st_extract()` fix if points coincide with boundary grid cell centers and bilinear interpolation is used; #720

* `st_extract()` if used with GDAL 3.10.0 uses InterpolateAtPoints, allowing for cubic and cubicspline interpolators (requiring sf >= 1.0-19).
 
* `Ops.stars()` (math ops) now also recycle arrays in the first argument; #718

* `c.stars()` verifies semantic equivalence of objects' CRS; #703

* initial support for `read_mdim()` to work with `proxy = TRUE`; #659 

# version 0.6-6

* skip `cubble` tests for cubble version 0.3.1; https://github.com/huizezhang-sherry/cubble/issues/30

* `st_transform.stars` transforms geometries in array elements

* `mutate.stars` (and others) handle attribute names with spaces in them; #689

* `st_crop()` gains an argument `normalize`; when set to `TRUE` `st_normalize()` is called on the returned value; #685, #686

* constrain reading full GEOLOCATION arrays to the case where they are 2-D; #678

# version 0.6-5

* fix `st_as_stars.Spatial()` for `Spatial` gridded objects with non-square grid cells, see https://github.com/r-spatial/gstat/issues/123

* add `prcomp()` methods for `stars` and `stars_proxy` objects, working on attributes or last dimension

* `st_rasterize()` with `align=TRUE` returns `NA` values where there are no data; #668

* `read_mdim()` reads tables with composity type, returning a `data.frame` in that case; #659

* `st_rotate()` transforms a rotated grid back to a curvilinear grid in unrotated coordinates.

* `aggregate.stars()` deals with functions that return more than one number, putting these in a new dimension like `st_apply()` does

* `st_as_stars.data.frame()` and `st_as_stars.sf()` better handle non-raster data cubes

* `plot.stars()` only resets layout when needed (more than one sub-plot, or key present)

* fixed `st_as_stars.im()`; #648

* `st_crs<-.stars()` is less critical on existing CRS being of class `crs` 

* `c.stars()` with a single (valid) argument and `along` specified adds a dimension; #646

* `st_join.stars()` keeps attributes from `x` complete; #643

* `st_as_stars.list()` requires a _named_ list, and will set names of array dimensions if not present

# version 0.6-4

* `plot.stars()` has a `fill` argument that shifts unused plotting space between sub-maps to the bottom or right side of the plotting area

* in `plot.stars()`, the `key.width` default is sensitive to `par("ps")`, the pointsize graphics parameter 

* `plot.stars()` and `image.stars()` are sensitive to `cex.axis`, for axes and key (requires sf >= 1.0-14); #642

* move `lwgeom` dependency to Suggests; using `st_transform_proj()` on vector data cubes requires loading `lwgeom` first

# version 0.6-3

* `st_downsample()` has argument `offset` to pixel-shift downsampled images

* `st_downsample()` has argument `FUN` to compute sub-tile aggregates; #604

* `st_as_stars.bbox()` retains `factor` values; #640

* fix CRAN error in test script

* `st_crop()` works (and warns) for the case when the crop area does not overlap with the area of the object; #638

# version 0.6-2

* `split.stars()` accepts `stars` objects with multiple attributes; #635

* `[.stars()` supports `NA` values in dimension ranges for vector geometry (`sfc`) dimensions, resulting in empty geometries

* `st_extract()` supports extracting points values from curvilinear grids (when not proxy); #632

* `read_mdim()` reads curvilinear rasters (geolocation arrays), so far only at full extent/resolution

* `st_as_stars.stars()` accepts curvilinear argument with lon/lat array names present in `.x`

* consistently use `OGC:CRS84` instead of `EPSG:4326`

* setting `values = NULL` in `st_set_dimensions()` removes dimension values

* more gracefully handle plotting of global coverage curvilinear grids; #632

* `image.stars()` plots images (e.g. of cross sections) when x and/or y are singular or absent; #628

* `st_as_stars.cubble_df()` adds conversion from cubble; `cubble::as_cubble()` methods converts back

* `[<-.stars()` accepts for `i` an indicator (numeric length character) to the array to be replaced; #626

* `plot.stars()` gains an argument `key.lab` to set the legend key label (requires sf >= 1.0-13)

# version 0.6-1

* remove `rgdal` dependency

* `read_stars()` fixes combining bands with different block sizes; #623

* `st_warp()` gets (proper) default value for `threshold`; #618

* `read_mdim()` reads "raster" with single pixel (as a point)

* `[.stars()`, as in `r[x]` allows `x` to be a logical stars object

* `[<-.stars_proxy()` clones environment, so that after `r[r > 100] = NA` we don't get infinite recursion when realizing `r`

* `read_stars()` avoids reading a raster twice to determine how to choose `proxy`; `proxy` can now be set as (and defaults to) the number of cells (bands * rows * columns) above which data will not be read in memory but returned as `stars_proxy` object; #609

* fix using `RasterIO` in `read_stars()` when `proxy=TRUE`; #608

* `plot.stars()` hook function can handle arguments `row`, `col`, `nrow`, `ncol`, `nr`, `value` and `bbox`; #600

* fix handling of categorical rasters with colors but without category labels; #595, fallout of #565

* fix subsetting of proxy objects over a time range; #596

# version 0.6-0

* `write_stars()` writes scaled and/or shifted values when using argument `scale_offset`; #589

* `aggregate.stars_proxy()` implements aggregation with non-spatial `by` objects (lazily)

* fix `[.stars_proxy()` when selecting dimension 3 and higher; #561

* in `plot.stars()`, `col` can also be a palette function

* `st_res()` returns spatial resolutions, and optionally all dimension resolutions; #557 thanks to Krzysztof Dyba

* `read_stars()` shortens band or array names that contain a common start or ending, unless names would become empty or `shorten=FALSE` was set; e.g. `shorten="B"` puts a `B` before shortened array names

* printing `stars` dimension tables omits fields with only `NULL` or `NA` values, unless `print(..., all = TRUE)` is given

* improve reading categorical rasters, which now avoids calling `factor()`; #565 thanks to Krzysztof Dyba

* `read_mdim()` will read bounds arrays for coordinates using the `bounds` attribute, and accepts a `bounds` argument to specify them when that attribute is missing

* `time()` returns time stamps of a time dimension

* `st_cells()` returns the cell index for a set of point coordinates, provided as `sf` or `sfc` object; #558

* reading & writing vector data cubes: `read_mdim()` reads CF compliant vector geometries, and reconstructs them into an `sfc` dimension; `write_mdim()` writes them.

* `write_mdim()` uses GDAL multidimensional array API;

* `read_mdim()` uses arguments `offset`, `count` and `step` to read sub-arrays or strided arrays (requires sf >= 1.0-9)

# version 0.5-6

* export `read_mdim()`, a reader using GDAL's multidimensional array API (for sf <= 1.0-8)

* remove `tos_O1_2001-2002.nc` from packaged datasets to keep source package size below 5 Mb

* `as.POSIXct.stars()` converts `PCICt` dimensions to `POSIXct` values.

* improve handling of `PCICt` 360 or 365 day calendars; read them in `read_mdim` (requires sf >= 1.0-9)

* `read_stars()` reads factor levels better from attribute table; #484 thanks to @ailich

* `read_stars()` puts band names from `band_meta` DESCRIPTION= tags into `values`;

* improve handling of categorical rasters, and their exchange with `terra`; #484

* `plot()` handles auto colors better for factor arrays

* `read_ncdf()` handles units more formally in setting crs; #533

* print message that dimensions of proxy objects do not reflect unevaluated operations; #530

* passing `na.action = na.omit` to `geom_stars()` removes `NA` values; #532

* `read_stars()` detects curvilinear grids automatically; #513

* `st_warp()` warps curvilinear grids (using Euclidean distances only on coordinates); #513

* `Ops.stars()` errors when (common) dimension are not identical; #506

* `guess_raster()` accepts empty rows/columns and sparse grids; #509

* speed up `rgb` plotting; #503

* Added a new helper function `st_tile()` to specify the block
  parameters (`nXOff`, `nYOff`, `nXsize`, `nYSize`) required by `RasterIO` 
  argument in `read_stars()`; #492 thanks to Krzysztof Dyba

# version 0.5-5

* `st_as_stars.bbox()` creates an empy raster file if `proxy = TRUE`; #489

* `st_rasterize()` has option `align = TRUE` to use a template for aligning the new raster to; #489

* `adrop.stars()` with missing dimensions no longer drops x/y raster dimensions; #485

* `aggregate.stars()` propagates units of arrays; #477

# version 0.5-4

* `c.stars()` fails if it tries to merge arrays with different units; #475

* For NetCDF files, `read_stars()` uses the `long_name` as array name; #475

* add `rename()` method; #470

* refresh CRS of packaged `L7_ETMs.tif`; #466

* `as.data.frame.stars()` works for mixed regular and rectilinear dimension; #458

* `plot.stars()` plots curvilinear rasters with color table, or without table but `col` argument passed; #456

* `st_extract()` accepts a matrix with points as `at` argument, for when performance is important; see e.g. https://github.com/rspatial/terra/issues/341

* fix bug in `st_crop()` when cropping area is larger than grid; #455

* export `st_downsample()`, e.g. to be used by `tmap`; https://github.com/r-tmap/tmap/issues/597

* argument `downsample` in `plot.stars()` and `st_as_stars.stars_proxy()` and `st_downsample()` has the same effect (removed a one-offset between them).

* `st_redimension()` works for curvilinear grids; #441

* `downsample` is propagated to subexpressions like `r[r < 50] = NA`

* `predict.stars()` obtains an argument `drop_dimensions` that, if `TRUE`, drops dimensions from the prediction `data.frame`; #362

* extend options in `st_rgb()`, #432, by Gabo Gaona

* allow subsetting with `[` by using labels, e.g. of band names.

# version 0.5-3

* `read_stars()` accepts a function (or list with functions) as first argument, 
   allowing for saving `stars` objects that read from package directories resolving 
   platform-dependent paths at run-time

* handle categorical rasters starting at value 0 (by adding 1, and warning); #428

* add `%in%` method; #424

* `read_stars()` gains an argument `tolerance` to control tolerance in dimension value comparisons; #414

* binary Ops (like `+`, `-`, `*` etc.) work for `stars_proxy` objects; #390

* `st_rasterize()` rasterizes multiple attributes, and handles factors (when sf >= 0.9-9)

* `write_stars()` deals better with `stars_proxy` objects; #404

* fix regression in reading some `stars_proxy` objects; #379

* add `[<-` (partially) and `is.na` methods for `stars_proxy` objects; #402

* add `replace_na()` methods; #402

# version 0.5-2

* read and write factor levels as GDAL category names; write color table; #392

* handle `normalize_path` for choosing to `proxy`; #391

* ignore units when there are different units across bands of a subdataset

* speed up `st_rgb()` using faster `st_apply()` approach; #315, #390

* improve handling of crs in Spatial objects (avoid loss of wkt comments)

* correctly write band subsets for smaller proxy objects; #291

* write arbitrarily cropped proxy objects; #291

* speed up `st_apply()` when a function is provided that works on chunks at a time; #390

* warn when breaks = "quantile" results in a single class; #388

* fix `[` bug selecting bands in proxy objects; #388

* for `stars_proxy` objects, `write_stars()` writes all objects into a multi-layer file; #385

* multi-file proxy objects can be `st_warp()`ed with `use_gdal = TRUE`; #385

# version 0.5-1

* fix weird GDAL-related bug in stars2 vignette

* `read_ncdf()` does not take time as mid-points of regular intervals, but as starting points; #378

# version 0.5-0

* fix handling of rasters with color tables; #375

* `st_apply()` and other methods for `stars_proxy` objects handle ... ; #374

* add `st_bbox()`, `st_crs()` methods for terra's `SpatVector` objects; https://github.com/r-tmap/tmap/issues/536

* add `st_bbox()`, `st_crs()` and `st_as_stars()` methods for terra's `SpatRaster` objects; https://github.com/r-tmap/tmap/issues/536

* allow for multi-resolution attributes in `stars_proxy` objects (e.g., all gray scale sentinel-2 bands); see vignettes 2 and 7 for examples.

* `plot()` defaults to a categorical color scale when plotting a factor variable; https://github.com/r-tmap/tmap/issues/526

* `st_extract()` extracts space-time points if `time_column` is specified, and handles time intervals; #352

* add `[[<-.stars` method, which is now called by `$<-.stars`, so that array names can be set programmatically

* add `transmute()` methods

* `plot.stars()` calls `droplevels` if a factor array has any `NA` levels; #339

* `read_stars()` reads `NaN`s as `NA`; #333

* improve `st_extract()` method for both `stars` and `stars_proxy` objects; interpolation options are reduced to bilinear; #322, #279, #290

* better handle categorical rasters that do not start at value 1; #329

* plot layout can be controlled with `mfrow = c(nr, nc)` argument

* `stars_proxy` objects have a normalized path; #331

* cropping or selecting with `bbox` treats cells always as small polygons; #330

* add faster `st_extract()` method for `stars` objects; #322

* added vignette: "How `raster` functions map to `stars` functions", by Sebastien Rochette; #122, #325

* fix bug in dimension `values` field when downsampling; #324

* `write_stars()` also writes out band names; #323

* add `rgdal` to Suggests:

* each `call_list` entry of a `stars_proxy` object carries its proper calling environment; #309

* `st_as_sf.stars()` copes with zero attribute (empty) stars objects

* add `st_set_bbox()` generic, to set raster extent, motivated by #315

* set up tic, with great help from @pat-s, #313

* get rid of more `proj4string`s for representing coordinate reference systems; #312

* as(x, "Spatial") correctly handles `from` dimension values different from one

* `read_stars()` now sets the `BANDNAME` GDAL metadata item, or else the band's GetDescription() as the band's dimension values

* `st_as_stars.data.frame()` reads simple tables (non-raster data) if `dims` has length less than 2

* band descriptions are in the band dimension values

* dimension tables are simpler, and are shown properly in Rstudio

* `st_rgb()` gains a `probs` argument, to cut off and stretch based on quantiles

* `as(x, "Raster")` merges multiple attributes before converting to raster brick

# version 0.4-3

* fix bug in `st_as_stars.Raster`; set crs to the one assigned by raster; https://github.com/r-tmap/tmap/issues/471

* add `s2` to Suggests:

* new function `st_rgb()` collapses (reduces) a dimension to rgb hex value; #302

# version 0.4-2

* `aggregate.stars()` handles arrays with NA values now correctly; brought up in #299 by Thorsten Simon

* `aggregate.stars()` gains an argument `exact` which, if `TRUE`, calls `exactextractr` for polygonal aggregation; #289 

* `read_stars()` reads all subdatasets with dimensions equal to first, and warns when ignoring others; #296

* make copying over of dimensions somewhat easier; #295

* `st_as_stars.Raster()` tries to read from file if the raster object is not an in-memory object.

* `write_stars()` normalizes path, as `read_stars` already did; #293

* `merge()` for proxy objects acts, and is no longer lazy; #290

* `st_as_stars.Raster()` returns a proxy object if the raster layer is on disk

* add `st_extract()` to extract e.g. time series from grids at point locations; #279; #290

* `read_stars()` chooses a value for `proxy` that depends on the data dimensions; #281

* x/y range subsetting of `stars_proxy` objects now only reads that range, similar to how crop already did this.

* `st_warp()` preserves levels and colors; https://github.com/r-tmap/tmap/issues/429

* `st_crop()` works with bounding boxes larger than the downsampled bounding box; #276

* `st_crop()` has a non-zero default for `epsilon` (bounding box shrinkage) to exclude cells touching the crop bounding box; #275

* `image.stars()` (and hence `plot.stars`) gains an `extent` argument for setting the extent of a plot; https://github.com/r-spatial/sf/issues/1193

# version 0.4-1

* `st_warp()` (stars native) flips longitudes a full cycle; #256, #264, #269

* handle axis order in `st_transform` (requires sf >= 0.9-1)

* depend on sf 0.9-0 

* adapt to cubelyr split-off from dplyr; add cubelyr to Suggests:; https://github.com/hadley/cubelyr/issues/2

* add `droplevels()` method

* handle color tables, category tables and raster attribute tables read through GDAL; #128, #245; https://github.com/r-spatial/mapview/issues/208

* handle dimension `crs` specially, for proxy objects now.

* handle new-style `crs` objects from upcoming sf, moving away from proj4strings

* handle full `crs` objects as `refsys` element in a spatial dimensions, rather than proj4string only

* `st_raster_type(x)` reveals the raster type of `x`; #248, https://github.com/r-tmap/tmap/issues/368

* add `st_as_stars.OpenStreetMap()` method; #241 by @mtennekes

* add `st_flip()` to flip arrays along one or more dimensions without changing dimension properties

* add `as.owin()` method to convert (2D raster) stars objects to spatstat `owin`; https://github.com/r-spatial/sf/issues/1233

* for temporal aggregation, `aggregate.stars` now also takes `by` arguments like "week", "month", or "5 days" 

* add `st_as_stars()` method for `xts` objects; improve `as.xts` for `stars` objects

* skip some tests on solaris

# version 0.4-0

* `plot()` now uses all data to figure out breaks, in order to also find extremes; #216

* `st_mosaic()` creates mosaics from spatially disjoint rasters; #210

* #205 large refactoring of `read_ncdf`, by David Blodgett and Mike Sumner, affecting #199, #89, #30, #86, #175

* allow for funny units like `m s**-1`; #201

* add `contour()` method for `stars` objects; #196

* plot uses `rasterImage` by default if available; #194

* the `x` and `y` raster dimensions can be set and unset with `xy` argument in `st_set_dimensions`; #190

* retain `factor` levels with dimension values when set in `st_set_dimensions`; #188

* add conversion from `stars_proxy` to `Raster`: #193

* make plotting of multiple curvilinear grids work

* plot by default no cell borders in case of curvilinear, rotated or sheared grids

* robustify handling of units

* allow `read_ncdf()` to ignore bounds

* scale was applied wrongly on multi-band images; #189, this requires sf >= 0.7-5

* `.nc` is now recognized correctly by `write_stars` and will write a NetCDF file; #186 

* `[` subset now works correctly with negative or logical indices; #184, #185

* `NA` values for float32 grids are now correctly detected; #182, this requires sf >= 0.7-5

* cropping of a `stars_proxy` object now works; #179

* `st_apply()` can now loop over Raster layers; examples in #176

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

* remove import of `ncdf4` in favor of `RNetCDF`, now in line with practice in `ncmeta` package. Thanks to David Blodgett for motivation and testing (see #87, #94). 

* `st_as_sf` uses date/time column names when appropriate

* allow missing trailing comma's when subsetting: `a[1,,]` and `a[1,]` now do the same.

* move `rlang` to Imports: ; rewrite `[` subset using rlang.

* add conversion to and from `Spatial*` classes, including the gridded ones, taking care of `factor` variables

* depend on sf 0.7-2

* add `logz` support for log-scale keys to `plot` and `image`

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
