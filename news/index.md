# Changelog

## version 0.7-0

- use `values` column from RAT table, and merge duplicate labels;
  [\#761](https://github.com/r-spatial/stars/issues/761)

- [`st_extract()`](../reference/st_extract.md) no longer stops when
  GDAL’s `InterpolateAtPoint` returns error value(s);
  [\#760](https://github.com/r-spatial/stars/issues/760)

- add [`st_as_stars()`](../reference/st_as_stars.md) methods for
  `CFVariable` and `CFDataset`, provided by package `ncdfCF`;
  [\#756](https://github.com/r-spatial/stars/issues/756),
  [\#757](https://github.com/r-spatial/stars/issues/757) by
  [@pvanlaake](https://github.com/pvanlaake)

- try harder to convert `CFTime` times to `Date` when they are dates.

- if set, `options("stars.regular")` controls the threshold below which
  dimension coordinates are considered regular, with default 1.0e-4

- removed dependency on PCICt, added dependency on CFtime;
  [\#754](https://github.com/r-spatial/stars/issues/754) by
  [@pvanlaake](https://github.com/pvanlaake)

- handling of dimensions with a single value is now more consistent;
  [\#754](https://github.com/r-spatial/stars/issues/754)

- [`slice.stars_proxy()`](../reference/dplyr.md) works (again) for
  single-file proxy objects;
  [\#751](https://github.com/r-spatial/stars/issues/751),
  [\#527](https://github.com/r-spatial/stars/issues/527)

- `[.stars()` with a `character` selector selects on names of a
  dimension if it has names;
  [\#747](https://github.com/r-spatial/stars/issues/747)

- handle “intervals” dimension in aggregate.stars() and
  [`st_as_sf.stars()`](../reference/st_as_sf.md);
  [\#745](https://github.com/r-spatial/stars/issues/745)

- fix [`c.stars()`](../reference/c.stars.md) for the case where some of
  the objects have a single slice on the `along=` dimension;
  [\#743](https://github.com/r-spatial/stars/issues/743)

- [`st_redimension()`](../reference/redimension.md) (and by that,
  [`merge.stars()`](../reference/merge.md)) use
  [`abind()`](https://rdrr.io/pkg/abind/man/abind.html) for numeric
  variables rather than [`c()`](https://rdrr.io/r/base/c.html), reducing
  memory overhead.

- fix problem with reading HDF4 files;
  [\#741](https://github.com/r-spatial/stars/issues/741) by Alexys
  Rodriguez

- use, by default,
  [`normalizePath()`](https://rdrr.io/r/base/normalizePath.html) on
  `filename` in [`read_mdim()`](../reference/mdim.md);
  [\#735](https://github.com/r-spatial/stars/issues/735)

- [`st_extract()`](../reference/st_extract.md) accepts empty points as
  target; [\#734](https://github.com/r-spatial/stars/issues/734)

- [`st_sfc2xy()`](../reference/st_xy2sfc.md) passes `...` on to
  [`st_as_stars()`](../reference/st_as_stars.md);
  [\#733](https://github.com/r-spatial/stars/issues/733)

## version 0.6-8

CRAN release: 2025-02-01

- address `/.difftime` issue new in R-devel rev 87670

- [`c.stars()`](../reference/c.stars.md) is more strict when combining
  time sequences; [\#703](https://github.com/r-spatial/stars/issues/703)

- fix plotting when breaks contain duplicates;
  [\#728](https://github.com/r-spatial/stars/issues/728)

- fix `st_as_stars.im()`;
  [\#727](https://github.com/r-spatial/stars/issues/727) and
  [\#648](https://github.com/r-spatial/stars/issues/648), thanks to
  Barry Rowlingson

## version 0.6-7

CRAN release: 2024-11-07

- [`st_extract()`](../reference/st_extract.md) fix if points coincide
  with boundary grid cell centers and bilinear interpolation is used;
  [\#720](https://github.com/r-spatial/stars/issues/720)

- [`st_extract()`](../reference/st_extract.md) if used with GDAL 3.10.0
  uses InterpolateAtPoints, allowing for cubic and cubicspline
  interpolators (requiring sf \>= 1.0-19).

- [`Ops.stars()`](../reference/ops_stars.md) (math ops) now also recycle
  arrays in the first argument;
  [\#718](https://github.com/r-spatial/stars/issues/718)

- [`c.stars()`](../reference/c.stars.md) verifies semantic equivalence
  of objects’ CRS;
  [\#703](https://github.com/r-spatial/stars/issues/703)

- initial support for [`read_mdim()`](../reference/mdim.md) to work with
  `proxy = TRUE`; [\#659](https://github.com/r-spatial/stars/issues/659)

## version 0.6-6

CRAN release: 2024-07-16

- skip `cubble` tests for cubble version 0.3.1;
  <https://github.com/huizezhang-sherry/cubble/issues/30>

- `st_transform.stars` transforms geometries in array elements

- `mutate.stars` (and others) handle attribute names with spaces in
  them; [\#689](https://github.com/r-spatial/stars/issues/689)

- [`st_crop()`](../reference/st_crop.md) gains an argument `normalize`;
  when set to `TRUE`
  [`st_normalize()`](https://r-spatial.github.io/sf/reference/st_normalize.html)
  is called on the returned value;
  [\#685](https://github.com/r-spatial/stars/issues/685),
  [\#686](https://github.com/r-spatial/stars/issues/686)

- constrain reading full GEOLOCATION arrays to the case where they are
  2-D; [\#678](https://github.com/r-spatial/stars/issues/678)

## version 0.6-5

CRAN release: 2024-04-04

- fix `st_as_stars.Spatial()` for `Spatial` gridded objects with
  non-square grid cells, see
  <https://github.com/r-spatial/gstat/issues/123>

- add [`prcomp()`](../reference/prcomp.md) methods for `stars` and
  `stars_proxy` objects, working on attributes or last dimension

- [`st_rasterize()`](../reference/st_rasterize.md) with `align=TRUE`
  returns `NA` values where there are no data;
  [\#668](https://github.com/r-spatial/stars/issues/668)

- [`read_mdim()`](../reference/mdim.md) reads tables with composity
  type, returning a `data.frame` in that case;
  [\#659](https://github.com/r-spatial/stars/issues/659)

- [`st_rotate()`](../reference/st_rotate.md) transforms a rotated grid
  back to a curvilinear grid in unrotated coordinates.

- [`aggregate.stars()`](../reference/aggregate.stars.md) deals with
  functions that return more than one number, putting these in a new
  dimension like [`st_apply()`](../reference/st_apply.md) does

- [`st_as_stars.data.frame()`](../reference/st_as_stars.md) and
  [`st_as_stars.sf()`](../reference/st_as_stars.md) better handle
  non-raster data cubes

- [`plot.stars()`](../reference/plot.md) only resets layout when needed
  (more than one sub-plot, or key present)

- fixed `st_as_stars.im()`;
  [\#648](https://github.com/r-spatial/stars/issues/648)

- `st_crs<-.stars()` is less critical on existing CRS being of class
  `crs`

- [`c.stars()`](../reference/c.stars.md) with a single (valid) argument
  and `along` specified adds a dimension;
  [\#646](https://github.com/r-spatial/stars/issues/646)

- [`st_join.stars()`](../reference/st_join.stars.md) keeps attributes
  from `x` complete;
  [\#643](https://github.com/r-spatial/stars/issues/643)

- [`st_as_stars.list()`](../reference/st_as_stars.md) requires a *named*
  list, and will set names of array dimensions if not present

## version 0.6-4

CRAN release: 2023-09-11

- [`plot.stars()`](../reference/plot.md) has a `fill` argument that
  shifts unused plotting space between sub-maps to the bottom or right
  side of the plotting area

- in [`plot.stars()`](../reference/plot.md), the `key.width` default is
  sensitive to `par("ps")`, the pointsize graphics parameter

- [`plot.stars()`](../reference/plot.md) and
  [`image.stars()`](../reference/plot.md) are sensitive to `cex.axis`,
  for axes and key (requires sf \>= 1.0-14);
  [\#642](https://github.com/r-spatial/stars/issues/642)

- move `lwgeom` dependency to Suggests; using `st_transform_proj()` on
  vector data cubes requires loading `lwgeom` first

## version 0.6-3

CRAN release: 2023-08-11

- [`st_downsample()`](../reference/st_downsample.md) has argument
  `offset` to pixel-shift downsampled images

- [`st_downsample()`](../reference/st_downsample.md) has argument `FUN`
  to compute sub-tile aggregates;
  [\#604](https://github.com/r-spatial/stars/issues/604)

- [`st_as_stars.bbox()`](../reference/st_as_stars.md) retains `factor`
  values; [\#640](https://github.com/r-spatial/stars/issues/640)

- fix CRAN error in test script

- [`st_crop()`](../reference/st_crop.md) works (and warns) for the case
  when the crop area does not overlap with the area of the object;
  [\#638](https://github.com/r-spatial/stars/issues/638)

## version 0.6-2

CRAN release: 2023-07-12

- [`split.stars()`](../reference/merge.md) accepts `stars` objects with
  multiple attributes;
  [\#635](https://github.com/r-spatial/stars/issues/635)

- `[.stars()` supports `NA` values in dimension ranges for vector
  geometry (`sfc`) dimensions, resulting in empty geometries

- [`st_extract()`](../reference/st_extract.md) supports extracting
  points values from curvilinear grids (when not proxy);
  [\#632](https://github.com/r-spatial/stars/issues/632)

- [`read_mdim()`](../reference/mdim.md) reads curvilinear rasters
  (geolocation arrays), so far only at full extent/resolution

- [`st_as_stars.stars()`](../reference/st_as_stars.md) accepts
  curvilinear argument with lon/lat array names present in `.x`

- consistently use `OGC:CRS84` instead of `EPSG:4326`

- setting `values = NULL` in
  [`st_set_dimensions()`](../reference/st_dimensions.md) removes
  dimension values

- more gracefully handle plotting of global coverage curvilinear grids;
  [\#632](https://github.com/r-spatial/stars/issues/632)

- [`image.stars()`](../reference/plot.md) plots images (e.g. of cross
  sections) when x and/or y are singular or absent;
  [\#628](https://github.com/r-spatial/stars/issues/628)

- [`st_as_stars.cubble_df()`](../reference/st_as_stars.md) adds
  conversion from cubble;
  [`cubble::as_cubble()`](https://huizezhang-sherry.github.io/cubble/reference/as_cubble.html)
  methods converts back

- `[<-.stars()` accepts for `i` an indicator (numeric length character)
  to the array to be replaced;
  [\#626](https://github.com/r-spatial/stars/issues/626)

- [`plot.stars()`](../reference/plot.md) gains an argument `key.lab` to
  set the legend key label (requires sf \>= 1.0-13)

## version 0.6-1

CRAN release: 2023-04-06

- remove `rgdal` dependency

- [`read_stars()`](../reference/read_stars.md) fixes combining bands
  with different block sizes;
  [\#623](https://github.com/r-spatial/stars/issues/623)

- [`st_warp()`](../reference/st_warp.md) gets (proper) default value for
  `threshold`; [\#618](https://github.com/r-spatial/stars/issues/618)

- [`read_mdim()`](../reference/mdim.md) reads “raster” with single pixel
  (as a point)

- `[.stars()`, as in `r[x]` allows `x` to be a logical stars object

- `[<-.stars_proxy()` clones environment, so that after
  `r[r > 100] = NA` we don’t get infinite recursion when realizing `r`

- [`read_stars()`](../reference/read_stars.md) avoids reading a raster
  twice to determine how to choose `proxy`; `proxy` can now be set as
  (and defaults to) the number of cells (bands \* rows \* columns) above
  which data will not be read in memory but returned as `stars_proxy`
  object; [\#609](https://github.com/r-spatial/stars/issues/609)

- fix using `RasterIO` in [`read_stars()`](../reference/read_stars.md)
  when `proxy=TRUE`;
  [\#608](https://github.com/r-spatial/stars/issues/608)

- [`plot.stars()`](../reference/plot.md) hook function can handle
  arguments `row`, `col`, `nrow`, `ncol`, `nr`, `value` and `bbox`;
  [\#600](https://github.com/r-spatial/stars/issues/600)

- fix handling of categorical rasters with colors but without category
  labels; [\#595](https://github.com/r-spatial/stars/issues/595),
  fallout of [\#565](https://github.com/r-spatial/stars/issues/565)

- fix subsetting of proxy objects over a time range;
  [\#596](https://github.com/r-spatial/stars/issues/596)

## version 0.6-0

CRAN release: 2022-11-21

- [`write_stars()`](../reference/write_stars.md) writes scaled and/or
  shifted values when using argument `scale_offset`;
  [\#589](https://github.com/r-spatial/stars/issues/589)

- `aggregate.stars_proxy()` implements aggregation with non-spatial `by`
  objects (lazily)

- fix `[.stars_proxy()` when selecting dimension 3 and higher;
  [\#561](https://github.com/r-spatial/stars/issues/561)

- in [`plot.stars()`](../reference/plot.md), `col` can also be a palette
  function

- [`st_res()`](../reference/st_res.md) returns spatial resolutions, and
  optionally all dimension resolutions;
  [\#557](https://github.com/r-spatial/stars/issues/557) thanks to
  Krzysztof Dyba

- [`read_stars()`](../reference/read_stars.md) shortens band or array
  names that contain a common start or ending, unless names would become
  empty or `shorten=FALSE` was set; e.g. `shorten="B"` puts a `B` before
  shortened array names

- printing `stars` dimension tables omits fields with only `NULL` or
  `NA` values, unless `print(..., all = TRUE)` is given

- improve reading categorical rasters, which now avoids calling
  [`factor()`](https://rdrr.io/r/base/factor.html);
  [\#565](https://github.com/r-spatial/stars/issues/565) thanks to
  Krzysztof Dyba

- [`read_mdim()`](../reference/mdim.md) will read bounds arrays for
  coordinates using the `bounds` attribute, and accepts a `bounds`
  argument to specify them when that attribute is missing

- [`time()`](https://rdrr.io/r/stats/time.html) returns time stamps of a
  time dimension

- [`st_cells()`](../reference/st_cells.md) returns the cell index for a
  set of point coordinates, provided as `sf` or `sfc` object;
  [\#558](https://github.com/r-spatial/stars/issues/558)

- reading & writing vector data cubes:
  [`read_mdim()`](../reference/mdim.md) reads CF compliant vector
  geometries, and reconstructs them into an `sfc` dimension;
  [`write_mdim()`](../reference/mdim.md) writes them.

- [`write_mdim()`](../reference/mdim.md) uses GDAL multidimensional
  array API;

- [`read_mdim()`](../reference/mdim.md) uses arguments `offset`, `count`
  and `step` to read sub-arrays or strided arrays (requires sf \>=
  1.0-9)

## version 0.5-6

CRAN release: 2022-07-21

- export [`read_mdim()`](../reference/mdim.md), a reader using GDAL’s
  multidimensional array API (for sf \<= 1.0-8)

- remove `tos_O1_2001-2002.nc` from packaged datasets to keep source
  package size below 5 Mb

- `as.POSIXct.stars()` converts `PCICt` dimensions to `POSIXct` values.

- improve handling of `PCICt` 360 or 365 day calendars; read them in
  `read_mdim` (requires sf \>= 1.0-9)

- [`read_stars()`](../reference/read_stars.md) reads factor levels
  better from attribute table;
  [\#484](https://github.com/r-spatial/stars/issues/484) thanks to
  [@ailich](https://github.com/ailich)

- [`read_stars()`](../reference/read_stars.md) puts band names from
  `band_meta` DESCRIPTION= tags into `values`;

- improve handling of categorical rasters, and their exchange with
  `terra`; [\#484](https://github.com/r-spatial/stars/issues/484)

- [`plot()`](../reference/plot.md) handles auto colors better for factor
  arrays

- [`read_ncdf()`](../reference/read_ncdf.md) handles units more formally
  in setting crs; [\#533](https://github.com/r-spatial/stars/issues/533)

- print message that dimensions of proxy objects do not reflect
  unevaluated operations;
  [\#530](https://github.com/r-spatial/stars/issues/530)

- passing `na.action = na.omit` to
  [`geom_stars()`](../reference/geom_stars.md) removes `NA` values;
  [\#532](https://github.com/r-spatial/stars/issues/532)

- [`read_stars()`](../reference/read_stars.md) detects curvilinear grids
  automatically; [\#513](https://github.com/r-spatial/stars/issues/513)

- [`st_warp()`](../reference/st_warp.md) warps curvilinear grids (using
  Euclidean distances only on coordinates);
  [\#513](https://github.com/r-spatial/stars/issues/513)

- [`Ops.stars()`](../reference/ops_stars.md) errors when (common)
  dimension are not identical;
  [\#506](https://github.com/r-spatial/stars/issues/506)

- `guess_raster()` accepts empty rows/columns and sparse grids;
  [\#509](https://github.com/r-spatial/stars/issues/509)

- speed up `rgb` plotting;
  [\#503](https://github.com/r-spatial/stars/issues/503)

- Added a new helper function [`st_tile()`](../reference/st_tile.md) to
  specify the block parameters (`nXOff`, `nYOff`, `nXsize`, `nYSize`)
  required by `RasterIO` argument in
  [`read_stars()`](../reference/read_stars.md);
  [\#492](https://github.com/r-spatial/stars/issues/492) thanks to
  Krzysztof Dyba

## version 0.5-5

CRAN release: 2021-12-19

- [`st_as_stars.bbox()`](../reference/st_as_stars.md) creates an empy
  raster file if `proxy = TRUE`;
  [\#489](https://github.com/r-spatial/stars/issues/489)

- [`st_rasterize()`](../reference/st_rasterize.md) has option
  `align = TRUE` to use a template for aligning the new raster to;
  [\#489](https://github.com/r-spatial/stars/issues/489)

- `adrop.stars()` with missing dimensions no longer drops x/y raster
  dimensions; [\#485](https://github.com/r-spatial/stars/issues/485)

- [`aggregate.stars()`](../reference/aggregate.stars.md) propagates
  units of arrays;
  [\#477](https://github.com/r-spatial/stars/issues/477)

## version 0.5-4

CRAN release: 2021-11-19

- [`c.stars()`](../reference/c.stars.md) fails if it tries to merge
  arrays with different units;
  [\#475](https://github.com/r-spatial/stars/issues/475)

- For NetCDF files, [`read_stars()`](../reference/read_stars.md) uses
  the `long_name` as array name;
  [\#475](https://github.com/r-spatial/stars/issues/475)

- add [`rename()`](https://dplyr.tidyverse.org/reference/rename.html)
  method; [\#470](https://github.com/r-spatial/stars/issues/470)

- refresh CRS of packaged `L7_ETMs.tif`;
  [\#466](https://github.com/r-spatial/stars/issues/466)

- [`as.data.frame.stars()`](../reference/st_coordinates.md) works for
  mixed regular and rectilinear dimension;
  [\#458](https://github.com/r-spatial/stars/issues/458)

- [`plot.stars()`](../reference/plot.md) plots curvilinear rasters with
  color table, or without table but `col` argument passed;
  [\#456](https://github.com/r-spatial/stars/issues/456)

- [`st_extract()`](../reference/st_extract.md) accepts a matrix with
  points as `at` argument, for when performance is important; see
  e.g. <https://github.com/rspatial/terra/issues/341>

- fix bug in [`st_crop()`](../reference/st_crop.md) when cropping area
  is larger than grid;
  [\#455](https://github.com/r-spatial/stars/issues/455)

- export [`st_downsample()`](../reference/st_downsample.md), e.g. to be
  used by `tmap`; <https://github.com/r-tmap/tmap/issues/597>

- argument `downsample` in [`plot.stars()`](../reference/plot.md) and
  [`st_as_stars.stars_proxy()`](../reference/st_as_stars.md) and
  [`st_downsample()`](../reference/st_downsample.md) has the same effect
  (removed a one-offset between them).

- [`st_redimension()`](../reference/redimension.md) works for
  curvilinear grids;
  [\#441](https://github.com/r-spatial/stars/issues/441)

- `downsample` is propagated to subexpressions like `r[r < 50] = NA`

- [`predict.stars()`](../reference/predict.stars.md) obtains an argument
  `drop_dimensions` that, if `TRUE`, drops dimensions from the
  prediction `data.frame`;
  [\#362](https://github.com/r-spatial/stars/issues/362)

- extend options in [`st_rgb()`](../reference/st_rgb.md),
  [\#432](https://github.com/r-spatial/stars/issues/432), by Gabo Gaona

- allow subsetting with `[` by using labels, e.g. of band names.

## version 0.5-3

CRAN release: 2021-06-08

- [`read_stars()`](../reference/read_stars.md) accepts a function (or
  list with functions) as first argument, allowing for saving `stars`
  objects that read from package directories resolving
  platform-dependent paths at run-time

- handle categorical rasters starting at value 0 (by adding 1, and
  warning); [\#428](https://github.com/r-spatial/stars/issues/428)

- add `%in%` method;
  [\#424](https://github.com/r-spatial/stars/issues/424)

- [`read_stars()`](../reference/read_stars.md) gains an argument
  `tolerance` to control tolerance in dimension value comparisons;
  [\#414](https://github.com/r-spatial/stars/issues/414)

- binary Ops (like `+`, `-`, `*` etc.) work for `stars_proxy` objects;
  [\#390](https://github.com/r-spatial/stars/issues/390)

- [`st_rasterize()`](../reference/st_rasterize.md) rasterizes multiple
  attributes, and handles factors (when sf \>= 0.9-9)

- [`write_stars()`](../reference/write_stars.md) deals better with
  `stars_proxy` objects;
  [\#404](https://github.com/r-spatial/stars/issues/404)

- fix regression in reading some `stars_proxy` objects;
  [\#379](https://github.com/r-spatial/stars/issues/379)

- add `[<-` (partially) and `is.na` methods for `stars_proxy` objects;
  [\#402](https://github.com/r-spatial/stars/issues/402)

- add `replace_na()` methods;
  [\#402](https://github.com/r-spatial/stars/issues/402)

## version 0.5-2

CRAN release: 2021-03-17

- read and write factor levels as GDAL category names; write color
  table; [\#392](https://github.com/r-spatial/stars/issues/392)

- handle `normalize_path` for choosing to `proxy`;
  [\#391](https://github.com/r-spatial/stars/issues/391)

- ignore units when there are different units across bands of a
  subdataset

- speed up [`st_rgb()`](../reference/st_rgb.md) using faster
  [`st_apply()`](../reference/st_apply.md) approach;
  [\#315](https://github.com/r-spatial/stars/issues/315),
  [\#390](https://github.com/r-spatial/stars/issues/390)

- improve handling of crs in Spatial objects (avoid loss of wkt
  comments)

- correctly write band subsets for smaller proxy objects;
  [\#291](https://github.com/r-spatial/stars/issues/291)

- write arbitrarily cropped proxy objects;
  [\#291](https://github.com/r-spatial/stars/issues/291)

- speed up [`st_apply()`](../reference/st_apply.md) when a function is
  provided that works on chunks at a time;
  [\#390](https://github.com/r-spatial/stars/issues/390)

- warn when breaks = “quantile” results in a single class;
  [\#388](https://github.com/r-spatial/stars/issues/388)

- fix `[` bug selecting bands in proxy objects;
  [\#388](https://github.com/r-spatial/stars/issues/388)

- for `stars_proxy` objects,
  [`write_stars()`](../reference/write_stars.md) writes all objects into
  a multi-layer file;
  [\#385](https://github.com/r-spatial/stars/issues/385)

- multi-file proxy objects can be
  [`st_warp()`](../reference/st_warp.md)ed with `use_gdal = TRUE`;
  [\#385](https://github.com/r-spatial/stars/issues/385)

## version 0.5-1

CRAN release: 2021-01-25

- fix weird GDAL-related bug in stars2 vignette

- [`read_ncdf()`](../reference/read_ncdf.md) does not take time as
  mid-points of regular intervals, but as starting points;
  [\#378](https://github.com/r-spatial/stars/issues/378)

## version 0.5-0

CRAN release: 2021-01-19

- fix handling of rasters with color tables;
  [\#375](https://github.com/r-spatial/stars/issues/375)

- [`st_apply()`](../reference/st_apply.md) and other methods for
  `stars_proxy` objects handle … ;
  [\#374](https://github.com/r-spatial/stars/issues/374)

- add
  [`st_bbox()`](https://r-spatial.github.io/sf/reference/st_bbox.html),
  [`st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
  methods for terra’s `SpatVector` objects;
  <https://github.com/r-tmap/tmap/issues/536>

- add
  [`st_bbox()`](https://r-spatial.github.io/sf/reference/st_bbox.html),
  [`st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html) and
  [`st_as_stars()`](../reference/st_as_stars.md) methods for terra’s
  `SpatRaster` objects; <https://github.com/r-tmap/tmap/issues/536>

- allow for multi-resolution attributes in `stars_proxy` objects (e.g.,
  all gray scale sentinel-2 bands); see vignettes 2 and 7 for examples.

- [`plot()`](../reference/plot.md) defaults to a categorical color scale
  when plotting a factor variable;
  <https://github.com/r-tmap/tmap/issues/526>

- [`st_extract()`](../reference/st_extract.md) extracts space-time
  points if `time_column` is specified, and handles time intervals;
  [\#352](https://github.com/r-spatial/stars/issues/352)

- add `[[<-.stars` method, which is now called by `$<-.stars`, so that
  array names can be set programmatically

- add
  [`transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
  methods

- [`plot.stars()`](../reference/plot.md) calls `droplevels` if a factor
  array has any `NA` levels;
  [\#339](https://github.com/r-spatial/stars/issues/339)

- [`read_stars()`](../reference/read_stars.md) reads `NaN`s as `NA`;
  [\#333](https://github.com/r-spatial/stars/issues/333)

- improve [`st_extract()`](../reference/st_extract.md) method for both
  `stars` and `stars_proxy` objects; interpolation options are reduced
  to bilinear; [\#322](https://github.com/r-spatial/stars/issues/322),
  [\#279](https://github.com/r-spatial/stars/issues/279),
  [\#290](https://github.com/r-spatial/stars/issues/290)

- better handle categorical rasters that do not start at value 1;
  [\#329](https://github.com/r-spatial/stars/issues/329)

- plot layout can be controlled with `mfrow = c(nr, nc)` argument

- `stars_proxy` objects have a normalized path;
  [\#331](https://github.com/r-spatial/stars/issues/331)

- cropping or selecting with `bbox` treats cells always as small
  polygons; [\#330](https://github.com/r-spatial/stars/issues/330)

- add faster [`st_extract()`](../reference/st_extract.md) method for
  `stars` objects;
  [\#322](https://github.com/r-spatial/stars/issues/322)

- added vignette: “How `raster` functions map to `stars` functions”, by
  Sebastien Rochette;
  [\#122](https://github.com/r-spatial/stars/issues/122),
  [\#325](https://github.com/r-spatial/stars/issues/325)

- fix bug in dimension `values` field when downsampling;
  [\#324](https://github.com/r-spatial/stars/issues/324)

- [`write_stars()`](../reference/write_stars.md) also writes out band
  names; [\#323](https://github.com/r-spatial/stars/issues/323)

- add `rgdal` to Suggests:

- each `call_list` entry of a `stars_proxy` object carries its proper
  calling environment;
  [\#309](https://github.com/r-spatial/stars/issues/309)

- [`st_as_sf.stars()`](../reference/st_as_sf.md) copes with zero
  attribute (empty) stars objects

- add [`st_set_bbox()`](../reference/st_set_bbox.md) generic, to set
  raster extent, motivated by
  [\#315](https://github.com/r-spatial/stars/issues/315)

- set up tic, with great help from [@pat-s](https://github.com/pat-s),
  [\#313](https://github.com/r-spatial/stars/issues/313)

- get rid of more `proj4string`s for representing coordinate reference
  systems; [\#312](https://github.com/r-spatial/stars/issues/312)

- as(x, “Spatial”) correctly handles `from` dimension values different
  from one

- [`read_stars()`](../reference/read_stars.md) now sets the `BANDNAME`
  GDAL metadata item, or else the band’s GetDescription() as the band’s
  dimension values

- [`st_as_stars.data.frame()`](../reference/st_as_stars.md) reads simple
  tables (non-raster data) if `dims` has length less than 2

- band descriptions are in the band dimension values

- dimension tables are simpler, and are shown properly in Rstudio

- [`st_rgb()`](../reference/st_rgb.md) gains a `probs` argument, to cut
  off and stretch based on quantiles

- `as(x, "Raster")` merges multiple attributes before converting to
  raster brick

## version 0.4-3

CRAN release: 2020-07-08

- fix bug in `st_as_stars.Raster`; set crs to the one assigned by
  raster; <https://github.com/r-tmap/tmap/issues/471>

- add `s2` to Suggests:

- new function [`st_rgb()`](../reference/st_rgb.md) collapses (reduces)
  a dimension to rgb hex value;
  [\#302](https://github.com/r-spatial/stars/issues/302)

## version 0.4-2

CRAN release: 2020-07-01

- [`aggregate.stars()`](../reference/aggregate.stars.md) handles arrays
  with NA values now correctly; brought up in
  [\#299](https://github.com/r-spatial/stars/issues/299) by Thorsten
  Simon

- [`aggregate.stars()`](../reference/aggregate.stars.md) gains an
  argument `exact` which, if `TRUE`, calls `exactextractr` for polygonal
  aggregation; [\#289](https://github.com/r-spatial/stars/issues/289)

- [`read_stars()`](../reference/read_stars.md) reads all subdatasets
  with dimensions equal to first, and warns when ignoring others;
  [\#296](https://github.com/r-spatial/stars/issues/296)

- make copying over of dimensions somewhat easier;
  [\#295](https://github.com/r-spatial/stars/issues/295)

- [`st_as_stars.Raster()`](../reference/st_as_stars.md) tries to read
  from file if the raster object is not an in-memory object.

- [`write_stars()`](../reference/write_stars.md) normalizes path, as
  `read_stars` already did;
  [\#293](https://github.com/r-spatial/stars/issues/293)

- [`merge()`](../reference/merge.md) for proxy objects acts, and is no
  longer lazy; [\#290](https://github.com/r-spatial/stars/issues/290)

- [`st_as_stars.Raster()`](../reference/st_as_stars.md) returns a proxy
  object if the raster layer is on disk

- add [`st_extract()`](../reference/st_extract.md) to extract e.g. time
  series from grids at point locations;
  [\#279](https://github.com/r-spatial/stars/issues/279);
  [\#290](https://github.com/r-spatial/stars/issues/290)

- [`read_stars()`](../reference/read_stars.md) chooses a value for
  `proxy` that depends on the data dimensions;
  [\#281](https://github.com/r-spatial/stars/issues/281)

- x/y range subsetting of `stars_proxy` objects now only reads that
  range, similar to how crop already did this.

- [`st_warp()`](../reference/st_warp.md) preserves levels and colors;
  <https://github.com/r-tmap/tmap/issues/429>

- [`st_crop()`](../reference/st_crop.md) works with bounding boxes
  larger than the downsampled bounding box;
  [\#276](https://github.com/r-spatial/stars/issues/276)

- [`st_crop()`](../reference/st_crop.md) has a non-zero default for
  `epsilon` (bounding box shrinkage) to exclude cells touching the crop
  bounding box; [\#275](https://github.com/r-spatial/stars/issues/275)

- [`image.stars()`](../reference/plot.md) (and hence `plot.stars`) gains
  an `extent` argument for setting the extent of a plot;
  <https://github.com/r-spatial/sf/issues/1193>

## version 0.4-1

CRAN release: 2020-04-07

- [`st_warp()`](../reference/st_warp.md) (stars native) flips longitudes
  a full cycle; [\#256](https://github.com/r-spatial/stars/issues/256),
  [\#264](https://github.com/r-spatial/stars/issues/264),
  [\#269](https://github.com/r-spatial/stars/issues/269)

- handle axis order in `st_transform` (requires sf \>= 0.9-1)

- depend on sf 0.9-0

- adapt to cubelyr split-off from dplyr; add cubelyr to Suggests:;
  <https://github.com/hadley/cubelyr/issues/2>

- add [`droplevels()`](https://rdrr.io/r/base/droplevels.html) method

- handle color tables, category tables and raster attribute tables read
  through GDAL; [\#128](https://github.com/r-spatial/stars/issues/128),
  [\#245](https://github.com/r-spatial/stars/issues/245);
  <https://github.com/r-spatial/mapview/issues/208>

- handle dimension `crs` specially, for proxy objects now.

- handle new-style `crs` objects from upcoming sf, moving away from
  proj4strings

- handle full `crs` objects as `refsys` element in a spatial dimensions,
  rather than proj4string only

- `st_raster_type(x)` reveals the raster type of `x`;
  [\#248](https://github.com/r-spatial/stars/issues/248),
  <https://github.com/r-tmap/tmap/issues/368>

- add [`st_as_stars.OpenStreetMap()`](../reference/st_as_stars.md)
  method; [\#241](https://github.com/r-spatial/stars/issues/241) by
  [@mtennekes](https://github.com/mtennekes)

- add [`st_flip()`](../reference/stars_subset.md) to flip arrays along
  one or more dimensions without changing dimension properties

- add `as.owin()` method to convert (2D raster) stars objects to
  spatstat `owin`; <https://github.com/r-spatial/sf/issues/1233>

- for temporal aggregation, `aggregate.stars` now also takes `by`
  arguments like “week”, “month”, or “5 days”

- add [`st_as_stars()`](../reference/st_as_stars.md) method for `xts`
  objects; improve `as.xts` for `stars` objects

- skip some tests on solaris

## version 0.4-0

CRAN release: 2019-10-10

- [`plot()`](../reference/plot.md) now uses all data to figure out
  breaks, in order to also find extremes;
  [\#216](https://github.com/r-spatial/stars/issues/216)

- [`st_mosaic()`](../reference/st_mosaic.md) creates mosaics from
  spatially disjoint rasters;
  [\#210](https://github.com/r-spatial/stars/issues/210)

- [\#205](https://github.com/r-spatial/stars/issues/205) large
  refactoring of `read_ncdf`, by David Blodgett and Mike Sumner,
  affecting [\#199](https://github.com/r-spatial/stars/issues/199),
  [\#89](https://github.com/r-spatial/stars/issues/89),
  [\#30](https://github.com/r-spatial/stars/issues/30),
  [\#86](https://github.com/r-spatial/stars/issues/86),
  [\#175](https://github.com/r-spatial/stars/issues/175)

- allow for funny units like `m s**-1`;
  [\#201](https://github.com/r-spatial/stars/issues/201)

- add [`contour()`](https://rdrr.io/r/graphics/contour.html) method for
  `stars` objects;
  [\#196](https://github.com/r-spatial/stars/issues/196)

- plot uses `rasterImage` by default if available;
  [\#194](https://github.com/r-spatial/stars/issues/194)

- the `x` and `y` raster dimensions can be set and unset with `xy`
  argument in `st_set_dimensions`;
  [\#190](https://github.com/r-spatial/stars/issues/190)

- retain `factor` levels with dimension values when set in
  `st_set_dimensions`;
  [\#188](https://github.com/r-spatial/stars/issues/188)

- add conversion from `stars_proxy` to `Raster`:
  [\#193](https://github.com/r-spatial/stars/issues/193)

- make plotting of multiple curvilinear grids work

- plot by default no cell borders in case of curvilinear, rotated or
  sheared grids

- robustify handling of units

- allow [`read_ncdf()`](../reference/read_ncdf.md) to ignore bounds

- scale was applied wrongly on multi-band images;
  [\#189](https://github.com/r-spatial/stars/issues/189), this requires
  sf \>= 0.7-5

- `.nc` is now recognized correctly by `write_stars` and will write a
  NetCDF file; [\#186](https://github.com/r-spatial/stars/issues/186)

- `[` subset now works correctly with negative or logical indices;
  [\#184](https://github.com/r-spatial/stars/issues/184),
  [\#185](https://github.com/r-spatial/stars/issues/185)

- `NA` values for float32 grids are now correctly detected;
  [\#182](https://github.com/r-spatial/stars/issues/182), this requires
  sf \>= 0.7-5

- cropping of a `stars_proxy` object now works;
  [\#179](https://github.com/r-spatial/stars/issues/179)

- [`st_apply()`](../reference/st_apply.md) can now loop over Raster
  layers; examples in
  [\#176](https://github.com/r-spatial/stars/issues/176)

## version 0.3-1

CRAN release: 2019-04-23

- `st_as_stars.bbox` now has an `ncells` and `pretty` argument, to
  better choose default raster dimensions

- `geom_stars` now works with `stars_proxy` objects, but needs
  `downsample` to be set;
  [\#21](https://github.com/r-spatial/stars/issues/21)

- `NA` values in Float32 rasters are now read correctly with
  `read_stars`; [\#182](https://github.com/r-spatial/stars/issues/182)

- handle bounds, when given, in `read_ncdf`

- provide time parsing (POSIXct, PCICt) for `read_ncdf`;
  [\#115](https://github.com/r-spatial/stars/issues/115)

## version 0.3-0

CRAN release: 2019-02-24

- add `st_area` method to return raster grid cell sizes;
  [\#99](https://github.com/r-spatial/stars/issues/99)

- fix `st_warp` with `use_gdal=TRUE`, allowing for multi-band warps

- add `st_get_dimension_values` to get the values of a particular
  dimension (if meaningful);
  [\#100](https://github.com/r-spatial/stars/issues/100)

- allow for setting intervals as dimension values; see examples of
  `st_dimensions`

- add `st_contour`, and clean up `st_as_sf`;
  [\#99](https://github.com/r-spatial/stars/issues/99)

- experimental color table support;
  <https://github.com/r-spatial/mapview/issues/208>

- rewrote vignettes, added vignettes;
  [\#99](https://github.com/r-spatial/stars/issues/99)

- deprecate `st_write.stars` for `write_stars`;
  [\#96](https://github.com/r-spatial/stars/issues/96)

- use “native” R array-factor support

- support for `PCICt` 360- and 365-day calendars;
  [\#29](https://github.com/r-spatial/stars/issues/29)

- remove import of `ncdf4` in favor of `RNetCDF`, now in line with
  practice in `ncmeta` package. Thanks to David Blodgett for motivation
  and testing (see [\#87](https://github.com/r-spatial/stars/issues/87),
  [\#94](https://github.com/r-spatial/stars/issues/94)).

- `st_as_sf` uses date/time column names when appropriate

- allow missing trailing comma’s when subsetting: `a[1,,]` and `a[1,]`
  now do the same.

- move `rlang` to Imports: ; rewrite `[` subset using rlang.

- add conversion to and from `Spatial*` classes, including the gridded
  ones, taking care of `factor` variables

- depend on sf 0.7-2

- add `logz` support for log-scale keys to `plot` and `image`

## version 0.2-0

CRAN release: 2018-10-25

- vignettes now use an external package, `starsdata`, for larger dataset
  examples

- support `[<-.stars` e.g. to mask out values; support `is.na.stars` to
  replace NA masks

- support `cut` methods and factor arrays (plot, subset);
  [\#56](https://github.com/r-spatial/stars/issues/56)

- add `st_rasterize`, which uses `GDALRasterize` to rasterize an sf
  object; [\#13](https://github.com/r-spatial/stars/issues/13)

- `st_as_sf.stars` now uses `GDAL(F)Polygonize` when give a regular or
  sheared grid grid cells are not points, and returns contour bands
  using `GDALContourGenerateEx` (requiring GDAL 2.4.0) in case cells are
  points; [\#13](https://github.com/r-spatial/stars/issues/13)

- support curvilinear grids; see
  [\#54](https://github.com/r-spatial/stars/issues/54) and the
  `data_model` vignette

- add vignette about how `stars_proxy` objects work

- `stars_proxy` objects defer processing of `st_apply` jobs until after
  subsampling; [\#50](https://github.com/r-spatial/stars/issues/50)

- allow reading sections of a raster, raster at a lower resolution,
  selected bands; [\#48](https://github.com/r-spatial/stars/issues/48)

- allow reading vectors (arrays) with more than 2^31 elements;
  [\#48](https://github.com/r-spatial/stars/issues/48)

- fold all higher dimensions into the third dimension before coercing to
  `Raster`; [\#40](https://github.com/r-spatial/stars/issues/40)

## version 0.1-1

CRAN release: 2018-07-25

- add meta data reader to `read_stars`

## version 0.1-0

- add `merge` (merge attributes into array dimension) and `split` (split
  dimension over attributes)

- interface to sf, raster and spacetime

- improve plotting

- handle `logical` arrays in plot

- add `st_apply`, analogous to `apply`

- add cropping/masking when used as x\[buf\] with buf an `sf`, `sfc` or
  `bbox` object; masking when `x[buf, crop = FALSE]`

- add Ops (+,-,/ etc) and Math (sqrt, pow, sin etc) methods

- add `dimnames` and `dimnames<-` methods for stars objects

- downsample large grids to only plot pixels actually shown

- can plot rectilinear grids (but will plot rgb images as regular grids)

- `rgb` argument to `image` works

- `[` array-like subsetting works; first index is attribute selector

## version 0.0

- interface the 9 C++ gdal utils through
  [`gdal_utils()`](https://r-spatial.github.io/sf/reference/gdal_utils.html)
  (now part of `sf`)
