# Package index

## All functions

- [`L7_ETMs`](L7_ETMs.md) : Landsat-7 bands for a selected region around
  Olinda, BR
- [`aggregate(`*`<stars>`*`)`](aggregate.stars.md) : spatially or
  temporally aggregate stars object
- [`bcsd_obs`](bcsd_obs.md) : Monthly Gridded Meteorological
  Observations
- [`c(`*`<stars_proxy>`*`)`](c.stars.md)
  [`c(`*`<stars>`*`)`](c.stars.md) : combine multiple stars objects, or
  combine multiple attributes in a single stars object into a single
  array
- [`as`](coerce-methods.md)
  [`coerce,stars,Raster-method`](coerce-methods.md)
  [`coerce,stars_proxy,Raster-method`](coerce-methods.md)
  [`coerce,stars,Terra-method`](coerce-methods.md)
  [`coerce,stars_proxy,Terra-method`](coerce-methods.md) : Coerce stars
  object into a Raster raster or brick
- [`contour(`*`<stars>`*`)`](contour.stars.md) : plot contours of a
  stars object
- [`cut(`*`<array>`*`)`](cut_stars.md)
  [`cut(`*`<matrix>`*`)`](cut_stars.md)
  [`cut(`*`<stars>`*`)`](cut_stars.md) : cut methods for stars objects
- [`filter.stars()`](dplyr.md) [`filter.stars_proxy()`](dplyr.md)
  [`mutate.stars()`](dplyr.md) [`mutate.stars_proxy()`](dplyr.md)
  [`transmute.stars()`](dplyr.md) [`transmute.stars_proxy()`](dplyr.md)
  [`select.stars()`](dplyr.md) [`select.stars_proxy()`](dplyr.md)
  [`rename.stars()`](dplyr.md) [`rename.stars_proxy()`](dplyr.md)
  [`pull.stars()`](dplyr.md) [`pull.stars_proxy()`](dplyr.md)
  [`as.tbl_cube.stars()`](dplyr.md) [`slice.stars()`](dplyr.md)
  [`slice.stars_proxy()`](dplyr.md) [`replace_na.stars()`](dplyr.md)
  [`replace_na.stars_proxy()`](dplyr.md) : dplyr verbs for stars objects
- [`expand_dimensions()`](expand_dimensions.md) : expand the dimension
  values into a list
- [`geom_stars()`](geom_stars.md) [`theme_stars()`](geom_stars.md) :
  ggplot geom for stars objects
- [`` `%in%`( ``*`<stars>`*`)`](in-methods.md) : evaluate whether cube
  values are in a given set
- [`make_intervals()`](make_intervals.md) : create an intervals object
- [`read_mdim()`](mdim.md) [`write_mdim()`](mdim.md) : Read or write
  data using GDAL's multidimensional array API
- [`split(`*`<stars>`*`)`](merge.md) [`merge(`*`<stars>`*`)`](merge.md)
  : merge or split stars object
- [`Ops(`*`<stars>`*`)`](ops_stars.md)
  [`Math(`*`<stars>`*`)`](ops_stars.md)
  [`Ops(`*`<stars_proxy>`*`)`](ops_stars.md)
  [`Math(`*`<stars_proxy>`*`)`](ops_stars.md) : S3 Ops Group Generic
  Functions for stars objects
- [`plot(`*`<nc_proxy>`*`)`](plot.md) [`plot(`*`<stars>`*`)`](plot.md)
  [`image(`*`<stars>`*`)`](plot.md)
  [`plot(`*`<stars_proxy>`*`)`](plot.md) : plot stars object, with
  subplots for each level of first non-spatial dimension
- [`prcomp(`*`<stars_proxy>`*`)`](prcomp.md)
  [`prcomp(`*`<stars>`*`)`](prcomp.md) : Principle components of stars
  object
- [`predict(`*`<stars_proxy>`*`)`](predict.stars.md)
  [`predict(`*`<stars>`*`)`](predict.stars.md) : Predict values, given a
  model object, for a stars or stars_proxy object
- [`as.data.frame(`*`<dimensions>`*`)`](print_stars.md)
  [`print(`*`<dimensions>`*`)`](print_stars.md)
  [`print(`*`<stars>`*`)`](print_stars.md) : print stars or dimensions
  object
- [`read_ncdf()`](read_ncdf.md) : Read NetCDF into stars object
- [`read_stars()`](read_stars.md) : read raster/array dataset from file
  or connection
- [`st_redimension()`](redimension.md) : redimension array, or collapse
  attributes into a new dimension
- [`st_apply(`*`<stars>`*`)`](st_apply.md) : st_apply apply a function
  to one or more array dimensions
- [`st_as_sfc(`*`<stars>`*`)`](st_as_sf.md)
  [`st_as_sf(`*`<stars>`*`)`](st_as_sf.md)
  [`st_as_sf(`*`<stars_proxy>`*`)`](st_as_sf.md) : Convert stars object
  into an sf object
- [`st_as_stars()`](st_as_stars.md) : convert objects into a stars
  object
- [`st_cells()`](st_cells.md) : return the cell index corresponding to
  the location of a set of points
- [`st_contour()`](st_contour.md) : Compute or plot contour lines or
  sets
- [`st_coordinates(`*`<stars>`*`)`](st_coordinates.md)
  [`as.data.frame(`*`<stars>`*`)`](st_coordinates.md)
  [`as_tibble.stars()`](st_coordinates.md) : retrieve coordinates for
  raster or vector cube cells
- [`st_crop(`*`<mdim>`*`)`](st_crop.md)
  [`st_crop(`*`<stars_proxy>`*`)`](st_crop.md)
  [`st_crop(`*`<stars>`*`)`](st_crop.md) : crop a stars object
- [`st_dim_to_attr()`](st_dim_to_attr.md) : create an array with
  dimension values
- [`st_dimensions()`](st_dimensions.md)
  [`` `st_dimensions<-`() ``](st_dimensions.md)
  [`st_set_dimensions()`](st_dimensions.md)
  [`st_get_dimension_values()`](st_dimensions.md) : get dimensions from
  stars object
- [`st_downsample()`](st_downsample.md) : downsample stars or
  stars_proxy objects
- [`st_extract()`](st_extract.md) : Extract cell values at point
  locations
- [`st_geotransform()`](st_geotransform.md)
  [`` `st_geotransform<-`() ``](st_geotransform.md) : get or set the
  geotransform, or rotation matrix
- [`st_intersects(`*`<stars>`*`)`](st_intersects.stars.md) : spatial
  intersect predicate for stars and sfc object
- [`st_join(`*`<stars>`*`)`](st_join.stars.md) : Spatially join a stars
  and an \`sf\` object
- [`st_mosaic()`](st_mosaic.md) : build mosaic (composite) of several
  spatially disjoint stars objects
- [`st_raster_type()`](st_raster_type.md) : get the raster type (if any)
  of a stars object
- [`st_rasterize()`](st_rasterize.md) : rasterize simple feature
  geometries
- [`st_res()`](st_res.md) : obtain (spatial) resolution of a stars
  object
- [`st_rgb()`](st_rgb.md) : reduce dimension to rgb (alpha) hex values
- [`st_rotate(`*`<stars>`*`)`](st_rotate.md)
  [`st_rotate(`*`<sfc>`*`)`](st_rotate.md)
  [`st_rotate(`*`<sf>`*`)`](st_rotate.md) : Transform rotated pole
  long/lat regular grid to unrotated curvilinear grid
- [`st_set_bbox()`](st_set_bbox.md) : set bounding box parameters of
  regular grid
- [`st_tile()`](st_tile.md) : Specify parameters to load raster in
  blocks
- [`st_transform(`*`<stars>`*`)`](st_transform.md)
  [`st_transform_proj.stars()`](st_transform.md) : transform geometries
  in stars objects to a new coordinate reference system, without warping
- [`st_warp()`](st_warp.md) : Warp (resample) grids in stars objects to
  a new grid, possibly in an new coordinate reference system
- [`st_xy2sfc()`](st_xy2sfc.md) [`st_sfc2xy()`](st_xy2sfc.md) : replace
  x y raster dimensions with simple feature geometry list (points, or
  polygons = rasterize) and vice versa
- [`stars_sentinel2`](stars_sentinel2.md) : Sentinel-2 sample tile
- [`` `[<-`( ``*`<stars_proxy>`*`)`](stars_subset.md)
  [`` `[`( ``*`<stars>`*`)`](stars_subset.md)
  [`` `[<-`( ``*`<stars>`*`)`](stars_subset.md)
  [`st_flip()`](stars_subset.md) : subset stars objects
- [`write_stars()`](write_stars.md) [`detect.driver()`](write_stars.md)
  : write stars object to gdal dataset (typically: to file)
