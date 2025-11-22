# st_apply apply a function to one or more array dimensions

st_apply apply a function to array dimensions: aggregate over space,
time, or something else

## Usage

``` r
# S3 method for class 'stars'
st_apply(
  X,
  MARGIN,
  FUN,
  ...,
  CLUSTER = NULL,
  PROGRESS = FALSE,
  FUTURE = FALSE,
  rename = TRUE,
  .fname,
  single_arg = has_single_arg(FUN, list(...)) || can_single_arg(FUN),
  keep = FALSE
)
```

## Arguments

- X:

  object of class `stars`

- MARGIN:

  see [apply](https://rdrr.io/r/base/apply.html); index number(s) or
  name(s) of the dimensions over which `FUN` will be applied

- FUN:

  see [apply](https://rdrr.io/r/base/apply.html) and see Details.

- ...:

  arguments passed on to `FUN`

- CLUSTER:

  cluster to use for parallel apply; see
  [makeCluster](https://rdrr.io/r/parallel/makeCluster.html)

- PROGRESS:

  logical; if `TRUE`, use
  [`pbapply::pbapply`](https://peter.solymos.org/pbapply/reference/pbapply.html)
  to show progress bar

- FUTURE:

  logical;if `TRUE`, use
  [`future.apply::future_apply`](https://future.apply.futureverse.org/reference/future_apply.html)

- rename:

  logical; if `TRUE` and `X` has only one attribute and `FUN` is a
  simple function name, rename the attribute of the returned object to
  the function name

- .fname:

  function name for the new attribute name (if one or more dimensions
  are reduced) or the new dimension (if a new dimension is created); if
  missing, the name of `FUN` is used

- single_arg:

  logical; if `TRUE`, FUN takes a single argument (like `fn_ndvi1`
  below), if `FALSE` FUN takes multiple arguments (like `fn_ndvi2`
  below).

- keep:

  logical; if `TRUE`, preserve dimension metadata (e.g. time stamps)

## Value

object of class `stars` with accordingly reduced number of dimensions;
in case `FUN` returns more than one value, a new dimension is created
carrying the name of the function used; see the examples. Following the
logic of [apply](https://rdrr.io/r/base/apply.html), This new dimension
is put before the other dimensions; use
[aperm](https://rdrr.io/r/base/aperm.html) to rearrange this, see last
example.

## Details

FUN is a function which either operates on a single object, which will
be the data of each iteration step over dimensions MARGIN, or a function
that has as many arguments as there are elements in such an object. See
the NDVI examples below. The second form can be VERY much faster e.g.
when a trivial function is not being called for every pixel, but only
once (example).

The heuristics for the default of `single_arg` work often, but not
always; try setting this to the right value when `st_apply` gives an
error.

## Examples

``` r
tif = system.file("tif/L7_ETMs.tif", package = "stars")
x = read_stars(tif)
st_apply(x, 1:2, mean) # mean band value for each pixel
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>       Min.  1st Qu.   Median     Mean 3rd Qu. Max.
#> mean  25.5 53.33333 68.33333 68.91242      82  255
#> dimension(s):
#>   from  to  offset delta                     refsys point x/y
#> x    1 349  288776  28.5 SIRGAS 2000 / UTM zone 25S FALSE [x]
#> y    1 352 9120761 -28.5 SIRGAS 2000 / UTM zone 25S FALSE [y]
st_apply(x, c("x", "y"), mean) # equivalent to the above
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>       Min.  1st Qu.   Median     Mean 3rd Qu. Max.
#> mean  25.5 53.33333 68.33333 68.91242      82  255
#> dimension(s):
#>   from  to  offset delta                     refsys point x/y
#> x    1 349  288776  28.5 SIRGAS 2000 / UTM zone 25S FALSE [x]
#> y    1 352 9120761 -28.5 SIRGAS 2000 / UTM zone 25S FALSE [y]
st_apply(x, 3, mean)   # mean of all pixels for each band
#> stars object with 1 dimensions and 1 attribute
#> attribute(s):
#>           Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#> mean  59.23541 61.07112 65.96675 68.91242 76.25445 83.18266
#> dimension(s):
#>      from to
#> band    1  6
if (FALSE) { # \dontrun{
 st_apply(x, "band", mean) # equivalent to the above
 st_apply(x, 1:2, range) # min and max band value for each pixel
 fn_ndvi1 = function(x) (x[4]-x[3])/(x[4]+x[3]) # ONE argument: will be called for each pixel
 fn_ndvi2 = function(red,nir) (nir-red)/(nir+red) # n arguments: will be called only once
 ndvi1 = st_apply(x, 1:2, fn_ndvi1)
   # note that we can select bands 3 and 4 in the first argument:
 ndvi2 = st_apply(x[,,,3:4], 1:2, fn_ndvi2) 
 all.equal(ndvi1, ndvi2)
 # compute the (spatial) variance of each band; https://github.com/r-spatial/stars/issues/430
 st_apply(x, 3, function(x) var(as.vector(x))) # as.vector is required!
 # to get a progress bar also in non-interactive mode, specify:
 if (require(pbapply)) { # install it, if FALSE
   pboptions(type = "timer")
 }
 st_apply(x, 1:2, range) # dimension "range" is first; rearrange by:
 st_apply(x, 1:2, range) %>% aperm(c(2,3,1))
} # }
```
