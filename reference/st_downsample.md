# downsample stars or stars_proxy objects

downsample a stars or stars_proxy object either by skipping rows,
columns and bands, or by computing a single value (e.g. the mean) from
the sub-tiles involved

## Usage

``` r
st_downsample(x, n, ...)

# S3 method for class 'stars'
st_downsample(x, n, ..., offset = 0, FUN)

# S3 method for class 'stars_proxy'
st_downsample(x, n, ...)
```

## Arguments

- x:

  object of class stars or stars_proxy

- n:

  integer; for each dimension the number of pixels/lines/bands etc that
  will be skipped; see Details.

- ...:

  arguments passed on to `FUN` (e.g., `na.rm = TRUE` to ignore missing
  values if FUN is `mean`)

- offset:

  integer; offset(s) for downsampling, in pixels, starting at the offset
  of each dimension; should be smaller or equal to `n`

- FUN:

  function; if given, downsampling will apply FUN to each of the the
  subtiles

## Details

If all n == 0, no downsampling takes place; if it is 1, every second
row/column/band is skipped, if it is 2, every second+third
row/column/band are skipped, etc.

Downsampling a `stars_proxy` object returns a `stars` object, is
equivalent to calling `st_as_stars(x, downsample = 2)`, and only
downsamples the first two (x and y) dimensions.

Downsampled regular rasters keep their dimension offsets, have a cell
size (delta) that is n\[i\]+1 times larger, and may result in a
(slightly) different extent.

Note that terra's
[aggregate](https://rspatial.github.io/terra/reference/aggregate.html)
with `fact=2` corresponds to `st_downsample(x, n = 1, FUN = mean)`:
`fact` is one larger than `n`.

## Examples

``` r
(m = matrix(1:121, 11, 11))
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11]
#>  [1,]    1   12   23   34   45   56   67   78   89   100   111
#>  [2,]    2   13   24   35   46   57   68   79   90   101   112
#>  [3,]    3   14   25   36   47   58   69   80   91   102   113
#>  [4,]    4   15   26   37   48   59   70   81   92   103   114
#>  [5,]    5   16   27   38   49   60   71   82   93   104   115
#>  [6,]    6   17   28   39   50   61   72   83   94   105   116
#>  [7,]    7   18   29   40   51   62   73   84   95   106   117
#>  [8,]    8   19   30   41   52   63   74   85   96   107   118
#>  [9,]    9   20   31   42   53   64   75   86   97   108   119
#> [10,]   10   21   32   43   54   65   76   87   98   109   120
#> [11,]   11   22   33   44   55   66   77   88   99   110   121
(s = st_as_stars(m))
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>     Min. 1st Qu. Median Mean 3rd Qu. Max.
#> A1     1      31     61   61      91  121
#> dimension(s):
#>    from to offset delta point x/y
#> X1    1 11      0     1 FALSE [x]
#> X2    1 11      0     1 FALSE [y]
st_downsample(s, 1)
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>     Min. 1st Qu. Median Mean 3rd Qu. Max.
#> A1     1    28.5     61   61    93.5  121
#> dimension(s):
#>    from to offset delta point x/y
#> X1    1  6      0     2 FALSE [x]
#> X2    1  6      0     2 FALSE [y]
st_downsample(s, 1)[[1]]
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,]    1   23   45   67   89  111
#> [2,]    3   25   47   69   91  113
#> [3,]    5   27   49   71   93  115
#> [4,]    7   29   51   73   95  117
#> [5,]    9   31   53   75   97  119
#> [6,]   11   33   55   77   99  121
st_downsample(s, 1, offset = 1)
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>     Min. 1st Qu. Median Mean 3rd Qu. Max.
#> A1    13      37     61   61      85  109
#> dimension(s):
#>    from to offset delta point x/y
#> X1    1  5      1     2 FALSE [x]
#> X2    1  5      1     2 FALSE [y]
st_downsample(s, 1, offset = 1)[[1]]
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   13   35   57   79  101
#> [2,]   15   37   59   81  103
#> [3,]   17   39   61   83  105
#> [4,]   19   41   63   85  107
#> [5,]   21   43   65   87  109
st_downsample(s, 1, offset = c(0,1))
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>     Min. 1st Qu. Median Mean 3rd Qu. Max.
#> A1    12    36.5     61   61    85.5  110
#> dimension(s):
#>    from to offset delta point x/y
#> X1    1  6      0     2 FALSE [x]
#> X2    1  5      1     2 FALSE [y]
st_downsample(s, 1, offset = c(0,1))[[1]]
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   12   34   56   78  100
#> [2,]   14   36   58   80  102
#> [3,]   16   38   60   82  104
#> [4,]   18   40   62   84  106
#> [5,]   20   42   64   86  108
#> [6,]   22   44   66   88  110
st_downsample(s, 1, FUN = mean)
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>     Min. 1st Qu. Median Mean 3rd Qu. Max.
#> A1     7      31     55   55      79  103
#> dimension(s):
#>    from to offset delta point x/y
#> X1    1  5      0     2 FALSE [x]
#> X2    1  5      0     2 FALSE [y]
st_downsample(s, 1, FUN = mean)[[1]]
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    7   29   51   73   95
#> [2,]    9   31   53   75   97
#> [3,]   11   33   55   77   99
#> [4,]   13   35   57   79  101
#> [5,]   15   37   59   81  103
st_downsample(s, 1, offset = 1, FUN = mean)
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>     Min. 1st Qu. Median Mean 3rd Qu. Max.
#> A1    19      43     67   67      91  115
#> dimension(s):
#>    from to offset delta point x/y
#> X1    1  5      1     2 FALSE [x]
#> X2    1  5      1     2 FALSE [y]
st_downsample(s, 1, offset = c(0,1), FUN = mean)[[1]]
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   18   40   62   84  106
#> [2,]   20   42   64   86  108
#> [3,]   22   44   66   88  110
#> [4,]   24   46   68   90  112
#> [5,]   26   48   70   92  114
```
