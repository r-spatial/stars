# Specify parameters to load raster in blocks

Helper function for specifying the block parameters (`nXOff`, `nYOff`,
`nXsize`, and `nYSize`) required by `RasterIO` argument in
[read_stars](read_stars.md)

## Usage

``` r
st_tile(img_rows, img_cols, x_window, y_window, overlap = 0)
```

## Arguments

- img_rows:

  number of input raster rows (integer)

- img_cols:

  number of input raster columns (integer)

- x_window:

  number of rows in block (integer)

- y_window:

  number of columns in block (integer)

- overlap:

  number of overlapping pixels (integer)

## Value

matrix with specified `nXOff`, `nYOff`, `nXsize`, and `nYSize`
parameters for every block

## Examples

``` r
if (FALSE) { # \dontrun{
tif = system.file("tif/L7_ETMs.tif", package = "stars")
r = read_stars(tif, proxy = TRUE)
tiles = st_tile(nrow(r), ncol(r), 256, 256)
for (i in seq_len(nrow(tiles))) {
  tile = read_stars(tif, proxy = FALSE, RasterIO = tiles[i, ])
  # write tiles to separate files
  write_stars(tile, dsn = paste0(i, ".tif"))
}
} # }
```
