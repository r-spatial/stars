# spatial intersect predicate for stars and sfc object

spatial intersect predicate for stars and sfc object

## Usage

``` r
# S3 method for class 'stars'
st_intersects(x, y, sparse = TRUE, ..., as_points = NA, transpose = FALSE)
```

## Arguments

- x:

  object of class stars

- y:

  object that has an \`st_geometry\` method: of class \`sf\` or \`sfc\`,
  or \`stars\` object with an \`sfc\` dimension

- sparse:

  logical; if TRUE, return the a sparse logical matrix (object of class
  \`sgbp\`), if FALSE, return a logical matrix

- ...:

  ignored, or passed on to \`st_intersects.sf\` for curvilinear grids

- as_points:

  logical, should grid cells be considered as points (TRUE) or polygons
  (FALSE)? Default: FALSE and warning emitted

- transpose:

  logical; should the transpose of the \`sgbp\` object be returned?

## Value

\`sgbp\` object if sparse = TRUE, logical matrix otherwise

## Details

curvilinear grids are always converted to polygons, so points on grid
boundaries may intersect with two cells touched; for other grids each
cell boundary or corner belongs only to one cell.
