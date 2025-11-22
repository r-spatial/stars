# Principle components of stars object

Compute principle components of stars object

## Usage

``` r
# S3 method for class 'stars_proxy'
prcomp(x, ..., downsample = 0)

# S3 method for class 'stars'
prcomp(x, ..., quiet = FALSE)
```

## Arguments

- x:

  object of class \`stars\` or \`stars_proxy\`

- ...:

  see [prcomp](https://rdrr.io/r/stats/prcomp.html)

- downsample:

  see [st_as_stars](st_as_stars.md)

- quiet:

  logical; if \`TRUE\`, suppress message that PCs will be computed on
  last dimension; see details

## Value

object of class \`prcomp\`, see
[prcomp](https://rdrr.io/r/stats/prcomp.html)

## Details

if \`x\` has only one attribute, principle components will be computed
in the space of the last dimension of \`x\` to predict PC scores into a
\`stars\` object, use [predict.stars](predict.stars.md); see example
below

## Examples

``` r
l7 = split(st_as_stars(L7_ETMs), 3) # use bands as features
plot(prcomp(l7))

plot(merge(predict(l7, model = prcomp(l7))))
#> downsample set to 1
```
