# Predict values, given a model object, for a stars or stars_proxy object

Predict values, given a model object, for a stars or stars_proxy object

## Usage

``` r
# S3 method for class 'stars_proxy'
predict(object, model, ...)

# S3 method for class 'stars'
predict(object, model, ..., drop_dimensions = FALSE)
```

## Arguments

- object:

  object of class \`stars\`

- model:

  model object of a class that has a predict method; check with
  \`methods(class = class(object))\`

- ...:

  arguments passed on to this predict method

- drop_dimensions:

  logical; if \`TRUE\`, remove dimensions (coordinates etc) from
  \`data.frame\` with predictors

## Details

separate predictors in object need to be separate attributes in object;
in case they are e.g. in a band dimension, use \`split(object)\`
