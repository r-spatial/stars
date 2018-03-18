# script.R
library(jsonlite) # base64_enc

# load stars
library(stars)

# load data
tif = system.file("tif/L7_ETMs.tif", package = "stars")
x = read_stars(tif)

data = list(x = x, y = "foo", z = 0:10)

#* @get /data
get_data <- function(name = NULL, expr = NULL) {
  if (!is.null(name) && !is.null(expr))
  	return("cannot return both name and expr") # is this the proper way to return errors?

  if (is.null(name) && is.null(expr))
  	names(data)
  else {
    if (!is.null(name))
      base64_enc(serialize(data[[name]], NULL)) # to char
	else
      base64_enc(serialize( eval(parse(text = expr), data), NULL)) # to char
  }
}


#* @post /sum
addTwo <- function(a, b){
  as.numeric(a) + as.numeric(b)
}
