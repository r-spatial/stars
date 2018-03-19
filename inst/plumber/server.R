# script.R
library(jsonlite) # base64_enc

# load stars
library(stars)

# load some imagery data
x = read_stars(system.file("tif/L7_ETMs.tif", package = "stars"))

# global database
data = list(x = x, y = "foo", z = 0:10)

#* @get /data
get_data <- function(expr = NULL) {
	if (is.null(expr))
		names(data)
	else 
		base64_enc(serialize( eval(parse(text = expr), data), NULL)) # to char
}

#* @put /data
put_data <- function(req, name, value) {
	data[[name]] <<- unserialize(base64_dec(fromJSON(value)))
	NULL
}
