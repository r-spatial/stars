library(httr)     # GET
library(jsonlite) # base64_dec
library(stars)

url = "http://localhost:8000/data"

get_data = function(url, expr = NULL) {
	if (is.null(expr))
		fromJSON( content(GET(url), "text", encoding = "UTF-8"))
	else {
		url = paste0(url, "?expr=", expr)
		unserialize(base64_dec(fromJSON( content(GET(url), "text", encoding = "UTF-8"))))
	}
}

put_data = function(url, name, value) {
	value = toJSON(base64_enc(serialize(value, NULL)))
	PUT(url, body = list(name = name, value = value), encode = "json")
}

get_data(url)
get_data(url, "y")
get_data(url, "z")

xx = get_data(url, "x")

tif = system.file("tif/L7_ETMs.tif", package = "stars")
x = read_stars(tif)

all.equal(x, xx)

#get_data(url, "x[,,,1]")
#get_data(url, "adrop(x[,,,1])")

put_data(url, "foo", "bar")
get_data(url)
get_data(url, "foo")
put_data(url, "z3", "3 * z")
get_data(url, "z3")

#put_data(url, "tif", x)
#get_data(url)

put_data(url, "xx", matrix(1:4,2))
get_data(url)
get_data(url, "xx")

library(sf)
pt = st_sf(a = 1, geom = st_sfc(st_point(3:4)))
put_data(url, "xx", pt)
get_data(url)
get_data(url, "xx")
