library(httr)     # GET
library(jsonlite) # base64_dec
library(stars)

GET("http://localhost:8000/data")
GET("http://localhost:8000/data?name=y")

get_data = function(obj) {
  url = paste0("http://localhost:8000/data?name=", obj)
  unserialize(base64_dec(fromJSON( content(GET(url), "text", encoding = "UTF-8"))))
}

get_expr = function(expr) {
  url = paste0("http://localhost:8000/data?expr=", expr)
  unserialize(base64_dec(fromJSON( content(GET(url), "text", encoding = "UTF-8"))))
}

get_data("y")
get_data("z")

xx = get_data("x")

tif = system.file("tif/L7_ETMs.tif", package = "stars")
x = read_stars(tif)

all.equal(x, xx)

get_expr("x[,,,1]")
get_expr("adrop(x[,,,1])")
