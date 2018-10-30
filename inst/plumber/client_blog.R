library(httr)     # GET, POST, PUT
library(jsonlite) # base64_dec, base64_enc, toJSON, fromJSON
library(tibble)   # print

get_data = function(url, expr = NULL) {
	if (is.null(expr))
		fromJSON( content(GET(url), "text", encoding = "UTF-8"))
	else
		unserialize(base64_dec(fromJSON( 
			content(POST(url, body = list(expr = expr), encode = "json"),
				"text", encoding = "UTF-8")
		)))
}

put_data = function(url, name, value) {
	value = toJSON(base64_enc(serialize(value, NULL)))
	PUT(url, body = list(name = name, value = value), encode = "json")
}

# from where?
url = "http://localhost:8000/data"

get_data(url) # list

library(stars)
md = get_data(url, "md")

# select a country:
nl = st_as_sf(raster::getData("GADM", country = "NLD", level = 0)) %>%
	st_transform(st_crs(md))

get_data(url) # any data there?
md = get_data(url, "md")

proxy = md[nl,]$proxy

plot(st_geometry(nl), axes = TRUE)
s = sapply(proxy, function(x) {
	r = st_as_stars(x, downsample = 100, url = url)
	image(r, add = TRUE, rgb = 1:3, maxColorValue = 30000)
})
plot(st_geometry(md), border = 'yellow', add = TRUE, lwd = .8)
plot(st_geometry(nl), add=TRUE, border = 'orange', lwd = 2)

ndvi = function(x) (x[4]-x[1])/(x[4]+x[1])
put_data(url, "ndvi", ndvi)
get_data(url, "ndvi")
plot(st_geometry(nl), axes = TRUE)

x = st_apply(proxy[[10]], 1:2, ndvi)
xs = st_as_stars(x, url = url, downsample = 50)
image(xs, add = TRUE)

s = sapply(proxy, function(x) {
	x = st_apply(x, 1:2, ndvi)
	r = st_as_stars(x, url = url, downsample = 100)
	image(r, add = TRUE, zlim = c(-1,1), col = colorRampPalette(c(grey(.1), grey(.5), 'green'))(10))
})
plot(st_geometry(nl), add = TRUE, border = 'red')
