# script.R
library(jsonlite) # base64_enc

# load stars
library(stars)

# load some imagery meta data
# from a S2 .zip file, create a readable gdal source:
s2_expand_zip = function(s) {
	paste0("/vsizip/", s, "/", sub(".zip", ".SAFE", s), "/MTD_MSIL1C.xml")
}
lst = list.files(pattern = "*.zip")
l = lapply(s2_expand_zip(lst), read_stars_meta, sub = 1)
bb <- do.call(c, lapply(l, function(x) st_as_sfc(st_bbox(st_dimensions(x)))))
fn = sapply(l, function(x) attr(x, "file_names"))
library(tibble)
md = st_sf(tibble(file = fn, geom = bb))

# global database
data = list(md = md)

#* Log some information about the incoming request
#* @filter logger
function(req){
  cat(as.character(Sys.time()), "-", 
    req$REQUEST_METHOD, req$PATH_INFO, "-", 
    req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, ":", req$postBody, "\n")
  plumber::forward()
}

# plumber REST end points /data:
#* @get  /data
#* @post /data
get_data <- function(req, expr = NULL) {
	print(expr)
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
