library(stars)

# wind:
library(gstat)
data(wind)
library(sp)
library(sf)

wind.loc$y = as.numeric(char2dms(as.character(wind.loc[["Latitude"]])))
wind.loc$x = as.numeric(char2dms(as.character(wind.loc[["Longitude"]])))
locs = st_as_sf(wind.loc, coords = c("x", "y"), crs = st_crs(4326))


# match order:

data = as.data.frame(t(wind[,-c(1:3)]))

data$geom = st_geometry(locs[match(names(wind)[-c(1:3)], locs$Code),])

st_stars(st_sf(data), times = ISOdate(wind$year+1900, wind$month, wind$day, 0))

# Produc:
data(Produc, package = "plm")

library(maps)
states = st_as_sf(map('state', plot=FALSE, fill=TRUE))[-8,]
yrs = 1970:1986
#time = as.POSIXct(paste(yrs, "-01-01", sep=""), tz = "GMT")

Pr = lapply(Produc[order(Produc[2], Produc[1]),-(1:3)], matrix, nrow = nrow(states))
(Produc.st = st_stars(states[-8], times = yrs, Pr))


