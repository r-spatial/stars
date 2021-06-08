#' Landsat-7 bands for a selected region around Olinda, BR
#' 
#' Probably containing the six 30 m bands:
#' \itemize{
#'  \item Band 1 Visible (0.45 - 0.52 µm) 30 m
#'  \item Band 2 Visible (0.52 - 0.60 µm) 30 m
#'  \item Band 3 Visible (0.63 - 0.69 µm) 30 m
#'  \item Band 4 Near-Infrared (0.77 - 0.90 µm) 30 m
#'  \item Band 5 Short-wave Infrared (1.55 - 1.75 µm) 30 m
#'  \item Band 7 Mid-Infrared (2.08 - 2.35 µm) 30 m
#' }
#'
"L7_ETMs"

#' Monthly Gridded Meteorological Observations
#'
#' These are the monthly observational data used for BCSD downscaling. See: http://gdo-dcp.ucllnl.org/downscaled_cmip_projections/dcpInterface.html#About for more information." ;
#' "Atmospheric Temperature, Air Temperature Atmosphere, Precipitation, Rain, Maximum Daily Temperature, Minimum  Daily Temperature" ;
#'
"bcsd_obs"

#' Sentinel-2 sample tile
#'
#' Sentinel-2 sample tile, downloaded from https://scihub.copernicus.eu/
#' reads the four 10-m bands: B2 (490 nm), B3 (560 nm), B4 (665 nm) and B8 (842 nm)
"stars_sentinel2"
