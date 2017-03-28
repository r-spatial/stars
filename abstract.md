## Scalable, Spatiotemporal Tidy Arrays for R (stars)

### Edzer Pebesma, Etienne Racine, Michael Sumner

Spatiotemporal data often comes in the form of dense arrays, with
space and time being array dimensions. Examples are socio-economic
or demographic data, environmental variables monitored at fixed
stations, satellite images, and climate model results. Currently,
R does not have infrastructure to handle and analyse such arrays
easily. Package raster is probably still the most powerful package
for handling this kind of data in memory and on disk, but does not
address non-raster time series, rasters time series with multiple
attributes, or rasters with mixed type attributes. This project
will not only deal with these cases, but also extend the "in memory
or on disk" model to that where the data are held remotely in
cloud storage, which is a more feasible option e.g. for satellite
data collected Today. We will implement pipe-based workflows that
are developed and tested on samples before they are evaluated for
complete datasets, and discuss the challenges of visualiasation and
storage in such workflows. This is work in progress, and the talk
will discuss the design stage and hopefully show an early prototype.
