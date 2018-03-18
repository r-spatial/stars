library(plumber)
r <- plumb("script.R")  # Where 'script.R' is the location of the file shown above
r$run(port=8000)
