library(plumber)
r <- plumb("server.R")  # Where 'script.R' is the location of the file shown above
r$run(port=8000)
