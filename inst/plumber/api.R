library(plumber)
r <- plumb("server.R")  # Where 'script.R' is the location of the file shown above
r$registerHook("exit", function(){
  print("Bye bye!")
})
r$run(port=8000)
