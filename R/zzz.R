.First.lib <- function(lib, pkg) {
  library.dynam("Mathematica4R", pkg, lib)
  # local({ 
  #    Mathematica4R:::init.M()
  #    Mathematica4R:::init.RForm()
  # },.GlobalEnv)
}

#.Last.lib <- function(libpath) {
#  cat("Bye Ruby\n")
#  .C("rb4R_finalize", PACKAGE="rb4R")
#}
