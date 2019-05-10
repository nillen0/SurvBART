#' Read in SurvBART output data from directory
#' 
#' Reads in data from specified directory
#' @param file.dir Character object specying the location of the saved data
#' @export


GatherResults = function(file.dir = NULL) {
  Output = list(n50  = list(c20 = list(),
                            c50 = list()),
                n100 = list(c20 = list(),
                            c50 = list()),
                n200 = list(c20 = list(),
                            c50 = list()))
  
  for (n in c(50, 100)){
    for(CensoringRate in c(2, 5)) {
      for(i in 1:400) {
        if (n == 50) {
          if (CensoringRate == 2) {
            Output$n50$c20[[i]] = readRDS(paste(c(file.dir, "res", n, ".",CensoringRate, ".", i), collapse = ""))
          } else {
            Output$n50$c50[[i]] = readRDS(paste(c(file.dir, "res", n, ".",CensoringRate, ".", i), collapse = ""))
          }
        } else if (n == 100) {
          if (CensoringRate == 2) {
            Output$n100$c20[[i]] = readRDS(paste(c(file.dir, "res", n, ".",CensoringRate, ".", i), collapse = ""))
          } else {
            Output$n100$c50[[i]] = readRDS(paste(c(file.dir, "res", n, ".",CensoringRate, ".", i), collapse = ""))
          }
        } else {
          if (CensoringRate == 2) {
            Output$n200$c20[[i]] = readRDS(paste(c(file.dir, "res", n, ".",CensoringRate, ".", i), collapse = ""))
          } else {
            Output$n200$c50[[i]] = readRDS(paste(c(file.dir, "res", n, ".",CensoringRate, ".", i), collapse = ""))
          }
        }
      }
    }
  }
  return(Output)
}
