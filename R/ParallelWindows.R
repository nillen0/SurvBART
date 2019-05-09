#' Obtain Posterior Draws on a non unix machine
#' 
#' Function which runs in parallel to obtain posterior draws of the survival function 
#' on a non-unix based machine.
#' @importFrom BART surv.bart
#' @importFrom parallel makeCluster stopCluster
#' @import doParallel
#' @import foreach
#' @param TrainX Explanatory variables for training (in sample) data. 
#' Must be a matrix with rows corresponding to observations and columns to variables
#' @param Times The time of event or right-censoring
#' @param Event The event indicator: 1 is and event while 0 is censored
#' @param TestX Explanatory variables for test (out of sample) data. Must be a matrix and 
#' have the same structure as TrainX 
#' @param NumCores Number of cores to run on, default is 2
#' @export

ParallelWindows = function(TrainX, Times, Event, TestX, NumCores) {
  # How many draws from each:
  HowMany = 1000 %/% NumCores
  leftover = 1000 - HowMany*NumCores
  
  # Obtain
  i = 1:NumCores
  cl = makeCluster(NumCores)
  registerDoParallel(cl)
  temp = foreach(i, .packages = "BART") %dopar% {
    SomePost = BART::surv.bart(times = Times,
                               delta = Event,
                               ndpost = HowMany + (i == NumCores)*leftover)
    out = SomePost$surv.test
    out
  }
  
  Post = do.call(rbind, temp)
  return(Post)
}
