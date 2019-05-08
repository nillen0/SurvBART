#' Obtain Posterior Draws on a non unix machine
#' 
#' Function which runs in parallel to obtain posterior draws of the survival function 
#' on a non-unix based machine.
#' @import BART
#' @import doParallel
#' @import foreach
#' @param TrainX Explanatory variables for training (in sample) data. 
#' Must be a matrix with rows corresponding to observations and columns to variables
#' @param Times The time of event or right-censoring
#' @param Event The event indicator: 1 is and event while 0 is censored
#' @param TestX Explanatory variables for test (out of sample) data. Must be a matrix and 
#' have the same structure as TrainX 
#' @param NumCores Number of cores to run on, default is 2
#' @param seed Random seed, only used for parallelization
#' @export

ParallelWindows = function(TrainX, Times, Event, TestX, NumCores, seed) {
  # How many draws from each:
  HowMany = 1000 %/% NumCores
  leftover = 1000 - HowMany*NumCores
  
  # Obtain
  i = 1:NumCores
  temp = foreach(i, .packages = "BART") %dopar% {
    set.seed(seed + i)
    SomePost = BART::surv.bart(times = Times,
                               delta = Event,
                               ndpost = HowMany + (i == NumCores)*leftover)
    out = SomePost$surv.test
    out
  }
  
  Post = do.call(rbind, temp)
  return(Post)
}