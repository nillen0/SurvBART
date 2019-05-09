#' Parallelize posterior draws for unix machine
#' 
#' Function which runs in parallel to obtain posterior draws of the 
#' survival function on a unix based machine
#' @import BART
#' @param TrainX Explanatory variables for training (in sample) data. 
#' Must be a matrix with rows corresponding to observations and columns to variables
#' @param Times The time of event or right-censoring
#' @param Event The event indicator: 1 is and event while 0 is censored
#' @param TestX Explanatory variables for test (out of sample) data. Must be a matrix and 
#' have the same structure as TrainX 
#' @param NumCores Number of cores to run on, default is 2


ParallelUnix = function(TrainX, Times, Event, TestX, NumCores) {
  temp = mc.surv.bart(x.train = TrainX,
               times = Times,
               delta = Event,
               x.test = TestX,
               mc.cores = NumCores)
  
  return(temp$surv.test)
}
