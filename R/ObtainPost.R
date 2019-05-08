#' Obtain Posterior Draws
#' 
#' Function to obtain draws from the posterior distribution of the survival curve.
#' @import BART
#' @param TrainX Explanatory variables for training (in sample) data. 
#' Must be a matrix with rows corresponding to observations and columns to variables
#' @param Times The time of event or right-censoring
#' @param Event The event indicator: 1 is and event while 0 is censored
#' @param TestX Explanatory variables for test (out of sample) data. Must be a matrix and 
#' have the same structure as TrainX 
#' @param parallel Whether to obtain posterior draws across multiple cores.
#' @param NumCores Number of cores to run on, default is 2
#' @param seed Random seed, only used for parallelization
#' @export


ObtainPost = function(TrainX = NULL, Times, Event, TestX = NULL,
                      parallel = F, NumCores = 2, seed = 807) {
  
  if (parallel == F) {
    
    Post = BART::surv.bart(times = Times, delta = Event)
    
  } else {
    
    if (.Platform$OS.type != "unix") {
      
      Post = ParallelWindows(TrainX = TrainX,
                             Times = Times,
                             Event = Event,
                             TestX = TestX,
                             NumCores = NumCores,
                             seed = seed)
      
    } else {
      
      Post = ParallelUnix(TrainX = TrainX,
                          Times = Times,
                          Event = Event,
                          TestX = TestX,
                          NumCores = NumCores,
                          seed = seed)
      
    }
  }
  
  return(Post)
  
}


