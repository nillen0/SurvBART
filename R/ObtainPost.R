#' Obtain Posterior Draws
#' 
#' Function to obtain draws from the posterior distribution of the survival curve.
#' @importFrom BART surv.bart
#' @importFrom parallel makeCluster
#' @importFrom foreach registerDoSEQ
#' @param TrainX Explanatory variables for training (in sample) data. 
#' Must be a matrix with rows corresponding to observations and columns to variables
#' @param Times The time of event or right-censoring
#' @param Event The event indicator: 1 is and event while 0 is censored
#' @param TestX Explanatory variables for test (out of sample) data. Must be a matrix and 
#' have the same structure as TrainX 
#' @param parallel Whether to obtain posterior draws across multiple cores.
#' @param NumCores Number of cores to run on, default is 2
#' @export


ObtainPost = function(TrainX = NULL, Times, Event, TestX = NULL,
                      parallel = F, NumCores = 2) {
  
  if (parallel == F) {
    
    temp = BART::surv.bart(times = Times, delta = Event)
    Post = temp$surv.test
    
  } else {
    
    if (.Platform$OS.type != "unix") {
      cl = makeCluster(NumCores)
      registerDoParallel(cl)
      Post = ParallelWindows(TrainX = TrainX,
                             Times = Times,
                             Event = Event,
                             TestX = TestX,
                             NumCores = NumCores)
      
    } else {
      
      Post = ParallelUnix(TrainX = TrainX,
                          Times = Times,
                          Event = Event,
                          TestX = TestX,
                          NumCores = NumCores)
      
    }
  }
  
  UniqueTimes = sort(unique(Times))
  
  temp = list(surv = Post, time = UniqueTimes)
  
  return(temp)
  
}


