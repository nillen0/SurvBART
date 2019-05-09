#' Perform one iteration of simulation
#' 
#' Performs one iteration of the simulation described in Sparapani et al.
#' @param OneSample Perform simulation described in "One-Sample Simulation" section
#' @param n Sample Size
#' @param CensoringRate Desired Censoring Rate, either 0.2 or 0.5
#' @param Parallel Run the function in parallel
#' @param NumCores If parallel, number of cores to use
#' @param seed Set random seed number
#' @param save.output Do you want to save results?
#' @param file.path Where to save
#' @export

Simulation = function(OneSample = T, n, CensoringRate, Parallel = F, NumCores = 2, seed,
                      save.output = F, file.path = NULL) {
  
  if (OneSample == T) {
    # One Sample Code ----
    set.seed(seed)
    # Simulate Data
    Data = SimulateData(n = n, censoring.rate = CensoringRate, TwoSample = F)
    
    # Obtain Posterior Draws:
    PostDraws = ObtainPost(Times = Data$time,
                           Event = Data$event,
                           parallel = Parallel, NumCores = NumCores)
    
    # Get Estimated Quantiles with confidence bounds:
    Quants = QuantAll(PosteriorDraws = PostDraws$surv,
                      Times = Data$time,
                      Event = Data$event,
                      Predictors = 1,
                      Quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9),
                      ConfLevel = 0.95)
    
    # Get Estimated Quantiles with confidence bounds:
    Quants = QuantAll(PosteriorDraws = PostDraws$surv,
                      Times = Data$time,
                      Event = Data$event,
                      Predictors = 1,
                      Quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9),
                      ConfLevel = 0.95)
    
    # Analyze data:
    Results = AnalyzeQuants(QuantMatrix = Quants, Quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9),
                  shape = 0.8, scale = 2.5)
    
    
  } else {
    # With Covariate ----
    print("Hello, Kristin")
    
    
  }
  
  if (save.output == T) {
      saveRDS(Results, file.path)
  }
  
  return(Results)
  
}

