#' Analyse results
#' 
#' This function analyses results from the Simulation() function. Given a list of results
#' from many runs of Simulation(), this function outputs a matrix of bias, RMSE, and coverage
#' probabilities
#' @param Results A list of results from multiple runs of Simulation()
#' @export

AnalyzeResults = function(Results) {
  CoverageMats = lapply(Results, function(x) x$CoverageMat)
  EstimatedMats = lapply(Results, function(x) x$EstimateMat)
  
  # Summarise Bias ----
  
  # How many inf:
  OutOfRange = Reduce("+", lapply(EstimatedMats, function(x) is.infinite(x)))
  
  
  # Matrix with number of valid obs:
  Denoms = matrix(400, nrow = 3, ncol = 5) - OutOfRange
  
  # Distance From Truth:
  Truth = matrix(qweibull(c(0.1,0.25,0.5,0.75,0.90), shape = 0.8, scale = 2.5),
                 nrow = 3, ncol = 5, byrow = T)
  Dist = lapply(EstimatedMats, function(x) x - Truth)
  
  # Convert inf to 0s:
  for (i in 1:length(Dist)) {
    Dist[[i]][is.infinite(Dist[[i]])] = 0
  }
  
  
  # Mean Bias:
  Bias = Reduce("+", Dist)/Denoms
  
  # Root MSE:
  rootSqErr = lapply(Dist, function(x) sqrt(x^2))
  RMSE = Reduce("+", rootSqErr)/Denoms
  
  # Summarise Coverage:
  Coverage = Reduce("+", CoverageMats)/400
  
  list("Bias" = Bias, "RMSE" = RMSE, "Coverage" = Coverage, "NotEstimated" = OutOfRange)

}

