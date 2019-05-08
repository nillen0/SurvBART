#' Estimate Quantiles using Kaplan-Meier Product Limit Estimator
#' 
#' Estimates quantiles of a survival function using the Kaplan-Meier Product
#' Limit Estimator. Returns a matrix of point estimates of the quantiles
#' along with confidence intervals.
#' @import survival
#' @param Times Observed times of event or right-censoring
#' @param Event Indicator of whether a unit is observed (1) or censored(0)
#' @param Quantiles The quantiles we wish to estimate
#' @param Predictors Categorical predictor variable
#' @param ConfLevel The confidence level for the quantile confidence
#' interval
#' @export

KMQuant = function(Times, Event, Quantiles, Predictors = 1, ConfLevel) {
  KMFit = survfit(Surv(Times, Event) ~ Predictors, conf.int = ConfLevel)
  
  Estimates = quantile(KMFit, Quantiles)
  
  PointKM = Estimates$quantile
  LowerKM = Estimates$lower
  UpperKM = Estimates$upper
  
  KMRes = matrix(c(PointKM, LowerKM, UpperKM) ,nrow = 3, ncol = length(Quantiles), byrow = TRUE)
  colnames(KMRes) = Quantiles
  rownames(KMRes) = c("KMpoint", "KMlower", "KMupper")
  
  return(KMRes)
}
