#' Obtain estimated Quantiles from Weibull regression
#'
#' Gives estimated quantiles and associated Wald confidence intervals
#' from a Weibull regression model.
#' @import survival
#' @param Times Observed time to event or right-censoring
#' @param Event Indicator of event observed (1) or censored(0)
#' @param Predictors The desired predictors of survival
#' @param Quantiles Quantiles we wish to estimate
#' @param ConfLevel The desired confidence level on which to construct
#' confidence intervals for the quantiles
#' @expot

WeibQuant = function(Times, Event, Predictors, Quantiles, ConfLevel) {
  
  formula = as.formula(paste("Surv(Times, Event) ~ ", Predictors))
  WeibFit = survreg(formula, dist = "weibull")
  
  temp = predict(WeibFit, type = "quantile", p = Quantiles, se.fit = T)
  
  cut = 1-ConfLevel/2
  Z = qnorm(cut, mean = 0, sd = 1)
  
  WeibPoint = temp$fit[1,]
  WeibSE = temp$se.fit[1,]
  WeibLower = WeibPoint - cut*WeibSE
  WeibUpper = WeibPoint + cut*WeibSE
  
  temp = matrix(c(WeibPoint, WeibLower, WeibUpper),
                nrow = 3, ncol = length(Quantiles), byrow = T)
  
  colnames(temp) = Quantiles
  rownames(temp) = c("WeibPoint", "WeibLower", "WeibUpper")
  
  return(temp)
}



