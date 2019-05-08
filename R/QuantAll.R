#' Estimate Survival Quantiles using BART, KM, and Weibull Regression
#' 
#' Estimate Survival Quantiles and associated confidence bounds using
#' BART, KM, and Weibull Regression.
#' @param PosteriorDraws Draws from the posterior distribution of the
#' survival function
#' @param Times Observed time to events or right-censoring
#' @param Event Vector of event status: 1 = Observed, 0 = Censored
#' @param Predictors Desired model predictors
#' @param Quantiles Quantiles to be estimated
#' @param ConfLevel Desired confidence level to construct confidence
#' intervals
#' @export

QuantAll = function(PosteriorDraws, Times, Event,
                    Predictors = 1, Quantiles, ConfLevel) {
  
  UniqueTimes = sort(unique(Times))
  
  # Estimate quantiles, BART:
  BARTEst = QuantEst(PosteriorDraws = PosteriorDraws,
                     Times = UniqueTimes,
                     Quantiles = Quantiles,
                     CredLevel = ConfLevel)
  
  # Estimate Quantiles, KM:
  KMEst = KMQuant(Times = Times, Event = Event,
                  Predictors = Predictors,
                  Quantiles = Quantiles,
                  ConfLevel = ConfLevel)
  
  # Estimate Quantiles, Weibull:
  WeibEst = WeibQuant(Times = Times, Event = Event,
                      Predictors = Predictors,
                      Quantiles = Quantiles,
                      ConfLevel = ConfLevel)
  
  rbind(BARTEst, KMEst, WeibEst)
  
  
}


