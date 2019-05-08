#' Estimate quantiles of survival function
#' 
#' Estimates the quantiles of the survival function along with credible bounds
#' based upon draws from the posterior distribution
#' @param PosteriorDraws Matrix of random draws from the posterior distribution
#' of the survival curve.
#' @param Times Vector of unique time points
#' @param Quantiles Quantiles to be estimated
#' @param CredLevel Credible level for upper and lower bounds of quantile estimate
#' @export

QuantEst = function(PosteriorDraws, Times, Quantiles, CredLevel) {
  
  # Obtain credible interval:
  CredInt = CredibleInt(PosteriorDraws = PosteriorDraws,
              CredLevel = CredLevel)
  
  # Obtain Mean values:
  MeanSurv = SurvEst(PosteriorDraws = PosteriorDraws)
  
  temp = sapply(Quantiles, function(q) {
    cut = 1-q
    BARTIndex = sum(MeanSurv > cut) + 1
    BARTLower = sum(CredInt[1,] > cut) + 1
    BARTUpper = sum(CredInt[2,] > cut) + 1 
    
    c("Mean" = Times[BARTIndex], "Lower" = Times[BARTLower], "Upper" = Times[BARTUpper])
  })
  
  colnames(temp) = Quantiles

  return(temp)
}
