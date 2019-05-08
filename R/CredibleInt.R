#' Construct Credible Interval for survival curve
#' 
#' Function which uses draws from the posterior distribution at each
#' time point to construct a credible interval with set credible level
#' @param PosteriorDraws Matrix of draws from the posterior distribution
#' of the survival curve. Columns correspond to all draws at fixed time.
#' Rows correspond to single draw of survival curve over all observed
#' time points.
#' @param CredLevel The Credible Level of the desired credible interval.
#' @export

CredibleInt = function(PosteriorDraws, CredLevel = 0.95) {
 
  # Check for valid input
  if (CredLevel <= 0 | CredLevel >= 1){
    stop("Confidence Level must be between 0 and 1")
  }
  
  dist = 1 - CredLevel
  TailLength = dist/2
  
  temp = apply(PosteriorDraws, 2, function(x) {quantile(x, p=c(0+TailLength, 1-TailLength))})
  
  return(temp)
}



