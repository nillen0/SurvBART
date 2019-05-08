#' Estimate the Survival Curve
#' 
#' Return the predicted survival probability at each unique time point
#' @param PosteriorDraws Matrix of random draws from the posterior distribution
#' of the survival curve.
#' @export

SurvEst = function(PosteriorDraws) {
  return(apply(PosteriorDraws, 2, mean))
}