#' Plot BART estimated Survival Curves
#' 
#' This function allows the user to plot the estimated survival curve
#' along with its credible interval.
#' @import graphics
#' @param PosteriorDraws Matrix of draws from the posterior distribution
#' of the survival function
#' @param UniqueTimes The unique times of observation or censoring
#' @param CredLevel The desired credible level
#' @export


PlotSurv = function(PosteriorDraws, UniqueTimes, CredLevel = 0.95) {
  
  # Find mean:
  MeanSurv = SurvEst(PosteriorDraws = PosteriorDraws)
  
  # Find Credible Interval:
  Cred = CredibleInt(PosteriorDraws = PosteriorDraws,
              CredLevel = CredLevel)
  
  # Plot
  temp = plot(y = MeanSurv, x = UniqueTimes,
       main = "Survival Curve",
       ylab = "Survival Probability",
       xlab = "Time",
       type = "s")
  lines(y = Cred[1,], x = UniqueTimes, lty = 2, type = "s")
  lines(y = Cred[2,], x = UniqueTimes, lty = 2, type = "s")
  
}

