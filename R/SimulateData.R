#' Simulate Data
#' 
#' A function to simulate data as specified in Sparparani et al. (2016)
#' @import stats
#' @param n Desired sample size
#' @param censoring.rate Desired censoring rate, either 0.2 or 0.5
#' @keywords simulate weibull
#' @export
#' @examples 
#' dat = SimulateData(n = 100, censoring.rate = 0.2)
#' head(dat)

SimulateData = function(n, censoring.rate) {
  
  if (!censoring.rate %in% c(0.20, 0.50)) {
    stop("Censoring rate either 0.2, or 0.5")
  }
  
  rate = ifelse(censoring.rate == 0.2, 0.094, 0.430)
  
  x = rweibull(n, shape = 0.8, scale = 2.5)
  c = rexp(n, rate = rate)
  
  t = pmin(x,c)
  delta = as.numeric(t==x)
  
  dat = data.frame("time" = t, "event" = delta)
  return(dat)
}
