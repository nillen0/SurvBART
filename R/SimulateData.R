#' Simulate Data
#' 
#' A function to simulate data as specified in Sparparani et al. (2016)
#' @import stats
#' @param n Desired sample size
#' @param censoring.rate Desired censoring rate, either 0.2 or 0.5
#' @param TwoSample Do you want to simulate under the two-sample settings
#' described in Sparapani (2016)?
#' @keywords simulate weibull
#' @export
#' @examples 
#' dat = SimulateData(n = 100, censoring.rate = 0.2)
#' head(dat)

SimulateData = function(n, censoring.rate, TwoSample = F) {
  
  if (!censoring.rate %in% c(0.20, 0.50)) {
    stop("Censoring rate either 0.2, or 0.5")
  }
  
  if (TwoSample == T) {
    z = rbinom(n, 1, 0.5)
    x = rweibull(n, shape = 0.8+0.5*z, scale = 2.5 + 1.05*z)
    
    rate = ifelse(censoring.rate == 0.2, 0.094, 0.330)
    c = rexp(n, rate = rate)
    
    t = pmin(x,c)
    delta = as.numeric(t==x)
    
    dat = data.frame("time" = t, "event" = delta, "z" = z)
  } else {
    x = rweibull(n, shape = 0.8, scale = 2.5)
    
    rate = ifelse(censoring.rate == 0.2, 0.094, 0.430)
    c = rexp(n, rate = rate)
    
    t = pmin(x,c)
    delta = as.numeric(t==x)
    
    dat = data.frame("time" = t, "event" = delta)
  }
  
  return(dat)
}
