#' Helper function, analyzes quantile results matrix
#' 
#' Helper function which extracts coverage and quantile estimates
#' @param QuantMatrix Input matrix which should come from QuantAll
#' @param Quantiles Quantiles being estimated
#' @param shape Shape of Weibull distribution, used to find true values
#' @param scale Scale of Weibull distribution, used to find true values
#' @export

AnalyzeQuants = function(QuantMatrix, Quantiles, shape, scale) {
  TrueVals = qweibull(Quantiles, shape = shape, scale = scale)
  
  CoverMat = sapply(1:length(Quantiles), function(i) {
    BoundsBART = QuantMatrix[c("Lower", "Upper"), i]
    BoundsKM = QuantMatrix[c("KMlower", "KMupper"), i]
    BoundsWeib = QuantMatrix[c("WeibLower", "WeibUpper"), i]
    
    
    ResBART = as.numeric(TrueVals[i] > BoundsBART[1] & TrueVals[i] < BoundsBART[2])
    ResKM = as.numeric(TrueVals[i] > BoundsKM[1] & TrueVals[i] < BoundsKM[2])
    ResWeib = as.numeric(TrueVals[i] > BoundsWeib[1] & TrueVals[i] < BoundsWeib[2])
    
    temp = c("BART" = ResBART, "KM" = ResKM, "Weib" = ResWeib)
    return(temp)
  })
  
  colnames(CoverMat) = Quantiles
  
  Estimates = QuantMatrix[c("Mean", "KMpoint", "WeibPoint"),]
  
  list("EstimateMat" = Estimates,
       "CoverageMat" = CoverMat)
}