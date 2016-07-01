
#' YN dprime estimator
#' 
#' Estimate dprime from Hit Rate and False Alarm Rate in a Yes/No task
#' 
#' @param hitR Hit Rate
#' @param faR False Alarm Rate
#' @return dprime value
#' 

perf2dp <- function(hitR,faR) {
  qnorm(hitR) - qnorm(faR)
}

#' YN criterion (c) estimator
#' 
#' Estimate criterion (c) from Hit Rate and False Alarm Rate in a Yes/No task
#' 
#' @param hitR Hit Rate
#' @param faR False Alarm Rate
#' @return dprime value
#' 

perf2c <- function(hitR,faR){
  -.5*(qnorm(hitR)+qnorm(faR))
}

#' YN dprime (dp) and criterion (c) estimator
#' 
#' Estimate criterion (c) from Hit Rate and False Alarm Rate in a Yes/No task
#' 
#' @param hitR Hit Rate
#' @param faR False Alarm Rate
#' @return a list with dp and c values
#' 

perf2dpc <- function(hitR,faR){
  perf <- list(dp=perf2dp(hitR,faR),c=perf2c(hitR,faR))
}

#' performance predictor based on dprime (dp) and criterion (c)
#' 
#' Predict Hit Rate and False Alarm Rates based on dprime (dp) and criterion (c) in a Yes/No task
#' 
#' @param dp d'
#' @param c criterion
#' @return a list with Hit and False Alarm rates
#' 

dpc2perf <- function(dp,c){
  HR <- pnorm(dp/2 - c)
  FAR <- pnorm(-dp/2 - c)
  perfs <- list(HR=HR,MR=1-HR, FAR=FAR,CR=1-FAR)
  perfs
}
