
#' YN dprime estimator
#' 
#' Estimate dprime from Hit Rate and False Alarm Rate in a Yes/No task
#' Formula (1) in Stanislaw and Todorov 1999
#' 
#' @param hitR Hit Rate
#' @param faR False Alarm Rate
#' @return dprime value
#' 

perf2dp <- function(hitR,faR) {
  qnorm(hitR) - qnorm(faR)
}

#' YN Aprime estimator
#' 
#' Estimate Aprime from Hit Rate and False Alarm Rate in a Yes/No task
#' Formula (3) in Stanislaw and Todorov 1999
#' 
#' @param hitR Hit Rate
#' @param faR False Alarm Rate
#' @return Aprime value
#' 

perf2Ap <- function(hitR,faR) {
  .5 + (sign(hitR - faR) * ((hitR - faR)^2 + abs(hitR - faR)) / (4 * max(hitR, faR) - 4 * hitR * faR))
}

#' YN B" estimator
#' 
#' Estimate B" from Hit Rate and False Alarm Rate in a Yes/No task
#' Formula (9) in Stanislaw and Todorov 1999
#' 
#' @param hitR Hit Rate
#' @param faR False Alarm Rate
#' @return B" value
#' 

perf2Bsecond <- function(hitR,faR) {
  sign(hitR - faR) * (hitR * (1 - hitR) - faR * (1 - faR)) / (hitR * (1 - hitR) + faR * (1 - faR))
}

#' YN bias (B) estimator
#' 
#' Estimate bias (B) from Hit Rate and False Alarm Rate in a Yes/No task
#' Formula (5) in Stanislaw and Todorov 1999 (note typo in formula, but not in software implementations later in pdf)
#' 
#' @param hitR Hit Rate
#' @param faR False Alarm Rate
#' @return B value
#' 

perf2B <- function(hitR,faR){
  exp((qnorm(faR)^2 - qnorm(hitR)^2) / 2)
}

#' YN criterion (c) estimator
#' 
#' Estimate criterion (c) from Hit Rate and False Alarm Rate in a Yes/No task
#' Formula (7) in Stanislaw and Todorov 1999
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
#' Estimate dprime (dp) and criterion (c) from Hit Rate and False Alarm Rate in a Yes/No task
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

