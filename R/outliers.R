
#' Remove outliers
#' 
#' Remove outliers from a vector.
#' 
#' @param x A vector
#' @param tail One of \code{'both'}, \code{'left'}, or \code{'right'}
#' @param meth One of \code{'trim'} or \code{'IQR'}
#' @param probs A 1 or 2 element numeric vector of probabilities
#' @param sdn when meth = 'sd', number of standard deviations away from the mean to detect outlier
#' @param na.rm A boolean whether to remove NA
#' @return A cleaned vector where all outliers outside of \code{quantile(x,probs)} (see details for individual methods) are changed to NA.
#' @details Each method will be described here in the future.
#' @examples
#' x <- rnorm(1000)
#' y <- rm.outliers(x,probs=c(.01,.99))
#' 

rm.outliers <- function(x, tail='both', meth='trim', probs=c(.25, .75), sd=2, na.rm = TRUE, ...) {
  x[find.outliers(x, tail=tail, meth=meth, probs=probs, na.rm=na.rm)] <- NA
}


#' Find outliers
#' 
#' Return logical true where outliers are in a vector.
#' 
#' @param x A vector
#' @param tail One of \code{'both'}, \code{'left'}, or \code{'right'}
#' @param meth One of \code{'trim'}, \code{'sd'} or \code{'IQR'}
#' @param probs A 1 or 2 element numeric vector of probabilities
#' @param sdn when meth = 'sd', number of standard deviations away from the mean to detect outlier
#' @param na.rm A boolean whether to remove NA
#' @return A boolean vector where outliers outside of \code{quantile(x,probs)} (see details for individual methods) are true.
#' @details Each method will be described here in the future.
#' @examples
#' x <- rnorm(1000)
#' y <- find.outliers(x,probs=c(.01,.99))
#' 

find.outliers <- function(x, tail='both', meth='trim', probs=c(.25, .75), sdn=2, na.rm = TRUE, ...) {
  if (length(probs) == 1) {
    probs = c(probs, 1-probs)
  }
  switch(meth,
         IQR={
           qnt <- quantile(x, probs=probs, na.rm = na.rm, ...)
           H <- 1.5 * IQR(x, na.rm = na.rm)
         },
         trim={
           qnt <- quantile(x, probs=probs, na.rm = na.rm, ...)
           H <- 0
         },
         sd={
           s = sd(x,na.rm = na.rm)
           m = mean(x, na.rm = na.rm)
           qnt <- c(m - sdn * s , m + sdn * s)
           H <- 0
         })
         
  y <- logical(length=length(x))
  ifelse(match(tail,c('both','left')),y[x < (qnt[1] - H)] <- T)
  ifelse(match(tail,c('both','right')),y[x > (qnt[2] + H)] <- T)
  y
}

