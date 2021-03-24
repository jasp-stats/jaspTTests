# Copyright (C) 2013-2021 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#' The plan of this file is to successively abstract functionality
#' from the individual t-tests into a common interface to reduce clutter
#'
.z.test <- function(x, y=NULL, alternative="two.sided", mu=0, sigma.x=1, sigma.y=1, 
                    ciValueMeanDiff=0.95, paired=FALSE, ciValueESMeanDiff=ciValueMeanDiff) {
  if (!is.null(y)) 
    stop("Paired and two-sample z.test not yet implemented")
  
  result <- list(statistic=NULL, p.value=NULL, conf.int=NULL, estimate=NULL, null.value=mu, 
                 alternative=alternative, method=NULL, data.name=NULL, 
                 d=NULL, confIntEffSize=NULL)
  
  nX <- length(x)
  nY <- NULL
  
  if (nX <= 1) 
    stop("not enough x observations")
  
  xBar <- mean(x)
  
  if (is.null(y)) {
    dname <- deparse(substitute(x))
    
    method <- "One-sample z-Test"
    estimate <- xBar - mu
    names(estimate) <- "mean difference of x"
    d <- (xBar-mu)/sigma.x
    z <- sqrt(nX) * d
    sdError <- sigma.x/sqrt(nX)
  } else {
    dname <- paste(deparse(substitute(x)), "and", paste(deparse(substitute(y))))
    
    if (paired==TRUE) {
      stop("Not yet implemented")
      method <- "Paired z-test"
    } 
    
    if (paired==FALSE) {
      nY <- length(y)
      
      if (nY <= 1) 
        stop("not enough y observations")
      
      yBar <- mean(y)
      method <- "Two-sample z-Test"
      estimate <- c(xBar, yBar)-mu
      names(estimate) <- c("mean difference of x", "mean difference of y")
      
      sdError <- sqrt(((sigma.x^2)/nX) + ((sigma.y^2)/nY))
      z <- (xBar - yBar - mu)/stderr
      nEff <- (1/nX + 1/nY)^(-1)
      d <- z/sqrt(nEff)
    }
  }
  
  confInt <- .zMeanDifferenceConfidenceIntervalStat("z"=z, 
                                                    "alternative"=alternative, 
                                                    "ciValue"=ciValueMeanDiff, 
                                                    "sdError"=sdError)
  
  # NOTE(Alexander): Only works for one-sample here
  confIntEffSize <- .zMeanDifferenceConfidenceIntervalStat("z"=z, 
                                                           "alternative"=alternative, 
                                                           "ciValue"=ciValueESMeanDiff, 
                                                           "sdError"=1/sqrt(nX))
  
  pValue <- switch(alternative,
                   "two.sided"=2 * pnorm(-abs(z)),
                   "less"=pnorm(z),
                   "greater"=1-pnorm(z))
  
  result[["statistic"]] <- z
  result[["p.value"]] <- pValue
  result[["conf.int"]] <- confInt
  result[["estimate"]] <- estimate
  result[["d"]] <- d
  result[["confIntEffSize"]] <- confIntEffSize
  result[["method"]] <- method
  result[["data.name"]] <- dname
  return(result)
}


.zMeanDifferenceConfidenceIntervalStat <- function(z, alternative, ciValue,
                                                   sdError=1) {
  result <- c(NA, NA)
  
  tempAlpha <- if (alternative == "two.sided") {
    (1-ciValue)/2
  } else {
    1-ciValue
  }
  
  deviation <- qnorm(1-tempAlpha)
  
  
  if (alternative=="two.sided") {
    result[1] <- sdError*z - sdError*deviation
    result[2] <- sdError*z + sdError*deviation
  } else if (alternative=="greater") {
    result[1] <- sdError*z - sdError*deviation
    result[2] <- Inf
  } else if (alternative=="less") {
    result[1] <- -Inf
    result[2] <- sdError*z + sdError*deviation
  }
  
  return(result)
}