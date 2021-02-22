.z.test <- function(x, y=NULL, alternative="two.sided", mu=0, sigma.x=1, sigma.y=1, conf.level=0.95, 
                    paired=FALSE, ciValueMeanDiffES=0.95) {
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
    zStat <- sqrt(nX) * d
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
      zStat <- (xBar - yBar - mu)/stderr
      nEff <- (1/nX + 1/nY)^(-1)
      d <- zStat/sqrt(nEff)
    }
  }
  
  # TODO(Alexander): Doesn't work for two-sample problems. Furthermore, only scaled wrt sigma.x
  # 
  # args(.zMeanDifferenceConfidenceIntervalStat)
  
  
  tempResult <- .zMeanDifferenceConfidenceIntervalStat("zStat"=zStat, "ciValueMeanDiff"=conf.level,
                                                       "sdError"=sdError, "nX"=nX, "nY"=nY, "alternative"=alternative,
                                                       "ciValueMeanDiffES"=ciValueMeanDiffES)
  confInt <- tempResult[["confInt"]]
  confIntEffSize <- tempResult[["confIntEffSize"]]
  
  pValue <- switch(alternative,
                   "two.sided"=2 * pnorm(-abs(zStat)),
                   "less"=pnorm(zStat),
                   "greater"=1-pnorm(zStat))
  
  result[["statistic"]] <- zStat
  result[["p.value"]] <- pValue
  result[["conf.int"]] <- confInt
  result[["estimate"]] <- estimate
  result[["d"]] <- d
  result[["confIntEffSize"]] <- confIntEffSize
  result[["method"]] <- method
  result[["data.name"]] <- dname
  return(result)
}


.zMeanDifferenceNonScaledConfidenceInterval <- function(zStat, alternative, ciValue) {
  result <- c(NA, NA)
  tempAlpha <- if (alternative == "two.sided") {
    (1-ciValue)/2
  } else {
    1-ciValue
  }
  
  deviation <- qnorm(1-tempAlpha)
  
  if (alternative=="two.sided") {
    result[1] <- zStat - deviation
    result[2] <- zStat + deviation
  } else if (alternative=="greater") {
    result[1] <- zStat - deviation
    result[2] <- Inf
  } else if (alternative=="less") {
    result[1] <- -Inf
    result[2] <- zStat + deviation
  }
  return(result)
}

.zMeanDifferenceConfidenceIntervalStat <- function(zStat, ciValueMeanDiff=0.95, alternative, sdError=1, nX=1, 
                                                   nY=NULL, ciValueMeanDiffES=0.95) {
  if (!is.null(nY))
    stop("two sample z-confidence intervals not yet implemented")
  
  result <- list("conf.int"=NULL, "confIntEffSize"=NULL)
  
  tempResult <- c(NA, NA)
  tempResult <- .zMeanDifferenceNonScaledConfidenceInterval("zStat"=zStat,
                                                            "alternative"=alternative,
                                                            "ciValue"=ciValueMeanDiff)
  
  result[["confInt"]] <- sdError*(tempResult)
  
  if (ciValueMeanDiff==ciValueMeanDiffES) {
    result[["confIntEffSize"]] <- 1/sqrt(nX)*(tempResult)
  } else {
    tempResult <- .zMeanDifferenceNonScaledConfidenceInterval("zStat"=zStat,
                                                              "alternative"=alternative,
                                                              "ciValue"=ciValueMeanDiffES)
    
    result[["confInt"]] <- 1/sqrt(nX)*(tempResult)
  }
  return(result)
}


# .zMeanDifferenceConfidenceIntervalStat <- function(dMeanDiff, ciValue, alternative, sdErrorConstant=1) {
#   # # Check with BSDA 
#   # less
#   # cint <- c(-Inf, zobs * stderr + qnorm(conf.level) * stderr)
#   # 
#   # greater
#   # cint <- c(zobs * stderr - qnorm(conf.level) * stderr, 
#   #           Inf)
#   # 
#   # two.sided
#   # cint <- c(zobs * stderr - qnorm((1 - alpha/2)) * stderr, 
#   #           zobs * stderr + qnorm((1 - alpha/2)) * stderr)
#   # 
#   
#   result <- c(NA, NA)
#   
#   tempAlpha <- if (alternative == "two.sided") {
#     (1-ciValue)/2
#   } else {
#     1-ciValue
#   }
#   
#   deviation <- sdErrorConstant*qnorm(1-tempAlpha)
#   
#   if (alternative=="two.sided") {
#     result[1] <- dMeanDiff - deviation
#     result[2] <- dMeanDiff + deviation
#   } else if (alternative=="greater") {
#     result[1] <- dMeanDiff - deviation
#     result[2] <- Inf
#   } else if (alternative=="less") {
#     result[1] <- -Inf
#     result[2] <- dMeanDiff + deviation
#   }
#   return(result)
# }
