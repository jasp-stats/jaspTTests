#
# Copyright (C) 2013-2018 University of Amsterdam
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

# This is a temporary fix
# TODO: remove it when R will solve this problem!
gettextf <- function(fmt, ..., domain = NULL)  {
  return(sprintf(gettext(fmt, domain = domain), ...))
}

.ttestReadData <- function(dataset, options, type) {
  if (!is.null(dataset))
    return(dataset)
  else {
    groups  <- options$group
    if (!is.null(groups) && groups == "")
      groups <- NULL
    if(type %in% c("one-sample", "independent"))
      depvars <- unlist(options$dependent)
    else if (type == 'paired') {
      depvars <- unlist(options$pairs)
      depvars <- depvars[depvars != ""]
    }
    exclude <- NULL
    if (options$naAction == "listwise")
      exclude <- depvars
    return(.readDataSetToEnd(columns.as.numeric  = depvars,
                             columns.as.factor   = groups,
                             exclude.na.listwise = exclude))
  }
}

.ttestCheckErrors <- function(dataset, options, type) {
  if(type == "paired")
    for (pair in options$pairs) {
      if(pair[[1]] == "" || pair[[2]] == "")
        next
      p1 <- pair[[1]]
      p2 <- pair[[2]]
      if(is.null(p1) || is.null(p2))
        return()
      datasetErrorCheck <- data.frame(dataset[[.v(p1)]] - dataset[[.v(p2)]])
      colnames(datasetErrorCheck) <- .v(paste0("Difference between ", p1, " and ", p2))
      .hasErrors(datasetErrorCheck,
                 type = "variance",
                 exitAnalysisIfErrors = TRUE)
    }
  else if(type == "independent") {
    if (length(options$dependent) != 0 && options$group != '')
      .hasErrors(dataset,
                 type = 'factorLevels',
                 factorLevels.target  = options$group,
                 factorLevels.amount  = '!= 2',
                 exitAnalysisIfErrors = TRUE)
      }
}

.ttestOptionsList <- function(options, type){
  optionsList <- list()
  optionsList$wantsEffect     <- options$effectSize
  optionsList$wantsConfidenceEffSize <- (options$effectSizeCi && options$effectSize)
  optionsList$wantsStudents   <- options$student
  optionsList$wantsDifference <- options$meanDifference
  optionsList$wantsConfidenceMeanDiff <- (options$meanDifferenceCi && options$meanDifference)

  if(type == "paired") {
    optionsList$wantsWilcox <- options$wilcoxon
    optionsList$whichTests  <- c("Student", "Wilcoxon")[c(optionsList$wantsStudents, optionsList$wantsWilcox)]
  }
  else if(type == "one-sample"){
    optionsList$wantsZtest  <- options$zTest
    optionsList$wantsWilcox <- options$wilcoxon
    optionsList$whichTests  <- c("Student", "Wilcoxon", "Z")[c(optionsList$wantsStudents, optionsList$wantsWilcox, optionsList$wantsZtest)]
  }
  optionsList$wantsConfidenceEffSize    <- (options$effectSizeCi && options$effectSize)
  if(type %in% c("paired", "one-sample")) {
    optionsList$percentConfidenceEffSize  <- options$effectSizeCiLevel
    optionsList$percentConfidenceMeanDiff <- options$meanDifferenceCiLevel
  } else if(type == "independent") {
    optionsList$wantsWelchs <- options$welch
    optionsList$wantsWilcox <- options$mannWhitneyU
    optionsList$whichTests  <- c("Student", "Welch", "Mann-Whitney")[c(optionsList$wantsStudents, optionsList$wantsWelchs, optionsList$wantsWilcox)]
    optionsList$percentConfidenceEffSize  <- options$effectSizeCiLevel
    optionsList$percentConfidenceMeanDiff <- options$meanDifferenceCiLevel
  }
  optionsList$allTests <- c(optionsList$wantsStudents, optionsList$wantsWilcox)
  if(type == "one-sample")
    optionsList$allTests <- c(optionsList$allTests, optionsList$wantsZtest)
  if(type == "independent")
    optionsList$allTests <- c(optionsList$allTests, optionsList$wantsWelchs)
  optionsList$onlyTest <- sum(optionsList$allTests) == 1

  return(optionsList)
}

.ttestRowIsNewGroup <- function(test, tests) {
  return(length(tests) > 1 && test == tests[1])
}

.ttestAssumptionCheckContainer <- function(jaspResults, options, type) {
  if(type == "independent")
    if(!options$normalityTest && !options$equalityOfVariancesTest)
      return()
  else if (type %in% c("paired", "one-sample"))
    if(!options$normalityTest)
      return()
  if (is.null(jaspResults[["AssumptionChecks"]])) {
    container <- createJaspContainer(gettext("Assumption Checks"))
    dependList <- c("dependent", "group", "pairs", "naAction",
                    "normalityTest", "equalityOfVariancesTest")
    container$dependOn(dependList)
    container$position <- 2
    jaspResults[["AssumptionChecks"]] <- container
  }
}

.ttestDescriptivesContainer <- function(jaspResults, options) {
  if(!options$descriptives && !options$descriptivesPlot && !options$barPlot && !options$raincloudPlot && isFALSE(options$differenceRaincloudPlot))
    return()
  if (is.null(jaspResults[["ttestDescriptives"]])) {
    container <- createJaspContainer(gettext("Descriptives"))
    container$dependOn(c("naAction", "dependent", "pairs", "group"))
    container$position <- 3
    jaspResults[["ttestDescriptives"]] <- container
  }
}

.ttestVovkSellke <- function(table, options) {
  if (options$vovkSellke) {
    message <-gettextf("Vovk-Sellke Maximum <em>p</em>-Ratio: Based on a two-sided <em>p</em>-value,the maximum possible odds in favor of H%1$s over H%2$s equals 1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> %3$s .37 (Sellke, Bayarri, & Berger, 2001).","\u2081","\u2080","\u2264");
    table$addFootnote(message, symbol = "\u002A")
    table$addColumnInfo(name = "VovkSellkeMPR", title = "VS-MPR\u002A", type = "number")
  }
}

.ttestMainGetDirection <- function(hypothesis) {
  if (hypothesis == "greater")
    return("greater")
  else if (hypothesis == "less")
    return("less")
  else
    return("two.sided")
}

.confidenceLimitsEffectSizes <- function(ncp, df, conf.level = .95, alpha.lower = NULL,
                                         alpha.upper = NULL, t.value, tol = 1e-9, ...) {
  # This function comes from the MBESS package, version 4.6, by Ken Kelley
  # https://cran.r-project.org/web/packages/MBESS/index.html
  # Note this function is new in version 4, replacing what was used in prior versions.
  # Internal functions for the noncentral t distribution; two appraoches.
  ###########


  # General stop checks.
  if(!is.null(conf.level) && is.null(alpha.lower) && is.null(alpha.upper)) {
    alpha.lower <- (1 - conf.level) / 2
    alpha.upper <- (1 - conf.level) / 2
  }

  .conf.limits.nct.M1 <- function(ncp, df, conf.level = NULL, alpha.lower, alpha.upper, tol = 1e-9, ...) {

    min.ncp <- min(-150, -5 * ncp)
    max.ncp <- max(150,   5 * ncp)

    # Internal function for upper limit.
    # Note the upper tail is used here, as we seek to find the NCP that has, in its upper tail (alpha.lower,
    # for the lower limit), the specified value of the observed t/ncp.
    ###########################

    .ci.nct.lower <- function(val.of.interest, ...)
      (qt(p = alpha.lower, df = df, ncp = val.of.interest, lower.tail = FALSE, log.p = FALSE) - ncp)^2
    ###########################

    # Internal function for lower limit.
    # Note the lower tail is used here, as we seek to find the NCP that has, in its lower tail (alpha.upper,
    # for the upper limit), the specified value of the observed t/ncp.
    ###########################
    .ci.nct.upper <- function(val.of.interest, ...)
      (qt(p = alpha.upper, df = df, ncp=val.of.interest, lower.tail = TRUE, log.p = FALSE) - ncp)^2

    if(alpha.lower != 0)
      Low.Lim <- suppressWarnings(optimize(f = .ci.nct.lower, interval = c(min.ncp, max.ncp),
                                           alpha.lower = alpha.lower, df = df, ncp = ncp,
                                           maximize = FALSE, tol = tol))

    if(alpha.upper != 0) {
      Up.Lim <- suppressWarnings(optimize(f = .ci.nct.upper, interval = c(min.ncp, max.ncp),
                                          alpha.upper = alpha.upper, df = df, ncp = ncp,
                                          maximize = FALSE, tol = tol))
    }

    if(alpha.lower == 0)
      Result <- list(Lower.Limit = -Inf, Prob.Less.Lower = 0, Upper.Limit = Up.Lim$minimum,
                     Prob.Greater.Upper = pt(q = ncp, ncp = Up.Lim$minimum, df = df))
    if(alpha.upper == 0)
      Result <- list(Lower.Limit = Low.Lim$minimum,
                     Prob.Less.Lower = pt(q = ncp, ncp = Low.Lim$minimum, df = df, lower.tail = FALSE),
                     Upper.Limit = Inf, Prob.Greater.Upper = 0)
    if(alpha.lower != 0 && alpha.upper != 0)
      Result <- list(Lower.Limit = Low.Lim$minimum,
                     Prob.Less.Lower = pt(q = ncp, ncp = Low.Lim$minimum, df = df, lower.tail = FALSE),
                     Upper.Limit = Up.Lim$minimum,
                     Prob.Greater.Upper = pt(q = ncp, ncp = Up.Lim$minimum, df = df))

    return(Result)
  }
  ################################################
  .conf.limits.nct.M2 <- function(ncp, df, conf.level = NULL, alpha.lower, alpha.upper, tol = 1e-9, ...) {

    # Internal function for upper limit.
    ###########################
    .ci.nct.lower <- function(val.of.interest, ...)
      (qt(p = alpha.lower, df = df, ncp = val.of.interest, lower.tail = FALSE, log.p = FALSE) - ncp)^2

    # Internal function for lower limit.
    ###########################
    .ci.nct.upper <- function(val.of.interest, ...)
      (qt(p = alpha.upper, df = df, ncp = val.of.interest, lower.tail = TRUE, log.p = FALSE) - ncp)^2

    Low.Lim <- suppressWarnings(nlm(f = .ci.nct.lower, p = ncp, ...))
    Up.Lim  <- suppressWarnings(nlm(f = .ci.nct.upper, p = ncp, ...))

    if(alpha.lower == 0)
      Result <- list(Lower.Limit = -Inf, Prob.Less.Lower = 0, Upper.Limit = Up.Lim$estimate,
                     Prob.Greater.Upper = pt(q = ncp, ncp = Up.Lim$estimate, df = df))
    if(alpha.upper == 0)
      Result <- list(Lower.Limit = Low.Lim$estimate,
                     Prob.Less.Lower = pt(q = ncp, ncp = Low.Lim$estimate, df = df, lower.tail = FALSE),
                     Upper.Limit = Inf, Prob.Greater.Upper = 0)
    if(alpha.lower != 0 & alpha.upper != 0)
      Result <- list(Lower.Limit = Low.Lim$estimate,
                     Prob.Less.Lower = pt(q = ncp, ncp = Low.Lim$estimate, df = df, lower.tail = FALSE),
                     Upper.Limit = Up.Lim$estimate,
                     Prob.Greater.Upper = pt(q = ncp, ncp = Up.Lim$estimate, df = df))

    return(Result)
  }
  # Now, use the each of the two methods.
  Res.M1 <- Res.M2 <- NULL
  try(Res.M1 <- .conf.limits.nct.M1(ncp = ncp, df = df, conf.level = NULL,
                                    alpha.lower = alpha.lower,
                                    alpha.upper = alpha.upper, tol = tol), silent = TRUE)
  if(length(Res.M1) != 4)
    Res.M1 <- NULL
  try(Res.M2 <- .conf.limits.nct.M2(ncp = ncp, df = df, conf.level = NULL,
                                    alpha.lower = alpha.lower,
                                    alpha.upper = alpha.upper, tol = tol), silent = TRUE)
  if(length(Res.M2) != 4)
    Res.M2 <- NULL

  # Now, set-up the test to find the best method.
  Low.M1        <- Res.M1$Lower.Limit
  Prob.Low.M1   <- Res.M1$Prob.Less.Lower
  Upper.M1      <- Res.M1$Upper.Limit
  Prob.Upper.M1 <- Res.M1$Prob.Greater.Upper

  Low.M2        <- Res.M2$Lower.Limit
  Prob.Low.M2   <- Res.M2$Prob.Less.Lower
  Upper.M2      <- Res.M2$Upper.Limit
  Prob.Upper.M2 <- Res.M2$Prob.Greater.Upper

  # Choose the best interval limits:
  ##Here low
  Min.for.Best.Low <- min((c(Prob.Low.M1, Prob.Low.M2) - alpha.lower)^2)

  if(!is.null(Res.M1))
    if(Min.for.Best.Low == (Prob.Low.M1 - alpha.lower)^2)
      Best.Low <- 1
  if(!is.null(Res.M2))
    if(Min.for.Best.Low == (Prob.Low.M2 - alpha.lower)^2)
      Best.Low <- 2
  ##Here high
  Min.for.Best.Up <- min((c(Prob.Upper.M1, Prob.Upper.M2) - alpha.upper)^2)

  if(!is.null(Res.M1))
    if(Min.for.Best.Up == (Prob.Upper.M1 - alpha.upper)^2)
      Best.Up <- 1
  if(!is.null(Res.M2))
    if(Min.for.Best.Up == (Prob.Upper.M2 - alpha.upper)^2)
      Best.Up <- 2
  #####################################

  if(is.null(Res.M1))
    Low.M1 <- Prob.Low.M1 <- Upper.M1 <- Prob.Upper.M1 <- NA
  if(is.null(Res.M2))
    Low.M2 <- Prob.Low.M2 <- Upper.M2 <- Prob.Upper.M2 <- NA

  Result <- list(Lower.Limit        = c(Low.M1, Low.M2)[Best.Low],
                 Prob.Less.Lower    = c(Prob.Low.M1, Prob.Low.M2)[Best.Low],
                 Upper.Limit        = c(Upper.M1, Upper.M2)[Best.Up],
                 Prob.Greater.Upper = c(Prob.Upper.M1, Prob.Upper.M2)[Best.Up])

  return(Result)
}

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE,
                      errorBarType="ci", usePooledSE=FALSE,
                      dependentName = "", subjectName = "") {

  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) {
      sum(!is.na(x))
    } else {
      length(x)
    }
  }

  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  # First aggregate over unused RM factors, if desired:
  if (usePooledSE && measurevar == dependentName) {

    data <- plyr::ddply(data, c(subjectName, groupvars), plyr::summarise, dependent = mean(JaspColumn_.dependent._Encoded))
    names(data)[which(names(data) == "dependent")] <- measurevar

  } else if (usePooledSE && measurevar == paste0(dependentName, "_norm")) {

    data <- plyr::ddply(data, c(subjectName, groupvars), plyr::summarise, dependent = mean(JaspColumn_.dependent._Encoded_norm))
    names(data)[which(names(data) == "dependent")] <- measurevar
  }

  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                       .fun = function(xx, col) {
                         c(N    = length2(xx[[col]], na.rm=na.rm),
                           mean = mean   (xx[[col]], na.rm=na.rm),
                           sd   = sd     (xx[[col]], na.rm=na.rm)
                         )
                       },
                       measurevar
  )

  # Rename the "mean" column
  datac <- plyr::rename(datac, c("mean" = measurevar))

  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult

  if (errorBarType == "ci") {

    datac$ciLower <- datac[,measurevar] - datac[,"ci"]
    datac$ciUpper <- datac[,measurevar] + datac[,"ci"]

  } else {

    datac$ciLower <- datac[,measurevar] - datac[,"se"]
    datac$ciUpper <- datac[,measurevar] + datac[,"se"]

  }

  return(datac)
}

.normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL, na.rm=FALSE, .drop=TRUE) {

  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- plyr::ddply(data, c(idvar, betweenvars), .drop=.drop,
                               .fun = function(xx, col, na.rm) {
                                 c(subjMean = mean(xx[,col], na.rm=na.rm))
                               },
                               measurevar,
                               na.rm
  )

  # Put the subject means with original data
  data <- base::merge(data, data.subjMean)

  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)

  # Remove this subject mean column
  data$subjMean <- NULL

  return(data)
}

summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL, idvar=NULL, na.rm=FALSE,
                            conf.interval=.95, .drop=TRUE, errorBarType="ci", usePooledSE=FALSE,
                            dependentName = .BANOVAdependentName,
                            subjectName = .BANOVAsubjectName) {

  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars), na.rm=na.rm,
                     conf.interval=conf.interval, .drop=.drop, errorBarType=errorBarType, usePooledSE=usePooledSE,
                     dependentName = dependentName, subjectName = subjectName)
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  datac$ciLower <- NULL
  datac$ciUpper <- NULL

  # Norm each subject's data
  ndata <- .normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)

  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")

  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars), na.rm=na.rm, conf.interval=conf.interval, .drop=.drop, errorBarType=errorBarType,
                      usePooledSE=usePooledSE, dependentName = dependentName, subjectName = subjectName)

  # get distinct observations in a way that works for both character and factor/ ordered data
  .nDistinctObservations <- function(x) {
    if (is.character(x))
      return(length(unique(x)))
    else if (is.factor(x))
      return(nlevels(x))
    else
      stop("nDistinctObservations got an object of type", paste(class(x), collapse = ", "))
  }

  # Apply correction from Morey (2008) to the standard error and confidence interval
  # Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=.nDistinctObservations, FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )

  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor

  if (errorBarType == "ci") {

    ndatac$ciLower <- datac[,measurevar] - ndatac[,"ci"]
    ndatac$ciUpper <- datac[,measurevar] + ndatac[,"ci"]

  } else {

    ndatac$ciLower <- datac[,measurevar] - ndatac[,"se"]
    ndatac$ciUpper <- datac[,measurevar] + ndatac[,"se"]

  }

  # Combine the un-normed means with the normed results
  df <- merge(datac, ndatac)

  return(df)
}

.descriptivesPlotsRainCloudFill <- function(dataset, variable, groups, yLabel, xLabel, addLines, horiz, testValue) {
  # Adapted under the MIT license from:
  # van Langen, J. (2020). Open-visualizations in R and Python.
  # https://github.com/jorvlan/open-visualizations
  #
  # Permission is hereby granted, free of charge, to any person obtaining a copy
  # of this software and associated documentation files (the "Software"), to deal
  # in the Software without restriction, including without limitation the rights
  # to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  # copies of the Software, and to permit persons to whom the Software is
  # furnished to do so, subject to the following conditions:
  #
  # The above copyright notice and this permission notice shall be included in all
  # copies or substantial portions of the Software.

  dataset <- na.omit(dataset) # only applies when cases are excluded per dependent variable
  n   <- nrow(dataset)
  y   <- dataset[[variable]]
  grp <- factor(dataset[[groups]])
  x   <- as.numeric(as.factor(grp)) - 1
  xj  <- jitter(x, amount = 0.1)
  if (horiz) {
    xb <- x
    xj <- xj - 0.3
  } else {
    xb <- x*0.4 + max(xj) + 0.4
  }

  pointBoxDf <- data.frame(xj = xj, xb = xb, y = y, grp = grp)

  # no. points to fill in the line from the end points of the density
  # this line is essentially constant, but it is necessary to add multiple points in case someone
  # cuts of the end points with plot editing.
  noAddedPoints <- 2^7 # 128
  noDensityPoints <- 2^9 # 512, the default for density

  dens    <- tapply(y, as.factor(grp), density, n = noDensityPoints)
  xDens   <- unlist(lapply(dens, function(x) {
    xr <- range(x[["x"]])
    # note: the order (right to left) matters.
    c(x[["x"]], seq(xr[2L], xr[1L], length.out = noAddedPoints))
    }), use.names = FALSE)
  yDens   <- unlist(lapply(dens, function(x) {
    c(x[["y"]], rep(0, noAddedPoints))
  }), use.names = FALSE)

  yDensN  <- (yDens - min(yDens)) / (max(yDens) - min(yDens)) * 0.5
  grpDens <- rep(seq_along(dens), each = noDensityPoints + noAddedPoints) - 1

  if (horiz)
    yDensNpos <- yDensN + grpDens
  else
    yDensNpos <- yDensN + max(xb) + 0.4

  densDf <- data.frame(x = xDens, y = yDensNpos, grp = factor(grpDens))

  levels(pointBoxDf$grp) <- levels(densDf$grp)

  xMin    <- min(c(x, xj, xb, yDensNpos))
  xMax    <- max(c(x, xj, xb, yDensNpos))
  xLimits <- range(pretty(c(xMin, xMax)))
  xBreaks <- unique(x)
  yBreaks <- pretty(range(xDens))
  xLabels <- as.character(unique(grp))

  geomLine <- NULL
  if (addLines) {
    id <- numeric(n)
    for (g in unique(grp)) {
      idx     <- which(grp == g)
      id[idx] <- 1:length(idx)
    }
    pointBoxDf$id <- id
    geomLine <- ggplot2::geom_line(data  = pointBoxDf, mapping = ggplot2::aes(x = xj, y = y, group = id), color = 'gray')
  }

  coordFlip <- if (horiz) ggplot2::coord_flip() else NULL

  geomHline <- NULL
  if (length(levels(grp)) == 1) {
    xBreaks <- NULL
    xLabels <- NULL
    if (!is.null(testValue))
      geomHline <- ggplot2::geom_hline(data = data.frame(testValue), ggplot2::aes(yintercept = testValue), linetype = "dashed")
  }

  p <-
    ggplot2::ggplot() +
    geomLine +
    ggplot2::geom_point  (data = pointBoxDf, mapping = ggplot2::aes(x = xj, y = y, color = grp), size = 3) +
    ggplot2::geom_polygon(data = densDf,     mapping = ggplot2::aes(x = y,  y = x, fill = grp ), color = "black", alpha = 0.5) +
    ggplot2::stat_boxplot(data = pointBoxDf, mapping = ggplot2::aes(x = xb, y = y, group = grp), geom = "errorbar",  width = 0.1, size = 1) +
    ggplot2::geom_boxplot(data = pointBoxDf, mapping = ggplot2::aes(x = xb, y = y, fill = grp ), outlier.shape = NA, width = 0.2, size = 1) +
    coordFlip +
    geomHline +
    ggplot2::scale_y_continuous(name = yLabel, breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::scale_x_continuous(name = xLabel, breaks = xBreaks, labels = xLabels) +
    ggplot2::scale_fill_brewer(palette = "Dark2") +
    ggplot2::scale_color_brewer(palette = "Dark2")

  p <- jaspGraphs::themeJasp(p)

  return(p)
}

.ttestDescriptivesBarPlotFill <- function(dataset, options, variable) {

  pair <- NULL
  test <- NULL
  errorType <- options[["barPlotErrorType"]]
  groups <- if (!is.null(options[["group"]])) options[["group"]] else NULL

  errors <- .hasErrors(dataset,
                       message = 'short',
                       type = c('observations', 'variance', 'infinity'),
                       all.target = variable,
                       observations.amount = '< 2',
                       observations.grouping = if (!is.null(groups)) groups else NULL)
  if (!identical(errors, FALSE))
    stop(errors$message)

  # Creating data frames and summary data
  if (length(variable) != 1) {  # checks whether paired t-test is used
    pair <- ggplot2::scale_x_discrete(labels = c(variable[[1]], variable[[2]]))
    data <- data.frame(id = rep(1:length(dataset[[variable[[1]]]]), 2),
                       dependent = c(dataset[[variable[[1]]]], dataset[[variable[[2]]]]),
                       groupingVariable = c(rep(paste("1.", variable[[1]], sep = ""), length(dataset[[variable[[1]]]])),
                                            rep(paste("2.", variable[[2]], sep = ""), length(dataset[[variable[[2]]]]))),
                       stringsAsFactors = TRUE)
    summaryStat <- summarySEwithin(data,
                                   measurevar = "dependent",
                                   withinvars = "groupingVariable",
                                   idvar = "id",
                                   conf.interval = options[["barPlotCiLevel"]],
                                   na.rm = TRUE,
                                   .drop = FALSE,
                                   errorBarType = if (errorType == "ci") errorType else "se")
  } else {
    data <- data.frame(dependent = dataset[[variable]],
                       groupingVariable = if (!is.null(groups)) dataset[[groups]] else rep(variable, length(dataset[[variable]])))
    data <- na.omit(data)
    summaryStat <- summarySE(data,
                             measurevar = "dependent",
                             groupvars = "groupingVariable",
                             conf.interval = options[["barPlotCiLevel"]],
                             na.rm = TRUE,
                             .drop = FALSE,
                             errorBarType = if (errorType == "ci") errorType else "se")
  }
  ciPos <- c(summaryStat[["ciLower"]], summaryStat[["ciUpper"]])

  if (!is.null(options[["testValue"]])) {
    ciPos <- c(options[["testValue"]], ciPos)
    testValue <- data.frame(testValue = options[["testValue"]])
    test <- ggplot2::geom_hline(data = testValue, ggplot2::aes(yintercept = testValue), linetype = "dashed")
  }

  if (!is.null(groups)) {
    ylab <- ggplot2::ylab(unlist(variable))
    xlab <- ggplot2::xlab(groups)
  } else {
    ylab <- ggplot2::ylab(NULL)
    xlab <- ggplot2::xlab(NULL)
  }
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(if (options[["barPlotYAxisFixedToZero"]]) c(0, ciPos) else ciPos)
  pd <- ggplot2::position_dodge(0.2)
  pd2 <- ggplot2::position_dodge2(preserve = "single")

  p <- ggplot2::ggplot(summaryStat, ggplot2::aes(x = groupingVariable, y = dependent, group = 1)) +
    ggplot2::geom_hline(yintercept = 0, color = "#858585", size = 0.3) +
    ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", width = .6, position = pd2) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = ciLower,  ymax = ciUpper), colour = "black", width = 0.2, position = pd) +
    test +
    ylab +
    xlab +
    pair +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks), oob = scales::rescale_none) +
    jaspGraphs::geom_rangeframe(sides = "l") +
    jaspGraphs::themeJaspRaw()

  return(p)
}
