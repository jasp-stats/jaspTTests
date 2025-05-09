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

TTestOneSampleInternal <- function(jaspResults, dataset = NULL, options, ...) {
  ready <- length(options$dependent) > 0
  type  <- "one-sample"
  if(ready) {
    dataset <- .ttestReadData(dataset, options, type)
    .ttestCheckErrors(        dataset, options, type)
  }
  # Output tables (each calls its own results function)
  .ttestOneSampleMainTable(  jaspResults, dataset, options, ready, type)
  .ttestOneSampleNormalTable(jaspResults, dataset, options, ready, type)
  .ttestQQPlot(              jaspResults, dataset, options, ready, type)

  # Descriptives
  vars <- unique(unlist(options$dependent))
  .ttestDescriptivesTable(                 jaspResults, dataset, options, ready, vars)
  .ttestOneSampleDescriptivesPlot(         jaspResults, dataset, options, ready)
  .ttestOneSampleDescriptivesRainCloudPlot(jaspResults, dataset, options, ready)
  .ttestOneSampleDescriptivesBarPlot(      jaspResults, dataset, options, ready)


  return()
}

.ttestOneSampleMainTable <- function(jaspResults, dataset, options, ready, type) {
  if (!is.null(jaspResults[["ttest"]]))
    return()

  optionsList <- .ttestOptionsList(options, type)

  # Create table
  ttest <- createJaspTable(title = gettext("One Sample T-Test"))
  ttest$dependOn(c("effectSize", "effectSizeCi",
                   "dependent", "effectSizeCiLevel", "student", "wilcoxon",
                   "meanDifference", "meanDifferenceCi", "zTestSd",
                   "meanDifferenceCiLevel", "alternative",
                   "vovkSellke", "naAction", "zTest", "testValue"))
  ttest$showSpecifiedColumnsOnly <- TRUE
  ttest$position <- 1

  if (optionsList$wantsWilcox && optionsList$onlyTest) {
    ttest$addFootnote(gettext("Wilcoxon signed-rank test."))
    testStat                <- "V"
    testStatName            <- gettext("V")
    nameOfLocationParameter <- gettext("Hodges-Lehmann Estimate")
    nameOfEffectSize        <- gettext("Rank-Biserial Correlation")
  } else if (optionsList$wantsStudents && optionsList$onlyTest) {
    ttest$addFootnote(gettext("Student's t-test."))
    testStat                <- "t"
    testStatName            <- gettext("t")
    nameOfLocationParameter <- gettext("Mean Difference")
    nameOfEffectSize        <- gettext("Cohen's d")
  } else if(optionsList$wantsZtest && optionsList$onlyTest){
    ttest$addFootnote(gettext("Z test."))
    testStat                <- "Z"
    testStatName            <- gettext("Z")
    nameOfLocationParameter <- gettext("Mean Difference")
    nameOfEffectSize        <- gettext("Cohen's d")
  } else {
    testStat                <- "Statistic"
    testStatName            <- gettext("Statistic")
    nameOfLocationParameter <- gettext("Location Difference")
    nameOfEffectSize        <- gettext("Effect Size")
  }

  ttest$addColumnInfo(name = "v",     type = "string", title = "", combine = TRUE)

  ## if the user wants more than one test, add a column called "Test"
  if (sum(optionsList$allTests) > 1)
    ttest$addColumnInfo(name = "test", type = "string", title = gettext("Test"))

  ttest$addColumnInfo(name = testStat, type = "number", title = testStatName)

  if (optionsList$wantsStudents)
    ttest$addColumnInfo(name = "df", type = "integer", title = gettext("df"))

  ttest$addColumnInfo(name = "p", type = "pvalue", title = gettext("p"))

  .ttestVovkSellke(ttest, options)

  if (optionsList$wantsStudents && optionsList$wantsZtest)
    testInNote <- gettext("Student t-test and Z-test")
  else if (optionsList$wantsStudents)
    testInNote <- gettext("Student t-test")
  else if (optionsList$wantsZtest)
    testInNote <- gettext("Z-test")

  if (optionsList$wantsDifference) {
    ttest$addColumnInfo(name = "m", title = nameOfLocationParameter, type = "number")

    if ((optionsList$wantsStudents || optionsList$wantsZtest) && optionsList$wantsWilcox) {
      ttest$addFootnote(gettextf(
        "For the %s, location difference estimate is given by the sample mean difference <em>d</em>. For the Wilcoxon test, location difference estimate is given by the Hodges-Lehmann estimate.", testInNote
      ))
    }
  }

  if (optionsList$wantsConfidenceMeanDiff) {
    title <- gettextf("%1$s%% CI for %2$s", 100 * optionsList$percentConfidenceMeanDiff, nameOfLocationParameter)
    ttest$addColumnInfo(name = "lowerCIlocationParameter", type = "number", title = gettext("Lower"), overtitle = title)
    ttest$addColumnInfo(name = "upperCIlocationParameter", type = "number", title = gettext("Upper"), overtitle = title)
  }

  if (optionsList$wantsEffect) {
    ttest$addColumnInfo(name = "d",            title = nameOfEffectSize,                      type = "number")
    ttest$addColumnInfo(name = "effectSizeSe", title = gettextf("SE %1$s", nameOfEffectSize), type = "number")
    if (sum(optionsList$wantsStudents, optionsList$wantsWilcox, optionsList$wantsZtest) > 1) {
      tNote <- wNote <- zNote <- NULL

      if (optionsList$wantsStudents && (optionsList$wantsZtest || optionsList$wantsWilcox))
        tNote <- gettext("For the Student t-test, effect size is given by Cohen's <em>d</em>.")

      if (optionsList$wantsWilcox && (optionsList$wantsZtest || optionsList$wantsStudents))
        wNote <- gettext("For the Wilcoxon test, effect size is given by the matched rank biserial correlation.")

      if (optionsList$wantsZtest && (optionsList$wantsStudents || optionsList$wantsWilcox))
        zNote <- gettext("For the Z test, effect size is given by Cohen's <em>d</em> (based on the provided population standard deviation).")

      ttest$addFootnote(paste(tNote, wNote, zNote))
    }
  }

  if (optionsList$wantsConfidenceEffSize) {
    title <- gettextf("%1$s%% CI for %2$s", 100 * optionsList$percentConfidenceEffSize, nameOfEffectSize)
    ttest$addColumnInfo(name = "lowerCIeffectSize", type = "number", title = gettext("Lower"), overtitle = title)
    ttest$addColumnInfo(name = "upperCIeffectSize", type = "number", title = gettext("Upper"), overtitle = title)
  }

  ### check the directionality
  if (options$alternative == "greater") {
    directionFootnote <- gettext("greater than")
    direction <- "greater"
  } else if (options$alternative == "less") {
    directionFootnote <- gettext("less than")
    direction <- "less"
  } else {
    directionFootnote <- gettext("different from")
    direction <- "two.sided"
  }

  if (optionsList$wantsStudents || optionsList$wantsWilcox || optionsList$wantsZtest) {
    tMessage <- wMessage <- NULL

    if (optionsList$wantsStudents || optionsList$wantsZtest)
      tMessage <- gettextf("For the %1$s, the alternative hypothesis specifies that the mean is %2$s %3$s.", testInNote, directionFootnote, options$testValue)

    if (optionsList$wantsWilcox)
      wMessage <- gettextf("For the Wilcoxon test, the alternative hypothesis specifies that the median is %1$s %2$s.", directionFootnote, options$testValue)

    ttest$addFootnote(paste(tMessage, wMessage))
  }

  jaspResults[["ttest"]] <- ttest

  if (ready)
    .ttestOneSampleMainFill(ttest, dataset, options, testStat, optionsList)
}

.ttestOneSampleNormalTable <- function(jaspResults, dataset, options, ready, type) {
  # Container
  .ttestAssumptionCheckContainer(jaspResults, options, type)
  container <- jaspResults[["AssumptionChecks"]]
  if (!options$normalityTest || !is.null(container[["ttestNormalTable"]]))
    return()
  container <- jaspResults[["AssumptionChecks"]]
  # Create table
  ttestNormalTable <- createJaspTable(title = gettext("Test of Normality (Shapiro-Wilk)"))
  ttestNormalTable$showSpecifiedColumnsOnly <- TRUE
  ttestNormalTable$position <- 2

  ttestNormalTable$addColumnInfo(name = "v", title = "",  type = "string")
  ttestNormalTable$addColumnInfo(name = "W", title = gettext("W"), type = "number")
  ttestNormalTable$addColumnInfo(name = "p", title = gettext("p"), type = "pvalue")

  message <- gettext("Significant results suggest a deviation from normality.")
  ttestNormalTable$addFootnote(message)

  container[["ttestNormalTable"]] <- ttestNormalTable

  if (ready)
    .ttestOneSampleNormalFill(ttestNormalTable, dataset, options)
}

.ttestOneSampleMainFill <-function(table, dataset, options, testStat, optionsList) {
  variables <- options$dependent
  for (variable in variables) {

    errors <- .hasErrors(dataset,
                         message = 'short',
                         type = c('observations', 'variance', 'infinity'),
                         all.target = variable,
                         observations.amount = '< 2')

    for (test in optionsList$whichTests) {

      row     <- list(v = variable, test = test, .isNewGroup = .ttestRowIsNewGroup(test, optionsList$whichTests))
      rowName <- paste(test, variable, sep = "-")

      errorMessage <- NULL
      if (identical(errors, FALSE)) {
        rowResults <- try(.ttestOneSampleComputeMainTableRow(variable, dataset, test, testStat, optionsList, options))

          if (!isTryError(rowResults))
            row <- c(row, rowResults)
          else
            errorMessage <- .extractErrorMessage(rowResults)

      } else {
        errorMessage <- errors$message
      }

      if (!is.null(errorMessage)) {
        row[[testStat]] <- NaN
        table$addFootnote(errorMessage, colNames = testStat, rowNames = rowName)
      }
      if (all(is.na(c(rowResults[["lowerCIeffectSize"]], rowResults[["lowerCIeffectSize"]]))))
        table$addFootnote(gettext("CI could not be computed for effect size, due to low sample size and/or extreme effect size."))

      if (!(rowResults[["usedConfLevel"]] == options[["meanDifferenceCiLevel"]]))
        table$addFootnote(gettextf("Sample size too small for desired confidence level. Using %.1f%% instead", rowResults[["usedConfLevel"]]*100))
      table$addRows(row, rowNames = rowName)
    }
  }
}

.ttestOneSampleComputeMainTableRow <- function(variable, dataset, test, testStat, optionsList, options) {
  direction <- switch(options[["alternative"]],
                      "twoSided"  ="two.sided",
                      "greater" ="greater",
                      "less"    ="less")
  dat <- na.omit(dataset[[ .v(variable) ]])
  n   <- length(dat)
  usedConfLevel <- options[["meanDifferenceCiLevel"]]
  if (test == "Wilcoxon") {
    tempResult <- stats::wilcox.test(dat, alternative = direction, mu = options[["testValue"]],
                                     conf.level = optionsList[["percentConfidenceMeanDiff"]], conf.int = TRUE)
    usedConfLevel <- attr(tempResult$conf.int, "conf.level")
    df   <- ifelse(is.null(tempResult[["parameter"]]), "", as.numeric(tempResult[["parameter"]]))
    nd   <- sum(dat != options[["testValue"]])
    maxw <- (nd * (nd + 1)) / 2
    d    <- as.numeric((tempResult[["statistic"]] / maxw) * 2 - 1)
    wSE  <- sqrt((nd * (nd + 1) * (2 * nd + 1)) / 6) /2
    mrSE <- sqrt(wSE^2  * 4 * (1 / maxw^2))
    # zSign <- (ww$statistic - ((n*(n+1))/4))/wSE
    zmbiss <- atanh(d)

    if(direction == "two.sided")
      confIntEffSize <- sort(c(tanh(zmbiss + qnorm((1-optionsList[["percentConfidenceEffSize"]])/2)*mrSE),
                               tanh(zmbiss + qnorm((1+optionsList[["percentConfidenceEffSize"]])/2)*mrSE)))
    else if (direction == "less")
      confIntEffSize <- sort(c(-Inf, tanh(zmbiss + qnorm(optionsList[["percentConfidenceEffSize"]])*mrSE)))
    else if (direction == "greater")
      confIntEffSize <- sort(c(tanh(zmbiss + qnorm((1-optionsList[["percentConfidenceEffSize"]]))*mrSE), Inf))

    effectSizeSe <- tanh(mrSE)
    if (confIntEffSize[1] == confIntEffSize[2]) confIntEffSize <- c(NA, NA)

  } else if (test == "Z"){
    tempResult <- .z.test("x"=dat, "alternative" = direction,
                          "mu" = options[["testValue"]], "sigma.x" = options[["zTestSd"]],
                          "ciValueMeanDiff"=optionsList[["percentConfidenceMeanDiff"]],
                          "ciValueESMeanDiff"=optionsList[["percentConfidenceEffSize"]])

    df <- ""
    d  <- tempResult[["d"]]
    effectSizeSe <- sqrt((1/n)+(as.numeric(d)^2 / (2*n)))
    confIntEffSize <- tempResult[["confIntEffSize"]]


  } else {
    tempResult <- stats::t.test(dat, alternative = direction, mu = options[["testValue"]],
                                conf.level = optionsList[["percentConfidenceMeanDiff"]])
    df <- ifelse(is.null(tempResult[["parameter"]]), "", as.numeric(tempResult[["parameter"]]))
    d  <- (mean(dat) - options[["testValue"]]) / sd(dat)
    t  <- as.numeric(tempResult[["statistic"]])

    effectSizeSe <- sqrt((1/n)+(as.numeric(d)^2 / (2*n)))
    #Introduction to Meta-Analysis. Michael Borenstein, L. V. Hedges, J. P. T. Higgins and H. R. Rothstein (2009). Chapter 4, equation (4.28(with r = .5))

    confIntEffSize <- c(0,0)


    if (optionsList[["wantsConfidenceEffSize"]]) {

      ciEffSize  <- options[["effectSizeCiLevel"]]
      alphaLevel <- ifelse(direction == "two.sided", 1 - (ciEffSize + 1) / 2, 1 - ciEffSize)

      confIntEffSize <- .confidenceLimitsEffectSizes(ncp = d * sqrt(n), df = df, alpha.lower = alphaLevel,
                                                     alpha.upper = alphaLevel)[c(1, 3)]
      confIntEffSize <- unlist(confIntEffSize) / sqrt(n)

      if (direction == "greater")
        confIntEffSize[2] <- Inf
      else if (direction == "less")
        confIntEffSize[1] <- -Inf

      confIntEffSize <- sort(confIntEffSize)
    }
  }

  ## same for all tests
  p     <- as.numeric(tempResult[["p.value"]])
  stat  <- as.numeric(tempResult[["statistic"]])

  if (test=="Z") {
    ciLow <- as.numeric(tempResult[["conf.int"]][1])
    ciUp  <- as.numeric(tempResult[["conf.int"]][2])
    m     <- as.numeric(tempResult[["estimate"]])
  } else {
    m     <- as.numeric(tempResult[["estimate"]] - tempResult[["null.value"]])
    ciLow <- as.numeric(tempResult[["conf.int"]][1] - tempResult[["null.value"]])
    ciUp  <- as.numeric(tempResult[["conf.int"]][2] - tempResult[["null.value"]])
  }

  ciLowEffSize <- as.numeric(confIntEffSize[1])
  ciUpEffSize  <- as.numeric(confIntEffSize[2])
  effectSizeSe <- as.numeric(effectSizeSe)

  if (suppressWarnings(is.na(t)))  # do not throw warning when test stat is not 't'
    stop("data are essentially constant")

  result <- list(df = df, p = p, m = m, d = d,
                 lowerCIlocationParameter = ciLow, upperCIlocationParameter = ciUp,
                 lowerCIeffectSize = ciLowEffSize, upperCIeffectSize = ciUpEffSize,
                 effectSizeSe = effectSizeSe, usedConfLevel = usedConfLevel)
  result[[testStat]] <- stat

  if (options[["vovkSellke"]])
    result[["VovkSellkeMPR"]] <- VovkSellkeMPR(p)

  return(result)
}

.ttestOneSampleNormalFill <- function(table, dataset, options) {
  variables <- options[["dependent"]]
  for (variable in variables) {
    row <- list(v = variable)

    errors <- .hasErrors(dataset,
                         message = 'short',
                         type = c('observations', 'variance', 'infinity'),
                         all.target = variable,
                         observations.amount = c('< 3', '> 5000'))

    if (!identical(errors, FALSE)) {
      row[["W"]] <- NaN
      table$addFootnote(errors$message, colNames = "W", rowNames = variable)
    } else {
      data <- na.omit(dataset[[.v(variable)]])

      tempResult <- stats::shapiro.test(data)
      row[["W"]] <- as.numeric(tempResult[["statistic"]])
      row[["p"]] <- tempResult[["p.value"]]
    }

    table$addRows(row, rowNames = variable)
  }
}

.ttestOneSampleDescriptivesPlot <- function(jaspResults, dataset, options, ready) {
  if(!options$descriptivesPlot)
    return()
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]

  if (is.null(container[["plots"]])) {
    subcontainer <- createJaspContainer(gettext("Descriptives Plots"), dependencies = c("descriptivesPlot", "descriptivesPlotCi", "testValue"))
    subcontainer$position <- 5
    container[["plots"]] <- subcontainer
  } else {
    subcontainer <- container[["plots"]]
  }

  for(variable in options$dependent) {
    if(!is.null(subcontainer[[variable]]))
      next
    descriptivesPlot <- createJaspPlot(title = variable, width = 480, height = 320)
    descriptivesPlot$dependOn(optionContainsValue = list(variables = variable))
    subcontainer[[variable]] <- descriptivesPlot
    if(ready){
      p <- try(.ttestOneSampleDescriptivesPlotFill(dataset, options, variable))
      if(isTryError(p))
        descriptivesPlot$setError(.extractErrorMessage(p))
      else
        descriptivesPlot$plotObject <- p
    }
  }
  return()
}

.ttestOneSampleDescriptivesPlotFill <- function(dataset, options, variable) {
  errors <- .hasErrors(dataset,
                       message = 'short',
                       type = c('observations', 'variance', 'infinity'),
                       all.target = variable,
                       observations.amount = c('< 2'))
  if (!isFALSE(errors))
    stop(errors$message)

  dataSubset <- data.frame(
    dependent = dataset[[variable]],
    group = rep(variable, nrow(dataset))
  )

  summaryStat <- .summarySE(
    dataSubset,
    measurevar    = "dependent",
    groupvars     = "group",
    conf.interval = options[["descriptivesPlotCiLevel"]],
    na.rm         = TRUE,
    .drop         = FALSE
  )

  p <- jaspGraphs::descriptivesPlot(
    x                      = summaryStat[["group"]],
    y                      = summaryStat[["dependent"]],
    ciLower                = summaryStat[["ciLower"]],
    ciUpper                = summaryStat[["ciUpper"]],
    group                  = summaryStat[["group"]],
    noXLevelNames          = FALSE,
    horizontalLine         = options[["testValue"]],
    horizontalLineLineType = "dashed"
  ) + jaspGraphs::themeJaspRaw(axis.title.cex = jaspGraphs::getGraphOption("axis.title.cex"))

  return(p)
}

.ttestOneSampleDescriptivesRainCloudPlot <- function(jaspResults, dataset, options, ready) {
  if(!options$raincloudPlot)
    return()
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]

  if (is.null(container[["plotsRainCloud"]])) {
    subcontainer <- createJaspContainer(gettext("Raincloud Plots"), dependencies = c("raincloudPlot", "raincloudPlotHorizontal", "testValue"))
    subcontainer$position <- 6
    container[["plotsRainCloud"]] <- subcontainer
  } else {
    subcontainer <- container[["plotsRainCloud"]]
  }

  horiz <- options$raincloudPlotHorizontal
  if(ready){
    errors <- .ttestBayesianGetErrorsPerVariable(dataset, options, "one-sample")
    for(variable in options$dependent) {
      if(!is.null(subcontainer[[variable]]))
        next
      descriptivesPlotRainCloud <- createJaspPlot(title = variable, width = 480, height = 320)
      descriptivesPlotRainCloud$dependOn(optionContainsValue = list(variables = variable))
      subcontainer[[variable]] <- descriptivesPlotRainCloud
      if(!isFALSE(errors[[variable]])) {
          descriptivesPlotRainCloud$setError(errors[[variable]]$message)
          next
      }
      groups  <- rep("1", nrow(dataset))
      subData <- data.frame(dependent = dataset[, .v(variable)], groups = groups)
      p <- try(.descriptivesPlotsRainCloudFill(subData, "dependent", "groups", variable, NULL, addLines = FALSE, horiz, options$testValue))
      if(isTryError(p))
        descriptivesPlotRainCloud$setError(.extractErrorMessage(p))
      else
        descriptivesPlotRainCloud$plotObject <- p
    }
  }
  return()
}

.ttestOneSampleDescriptivesBarPlot <- function(jaspResults, dataset, options, ready) {
  if (!options[["barPlot"]])
    return()
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]

  if (is.null(container[["barPlots"]])) {
    subcontainer <- createJaspContainer(gettext("Bar Plots"), dependencies = c("barPlot",
                                                                               "barPlotCiLevel",
                                                                               "barPlotErrorType",
                                                                               "testValue",
                                                                               "barPlotYAxisFixedToZero"))
    subcontainer$position <- 7
    container[["barPlots"]] <- subcontainer
  } else {
    subcontainer <- container[["barPlots"]]
  }

  for (variable in options[["dependent"]]) {
    if (!is.null(subcontainer[[variable]]))
      next
    descriptivesBarPlot <- createJaspPlot(title = variable, width = 480, height = 320)
    descriptivesBarPlot$dependOn(optionContainsValue = list(variables = variable))
    subcontainer[[variable]] <- descriptivesBarPlot
    if (ready) {
      p <- try(.ttestDescriptivesBarPlotFill(dataset, options, variable))
      if (isTryError(p))
        descriptivesBarPlot$setError(.extractErrorMessage(p))
      else
        descriptivesBarPlot$plotObject <- p
    }
  }
  return()
}

