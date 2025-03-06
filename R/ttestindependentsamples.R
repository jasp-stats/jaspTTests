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

TTestIndependentSamplesInternal <- function(jaspResults, dataset = NULL, options, ...) {
  #at least one variable and one grouping variable
  ready <- length(options$dependent) > 0 && options$group != ""
  type  <- "independent"
  if(ready) {
    dataset <- .ttestReadData(dataset, options, type)
    .ttestCheckErrors(        dataset, options, type)
  }
  # Output tables (each calls its own results function)
  .ttestIndependentMainTable(  jaspResults, dataset, options, ready, type)
  .ttestIndependentNormalTable(jaspResults, dataset, options, ready, type)
  .ttestQQPlot(                jaspResults, dataset, options, ready, type)
  .ttestIndependentEqVarTable( jaspResults, dataset, options, ready, type)
  # Descriptives
  .ttestIndependentDescriptivesTable(        jaspResults, dataset, options, ready)
  .ttestIndependentDescriptivesPlot(         jaspResults, dataset, options, ready)
  .ttestIndependentDescriptivesRainCloudPlot(jaspResults, dataset, options, ready)
  .ttestIndependentDescriptivesBarPlot(      jaspResults, dataset, options, ready)

  return()
}

.ttestIndependentMainTable <- function(jaspResults, dataset, options, ready, type) {
  if (!is.null(jaspResults[["ttest"]]))
    return()

  optionsList <- .ttestOptionsList(options, type)

  # Create table
  ttest <- createJaspTable(title = gettext("Independent Samples T-Test"))
  ttest$dependOn(c("effectSize", "effectSizeCi", "dependent",
                   "effectSizeCiLevel", "student",
                   "meanDifference", "meanDifferenceCi",
                   "meanDifferenceCiLevel", "alternative",
                   "vovkSellke", "naAction", "group", "effectSizeType",
                   "welch", "mannWhitneyU", "equalityOfVariancesTest", "equalityOfVariancesTestType"))
  ttest$showSpecifiedColumnsOnly <- TRUE
  ttest$position <- 1

  if (optionsList$wantsWilcox && optionsList$onlyTest) {
    ttest$addFootnote(gettext("Mann-Whitney U test."))
    testStat <- "U"
    testStatName <- gettext("U")
  } else if (optionsList$wantsWelchs && optionsList$onlyTest) {
    ttest$addFootnote(gettext("Welch's t-test."))
    testStat <- "t"
    testStatName <- gettext("t")
  } else if (optionsList$wantsStudents && optionsList$onlyTest) {
    ttest$addFootnote(gettext("Student's t-test."))
    testStat     <- "t"
    testStatName <- gettext("t")
  } else {
    testStat     <- "Statistic"
    testStatName <- gettext("Statistic")
  }

  dfType <- ifelse(optionsList$wantsWelchs, "number", "integer")

  ttest$addColumnInfo(name = "v", title = " ", type = "string", combine = TRUE)

  if (sum(optionsList$allTests) >= 2)
    ttest$addColumnInfo(name = "test", type = "string",  title = gettext("Test"))

  ttest$addColumnInfo(name = testStat, type = "number",  title = testStatName)
  ttest$addColumnInfo(name = "df",     type = dfType,    title = gettext("df"))
  ttest$addColumnInfo(name = "p",      type = "pvalue",  title = gettext("p"))

  .ttestVovkSellke(ttest, options)

  if (options$effectSizeType == "cohen")
    effSize <- "cohen"
  else if (options$effectSizeType == "glass")
    effSize <- "glass"
  else if (options$effectSizeType == "hedges")
    effSize <- "hedges"

  nameOfEffectSizeParametric <- switch(effSize,
                                       cohen  = gettext("Cohen's d"),
                                       glass  = gettext("Glass' delta"),
                                       hedges = gettext("Hedges' g"))

  if (!optionsList$wantsWilcox) {
    nameOfLocationParameter <- gettext("Mean Difference")
    nameOfEffectSize        <- nameOfEffectSizeParametric
  } else if (optionsList$wantsWilcox && optionsList$onlyTest) {
    nameOfLocationParameter <- gettext("Hodges-Lehmann Estimate")
    nameOfEffectSize        <- gettext("Rank-Biserial Correlation")
  } else if (optionsList$wantsWilcox && (optionsList$wantsStudents || optionsList$wantsWelchs)) {
    nameOfLocationParameter <-  gettext("Location Parameter")
    nameOfEffectSize        <-  gettext("Effect Size")
  }

  if (optionsList$wantsStudents && optionsList$wantsWelchs)
    testInNote <- gettext("Student t-test and Welch t-test")
  else if (optionsList$wantsStudents)
    testInNote <- gettext("Student t-test")
  else if (optionsList$wantsWelchs)
    testInNote <- gettext("Welch t-test")

  ## add mean difference and standard error difference
  if (optionsList$wantsDifference) {
    ttest$addColumnInfo(name = "md", title = nameOfLocationParameter, type = "number")

    if (!(optionsList$wantsWilcox && optionsList$onlyTest))  # Only add SE Difference if not only MannWhitney is requested
      ttest$addColumnInfo(name = "sed", title = gettext("SE Difference"), type = "number")

    if (optionsList$wantsWilcox && (optionsList$wantsStudents || optionsList$wantsWelchs))
      ttest$addFootnote(gettextf("For the %s, location parameter is given by mean difference. For the Mann-Whitney test, location parameter is given by the Hodges-Lehmann estimate.", testInNote))
  }

  if (optionsList$wantsConfidenceMeanDiff) {
    title <- gettextf("%1$s%% CI for %2$s", 100 * optionsList$percentConfidenceMeanDiff, nameOfLocationParameter)
    ttest$addColumnInfo(name = "lowerCIlocationParameter", type = "number", title = gettext("Lower"), overtitle = title)
    ttest$addColumnInfo(name = "upperCIlocationParameter", type = "number", title = gettext("Upper"), overtitle = title)
  }

  ## add Cohen's d
  if (optionsList$wantsEffect) {
    ttest$addColumnInfo(name = "d",            title = nameOfEffectSize,                      type = "number")
    ttest$addColumnInfo(name = "effectSizeSe", title = gettextf("SE %1$s", nameOfEffectSize), type = "number")

    if (optionsList$wantsWilcox) {
      wNote <- gettext("For the Mann-Whitney test, effect size is given by the rank biserial correlation.")

      twNote <- NULL
      if (optionsList$wantsStudents || optionsList$wantsWelchs)
        twNote <- gettextf("For the %1$s, effect size is given by %2$s.", testInNote, nameOfEffectSizeParametric)

      ttest$addFootnote(paste(twNote, wNote))
    }
  }

  if (optionsList$wantsConfidenceEffSize) {
    title <- gettextf("%1$s%% CI for %2$s", 100 * optionsList$percentConfidenceEffSize, nameOfEffectSize)
    ttest$addColumnInfo(name = "lowerCIeffectSize", type = "number", title = gettext("Lower"), overtitle = title)
    ttest$addColumnInfo(name = "upperCIeffectSize", type = "number", title = gettext("Upper"), overtitle = title)
  }

  jaspResults[["ttest"]] <- ttest

  if (ready)
    .ttestIndependentMainFill(ttest, dataset, options, testStat, optionsList)
}

.ttestIndependentNormalTable <- function(jaspResults, dataset, options, ready, type) {
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

  ttestNormalTable$addColumnInfo(name = "dep", type = "string", title = gettext("Residuals"))
  ttestNormalTable$addColumnInfo(name = "W",   type = "number", title = gettext("W"))
  ttestNormalTable$addColumnInfo(name = "p",   type = "pvalue", title = gettext("p"))

  message <- gettext("Significant results suggest a deviation from normality.")
  ttestNormalTable$addFootnote(message)

  container[["ttestNormalTable"]] <- ttestNormalTable

  if (ready)
    .ttestIndependentNormalFill(ttestNormalTable, dataset, options)
}

.ttestIndependentEqVarTable <- function(jaspResults, dataset, options, ready, type){
  # Container
  .ttestAssumptionCheckContainer(jaspResults, options, type)
  container <- jaspResults[["AssumptionChecks"]]

  if (!options$equalityOfVariancesTest || !is.null(container[["equalityVariance"]]))
    return()

  nameOfEqVarTest <- switch(options$equalityOfVariancesTestType,
                            "brownForsythe" = gettext("Brown-Forsythe"),
                            "levene" = gettext("Levene's"))

  # Create table
  equalityVariance <- createJaspTable(title = gettextf("Test of Equality of Variances (%1$s)", nameOfEqVarTest))
  equalityVariance$dependOn(c("equalityOfVariancesTestType"))
  equalityVariance$showSpecifiedColumnsOnly <- TRUE
  equalityVariance$position <- 3
  equalityVariance$addColumnInfo(name = "variable", type = "string",  title = "")
  equalityVariance$addColumnInfo(name = "fStat",    type = "number",  title = gettext("F"))
  equalityVariance$addColumnInfo(name = "dfOne",    type = "integer", title = gettextf("%s<sub>1</sub>", "df"))
  equalityVariance$addColumnInfo(name = "dfTwo",    type = "integer", title = gettextf("%s<sub>2</sub>", "df"))
  equalityVariance$addColumnInfo(name = "p",        type = "pvalue",  title = gettext("p"))

  container[["equalityVariance"]] <- equalityVariance

  if (ready)
    .ttestIndependentEqVarFill(equalityVariance, dataset, options)
}

.ttestIndependentMainFill <- function(table, dataset, options, testStat, optionsList) {

  if (options$effectSizeType == "cohen")
    effSize <- "cohen"
  else if (options$effectSizeType == "glass")
    effSize <- "glass"
  else if (options$effectSizeType == "hedges")
    effSize <- "hedges"

  levels <- levels(dataset[[ options$group ]])

  if (options$alternative == "greater" || options$alternative == "less") {
    directionNote <- ifelse(options$alternative == "greater", gettext("greater"), gettext("less"))
    table$addFootnote(gettextf("For all tests, the alternative hypothesis specifies that group %1$s is %2$s than group %3$s.",
                                                paste("<em>", levels[1], "</em>"), directionNote, paste("<em>", levels[2], "</em>")))
  }

  ## for each variable specified, run each test that the user wants
  for (variable in options$dependent) {
    errors <- .hasErrors(dataset,
                         message = 'short',
                         type = c('observations', 'variance', 'infinity'),
                         all.target = variable,
                         all.grouping = options$group,
                         observations.amount = '< 2')

    for (test in optionsList$whichTests) {

      row     <- list(v = variable, test = test, .isNewGroup = .ttestRowIsNewGroup(test, optionsList$whichTests))
      rowName <- paste(test, variable, sep = "-")

      errorMessage <- NULL
      if (identical(errors, FALSE)) {
        result <- try(ttestIndependentMainTableRow(variable, dataset, test, testStat, effSize, optionsList, options))

        if (!isTryError(result)) {
          row <- c(row, result[["row"]])
          if (result[["leveneViolated"]] && options$equalityOfVariancesTestType == "brownForsythe")
            table$addFootnote(gettext("Brown-Forsythe test is significant (p < .05), suggesting a violation of the equal variance assumption"), colNames = "p", rowNames = rowName)
          else if (result[["leveneViolated"]] && options$equalityOfVariancesTestType == "levene")
            table$addFootnote(gettext("Levene's test is significant (p < .05), suggesting a violation of the equal variance assumption"), colNames = "p", rowNames = rowName)
        } else {
          errorMessage <- .extractErrorMessage(result)
        }

      } else {
        errorMessage <- errors$message
      }

      if (!is.null(errorMessage)) {
        row[[testStat]] <- NaN
        table$addFootnote(errorMessage, colNames = testStat, rowNames = rowName)
      }

      table$addRows(row, rowNames = rowName)
    }

    if (effSize == "glass") {
      ns  <- tapply(dataset[[variable]], dataset[[options$group]], function(x) length(na.omit(x)))
      sdMessage <- gettextf("Glass' delta uses the standard deviation of group %1$s of variable %2$s.", names(ns[2]), options$group)
      table$addFootnote(sdMessage)
    }
  }
}

ttestIndependentMainTableRow <- function(variable, dataset, test, testStat, effSize, optionsList, options) {
  ciEffSize  <- optionsList$percentConfidenceEffSize
  ciMeanDiff <- optionsList$percentConfidenceMeanDiff
  f <- as.formula(paste(variable, "~",
                        options$group))

  variableData <- dataset[[ variable ]]
  groupingData <- dataset[[ options$group ]]

  sds <- tapply(variableData, groupingData, sd, na.rm = TRUE)
  ms  <- tapply(variableData, groupingData, mean, na.rm = TRUE)
  ns  <- tapply(variableData, groupingData, function(x) length(na.omit(x)))

  direction <- .ttestMainGetDirection(options$alternative)

  if (test == "Mann-Whitney") {
    r <- stats::wilcox.test(f, data = dataset,
                            alternative = direction,
                            conf.int = TRUE, conf.level = ciMeanDiff)
    df   <- ""
    sed  <- ""
    stat <- as.numeric(r$statistic)
    m    <- as.numeric(r$estimate)
    d    <- as.numeric(1-(2*stat)/(ns[1]*ns[2])) # rankBis (Kerby, 2014)
    wSE <- sqrt((ns[1]*ns[2] * (ns[1]+ns[2] + 1))/12)
    rankBisSE <- sqrt(4 * 1/(ns[1]*ns[2])^2 * wSE^2)
    zRankBis  <- atanh(d)

    if(direction == "two.sided")
      confIntEffSize <- sort(c(tanh(zRankBis + qnorm((1-ciEffSize)/2)*rankBisSE),
                               tanh(zRankBis + qnorm((1+ciEffSize)/2)*rankBisSE)))
    else if (direction == "less")
      confIntEffSize <- sort(c(-Inf, tanh(zRankBis + qnorm(ciEffSize)*rankBisSE)))
    else if (direction == "greater")
      confIntEffSize <- sort(c(tanh(zRankBis + qnorm((1-ciEffSize))*rankBisSE), Inf))

    effectSizeSe <- tanh(rankBisSE)
  } else {
    r <- stats::t.test(f, data = dataset, alternative = direction,
                       var.equal = test != "Welch", conf.level = ciMeanDiff)

    df   <- as.numeric(r$parameter)
    m    <- as.numeric(r$estimate[1]) - as.numeric(r$estimate[2])
    stat <- as.numeric(r$statistic)

    num <-  (ns[1] - 1) * sds[1]^2 + (ns[2] - 1) * sds[2]^2
    sdPooled <- sqrt(num / (ns[1] + ns[2] - 2))
    if (test == "Welch")  # Use different SE when using Welch T test!
      sdPooled <- sqrt(((sds[1]^2) + (sds[2]^2)) / 2)

    d <- "."
    if (optionsList$wantsEffect) {
      # Sources are https://en.wikipedia.org/wiki/Effect_size for now.
      if (options$effectSizeType == "cohen")
        d <- as.numeric((ms[1] - ms[2]) / sdPooled)
      else if (options$effectSizeType == "glass")
        d <- as.numeric((ms[1] - ms[2]) / sds[2])
      # Should give feedback on which data is considered 2.
      else if (options$effectSizeType == "hedges") {
        a <- sum(ns) - 2
        logCorrection <- lgamma(a / 2) - (log(sqrt(a / 2)) + lgamma((a - 1) / 2))
        d <- as.numeric((ms[1] - ms[2]) / sdPooled) * exp(logCorrection) # less biased / corrected version
      }
    }
    sed <- (as.numeric(r$estimate[1]) - as.numeric(r$estimate[2])) / stat

    #compute effect size SE
    j <- ifelse(effSize == "hedges", exp(logCorrection), 1)
    ni <- ifelse(effSize == "glass", ns[2], ns)

    effectSizeVar <- as.numeric(j)^2 * (sum(ns)/prod(ns) + (as.numeric(d)^2 / (2*sum(ni))))
    #Introduction to Meta-Analysis. Michael Borenstein, L. V. Hedges, J. P. T. Higgins and H. R. Rothstein (2009). Chapter 4, equation (4.20/4.24).
    effectSizeSe <- sqrt(effectSizeVar)

    confIntEffSize <- c(0,0)

    if (optionsList$wantsConfidenceEffSize){
      # From MBESS package by Ken Kelley, v4.6
      dfEffSize  <-  ifelse(effSize == "glass", ns[2] - 1, df)
      alphaLevel <- ifelse(direction == "two.sided", 1 - (ciEffSize + 1) / 2, 1 - ciEffSize)
      confIntEffSize <- .confidenceLimitsEffectSizes(ncp = d * sqrt((prod(ns)) / (sum(ns))),
                                                     df = dfEffSize, alpha.lower = alphaLevel,
                                                     alpha.upper = alphaLevel)[c(1, 3)]
      confIntEffSize <- unlist(confIntEffSize) * sqrt((sum(ns)) / (prod(ns)))

      if (direction == "greater")
        confIntEffSize[2] <- Inf
      else if (direction == "less")
        confIntEffSize[1] <- -Inf

      confIntEffSize <- sort(confIntEffSize)
    }
  }
  ## if the user doesn't want a Welch's t-test or Mann-Whitney,
  ## give a footnote indicating if the equality of variance
  ## assumption is met; seems like in this setting there is no
  ## sampling plan, thus the p-value is not defined. haha!
  leveneViolated <- FALSE
  if (!optionsList$wantsWelchs && !optionsList$wantsWilcox && optionsList$wantsStudents) {

    if (options$equalityOfVariancesTestType == "brownForsythe")
      center <- "median"
    else
      center <- "mean"

    levene <- car::leveneTest(variableData, groupingData, center)
    ## arbitrary cut-offs are arbitrary
    if (!is.na(levene[1, 3]) && levene[1, 3] < 0.05)
      leveneViolated <- TRUE
  }

  ## same for all t-tests
  p     <- as.numeric(r$p.value)
  ciLow <- r$conf.int[1]
  ciUp  <- r$conf.int[2]
  lowerCIeffectSize <- as.numeric(confIntEffSize[1])
  upperCIeffectSize <- as.numeric(confIntEffSize[2])

  # this will be the results object
  row <- list(df = df, p = p, md = m, d = d,
              lowerCIlocationParameter = ciLow, upperCIlocationParameter = ciUp,
              lowerCIeffectSize = lowerCIeffectSize, upperCIeffectSize = upperCIeffectSize,
              effectSizeSe = effectSizeSe, sed = sed)

  row[[testStat]] <- stat

  if (options$vovkSellke)
    row[["VovkSellkeMPR"]] <- VovkSellkeMPR(p)

  return(list(row = row, leveneViolated = leveneViolated))
}

.ttestIndependentEqVarFill <- function(table, dataset, options) {
  variables <- options$dependent
  groups    <- options$group

  levels <- levels(dataset[[ groups ]])

  for (variable in variables) {

    row <- list(variable = variable)

    errors <- .hasErrors(dataset,
                        message = 'short',
                        type = c('observations', 'variance', 'infinity'),
                        all.target = variable,
                        observations.amount = c('< 3'),
                        all.grouping = groups)

    errorMessage <- NULL
    if (identical(errors, FALSE)) {
      result <- try(.ttestIndependentEqVarRow(table, variable, groups, dataset, options))

      if (!isTryError(result))
        row <- c(row, result[["row"]])
      else
        errorMessage <- .extractErrorMessage(result)

    } else {
      errorMessage <- errors$message
    }

    if (!is.null(errorMessage)) {
      row[["fStat"]] <- NaN
      table$addFootnote(errorMessage, colNames = "fStat", rowNames = variable)
    } else if (!result[["LeveneComputed"]])
      table$addFootnote(gettext("F-statistic could not be calculated"), colNames = "fStat", rowNames = variable)

    table$addRows(row, rowNames = variable)
  }
}

.ttestIndependentEqVarRow <- function(table, variable, groups, dataset, options) {

  if (options$equalityOfVariancesTestType == "brownForsythe")
    center <- "median"
  else
    center <- "mean"

  levene <- car::leveneTest(dataset[[ variable ]], dataset[[ groups ]], center)


  fStat  <- levene[1, "F value"]
  dfOne <- levene[1, "Df"]
  dfTwo <- levene[2, "Df"]
  p  <- levene[1, "Pr(>F)"]

  row <- list(fStat = fStat, dfOne = dfOne, dfTwo = dfTwo, p = p)

  LeveneComputed <- TRUE
  if (is.na(levene[1, "F value"]))
    LeveneComputed <- FALSE

  return(list(row = row, LeveneComputed = LeveneComputed))
  }

.ttestIndependentNormalFill <- function(table, dataset, options) {
  ## for a independent t-test, we need to check both group vectors for normality
  variables <- options$dependent
  factor    <- options$group
  levels    <- levels(dataset[[factor]])

  for (variable in variables) {

    row <- list(dep = variable)
    rowName <- variable

    errors <- .hasErrors(dataset,
                         message = 'short',
                         type = c('observations', 'variance', 'infinity'),
                         all.target = variable,
                         observations.amount = c('< 3', '> 5000'),
                         all.grouping = factor)

    if (!identical(errors, FALSE)) {
      row[["W"]] <- NaN
      table$addFootnote(errors$message, colNames = "W", rowNames = rowName)
    } else {

      # NOTE: this is only valid because the Student and Welch t-test have identical residuals.
      # centering the groups individually is equivalent to subtracting the mean, which is the point estimate.
      # what remains is thus the residual.
      data <- na.omit(unlist(tapply(dataset[[variable]], dataset[[factor]], scale, center = TRUE, scale = FALSE)))
      r <- stats::shapiro.test(data)
      row[["W"]] <- as.numeric(r$statistic)
      row[["p"]] <- r$p.value
    }

    table$addRows(row, rowNames = rowName)

  }
}

.ttestIndependentDescriptivesTable <- function(jaspResults, dataset, options, ready) {
  # Container
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]
  if (!options$descriptives || !is.null(container[["table"]]))
    return()
  # Create table
  ttestDescriptivesTable <- createJaspTable(title = gettext("Group Descriptives"), dependencies = "descriptives")
  ttestDescriptivesTable$showSpecifiedColumnsOnly <- TRUE
  ttestDescriptivesTable$position <- 4
  ttestDescriptivesTable$addColumnInfo(name = "variable", type = "string",  title = "", combine = TRUE)
  ttestDescriptivesTable$addColumnInfo(name = "group",    type = "string",  title = gettext("Group"))
  ttestDescriptivesTable$addColumnInfo(name = "N",        type = "integer", title = gettext("N"))
  ttestDescriptivesTable$addColumnInfo(name = "mean",     type = "number",  title = gettext("Mean"))
  ttestDescriptivesTable$addColumnInfo(name = "sd",       type = "number",  title = gettext("SD"))
  ttestDescriptivesTable$addColumnInfo(name = "se",       type = "number",  title = gettext("SE"))
  ttestDescriptivesTable$addColumnInfo(name = "coefOfVariation",
                                                          type = "number",  title = gettext("Coefficient of variation"))
  if (options[["mannWhitneyU"]]) {
    ttestDescriptivesTable$addColumnInfo(name = "meanRank", type = "number",  title = gettext("Mean Rank"))
    ttestDescriptivesTable$addColumnInfo(name = "sumRank",  type = "number",  title = gettext("Sum Rank"))
  }
  ttestDescriptivesTable$dependOn("mannWhitneyU")

  container[["table"]] <- ttestDescriptivesTable

  if(ready)
    .ttestIndependentDescriptivesFill(ttestDescriptivesTable, dataset, options)
}

.ttestIndependentDescriptivesFill <- function(table, dataset, options) {
  variables <- options$dependent
  groups <- options$group
  levels <- base::levels(dataset[[ groups ]])
  groupingData <- dataset[[groups]]

  for (variable in variables) {

    for (level in levels) {

      row <- list(variable = variable, group = level, .isNewGroup = (level == levels[1]))

      variableData <- dataset[[variable]]
      groupData   <- variableData[groupingData == level]
      groupDataOm <- na.omit(groupData)

      dataRank <- rank((dataset[[variable]]), na.last = "keep")
      groupDataRank <- dataRank[groupingData == level]
      groupDataRankOm <- na.omit(groupDataRank)

      if (class(groupDataOm) != "factor") {

        n    <- length(groupDataOm)
        mean <- mean(groupDataOm)
        std  <- sd(groupDataOm)
        sem  <- std / sqrt(n)
        coefOfVariation <- std / mean

        meanRank <- mean(groupDataRankOm)
        sumRank <- sum(groupDataRankOm)

        row <- c(row, list(N = n, mean = mean, sd = std, se = sem,
                           coefOfVariation = coefOfVariation,
                           meanRank = meanRank,
                           sumRank = sumRank))

      } else {
        n   <- length(groupDataOm)
        row <- c(row, list(n = n))
      }

      table$addRows(row)
    }
  }
}

.ttestIndependentDescriptivesPlot <- function(jaspResults, dataset, options, ready) {
  if(!options$descriptivesPlot)
    return()
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]
  if (is.null(container[["plots"]])) {
    subcontainer <- createJaspContainer(gettext("Descriptives Plots"), dependencies = c("descriptivesPlot", "descriptivesPlotCiLevel"))
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
      p <- try(.ttestIndependentDescriptivesPlotFill(dataset, options, variable))
      if(isTryError(p))
        descriptivesPlot$setError(.extractErrorMessage(p))
      else
        descriptivesPlot$plotObject <- p
    }
  }
  return()
}

.ttestIndependentDescriptivesPlotFill <- function(dataset, options, variable) {

  groups <- options$group
  errors <- .hasErrors(dataset,
                       message = 'short',
                       type = c('observations', 'variance', 'infinity'),
                       all.target = variable,
                       observations.amount = '< 2',
                       observations.grouping = groups)

  if (!isFALSE(errors))
    stop(errors$message)

  dataset <- na.omit(dataset[, c(groups, variable)])
  summaryStat <- .summarySE(
    dataset,
    measurevar    = variable,
    groupvars     = groups,
    conf.interval = options[["descriptivesPlotCiLevel"]],
    na.rm         = TRUE,
    .drop         = FALSE
  )

  colnames(summaryStat)[which(colnames(summaryStat) == variable)] <- "dependent"
  colnames(summaryStat)[which(colnames(summaryStat) == groups)]   <- "group"

  p <- jaspGraphs::descriptivesPlot(
    x                      = summaryStat[["group"]],
    y                      = summaryStat[["dependent"]],
    ciLower                = summaryStat[["ciLower"]],
    ciUpper                = summaryStat[["ciUpper"]],
    group                  = summaryStat[["group"]],
    noXLevelNames          = FALSE,
    yName                  = variable,
    xName                  = groups
  ) + jaspGraphs::themeJaspRaw(axis.title.cex = jaspGraphs::getGraphOption("axis.title.cex"))

  return(p)
}

.ttestIndependentDescriptivesRainCloudPlot <- function(jaspResults, dataset, options, ready) {
  if(!options$raincloudPlot)
    return()
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]

  if (is.null(container[["plotsRainCloud"]])) {
    subcontainer <- createJaspContainer(gettext("Raincloud Plots"), dependencies = c("raincloudPlot", "raincloudPlotHorizontal"))
    subcontainer$position <- 6
    container[["plotsRainCloud"]] <- subcontainer
  } else {
    subcontainer <- container[["plotsRainCloud"]]
  }
  horiz <- options$raincloudPlotHorizontal
  if(ready){
    groups <- options$group
    errors <- .ttestBayesianGetErrorsPerVariable(dataset, options, "independent")
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
      p <- try(.descriptivesPlotsRainCloudFill(dataset, variable, groups, variable, groups, addLines = FALSE, horiz, NULL))
      if(isTryError(p))
        descriptivesPlotRainCloud$setError(.extractErrorMessage(p))
      else
        descriptivesPlotRainCloud$plotObject <- p
    }
  }
  return()
}

.ttestIndependentDescriptivesBarPlot <- function(jaspResults, dataset, options, ready) {
  if (!options[["barPlot"]])
    return()
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]
  if (is.null(container[["barPlots"]])) {
    subcontainer <- createJaspContainer(gettext("Bar Plots"), dependencies = c("barPlot",
                                                                               "barPlotCiLevel",
                                                                               "barPlotErrorType",
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

