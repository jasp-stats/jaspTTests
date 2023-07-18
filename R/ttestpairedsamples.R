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

TTestPairedSamplesInternal <- function(jaspResults, dataset = NULL, options, ...) {
  ready <- length(options$pairs) > 0  #at least one variable pair
  type  <- "paired"
  if(ready) {
    dataset <- .ttestReadData(dataset, options, type)
    .ttestCheckErrors(        dataset, options, type)
  }
  # Output tables (each calls its own results function)
  .ttestPairedMainTable(  jaspResults, dataset, options, ready, type)
  .ttestPairedNormalTable(jaspResults, dataset, options, ready, type)
  # Descriptives
  vars <- unique(unlist(options$pairs))
  .ttestDescriptivesTable(                        jaspResults, dataset, options, ready, vars)
  .ttestPairedDescriptivesPlot(                   jaspResults, dataset, options, ready)
  .ttestPairedDescriptivesBarPlot(                jaspResults, dataset, options, ready)
  .ttestPairedDescriptivesRainCloudPlot(          jaspResults, dataset, options, ready)
  .ttestPairedDescriptivesRainCloudDifferencePlot(jaspResults, dataset, options, ready)

  return()
}

.ttestPairedMainTable <- function(jaspResults, dataset, options, ready, type) {
  if (!is.null(jaspResults[["ttest"]]))
    return()

  optionsList <- .ttestOptionsList(options, type)

  # Create table
  ttest <- createJaspTable(title = gettext("Paired Samples T-Test"))
  ttest$dependOn(c("effectSize", "variables", "effectSizeCi", "effectSizeCiLevel",
                   "student", "wilcoxon",
                   "meanDifference", "meanDifferenceCi",
                   "meanDifferenceCiLevel", "alternative",
                   "vovkSellke", "naAction", "pairs"))
  ttest$showSpecifiedColumnsOnly <- TRUE
  ttest$position <- 1

  ttest$addColumnInfo(name = "v1",  type = "string",    title = gettext("Measure 1"))
  ttest$addColumnInfo(name = "sep", type = "separator", title = "")
  ttest$addColumnInfo(name = "v2",  type = "string",    title = gettext("Measure 2"))

  if (optionsList$wantsWilcox && optionsList$onlyTest) {
    ttest$addFootnote(gettext("Wilcoxon signed-rank test."))
    testStat                <- "W"
    testStatName            <- gettext("W")
    nameOfLocationParameter <- gettext("Hodges-Lehmann Estimate")
    nameOfEffectSize        <- gettext("Rank-Biserial Correlation")
  } else if (optionsList$wantsStudents && optionsList$onlyTest) {
    ttest$addFootnote(gettext("Student's t-test."))
    testStat                <- "t"
    testStatName            <- gettext("t")
    nameOfLocationParameter <- gettext("Mean Difference")
    nameOfEffectSize        <- gettext("Cohen's d")
  } else {
    testStat                <- "Statistic"
    testStatName            <- gettext("Statistic")
    nameOfLocationParameter <- gettext("Location Parameter")
    nameOfEffectSize        <- gettext("Effect Size")
  }

  ## if the user wants all tests, add a column called "Test"
  if (sum(optionsList$allTests) > 1)
    ttest$addColumnInfo(name = "test", title = gettext("Test"), type = "string")

  ttest$addColumnInfo(name = testStat, title = testStatName,    type = "number")
  if (optionsList$wantsWilcox) {
    ttest$addColumnInfo(name = "zstat", title = gettext("z"),    type = "number")
  }
  ttest$addColumnInfo(name = "df",     title = gettext("df"),   type = "integer")
  ttest$addColumnInfo(name = "p",      title = gettext("p"),    type = "pvalue")

  .ttestVovkSellke(ttest, options)

  if (optionsList$wantsDifference) {
    ttest$addColumnInfo(name = "md", title = nameOfLocationParameter, type = "number")

    if (optionsList$wantsStudents)
      ttest$addColumnInfo(name = "sed", title = gettext("SE Difference"), type = "number")

    if (optionsList$wantsWilcox && optionsList$wantsStudents)
      ttest$addFootnote(gettext("For the Student t-test, location parameter is given by mean difference <em>d</em>. For the Wilcoxon test, location parameter is given by the Hodges-Lehmann estimate."))
  }

  if (optionsList$wantsConfidenceMeanDiff) {
    title <- gettextf("%1$s%% CI for %2$s", 100 * optionsList$percentConfidenceMeanDiff, nameOfLocationParameter)
    ttest$addColumnInfo(name = "lowerCIlocationParameter", type = "number", title = gettext("Lower"), overtitle = title)
    ttest$addColumnInfo(name = "upperCIlocationParameter", type = "number", title = gettext("Upper"), overtitle = title)
  }

  if (optionsList$wantsEffect) {
    ttest$addColumnInfo(name = "d",            title = nameOfEffectSize,                      type = "number")
    ttest$addColumnInfo(name = "effectSizeSe", title = gettextf("SE %1$s", nameOfEffectSize), type = "number")
    if (optionsList$wantsWilcox && optionsList$wantsStudents)
      ttest$addFootnote(gettext("For the Student t-test, effect size is given by Cohen's <em>d</em>. For the Wilcoxon test, effect size is given by the matched rank biserial correlation."))
  }

  if (optionsList$wantsConfidenceEffSize) {
    title <- gettextf("%1$s%% CI for %2$s", 100 * optionsList$percentConfidenceEffSize, nameOfEffectSize)
    ttest$addColumnInfo(name = "lowerCIeffectSize", type = "number", title = gettext("Lower"), overtitle = title)
    ttest$addColumnInfo(name = "upperCIeffectSize", type = "number", title = gettext("Upper"), overtitle = title)
  }

  if (options$alternative == "greater" || options$alternative == "less")
    ttest$addFootnote(.ttestPairedGetHypothesisFootnote(options[["alternative"]], options[["pairs"]]))

  jaspResults[["ttest"]] <- ttest

  if (ready)
    .ttestPairedMainFill(ttest, dataset, options, testStat, optionsList)

}

.ttestPairedNormalTable <- function(jaspResults, dataset, options, ready, type) {
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


  ttestNormalTable$addColumnInfo(name = "v1",  type = "string", title = "")
  ttestNormalTable$addColumnInfo(name = "sep", type = "separator", title = "")
  ttestNormalTable$addColumnInfo(name = "v2",  type = "string", title = "")
  ttestNormalTable$addColumnInfo(name = "W",   type = "number", title = gettext("W"))
  ttestNormalTable$addColumnInfo(name = "p",   type = "pvalue", title = gettext("p"))

  message <- gettext("Significant results suggest a deviation from normality.")
  ttestNormalTable$addFootnote(message)

  container[["ttestNormalTable"]] <- ttestNormalTable

  if (ready)
    .ttestPairedNormalFill(ttestNormalTable, dataset, options)
}

.ttestPairedMainFill <-function(table, dataset, options, testStat, optionsList) {
  ## for each pair, run the checked tests and update the table
  for (pair in options$pairs) {
    p1 <- pair[[1]]
    p2 <- pair[[2]]

    errors <- .hasErrors(dataset,
                         message = 'short',
                         type = c('observations', 'variance', 'infinity'),
                         all.target = c(p1, p2),
                         observations.amount  = c('< 2'))

    for (test in optionsList$whichTests) {

      row     <- list(test = test, .isNewGroup = .ttestRowIsNewGroup(test, optionsList$whichTests))
      rowName <- paste(test, p1, p2, sep = "-")

      ## hide the name of the variable pair for the second statistic
      numTests <- length(optionsList$whichTests)
      isSecondStatisticOfPair <- numTests > 1 && test == optionsList$whichTests[numTests]
      row[["v1"]]  <- ifelse(isSecondStatisticOfPair, "", p1)
      row[["sep"]] <- ifelse(isSecondStatisticOfPair, "", "-")
      row[["v2"]]  <- ifelse(isSecondStatisticOfPair, "", p2)

      if (p1 != "" && p2 != "") {
        errorMessage <- NULL
        if (identical(errors, FALSE)) {
          rowResults <- try(.ttestPairedComputeMainTableRow(p1, p2, dataset, test, testStat, optionsList, options))

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
      } else {
        row <- c(row, list(df = "", p = "", md = "", d = "", lowerCI = "", upperCI = "", sed = "", zstat = ""))
        row[[testStat]] <- ""
      }

      table$addRows(row, rowNames = rowName)
    }
  }

}

.ttestPairedComputeMainTableRow <- function(p1, p2, dataset, test, testStat, optionsList, options) {
  c1 <- dataset[[ .v(p1) ]]
  c2 <- dataset[[ .v(p2) ]]
  df <- na.omit(data.frame(c1 = c1, c2 = c2))
  c1 <- df$c1
  c2 <- df$c2
  n  <- length(c1)
  zstat <- NULL
  direction <- .ttestMainGetDirection(options$alternative)

  ## if Wilcox box is ticked, run a paired wilcoxon signed rank test
  if (test == "Wilcoxon") {
    res <- stats::wilcox.test(c1, c2, paired = TRUE,
                              conf.level = optionsList$percentConfidenceMeanDiff,
                              conf.int = TRUE,
                              alternative = direction)
    # only count the difference scores that are not 0.
    nd   <- sum(c1 - c2 != 0)
    maxw <- (nd * (nd + 1))/2
    d    <- as.numeric((res$statistic / maxw) * 2 - 1)
    zstat <- (res$statistic - ((nd * (nd + 1)) / 4 ) ) /
      sqrt(((nd * (nd + 1)) * (2 * nd + 1)) / 24 )
    wSE  <- sqrt((nd * (nd + 1) * (2 * nd + 1)) / 6) / 2
    mrSE <- sqrt(wSE^2  * 4 * (1 / maxw^2))
    # zSign <- (ww$statistic - ((n*(n+1))/4))/wSE
    zmbiss <- atanh(d)
    if(direction == "two.sided")
      confIntEffSize <- sort(c(tanh(zmbiss + qnorm((1-optionsList$percentConfidenceEffSize)/2)*mrSE), tanh(zmbiss + qnorm((1+optionsList$percentConfidenceEffSize)/2)*mrSE)))
    else if (direction == "less")
      confIntEffSize <- sort(c(-Inf, tanh(zmbiss + qnorm(optionsList$percentConfidenceEffSize)*mrSE)))
    else if (direction == "greater")
      confIntEffSize <- sort(c(tanh(zmbiss + qnorm((1-optionsList$percentConfidenceEffSize))*mrSE), Inf))

    effectSizeSe <- tanh(mrSE)

    ## else run a simple paired t-test
  } else {
    res <- stats::t.test(c1, c2, paired = TRUE, conf.level = optionsList$percentConfidenceMeanDiff,
                         alternative = direction)
    df  <- ifelse(is.null(res$parameter), "", as.numeric(res$parameter))
    d   <- mean(c1 - c2) / sd(c1 - c2)

    #compute effect size SE
    effectSizeVar <- ((1/n)+(as.numeric(d)^2 / (2*n))) * (2*(1-cor(c1,c2)))
    #Introduction to Meta-Analysis. Michael Borenstein, L. V. Hedges, J. P. T. Higgins and H. R. Rothstein (2009). Chapter 4, equation (4.28)
    effectSizeSe <- sqrt(effectSizeVar)

    confIntEffSize <- c(0,0)


    if (optionsList$wantsConfidenceEffSize) {

      ciEffSize  <- options$effectSizeCiLevel
      alphaLevel <- ifelse(direction == "two.sided", 1 - (ciEffSize + 1) / 2, 1 - ciEffSize)

      confIntEffSize <- .confidenceLimitsEffectSizes(ncp = d * sqrt(n), df = df,
                                                     alpha.lower = alphaLevel,
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
  p    <- as.numeric(res$p.value)
  stat <- as.numeric(res$statistic)
  sed  <- sd(c1 - c2) / sqrt(length(c1))
  # num <- sqrt(sd(c1)^2 + sd(c2)^2 -  2 * cov(c1, c2))
  # d   <- mean(c1) - mean(c2) / num

  m <- as.numeric(res$estimate)
  ciLow <- ifelse(direction == "less", -Inf,
                  as.numeric(res$conf.int[1]))
  ciUp  <- ifelse(direction == "greater", Inf,
                  as.numeric(res$conf.int[2]))
  ciLowEffSize <- as.numeric(confIntEffSize[1])
  ciUpEffSize  <- as.numeric(confIntEffSize[2])

  ## paired t-test has it, wilcox doesn't!
  df  <- ifelse(is.null(res$parameter), "", as.numeric(res$parameter))
  sed <- ifelse(is.null(res$parameter), "", sed)

  # add things to the intermediate results object
  result <- list(df = df, p = p, md = m, d = d,
                     lowerCIlocationParameter = ciLow, upperCIlocationParameter = ciUp,
                     lowerCIeffectSize = ciLowEffSize, upperCIeffectSize = ciUpEffSize,
                     effectSizeSe = effectSizeSe, sed = sed, zstat = zstat)

  result[testStat] <- stat

  if (options$vovkSellke)
    result[["VovkSellkeMPR"]] <- VovkSellkeMPR(p)

  return(result)
}

.ttestPairedNormalFill <- function(table, dataset, options) {
  pairs <- options$pairs
  for (pair in pairs) {

    if (length(pair) < 2 || pair[[1]] == pair[[2]])
      next

    p1 <- pair[[1]]
    p2 <- pair[[2]]

    row     <- list(v1 = p1, sep = "-", v2 = p2)
    rowName <- paste(p1, p2, sep = "-")

    if (p1 != "" && p2 != "") {
      errors <- .hasErrors(dataset,
                           message = 'short',
                           type = c('observations', 'variance', 'infinity'),
                           all.target = c(p1, p2),
                           observations.amount  = c('< 3', '> 5000'))

      if (!identical(errors, FALSE)) {
        row[["W"]] <- NaN
        table$addFootnote(errors$message, colNames = "W", rowNames = rowName)
      } else {
        c1   <- dataset[[ .v(p1) ]]
        c2   <- dataset[[ .v(p2) ]]
        data <- na.omit(c1 - c2)

        r <- stats::shapiro.test(data)
        row[["W"]] <- as.numeric(r$statistic)
        row[["p"]] <- r$p.value
      }
    }

    table$addRows(row, rowNames = rowName)
  }
}

.ttestDescriptivesTable <- function(jaspResults, dataset, options, ready, vars) {
  # Container
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]
  if (!options$descriptives || !is.null(container[["table"]]))
    return()
  # Create table
  ttestDescriptivesTable <- createJaspTable(title = gettext("Descriptives"), dependencies = "descriptives")
  ttestDescriptivesTable$showSpecifiedColumnsOnly <- TRUE
  ttestDescriptivesTable$position <- 4
  ttestDescriptivesTable$addColumnInfo(name = "v",    type = "string",  title = "")
  ttestDescriptivesTable$addColumnInfo(name = "N",    type = "integer", title = gettext("N"))
  ttestDescriptivesTable$addColumnInfo(name = "mean", type = "number",  title = gettext("Mean"))
  ttestDescriptivesTable$addColumnInfo(name = "sd",   type = "number",  title = gettext("SD"))
  ttestDescriptivesTable$addColumnInfo(name = "se",   type = "number",  title = gettext("SE"))
  ttestDescriptivesTable$addColumnInfo(name = "coefOfVariation",
                                                      type = "number",  title = gettext("Coefficient of variation"))
  container[["table"]] <- ttestDescriptivesTable

  if (ready)
    .ttestDescriptivesFill(ttestDescriptivesTable, dataset, options, desc.vars = vars)
}

.ttestDescriptivesFill <- function(table, dataset, options, desc.vars) {
  desc.vars <- desc.vars[desc.vars != ""]

  if (length(desc.vars) == 0)
    return()

  for (var in desc.vars) {
    row <- list(v = var)

    dat <- na.omit(dataset[[ .v(var) ]])
    n   <- as.numeric(length(dat))
    m   <- as.numeric(mean(dat, na.rm = TRUE))
    std <- as.numeric(sd(dat,   na.rm = TRUE))

    if (is.numeric(std)) {
      se              <- as.numeric(std/sqrt(n))
      coefOfVariation <- as.numeric(std/m)
    }
    else {
      se              <- NaN
      coefOfVariation <- NaN
    }

    row[["N"]]                 <- n
    row[["mean"]]              <- m
    row[["sd"]]                <- std
    row[["se"]]                <- se
    row[["coefOfVariation"]]   <- coefOfVariation

    table$addRows(row)
  }
}

.ttestPairedDescriptivesPlot <- function(jaspResults, dataset, options, ready) {
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

  for(pair in options$pairs) {
    title <- paste(pair, collapse = " - ")
    if(!is.null(subcontainer[[title]]))
      next
    descriptivesPlot <- createJaspPlot(title = title, width = 480, height = 320)
    descriptivesPlot$dependOn(optionContainsValue = list(pairs = pair))
    subcontainer[[title]] <- descriptivesPlot
    if(ready){
      p <- try(.ttestPairedDescriptivesPlotFill(dataset, options, pair))
      if(isTryError(p))
        descriptivesPlot$setError(.extractErrorMessage(p))
      else
        descriptivesPlot$plotObject <- p
    }
  }
  return()
}

.ttestPairedDescriptivesPlotFill <- function(dataset, options, pair) {
  errors <- .hasErrors(dataset,
                       message = 'short',
                       type = c('variance', 'infinity'),
                       all.target = pair)
  if(!identical(errors, FALSE))
    stop(errors$message)
  base_breaks_x <- function(x) {
    b <- unique(as.numeric(x))
    d <- data.frame(y = -Inf, yend = -Inf, x = min(b), xend = max(b))
    list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y, xend = xend,
                                                      yend = yend), inherit.aes = FALSE, size = 1))
  }

  base_breaks_y <- function(x) {
    ci.pos <- c(x[, "dependent"] - x[, "ci"], x[, "dependent"] + x[, "ci"])
    b <- pretty(ci.pos)
    d <- data.frame(x = -Inf, xend = -Inf, y = min(b), yend = max(b))
    list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE, size = 1),
         ggplot2::scale_y_continuous(breaks = c(min(b), max(b))))
  }
  c1 <- dataset[[pair[[1]]]]
  c2 <- dataset[[pair[[2]]]]
  ####
  data <- data.frame(id = rep(1:length(c1), 2), dependent = c(c1, c2),
                     groupingVariable = c(rep(paste("1.", pair[[1]], sep = ""), length(c1)),
                                          rep(paste("2.", pair[[2]], sep = ""), length(c2))),
                     stringsAsFactors = TRUE)

  summaryStat <- .summarySEwithin(data, measurevar = "dependent", withinvars = "groupingVariable",
                                 idvar = "id", conf.interval = options$descriptivesPlotCiLevel,
                                 na.rm = TRUE, .drop = FALSE)

  pd <- ggplot2::position_dodge(0.2)

  p <- ggplot2::ggplot(summaryStat, ggplot2::aes(x = groupingVariable, y = dependent, group = 1)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = ciLower, ymax = ciUpper), colour = "black", width = 0.2, position = pd) +
    ggplot2::geom_line(position = pd, size = 0.7) +
    ggplot2::geom_point(position = pd, size = 4) +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(NULL) +
    base_breaks_y(summaryStat) +
    base_breaks_x(summaryStat$groupingVariable) +
    ggplot2::scale_x_discrete(labels = c(pair[[1]], pair[[2]]))

  p <- jaspGraphs::themeJasp(p)

  return(p)
}

.ttestPairedDescriptivesBarPlot <- function(jaspResults, dataset, options, ready) {
  if (!options[["barPlot"]])
    return()
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]

  if (is.null(container[["barPlots"]])) {
    subcontainer <- createJaspContainer(gettext("Bar Plots"), dependencies = c("barPlot",
                                                                               "barPlotCiLevel",
                                                                               "barPlotErrorType",
                                                                               "barPlotYAxisFixedToZero"))
    subcontainer$position <- 6
    container[["barPlots"]] <- subcontainer
  } else {
    subcontainer <- container[["barPlots"]]
  }

  for (pair in options[["pairs"]]) {
    title <- paste(pair, collapse = " - ")
    if (!is.null(subcontainer[[title]]))
      next
    descriptivesBarPlot <- createJaspPlot(title = title, width = 480, height = 320)
    descriptivesBarPlot$dependOn(optionContainsValue = list(pairs = pair))
    subcontainer[[title]] <- descriptivesBarPlot
    if (ready) {
      p <- try(.ttestDescriptivesBarPlotFill(dataset, options, pair))
      if (isTryError(p))
        descriptivesBarPlot$setError(.extractErrorMessage(p))
      else
        descriptivesBarPlot$plotObject <- p
    }
  }
  return()
}

.ttestPairedDescriptivesRainCloudPlot <- function(jaspResults, dataset, options, ready) {
  if(!options$raincloudPlot)
    return()
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]

  if (is.null(container[["plotsRainCloud"]])) {
    subcontainer <- createJaspContainer(gettext("Raincloud Plots"), dependencies = "raincloudPlot")
    subcontainer$position <- 7
    container[["plotsRainCloud"]] <- subcontainer
  } else {
    subcontainer <- container[["plotsRainCloud"]]
  }

  if(ready){
    errors <- .ttestBayesianGetErrorsPerVariable(dataset, options, "paired")
    for(pair in options$pairs) {
      title <- paste(pair, collapse = " - ")
      if(!is.null(subcontainer[[title]]))
        next
      descriptivesPlotRainCloud <- createJaspPlot(title = title, width = 480, height = 320)
      descriptivesPlotRainCloud$dependOn(optionContainsValue = list(pairs = pair))
      subcontainer[[title]] <- descriptivesPlotRainCloud
      if(!isFALSE(errors[[title]])) {
        descriptivesPlotRainCloud$setError(errors[[title]]$message)
        next
      }
      groups  <- rep(pair, each = nrow(dataset))
      subData <- data.frame(dependent = unlist(dataset[, pair]), groups = groups)
      p <- try(.descriptivesPlotsRainCloudFill(subData, "dependent", "groups", "", "", addLines = TRUE, horiz = FALSE, NULL))
      if(isTryError(p))
        descriptivesPlotRainCloud$setError(.extractErrorMessage(p))
      else
        descriptivesPlotRainCloud$plotObject <- p
    }
  }
  return()
}

.ttestPairedDescriptivesRainCloudDifferencePlot <- function(jaspResults, dataset, options, ready) {
  if(!options$differenceRaincloudPlot)
    return()
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]

  if (is.null(container[["plotsRainCloudDifference"]])) {
    subcontainer <- createJaspContainer(gettext("Raincloud Difference Plots"), dependencies = c("differenceRaincloudPlot", "differenceRaincloudPlotHorizontal"))
    subcontainer$position <- 8
    container[["plotsRainCloudDifference"]] <- subcontainer
  } else {
    subcontainer <- container[["plotsRainCloudDifference"]]
  }

  horiz <- options$differenceRaincloudPlotHorizontal
  if(ready){
    errors <- .ttestBayesianGetErrorsPerVariable(dataset, options, "paired")
    for(pair in options$pairs) {
      title <- paste(pair, collapse = " - ")
      if(!is.null(subcontainer[[title]]))
        next
      descriptivesPlotRainCloudDifference <- createJaspPlot(title = title, width = 480, height = 320)
      descriptivesPlotRainCloudDifference$dependOn(optionContainsValue = list(pairs = pair))
      subcontainer[[title]] <- descriptivesPlotRainCloudDifference
      if(!isFALSE(errors[[title]])) {
        descriptivesPlotRainCloudDifference$setError(errors[[title]]$message)
        next
      }
      groups    <- rep("1", nrow(dataset))
      dependent <- dataset[, pair[[1]]] - dataset[, pair[[2]]]
      subData   <- data.frame(dependent = dependent, groups = groups)
      p <- try(.descriptivesPlotsRainCloudFill(subData, "dependent", "groups", title, "", addLines = FALSE, horiz, NULL))
      if(isTryError(p))
        descriptivesPlotRainCloudDifference$setError(.extractErrorMessage(p))
      else
        descriptivesPlotRainCloudDifference$plotObject <- p
    }
  }
  return()
}

.ttestPairedGetHypothesisFootnote <- function(hypothesis, pairs) {

  idx <- .ttestPairedGetIndexOfFirstNonEmptyPair(pairs)
  onePair <- length(pairs) == 1L
  isLess <- hypothesis == "less" # greater -> 1 is less than 2

  if (idx == 0L) { # no pairs, no example
    ans <- if (isLess) {
      gettext("For all tests, the alternative hypothesis specifies that Measure 1 is less than Measure 2.")
    } else {
      gettext("For all tests, the alternative hypothesis specifies that Measure 1 is greater than Measure 2.")
    }
  } else {
    pair1 <- pairs[[idx]][[1L]]
    pair2 <- pairs[[idx]][[2L]]
    if (onePair) { # one pair, only give example
      ans <- if (isLess) {
        gettextf("For all tests, the alternative hypothesis specifies that %1$s is less than %2$s.", pair1, pair2)
      } else {
        gettextf("For all tests, the alternative hypothesis specifies that %1$s is greater than %2$s.", pair1, pair2)
      }
    } else { # multiple pairs, general + example
      ans <- if (isLess) {
        gettextf("For all tests, the alternative hypothesis specifies that Measure 1 is less than Measure 2. For example, %1$s is less than %2$s.", pair1, pair2)
      } else {
        gettextf("For all tests, the alternative hypothesis specifies that Measure 1 is greater than Measure 2. For example, %1$s is greater than %2$s.", pair1, pair2)
      }
    }
  }
  return(ans)
}

.ttestPairedGetIndexOfFirstNonEmptyPair <- function(pairs) {
  for (i in seq_along(pairs))
    if (pairs[[i]][1L] != "" && pairs[[i]][[2L]] != "")
      return(i)
  return(0L)
}

.summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL, idvar=NULL, na.rm=FALSE,
                             conf.interval=.95, .drop=TRUE, errorBarType="confidenceInterval", usePooledSE=FALSE) {

  # Get the means from the un-normed data
  datac <- .summarySE(data, measurevar, groupvars=c(betweenvars, withinvars), na.rm=na.rm,
                      conf.interval=conf.interval, .drop=.drop, errorBarType=errorBarType, usePooledSE=usePooledSE)
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
  ndatac <- .summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars), na.rm=na.rm, conf.interval=conf.interval, .drop=.drop, errorBarType=errorBarType,
                       usePooledSE=usePooledSE)

  # Apply correction from Morey (2008) to the standard error and confidence interval
  # Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels, FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )

  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor

  if (errorBarType == "confidenceInterval") {

    ndatac$ciLower <- datac[,measurevar] - ndatac[,"ci"]
    ndatac$ciUpper <- datac[,measurevar] + ndatac[,"ci"]

  } else {

    ndatac$ciLower <- datac[,measurevar] - ndatac[,"se"]
    ndatac$ciUpper <- datac[,measurevar] + ndatac[,"se"]

  }

  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}

.summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE,
                       errorBarType="confidenceInterval", usePooledSE=FALSE) {

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
  if (usePooledSE && measurevar == .BANOVAdependentName) {

    data <- plyr::ddply(data, c(.BANOVAsubjectName, groupvars), plyr::summarise, dependent = mean(JaspColumn_.dependent._Encoded))
    names(data)[which(names(data) == "dependent")] <- measurevar

  } else if (usePooledSE && measurevar == paste0(.BANOVAdependentName, "_norm")) {

    data <- plyr::ddply(data, c(.BANOVAsubjectName, groupvars), plyr::summarise, dependent = mean(JaspColumn_.dependent._Encoded_norm))
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

  if (errorBarType == "confidenceInterval") {

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
