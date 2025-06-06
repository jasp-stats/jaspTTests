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
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#

# abbreviation:
# ttestBPS = ttestBayesianPairedSamples
TTestBayesianPairedSamplesInternal <- function(jaspResults, dataset, options) {

  .ttestBayesianRunAnalysis(jaspResults, dataset, options, "paired")

}

.ttestBPSTTest <- function(ttestContainer, dataset, options, derivedOptions, errors, ttestState) {

  ttestTable <- .ttestBPSTTestMarkup(options, derivedOptions)
  ttestResults <- .ttestBayesianEmptyObject(options, derivedOptions, ttestState)
  dependents <- derivedOptions[["dependents"]]
  ttestRows <- .ttestBayesianCreateTtestRows(dependents, options, derivedOptions, ttestState)
  ttestTable$setData(ttestRows)

  if (!is.null(derivedOptions[["footnotes"]]))
    ttestTable$addFootnote(derivedOptions[["footnotes"]])

  ttestContainer[["ttestTable"]] <- ttestTable
  if (!derivedOptions[["ready"]])
    return(ttestResults)

  alreadyComputed <- !is.na(ttestRows[, "BF"]) & ttestResults[["hypothesis"]] == options[["alternative"]]
  .ttestBayesianSetFootnotesMainTable(ttestTable, ttestResults, dependents[alreadyComputed])
  .ttestBayesianInitBayesFactorPackageOptions()

  oneSided <- derivedOptions[["oneSided"]]
  bf.type <- options[["bayesFactorType"]]
  BFH1H0 <- ttestResults[["BFH1H0"]]
  nvar <- length(dependents)

  if (derivedOptions[["wilcoxTest"]])
    .ttestBayesianSetupWilcoxProgressBar(nvar, ttestState, options[["wilcoxonSamples"]])

  for (var in dependents[!alreadyComputed]) {

    pair <- derivedOptions[["pairs"]][[var]]
    row <- list(variable1 = pair[[1L]], separator = "-", variable2 = pair[[2L]])

    if (!(pair[[1L]] == "" || pair[[2L]] == "")) {

      if (!isFALSE(errors[[var]])) {

        errorMessage <- errors[[var]]$message
        ttestTable$addFootnote(errorMessage, rowNames = var, colNames = "BF")
        ttestResults[["status"]][var] <- "error"
        ttestResults[["errorFootnotes"]][[var]] <- errorMessage
        ttestRows[var, c("BF", "error")] <- NaN

      } else {

        # these objects are made here so they don't need to be created every time a try fails,
        # which means they could be forgotten and not created
        bf.raw <- NaN
        error  <- NaN

        subDataSet <- dataset[, .v(c(pair[[1L]], pair[[2L]]))]
        subDataSet <- subDataSet[complete.cases(subDataSet), ]

        x <- subDataSet[[1L]]
        y <- subDataSet[[2L]]

        if (!derivedOptions[["wilcoxTest"]]) {


          r <- try({.generalTtestBF(x = x, y = y, paired = TRUE, oneSided = oneSided, options = options)})

          if (isTryError(r)) {

            errorMessage <- .extractErrorMessage(r)
            ttestResults[["status"]][var] <- "error"
            ttestResults[["errorFootnotes"]][[var]] <- errorMessage
            ttestTable$addFootnote(message = errorMessage, rowNames = var, colNames = "BF")

          } else {

            bf.raw <- r[["bf"]]
            error  <- r[["error"]]
            ttestResults[["tValue"]][[var]] <- r[["tValue"]]
            ttestResults[["n1"]][var]       <- r[["n1"]]
            # ttestResults[["n2"]][var]       <- r[["n2"]]
            ttestResults[["tValue"]][var]   <- r[["tValue"]]

            if (!is.null(error) && is.na(error) && grepl("approximation", r[["method"]])) {
              error <- NaN
              ttestTable$addFootnote(
                message = gettext("t-value is large. A Savage-Dickey approximation was used to compute the Bayes factor but no error estimate can be given."),
                symbol = "", rowNames = var, colNames = "error")
            }
            if (is.null(error) && options[["effectSizeStandardized"]] == "informative" &&
                options[["informativeStandardizedEffectSize"]] == "normal") {
              error <- NA_real_
              ttestTable$addFootnote(message = gettext("No error estimate is available for normal priors."))
            }
          }

        } else { # wilcoxtest

          # If the samples can be reused, don't call the Gibbs sampler again, but recalculate the
          # Bayes factor with new settings and take the samples from state.
          if (!is.null(ttestResults[["delta"]][[var]]) && all(!is.na(ttestResults[["delta"]][[var]]))) {

            bf.raw <- try(.computeBayesFactorWilcoxon(
              deltaSamples         = ttestResults[["delta"]][[var]],
              cauchyPriorParameter = options[["priorWidth"]],
              oneSided             = oneSided
            ))
            rHat <- ttestResults[["rHat"]][[var]]

          } else {

            .setSeedJASP(options)
            r <- try(.signRankGibbsSampler(
              x = x, y = y, nSamples = options[["wilcoxonSamples"]], nBurnin = 0,
              cauchyPriorParameter = options[["priorWidth"]]
            ))

            if (isTryError(r)) {

              errorMessage <- .extractErrorMessage(r)
              ttestResults[["status"]][var] <- "error"
              ttestResults[["errorFootnotes"]][[var]] <- errorMessage
              ttestTable$addFootnote(message = errorMessage, rowNames = var, colNames = "BF")

            } else {

              ttestResults[["delta"]][[var]]  <- r[["deltaSamples"]]
              ttestResults[["rHat"]][[var]]  <- r[["rHat"]]

              bf.raw <- .computeBayesFactorWilcoxon(
                deltaSamples         = r[["deltaSamples"]],
                cauchyPriorParameter = options[["priorWidth"]],
                oneSided             = oneSided)

              rHat <- r[["rHat"]]

            }
          }

          if (!is.null(ttestResults[["delta"]][[var]]))
            ttestResults[["tValue"]][[var]] <- median(ttestResults[["delta"]][[var]])
          ttestResults[["n1"]][var]       <- length(x)
          wValue <- unname(wilcox.test(x, y, paired = TRUE)[["statistic"]])
          error <- wValue
          ttestRows[var, "rHat"] <- rHat

        }


        ttestResults[["BF10post"]][var] <- bf.raw
        BF <- .recodeBFtype(bfOld     = bf.raw,
                            newBFtype = bf.type,
                            oldBFtype = "BF10")

        msg <- .ttestBayesianCheckBFPlot(BF)
        if (!is.null(msg)) {
          ttestResults[["plottingError"]][[var]] <- msg
          ttestResults[["status"]][var] <- "error"
        }
        ttestRows[var, "BF"]    <- BF
        ttestRows[var, "error"] <- error

      }
    }
    ttestTable$setData(ttestRows)
  }

  ttestResults[["ttestRows"]] <- ttestRows

  return(ttestResults)
}

.ttestBPSTTestMarkup <- function(options, derivedOptions) {


  jaspTable <- createJaspTable()
  jaspTable$title <- if (derivedOptions[["wilcoxTest"]]) {
    gettext("Bayesian Wilcoxon Signed-Rank Test")
  } else {
    gettext("Bayesian Paired Samples T-Test")
  }

  jaspTable$dependOn(c("bayesFactorType", "pairs"))

  if (options[["effectSizeStandardized"]] == "default" && !derivedOptions[["wilcoxTest"]]) {
    citations <- .ttestBayesianCitations[c("MoreyEtal2015", "RouderEtal2009")]
  } else if (derivedOptions[["wilcoxTest"]]) {
    citations <- .ttestBayesianCitations["doorn2020bayesian"]
  } else if (options[["effectSizeStandardized"]] == "informative") {
    citations <- .ttestBayesianCitations["gronau2020informed"]
  }

  jaspTable$addCitation(citations)

  bfType <- options[["bayesFactorType"]]

  hypothesis <- switch(options[["alternative"]],
                       "twoSided"  = "equal",
                       "greater" = "greater",
                       "less" = "smaller"
  )
  bfTitle <- .ttestBayesianGetBFTitle(bfType, hypothesis)

  jaspTable$addColumnInfo(name = "variable1", title = gettext("Measure 1"), type = "string")
  jaspTable$addColumnInfo(name = "separator", title = "",                   type = "separator")
  jaspTable$addColumnInfo(name = "variable2", title = gettext("Measure 2"), type = "string")
  jaspTable$addColumnInfo(name = "BF",        title = bfTitle,              type = "number")

  if (derivedOptions[["wilcoxTest"]]) {
    jaspTable$addColumnInfo(name = "error", type = "number", title = "W")
    jaspTable$addColumnInfo(name = "rHat", type = "number", title = "Rhat")
    jaspTable$addFootnote(gettextf("Result based on data augmentation algorithm with 5 chains of %.0f iterations.", options[["wilcoxonSamples"]]))
  } else {
    if (options[["alternative"]] == "twoSided") {
      fmt <- "sf:4;dp:3"
    } else {
      fmt <- "sf:4;dp:3;~"
    }
    jaspTable$addColumnInfo(name = "error", type = "number", format = fmt, title = gettextf("error %%"))
  }

  if (options[["alternative"]] == "greater" || options[["alternative"]] == "less")
    jaspTable$addFootnote(.ttestPairedGetHypothesisFootnote(options[["alternative"]], options[["pairs"]]))

  return(jaspTable)
}


# Wilcoxon functions ----
.signRankGibbsSampler <- function(xVals, yVals = NULL, nSamples = 1e3, cauchyPriorParameter = 1/sqrt(2), testValue = 0,
                                 nBurnin = 1, nGibbsIterations = 10, nChains = 5){

  n <- length(xVals)

  if (!is.null(yVals)) {
    differenceScores <- xVals - yVals
  } else {
    differenceScores <- xVals - testValue
  }

  differenceSigns <- (sign(differenceScores))
  absDifferenceRanked <- rank(abs(differenceScores))
  prodSignAbsRank <- differenceSigns * absDifferenceRanked

  initDiffSamples <- sort(abs(rnorm(n)))[absDifferenceRanked]
  sampledDiffsAbs <- abs(initDiffSamples)
  diffSamples <- numeric(n)


  deltaSamples <- numeric(nSamples)
  deltaSamplesMatrix <- matrix(ncol = nChains, nrow = nSamples-nBurnin)
  oldDeltaProp <- 0

  for(thisChain in 1:nChains) {

    for (j in 1:nSamples) {

      for (i in sample(1:n)) {

        currentRank <- absDifferenceRanked[i]

        currentBounds <- .upperLowerTruncation(ranks=absDifferenceRanked, values=sampledDiffsAbs, currentRank=currentRank)
        if (is.infinite(currentBounds[["under"]])) {currentBounds[["under"]] <- 0}

        sampledDiffsAbs[i] <- .truncNormSample(currentBounds[["under"]], currentBounds[["upper"]], mu = abs(oldDeltaProp), sd=1)

      }

      diffSamples <- sampledDiffsAbs * differenceSigns

      if (any(differenceSigns == 0)) {
        nullSamples <- sampledDiffsAbs[differenceSigns == 0] * sample(c(-1,1), size = sum(differenceSigns == 0), replace = TRUE)
        diffSamples[which(differenceSigns == 0)] <- nullSamples
      }

      sampledDiffsAbs <- abs(diffSamples)
      gibbsOutput <- .sampleGibbsOneSampleWilcoxon(diffScores = diffSamples, nIter = nGibbsIterations, rscale = cauchyPriorParameter)

      deltaSamples[j] <- oldDeltaProp <- gibbsOutput
      progressbarTick()

    }

    if (nBurnin > 0) {
      deltaSamples <- deltaSamples[-(1:nBurnin)]
    } else {
      deltaSamples <- deltaSamples
    }
    deltaSamplesMatrix[, thisChain] <- deltaSamples
  }

  betweenChainVar <- (nSamples / (nChains - 1)) * sum((apply(deltaSamplesMatrix, 2, mean)  - mean(deltaSamplesMatrix))^2)
  withinChainVar <- (1/ nChains) * sum(apply(deltaSamplesMatrix, 2, var))

  fullVar <- ((nSamples - 1) / nSamples) * withinChainVar + (betweenChainVar / nSamples)
  rHat <- sqrt(fullVar/withinChainVar)

  return(list(deltaSamples = as.vector(deltaSamplesMatrix), rHat = rHat))
}

.sampleGibbsOneSampleWilcoxon <- function(diffScores, nIter = 10, rscale = 1/sqrt(2)){
  ybar <- mean(diffScores)
  n <- length(diffScores)
  sigmaSq <- 1
  mu <- ybar
  g <- ybar^2 / sigmaSq + 1

  for(i in 1:nIter){
    #sample mu
    varMu  <- sigmaSq / (n + (1 / g))
    meanMu <- (n * ybar) / (n + (1 / g))
    mu <- rnorm(1, meanMu, sqrt(varMu) )

    # sample g
    scaleg <- (mu^2 + sigmaSq * rscale^2) / (2*sigmaSq)
    g = 1 / rgamma(1, 1, scaleg )

    delta <- mu / sqrt(sigmaSq)
  }
  return(delta)
}
