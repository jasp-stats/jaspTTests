#
# Copyright (C) 2013-2025 University of Amsterdam
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

# This is a generated file. Don't change it!

#' Bayesian Paired Samples T-Test
#'
#' The paired samples t-test allows you to estimate the effect size and test the null hypothesis that the population mean of the difference between paired (dependent) observations equals 0.
#' ## Assumptions
#' - Continuous difference score.
#' - The difference scores are a random sample from the population.
#' - The difference scores are normally distributed in the population.
#'
#' @param barPlotErrorType, Displays a bar plot of the sample mean(s), including error bars.
#' \itemize{
#'   \item \code{"se"}: By selecting this option, the error bars will represent standard errors of the mean of each condition.
#'   \item \code{"ci"}: Coverage of the confidence intervals (Or credible intervals in case of a Bayesian analysis) in percentages. By default, the confidence interval is set to 95%. This can be changed into the desired percentage.
#' }
#' @param barPlotYAxisFixedToZero, Forces the graph to show the default x-axis at y = 0.
#'    Defaults to \code{TRUE}.
#' @param bfRobustnessPlot, Displays the Bayes factor across different values of cauchy prior width. The scale of the Cauchy prior is varied between 0 and 1.5, creating progressively more uninformative priors.
#'    Defaults to \code{FALSE}.
#' @param bfRobustnessPlotAdditionalInfo, Adds the Bayes factor computed with the user-defined prior; adds a probability wheel depicting how likely the data is under the null vs. alternative hypothesis; adds the median and the 95% credible interval of the posterior distribution of the effect size.
#'    Defaults to \code{TRUE}.
#' @param bfSequentialPlot, Displays the development of the Bayes factor as the data come in using the user-defined prior.
#'    Defaults to \code{FALSE}.
#' @param bfSequentialPlotRobustness, Adds the results of the sequential analysis using the wide (scale=1) and ultrawide prior (scale=sqrt(2)).
#'    Defaults to \code{FALSE}.
#' @param descriptivesPlot, Display descriptives plots. Includes central credible interval, which shows the probability (conditional on the alternative model) that the true effect size lies within certain values. The default credible interval is set at 95% and can be changed by the user.
#'    Defaults to \code{FALSE}.
#' @param differenceRaincloudPlot, Displays a raincloud plot of the differences between the two measures.
#'    Defaults to \code{FALSE}.
#' @param differenceRaincloudPlotHorizontal, Changes the orientation of the raincloud plot so that the x-axis represents the dependent variable.
#'    Defaults to \code{FALSE}.
#' @param pairs, In this box the variables are selected for which the difference is computed. Multiple differences can be analysed at the same time by specifying different rows with two variables for which the difference is computed. In other words, each row represents a separate set of difference scores.
#' @param priorAndPosteriorPlot, Displays the prior and posterior distributions of the effect size after observing the data.
#'    Defaults to \code{FALSE}.
#' @param priorAndPosteriorPlotAdditionalInfo, Adds the Bayes factor computed with the user-defined prior; adds a probability wheel depicting how likely the data is under the null vs. alternative hypothesis; adds the median and the 95% credible interval of the posterior distribution of the effect size.
#'    Defaults to \code{TRUE}.
#' @param raincloudPlot, Displays the individual cases (colored dots), box plots, and densities for each group.
#'    Defaults to \code{FALSE}.
TTestBayesianPairedSamples <- function(
          data = NULL,
          version = "0.96.1",
          alternative = "twoSided",
          barPlot = FALSE,
          barPlotCiLevel = 0.95,
          barPlotErrorType = "ci",
          barPlotYAxisFixedToZero = TRUE,
          bayesFactorType = "BF10",
          bfRobustnessPlot = FALSE,
          bfRobustnessPlotAdditionalInfo = TRUE,
          bfSequentialPlot = FALSE,
          bfSequentialPlotRobustness = FALSE,
          defaultStandardizedEffectSize = "cauchy",
          descriptives = FALSE,
          descriptivesPlot = FALSE,
          descriptivesPlotCiLevel = 0.95,
          dienesEffectSize = "uniform",
          differenceRaincloudPlot = FALSE,
          differenceRaincloudPlotHorizontal = FALSE,
          effectSize = "standardized",
          effectSizeStandardized = "default",
          halfNormalDienesStd = 0.707,
          informativeCauchyLocation = 0,
          informativeCauchyScale = 0.707,
          informativeNormalMean = 0,
          informativeNormalStd = 0.707,
          informativeStandardizedEffectSize = "cauchy",
          informativeTDf = 1,
          informativeTLocation = 0,
          informativeTScale = 0.707,
          naAction = "perDependent",
          normalDienesMean = 0.707,
          normalDienesStd = 0.707,
          pairs = list(),
          plotHeight = 340,
          plotWidth = 420,
          priorAndPosteriorPlot = FALSE,
          priorAndPosteriorPlotAdditionalInfo = TRUE,
          priorAndPosteriorPlotCiLevel = 0.95,
          priorWidth = 0.707,
          raincloudPlot = FALSE,
          test = "student",
          uniformDienesLowerBound = 0.707,
          uniformDienesUpperBound = 0.707,
          wilcoxonSamples = 1000) {

   defaultArgCalls <- formals(jaspTTests::TTestBayesianPairedSamples)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("pairs")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspTTests", "TTestBayesianPairedSamples", "TTestBayesianPairedSamples.qml", options, version, TRUE))
}