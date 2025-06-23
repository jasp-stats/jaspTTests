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

#' Bayesian One Sample T-Test
#'
#' The paired samples t-test allows you to estimate the effect size and test the null hypothesis that the population mean equals a specific constant,i.e., the test value.
#' ## Assumptions
#' - Continuous dependent variable.
#' - The data are a random sample from the population.
#' - The dependent variable is normally distributed in the population.
#'
#' @param barPlotYAxisFixedToZero, Fix horizontal axis to 0: Forces the graph to show the default x-axis at y = 0
#'    Defaults to \code{TRUE}.
#' @param bfRobustnessPlot, Displays the Bayes factor accross different values of cauchy prior width. The scale of the Cauchy prior is varied between 0 and 1.5, creating progressively more uninformative priors.
#'    Defaults to \code{FALSE}.
#' @param bfRobustnessPlotAdditionalInfo, Adds the Bayes factor computed with the user-defined prior; adds a probability wheel depicting how likely the data is under the null vs. alternative hypothesis
#'    Defaults to \code{TRUE}.
#' @param bfSequentialPlot, Displays the development of the Bayes factor as the data come in using the user-defined prior.
#'    Defaults to \code{FALSE}.
#' @param bfSequentialPlotRobustness, Adds the results of the sequential analysis using the wide (scale=1) and ultrawide prior (scale=sqrt(2)).
#'    Defaults to \code{FALSE}.
#' @param dependent, In this box the dependent variable is selected.
#' @param descriptivesPlot, Display descriptives plots
#'    Defaults to \code{FALSE}.
#' @param descriptivesPlotCiLevel, Display central credible intervals. A credible interval shows the probability that the true effect size lies within certain values. The default credible interval is set at 95%.
#' @param priorAndPosteriorPlot, Displays the prior and posterior distribution of the effect size after the data is seen.
#'    Defaults to \code{FALSE}.
#' @param priorAndPosteriorPlotAdditionalInfo, Adds the Bayes factor computed with the user-defined prior; adds a probability wheel depicting how likely the data is under the null vs. alternative hypothesis
#'    Defaults to \code{TRUE}.
#' @param priorAndPosteriorPlotCiLevel, adds the median and the 95% credible interval of the posterior distribution of the effect size.
#' @param raincloudPlot, Displays the individual cases (colored dots), box plots, and densities for each group.
#'    Defaults to \code{FALSE}.
#' @param raincloudPlotHorizontal, Changes the orientation of the raincloud plot so that the x-axis represents the dependent variable.
#'    Defaults to \code{FALSE}.
#' @param testValue, Test value specified in the null hypothesis. The mean of the data is compared to this value.
TTestBayesianOneSample <- function(
          data = NULL,
          version = "0.95",
          formula = NULL,
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
          dependent = list(types = list(), value = list()),
          descriptives = FALSE,
          descriptivesPlot = FALSE,
          descriptivesPlotCiLevel = 0.95,
          dienesEffectSize = "uniform",
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
          naAction = "listwise",
          normalDienesMean = 0.707,
          normalDienesStd = 0.707,
          plotHeight = 240,
          plotWidth = 320,
          priorAndPosteriorPlot = FALSE,
          priorAndPosteriorPlotAdditionalInfo = TRUE,
          priorAndPosteriorPlotCiLevel = 0.95,
          priorWidth = 0.707,
          raincloudPlot = FALSE,
          raincloudPlotHorizontal = FALSE,
          standardizedEffectSize = TRUE,
          test = "student",
          testValue = 0,
          uniformDienesLowerBound = 0.707,
          uniformDienesUpperBound = 0.707,
          wilcoxonSamples = 1000) {

   defaultArgCalls <- formals(jaspTTests::TTestBayesianOneSample)
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

   if (!is.null(formula)) {
      if (!inherits(formula, "formula")) {
         formula <- as.formula(formula)
      }
      options$formula <- jaspBase::jaspFormula(formula, data)
   }
   optionsWithFormula <- c("dependent")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspTTests", "TTestBayesianOneSample", "TTestBayesianOneSample.qml", options, version, TRUE))
}