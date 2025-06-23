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

#' Independent Samples T-Test
#'
#' The independent samples t-test allows the user to estimate the effect size and test the null hypothesis that the population means of two independent groups are equal.
#' ## Assumptions
#' - The dependent variable is continuous.
#' - The observations in both groups are a random sample from the population.
#' - The dependent variable is normally distributed in each group of the independent variable.
#' - The population variances in the two groups are homogeneous.
#'
#' @param barPlotYAxisFixedToZero, Fix horizontal axis to 0: Forces the graph to show the default x-axis at y = 0
#'    Defaults to \code{TRUE}.
#' @param dependent, In this box the dependent variable is selected
#' @param descriptives, Sample size, sample mean, sample standard deviation, standard error of the mean for each group.
#'    Defaults to \code{FALSE}.
#' @param descriptivesPlot, Displays the sample means and the confidence intervals for each group. By default it is set at 95%. This can be changed into the desired percentage.
#'    Defaults to \code{FALSE}.
#' @param effectSize, For the Student t-test and Welch t-test, the effect size can be selected below; for the Mann-Whitney test, the effect size is given by the rank biserial correlation.
#'    Defaults to \code{FALSE}.
#' @param effectSizeCi, Confidence interval for the effect size based on the non-central t-distribution for Cohen's d, Glass' delta and Hedges' g, and normal approximation of the Fisher transformed rank biserial correlation.
#'    Defaults to \code{FALSE}.
#' @param equalityOfVariancesTest, BrownForsythe or Levene's tests to check if variances are qually distributed accross groups
#'    Defaults to \code{FALSE}.
#' @param group, In this box the variable defining the groups is selected. e.g experimental condition
#' @param mannWhitneyU, Non-parametric independent T-test. Use when data is not normally distributed
#'    Defaults to \code{FALSE}.
#' @param meanDifference, For the Student's t-test and Welch's t-test, the location parameter is given by mean difference; for the Mann-Whitney test, the location parameter is given by the Hodges-Lehmann estimate.
#'    Defaults to \code{FALSE}.
#' @param meanDifferenceCi, Confidence interval for the location parameter. By default, the confidence interval is set to 95%. This can be changed into the desired percentage.
#'    Defaults to \code{FALSE}.
#' @param normalityTest, Shapiro-Wilk test of normality.
#'    Defaults to \code{FALSE}.
#' @param qqPlot, Displays Q-Q plot of the standardized residuals.
#'    Defaults to \code{FALSE}.
#' @param raincloudPlot, Displays the individual cases (colored dots), box plots, and densities for each group.
#'    Defaults to \code{FALSE}.
#' @param raincloudPlotHorizontal, Changes the orientation of the raincloud plot so that the x-axis represents the dependent variable and the y-axis the grouping variable.
#'    Defaults to \code{FALSE}.
#' @param student, Good old fashioned T-test. Selected by default
#'    Defaults to \code{TRUE}.
#' @param vovkSellke, Shows the maximum ratio of the lieklihood of the obtained p value under H1 vs the likelihood of the obtained p value under H0. For example, if the two-sided p-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this p-value is at most 2.46 times more likely to occur under H1 than under H0.
#'    Defaults to \code{FALSE}.
#' @param welch, Use when variances are not equally distributed accross groups
#'    Defaults to \code{FALSE}.
TTestIndependentSamples <- function(
          data = NULL,
          version = "0.95",
          formula = NULL,
          alternative = "twoSided",
          barPlot = FALSE,
          barPlotCiLevel = 0.95,
          barPlotErrorType = "ci",
          barPlotYAxisFixedToZero = TRUE,
          dependent = list(types = list(), value = list()),
          descriptives = FALSE,
          descriptivesPlot = FALSE,
          descriptivesPlotCiLevel = 0.95,
          effectSize = FALSE,
          effectSizeCi = FALSE,
          effectSizeCiLevel = 0.95,
          effectSizeType = "cohen",
          equalityOfVariancesTest = FALSE,
          equalityOfVariancesTestType = "brownForsythe",
          group = list(types = list(), value = ""),
          mannWhitneyU = FALSE,
          meanDifference = FALSE,
          meanDifferenceCi = FALSE,
          meanDifferenceCiLevel = 0.95,
          naAction = "perDependent",
          normalityTest = FALSE,
          plotHeight = 300,
          plotWidth = 350,
          qqPlot = FALSE,
          raincloudPlot = FALSE,
          raincloudPlotHorizontal = FALSE,
          student = TRUE,
          vovkSellke = FALSE,
          welch = FALSE) {

   defaultArgCalls <- formals(jaspTTests::TTestIndependentSamples)
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
   optionsWithFormula <- c("dependent", "group")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspTTests", "TTestIndependentSamples", "TTestIndependentSamples.qml", options, version, TRUE))
}