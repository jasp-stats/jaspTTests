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

#' Bayesian
#'
#' The one sample t-test allows the user to estimate the effect size and test the null hypothesis that the population mean equals a specific constant, i.e., the test value.
#' ## Assumptions
#' - The dependent variable is continuous
#' - The data are a random sample from the population.
#' - The dependent variable is normally distributed in the population.
#'
#' @param barPlotYAxisFixedToZero, Fix horizontal axis to 0: Forces the graph to show the default x-axis at y = 0
#'    Defaults to \code{TRUE}.
#' @param dependent, In this box the dependent variable is selected.
#' @param descriptives, Sample size, sample mean, sample standard deviation, standard error of the mean for each measure.
#'    Defaults to \code{FALSE}.
#' @param descriptivesPlot, Displays the sample mean and the confidence interval. The CI is set at 95% by default and can be changed.
#'    Defaults to \code{FALSE}.
#' @param effectSize, For the Student t-test, the effect size is given by Cohen's d; for the Wilcoxon test, the effect size is given by the matched rank biserial correlation; for the Z test, the effect size is given by Cohen's d (based on the provided population standard deviation).
#'    Defaults to \code{FALSE}.
#' @param effectSizeCi, Confidence interval for the effect size.
#'    Defaults to \code{FALSE}.
#' @param meanDifference, Average difference between the data points and the test value. For the Student's t-test and the Z test the location difference estimate is given by mean difference divided by the (hypothesized) standard deviation d; for the Wilcoxon signed-rank test, the location difference estimate is given by the Hodges-Lehmann estimate.
#'    Defaults to \code{FALSE}.
#' @param meanDifferenceCi, Confidence interval for the location parameter. By default, the confidence interval is set to 95%. This can be changed into the desired percentage.
#'    Defaults to \code{FALSE}.
#' @param normalityTest, Shapiro-Wilk test of normality.
#'    Defaults to \code{FALSE}.
#' @param qqPlot, Q-Q plot of the standardized residuals.
#'    Defaults to \code{FALSE}.
#' @param raincloudPlot, Displays the individual cases, box plot, and density.
#'    Defaults to \code{FALSE}.
#' @param raincloudPlotHorizontal, Changes the orientation of the raincloud plot so that the x-axis represents the dependent variable
#'    Defaults to \code{FALSE}.
#' @param student, The student's t-test. This options is selected by default.
#'    Defaults to \code{TRUE}.
#' @param vovkSellke, Shows the maximum ratio of the lieklihood of the obtained p value under H1 vs the likelihood of the obtained p value under H0. For example, if the two-sided p-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this p-value is at most 2.46 times more likely to occur under H1 than under H0
#'    Defaults to \code{FALSE}.
#' @param wilcoxon, Wilcoxon signed-rank test. Use when data is not normally distributed.
#'    Defaults to \code{FALSE}.
#' @param zTest, The Z test. Use for testing whether two population means are different. The test value is set to 0 by default and the standard deviation is set to 1
#'    Defaults to \code{FALSE}.
TTestOneSample <- function(
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
          meanDifference = FALSE,
          meanDifferenceCi = FALSE,
          meanDifferenceCiLevel = 0.95,
          naAction = "perVariable",
          normalityTest = FALSE,
          plotHeight = 320,
          plotWidth = 480,
          qqPlot = FALSE,
          raincloudPlot = FALSE,
          raincloudPlotHorizontal = FALSE,
          student = TRUE,
          testValue = 0,
          vovkSellke = FALSE,
          wilcoxon = FALSE,
          zTest = FALSE,
          zTestSd = 1) {

   defaultArgCalls <- formals(jaspTTests::TTestOneSample)
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

   return(jaspBase::runWrappedAnalysis("jaspTTests", "TTestOneSample", "TTestOneSample.qml", options, version, TRUE))
}