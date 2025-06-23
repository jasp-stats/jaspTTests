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

#' Paired Samples T-Test
#'
#' The paired samples t-test allows the user to estimate the effect size and test the null hypothesis that the population mean of the difference between observations equals 0 in dependent groups.
#' ## Assumptions
#' - The difference score is continuous.
#' - The difference scores are a random sample from the population.
#' - The difference scores are normally distributed in the population.
#'
#' @param barPlotYAxisFixedToZero, Fix horizontal axis to 0: Forces the graph to show the default x-axis at y = 0
#'    Defaults to \code{TRUE}.
#' @param descriptives, Sample size, sample mean, sample standard deviation, standard error of the mean for each measure.
#'    Defaults to \code{FALSE}.
#' @param descriptivesPlot, Displays the sample means and the confidence intervals for each measure (see Morey [2008] for the computation of the standard error of the mean in paired designs.)
#'    Defaults to \code{FALSE}.
#' @param differenceRaincloudPlot, Displays a raincloud plot of the differences between the two measures.
#'    Defaults to \code{FALSE}.
#' @param differenceRaincloudPlotHorizontal, Changes the orientation of the raincloud difference plot so that the x-axis represents the dependent variable and the y-axis the difference between measures.
#'    Defaults to \code{FALSE}.
#' @param effectSize, For the Student t-test, the effect size is given by Cohen's d; for the Wilcoxon test, the effect size is given by the matched rank biserial correlation.
#'    Defaults to \code{FALSE}.
#' @param effectSizeCi, Confidence interval for the effect size.
#'    Defaults to \code{FALSE}.
#' @param effectSizeCorrection, Correct the effect size for the correlation between the observed values, to prevent overestimating the effect (Dunlap et al., 1996).
#'    Defaults to \code{FALSE}.
#' @param meanDifference, For the Student's t-test the location parameter is given by mean difference d; for the Wilcoxon signed-rank test, the location parameter is given by the Hodges-Lehmann estimate.
#'    Defaults to \code{FALSE}.
#' @param meanDifferenceCi, Confidence interval for the location parameter. By default, the confidence interval is set to 95%. This can be changed into the desired percentage.
#'    Defaults to \code{FALSE}.
#' @param normalityTest, Shapiro-Wilk test of normality.
#'    Defaults to \code{FALSE}.
#' @param pairs, The variables here have their difference computed. Multiple differences can be analysed at the same time by specifying different rows. In other words, each row represents a difference score.
#' @param qqPlot, Q-Q plot of the standardized residuals.
#'    Defaults to \code{FALSE}.
#' @param raincloudPlot, Displays the individual cases (colored dots), box plots, and densities for each measure.
#'    Defaults to \code{FALSE}.
#' @param student, Student's paired sample t-test.This option is selected by default
#'    Defaults to \code{TRUE}.
#' @param vovkSellke, Shows the maximum ratio of the lieklihood of the obtained p value under H1 vs the likelihood of the obtained p value under H0. For example, if the two-sided p-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this p-value is at most 2.46 times more likely to occur under H1 than under H0.
#'    Defaults to \code{FALSE}.
#' @param wilcoxon, Non-parametric version of paired samples t-test. Use when data is not normally distributed.
#'    Defaults to \code{FALSE}.
TTestPairedSamples <- function(
          data = NULL,
          version = "0.95",
          alternative = "twoSided",
          barPlot = FALSE,
          barPlotCiLevel = 0.95,
          barPlotErrorType = "ci",
          barPlotYAxisFixedToZero = TRUE,
          descriptives = FALSE,
          descriptivesPlot = FALSE,
          descriptivesPlotCiLevel = 0.95,
          differenceRaincloudPlot = FALSE,
          differenceRaincloudPlotHorizontal = FALSE,
          effectSize = FALSE,
          effectSizeCi = FALSE,
          effectSizeCiLevel = 0.95,
          effectSizeCorrection = FALSE,
          meanDifference = FALSE,
          meanDifferenceCi = FALSE,
          meanDifferenceCiLevel = 0.95,
          naAction = "perDependent",
          normalityTest = FALSE,
          pairs = list(),
          plotHeight = 300,
          plotWidth = 350,
          qqPlot = FALSE,
          raincloudPlot = FALSE,
          student = TRUE,
          vovkSellke = FALSE,
          wilcoxon = FALSE) {

   defaultArgCalls <- formals(jaspTTests::TTestPairedSamples)
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

   return(jaspBase::runWrappedAnalysis("jaspTTests", "TTestPairedSamples", "TTestPairedSamples.qml", options, version, TRUE))
}