#
# Copyright (C) 2013-2022 University of Amsterdam
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

# This is a generated file. Don't change it

TTestBayesianOneSample <- function(
          data = NULL,
          version = "0.18.2",
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
          dependent = list(),
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
          naAction = "perDependent",
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

   if (!is.null(formula)) {
      if (!inherits(formula, "formula")) {
         formula <- as.formula(formula)
      }
      options$formula <- jaspBase::jaspFormula(formula, data)
   }

   optionsWithFormula <- c("dependent")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspTTests::TTestBayesianOneSample", data, options, version))
}