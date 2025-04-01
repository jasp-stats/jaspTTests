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

TTestOneSample <- function(
          data = NULL,
          version = "0.19.3",
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

   return(jaspBase::runWrappedAnalysis("jaspTTests", "TTestOneSample", "TTestOneSample.qml", options, version, FALSE))
}