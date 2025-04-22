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

TTestPairedSamples <- function(
          data = NULL,
          version = "0.19.3",
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

   return(jaspBase::runWrappedAnalysis("jaspTTests", "TTestPairedSamples", "TTestPairedSamples.qml", options, version, FALSE))
}