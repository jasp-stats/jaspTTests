context("Bayesian Paired Samples T-Test")

# does not test
# - bftype (01, 10)
# - missing value exclusion
# - default cauchy and informed prior cauchy/Normal
# - error handling of plots

getTtestTable <- function(x) x[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_ttestTable"]]
getDescriptivesTable <- function(x) x[["results"]][["descriptivesContainer"]][["collection"]][["descriptivesContainer_table"]]

test_that("Main table results match", {
  options <- jaspTools::analysisOptions("TTestBayesianPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$hypothesis <- "groupOneGreater"
  options$effectSizeStandardized <- "informative"
  options$informativeStandardizedEffectSize <- "t"
  options$informativeTLocation <- 0.2
  options$informativeTScale <- 0.5
  options$informativeTDf <- 2
  results <- jaspTools::runAnalysis("TTestBayesianPairedSamples", "test.csv", options)
  table <- getTtestTable(results)[["data"]]
  jaspTools::expect_equal_tables(table, list(0, 1.05297943818791e-20, "-", "contNormal", "contGamma"))
})

test_that("Inferential and descriptives plots match", {
  set.seed(0)
  options <- jaspTools::analysisOptions("TTestBayesianPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$plotPriorAndPosterior <- TRUE
  options$plotPriorAndPosteriorAdditionalInfo <- FALSE

  options$plotBayesFactorRobustness <- TRUE
  options$plotBayesFactorRobustnessAdditionalInfo <- FALSE

  options$plotSequentialAnalysis <- TRUE
  options$plotSequentialAnalysisRobustness <- FALSE

  options$descriptives <- TRUE
  options$descriptivesPlots <- TRUE
  options$descriptivesPlotsCredibleInterval <- 0.90

  results <- jaspTools::runAnalysis("TTestBayesianPairedSamples", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "descriptives")

  table <- getDescriptivesTable(results)[["data"]]
  jaspTools::expect_equal_tables(table,
    list(100, -0.364486647151235, -0.18874858754, 1.05841360919316, 0.105841360919316,
         -0.013010527928765, "contNormal", 100, 1.77852060807581, 2.03296079621,
         1.53241112621044, 0.153241112621044, 2.28740098434418, "contGamma"
    ),
    label = "Descriptives table"
  )

  testPlot <- results[["state"]][["figures"]][[2]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-posterior")

  testPlot <- results[["state"]][["figures"]][[3]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "robustness-check")

  testPlot <- results[["state"]][["figures"]][[4]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "sequential-analysis")

})

test_that("Inferential plots with additional info match", {
  set.seed(0)
  options <- jaspTools::analysisOptions("TTestBayesianPairedSamples")
  options$pairs <- list(c("contcor1", "contcor2"))
  options$plotPriorAndPosterior <- TRUE
  options$plotPriorAndPosteriorAdditionalInfo <- TRUE

  options$plotBayesFactorRobustness <- TRUE
  options$plotBayesFactorRobustnessAdditionalInfo <- TRUE

  options$plotSequentialAnalysis <- TRUE
  options$plotSequentialAnalysisRobustness <- TRUE

  results <- jaspTools::runAnalysis("TTestBayesianPairedSamples", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-posterior-additional")

  testPlot <- results[["state"]][["figures"]][[2]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "robustness-check-additional")

  testPlot <- results[["state"]][["figures"]][[3]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "sequential-analysis-additional")

})

test_that("Raincloud plot matches", {
  options <- jaspTools::analysisOptions("TTestBayesianPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$descriptivesPlotsRainCloud <- TRUE
  set.seed(12312414)
  results <- jaspTools::runAnalysis("TTestBayesianPairedSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "raincloud")
})

test_that("Raincloud difference plot matches (vertical)", {
  options <- jaspTools::analysisOptions("TTestBayesianPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$descriptivesPlotsRainCloudDifference <- TRUE
  set.seed(12312414)
  results <- jaspTools::runAnalysis("TTestBayesianPairedSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "raincloud-diff-vertical")
})

test_that("Raincloud difference plot matches (horizontal)", {
  options <- jaspTools::analysisOptions("TTestBayesianPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$descriptivesPlotsRainCloudDifference <- TRUE
  options$descriptivesPlotsRainCloudDifferenceHorizontalDisplay <- TRUE
  set.seed(12312414)
  results <- jaspTools::runAnalysis("TTestBayesianPairedSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "raincloud-diff-horizontal")
})

test_that("Analysis handles errors", {
  options <- jaspTools::analysisOptions("TTestBayesianPairedSamples")

  options$pairs <- list(c("contNormal", "debInf"))

  results <- jaspTools::runAnalysis("TTestBayesianPairedSamples", "test.csv", options)
  notes <- unlist(getTtestTable(results)[["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")

  options$pairs <- list(c("contNormal", "debSame"))
  results <- jaspTools::runAnalysis("TTestBayesianPairedSamples", "test.csv", options)
  notes <- unlist(getTtestTable(results)[["footnotes"]])
  expect_true(any(grepl("variance", notes, ignore.case=TRUE)), label = "No variance check")

  options$pairs <- list(c("contNormal", "debMiss99"))
  results <- jaspTools::runAnalysis("TTestBayesianPairedSamples", "test.csv", options)
  notes <- unlist(getTtestTable(results)[["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")
})
