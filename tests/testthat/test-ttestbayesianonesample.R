context("Bayesian One Sample T-Test")

# does not test
# - bftype (01, 10)
# - missing value exclusion
# - default cauchy and informed prior cauchy/Normal
# - error handling of plots

getTtestTable <- function(x) x[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_ttestTable"]]
getDescriptivesTable <- function(x) x[["results"]][["descriptivesContainer"]][["collection"]][["descriptivesContainer_table"]]

test_that("Main table results match", {
  options <- jaspTools::analysisOptions("TTestBayesianOneSample")
  options$variables <- "contNormal"
  options$effectSizeStandardized <- "default"
  options$defaultStandardizedEffectSize <- "cauchy"
  options$priorWidth <- 0.707
  results <- jaspTools::runAnalysis("TTestBayesianOneSample", "test.csv", options)
  table <- getTtestTable(results)[["data"]]
  jaspTools::expect_equal_tables(table, list(0.508160332005368, 0.0404156306791769, "contNormal"))
})

test_that("Inferential and descriptives plots match", {
  set.seed(0)
  options <- jaspTools::analysisOptions("TTestBayesianOneSample")
  options$variables <- "contNormal"
  options$plotPriorAndPosterior <- TRUE
  options$plotPriorAndPosteriorAdditionalInfo <- FALSE

  options$plotBayesFactorRobustness <- TRUE
  options$plotBayesFactorRobustnessAdditionalInfo <- FALSE

  options$plotSequentialAnalysis <- TRUE
  options$plotSequentialAnalysisRobustness <- FALSE

  options$descriptives <- TRUE
  options$descriptivesPlots <- TRUE
  options$descriptivesPlotsCredibleInterval <- 0.90

  results <- jaspTools::runAnalysis("TTestBayesianOneSample", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "descriptives")

  table <- getDescriptivesTable(results)[["data"]]
  jaspTools::expect_equal_tables(table,
    list(100, -5.60753128268502, -0.364486647151235, -0.18874858754, 1.05841360919316,
         0.105841360919316, -0.013010527928765, "contNormal"),
    label = "Descriptives table"
  )

  testPlot <- results[["state"]][["figures"]][[2]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-posterior")

  testPlot <- results[["state"]][["figures"]][[3]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "robustness-check")

  testPlot <- results[["state"]][["figures"]][[4]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "sequential-analysis")

})

test_that("Raincloud plot matches (vertical)", {
  options <- jaspTools::analysisOptions("TTestBayesianOneSample")
  options$variables <- "contGamma"
  options$descriptivesPlotsRainCloud <- TRUE
  set.seed(12312414)
  results <- jaspTools::runAnalysis("TTestBayesianOneSample", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "raincloud-vertical")
})

test_that("Raincloud plot matches (horizontal)", {
  options <- jaspTools::analysisOptions("TTestBayesianOneSample")
  options$variables <- "contGamma"
  options$descriptivesPlotsRainCloud <- TRUE
  options$descriptivesPlotsRainCloudHorizontalDisplay <- TRUE
  set.seed(12312414)
  results <- jaspTools::runAnalysis("TTestBayesianOneSample", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "raincloud-horizontal")
})

test_that("Inferential plots with additional info match", {
  set.seed(0)
  options <- jaspTools::analysisOptions("TTestBayesianOneSample")
  options$variables <- "contcor1"
  options$plotPriorAndPosterior <- TRUE
  options$plotPriorAndPosteriorAdditionalInfo <- TRUE

  options$plotBayesFactorRobustness <- TRUE
  options$plotBayesFactorRobustnessAdditionalInfo <- TRUE

  options$plotSequentialAnalysis <- TRUE
  options$plotSequentialAnalysisRobustness <- TRUE

  results <- jaspTools::runAnalysis("TTestBayesianOneSample", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-posterior-additional")

  testPlot <- results[["state"]][["figures"]][[2]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "robustness-check-additional")

  testPlot <- results[["state"]][["figures"]][[3]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "sequential-analysis-additional")

})

test_that("Prior and posterior plot custom CI level match", {
  options <- jaspTools::analysisOptions("TTestBayesianOneSample")
  options$variables <- "contcor1"
  options$plotPriorAndPosterior <- TRUE

  options$priorAndPosteriorPlotsCredibleInterval <- 0.8

  results  <- jaspTools::runAnalysis("TTestBayesianOneSample", "test.csv", options)
  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_inferentialPlots"]][["collection"]][["ttestContainer_inferentialPlots_contcor1"]][["collection"]][["ttestContainer_inferentialPlots_contcor1_plotPriorAndPosterior"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-posterior-ci-level-80")

  options$priorAndPosteriorPlotsCredibleInterval <- 0.99

  results  <- jaspTools::runAnalysis("TTestBayesianOneSample", "test.csv", options)
  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_inferentialPlots"]][["collection"]][["ttestContainer_inferentialPlots_contcor1"]][["collection"]][["ttestContainer_inferentialPlots_contcor1_plotPriorAndPosterior"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-posterior-ci-level-99")

  options$priorAndPosteriorPlotsCredibleInterval <- 0.999

  results  <- jaspTools::runAnalysis("TTestBayesianOneSample", "test.csv", options)
  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_inferentialPlots"]][["collection"]][["ttestContainer_inferentialPlots_contcor1"]][["collection"]][["ttestContainer_inferentialPlots_contcor1_plotPriorAndPosterior"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-posterior-ci-level-99.9")
})

test_that("Wilcoxon results match", {
  set.seed(0)
  suppressWarnings(RNGkind(sample.kind = "Rounding"))
  options <- jaspTools::analysisOptions("TTestBayesianOneSample")
  options$variables <- c("contNormal", "contExpon")
  options$effectSizeStandardized <- "default"
  options$defaultStandardizedEffectSize <- "cauchy"
  options$priorWidth <- 0.707
  options$wilcoxonSamplesNumber <- 1e1
  options$testValue <- 1
  options$testStatistic <- "Wilcoxon"
  options$hypothesis <- "greaterThanTestValue"
  results <- jaspTools::runAnalysis("TTestBayesianOneSample", "test.csv", options)
  table <- getTtestTable(results)[["data"]]
  jaspTools::expect_equal_tables(table, list(0.0451492401700035, 351, 1.06493226208061, "contNormal", 42.1408097943746,
                                             3339, 0.987641807418307, "contExpon")
  )
})

test_that("Analysis handles errors", {
  options <- jaspTools::analysisOptions("TTestBayesianOneSample")

  options$variables <- "debInf"

  results <- jaspTools::runAnalysis("TTestBayesianOneSample", "test.csv", options)
  notes <- unlist(getTtestTable(results)[["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")

  options$variables <- "debSame"
  results <- jaspTools::runAnalysis("TTestBayesianOneSample", "test.csv", options)
  notes <- unlist(getTtestTable(results)[["footnotes"]])
  expect_true(any(grepl("variance", notes, ignore.case=TRUE)), label = "No variance check")

  options$variables <- "debMiss99"
  results <- jaspTools::runAnalysis("TTestBayesianOneSample", "test.csv", options)
  notes <- unlist(getTtestTable(results)[["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")
})
