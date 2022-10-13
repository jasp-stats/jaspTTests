context("Bayesian One Sample T-Test")

# does not test
# - bftype (01, 10)
# - missing value exclusion
# - default cauchy and informed prior cauchy/Normal
# - error handling of plots

getTtestTable <- function(x) x[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_ttestTable"]]
getDescriptivesTable <- function(x) x[["results"]][["descriptivesContainer"]][["collection"]][["descriptivesContainer_table"]]

test_that("Main table results match", {
  options <- initTTestOptions("TTestBayesianOneSample")
  options$dependent <- "contNormal"
  options$effectSizeStandardized <- "default"
  options$defaultStandardizedEffectSize <- "cauchy"
  options$priorWidth <- 0.707
  results <- jaspTools::runAnalysis("TTestBayesianOneSample", "test.csv", options)
  table <- getTtestTable(results)[["data"]]
  jaspTools::expect_equal_tables(table, list(0.508160332005368, 0.0404156306791769, "contNormal"))
})

test_that("Inferential and descriptives plots match", {
  set.seed(0)
  options <- initTTestOptions("TTestBayesianOneSample")
  options$dependent <- "contNormal"
  options$priorAndPosteriorPlot <- TRUE
  options$priorAndPosteriorPlotAdditionalInfo <- FALSE

  options$bfRobustnessPlot <- TRUE
  options$bfRobustnessPlotAdditionalInfo <- FALSE

  options$bfSequentialPlot <- TRUE
  options$bfSequentialPlotRobustness <- FALSE

  options$descriptives <- TRUE
  options$descriptivesPlot <- TRUE
  options$descriptivesPlotCiLevel <- 0.90

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

test_that("Bar plot matches", {
  options <- initTTestOptions("TTestBayesianOneSample")
  options$dependent <- "contGamma"
  options$barPlot <- TRUE
  options$barPlotErrorType <- "se"
  results <- jaspTools::runAnalysis("TTestBayesianOneSample", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "barPlot")
})

test_that("Raincloud plot matches (vertical)", {
  options <- initTTestOptions("TTestBayesianOneSample")
  options$dependent <- "contGamma"
  options$raincloudPlot <- TRUE
  set.seed(12312414)
  results <- jaspTools::runAnalysis("TTestBayesianOneSample", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "raincloud-vertical")
})

test_that("Raincloud plot matches (horizontal)", {
  options <- initTTestOptions("TTestBayesianOneSample")
  options$dependent <- "contGamma"
  options$raincloudPlot <- TRUE
  options$raincloudPlotHorizontal <- TRUE
  set.seed(12312414)
  results <- jaspTools::runAnalysis("TTestBayesianOneSample", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "raincloud-horizontal")
})

test_that("Inferential plots with additional info match", {
  set.seed(0)
  options <- initTTestOptions("TTestBayesianOneSample")
  options$dependent <- "contcor1"
  options$priorAndPosteriorPlot <- TRUE
  options$priorAndPosteriorPlotAdditionalInfo <- TRUE

  options$bfRobustnessPlot <- TRUE
  options$bfRobustnessPlotAdditionalInfo <- TRUE

  options$bfSequentialPlot <- TRUE
  options$bfSequentialPlotRobustness <- TRUE

  results <- jaspTools::runAnalysis("TTestBayesianOneSample", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-posterior-additional")

  testPlot <- results[["state"]][["figures"]][[2]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "robustness-check-additional")

  testPlot <- results[["state"]][["figures"]][[3]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "sequential-analysis-additional")

})

test_that("Prior and posterior plot custom CI level match", {
  options <- initTTestOptions("TTestBayesianOneSample")
  options$dependent <- "contcor1"
  options$priorAndPosteriorPlot <- TRUE

  options$priorAndPosteriorPlotCiLevel <- 0.8

  results  <- jaspTools::runAnalysis("TTestBayesianOneSample", "test.csv", options)
  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_inferentialPlots"]][["collection"]][["ttestContainer_inferentialPlots_contcor1"]][["collection"]][["ttestContainer_inferentialPlots_contcor1_priorAndPosteriorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-posterior-ci-level-80")

  options$priorAndPosteriorPlotCiLevel <- 0.99

  results  <- jaspTools::runAnalysis("TTestBayesianOneSample", "test.csv", options)
  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_inferentialPlots"]][["collection"]][["ttestContainer_inferentialPlots_contcor1"]][["collection"]][["ttestContainer_inferentialPlots_contcor1_priorAndPosteriorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-posterior-ci-level-99")

  options$priorAndPosteriorPlotCiLevel <- 0.999

  results  <- jaspTools::runAnalysis("TTestBayesianOneSample", "test.csv", options)
  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_inferentialPlots"]][["collection"]][["ttestContainer_inferentialPlots_contcor1"]][["collection"]][["ttestContainer_inferentialPlots_contcor1_priorAndPosteriorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-posterior-ci-level-99.9")
})

test_that("Wilcoxon results match", {
  set.seed(0)
  suppressWarnings(RNGkind(sample.kind = "Rounding"))
  options <- initTTestOptions("TTestBayesianOneSample")
  options$dependent <- c("contNormal", "contExpon")
  options$effectSizeStandardized <- "default"
  options$defaultStandardizedEffectSize <- "cauchy"
  options$priorWidth <- 0.707
  options$wilcoxonSamples <- 1e1
  options$testValue <- 1
  options$test <- "wilcoxon"
  options$alternative <- "greater"
  results <- jaspTools::runAnalysis("TTestBayesianOneSample", "test.csv", options)
  table <- getTtestTable(results)[["data"]]
  jaspTools::expect_equal_tables(table, list(0.0451492401700035, 351, 1.06493226208061, "contNormal", 42.1408097943746,
                                             3339, 0.987641807418307, "contExpon")
  )
})

test_that("Analysis handles errors", {
  options <- initTTestOptions("TTestBayesianOneSample")

  options$dependent <- "debInf"

  results <- jaspTools::runAnalysis("TTestBayesianOneSample", "test.csv", options)
  notes <- unlist(getTtestTable(results)[["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")

  options$dependent <- "debSame"
  results <- jaspTools::runAnalysis("TTestBayesianOneSample", "test.csv", options)
  notes <- unlist(getTtestTable(results)[["footnotes"]])
  expect_true(any(grepl("variance", notes, ignore.case=TRUE)), label = "No variance check")

  options$dependent <- "debMiss99"
  results <- jaspTools::runAnalysis("TTestBayesianOneSample", "test.csv", options)
  notes <- unlist(getTtestTable(results)[["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")
})
