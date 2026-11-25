context("Bayesian Paired Samples T-Test")

# does not test
# - bftype (01, 10)
# - missing value exclusion
# - default cauchy and informed prior cauchy/Normal
# - error handling of plots

getTtestTable <- function(x) x[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_ttestTable"]]
getDescriptivesTable <- function(x) x[["results"]][["descriptivesContainer"]][["collection"]][["descriptivesContainer_table"]]

test_that("Main table results match", {
  options <- initTTestOptions("TTestBayesianPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$alternative <- "greater"
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
  options <- initTTestOptions("TTestBayesianPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$priorAndPosteriorPlot <- TRUE
  options$priorAndPosteriorPlotAdditionalInfo <- FALSE

  options$bfRobustnessPlot <- TRUE
  options$bfRobustnessPlotAdditionalInfo <- FALSE

  options$bfSequentialPlot <- TRUE
  options$bfSequentialPlotRobustness <- FALSE

  options$descriptives <- TRUE
  options$descriptivesPlot <- TRUE
  options$descriptivesPlotCiLevel <- 0.90

  results <- jaspTools::runAnalysis("TTestBayesianPairedSamples", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "descriptives")

  table <- getDescriptivesTable(results)[["data"]]
  jaspTools::expect_equal_tables(table,
    list(100, -5.60753128268502, -0.364486647151235, -0.18874858754, 1.05841360919316,
         0.105841360919316, -0.013010527928765, "contNormal", 100, 0.753782920490781,
         1.77852060807582, 2.03296079621, 1.53241112621044, 0.153241112621044,
         2.28740098434418, "contGamma"),
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
  options <- initTTestOptions("TTestBayesianPairedSamples")
  options$pairs <- list(c("contcor1", "contcor2"))
  options$priorAndPosteriorPlot <- TRUE
  options$priorAndPosteriorPlotAdditionalInfo <- TRUE

  options$bfRobustnessPlot <- TRUE
  options$bfRobustnessPlotAdditionalInfo <- TRUE

  options$bfSequentialPlot <- TRUE
  options$bfSequentialPlotRobustness <- TRUE

  results <- jaspTools::runAnalysis("TTestBayesianPairedSamples", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-posterior-additional")

  testPlot <- results[["state"]][["figures"]][[2]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "robustness-check-additional")

  testPlot <- results[["state"]][["figures"]][[3]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "sequential-analysis-additional")

})

test_that("Bar plot matches", {
  options <- initTTestOptions("TTestBayesianPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$barPlot <- TRUE
  options$barPlotErrorType <- "se"
  results <- jaspTools::runAnalysis("TTestBayesianPairedSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "barPlot")
})

test_that("Raincloud plot matches", {
  options <- initTTestOptions("TTestBayesianPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$raincloudPlot <- TRUE
  set.seed(12312414)
  results <- jaspTools::runAnalysis("TTestBayesianPairedSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "raincloud")
})

test_that("Raincloud difference plot matches (vertical)", {
  options <- initTTestOptions("TTestBayesianPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$differenceRaincloudPlot <- TRUE
  set.seed(12312414)
  results <- jaspTools::runAnalysis("TTestBayesianPairedSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "raincloud-diff-vertical")
})

test_that("Raincloud difference plot matches (horizontal)", {
  options <- initTTestOptions("TTestBayesianPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$differenceRaincloudPlot <- TRUE
  options$differenceRaincloudPlotHorizontal <- TRUE
  set.seed(12312414)
  results <- jaspTools::runAnalysis("TTestBayesianPairedSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "raincloud-diff-horizontal")
})

test_that("Analysis handles errors", {
  options <- initTTestOptions("TTestBayesianPairedSamples")
  dat <- errorCheckData()

  options$pairs <- list(c("x", "withInf"))
  results <- jaspTools::runAnalysis("TTestBayesianPairedSamples", dat, options)
  notes <- unlist(getTtestTable(results)[["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")

  options$pairs <- list(c("x", "xPlus1"))
  results <- jaspTools::runAnalysis("TTestBayesianPairedSamples", dat, options)
  notes <- unlist(getTtestTable(results)[["footnotes"]])
  expect_true(any(grepl("constant", notes, ignore.case=TRUE)), label = "No variance check")

  options$pairs <- list(c("x", "onePair"))
  results <- jaspTools::runAnalysis("TTestBayesianPairedSamples", dat, options)
  notes <- unlist(getTtestTable(results)[["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")
})

test_that("Descriptives plot and bar plot use only the complete pairs", {
  # pre is missing in rows 6 and 7, so the plots use the 6 complete pairs, as the raincloud plot does.
  # With the Jeffreys prior the posterior of each mean is a t distribution with 5 df around the sample
  # mean with scale sd / sqrt(6): medians 10.5 and 71 / 6 = 11.833, sample variances 1.1 and 17 / 30
  dataset <- data.frame(pre  = c(10, 11,  9, 12, 10, NA, NA, 11),
                        post = c(12, 12, 11, 13, 11, 15, 16, 12))

  options <- initTTestOptions("TTestBayesianPairedSamples")
  options$pairs <- list(c("pre", "post"))
  options$descriptivesPlot <- TRUE
  options$barPlot <- TRUE
  results <- jaspTools::runAnalysis("TTestBayesianPairedSamples", dataset, options)
  halfWidth <- qt(.975, 5) * sqrt(c(1.1, 17 / 30)) / sqrt(6)
  expect_length(results[["state"]][["figures"]], 2)
  for (figure in results[["state"]][["figures"]]) {
    plot <- figure[["obj"]]
    isErrorBar <- vapply(plot[["layers"]], function(layer) inherits(layer[["geom"]], "GeomErrorbar"), logical(1))
    errorBars <- ggplot2::layer_data(plot, which(isErrorBar))
    errorBars <- errorBars[order(errorBars[["x"]]), ]
    expect_equal(errorBars[["y"]], c(10.5, 71 / 6))
    expect_equal(errorBars[["ymax"]] - errorBars[["y"]], halfWidth)
    expect_equal(errorBars[["y"]] - errorBars[["ymin"]], halfWidth)
  }
})

test_that("Descriptives plot and bar plot show an error without complete pairs", {
  # no row has both pre and post, so there is no complete pair to plot
  dataset <- data.frame(pre  = c(10, 11,  9, NA, NA, NA),
                        post = c(NA, NA, NA, 12, 11, 13))

  options <- initTTestOptions("TTestBayesianPairedSamples")
  options$pairs <- list(c("pre", "post"))
  options$descriptivesPlot <- TRUE
  options$barPlot <- TRUE
  results <- jaspTools::runAnalysis("TTestBayesianPairedSamples", dataset, options)
  descriptives <- results[["results"]][["descriptivesContainer"]][["collection"]]
  for (plots in c("descriptivesContainer_plots", "descriptivesContainer_barPlots")) {
    plot <- descriptives[[plots]][["collection"]][[paste0(plots, "_pre - post")]]
    expect_identical(plot[["status"]], "error")
    expect_true(grepl("Number of observations is < 2", plot[["error"]][["errorMessage"]], fixed = TRUE))
  }
})
