context("Paired Samples TTest")

# does not test
# - missing values exclusion
# - error handling of plots

test_that("Main table results match for one pair * multiple tests", {
  options <- initTTestOptions("TTestPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$wilcoxon <- TRUE
  options$alternative <- "greater"
  options$meanDifference <- TRUE
  options$effectSize <- TRUE
  options$effectSizeCi <- TRUE
  options$vovkSellke <- TRUE
  results <- jaspTools::runAnalysis("TTestPairedSamples", "test.csv", options)
  table <- results[["results"]][["ttest"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("TRUE", -11.6121720596087, 1, -1.16121720596087, 99, 0.188325608332799,
                                      -1.37211873031366, -2.22170938375, 1, 0.191325909773409, "-",
                                      "Student", "<unicode>", "contNormal", "contGamma", "FALSE",
                                      199, 1, -0.921188118811881, "", 0.114677177073573, -0.94536640190499,
                                      -2.1796113893332, 0.999999999999999, "", "", "Wilcoxon", "<unicode>",
                                      "", "", -7.99754358622852))
})

test_that("Main table results match for multiple pairs * one test", {
  options <- initTTestOptions("TTestPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"), c("contNormal", "contcor1"))
  results <- jaspTools::runAnalysis("TTestPairedSamples", "test.csv", options)
  table <- results[["results"]][["ttest"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list("FALSE", 99, 3.4809614504484e-20, "-", -11.6121720596087, "contNormal",
	                         "contGamma", "FALSE", 99, 0.0750733655901379, "-", -1.79895113042557,
	                         "contNormal", "contcor1")
  )
})

test_that("Normality table matches", {
  options <- initTTestOptions("TTestPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$normalityTest <- TRUE
  results <- jaspTools::runAnalysis("TTestPairedSamples", "test.csv", options)
  table <- results[["results"]][["AssumptionChecks"]][["collection"]][["AssumptionChecks_ttestNormalTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(0.969542808533914, 0.0203952735337306, "-", "contNormal", "contGamma")
  )
})

test_that("Descriptives table matches", {
  options <- initTTestOptions("TTestPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$descriptives <- TRUE
  results <- jaspTools::runAnalysis("TTestPairedSamples", "test.csv", options)
  table <- results[["results"]][["ttestDescriptives"]][["collection"]][["ttestDescriptives_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
      list(100, -5.60753128268502, -0.18874858754, 1.05841360919316, 0.105841360919316,
           "contNormal", 100, 0.753782920490781, 2.03296079621, 1.53241112621044,
           0.153241112621044, "contGamma"))
})

test_that("Descriptives plot matches", {
  options <- initTTestOptions("TTestPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$descriptivesPlot <- TRUE
  results <- jaspTools::runAnalysis("TTestPairedSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "descriptives")
})

test_that("Bar plot matches", {
  options <- initTTestOptions("TTestPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$barPlot <- TRUE
  options$barPlotErrorType <- "se"
  results <- jaspTools::runAnalysis("TTestPairedSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "barPlot")
})

test_that("Raincloud plot matches", {
  options <- initTTestOptions("TTestPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$raincloudPlot <- TRUE
  set.seed(12312414)
  results <- jaspTools::runAnalysis("TTestPairedSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "raincloud")
})

test_that("Raincloud difference plot matches (vertical)", {
  options <- initTTestOptions("TTestPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$differenceRaincloudPlot <- TRUE
  set.seed(12312414)
  results <- jaspTools::runAnalysis("TTestPairedSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "raincloud-diff-vertical")
})

test_that("Raincloud difference plot matches (horizontal)", {
  options <- initTTestOptions("TTestPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$differenceRaincloudPlot <- TRUE
  options$differenceRaincloudPlotHorizontal <- TRUE
  set.seed(12312414)
  results <- jaspTools::runAnalysis("TTestPairedSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "raincloud-diff-horizontal")
})

test_that("Analysis handles errors", {
  options <- initTTestOptions("TTestPairedSamples")
  dat <- errorCheckData()

  options$pairs <- list(c("x", "withInf"))
  results <- jaspTools::runAnalysis("TTestPairedSamples", dat, options)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")

  options$pairs <- list(c("x", "xPlus1"))
  results <- jaspTools::runAnalysis("TTestPairedSamples", dat, options)
  expect_identical(results[["status"]], "validationError", label = "No variance check")
  expect_true(grepl("variance", results[["results"]][["errorMessage"]], ignore.case=TRUE), label = "No variance message")

  options$pairs <- list(c("x", "onePair"))
  results <- jaspTools::runAnalysis("TTestPairedSamples", dat, options)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")

  options <- initTTestOptions("TTestPairedSamples")
  options$pairs <- list(c("contNormal", "contNormal"))
  results <- jaspTools::runAnalysis("TTestPairedSamples", "test.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError")
})

test_that("Descriptives plot and bar plot use only the complete pairs", {
  # pre is missing in rows 6 and 7, so the plots use the 6 complete pairs, as the raincloud plot does:
  # means 10.5 and 71 / 6 = 11.833. The within-subject 95% CI for two conditions (Cousineau-Morey) has
  # half-width qt(.975, 5) * sd(pre - post) / sqrt(2 * 6), with sd(pre - post) = sqrt(4 / 15), so 0.383
  dataset <- data.frame(pre  = c(10, 11,  9, 12, 10, NA, NA, 11),
                        post = c(12, 12, 11, 13, 11, 15, 16, 12))

  options <- initTTestOptions("TTestPairedSamples")
  options$pairs <- list(c("pre", "post"))
  options$descriptivesPlot <- TRUE
  options$barPlot <- TRUE
  results <- jaspTools::runAnalysis("TTestPairedSamples", dataset, options)
  halfWidth <- qt(.975, 5) * sqrt(4 / 15) / sqrt(12)
  expect_length(results[["state"]][["figures"]], 2)
  for (figure in results[["state"]][["figures"]]) {
    plot <- figure[["obj"]]
    isErrorBar <- vapply(plot[["layers"]], function(layer) inherits(layer[["geom"]], "GeomErrorbar"), logical(1))
    errorBars <- ggplot2::layer_data(plot, which(isErrorBar))
    errorBars <- errorBars[order(errorBars[["x"]]), ]
    expect_equal(errorBars[["y"]], c(10.5, 71 / 6))
    expect_equal(errorBars[["ymax"]] - errorBars[["y"]], rep(halfWidth, 2))
    expect_equal(errorBars[["y"]] - errorBars[["ymin"]], rep(halfWidth, 2))
  }
})

test_that("Descriptives plot and bar plot show an error without complete pairs", {
  # no row has both pre and post, so there is no complete pair to plot
  dataset <- data.frame(pre  = c(10, 11,  9, NA, NA, NA),
                        post = c(NA, NA, NA, 12, 11, 13))

  options <- initTTestOptions("TTestPairedSamples")
  options$pairs <- list(c("pre", "post"))
  options$descriptivesPlot <- TRUE
  options$barPlot <- TRUE
  results <- jaspTools::runAnalysis("TTestPairedSamples", dataset, options)
  descriptives <- results[["results"]][["ttestDescriptives"]][["collection"]]
  for (plots in c("ttestDescriptives_plots", "ttestDescriptives_barPlots")) {
    plot <- descriptives[[plots]][["collection"]][[paste0(plots, "_pre - post")]]
    expect_identical(plot[["status"]], "error")
    expect_true(grepl("Number of observations is < 1", plot[["error"]][["errorMessage"]], fixed = TRUE))
  }
})

test_that("Descriptives plot and bar plot draw a single complete pair", {
  # only row 4 has both pre and post, so the plots draw that pair (pre 12, post 14) without error bars
  dataset <- data.frame(pre  = c(10, 11, NA, 12),
                        post = c(NA, NA, 12, 14))

  options <- initTTestOptions("TTestPairedSamples")
  options$pairs <- list(c("pre", "post"))
  options$descriptivesPlot <- TRUE
  options$barPlot <- TRUE
  results <- jaspTools::runAnalysis("TTestPairedSamples", dataset, options)
  descriptives <- results[["results"]][["ttestDescriptives"]][["collection"]]
  for (plots in c("ttestDescriptives_plots", "ttestDescriptives_barPlots")) {
    plot <- descriptives[[plots]][["collection"]][[paste0(plots, "_pre - post")]]
    expect_identical(plot[["status"]], "complete")
  }
  expect_length(results[["state"]][["figures"]], 2)
  for (figure in results[["state"]][["figures"]]) {
    plot <- figure[["obj"]]
    isMean <- vapply(plot[["layers"]], function(layer) inherits(layer[["geom"]], c("GeomPoint", "GeomBar")), logical(1))
    means <- ggplot2::layer_data(plot, which(isMean)[1])
    expect_equal(means[order(means[["x"]]), "y"], c(12, 14))
    yLimits <- ggplot2::ggplot_build(plot)[["layout"]][["panel_scales_y"]][[1]]$get_limits()
    expect_true(yLimits[1] <= 12 && yLimits[2] >= 14)
  }
})
