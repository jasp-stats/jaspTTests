context("One Sample TTest")

# does not test
# - missing values exclusion
# - error handling of plots

test_that("Main table results match for t-test", {
  options <- initTTestOptions("TTestOneSample")
  options$dependent <- "contGamma"
  options$meanDifference <- TRUE
  options$effectSize <- TRUE
  options$vovkSellke <- TRUE
  results <- jaspTools::runAnalysis("TTestOneSample", "test.csv", options)
  table <- results[["results"]][["ttest"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("FALSE", 6.42284229078859e+20, 1.32664189226908, 99, 0.137112703830157,
                                      2.03296079621, 1.08315413981152e-23, 13.2664189226908, "contGamma"
                                 )
  )
})

test_that("Main table results match for Wilcoxon signed rank", {
  options <- initTTestOptions("TTestOneSample")
  options$dependent <- "contNormal"
  options$meanDifference <- TRUE
  options$effectSize <- TRUE
  options$effectSizeCi <- TRUE
  options$effectSizeSE <- TRUE
  options$student <- FALSE
  options$wilcoxon <- TRUE

  results <- jaspTools::runAnalysis("TTestOneSample", "test.csv", options)
  table <- results[["results"]][["ttest"]][["data"]]

  jaspTools::expect_equal_tables(table,
                                 list("FALSE", 1789, -0.291485148514852, 0.114677177073573, -0.482275183604466,
                                      -0.225731139327267, 0.0114424559827519, -0.0742951335226289,
                                      "contNormal"))
})

# https://github.com/jasp-stats/jasp-issues/issues/1158
test_that("Rank biserial is consistent with rcompanion::wilcoxonOneSampleRC", {
  tempTestDat <- data.frame("marieAguirre"=c(0, 1/3, 1/2, 3/7, 1/4, 1/4, 1/2, 0, 0, 1/5, 3/7, 3/4, 1, 1, 3/4, 0, 1, 1))

  options <- initTTestOptions("TTestOneSample")
  options$testValue <- 0.5
  options$meanDifference <- TRUE
  options$effectSize <- TRUE
  options$wilcoxon <- TRUE
  options$effectSize <- TRUE
  options$meanDifferenceCi <- TRUE
  options$effectSizeCi <- TRUE
  options$effectSizeSE <- TRUE
  options$dependent <- "marieAguirre"

  results <- jaspTools::runAnalysis("TTestOneSample", tempTestDat, options)

  resultTable <- results[["results"]][["ttest"]][["data"]]

  jaspTools::expect_equal_tables(
    "test"=resultTable,
    "ref"=list("TRUE", -0.385804772181173, -0.0909350568744795, 17, 0.236189024145726,
               -0.552576696667502, -0.219042855717775, -0.0338624338624338,
               0.704426623388777, "Student", 0.373348871605267, 0.151317987992907,
               "marieAguirre", "FALSE", 61, -0.102941176470588, "", 0.276970835977861,
               -0.578840649433725, -0.28571825336767, -6.94926732683898e-05,
               0.73267192272089, "Wilcoxon", 0.425265962284788, 0.166692689375401,
               "marieAguirre"))
})

test_that("Main table results match for Z-test", {
  options <- initTTestOptions("TTestOneSample")
  options$dependent <- "contNormal"
  options$student <- FALSE
  options$zTest <- TRUE
  options$zTestSd <- 1.5
  options$effectSize <- TRUE
  options$effectSizeCi <- TRUE
  results <- jaspTools::runAnalysis("TTestOneSample", "test.csv", options)
  table <- results[["results"]][["ttest"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("FALSE", -1.25832391693333, -0.125832391693333, 0.100395064390618,
                                      -0.321828790147339, 0.208274634966236, 0.070164006760672, "contNormal"
                                 ))
})

test_that("Normality table matches", {
  options <- initTTestOptions("TTestOneSample")
  options$dependent <- "contGamma"
  options$normalityTest <- TRUE
  results <- jaspTools::runAnalysis("TTestOneSample", "test.csv", options)
  table <- results[["results"]][["AssumptionChecks"]][["collection"]][["AssumptionChecks_ttestNormalTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(0.876749741598208, 1.32551553117109e-07, "contGamma"))
})

test_that("Descriptives table matches", {
  options <- initTTestOptions("TTestOneSample")
  options$dependent <- "contGamma"
  options$descriptives <- TRUE
  results <- jaspTools::runAnalysis("TTestOneSample", "test.csv", options)
  table <- results[["results"]][["ttestDescriptives"]][["collection"]][["ttestDescriptives_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(100, 0.753782920490781, 2.03296079621, 1.53241112621044, 0.153241112621044,
                                      "contGamma"))
})

test_that("Descriptives plot matches", {
  options <- initTTestOptions("TTestOneSample")
  options$dependent <- "contGamma"
  options$descriptivesPlot <- TRUE
  results <- jaspTools::runAnalysis("TTestOneSample", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "descriptives")
})

test_that("Bar plot matches", {
  options <- initTTestOptions("TTestOneSample")
  options$dependent <- "contGamma"
  options$barPlot <- TRUE
  options$barPlotErrorType <- "se"
  results <- jaspTools::runAnalysis("TTestOneSample", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "barPlot")
})

test_that("Raincloud plot matches (vertical)", {
  options <- initTTestOptions("TTestOneSample")
  options$dependent <- "contGamma"
  options$raincloudPlot <- TRUE
  set.seed(12312414)
  results <- jaspTools::runAnalysis("TTestOneSample", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "raincloud-vertical")
})

test_that("Raincloud plot matches (horizontal)", {
  options <- initTTestOptions("TTestOneSample")
  options$dependent <- "contGamma"
  options$raincloudPlot <- TRUE
  options$raincloudPlotHorizontal <- TRUE
  set.seed(12312414)
  results <- jaspTools::runAnalysis("TTestOneSample", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "raincloud-horizontal")
})

test_that("Raincloud plot matches (missing data)", {
  options <- initTTestOptions("TTestOneSample")
  options$dependent <- "debMiss30"
  options$raincloudPlot <- TRUE
  set.seed(12312414)
  results <- jaspTools::runAnalysis("TTestOneSample", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "raincloud-missing")
})

test_that("Analysis handles errors", {
  options <- initTTestOptions("TTestOneSample")

  options$dependent <- "debInf"
  results <- jaspTools::runAnalysis("TTestOneSample", "test.csv", options)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")

  options$dependent <- "debSame"
  results <- jaspTools::runAnalysis("TTestOneSample", "test.csv", options)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("variance", notes, ignore.case=TRUE)), label = "No variance check")

  options$dependent <- "debMiss99"
  results <- jaspTools::runAnalysis("TTestOneSample", "test.csv", options)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")
})
