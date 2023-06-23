context("Independent Samples TTest")

# does not test
# - missing values exclusion
# - error handling of plots

test_that("Main table results match", {
  options <- initTTestOptions("TTestIndependentSamples")
  options$dependent <- "contNormal"
  options$group <- "contBinom"
  options$welch <- TRUE
  options$mannWhitneyU  <- TRUE
  options$meanDifference <- TRUE
  options$effectSize <- TRUE
  options$vovkSellke <- TRUE
  options$effectSizeCi <- TRUE
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "test.csv", options)
  table <- results[["results"]][["ttest"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("TRUE", 0.760172707980336, 1, 0.15401876311258, 98, 0.203114256560352,
                                      -0.244064746209808, 0.163364220743842, 0.448976320466698, 0.214904085649005,
                                      "Student", 0.551319670653115, "contNormal", "FALSE", 0.773250564688269,
                                      1, 0.155340050635411, 93.4114683704755, 0.20312293061516, -0.242805192811962,
                                      0.163364220743842, 0.441326472332004, 0.211269449004155, "Welch",
                                      0.552657418835939, "contNormal", "FALSE", 1290, 1, 0.0591133004926108,
                                      "", 0.117021894944905, -0.169577908162339, 0.0932984248674163,
                                      0.617539087467476, "", "Mann-Whitney", 0.281763520076616, "contNormal"
                                 ))
})

test_that("Normality table matches", {
  options <- initTTestOptions("TTestIndependentSamples")
  options$dependent <- "contNormal"
  options$group <- "contBinom"
  options$normalityTest <- TRUE
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "test.csv", options)
  table <- results[["results"]][["AssumptionChecks"]][["collection"]][["AssumptionChecks_ttestNormalTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("contNormal", 0, 0.933547444665698, 0.00342000811150064, "TRUE",
         "contNormal", 1, 0.972586424088514, 0.401705854633909, "FALSE")
  )
})

test_that("Equality of variances table matches", {
  options <- initTTestOptions("TTestIndependentSamples")
  options$dependent <- "contNormal"
  options$group <- "contBinom"
  options$equalityOfVariancesTest<- TRUE
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "test.csv", options)
  table <- results[["results"]][["AssumptionChecks"]][["collection"]][["AssumptionChecks_equalityVariance"]][["data"]]
  jaspTools::expect_equal_tables(table, list(1, 98, 0.575606965634445, 0.449860273665838, "contNormal"))
})

test_that("Descriptives table matches", {
  options <- initTTestOptions("TTestIndependentSamples")
  options$dependent <- "contNormal"
  options$group <- "contBinom"
  options$descriptives <- TRUE
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "test.csv", options)
  table <- results[["results"]][["ttestDescriptives"]][["collection"]][["ttestDescriptives_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("TRUE", 58, -9.20426328242848, 0, -0.120135614827586, 1.10575982846952,
                                      0.145193378675912, "contNormal", "FALSE", 42, -3.50833504087324,
                                      1, -0.283499835571429, 0.994612407217046, 0.15347202634745,
                                      "contNormal"))
})

test_that("Descriptives plot matches", {
  options <- initTTestOptions("TTestIndependentSamples")
  options$dependent <- "contNormal"
  options$group <- "contBinom"
  options$descriptivesPlot <- TRUE
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "descriptives")
})

test_that("Bar plot matches", {
  options <- initTTestOptions("TTestIndependentSamples")
  options$dependent <- "contNormal"
  options$group <- "contBinom"
  options$barPlot <- TRUE
  options$barPlotErrorType <- "se"
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "barPlot")
})

test_that("Raincloud plot matches (vertical)", {
  options <- initTTestOptions("TTestIndependentSamples")
  options$dependent <- "contNormal"
  options$group <- "contBinom"
  options$raincloudPlot <- TRUE
  set.seed(12312414)
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "raincloud-vertical")
})

test_that("Raincloud plot matches (horizontal)", {
  options <- initTTestOptions("TTestIndependentSamples")
  options$dependent <- "contNormal"
  options$group <- "contBinom"
  options$raincloudPlot <- TRUE
  options$raincloudPlotHorizontal <- TRUE
  set.seed(12312414)
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "raincloud-horizontal")
})

test_that("Analysis handles errors", {
  options <- initTTestOptions("TTestIndependentSamples")

  options$dependent <- "debInf"
  options$group <- "contBinom"
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "test.csv", options)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")

  options$dependent <- "debSame"
  options$group <- "contBinom"
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "test.csv", options)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("variance", notes, ignore.case=TRUE)), label = "No variance check")

  options$dependent <- "debMiss99"
  options$group <- "contBinom"
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "test.csv", options)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")

  options$dependent <- "contNormal"
  options$group <- "debSame"
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "test.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError", label = "1-level factor check")
})


test_that("Analysis works with unicode", {

  options <- initTTestOptions("TTestIndependentSamples")
  options$descriptives <- TRUE
  options$descriptivesPlot <- TRUE
  options$raincloudPlot <- TRUE
  options$effectSize <- TRUE
  options$equalityOfVariancesTest <- TRUE
  options$group <- "Cloak"
  options$meanDifference <- TRUE
  options$normalityTest <- TRUE
  options$plotHeight <- 300
  options$plotWidth <- 350
  options$dependent <- "Mischief"
  options$welch <- TRUE
  set.seed(1)
  dataset <- structure(list(Participant = 1:24,
                            Cloak = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L),
                                              .Label = c("\U672A\U9009\U4E2D", "\U9009\U4E2D"), class = "factor"),
                            Mischief = c(3L, 1L, 5L, 4L, 6L, 4L, 6L, 2L, 0L, 5L, 4L, 5L, 4L, 3L, 6L, 6L, 8L, 5L, 5L, 4L, 2L, 5L, 7L, 5L)),
                       row.names = c(NA, -24L), class = "data.frame")

  results <- jaspTools::runAnalysis("TTestIndependentSamples", dataset, options)


  table <- results[["results"]][["AssumptionChecks"]][["collection"]][["AssumptionChecks_equalityVariance"]][["data"]]

  jaspTools::expect_equal_tables(table, list(1, 22, 0.269754768392371, 0.60868258906419, "Mischief"), label = "Test of Equality of Variances (Brown-Forsythe) table results match")


  # NOTE: these results are bogus and this test should be updated, once .hasErrors handles unicode properly so that the shapiro wilk test actually runs
  # table <- results[["results"]][["AssumptionChecks"]][["collection"]][["AssumptionChecks_ttestNormalTable"]][["data"]]
  # jaspTools::expect_equal_tables(
  #   table,
  #   list(1, "TRUE", "NaN", "Mischief", "<unicode><unicode><unicode><unicode><unicode><unicode><unicode><unicode><unicode>",
  #        1, "FALSE", "NaN", "Mischief", "<unicode><unicode><unicode><unicode><unicode><unicode>"
  #   ),
  #   label = "Test of Normality (Shapiro-Wilk) table results match"
  # )

  table <- results[["results"]][["ttest"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list("TRUE", -1.71345938396515, -0.699516864283035, 22, 0.432498740047123,
         -1.25, 0.100686344874811, 0.72951831347607, "Student", "Mischief",
         "FALSE", -1.71345938396515, -0.699516864283035, 21.5414052230847,
         0.432498740047123, -1.25, 0.100984669515183, 0.72951831347607,
         "Welch", "Mischief"),
    label = "Independent Samples T-Test table results match"
  )

  plotName <- results[["results"]][["ttestDescriptives"]][["collection"]][["ttestDescriptives_plots"]][["collection"]][["ttestDescriptives_plots_Mischief"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "mischief-Descriptives-plot")

  plotName <- results[["results"]][["ttestDescriptives"]][["collection"]][["ttestDescriptives_plotsRainCloud"]][["collection"]][["ttestDescriptives_plotsRainCloud_Mischief"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "mischief-raincloud-plot")

  table <- results[["results"]][["ttestDescriptives"]][["collection"]][["ttestDescriptives_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 12, 0.51010001000002, "<unicode><unicode><unicode>", 3.75,
         1.91287503750007, 0.552199458913392, "Mischief", "FALSE", 12,
         0.330289129537908, "<unicode><unicode>", 5, 1.65144564768954,
         0.476731294622796, "Mischief"),
    label = "Group Descriptives table results match"
  )

})
