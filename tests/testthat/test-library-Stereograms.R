context("Library: Stereograms")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("TTestBayesianIndependentSamples (analysis 2) results match", {

  # Run the R syntax of analysis 2 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Stereograms.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspTTests::TTestBayesianIndependentSamples(
          data = NULL,
          version = "0.96.5",
          formula =  ~ logFuseTime,
          alternative = "greater",
          bfRobustnessPlot = TRUE,
          descriptivesPlot = TRUE,
          group = ~ condition,
          priorAndPosteriorPlot = TRUE))

  plotName <- results[["results"]][["descriptivesContainer"]][["collection"]][["descriptivesContainer_plots"]][["collection"]][["descriptivesContainer_plots_logFuseTime"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_logfusetime")

  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_inferentialPlots"]][["collection"]][["ttestContainer_inferentialPlots_logFuseTime"]][["collection"]][["ttestContainer_inferentialPlots_logFuseTime_priorAndPosteriorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-2_prior-and-posterior")

  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_inferentialPlots"]][["collection"]][["ttestContainer_inferentialPlots_logFuseTime"]][["collection"]][["ttestContainer_inferentialPlots_logFuseTime_robustnessPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-3_bayes-factor-robustness-check")

  table <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_ttestTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(4.56741526796301, 0.000106268501395229, "logFuseTime"))

})

