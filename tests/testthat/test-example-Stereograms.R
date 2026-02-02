context("Example: Stereograms")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("TTestBayesianIndependentSamples (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Stereograms.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("TTestBayesianIndependentSamples", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["descriptivesContainer"]][["collection"]][["descriptivesContainer_plots"]][["collection"]][["descriptivesContainer_plots_jaspColumn1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_jaspcolumn1")

  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_inferentialPlots"]][["collection"]][["ttestContainer_inferentialPlots_jaspColumn1"]][["collection"]][["ttestContainer_inferentialPlots_jaspColumn1_priorAndPosteriorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-2_prior-and-posterior")

  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_inferentialPlots"]][["collection"]][["ttestContainer_inferentialPlots_jaspColumn1"]][["collection"]][["ttestContainer_inferentialPlots_jaspColumn1_robustnessPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-3_bayes-factor-robustness-check")

  table <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_ttestTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(4.56741526195086, 0.000106268502027034, "jaspColumn1"))

})

