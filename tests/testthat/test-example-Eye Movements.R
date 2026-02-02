context("Example: Eye Movements")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("TTestBayesianIndependentSamples results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Eye Movements.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("TTestBayesianIndependentSamples", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["descriptivesContainer"]][["collection"]][["descriptivesContainer_plots"]][["collection"]][["descriptivesContainer_plots_jaspColumn1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_jaspcolumn1")

  table <- results[["results"]][["descriptivesContainer"]][["collection"]][["descriptivesContainer_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(24, 0.41694367295095, "Fixation", 12.5994177875246, 15.2916666666667,
     6.37576366554161, 1.30144730842949, 17.9839155458087, "jaspColumn1",
     25, 0.397387754095367, "Horizontal", 9.09531440134882, 10.88,
     4.32357876455759, 0.864715752911518, 12.6646855986512, "jaspColumn1"
    ))

  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_inferentialPlots"]][["collection"]][["ttestContainer_inferentialPlots_jaspColumn1"]][["collection"]][["ttestContainer_inferentialPlots_jaspColumn1_priorAndPosteriorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_prior-and-posterior")

  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_inferentialPlots"]][["collection"]][["ttestContainer_inferentialPlots_jaspColumn1"]][["collection"]][["ttestContainer_inferentialPlots_jaspColumn1_robustnessPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_bayes-factor-robustness-check")

  table <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_ttestTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0853917198054242, 0.00162118018126082, "jaspColumn1"))

})

