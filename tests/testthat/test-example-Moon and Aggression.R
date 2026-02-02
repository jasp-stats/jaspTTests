context("Example: Moon and Aggression")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("TTestPairedSamples results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Moon and Aggression.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("TTestPairedSamples", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["AssumptionChecks"]][["collection"]][["AssumptionChecks_ttestNormalTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.912568631356825, 0.148263516031602, "-", "jaspColumn1", "jaspColumn2"
    ))

  table <- results[["results"]][["ttest"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("FALSE", 14, 2.43266666666667, 1.51815210097271e-05, 0.377053067714633,
     "-", 6.45178855435753, "jaspColumn1", "jaspColumn2"))

  plotName <- results[["results"]][["ttestDescriptives"]][["collection"]][["ttestDescriptives_plots"]][["collection"]][["ttestDescriptives_plots_jaspColumn1 - jaspColumn2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_jaspcolumn1-jaspcolumn2")

  table <- results[["results"]][["ttestDescriptives"]][["collection"]][["ttestDescriptives_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(15, 0.49595363275689, 3.022, 1.49877187819132, 0.386981234933267,
     "jaspColumn1", 15, 0.75491934246972, 0.589333333333333, 0.444899132495488,
     0.11487246205981, "jaspColumn2"))

})

