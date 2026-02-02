context("Example: Invisibility Cloak")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("TTestIndependentSamples results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Invisibility Cloak.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("TTestIndependentSamples", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["AssumptionChecks"]][["collection"]][["AssumptionChecks_equalityVariance"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 22, 0.269754768392371, 0.60868258906419, "jaspColumn1"))

  table <- results[["results"]][["AssumptionChecks"]][["collection"]][["AssumptionChecks_ttestNormalTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.964973452643166, "jaspColumn1", 0.546075577329177))

  table <- results[["results"]][["ttest"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", -1.71345938396515, -0.699516864283035, 22, 0.432498740047123,
     -1.25, 0.100686344874811, 0.72951831347607, "Student", "jaspColumn1",
     "FALSE", -1.71345938396515, -0.699516864283035, 21.5414052230847,
     0.432498740047123, -1.25, 0.100984669515183, 0.72951831347607,
     "Welch", "jaspColumn1"))

  plotName <- results[["results"]][["ttestDescriptives"]][["collection"]][["ttestDescriptives_plots"]][["collection"]][["ttestDescriptives_plots_jaspColumn1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_jaspcolumn1")

  table <- results[["results"]][["ttestDescriptives"]][["collection"]][["ttestDescriptives_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 12, 0.51010001000002, 0, 3.75, 1.91287503750007, 0.552199458913392,
     "jaspColumn1", "FALSE", 12, 0.330289129537908, 1, 5, 1.65144564768954,
     0.476731294622796, "jaspColumn1"))

})

