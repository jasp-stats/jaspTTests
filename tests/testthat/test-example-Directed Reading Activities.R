context("Example: Directed Reading Activities")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("TTestIndependentSamples (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Directed Reading Activities.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("TTestIndependentSamples", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["AssumptionChecks"]][["collection"]][["AssumptionChecks_equalityVariance"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 42, 2.3418185975754, 0.133440294168933, "jaspColumn1"))

  table <- results[["results"]][["AssumptionChecks"]][["collection"]][["AssumptionChecks_ttestNormalTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.973383660123722, "jaspColumn1", 0.396069316406026))

  table <- results[["results"]][["ttest"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", -2.26655159958594, 6.05193730854526, -0.684098098673145,
     42, 0.318231061770764, -9.95445134575569, 0.0143147414161229,
     4.3918926653045, "Student", "jaspColumn1", "FALSE", -2.31088919785423,
     6.44337183523916, -0.690847570868059, 37.8554006594391, 0.318547879025324,
     -9.95445134575569, 0.0131912064122124, 4.30762814374609, "Welch",
     "jaspColumn1"))

  plotName <- results[["results"]][["ttestDescriptives"]][["collection"]][["ttestDescriptives_plots"]][["collection"]][["ttestDescriptives_plots_jaspColumn1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_jaspcolumn1")

  table <- results[["results"]][["ttestDescriptives"]][["collection"]][["ttestDescriptives_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 23, 0.413006140610563, "Control", 41.5217391304348, 17.1487332296995,
     3.57575806121187, "jaspColumn1", "FALSE", 21, 0.213833944302951,
     "Treat", 51.4761904761905, 11.0073568472138, 2.4020021882737,
     "jaspColumn1"))

})

