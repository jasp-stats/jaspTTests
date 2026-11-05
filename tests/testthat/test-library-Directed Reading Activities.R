context("Library: Directed Reading Activities")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("TTestIndependentSamples (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Directed Reading Activities.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("TTestIndependentSamples", dataset, opts)

  table <- results[["results"]][["AssumptionChecks"]][["collection"]][["AssumptionChecks_equalityVariance"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 42, 2.3418185975754, 0.133440294168933, "drp"))

  table <- results[["results"]][["AssumptionChecks"]][["collection"]][["AssumptionChecks_ttestNormalTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.973383660123722, "drp", 0.396069316406026))

  table <- results[["results"]][["ttest"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", -2.26655159958594, 6.05193730854525, -0.684098098673145,
     42, 0.318231061770764, -9.95445134575569, 0.0143147414161229,
     4.3918926653045, "Student", "drp", "FALSE", -2.31088919785423,
     6.44337183523916, -0.690847570868059, 37.8554006594391, 0.318547879025324,
     -9.95445134575569, 0.0131912064122124, 4.30762814374609, "Welch",
     "drp"))

  plotName <- results[["results"]][["ttestDescriptives"]][["collection"]][["ttestDescriptives_plots"]][["collection"]][["ttestDescriptives_plots_drp"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_drp")

  table <- results[["results"]][["ttestDescriptives"]][["collection"]][["ttestDescriptives_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 23, 0.413006140610564, "Control", 41.5217391304348, 17.1487332296995,
     3.57575806121188, "drp", "FALSE", 21, 0.213833944302951, "Treat",
     51.4761904761905, 11.0073568472138, 2.4020021882737, "drp"
    ))

})

