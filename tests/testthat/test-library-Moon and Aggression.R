context("Library: Moon and Aggression")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("TTestPairedSamples results match", {

  # Run the R syntax of analysis 1 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Moon and Aggression.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspTTests::TTestPairedSamples(
          data = NULL,
          version = "0.96.5",
          descriptives = TRUE,
          descriptivesPlot = TRUE,
          meanDifference = TRUE,
          normalityTest = TRUE,
          pairs = list("Moon", "Other")))

  table <- results[["results"]][["AssumptionChecks"]][["collection"]][["AssumptionChecks_ttestNormalTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.912568631356825, 0.148263516031602, "-", "Moon", "Other"))

  table <- results[["results"]][["ttest"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("FALSE", 14, 2.43266666666667, 1.51815210097271e-05, 0.377053067714633,
     "-", 6.45178855435753, "Moon", "Other"))

  plotName <- results[["results"]][["ttestDescriptives"]][["collection"]][["ttestDescriptives_plots"]][["collection"]][["ttestDescriptives_plots_Moon - Other"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_moon-other")

  table <- results[["results"]][["ttestDescriptives"]][["collection"]][["ttestDescriptives_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(15, 0.49595363275689, 3.022, 1.49877187819132, 0.386981234933267,
     "Moon", 15, 0.75491934246972, 0.589333333333333, 0.444899132495488,
     0.11487246205981, "Other"))

})

