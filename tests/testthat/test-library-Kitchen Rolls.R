context("Library: Kitchen Rolls")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("TTestBayesianIndependentSamples (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Kitchen Rolls.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("TTestBayesianIndependentSamples", dataset, opts)

  table <- results[["results"]][["descriptivesContainer"]][["collection"]][["descriptivesContainer_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(48, 0.77407560299628, "clock", 0.496632919778604, 0.640625, 0.495892183169492,
     0.0715758713604844, 0.784617080221396, "mean_NEO", 54, 0.663396245313462,
     "counter", 0.583865106085498, 0.712962962962963, 0.472976952677191,
     0.0643640107864235, 0.842060819840428, "mean_NEO"))

  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_inferentialPlots"]][["collection"]][["ttestContainer_inferentialPlots_mean_NEO"]][["collection"]][["ttestContainer_inferentialPlots_mean_NEO_priorAndPosteriorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_prior-and-posterior")

  table <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_ttestTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(7.74885083455646, 0.00163909954014538, "mean_NEO"))

})

test_that("TTestIndependentSamples (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Kitchen Rolls.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("TTestIndependentSamples", dataset, opts)

  table <- results[["results"]][["AssumptionChecks"]][["collection"]][["AssumptionChecks_equalityVariance"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 100, 0.271093100039441, 0.603750408751182, "mean_NEO"))

  table <- results[["results"]][["AssumptionChecks"]][["collection"]][["AssumptionChecks_ttestNormalTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.984216146448692, "mean_NEO", 0.266049143000539))

  table <- results[["results"]][["ttest"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("FALSE", 100, 0.773571517332578, -0.753605326123022, "mean_NEO"
    ))

  table <- results[["results"]][["ttestDescriptives"]][["collection"]][["ttestDescriptives_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 48, 0.77407560299628, "clock", 0.640625, 0.495892183169492,
     0.0715758713604844, "mean_NEO", "FALSE", 54, 0.663396245313462,
     "counter", 0.712962962962963, 0.472976952677191, 0.0643640107864235,
     "mean_NEO"))

})

