context("Library: Kitchen Rolls")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("TTestBayesianIndependentSamples (analysis 1) results match", {

  # Run the R syntax of analysis 1 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Kitchen Rolls.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspTTests::TTestBayesianIndependentSamples(
          data = NULL,
          version = "0.96.5",
          formula =  ~ mean_NEO,
          alternative = "greater",
          bayesFactorType = "BF01",
          bfRobustnessPlotAdditionalInfo = FALSE,
          descriptives = TRUE,
          group = ~ Rotation,
          priorAndPosteriorPlot = TRUE))

  table <- results[["results"]][["descriptivesContainer"]][["collection"]][["descriptivesContainer_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(48, 0.774075602981182, "clock", 0.496632919782381, 0.64062500000125,
     0.495892183160787, 0.071575871359228, 0.784617080220119, "mean_NEO",
     54, 0.663396245316647, "counter", 0.583865106090338, 0.71296296296963,
     0.472976952683884, 0.0643640107873343, 0.842060819848921, "mean_NEO"
    ))

  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_inferentialPlots"]][["collection"]][["ttestContainer_inferentialPlots_mean_NEO"]][["collection"]][["ttestContainer_inferentialPlots_mean_NEO_priorAndPosteriorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_prior-and-posterior")

  table <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_ttestTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(7.74885083478572, 0.00163909888468653, "mean_NEO"))

})

test_that("TTestIndependentSamples (analysis 2) results match", {

  # Run the R syntax of analysis 2 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Kitchen Rolls.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspTTests::TTestIndependentSamples(
          data = NULL,
          version = "0.96.5",
          formula =  ~ mean_NEO,
          alternative = "greater",
          descriptives = TRUE,
          equalityOfVariancesTest = TRUE,
          group = ~ Rotation,
          normalityTest = TRUE,
          student = TRUE,
          welch = FALSE))

  table <- results[["results"]][["AssumptionChecks"]][["collection"]][["AssumptionChecks_equalityVariance"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 100, 0.271093100265109, 0.603750408600743, "mean_NEO"))

  table <- results[["results"]][["AssumptionChecks"]][["collection"]][["AssumptionChecks_ttestNormalTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.984216146460972, "mean_NEO", 0.266049143567561))

  table <- results[["results"]][["ttest"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("FALSE", 100, 0.773571517349786, -0.753605326180581, "mean_NEO"
    ))

  table <- results[["results"]][["ttestDescriptives"]][["collection"]][["ttestDescriptives_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 48, 0.774075602981182, "clock", 0.64062500000125, 0.495892183160787,
     0.071575871359228, "mean_NEO", "FALSE", 54, 0.663396245316647,
     "counter", 0.71296296296963, 0.472976952683884, 0.0643640107873343,
     "mean_NEO"))

})

