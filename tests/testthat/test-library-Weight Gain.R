context("Library: Weight Gain")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("TTestOneSample (analysis 2) results match", {

  # Run the R syntax of analysis 2 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Weight Gain.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspTTests::TTestOneSample(
          data = NULL,
          version = "0.96.5",
          formula =  ~ Difference,
          meanDifference = TRUE,
          meanDifferenceCi = TRUE,
          normalityTest = TRUE,
          plotHeight = 300,
          plotWidth = 160,
          testValue = 16))

  table <- results[["results"]][["AssumptionChecks"]][["collection"]][["AssumptionChecks_ttestNormalTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.938014342779832, 0.325399425988806, "Difference"))

  table <- results[["results"]][["ttest"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("FALSE", 15, -7.63778185966003, -5.59125, 3.35479468996568e-05,
     -5.82325030287623, -3.54471814033998, "Difference"))

})

