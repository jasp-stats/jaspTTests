context("Paired Samples TTest -- Verification project")

# does not test
# - missing values exclusion
# - error handling of plots


# https://jasp-stats.github.io/jasp-verification-project/t-tests.html#paired-samples-t-test
test_that("Main table results match R, SPSS, SAS and MiniTab", {
  options <- jaspTools::analysisOptions("TTestPairedSamples")
  options$pairs <- list(c("Husband", "Wife"))
  options$meanDifference <- TRUE
  options$meanDifferenceCi <- TRUE
  results <- jaspTools::runAnalysis("TTestPairedSamples", "Pairedsamplettest.csv", options)

  resultTable <- results$results$ttest$data

  jaspTools::expect_equal_tables(
    "test"=resultTable,
    "ref"=list("FALSE", 7, -1.10992653616265, 3.875, 0.108643283879839, 2.10812628382918,
               "-", 1.83812517766321, 8.85992653616265, "Husband", "Wife"))
})


# https://jasp-stats.github.io/jasp-verification-project/t-tests.html#wilcoxon-test
test_that("Wilcoxon results match R, SPSS, SAS and MiniTab", {
  options <- jaspTools::analysisOptions("TTestPairedSamples")
  options$pairs <- list(c("Control", "Treatment."))
  options$student <- FALSE
  options$wilcoxon <- TRUE

  results <- jaspTools::runAnalysis("TTestPairedSamples", "Wilcoxon.csv", options)

  # Main Table
  resultTable <- results$results$ttest$data

  jaspTools::expect_equal_tables(
    "test"=resultTable,
    "ref"=list("FALSE", 15, "", 0.220971735391012, "-", "Control", "Treatment.",
               -1.27411797859406))
})
