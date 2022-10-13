context("One Sample TTest -- Verification project")


# https://jasp-stats.github.io/jasp-verification-project/t-tests.html#one-sample-t-test
test_that("Main table results match R, SPSS, SAS and MiniTab", {
  options <- jaspTools::analysisOptions("TTestOneSample")

  options$testValue <- 3
  options$meanDifference <- TRUE
  options$effectSize <- TRUE
  options$meanDifferenceCi <- TRUE
  options$effectSizeCi <- TRUE
  options$dependent <- "V1"

  results <- jaspTools::runAnalysis("TTestOneSample", "OSTT.csv", options)

  resultTable <- results$results$ttest$data

  jaspTools::expect_equal_tables(
    "test"=resultTable,
    "ref"=list("FALSE", 2, 99, 0.173205080756888, 1.65788291072626, 1.80157830484136,
               2, 1.50644485946017e-36, 20, 2.33846083735979, 2.19842169515864,
               "V1")
  )
})
