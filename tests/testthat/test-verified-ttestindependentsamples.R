context("Independent Samples TTest -- Verification project")

# does not test
# - missing values exclusion
# - error handling of plots


options <- jaspTools::analysisOptions("TTestIndependentSamples")
options$variables <- "Score"
options$groupingVariable <- "Group"
options$students <- TRUE
options$welchs <- TRUE
options$mannWhitneyU <- FALSE
options$meanDifference <- TRUE
options$meanDiffConfidenceIntervalCheckbox <- TRUE
options$descriptives <- TRUE

results <- jaspTools::runAnalysis("TTestIndependentSamples", "IndependentSamplettest.csv", options)

# https://jasp-stats.github.io/jasp-verification-project/t-tests.html#independent-samples-t-test
test_that("Main table results match R, SPSS, SAS and Minitab 1", { 
  resultTable <- results$results$ttest$data
  
  jaspTools::expect_equal_tables(
    "test"=resultTable, 
    "ref"=list("TRUE", 2.02566265728829, 273, 0.00562492844383727, 0.2, 0.0437720197601347,
               0.0987331228526149, "Student", 0.394375071556162, "Score", "FALSE",
               2.0716769761626, 271.761535410372, 0.00993836487745862, 0.2,
               0.0392389830584111, 0.0965401470891773, "Welch", 0.39006163512254,
               "Score"))
})

# https://jasp-stats.github.io/jasp-verification-project/t-tests.html#independent-samples-t-test
test_that("Descriptives table results match R, SPSS, SAS and Minitab 1", { 
  # Descriptives
  resultTable <- results$results$ttestDescriptives$collection$ttestDescriptives_table$data
  
  jaspTools::expect_equal_tables(
    "test"=resultTable, 
    "ref"=list("TRUE", 125, 1, 1.8, 0.7, 0.0626099033699941, "Score", "FALSE",
               150, 2, 1.6, 0.9, 0.0734846922834953, "Score"))
})

# https://jasp-stats.github.io/jasp-verification-project/t-tests.html#mann-whitney-test
test_that("Mann-Whitney U table results match R, SPSS, SAS and MiniTab", { 
  options <- jaspTools::analysisOptions("TTestIndependentSamples")
  options$variables <- "Score"
  options$groupingVariable <- "Group"
  options$students <- FALSE
  options$welchs <- FALSE
  options$mannWhitneyU <- TRUE
  
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "MannWhitney.csv", options)
  
  # Main table
  resultTable <- results$results$ttest$data
  
  jaspTools::expect_equal_tables(
    "test"=resultTable, 
    "ref"=list("FALSE", 5, "", 0.285714285714286, "Score"))
})
