context("Independent Samples TTest")

# does not test
# - missing values exclusion
# - error handling of plots
 
# https://jasp-stats.github.io/jasp-verification-project/t-tests.html#independent-samples-t-test
test_that("Main table results match R, SAS and SPSS 1", { 
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
  resultTable <- results[["results"]][["ttest"]]
  
  jaspTools::expect_equal_tables(
    "test"=resultTable, 
    "ref"=list("FALSE", "TRUE", 2.02566265728829, 273, 0.00562492844383727, 0.2,
               0.0437720197601347, 0.0987331228526149, "Student", 0.394375071556162,
               "Score", "FALSE", 2.0716769761626, 271.761535410372, 0.00993836487745862,
               0.2, 0.0392389830584111, 0.0965401470891773, "Welch", 0.39006163512254,
               "Score", "ttest", "FALSE", "TRUE", "v", " ", "string", "test",
               "Test", "string", "sf:4;dp:3", "Statistic", "Statistic", "number",
               "sf:4;dp:3", "df", "df", "number", "dp:3;p:.001", "p", "p",
               "pvalue", "sf:4;dp:3", "md", "Mean Difference", "number", "sf:4;dp:3",
               "sed", "SE Difference", "number", "sf:4;dp:3", "lowerCIlocationParameter",
               "95% CI for Mean Difference", "Lower", "number", "sf:4;dp:3",
               "upperCIlocationParameter", "95% CI for Mean Difference", "Upper",
               "number", "complete", "Independent Samples T-Test"))
  
  resultTable <- results[["results"]][["ttestDescriptives"]]
  
  jaspTools::expect_equal_tables("test"=resultTable, 
                                 "ref"=list("FALSE", "TRUE", 125, 1, 1.8, 0.7, 0.0626099033699941, "Score",
                                            "FALSE", 150, 2, 1.6, 0.9, 0.0734846922834953, "Score", "ttestDescriptives_table",
                                            "FALSE", "TRUE", "variable", "", "string", "group", "Group",
                                            "string", "N", "N", "integer", "sf:4;dp:3", "mean", "Mean",
                                            "number", "sf:4;dp:3", "sd", "SD", "number", "sf:4;dp:3", "se",
                                            "SE", "number", "complete", "Group Descriptives", "FALSE", "ttestDescriptives",
                                            "Descriptives"))
})

# https://jasp-stats.github.io/jasp-verification-project/t-tests.html#mann-whitney-test
test_that("Mann-Whitney U table results match R, SAS and SPSS 2", { 
  options <- jaspTools::analysisOptions("TTestIndependentSamples")
  options$variables <- "Score"
  options$groupingVariable <- "Group"
  options$students <- FALSE
  options$welchs <- FALSE
  options$mannWhitneyU <- TRUE
  
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "MannWhitney.csv", options)
  resultTable <- results[["results"]][["ttest"]]
  
  jaspTools::expect_equal_tables(
    "test"=resultTable, 
    "ref"=list("FALSE", "FALSE", 5, "", 0.285714285714286, "Score", 0, "<em>Note.</em>",
               "Mann-Whitney U test.", "ttest", "FALSE", "TRUE", "v", " ",
               "string", "sf:4;dp:3", "W", "W", "number", "df", "df", "integer",
               "dp:3;p:.001", "p", "p", "pvalue", "complete", "Independent Samples T-Test")
  )
})


test_that("Main table results match", {
  options <- jaspTools::analysisOptions("TTestIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$welchs <- TRUE
  options$mannWhitneyU <- TRUE
  options$meanDifference <- TRUE
  options$effectSize <- TRUE
  options$VovkSellkeMPR <- TRUE
  options$effSizeConfidenceIntervalCheckbox <- TRUE
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "test.csv", options)
  table <- results[["results"]][["ttest"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 0.760172707980336, 1, 0.15401876311258, 98, -0.244064746209808,
	       0.163364220743842, 0.448976320466698, 0.214904085649005, "Student",
	       0.551319670653115, "contNormal", "FALSE", 0.773250564688269,
	       1, 0.155340050635411, 93.4114683704755, -0.242805192811962,
	       0.163364220743842, 0.441326472332004, 0.211269449004155, "Welch",
	       0.552657418835939, "contNormal", "FALSE", 1290, 1, 0.0591133004926108,
	       "", -0.169577908162339, 0.0932984248674163, 0.617539087467476,
	       "", "Mann-Whitney", 0.281763520076616, "contNormal")
  )
})

test_that("Normality table matches", {
  options <- jaspTools::analysisOptions("TTestIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$normalityTests <- TRUE
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "test.csv", options)
  table <- results[["results"]][["AssumptionChecks"]][["collection"]][["AssumptionChecks_ttestNormalTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("contNormal", 0, 0.933547444665698, 0.00342000811150064, "TRUE",
         "contNormal", 1, 0.972586424088514, 0.401705854633909, "FALSE")
  )
})

test_that("Equality of variances table matches", {
  options <- jaspTools::analysisOptions("TTestIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$equalityOfVariancesTests<- TRUE
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "test.csv", options)
  table <- results[["results"]][["AssumptionChecks"]][["collection"]][["AssumptionChecks_equalityVariance"]][["data"]]
  jaspTools::expect_equal_tables(table, list("contNormal", 0.474760708390762, 1, 0.492433247088434))
})

test_that("Descriptives table matches", {
  options <- jaspTools::analysisOptions("TTestIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$descriptives <- TRUE
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "test.csv", options)
  table <- results[["results"]][["ttestDescriptives"]][["collection"]][["ttestDescriptives_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 58, 0, -0.120135614827586, 1.10575982846952, 0.145193378675912,
	       "contNormal", "FALSE", 42, 1, -0.283499835571428, 0.994612407217046,
	       0.15347202634745, "contNormal")
  )
})

test_that("Descriptives plot matches", {
  options <- jaspTools::analysisOptions("TTestIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$descriptivesPlots <- TRUE
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "descriptives", dir="TTestIndependentSamples")
})

test_that("Analysis handles errors", {
  options <- jaspTools::analysisOptions("TTestIndependentSamples")
  
  options$variables <- "debInf"
  options$groupingVariable <- "contBinom"
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "test.csv", options)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")
  
  options$variables <- "debSame"
  options$groupingVariable <- "contBinom"
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "test.csv", options)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("variance", notes, ignore.case=TRUE)), label = "No variance check")
  
  options$variables <- "debMiss99"
  options$groupingVariable <- "contBinom"
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "test.csv", options)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")
  
  options$dependent <- "contNormal"
  options$groupingVariable <- "debSame"
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "test.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError", label = "1-level factor check")
})

