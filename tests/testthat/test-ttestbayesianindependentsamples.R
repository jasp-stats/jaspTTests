context("Bayesian Independent Samples T-Test")

# does not test
# - bftype (01, 10)
# - missing value exclusion
# - informed prior Normal distributions
# - error handling of plots

getTtestTable <- function(x) x[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_ttestTable"]]
getDescriptivesTable <- function(x) x[["results"]][["descriptivesContainer"]][["collection"]][["descriptivesContainer_table"]]

test_that("Main table results match for Student test", {
  set.seed(0)
  options <- initTTestOptions("TTestBayesianIndependentSamples")
  options$dependent <- "contNormal"
  options$group <- "contBinom"
  options$alternative <- "greater"
  options$effectSizeStandardized <- "informative"
  options$informativeCauchyLocation <- 1
  options$informativeCauchyScale <- 0.5
  results <- jaspTools::runAnalysis("TTestBayesianIndependentSamples", "test.csv", options)
  table <- getTtestTable(results)[["data"]]
  jaspTools::expect_equal_tables(table, list(0.123678036469976, 0.021743789040271, "contNormal"))
})

# Independent positive-half-space references; likelihood and prior df differ.
test_that("Informed one-sided BF matches the independent Cauchy reference", {
  dataset <- data.frame(
    group = factor(rep(c("A", "B"), each = 30), levels = c("A", "B")),
    score = c(rep(c(-1, 1), 15), rep(c(1.363515791475006, 3.363515791475006), 15))
  )
  options <- initTTestOptions("TTestBayesianIndependentSamples")
  options$dependent <- "score"
  options$group <- "group"
  options$alternative <- "greater"
  options$effectSizeStandardized <- "informative"
  options$informativeStandardizedEffectSize <- "cauchy"
  options$informativeCauchyLocation <- 0.35
  options$informativeCauchyScale <- 0.15
  options$test <- "student"
  options$bayesFactorType <- "BF10"
  results <- jaspTools::runAnalysis("TTestBayesianIndependentSamples", dataset, options)
  bf <- getTtestTable(results)[["data"]][[1L]][["BF"]]
  expect_type(bf, "double")
  expect_length(bf, 1L)
  expect_true(length(bf) == 1L && is.numeric(bf) && all(is.finite(bf)))
  expect_equal(bf, 0.021172000158216957, tolerance = 1e-9)
})

test_that("Informed Student-t BF matches the independent large-sample reference", {
  bf <- jaspTTests::.bf10_t(t = -8, n1 = 500, n2 = 500, oneSided = "right",
    independentSamples = TRUE, prior.location = 0.35, prior.scale = 0.15, prior.df = 3)$bf
  expect_type(bf, "double")
  expect_length(bf, 1L)
  expect_true(length(bf) == 1L && is.numeric(bf) && all(is.finite(bf)))
  expect_equal(bf, 0.0027787145536857295, tolerance = 1e-8)
})

test_that("Informed paired BF matches its independent reference", {
  options <- initTTestOptions("TTestBayesianPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$alternative <- "greater"
  options$effectSizeStandardized <- "informative"
  options$informativeStandardizedEffectSize <- "t"
  options$informativeTLocation <- 0.2
  options$informativeTScale <- 0.5
  options$informativeTDf <- 2
  results <- jaspTools::runAnalysis("TTestBayesianPairedSamples", "test.csv", options)
  bf <- getTtestTable(results)[["data"]][[1L]][["BF"]]
  expect_type(bf, "double")
  expect_length(bf, 1L)
  expect_true(length(bf) == 1L && is.numeric(bf) && all(is.finite(bf)))
  expect_equal(bf, 0.013095851317331353, tolerance = 1e-8)
})

test_that("Informed one-sample BF matches its independent reference", {
  paired <- read.csv(system.file("extdata", "test.csv", package = "jaspTools"))
  dataset <- data.frame(difference = paired$contNormal - paired$contGamma)
  options <- initTTestOptions("TTestBayesianOneSample")
  options$dependent <- "difference"
  options$alternative <- "greater"
  options$effectSizeStandardized <- "informative"
  options$informativeStandardizedEffectSize <- "t"
  options$informativeTLocation <- 0.2
  options$informativeTScale <- 0.5
  options$informativeTDf <- 2
  options$bayesFactorType <- "BF10"
  results <- jaspTools::runAnalysis("TTestBayesianOneSample", dataset, options)
  bf <- getTtestTable(results)[["data"]][[1L]][["BF"]]
  expect_type(bf, "double")
  expect_length(bf, 1L)
  expect_true(length(bf) == 1L && is.numeric(bf) && all(is.finite(bf)))
  expect_equal(bf, 0.013095851317331353, tolerance = 1e-8)
})

test_that("Informed one-sided factors respect sign reflection", {
  bf <- c(
    jaspTTests::.bf10_t(-9, 30, 30, "right", TRUE, 0.35, 0.15, 1)$bf,
    jaspTTests::.bf10_t(9, 30, 30, "left", TRUE, -0.35, 0.15, 1)$bf,
    jaspTTests::.bf10_t(-8, 500, 500, "right", TRUE, 0.35, 0.15, 3)$bf,
    jaspTTests::.bf10_t(8, 500, 500, "left", TRUE, -0.35, 0.15, 3)$bf)
  expect_type(bf, "double")
  expect_length(bf, 4L)
  expect_true(length(bf) == 4L && all(is.finite(bf)))
  expect_equal(bf, c(rep(0.021172000158216957, 2), rep(0.0027787145536857295, 2)),
               tolerance = 1e-8)
})

test_that("Main table results match for Wilcoxon test", {
  set.seed(0)
  options <- initTTestOptions("TTestBayesianIndependentSamples")
  options$dependent <- "contNormal"
  options$group <- "contBinom"
  options$test <- "wilcoxon"
  options$wilcoxonSamples <- 100
  options$alternative <- "greater"
  results <- jaspTools::runAnalysis("TTestBayesianIndependentSamples", "test.csv", options)
  table <- getTtestTable(results)[["data"]]
  jaspTools::expect_equal_tables(table, list(0.354513046015919, 1290, 1.00753043165554, "contNormal"))
})

test_that("Inferential and descriptives plots match", {
  set.seed(0)
  options <- initTTestOptions("TTestBayesianIndependentSamples")
  options$dependent <- "contNormal"
  options$group <- "contBinom"
  options$priorAndPosteriorPlot <- TRUE
  options$priorAndPosteriorPlotAdditionalInfo <- FALSE

  options$bfRobustnessPlot <- TRUE
  options$bfRobustnessPlotAdditionalInfo <- FALSE

  options$bfSequentialPlot <- TRUE
  options$bfSequentialPlotRobustness <- FALSE

  options$descriptives <- TRUE
  options$descriptivesPlot <- TRUE
  options$descriptivesPlotCiLevel <- 0.90

  results <- jaspTools::runAnalysis("TTestBayesianIndependentSamples", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "descriptives")

  table <- getDescriptivesTable(results)[["data"]]
  jaspTools::expect_equal_tables(table,
     list(58, -9.20426328242848, 0, -0.362903138386961, -0.120135614827586,
          1.10575982846952, 0.145193378675912, 0.122631908731789, "contNormal",
          42, -3.50833504087324, 1, -0.541774532654284, -0.283499835571429,
          0.994612407217046, 0.15347202634745, -0.0252251384885728, "contNormal"
          ),
    label = "Descriptives table"
  )

  testPlot <- results[["state"]][["figures"]][[2]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-posterior")

  testPlot <- results[["state"]][["figures"]][[3]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "robustness-check")

  testPlot <- results[["state"]][["figures"]][[4]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "sequential-analysis")

})

test_that("Inferential plots with additional info match", {
  set.seed(0)
  options <- initTTestOptions("TTestBayesianIndependentSamples")
  options$dependent <- "contcor1"
  options$group <- "facGender"
  options$priorAndPosteriorPlot <- TRUE
  options$priorAndPosteriorPlotAdditionalInfo <- TRUE

  options$bfRobustnessPlot <- TRUE
  options$bfRobustnessPlotAdditionalInfo <- TRUE

  options$bfSequentialPlot <- TRUE
  options$bfSequentialPlotRobustness <- TRUE

  results <- jaspTools::runAnalysis("TTestBayesianIndependentSamples", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-posterior-additional")

  testPlot <- results[["state"]][["figures"]][[2]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "robustness-check-additional")

  testPlot <- results[["state"]][["figures"]][[3]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "sequential-analysis-additional")


})

test_that("Bar plot matches", {
  options <- initTTestOptions("TTestBayesianIndependentSamples")
  options$dependent <- "contNormal"
  options$group <- "contBinom"
  options$barPlot <- TRUE
  options$barPlotErrorType <- "se"
  results <- jaspTools::runAnalysis("TTestBayesianIndependentSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "barPlot")
})

test_that("Raincloud plot matches (vertical)", {
  options <- initTTestOptions("TTestBayesianIndependentSamples")
  options$dependent <- "contNormal"
  options$group <- "contBinom"
  options$raincloudPlot <- TRUE
  set.seed(12312414)
  results <- jaspTools::runAnalysis("TTestBayesianIndependentSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "raincloud-vertical")
})

test_that("Raincloud plot matches (horizontal)", {
  options <- initTTestOptions("TTestBayesianIndependentSamples")
  options$dependent <- "contNormal"
  options$group <- "contBinom"
  options$raincloudPlot <- TRUE
  options$raincloudPlotHorizontal <- TRUE
  set.seed(12312414)
  results <- jaspTools::runAnalysis("TTestBayesianIndependentSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "raincloud-horizontal")
})

test_that("Analysis handles errors", {
  options <- initTTestOptions("TTestBayesianIndependentSamples")

  options$dependent <- c("debInf", "debSame")
  options$group <- "contBinom"
  results <- jaspTools::runAnalysis("TTestBayesianIndependentSamples", "test.csv", options)
  notes <- unlist(getTtestTable(results)[["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")
  expect_true(any(grepl("variance", notes, ignore.case=TRUE)), label = "variance check")
  expect_null(results[["results"]][["errorMessage"]])

  options$dependent <- "debMiss99"
  options$group <- "contBinom"
  results <- jaspTools::runAnalysis("TTestBayesianIndependentSamples", "test.csv", options)
  notes <- unlist(getTtestTable(results)[["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")

  options$dependent <- "contNormal"
  options$group <- "debSame"
  results <- jaspTools::runAnalysis("TTestBayesianIndependentSamples", "test.csv", options)
  msg <- results[["results"]][["errorMessage"]]
  expect_true(any(grepl("levels", msg, ignore.case=TRUE)), label = "1-level factor check")
})

test_that("Analysis handles integer overflow", {

  # started failing inexplicably in https://github.com/jasp-stats/jaspTTests/pull/258
  testthat::skip_on_os("mac")

  set.seed(4491)
  dat <- data.frame(dependent_var = rnorm(2e5),
                    grouping      = factor(rep(c(1, 2), each = 1e5)))

  options <- initTTestOptions("TTestBayesianIndependentSamples")
  options$dependent <- "dependent_var"
  options$group <- "grouping"
  results <- jaspTools::runAnalysis("TTestBayesianIndependentSamples", dat, options)

  table <- getTtestTable(results)[["data"]]
  # ???: the error statistic differs between osx <-> windows. if anyone can figure out why i'd be interested (especially because the BF is the same)
  # vandenman: The difference appears in integrate, for example:
  # BayesFactor:::meta.bf.interval(-Inf, Inf, t = 0.158315759266202, N = 50000, df = 199998, rscale = 0.707)
  if (identical(.Platform$OS.type, "windows"))
     jaspTools::expect_equal_tables(table, list(0.00511047754408505, 4.61848285988607, "dependent_var"))
  else
     jaspTools::expect_equal_tables(table, list(0.00511717623883834, 0.185588508623007, "dependent_var"))
})

# all combinations of hypotheses and Bayes factor type
options <- initTTestOptions("TTestBayesianIndependentSamples")
options$group <- "Rotation"
options$bfRobustnessPlot <- TRUE
options$priorAndPosteriorPlot <- TRUE
options$bfSequentialPlot <- TRUE
options$dependent <- list("mean_NEO")

hypotheses <- c("twoSided", "greater", "less")
bftypes <- c("BF10", "BF01", "LogBF10")

# uncomment this function to generate tables
# makeTables <- function(tables) {
#   for (r in rownames(tables)) for (c in colnames(tables))
#     cat(sprintf("tables[[\"%s\", \"%s\"]] <- %s\n", r, c, tables[[r, c]]))
# }

tables <- matrix(list(), 3, 3, dimnames = list(bftypes, hypotheses))
tables[["BF10",    "twoSided" ]] <- list(0.26954423651714, 0.0237083065632031, "mean_NEO")
tables[["BF10",    "greater"]] <- list(0.129051393730452, 0.00163909809942946, "mean_NEO")
tables[["BF10",    "less"]] <- list(0.410037079311444, 1.19851400702814e-06, "mean_NEO")
tables[["BF01",    "twoSided" ]] <- list(3.70996617446284, 0.0237083065632031, "mean_NEO")
tables[["BF01",    "greater"]] <- list(7.74885083448756, 0.00163909809942946, "mean_NEO")
tables[["BF01",    "less"]] <- list(2.43880383130046, 1.19851400702814e-06, "mean_NEO")
tables[["LogBF10", "twoSided" ]] <- list(-1.31102275918232, 0.0237083065632031, "mean_NEO")
tables[["LogBF10", "greater"]] <- list(-2.04754455294982, 0.00163909809942946, "mean_NEO")
tables[["LogBF10", "less"]] <- list(-0.891507686028008, 1.19851400702814e-06, "mean_NEO")

set.seed(1)
for (hypo in hypotheses) {

  options$alternative <- hypo

  for (bftype in bftypes) {

    options$bayesFactorType <- bftype

    results <- jaspTools::runAnalysis("TTestBayesianIndependentSamples", "Kitchen Rolls", options)
    # run once with this uncommented to generate tables, afterwards call makeTables(tables)
    # tables[[bftype, hypo]] <- makeTestTable(getTtestTable(results)[["data"]], print = FALSE)

    test_that(sprintf("%s-%s Prior and Posterior plot matches", hypo, bftype), {
      plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_inferentialPlots"]][["collection"]][["ttestContainer_inferentialPlots_mean_NEO"]][["collection"]][["ttestContainer_inferentialPlots_mean_NEO_priorAndPosteriorPlot"]][["data"]]
      testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
      jaspTools::expect_equal_plots(testPlot, sprintf("%s-%s-prior-and-posterior", hypo, bftype))
    })

    test_that(sprintf("%s-%s Bayes Factor Robustness Check plot matches", hypo, bftype), {
      plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_inferentialPlots"]][["collection"]][["ttestContainer_inferentialPlots_mean_NEO"]][["collection"]][["ttestContainer_inferentialPlots_mean_NEO_robustnessPlot"]][["data"]]
      testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
      jaspTools::expect_equal_plots(testPlot, sprintf("%s-%s-bayes-factor-robustness-check", hypo, bftype))
    })

    test_that(sprintf("%s-%s Sequential Analysis plot matches", hypo, bftype), {
      plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_inferentialPlots"]][["collection"]][["ttestContainer_inferentialPlots_mean_NEO"]][["collection"]][["ttestContainer_inferentialPlots_mean_NEO_sequentialPlot"]][["data"]]
      testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
      jaspTools::expect_equal_plots(testPlot, sprintf("%s-%s-sequential-analysis", hypo, bftype))
    })

    test_that(sprintf("%s-%s Bayesian Independent Samples T-Test table results match", hypo, bftype), {
      table <- getTtestTable(results)[["data"]]
      jaspTools::expect_equal_tables(table, tables[[bftype, hypo]])
    })
  }
}
