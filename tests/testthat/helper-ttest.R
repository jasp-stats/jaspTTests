initTTestOptions <- function(analysis = c("TTestIndependentSamples", "TTestPairedSamples", "TTestOneSample",
                                          "TTestBayesianIndependentSamples", "TTestBayesianPairedSamples", "TTestBayesianOneSample")) {
  options <- jaspTools::analysisOptions(analysis)
  options <- addCommonQMLoptions(options)

  return(options)
}

addCommonQMLoptions <- function(options) {
  # jaspTools doesn't recognize common QML elements so this function adds the defaults manually
  root <- if (nzchar(Sys.getenv("R_COVR"))) {
    # We are running inside covr (installed package structure)
    system.file("qml", "common", package = "jaspTTests")
  } else {
    testthat::test_path(file.path("..", "..", "inst", "qml", "common"))
  }
  if (!dir.exists(root)) {
    stop("Could not find common QML directory! Check the path construction in addCommonQMLoptions().")
  }
  c(
    options,
    jaspTools:::readQML(file.path(root, "BarPlots.qml"))
  )
}

# Data for the "Analysis handles errors" tests. jaspSyntax checks the options against the QML
# before the analysis runs, as JASP does, so a variable with fewer than 2 numeric values
# (debInf, debSame, debMiss99 in test.csv) never reaches the R checks anymore. These columns
# pass the QML checks and still trigger the R ones.
errorCheckData <- function() {
  data.frame(
    group       = rep(c("a", "b"), each = 10),
    withInf     = c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3, 2, 3, 8, Inf),
    perGroup    = rep(c(1, 2), each = 10),           # no variance within either group
    onePerGroup = c(1, rep(NA, 9), 2, rep(NA, 9)),   # one observation in each group
    x           = c(NA, 2:20),
    xPlus1      = c(NA, 2:20) + 1,                   # x - xPlus1 has no variance
    onePair     = c(5, 7, rep(NA, 18))               # only one complete pair with x
  )
}

expectRejectedByQml <- function(name, options, dataset = "test.csv", pattern = "Minimum number of") {
  testthat::expect_error(jaspTools::runAnalysis(name, dataset, options), pattern)
}
