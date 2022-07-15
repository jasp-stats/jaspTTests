initTTestOptions <- function(analysis = c("TTestIndependentSamples", "TTestPairedSamples", "TTestOneSample",
                                          "TTestBayesianIndependentSamples", "TTestBayesianPairedSamples", "TTestBayesianOneSample")) {
  options <- jaspTools::analysisOptions(analysis)
  options <- addCommonQMLoptions(options)

  return(options)
}

addCommonQMLoptions <- function(options) {
  # jaspTools doesn't recognize common QML elements so this function adds the defaults manually
  root <- testthat::test_path(file.path("..", "..", "inst", "qml", "common"))
  c(
    options,
    jaspTools:::readQML(file.path(root, "BarPlots.qml"))
  )
}
