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
