test_that("descriptive and bar plots store summaries in serializable recipes", {
  withr::local_options(lifecycle_verbosity = "quiet")
  dataset <- data.frame(
    value = c(2, 4, 3, 7, 5, 8),
    other = c(3, 6, 4, 8, 5, 10),
    group = factor(rep(c("A", "B"), each = 3))
  )
  options <- list(descriptivesPlotCiLevel = .95, testValue = 1,
                  barPlotCiLevel = .95, barPlotErrorType = "ci",
                  barPlotYAxisFixedToZero = TRUE)
  independentOptions <- options
  independentOptions$group <- "group"
  independentOptions$testValue <- NULL
  pairedOptions <- options
  pairedOptions$testValue <- NULL

  recipes <- list(
    jaspTTests:::.ttestOneSampleDescriptivesPlotFill(dataset, options, "value"),
    jaspTTests:::.ttestIndependentDescriptivesPlotFill(dataset, independentOptions, "value"),
    jaspTTests:::.ttestPairedDescriptivesPlotFill(dataset, pairedOptions, c("value", "other")),
    jaspTTests:::.ttestDescriptivesBarPlotFill(dataset, options, "value"),
    jaspTTests:::.ttestDescriptivesBarPlotFill(dataset, independentOptions, "value"),
    jaspTTests:::.ttestDescriptivesBarPlotFill(dataset, pairedOptions, c("value", "other"))
  )
  for (paired in c(FALSE, TRUE)) {
    grouping <- if (paired) "group" else NULL
    data <- if (paired) dataset[c("value", "group")] else dataset$value
    for (builder in list(jaspTTests:::.ttestBayesianPlotKGroupMeans,
                         jaspTTests:::.ttestBayesianBarPlotKGroupMeans)) {
      recipes[[length(recipes) + 1L]] <- builder(
        data = data, var = "value", grouping = grouping,
        groupNames = c("A", "B"), paired = paired,
        testValueOpt = if (paired) NULL else 1
      )
    }
  }

  for (recipe in recipes) {
    expect_true(jaspGraphs::isJaspPlotRecipe(recipe))
    restored <- unserialize(serialize(recipe, NULL))
    expect_identical(restored, recipe)
    expect_true(ggplot2::is_ggplot(jaspGraphs::materializeJaspPlotRecipe(restored)))
  }
})

test_that("raincloud recipes preserve jitter and the RNG state on redraw", {
  withr::local_options(lifecycle_verbosity = "quiet")
  dataset <- data.frame(value = c(2, 4, 3, 7, 5, 8),
                        group = factor(rep(c("A", "B"), each = 3)))
  for (horiz in c(FALSE, TRUE)) {
    recipe <- jaspTTests:::.descriptivesPlotsRainCloudFill(
      dataset, "value", "group", "Value", "Group",
      addLines = !horiz, horiz = horiz, testValue = NULL
    )
    expect_true(jaspGraphs::isJaspPlotRecipe(recipe))
    restored <- unserialize(serialize(recipe, NULL))
    rng <- .Random.seed
    first <- ggplot2::ggplot_build(jaspGraphs::materializeJaspPlotRecipe(restored))$data
    second <- ggplot2::ggplot_build(jaspGraphs::materializeJaspPlotRecipe(restored))$data
    expect_identical(first, second)
    expect_identical(.Random.seed, rng)
  }
})

test_that("plot edits remain in the recipe after serialization", {
  recipe <- jaspGraphs::createJaspPlotRecipe(
    "jaspTTests:::.ttestDescriptivesPlot",
    list(x = factor(c("A", "B")), y = c(3, 5),
         ciLower = c(2, 4), ciUpper = c(4, 6), noXLevelNames = FALSE)
  )
  edits <- jaspGraphs::plotEditingOptions(recipe)
  edits$xAxis$settings$title <- "Edited group"
  edits$xAxis$settings$titleType <- "character"
  edited <- jaspGraphs::plotEditing(recipe, edits)
  restored <- unserialize(serialize(edited, NULL))
  expect_true(jaspGraphs::isJaspPlotRecipe(restored))
  expect_identical(restored$editOptions$xAxis$settings$title, "Edited group")
  plot <- jaspGraphs::materializeJaspPlotRecipe(restored)
  expect_identical(jaspGraphs::plotEditingOptions(plot)$xAxis$settings$title, "Edited group")
})

test_that("prior/posterior recipes preserve the effect size expression", {
  withr::local_options(lifecycle_verbosity = "quiet")
  recipe <- jaspGraphs::createJaspPlotRecipe(
    "jaspTTests:::.ttestPriorAndPosteriorPlot",
    list(dfLines = data.frame(x = c(-1, 0, 1), y = c(.1, .5, .1)),
         bfType = "BF10", hypothesis = "equal", effectSizeLabel = "Effect size")
  )
  restored <- unserialize(serialize(recipe, NULL))
  plot <- jaspGraphs::materializeJaspPlotRecipe(restored)
  expect_identical(plot$scales$get_scales("x")$name, quote(paste("Effect size", ~delta)))
})
