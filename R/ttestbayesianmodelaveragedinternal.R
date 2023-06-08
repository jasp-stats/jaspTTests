#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#

TTestBayesianModelAveragedInternal <- function(jaspResults, dataset, options) {

  # load & check data (re-using .ttestBayesian functions)
  if (.robttCheckReady(options)) {
    options[["naAction"]] <- "listwise"
    dataset <- .ttestBayesianReadData(dataset, options)
    errors  <- .ttestBayesianGetErrorsPerVariable(dataset, options, analysis = "independent")
  }

  # get the priors
  .robttGetPriors(jaspResults, options)

  # show the model preview
  if (is.null(jaspResults[["model"]]))
   .robttModelPreviewTable(jaspResults, options)

  # priors plot
  if (options[["priorDistributionPlot"]])
    .robttPriorsPlots(jaspResults, options)

  # fit model model
  if (.robttCheckReady(options))
    .robttFitModel(jaspResults, dataset, options)

  ### Inference
  # default summary
  if (.robttCheckReady(options))
    .robttSummaryTable(jaspResults, options)
  # models overview
  if (options[["inferenceModelsOverview"]])
    .robttModelsOvervievTable(jaspResults, options)
  # models summary
  if (options[["inferenceIndividualModels"]])
    .robttModelsSummaryTable(jaspResults, options)

  ### Plots
  # pooled estimates plots
  if (options[["plotsPooledEstimatesEffect"]])
    .robttEstimatesPlot(jaspResults, options, "delta")
  if (options[["plotsPooledEstimatesHeterogeneity"]])
    .robttEstimatesPlot(jaspResults, options, "rho")
  if (options[["plotsPooledEstimatesOutliers"]])
    .robttEstimatesPlot(jaspResults, options, "nu")

  ### Diagnostics
  # overview
  if (options[["mcmcDiagnosticsOverviewTable"]])
    .robttDiagnosticsOverviewTable(jaspResults, options)
  # plots
  if ((
    options[["mcmcDiagnosticsPlotEffect"]]    ||
    options[["mcmcDiagnosticsPlotHeterogeneity"]]   ||
    options[["mcmcDiagnosticsPlotOutliers"]]
  ) ||
  (
    options[["mcmcDiagnosticsPlotTypeTrace"]]           ||
    options[["mcmcDiagnosticsPlotTypeAutocorrelation"]] ||
    options[["mcmcDiagnosticsPlotTypePosteriorSamplesDensity"]]
  ))
  .robttDiagnosticsPlots(jaspResults, options)


  return()
}

.robttDependencies <- c(
  "dependent", "group",
  "modelsEffect", "modelsEffectNull", "modelsHeterogeneity", "modelsHeterogeneityNull", "modelsOutliers", "modelsOutliersNull",
  "advancedMcmcAdaptation", "advancedMcmcSamples", "advancedMcmcChains",
  "seed", "setSeed"
)
# priors related functions
.robttExtractPriorsFromOptions <- function(optionsPrior) {

  if (optionsPrior[["type"]] == "None")
    return(NULL)

  optionsPrior <- .robttEvalOptionsToPriors(optionsPrior)
  optionsPrior <- .robttMapOptionsToPriors(optionsPrior)

  return(do.call(
    what = RoBTT::prior,
    args = optionsPrior
  ))
}
.robttEvalOptionsToPriors      <- function(x) {

  evalNames <-
    c(
      "a",
      "b",
      "alpha",
      "beta",
      "nu",
      "x0",
      "mu",
      "sigma",
      "theta",
      "lambda",
      "k",
      "priorWeight",
      "truncationLower",
      "truncationUpper"
    )
  for (n in evalNames) {
    if (!is.null(x[[n]]))
      x[[n]] <- eval(parse(text = x[[n]]))
  }

  return(x)
}
.robttMapOptionsToPriors       <- function(optionsPrior) {

  arguments <- list()

  arguments[["distribution"]] <- switch(
    optionsPrior[["type"]],
    "gammaAB" = "gamma",
    "gammaK0" = "gamma",
    optionsPrior[["type"]]
  )

  arguments[["parameters"]] <- switch(
    optionsPrior[["type"]],
    "normal"      = list("mean" = optionsPrior[["mu"]], "sd" = optionsPrior[["sigma"]]),
    "t"           = list("location" = optionsPrior[["mu"]], "scale" = optionsPrior[["sigma"]], "df" = optionsPrior[["nu"]]),
    "cauchy"      = list("location" = optionsPrior[["mu"]], "scale" = optionsPrior[["theta"]]),
    "gammaAB"     = list("shape" = optionsPrior[["alpha"]], "rate" = optionsPrior[["beta"]]),
    "gammaK0"     = list("shape" = optionsPrior[["k"]], "rate" = 1/optionsPrior[["theta"]]),
    "invgamma"    = list("shape" = optionsPrior[["alpha"]], "scale" = optionsPrior[["beta"]]),
    "lognormal"   = list("meanlog" = optionsPrior[["mu"]], "sdlog" = optionsPrior[["sigma"]]),
    "beta"        = list("alpha" = optionsPrior[["alpha"]], "beta" = optionsPrior[["beta"]]),
    "uniform"     = list("a" = optionsPrior[["a"]], "b" = optionsPrior[["b"]]),
    "exponential" = list("rate" = optionsPrior[["lambda"]]),
    "spike"       = list("location" = optionsPrior[["x0"]])
  )

  if(!arguments[["distribution"]] %in% c("spike", "uniform")) {
    arguments[["truncation"]] <- list(
      lower   = optionsPrior[["truncationLower"]],
      upper   = optionsPrior[["truncationUpper"]]
    )
  }

  arguments[["prior_weights"]] = optionsPrior[["priorWeight"]]

  return(arguments)
}
# helper functions
.robttCheckReady          <- function(options) {

  return(options[["dependent"]] != "" && options[["group"]] != "" )
}
.robttGetPriors           <- function(jaspResults, options) {

  if (!is.null(jaspResults[["priors"]])) {
    return()
  } else {
    priors <- createJaspState()
    priors$dependOn(.robttDependencies)
    jaspResults[["priors"]] <- priors
  }

  object <- list()
  for(type in c("", "Null")) {

    priorElements <- paste0(c("modelsEffect", "modelsHeterogeneity", "modelsOutliers"), type)

    for (i in seq_along(priorElements)) {

      if(length(options[[priorElements[i]]]) == 0){
        # remove component if prior is unspecified
        tmpPrior <- NULL
      }else if(options[[priorElements[i]]][[1]][["type"]] == "none"){
        # use default null hypothesis prior if prior is none
        tmpPrior <- RoBTT::prior_none()
      }else{
        tmpPrior <- try(.robttExtractPriorsFromOptions(options[[priorElements[i]]][[1]]))
        if (jaspBase::isTryError(tmpPrior))
          .quitAnalysis(tmpPrior)
      }

      object[[priorElements[i]]] <- tmpPrior
    }
  }

  saveRDS(object, file = "C:/JASP/priors.RDS")
  priors[["object"]] <- object

  return()
}

# table filling functions
.robttTableFillCoef           <- function(jaspTable, resultsTable, options, individual = FALSE) {

  overtitleCi <- gettextf("%s%% CI", 100 * options[["inferenceCiWidth"]])
  # add columns
  jaspTable$addColumnInfo(name = "terms",  title = "",                type = "string")
  jaspTable$addColumnInfo(name = "mean",   title = gettext("Mean"),   type = "number")
  jaspTable$addColumnInfo(name = "median", title = gettext("Median"), type = "number")
  jaspTable$addColumnInfo(name = "lowerCI",title = gettext("Lower"),  type = "number", overtitle = overtitleCi)
  jaspTable$addColumnInfo(name = "upperCI",title = gettext("Upper"),  type = "number", overtitle = overtitleCi)

  if (individual) {
    jaspTable$addColumnInfo(name = "ess",         title = gettext("ESS"),           type = "integer")
    jaspTable$addColumnInfo(name = "rHat",        title = gettext("R-hat"),         type = "number")
  }


  if (is.null(resultsTable))
    return(jaspTable)

  # deal with summary/individual tables
  resultsTable <- .robttAllignSummaryTableNames(resultsTable)


  # fill rows
  for (i in c(1:nrow(resultsTable))[rownames(resultsTable) %in% c("delta", "rho", "nu")]) {

    if (rownames(resultsTable)[i] == "rho" && options[["inferenceHeterogeneityAsStandardDeviationRatio"]]) {
      tempRow <- list(
        terms    = "Standard deviation ratio",
        mean     = if(individual) NA else attr(resultsTable, "mean_sdr"),
        median   = RoBTT::rho2logsdr$fun(resultsTable[i, "Median"]),
        lowerCI  = RoBTT::rho2logsdr$fun(resultsTable[i, if (individual) "lCI" else as.character(.5 - options[["inferenceCiWidth"]] / 2)]),
        upperCI  = RoBTT::rho2logsdr$fun(resultsTable[i, if (individual) "uCI" else as.character(.5 + options[["inferenceCiWidth"]] / 2)])
      )
    } else {
      tempRow <- list(
        terms    = .robttCoefNames(rownames(resultsTable)[i], options),
        mean     = resultsTable[i, "Mean"],
        median   = resultsTable[i, "Median"],
        lowerCI  = resultsTable[i, if (individual) "lCI" else as.character(.5 - options[["inferenceCiWidth"]] / 2)],
        upperCI  = resultsTable[i, if (individual) "uCI" else as.character(.5 + options[["inferenceCiWidth"]] / 2)]
      )
    }

    if (individual) {
      tempRow[["ess"]]   <- round(resultsTable[i, "ESS"])
      tempRow[["rHat"]]  <- resultsTable[i, "R_hat"]
    }

    jaspTable$addRows(tempRow)
  }

  # add footnote
  footnotes       <- attr(resultsTable, "footnotes")

  return(jaspTable)
}
.robttCoefNames               <- function(coefficient, options) {
  if (coefficient == "delta")
    return(gettextf("Effect size (%s)", "\u03B4"))
  else if (coefficient == "rho")
    return(gettextf("Precision allocation (%s)","\u03C1"))
  else if (coefficient == "nu")
    return(gettextf("Degrees of freedom (%s)","\u03BD"))
}
.robttAllignSummaryTableNames <- function(resultsTable) {

  rownames(resultsTable) <- gsub("delta[1]", "delta", rownames(resultsTable), fixed = TRUE)
  rownames(resultsTable) <- gsub("rho[1]",   "rho",   rownames(resultsTable), fixed = TRUE)
  rownames(resultsTable) <- gsub("nu[1]",    "nu",    rownames(resultsTable), fixed = TRUE)

  return(resultsTable)
}

# main functions
.robttPriorsPlots              <- function(jaspResults, options) {

  # create / access the container
  if (!is.null(jaspResults[["priorPlots"]]))
    priorPlots <- jaspResults[["priorPlots"]]
  else {
    priorPlots <- createJaspContainer(title = gettext("Prior Plots"))
    priorPlots$dependOn(c("priorDistributionPlot", "inferenceHeterogeneityAsStandardDeviationRatio"))
    priorPlots$position <- 2
    jaspResults[["priorPlots"]] <- priorPlots
  }

  # extract the priors
  priors <- jaspResults[["priors"]][["object"]]

  # create container for each of the parameters
  for (parameter in c("effect", "heterogeneity", "outliers")) {

    if (!is.null(priorPlots[[parameter]]))
      parameterContainer <- priorPlots[[parameter]]
    else {
      parameterContainer <- createJaspContainer(title = switch(
        parameter,
        "effect"          = gettext("Effect"),
        "heterogeneity"   = gettext("Heterogeneity"),
        "outliers"        = gettext("Outliers")
      ))
      parameterContainer$position <- switch(
        parameter,
        "effect"          = 1,
        "heterogeneity"   = 2,
        "outliers"        = 3
      )
      priorPlots[[parameter]] <- parameterContainer
    }

    # create container for null and alternative models
    for (type in c("null", "alternative")) {

      if (!is.null(parameterContainer[[type]]))
        next

      tempPriors <- switch(
        paste0(parameter, "-", type),
        "effect-alternative"        = priors[["modelsEffect"]],
        "effect-null"               = priors[["modelsEffectNull"]],
        "heterogeneity-alternative" = priors[["modelsHeterogeneity"]],
        "heterogeneity-null"        = priors[["modelsHeterogeneityNull"]],
        "outliers-alternative"      = priors[["modelsOutliers"]],
        "outliers-null"             = priors[["modelsOutliersNull"]]
      )

      if (length(tempPriors) == 0)
        next

      typePrior <- createJaspPlot(width = 400,  height = 300, title = switch(
        type,
        "null"         = gettext("Null"),
        "alternative"  = gettext("Alternative")
      ))
      typePrior$position <- switch(
        type,
        "null"         = 1,
        "alternative"  = 2
      )
      typePrior$dependOn(switch(
        parameter,
        "effect"        = c("modelsEffect", "modelsEffectNull"),
        "heterogeneity" = c("modelsHeterogeneity", "modelsHeterogeneityNull"),
        "outliers"      = c("modelsOutliers", "modelsOutliersNull")
      ))
      parameterContainer[[type]] <- typePrior

      p <- plot(
        tempPriors,
        plot_type = "ggplot",
        par_name  = switch(
          parameter,
          "effect"        = bquote(delta),
          "heterogeneity" = bquote(rho),
          "outliers"      = bquote(nu)),
        xlim                     = if(parameter == "heterogeneity" && options[["inferenceHeterogeneityAsStandardDeviationRatio"]]) log(2^c(-4,4)),
        transformation           = switch(
          parameter,
          "effect"        = NULL,
          "heterogeneity" = if(options[["inferenceHeterogeneityAsStandardDeviationRatio"]]) RoBTT::rho2logsdr else NULL,
          "outliers"      = "lin")
        ,
        transformation_arguments = if(parameter == "outliers") list(a = 2, b = 1),
        transformation_settings  = parameter == "heterogeneity"
      )

      if(parameter == "heterogeneity" && options[["inferenceHeterogeneityAsStandardDeviationRatio"]]){
        p <- p + ggplot2::scale_x_continuous("Standard deviation ratio", limits = log(2^c(-4,4)), breaks = log(2^seq(-4,4,1)), labels = round(2^seq(-4,4,1), 3))
      }

      p <- jaspGraphs::themeJasp(p)

      typePrior[["plotObject"]] <- p
    }
  }

  return()
}
.robttModelPreviewTable        <- function(jaspResults, options) {

  # create / access the container
  if (!is.null(jaspResults[["modelPreview"]])) {
    return()
  } else {
    modelPreview <- createJaspContainer(title = gettext("Model Preview"))
    modelPreview$dependOn(.robttDependencies)
    modelPreview$position <- 1
    jaspResults[["modelPreview"]] <- modelPreview
  }


  # extract the priors
  priors  <- jaspResults[["priors"]][["object"]]


  # set error if no priors are specified
  if (
    (length(priors[["modelsEffect"]])        == 0 && length(priors[["modelsEffectNull"]])        == 0) ||
    (length(priors[["modelsHeterogeneity"]]) == 0 && length(priors[["modelsHeterogeneityNull"]]) == 0) ||
    (length(priors[["modelsOutliers"]])      == 0 && length(priors[["modelsOutliersNull"]])      == 0)
  ) {
    priorsError <- createJaspTable()
    priorsError$setError(gettext("Please specify a prior distribution for each parameter in the Models specification section (either null or alternative)."))
    modelPreview[["priorsError"]] <- priorsError
    return()
  }

  # create the setup table
  fitSummary   <- RoBTT::check_setup(
    prior_delta      = priors[["modelsEffect"]],
    prior_rho        = priors[["modelsHeterogeneity"]],
    prior_nu         = priors[["modelsOutliers"]],
    prior_delta_null = priors[["modelsEffectNull"]],
    prior_rho_null   = priors[["modelsHeterogeneityNull"]],
    prior_nu_null    = priors[["modelsOutliersNull"]],
    models           = TRUE,
    silent           = TRUE
  )


  ### create overview table
  overallSummary <- createJaspTable(title = gettext("Model Summary"))
  overallSummary$position <- 1

  overallSummary$addColumnInfo(name = "terms",     title = "",                type = "string")
  overallSummary$addColumnInfo(name = "models",    title = gettext("Models"), type = "string")
  overallSummary$addColumnInfo(name = "priorProb", title = gettext("P(M)"),   type = "number")

  for (i in 1:nrow(fitSummary[["components"]])) {
    tempRow <- list(
      terms     = if (i == 1) gettext("Effect") else if (i == 2) gettext("Heterogeneity") else if (i == 3) gettext("Outliers"),
      models    = paste0(fitSummary[["components"]][[i, "models"]], "/", attr(fitSummary[["components"]], "n_models")),
      priorProb = fitSummary[["components"]][[i, "prior_prob"]]
    )

    overallSummary$addRows(tempRow)
  }
  overallSummary$addFootnote(gettext("This analysis uses MCMC and might require a prolonged time to complete."), symbol = "\u26A0")
  modelPreview[["overallSummary"]] <- overallSummary


  ### create models overview table
  modelsSummary <- createJaspTable(title = gettext("Model Specification Preview"))
  modelsSummary$position <- 2

  overtitlePrior <- gettext("Prior Distribution")

  modelsSummary$addColumnInfo(name = "number",              title = "#",                         type = "integer")
  modelsSummary$addColumnInfo(name = "distribution",        title = gettext("Distribution"),     type = "string")
  modelsSummary$addColumnInfo(name = "priorEffect",         title = gettext("Effect"),           type = "string", overtitle = overtitlePrior)
  modelsSummary$addColumnInfo(name = "priorHeterogeneity",  title = gettext("Heterogeneity"),    type = "string", overtitle = overtitlePrior)
  modelsSummary$addColumnInfo(name = "priorOutliers",       title = gettext("Outliers"),         type = "string", overtitle = overtitlePrior)
  modelsSummary$addColumnInfo(name = "priorProb",           title = gettext("P(M)"),             type = "number")

  for (i in 1:nrow(fitSummary[["summary"]])) {
    tempRow <- list(
      number             = fitSummary[["summary"]][i, "Model"],
      distribution       = fitSummary[["summary"]][i, "Distribution"],
      priorEffect        = fitSummary[["summary"]][i, "delta"],
      priorHeterogeneity = fitSummary[["summary"]][i, "rho"],
      priorOutliers      = fitSummary[["summary"]][i, "nu"],
      priorProb          = fitSummary[["summary"]][i, "prior_prob"]
    )

    modelsSummary$addRows(tempRow)
  }
  modelsSummary$addFootnote(gettext("The analysis will estimate multiple models using MCMC and might require a prolonged time to complete."), symbol = "\u26A0")

  modelPreview[["modelsSummary"]] <- modelsSummary


  return()
}
.robttFitModel                 <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["model"]]))
    return()

  model <- createJaspState()
  model$dependOn(.robttDependencies)
  jaspResults[["model"]] <- model


  priors <- jaspResults[["priors"]]$object
  fit    <- try(RoBTT::RoBTT(
    # data
    x1 = dataset[[options[["dependent"]]]][dataset[[options[["group"]]]] == levels(dataset[[options[["group"]]]])[1]],
    x2 = dataset[[options[["dependent"]]]][dataset[[options[["group"]]]] == levels(dataset[[options[["group"]]]])[2]],

    # priors
    prior_delta      = priors[["modelsEffect"]],
    prior_rho        = priors[["modelsHeterogeneity"]],
    prior_nu         = priors[["modelsOutliers"]],
    prior_delta_null = priors[["modelsEffectNull"]],
    prior_rho_null   = priors[["modelsHeterogeneityNull"]],
    prior_nu_null    = priors[["modelsOutliersNull"]],
    # sampling settings
    chains  = options[["advancedMcmcChains"]],
    warmup  = options[["advancedMcmcAdaptation"]],
    iter    = options[["advancedMcmcAdaptation"]] + options[["advancedMcmcSamples"]],
    thin    = options[["advancedMcmcThin"]],
    # additional settings
    convergence_checks = RoBTT::set_convergence_checks(
      max_Rhat            = 1.05,
      min_ESS             = 500
    ),
    save     = "all",
    seed     = options[["seed"]], # TODO: .getSeedJASP(options)
    silent   = TRUE,
    is_JASP  = TRUE
  ))


  # error handling
  if (jaspBase::isTryError(fit))
    .quitAnalysis(fit)


  # update the fit and reset notifier
  model[["object"]] <- fit

  return()
}
.robttSummaryTable             <- function(jaspResults, options) {

  if (!is.null(jaspResults[["mainSummary"]])) {
    return()
  } else {
    # create container
    mainSummary <- createJaspContainer(title = gettext("Summary"))
    mainSummary$position <- 3
    mainSummary$dependOn( c(.robttDependencies, "bayesFactorType", "inferenceCiWidth", "inferenceConditionalParameterEstimates", "inferenceHeterogeneityAsStandardDeviationRatio"))
    jaspResults[["mainSummary"]] <- mainSummary
  }

  if (is.null(jaspResults[["model"]])) {
    if (options[["inferenceConditionalParameterEstimates"]]) {
      conditionalSummary <- createJaspTable(title = gettext("Conditional Estimates"), dependencies = "inferenceConditionalParameterEstimates")
      conditionalSummary <- .robttTableFillCoef(conditionalSummary, NULL, options)
      jaspResults[["mainSummary"]][["conditionalSummary"]] <- conditionalSummary
    }
    return()
  }

  # remove the model preview
  jaspResults[["modelPreview"]] <- NULL

  # extract the model
  fit   <- jaspResults[["model"]][["object"]]

  # some shared info
  fitSummary <- summary(
    fit,
    logBF        = options[["bayesFactorType"]] == "LogBF10",
    BF01         = options[["bayesFactorType"]] == "BF01",
    probs        = c(.5 + c(-1, 1) * options[["inferenceCiWidth"]] / 2),
    conditional  = options[["inferenceConditionalParameterEstimates"]],
  )

  titleBF <- switch(
    options[["bayesFactorType"]],
    "BF10"    = gettext("Inclusion BF"),
    "BF01"    = gettext("Exclusion BF"),
    "LogBF10" = gettext("log(Inclusion BF)")
  )

  ### create overview table
  overallSummary <- createJaspTable(title = gettext("Model Summary"))
  overallSummary$position <- 1

  overallSummary$addColumnInfo(name = "terms",     title = "",                   type = "string")
  overallSummary$addColumnInfo(name = "models",    title = gettext("Models"),    type = "string")
  overallSummary$addColumnInfo(name = "priorProb", title = gettext("P(M)"),      type = "number")
  overallSummary$addColumnInfo(name = "postProb",  title = gettext("P(M|data)"), type = "number")
  overallSummary$addColumnInfo(name = "BF",        title = titleBF,              type = "number")

  for (i in 1:nrow(fitSummary[["components"]])) {
    overallSummary$addRows(list(
      terms     = rownames(fitSummary[["components"]])[i],
      models    = paste0(fitSummary[["components"]][i, "models"], "/",  attr(fitSummary[["components"]], "n_models")[i]),
      priorProb = fitSummary[["components"]][i, "prior_prob"],
      postProb  = fitSummary[["components"]][i, "post_prob"],
      BF        = fitSummary[["components"]][i, 4]
    ))
  }

  errorsAndWarnings <- RoBTT::check_RoBTT(fit)
  for (i in seq_along(errorsAndWarnings)) {
    overallSummary$addFootnote(symbol = gettext("Warning:"), errorsAndWarnings[i])
  }

  mainSummary[["overallSummary"]] <- overallSummary


  ### create model averaged results tables
  # estimate table
  averagedSummary <- createJaspTable(title = gettext("Model Averaged Estimates"))
  averagedSummary$position <- 2
  attr(fitSummary[["estimates"]], "mean_sdr") <- mean(RoBTT::rho2logsdr$fun(fit[["RoBTT"]][["posteriors"]][["rho"]]))
  averagedSummary <- .robttTableFillCoef(averagedSummary, fitSummary[["estimates"]], options)
  mainSummary[["averagedSummary"]] <- averagedSummary


  ### create conditional models results tables
  if (options[["inferenceConditionalParameterEstimates"]]) {
    # estimate table
    conditionalSummary <- createJaspTable(title = gettext("Conditional Estimates"))
    conditionalSummary$position <- 5
    attr(fitSummary[["estimates_conditional"]], "mean_sdr") <- mean(RoBTT::rho2logsdr$fun(fit[["RoBTT"]][["posteriors_conditional"]][["rho"]]))
    conditionalSummary <- .robttTableFillCoef(conditionalSummary, fitSummary[["estimates_conditional"]], options)
    mainSummary[["conditionalSummary"]] <- conditionalSummary
  }

  return()
}
.robttModelsOvervievTable      <- function(jaspResults, options) {

  ### create overview table
  modelsSummary <- createJaspTable(title = gettext("Models Overview"))
  modelsSummary$position <- 6
  modelsSummary$dependOn(c(.robttDependencies, "bayesFactorType", "inferenceModelsOverview", "inferenceModelsOverviewBF", "inferenceModelsOverviewOrder", "inferenceShortenPriorName"))
  jaspResults[["mainSummary"]][["modelsSummary"]] <- modelsSummary

  if (options[["inferenceModelsOverviewBfComparison"]] == "inclusion")
    titleBF <- switch(
      options[["bayesFactorType"]],
      "BF10"    = gettext("Inclusion BF"),
      "BF01"    = gettext("Exclusion BF"),
      "LogBF10" = gettext("log(Inclusion BF)")
    )
  else
    titleBF <- switch(
      options[["bayesFactorType"]],
      "BF10"    = gettextf("BF%s",     "\u2081\u2080"),
      "BF01"    = gettextf("BF%s",     "\u2080\u2081"),
      "LogBF10" = gettextf("log(BF%s)","\u2081\u2080")
    )

  overtitlePrior <- gettext("Prior Distribution")

  modelsSummary$addColumnInfo(name = "number",             title = "#",                         type = "integer")
  modelsSummary$addColumnInfo(name = "distribution",       title = gettext("Distribution"),     type = "string")
  modelsSummary$addColumnInfo(name = "priorEffect",        title = gettext("Effect"),           type = "string",  overtitle = overtitlePrior)
  modelsSummary$addColumnInfo(name = "priorHeterogeneity", title = gettext("Heterogeneity"),    type = "string",  overtitle = overtitlePrior)
  modelsSummary$addColumnInfo(name = "priorOutliers",      title = gettext("Outliers"),         type = "string",  overtitle = overtitlePrior)
  modelsSummary$addColumnInfo(name = "priorProb",          title = gettext("P(M)"),             type = "number")
  modelsSummary$addColumnInfo(name = "postProb",           title = gettext("P(M|data)"),        type = "number")
  modelsSummary$addColumnInfo(name = "marglik",            title = gettext("log(MargLik)"),     type = "number")
  modelsSummary$addColumnInfo(name = "BF",                 title = titleBF,                     type = "number")

  if (is.null(jaspResults[["model"]]))
    return()

  # extract the model
  fit   <- jaspResults[["model"]][["object"]]

  # some shared info
  fitSummary <- summary(
    fit,
    type       = "models",
    short_name = options[["inferenceShortenPriorName"]]
  )

  # do ordering
  if (options[["inferenceModelsOverviewOrder"]] == "marginalLikelihood")
    fitSummary[["summary"]] <- fitSummary[["summary"]][order(fitSummary[["summary"]][["marglik"]], decreasing = TRUE),]
  else if (options[["inferenceModelsOverviewOrder"]] == "posteriorProbability")
    fitSummary[["summary"]] <- fitSummary[["summary"]][order(fitSummary[["summary"]][["post_prob"]], decreasing = TRUE),]

  # compute the BF requested
  if (options[["inferenceModelsOverviewBfComparison"]] == "inclusion") {
    bf <- fitSummary[["summary"]][, "inclusion_BF"]
  } else if (options[["inferenceModelsOverviewBfComparison"]] == "best") {
    bf <- exp(fitSummary[["summary"]][["marglik"]] - max(fitSummary[["summary"]][["marglik"]]))
  } else if (options[["inferenceModelsOverviewBfComparison"]] == "previous") {
    tempThisMargLik <- fitSummary[["summary"]][["marglik"]][-length(fitSummary[["summary"]][["marglik"]])]
    tempPrevMargLik <- fitSummary[["summary"]][["marglik"]][-1]
    bf <- c(1, exp(tempPrevMargLik - tempThisMargLik))
  }

  # fill the rows
  for (i in 1:nrow(fitSummary[["summary"]])) {
    modelsSummary$addRows(list(
      number             = fitSummary[["summary"]][i, "Model"],
      distribution       = fitSummary[["summary"]][i, "Distribution"],
      priorEffect        = fitSummary[["summary"]][i, "delta"],
      priorHeterogeneity = fitSummary[["summary"]][i, "rho"],
      priorOutliers      = fitSummary[["summary"]][i, "nu"],
      priorProb          = fitSummary[["summary"]][i, "prior_prob"],
      postProb           = fitSummary[["summary"]][i, "post_prob"],
      marglik            = fitSummary[["summary"]][i, "marglik"],
      BF                 = BayesTools::format_BF(bf[i], logBF = options[["bayesFactorType"]] == "LogBF10", BF01 = options[["bayesFactorType"]] == "BF01")
    ))
  }

  return()
}
.robttModelsSummaryTable       <- function(jaspResults, options) {

  if (!is.null(jaspResults[["individualModels"]])) {
    return()
  } else {
    individualModels <- createJaspContainer(title = gettext("Individual Models Summary"))
    individualModels$position <- 5
    individualModels$dependOn(c(.robttDependencies, "bayesFactorType", "inferenceIndividualModels", "inferenceIndividualModelsSingleModel", "inferenceIndividualModelsSingleModelNumber", "inferenceShortenPriorName", "inferenceOutputScale"))
    jaspResults[["individualModels"]] <- individualModels
  }

  titleBF <- switch(
    options[["bayesFactorType"]],
    "BF10"    = gettext("Inclusion BF"),
    "BF01"    = gettext("Exclusion BF"),
    "LogBF10" = gettext("log(Inclusion BF)")
  )

  if (is.null(jaspResults[["model"]])) {

    tempModel <- createJaspContainer(title = gettext("Model #"))
    individualModels[["modelI"]] <- tempModel

    tempPriors <- createJaspTable(title = gettext("Priors"))
    tempPriors$addColumnInfo(name = "priorDelta",   title = gettext("Effect"),        type = "string")
    tempPriors$addColumnInfo(name = "priorRho",     title = gettext("Heterogeneity"), type = "string")
    tempPriors$addColumnInfo(name = "priorNu",      title = gettext("Outliers"),      type = "string")
    tempModel[["tempPriors"]] <- tempPriors

    tempInfo <- createJaspTable(title = gettext("Information"))
    tempInfo$addColumnInfo(name = "distribution", title = gettext("Distribution"),  type = "string")
    tempInfo$addColumnInfo(name = "priorProb",    title = gettext("P(M)"),          type = "number")
    tempInfo$addColumnInfo(name = "postProb",     title = gettext("P(M|data)"),     type = "number")
    tempInfo$addColumnInfo(name = "marglik",      title = gettext("log(MargLik)"),  type = "number")
    tempInfo$addColumnInfo(name = "BF",           title = titleBF,                  type = "number")
    tempModel[["tempInfo"]] <- tempInfo

    tempCoef <- createJaspTable(title = gettext("Model Estimates"))
    tempCoef <- .robttTableFillCoef(tempCoef, NULL, options, individual = TRUE)
    tempModel[["tempCoef"]] <- tempCoef

    return()
  }

  # extract the model
  fit   <- jaspResults[["model"]][["object"]]

  # some shared info
  fitSummary <- summary(
    fit,
    type          = "individual",
    short_name    = options[["inferenceShortenPriorName"]]
  )

  ### create tables for individual models

  # select models to iterate over
  if (options[["inferenceIndividualModelsSingleModel"]]) {
    modelsI <- options[["inferenceIndividualModelsSingleModelNumber"]]
    if (modelsI < 1 || modelsI > length(fit[["models"]])) {
      tempModel  <- createJaspContainer(title = gettextf("Model %i", modelsI))
      tempError  <- createJaspTable(title = "")
      tempError$setError(gettextf("Model %i does not exist. Select one of the models between 1 and %i.", modelsI, length(fit[["models"]])))
      tempModel[["tempError"]]                     <- tempError
      individualModels[[paste0("model", modelsI)]] <- tempModel
      return()
    }
  } else {
    modelsI <- 1:length(fit[["models"]])
  }


  # do the iteration
  for (i in modelsI) {

    tempModel <- createJaspContainer(title = gettextf("Model %i", i))
    individualModels[[paste0("model", i)]] <- tempModel

    ### model priors
    tempPriors <- createJaspTable(title = gettext("Priors"))
    tempPriors$addColumnInfo(name = "priorDelta",  title = gettext("Effect"),         type = "string")
    tempPriors$addColumnInfo(name = "priorRho",    title = gettext("Heterogeneity"),  type = "string")
    tempPriors$addColumnInfo(name = "priorNu",     title = gettext("Outliers"),       type = "string")

    tempPriors$addRows(list(
      priorDelta  = print(fit[["models"]][[i]][["priors"]][["delta"]], silent = TRUE, short_name = options[["inferenceShortenPriorName"]]),
      priorRho    = print(fit[["models"]][[i]][["priors"]][["rho"]],   silent = TRUE, short_name = options[["inferenceShortenPriorName"]]),
      priorNu     = if (is.null(fit[["models"]][[i]][["priors"]][["nu"]]))
        ""
      else
        print(fit[["models"]][[i]][["priors"]][["nu"]], silent = TRUE, short_name = options[["inferenceShortenPriorName"]])
    ))

    tempModel[["tempPriors"]] <- tempPriors


    ### model information
    tempInfo <- createJaspTable(title = gettext("Information"))

    tempInfo$addColumnInfo(name = "distribution", title = gettext("Distribution"), type = "string")
    tempInfo$addColumnInfo(name = "priorProb",   title = gettext("P(M)"),          type = "number")
    tempInfo$addColumnInfo(name = "postProb",    title = gettext("P(M|data)"),     type = "number")
    tempInfo$addColumnInfo(name = "marglik",     title = gettext("log(MargLik)"),  type = "number")
    tempInfo$addColumnInfo(name = "BF",          title = titleBF,                  type = "number")

    tempInfo$addRows(list(
      distribution = fit[["models"]][[i]][["likelihood"]],
      priorProb    = fit[["models"]][[i]][["inference"]][["prior_prob"]],
      postProb     = fit[["models"]][[i]][["inference"]][["post_prob"]],
      marglik      = fit[["models"]][[i]][["inference"]][["marglik"]],
      BF           = BayesTools::format_BF(fit[["models"]][[i]][["inference"]][["inclusion_BF"]], logBF = options[["bayesFactorType"]] == "LogBF10", BF01 = options[["bayesFactorType"]] == "BF01")
    ))

    tempModel[["tempInfo"]] <- tempInfo


    ### model coefficients
    # estimate table
    tempCoef <- createJaspTable(title = gettext("Model Estimates"))
    tempCoef <- .robttTableFillCoef(tempCoef, fitSummary$models[[i]][["estimates"]], options, individual = TRUE)
    tempModel[["tempCoef"]] <- tempCoef

  }

  return()
}
.robttEstimatesPlot            <- function(jaspResults, options, parameter) {

  # create / access the container
  if (is.null(jaspResults[["estimatesPlots"]])) {
    estimatesPlots <- createJaspContainer(title = gettext("Posterior Distribution Plots"))
    estimatesPlots$position <- 7
    estimatesPlots$dependOn(c(.robttDependencies, "plotsPooledEstimatesType", "plotsPooledEstimatesPriorDistribution"))
    jaspResults[["estimatesPlots"]] <- estimatesPlots
  } else {
    estimatesPlots <- jaspResults[["estimatesPlots"]]
  }

  # don't redo an already created plot
  if (!is.null(estimatesPlots[[parameter]]))
    return()

  # prepare the plot object
  title  <- sprintf(
    "%1$s %2$s",
    switch(
      options[["plotsPooledEstimatesType"]],
      "conditional" = gettext("Conditional"),
      "averaged"    = gettext("Model Averaged")
    ),
    switch(
      parameter,
      "delta" = gettext("Effect Size Estimate"),
      "rho"   = gettext("Heterogeneity Estimate (Precision Allocation)"),
      "nu"    = gettext("Outliers Estimate (Degrees of Freedom)")
    ))
  height <- 350
  width  <- 600

  tempPlot <- createJaspPlot(title = title, width = width, height = height)
  tempPlot$position <- switch(
    parameter,
    "delta" = 1,
    "rho"   = 2,
    "nu"    = 3
  )
  tempPlot$dependOn(switch(
    parameter,
    "delta" = "plotsPooledEstimatesEffect",
    "rho"   = "plotsPooledEstimatesHeterogeneity",
    "nu"    = c("plotsPooledEstimatesOutliers", "inferenceHeterogeneityAsStandardDeviationRatio")
  ))
  estimatesPlots[[parameter]] <- tempPlot

  if (is.null(jaspResults[["model"]]))
    return()

  # extract the model
  fit   <- jaspResults[["model"]][["object"]]

  # plot
  p <- try(plot(
    fit,
    parameter     = parameter,
    prior         = options[["plotsPooledEstimatesPriorDistribution"]],
    conditional   = options[["plotsPooledEstimatesType"]] == "conditional",
    transform_rho = options[["inferenceHeterogeneityAsStandardDeviationRatio"]],
    plot_type     = "ggplot"
  ))

  if (jaspBase::isTryError(p)) {
    tempPlot$setError(p)
    return()
  }

  if (attr(p, "sec_axis"))
    p <- jaspGraphs::themeJasp(p, sides = "blr") + ggplot2::theme(
      axis.title.y.right = ggplot2::element_text(vjust = 3.25),
      plot.margin        = ggplot2::margin(t = 3, r = 12, b = 0, l = 1))
  else
    p <- jaspGraphs::themeJasp(p, sides = "bl")

  estimatesPlots[[parameter]]$plotObject <- p

  return()
}
.robttDiagnosticsOverviewTable <- function(jaspResults, options) {

  # create / access the container
  if (is.null(jaspResults[["diagnostics"]])) {
    diagnostics <- createJaspContainer(title = gettext("Diagnostics"))
    diagnostics$position <- 9
    diagnostics$dependOn(.robttDependencies)
    jaspResults[["diagnostics"]] <- diagnostics
  } else {
    diagnostics <- jaspResults[["diagnostics"]]
  }

  if (!is.null(diagnostics[["diagosticsTable"]])) {
    return()
  }


  ### create overview table
  diagosticsTable <-  createJaspTable(title = gettext("Models Diagnostics Overview"))
  diagosticsTable$position <- 1
  diagosticsTable$dependOn(c(.robttDependencies, "mcmcDiagnosticsOverviewTable", "inferenceShortenPriorName"))
  diagnostics[["diagosticsTable"]] <- diagosticsTable

  overtitlePrior <- gettext("Prior Distribution")

  diagosticsTable$addColumnInfo(name = "number",             title = "#",                         type = "integer")
  diagosticsTable$addColumnInfo(name = "distribution",       title = gettext("Distribution"),     type = "string")
  diagosticsTable$addColumnInfo(name = "priorEffect",        title = gettext("Effect"),           type = "string",  overtitle = overtitlePrior)
  diagosticsTable$addColumnInfo(name = "priorHeterogeneity", title = gettext("Heterogeneity"),    type = "string",  overtitle = overtitlePrior)
  diagosticsTable$addColumnInfo(name = "priorOutliers",      title = gettext("Outliers"),         type = "string",  overtitle = overtitlePrior)
  diagosticsTable$addColumnInfo(name = "ess",                title = gettext("min(ESS)"),         type = "integer")
  diagosticsTable$addColumnInfo(name = "rHat",               title = gettext("max(R-hat)"),       type = "number")


  if (is.null(jaspResults[["model"]]))
    return()

  # extract the model
  fit   <- jaspResults[["model"]][["object"]]

  # get the diagnostics summary
  fitSummary <- summary(
    fit,
    type       = "diagnostics",
    short_name = options[["inferenceShortenPriorName"]]
  )

  for (i in 1:nrow(fitSummary[["diagnostics"]])) {
    diagosticsTable$addRows(list(
      number             = fitSummary[["diagnostics"]][i, "Model"],
      distribution       = fitSummary[["diagnostics"]][i, "Distribution"],
      priorEffect        = fitSummary[["diagnostics"]][i, "delta"],
      priorHeterogeneity = fitSummary[["diagnostics"]][i, "rho"],
      priorOutliers      = fitSummary[["diagnostics"]][i, "nu"],
      ess                = round(fitSummary[["diagnostics"]][i, "min_ESS"]),
      rHat               = fitSummary[["diagnostics"]][i, "max_R_hat"]
    ))
  }

  return()
}
.robttDiagnosticsPlots         <- function(jaspResults, options) {

  # create / access the container
  if (is.null(jaspResults[["diagnostics"]])) {
    diagnostics <- createJaspContainer(title = gettext("Diagnostics"))
    diagnostics$position <- 9
    diagnostics$dependOn(.robttDependencies)
    jaspResults[["diagnostics"]] <- diagnostics
  } else {
    diagnostics <- jaspResults[["diagnostics"]]
  }


  # create waiting plot
  if (!(options[["mcmcDiagnosticsPlotEffect"]] || options[["mcmcDiagnosticsPlotHeterogeneity"]] || options[["mcmcDiagnosticsPlotOutliers"]]) &&
      (options[["mcmcDiagnosticsPlotTypeTrace"]] || options[["mcmcDiagnosticsPlotTypeAutocorrelation"]] || options[["mcmcDiagnosticsPlotTypePosteriorSamplesDensity"]]) ||
      is.null(jaspResults[["model"]])) {
    tempWait  <- createJaspPlot(title = "")
    tempWait$dependOn(c("mcmcDiagnosticsPlotEffect", "mcmcDiagnosticsPlotHeterogeneity", "mcmcDiagnosticsPlotOutliers", "mcmcDiagnosticsPlotTypeTrace", "mcmcDiagnosticsPlotTypeAutocorrelation", "mcmcDiagnosticsPlotTypePosteriorSamplesDensity"))
    diagnostics[["tempWait"]] <- tempWait
    return()
  }


  # extract the model
  fit   <- jaspResults[["model"]][["object"]]

  # select models to iterate over
  if (options[["mcmcDiagnosticsPlotSingleModel"]]) {
    modelsI <- options[["mcmcDiagnosticsPlotSingleModelNumber"]]
    if (modelsI < 1 || modelsI > length(fit[["models"]])) {
      tempModel  <- createJaspContainer(title = gettextf("Model %i", modelsI))
      diagnostics[[paste0("model", modelsI)]] <- tempModel
      tempError  <- createJaspPlot(title = "")
      tempError$dependOn("mcmcDiagnosticsPlotSingleModelNumber", "mcmcDiagnosticsPlotSingleModel")
      tempError$setError(gettextf("Model %1$i does not exist. Select one of the models between 1 and %2$i.", modelsI, length(fit[["models"]])))
      tempModel[["tempError"]] <- tempError
      return()
    }
  } else {
    modelsI <- 1:length(fit[["models"]])
  }

  # collect the parameters
  parameters <- NULL
  if (options[["mcmcDiagnosticsPlotEffect"]])
    parameters <- c(parameters, "delta")
  if (options[["mcmcDiagnosticsPlotHeterogeneity"]])
    parameters <- c(parameters, "rho")
  if (options[["mcmcDiagnosticsPlotOutliers"]])
    parameters <- c(parameters, "nu")


  # do the iterations
  for (i in modelsI) {
    # create / access container for individual models
    if (is.null(diagnostics[[paste0("model_", i)]])) {
      tempModel <- createJaspContainer(title = gettextf("Model %i", i))
      tempModel$position <- i
      tempModel$dependOn(c("mcmcDiagnosticsPlotSingleModelNumber", "mcmcDiagnosticsPlotSingleModel"))
      diagnostics[[paste0("model", i)]] <- tempModel
    } else {
      tempModel <- diagnostics[[paste0("model", i)]]
    }

    noPars <- TRUE # tracker for checking whether any parameter was plotted

    for (par in parameters) {
      # create / access container for individual parameters
      if (is.null(tempModel[[par]])) {
        tempPar <- createJaspContainer(title = switch(
          par,
          "delta" = gettext("Effect"),
          "rho"   = gettext("Heterogeneity (Precision Allocation)"),
          "nu"    = gettext("Outliers (Degrees of Freedom)")
        ))
        tempPar$position <- switch(
          par,
          "delta" = 1,
          "rho"   = 2,
          "nu"    = 3
        )
        tempPar$dependOn(switch(
          par,
          "delta" = "mcmcDiagnosticsPlotEffect",
          "rho"   = "mcmcDiagnosticsPlotHeterogeneity",
          "nu"    = "mcmcDiagnosticsPlotOutliers"
        ))
        tempModel[[par]] <- tempPar
      } else {
        tempPar <- tempModel[[par]]
      }


      # add trace plots
      if (options[["mcmcDiagnosticsPlotTypeTrace"]]) {
        # create / access container for trace plots
        if (is.null(tempPar[["trace"]])) {
          tempPlots <- createJaspContainer(gettext("Trace plots"))
          tempPlots$position <- 1
          tempPlots$dependOn("mcmcDiagnosticsPlotTypeTrace")
          tempPar[["trace"]] <- tempPlots
        } else {
          tempPlots <- tempPar[["trace"]]
        }

        # create plots
        newPlot  <- RoBTT::diagnostics(
          fit,
          parameter     = par,
          type          = "trace",
          show_models   = i,
          title         = FALSE
        )

        if (is.null(newPlot))
          next

        tempPlot             <- createJaspPlot(width = 400, aspectRatio = .7)
        tempPlot$plotObject  <- newPlot + jaspGraphs::geom_rangeframe(sides = "bl") + jaspGraphs::themeJaspRaw()
        tempPar[["trace"]] <- tempPlot

        noPars <- FALSE
      }


      # add autocorrelation plots
      if (options[["mcmcDiagnosticsPlotTypeAutocorrelation"]]) {
        # create / access container for trace plots
        if (is.null(tempPar[["autocor"]])) {
          tempPlots <- createJaspContainer(gettext("Average autocorrelations"))
          tempPlots$position <- 2
          tempPlots$dependOn("mcmcDiagnosticsPlotTypeAutocorrelation")
          tempPar[["autocor"]] <- tempPlots
        } else {
          tempPlots <- tempPar[["autocor"]]
        }

        # create plots
        newPlot <- RoBTT::diagnostics(
          fit,
          parameter     = par,
          type          = "autocorrelations",
          show_models   = i,
          title         = FALSE
        )

        if (is.null(newPlot))
          next

        tempPlot             <- createJaspPlot(width = 400, aspectRatio = .7)
        tempPlot$plotObject  <- newPlot + jaspGraphs::geom_rangeframe(sides = "bl") + jaspGraphs::themeJaspRaw()
        tempPar[["autocorrelations"]] <- tempPlot

        noPars <- FALSE
      }


      # add sample densities plots
      if (options[["mcmcDiagnosticsPlotTypePosteriorSamplesDensity"]]) {
        # create / access container for trace plots
        if (is.null(tempPar[["samples"]])) {
          tempPlots <- createJaspContainer(gettext("Posterior samples densities"))
          tempPlots$position <- 3
          tempPlots$dependOn("mcmcDiagnosticsPlotTypePosteriorSamplesDensity")
          tempPar[["samples"]] <- tempPlots
        } else {
          tempPlots <- tempPar[["samples"]]
        }

        # create plots
        newPlot <- RoBTT::diagnostics(
          fit,
          parameter     = par,
          type          = "densities",
          show_models   = i,
          title         = FALSE
        )

        if (is.null(newPlot))
          next

        tempPlot             <- createJaspPlot(width = 400, aspectRatio = .7)
        tempPlot$plotObject  <- newPlot + jaspGraphs::geom_rangeframe(sides = "b") + jaspGraphs::themeJaspRaw()
        tempPar[["densities"]] <- tempPlot

        noPars <- FALSE
      }

    }

    # show error if only one model is selected but doesn't contain any of the diagnostics
    if (noPars && options[["mcmcDiagnosticsPlotSingleModelNumber"]]) {
      tempError  <- createJaspPlot(title = "")
      tempError$dependOn(c("mcmcDiagnosticsPlotEffect", "mcmcDiagnosticsPlotHeterogeneity", "mcmcDiagnosticsPlotOutliers", "mcmcDiagnosticsPlotTypeTrace", "mcmcDiagnosticsPlotTypeAutocorrelation", "mcmcDiagnosticsPlotTypePosteriorSamplesDensity"))
      tempError$setError(gettextf("Model %i does not contain any of the selected parameters.", i))
      tempModel[["tempError"]] <- tempError
    }
  }

  return()
}
