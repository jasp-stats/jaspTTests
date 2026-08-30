# ==============================================================================
# RoboReport Script: Bayesian Independent Samples T-Test Report
# ------------------------------------------------------------------------------
# Target:      jaspTTests::TTestBayesianIndependentSamples
# Version:     >=0.20.0
# Description: Evidence-strength report using Bayes factors with effect size
#              and robustness interpretation. Designed to be readable by
#              non-statisticians.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Plan: read initial options -> mapped options + flow decisions
# ------------------------------------------------------------------------------

plan_report <- function(opts) {

  dependent_vars <- opts$dependent$value
  group_var      <- opts$group$value
  hypothesis     <- opts$alternative

  # Map: merge user's options with what the report needs.
  # Preserve dependent, group, alternative, prior, test type, naAction.
  report_opts <- modifyList(opts, list(
    descriptives                        = TRUE,
    priorAndPosteriorPlot               = TRUE,
    priorAndPosteriorPlotAdditionalInfo = TRUE,
    bfRobustnessPlot                    = TRUE,
    bfRobustnessPlotAdditionalInfo      = TRUE
  ))

  flow <- list(
    dependent_vars = dependent_vars,
    group_var      = group_var,
    hypothesis     = hypothesis,
    prior_width    = opts$priorWidth,
    bf_type        = opts$bayesFactorType
  )

  list(options = report_opts, flow = flow)
}

# ------------------------------------------------------------------------------
# 2. Extract: read results from the RDS
# ------------------------------------------------------------------------------

get_results <- function(analysisId) {
  raw <- rr_results(analysisId)

  # Main BF table — one row per dependent variable
  ttest_container <- rr_get(raw, "ttestContainer")
  main <- rr_select(rr_get(ttest_container, "ttestTable"), c("variable", "BF", "error"))

  # Descriptives (optional)
  desc_container <- rr_get(raw, "descriptivesContainer")
  descriptives <- rr_select(
    rr_get(desc_container, "table"),
    c("variable", "group", "N", "mean", "sd", "se")
  )

  # Inferential plots — each dependent variable has a sub-collection with
  # the prior-and-posterior plot and the robustness plot. Their $export
  # fields carry the machine-readable computed values.
  plots <- list()
  plots_container <- rr_get(ttest_container, "inferentialPlots")
  if (!is.null(plots_container)) {
    for (nm in names(plots_container)) {
      var_plots <- plots_container[[nm]]
      plots[[nm]] <- list(
        prior_posterior = rr_get(var_plots, "priorAndPosteriorPlot"),
        robustness       = rr_get(var_plots, "robustnessPlot")
      )
    }
  }

  list(main = main, descriptives = descriptives, plots = plots)
}

# ------------------------------------------------------------------------------
# 3. Build: compose annotation elements
# ------------------------------------------------------------------------------

# Convert a BF value from the user's chosen type to BF10, which is the
# canonical input for bf_evidence_category().
to_bf10 <- function(bf, bf_type) {
  bf <- suppressWarnings(as.numeric(bf))
  if (is.na(bf)) return(NA_real_)
  switch(bf_type,
    "BF10"    = bf,
    "BF01"    = 1 / bf,
    "LogBF10" = exp(bf),
    bf
  )
}

# Synthesize an overall conclusion from the per-variable BF results.
build_conclusion <- function(var_infos, flow) {
  n     <- length(var_infos)
  n_h0  <- sum(vapply(var_infos, function(x) x$direction == "H0", logical(1)))
  n_h1  <- sum(vapply(var_infos, function(x) x$direction == "H1", logical(1)))
  n_na  <- n - n_h0 - n_h1

  vars_h0 <- names(var_infos)[vapply(var_infos, function(x) x$direction == "H0", logical(1))]
  vars_h1 <- names(var_infos)[vapply(var_infos, function(x) x$direction == "H1", logical(1))]

  fmt_vars <- function(v) paste0("`", v, "`", collapse = ", ")

  conclusion <- "#### Conclusion\n"

  if (n_h0 == n) {
    conclusion <- paste0(conclusion, glue::glue(
      "Across all {n} outcome variable{if (n > 1) 's' else ''}, the data provide ",
      "evidence **against** a group difference. "
    ))
    strongest <- var_infos[[which.max(vapply(var_infos, function(x) x$bf, numeric(1)))]]
    conclusion <- paste0(conclusion, glue::glue(
      "The strongest evidence was observed for `{names(var_infos)[which.max(vapply(var_infos, function(x) x$bf, numeric(1)))]}` ",
      "({strongest$bf_label}, {strongest$category} evidence). "
    ))
    conclusion <- paste0(conclusion,
      "Unlike a non-significant p-value, this Bayesian result actively supports ",
      "the absence of a meaningful effect, suggesting that additional data ",
      "collection for these variables is unlikely to reveal a practically ",
      "significant group difference."
    )
  } else if (n_h1 == n) {
    conclusion <- paste0(conclusion, glue::glue(
      "Across all {n} outcome variable{if (n > 1) 's' else ''}, the data provide ",
      "evidence **for** a group difference. "
    ))
    strongest <- var_infos[[which.max(vapply(var_infos, function(x) x$bf, numeric(1)))]]
    conclusion <- paste0(conclusion, glue::glue(
      "The strongest evidence was observed for `{names(var_infos)[which.max(vapply(var_infos, function(x) x$bf, numeric(1)))]}` ",
      "({strongest$bf_label}, {strongest$category} evidence). "
    ))
    conclusion <- paste0(conclusion,
      "These results suggest that the grouping variable has a measurable effect ",
      "on the outcome(s), and the observed differences are unlikely to be due ",
      "to chance alone."
    )
  } else if (n_h0 > 0 && n_h1 > 0) {
    conclusion <- paste0(conclusion, glue::glue(
      "The evidence is **mixed** across the outcome variables. "
    ))
    if (n_h0 > 0)
      conclusion <- paste0(conclusion, glue::glue(
        "For {fmt_vars(vars_h0)}, the data favor the null hypothesis ",
        "(no group difference). "
      ))
    if (n_h1 > 0)
      conclusion <- paste0(conclusion, glue::glue(
        "For {fmt_vars(vars_h1)}, the data favor the alternative hypothesis ",
        "(a group difference is present). "
      ))
    conclusion <- paste0(conclusion,
      "This pattern suggests the grouping variable affects some outcomes but ",
      "not others, and each variable should be interpreted on its own merits."
    )
  } else {
    conclusion <- paste0(conclusion,
      "The Bayes factors could not be conclusively interpreted for one or more ",
      "variables. No firm conclusion can be drawn."
    )
  }

  conclusion
}

build_elements <- function(data, opts, flow) {
  m    <- data$main
  desc <- data$descriptives

  # ==========================================================================
  # Abstract
  # ==========================================================================

  n_vars <- length(flow$dependent_vars)
  var_list <- paste0("`", flow$dependent_vars, "`", collapse = ", ")

  n_var_text <- if (n_vars == 1) "the continuous outcome variable"
                else sprintf("%d continuous outcome variables", n_vars)

  hypothesis_text <- switch(flow$hypothesis,
    "twoSided" = "the two-sided alternative hypothesis",
    "greater"  = "the one-sided alternative hypothesis (group 1 > group 2)",
    "less"     = "the one-sided alternative hypothesis (group 1 < group 2)",
    "the alternative hypothesis"
  )

  abstract <- glue::glue(
    "## Abstract\n\n",
    "A Bayesian independent samples t-test was conducted to examine whether two ",
    "groups (defined by the variable `{flow$group_var}`) differ on ",
    "{n_var_text}: {var_list}. ",
    "A Cauchy prior (r = {sprintf('%.3f', flow$prior_width)}) was specified for the ",
    "standardized effect size under {hypothesis_text}. ",
    "The analysis aimed to quantify evidence for or against a group difference ",
    "using the Bayes factor."
  )

  # ==========================================================================
  # BF primer + prior specification
  # ==========================================================================

  primer <- paste(
    "#### What is a Bayes Factor?\n",
    "The **Bayes factor (BF)** measures how much the observed data support one",
    "hypothesis over another. A BF\u2081\u2080 greater than 1 means the data are more",
    "likely under the alternative hypothesis (a real difference between groups);",
    "a BF\u2081\u2080 less than 1 means the data are more likely under the null",
    "hypothesis (no difference). Unlike a p-value, the BF can provide **evidence",
    "for the null** as well as against it.\n",
    "The strength of evidence is commonly interpreted using **Jeffreys' (1961)**",
    "categories**: BF 1\u20133 is *anecdotal*, 3\u201310 is *moderate*, 10\u201330",
    "is *strong*, 30\u2013100 is *very strong*, and >100 is *extreme*.\n",
    glue::glue(
      "All tests use a **Cauchy prior** on the standardized effect size with width ",
      "r = {sprintf('%.3f', flow$prior_width)} (selected in the input panel). This expresses the ",
      "expected magnitude of the effect *before* seeing the data: smaller widths ",
      "reflect a skeptical viewpoint (expecting small effects), larger widths a ",
      "more open-minded one."
    )
  )

  # ==========================================================================
  # Main table interpretation
  # ==========================================================================

  interp_parts <- character()
  var_infos    <- list()  # per-variable BF info for the conclusion

  for (i in seq_len(nrow(m))) {
    var    <- m$variable[i]
    bf_raw <- m$BF[i]
    err    <- m$error[i]

    bf10 <- to_bf10(bf_raw, flow$bf_type)
    info <- bf_evidence_category(bf10)
    var_infos[[var]] <- info

    if (info$direction == "H1") {
      interp_parts <- c(interp_parts, glue::glue(
        "For `{var}`, {info$bf_label}, constituting **{info$category} evidence** ",
        "that the groups differ. The data are approximately ",
        "**{sprintf('%.1f', info$bf)} times more likely** under a model assuming a ",
        "real group difference than under a model assuming no difference."
      ))
    } else if (info$direction == "H0") {
      interp_parts <- c(interp_parts, glue::glue(
        "For `{var}`, {info$bf_label}, constituting **{info$category} evidence** ",
        "that the groups do **not** differ. The data are approximately ",
        "**{sprintf('%.1f', info$bf)} times more likely** under a model assuming ",
        "no group difference than under a model assuming a real difference."
      ))
    } else {
      interp_parts <- c(interp_parts, glue::glue(
        "For `{var}`, the Bayes factor could not be interpreted (BF = NA)."
      ))
    }

    # Monte Carlo error caveat
    if (!is.na(err) && err > 0.05) {
      interp_parts <- c(interp_parts, glue::glue(
        "*Note:* the Bayes factor for `{var}` was estimated via Monte Carlo ",
        "integration with an error of {sprintf('%.2f', err * 100)}%. Larger values ",
        "indicate the BF estimate is somewhat noisy."
      ))
    }
  }

  main_interp <- paste(
    "**Interpretation of the main table.**",
    paste(interp_parts, collapse = " ")
  )

  # ==========================================================================
  # Build interleaved elements: prose and per-variable plots alternate
  # ==========================================================================

  elements <- list()

  # 1. Abstract
  elements <- c(elements, list(el_md(abstract)))

  # 2. Main BF table
  elements <- c(elements, list(el_ref("ttestContainer_ttestTable")))

  # 3. BF primer + main table interpretation
  elements <- c(elements, list(el_md(paste(primer, main_interp, sep = "\n\n"))))

  # 4. Per-variable sections: heading + text, plot, text, plot, text ...
  for (i in seq_len(nrow(m))) {
    var <- m$variable[i]

    var_plots <- rr_get(data$plots, var)
    pp_export  <- if (!is.null(var_plots)) rr_get(var_plots$prior_posterior, "export") else NULL
    rob_export <- if (!is.null(var_plots)) rr_get(var_plots$robustness,       "export") else NULL

    has_pp  <- !is.null(var_plots) && !is.null(var_plots$prior_posterior)
    has_rob <- !is.null(var_plots) && !is.null(var_plots$robustness)

    # --- Effect size / CI from the prior-posterior export ---
    effect_intro <- NULL
    ci_interp    <- NULL

    if (!is.null(pp_export) && !is.na(pp_export$medianDelta)) {
      d_med <- pp_export$medianDelta
      d_lo  <- pp_export$ciLow
      d_hi  <- pp_export$ciHigh
      level <- pp_export$ciLevel

      magnitude <- if (abs(d_med) < 0.2) "negligible"
                   else if (abs(d_med) < 0.5) "small"
                   else if (abs(d_med) < 0.8) "medium"
                   else "large"

      ci_crosses_zero <- is.na(d_lo) || is.na(d_hi) || (d_lo < 0 && d_hi > 0)

      effect_intro <- glue::glue(
        "The **posterior median** standardized effect size (\u03b4, similar to ",
        "Cohen's d) is **{sprintf('%.2f', d_med)}** ({magnitude} magnitude). ",
        "The {sprintf('%.0f', level * 100)}% credible interval runs from ",
        "**{sprintf('%.2f', d_lo)}** to **{sprintf('%.2f', d_hi)}**."
      )

      ci_interp <- if (ci_crosses_zero) {
        paste(
          "Because the credible interval includes zero, the data are compatible",
          "with both a meaningful effect and essentially no effect \u2014 the",
          "uncertainty about the effect's direction and size is substantial."
        )
      } else {
        paste(
          "The credible interval excludes zero, so the data point toward a",
          "non-negligible effect whose sign is consistent."
        )
      }
    }

    # --- Per-variable plot collection (renders plots side-by-side) ---
    if (has_pp || has_rob) {
      coll_name <- paste0("ttestContainer_inferentialPlots_", var)
      elements <- c(elements, list(el_ref(coll_name)))
    }

    # --- All interpretation after the plots ---
    post_parts <- character()

    if (!is.null(effect_intro))
      post_parts <- c(post_parts, effect_intro)

    if (!is.null(ci_interp))
      post_parts <- c(post_parts, ci_interp)

    if (!is.null(rob_export)) {
      user_bf <- rob_export$userPriorBF
      max_bf  <- rob_export$maxBF
      max_r   <- rob_export$maxBFPriorWidth

      ref_bfs <- c(
        medium    = rob_export$mediumPriorBF,
        wide      = rob_export$widePriorBF,
        ultrawide = rob_export$ultrawidePriorBF
      )
      ref_bfs <- ref_bfs[!is.na(ref_bfs)]

      robust_stmt <- ""
      if (length(ref_bfs) >= 2) {
        all_h1  <- all(ref_bfs > 1)
        all_h0  <- all(ref_bfs < 1)
        user_h1 <- !is.na(user_bf) && user_bf > 1

        if ((all_h1 && user_h1) || (all_h0 && !user_h1)) {
          robust_stmt <- paste(
            "The conclusion is **robust** to the choice of prior: across a range",
            "of reasonable prior widths (medium r = 0.707, wide r = 1, ultrawide",
            "r = 1.414), the Bayes factor consistently points in the same direction."
          )
        } else {
          robust_stmt <- paste(
            "The conclusion is **somewhat sensitive** to the prior: different",
            "reasonable prior widths yield Bayes factors that differ in magnitude",
            "or direction. The evidence should be interpreted with this in mind."
          )
        }
      }

      if (!is.na(max_bf) && !is.na(max_r)) {
        max_info <- bf_evidence_category(max_bf)
        direction_txt <- if (max_info$direction == "H1") "for a difference" else "for no difference"
        post_parts <- c(post_parts, glue::glue(
          "The **robustness check** shows the Bayes factor as a function of the ",
          "Cauchy prior width. The strongest evidence {direction_txt} peaks at a ",
          "prior width of **r = {sprintf('%.3f', max_r)}** with ",
          "BF = {sprintf('%.3f', max_bf)}. {robust_stmt}"
        ))
      }
    }

    if (!is.null(desc) && var %in% desc$variable) {
      var_desc <- desc[desc$variable == var, , drop = FALSE]
      if (nrow(var_desc) == 2) {
        post_parts <- c(post_parts, glue::glue(
          "For context, the two groups have means of ",
          "**{sprintf('%.2f', var_desc$mean[1])}** (n = {var_desc$N[1]}) and ",
          "**{sprintf('%.2f', var_desc$mean[2])}** (n = {var_desc$N[2]}), ",
          "a raw difference of ",
          "**{sprintf('%.2f', var_desc$mean[1] - var_desc$mean[2])}**."
        ))
      }
    }

    if (length(post_parts) > 0)
      elements <- c(elements, list(el_md(paste(post_parts, collapse = "\n\n"))))
  }

  # 5. Descriptives table
  if (!is.null(data$descriptives))
    elements <- c(elements, list(el_ref("descriptivesContainer")))

  # 6. Overall conclusion
  conclusion <- build_conclusion(var_infos, flow)
  elements <- c(elements, list(el_md(conclusion)))

  elements
}

# ------------------------------------------------------------------------------
# 4. Entry point - required by the RoboReport contract
# ------------------------------------------------------------------------------

roboreport_main <- function(analysisId) {

  state <- rr_get_analyses_state(as.integer(analysisId),
                                 include_options = TRUE,
                                 options_meta_diff = FALSE)
  if (length(state$analyses) == 0)
    stop("Analysis not found: ", analysisId, call. = FALSE)

  src <- state$analyses[[1]]
  initial_opts <- src$options

  plan <- plan_report(initial_opts)

  sibling_id <- rr_create_and_run(
    module   = src$module,
    analysis = src$name,
    options  = plan$options
  )

  data <- get_results(sibling_id)

  elements <- build_elements(data, initial_opts, plan$flow)
  rr_compose_results(sibling_id, elements)

  invisible(NULL)
}
