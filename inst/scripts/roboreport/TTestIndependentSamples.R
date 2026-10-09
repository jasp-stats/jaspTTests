# ==============================================================================
# RoboReport Script: Independent Samples T-Test Report
# ------------------------------------------------------------------------------
# Target:      jaspTTests::TTestIndependentSamples
# Version:     >=0.20.0
# Description: Assumption-driven report with test selection rationale,
#              effect size interpretation, and practical significance flags.
# ==============================================================================
#
# This script generates an annotated report for an Independent Samples T-Test.
# It is sourced and called by jaspRoboReport::run_script(), which invokes
# roboreport_main(analysisId).
#
# Pipeline:
#   1. Read the source analysis's initial options.
#   2. Plan the report: map options (enable all tests, assumptions, etc.).
#   3. Create a sibling analysis with the mapped options and run it.
#   4. Extract results from the sibling's RDS.
#   5. Build annotation elements (prose interleaved with result references).
#   6. Commit as an annotation on the sibling.
#
# Output layout (mirrors the reference annotation):
#   Abstract
#   Descriptives            (table + raincloud plots, via collection ref)
#   Descriptive Statistics  (prose)
#   Assumption Checks       (normality + equality-of-variance tables, via ref)
#   Assumption Checks & Test Selection (prose + rationale)
#   Inferential Tests       (main table + per-variable primary/supplementary prose)
#   Conclusion              (prose)

# ------------------------------------------------------------------------------
# Small numeric/formatting helpers
# ------------------------------------------------------------------------------

# Coerce to number defensively (the RDS stores "" for Mann-Whitney df/sed).
num <- function(x) suppressWarnings(as.numeric(x))

# Cohen-style magnitude label for an effect size (also used for rank biserial).
es_magnitude <- function(d) {
  d <- num(d)
  if (is.na(d)) return(NA_character_)
  ad <- abs(d)
  if (ad < 0.2) "negligible"
  else if (ad < 0.5) "small"
  else if (ad < 0.8) "medium"
  else "large"
}

# The effect-size metric depends on the test.
es_metric <- function(test_name) {
  if (identical(test_name, "Mann-Whitney")) "rank biserial correlation" else "Cohen's d"
}

# Human-readable test name for prose.
test_display <- function(test_name) {
  switch(test_name,
    "Mann-Whitney" = "Mann-Whitney U test",
    "Welch"        = "Welch's t-test",
    "Student"      = "Student's t-test",
    test_name)
}

# Statistic string: U for Mann-Whitney, t(df) otherwise.
stat_string <- function(row, test_name) {
  stat <- num(row$statistic[1])
  dfv  <- num(row$df[1])
  if (identical(test_name, "Mann-Whitney")) sprintf("U = %.0f", stat)
  else fmt_stat(stat, dfv, stat_name = "t")
}

# Full result sentence for a primary analysis row.
format_result_sentence <- function(row, test_name) {
  pv  <- num(row$p[1])
  d   <- num(row$d[1])
  dlo <- num(row$lowerCIeffectSize[1])
  dhi <- num(row$upperCIeffectSize[1])
  md  <- num(row$md[1])
  mdlo <- num(row$lowerCIlocationParameter[1])
  mdhi <- num(row$upperCIlocationParameter[1])

  eff <- ""
  if (!is.na(d)) {
    ci <- if (!is.na(dlo) && !is.na(dhi)) sprintf(", 95%% CI [%.2f, %.2f]", dlo, dhi) else ""
    eff <- sprintf(" The effect size (%s) was %.2f (%s%s).",
                   es_metric(test_name), d, es_magnitude(d), ci)
  }

  loc <- ""
  if (!identical(test_name, "Mann-Whitney") && !is.na(md)) {
    mdci <- if (!is.na(mdlo) && !is.na(mdhi)) sprintf(", 95%% CI [%.2f, %.2f]", mdlo, mdhi) else ""
    loc <- sprintf(" The mean difference was %.2f%s.", md, mdci)
  }

  sig_txt <- if (!is.na(pv) && pv < 0.05) " This result is statistically significant."
             else " This result is not statistically significant."

  paste0(stat_string(row, test_name), ", ", fmt_p(pv), ".", eff, loc, sig_txt)
}

# Brief result fragment for a supplementary analysis row.
format_brief <- function(row, test_name) {
  sprintf("%s: %s, %s", test_name, stat_string(row, test_name), fmt_p(num(row$p[1])))
}

# ------------------------------------------------------------------------------
# 1. Plan: read initial options -> mapped options + flow decisions
# ------------------------------------------------------------------------------

plan_report <- function(opts) {

  dependent_vars <- opts$dependent$value
  group_var      <- opts$group$value
  hypothesis     <- opts$alternative

  eq_var_type <- if (is.null(opts$equalityOfVariancesTestType)) "brownForsythe"
                 else opts$equalityOfVariancesTestType
  eq_var_label <- if (identical(eq_var_type, "levene")) "Levene's" else "Brown-Forsythe"

  # Map: merge user's options with what the report needs.
  # The user's dependent, group, alternative, naAction are preserved.
  report_opts <- modifyList(opts, list(
    # Enable all three test types for comprehensive comparison
    student      = TRUE,
    welch        = TRUE,
    mannWhitneyU = TRUE,
    # Effect sizes
    effectSize   = TRUE,
    effectSizeCi = TRUE,
    # Mean difference + CI
    meanDifference   = TRUE,
    meanDifferenceCi = TRUE,
    # Assumption checks
    normalityTest             = TRUE,
    equalityOfVariancesTest   = TRUE,
    # Descriptives table + raincloud plots
    descriptives   = TRUE,
    raincloudPlot  = TRUE
  ))

  flow <- list(
    dependent_vars = dependent_vars,
    group_var      = group_var,
    hypothesis     = hypothesis,
    eq_var_label   = eq_var_label
  )

  list(options = report_opts, flow = flow)
}

# ------------------------------------------------------------------------------
# 2. Extract: read results from the RDS
# ------------------------------------------------------------------------------

get_results <- function(analysisId) {
  raw <- rr_results(analysisId)

  # The statistic column is named "t" when only Student is enabled,
  # but "Statistic" when multiple test types are enabled. Select whichever.
  stat_col <- if ("Statistic" %in% names(raw$ttest)) "Statistic" else "t"
  main <- rr_select(raw$ttest, c(
    "v", "test", stat_col, "df", "p",
    "md", "sed",
    "d", "effectSizeSe",
    "lowerCIlocationParameter", "upperCIlocationParameter",
    "lowerCIeffectSize", "upperCIeffectSize"
  ))
  names(main)[names(main) == stat_col] <- "statistic"

  # Descriptives table (key "table" inside "ttestDescriptives")
  desc_container <- rr_get(raw, "ttestDescriptives")
  descriptives <- rr_select(
    rr_get(desc_container, "table"),
    c("variable", "group", "N", "mean", "sd", "se", "meanRank")
  )

  # Assumption checks
  assumptions <- rr_get(raw, "AssumptionChecks")
  normality <- rr_select(
    rr_get(assumptions, "ttestNormalTable"),
    c("dep", "W", "p")
  )
  variance <- rr_select(
    rr_get(assumptions, "equalityVariance"),
    c("variable", "fStat", "dfOne", "dfTwo", "p")
  )

  list(
    main         = main,
    descriptives = descriptives,
    normality    = normality,
    variance     = variance
  )
}

# ------------------------------------------------------------------------------
# 3. Decide: based on assumption checks, pick the appropriate test per variable
# ------------------------------------------------------------------------------

# Returns a data.frame with columns: variable, recommended, reason,
# normality_ok (lgl), variances_equal (lgl)
decide_tests <- function(data) {
  vars <- unique(data$main$v)
  results <- data.frame(
    variable        = vars,
    recommended     = NA_character_,
    reason          = NA_character_,
    normality_ok    = NA,
    variances_equal = NA,
    stringsAsFactors = FALSE
  )

  for (i in seq_along(vars)) {
    v <- vars[i]

    normality_ok <- TRUE
    if (!is.null(data$normality) && v %in% data$normality$dep) {
      p_norm <- num(data$normality$p[data$normality$dep == v][1])
      normality_ok <- !is.na(p_norm) && p_norm >= 0.05
    }

    variances_equal <- TRUE
    if (!is.null(data$variance) && v %in% data$variance$variable) {
      p_var <- num(data$variance$p[data$variance$variable == v][1])
      variances_equal <- !is.na(p_var) && p_var >= 0.05
    }

    #   Normality violated            -> Mann-Whitney (non-parametric)
    #   Normality OK, unequal vars    -> Welch (robust)
    #   Normality OK, equal vars      -> Student (most powerful)
    if (!normality_ok) {
      recommended <- "Mann-Whitney"; reason <- "normality violated"
    } else if (!variances_equal) {
      recommended <- "Welch";        reason <- "variances unequal"
    } else {
      recommended <- "Student";      reason <- "assumptions met"
    }

    results$recommended[i]     <- recommended
    results$reason[i]          <- reason
    results$normality_ok[i]    <- normality_ok
    results$variances_equal[i] <- variances_equal
  }

  results
}

# ------------------------------------------------------------------------------
# 4. Prose builders
# ------------------------------------------------------------------------------

build_abstract <- function(data, decisions, flow) {
  n_vars   <- length(flow$dependent_vars)
  var_list <- paste0("**", flow$dependent_vars, "**", collapse = " and ")

  desc   <- data$descriptives
  groups <- unique(desc$group)
  v1     <- flow$dependent_vars[1]
  vd     <- desc[desc$variable == v1, , drop = FALSE]
  total_N <- sum(num(vd$N))

  group_desc <- if (length(groups) >= 2) sprintf(" (%s vs. %s; total N = %d)", groups[1], groups[2], total_N)
                else sprintf(" (total N = %d)", total_N)

  recs <- decisions$recommended
  test_sentence <- if (all(recs == "Mann-Whitney")) {
    "Because the Shapiro-Wilk test indicated violations of normality, the non-parametric Mann-Whitney U test is the primary analysis; Student and Welch t-tests are reported as supplementary."
  } else if (all(recs == "Student")) {
    "Because the assumptions of normality and equal variances were met, Student's t-test is the primary analysis; Welch's and the Mann-Whitney tests are reported as supplementary."
  } else if (all(recs == "Welch")) {
    "Because normality held but variances were unequal, Welch's t-test is the primary analysis; Student's and the Mann-Whitney tests are reported as supplementary."
  } else {
    "Test selection was guided by the assumption checks below; the recommended (primary) test varies by variable, with alternatives reported as supplementary."
  }

  n_var_txt <- if (n_vars > 1) sprintf("%d continuous variables", n_vars) else "a continuous variable"

  glue::glue(
    "## Abstract\n\n",
    "This analysis compares {n_var_txt} \u2014 {var_list} \u2014 between two independent ",
    "groups defined by **{flow$group_var}**{group_desc}. ", 
    "Independent-samples tests were used to assess group differences. {test_sentence} ",
    "Equality of variances was assessed with the {flow$eq_var_label} test. ",
    "Descriptive statistics and raincloud plots summarise the group distributions."
  )
}

build_descriptives_prose <- function(data, flow) {
  desc <- data$descriptives
  parts <- c(
    "### Descriptive Statistics",
    "The Group Descriptives table reports sample sizes, means, and standard deviations for each variable by group; the raincloud plots above show the full distributional shape (density, box, and individual points)."
  )
  for (v in flow$dependent_vars) {
    vd <- desc[desc$variable == v, , drop = FALSE]
    if (nrow(vd) >= 2) {
      parts <- c(parts, sprintf(
        "For **%s**, the %s group (n = %d) had a mean of %.2f (SD = %.2f) and the %s group (n = %d) a mean of %.2f (SD = %.2f).",
        v,
        vd$group[1], vd$N[1], num(vd$mean[1]), num(vd$sd[1]),
        vd$group[2], vd$N[2], num(vd$mean[2]), num(vd$sd[2])
      ))
    }
  }
  paste(parts, collapse = "\n\n")
}

build_assumption_prose <- function(data, decisions, flow) {
  parts <- c("### Assumption Checks & Test Selection")

  if (!is.null(data$normality) && nrow(data$normality) > 0) {
    lines <- vapply(seq_len(nrow(data$normality)), function(i) {
      dep <- data$normality$dep[i]
      W   <- num(data$normality$W[i])
      p   <- num(data$normality$p[i])
      if (is.na(p))
        sprintf("- **%s**: W = %.3f, %s.", dep, W, fmt_p(p))
      else if (p < 0.05)
        sprintf("- **%s**: W = %.3f, %s \u2014 normality rejected (residuals deviate from normal).", dep, W, fmt_p(p))
      else
        sprintf("- **%s**: W = %.3f, %s \u2014 no evidence against normality.", dep, W, fmt_p(p))
    }, character(1))
    parts <- c(parts, "**Normality (Shapiro-Wilk):**", paste(lines, collapse = "\n"))
  }

  if (!is.null(data$variance) && nrow(data$variance) > 0) {
    lines <- vapply(seq_len(nrow(data$variance)), function(i) {
      v   <- data$variance$variable[i]
      Fv  <- num(data$variance$fStat[i])
      df1 <- num(data$variance$dfOne[i])
      df2 <- num(data$variance$dfTwo[i])
      p   <- num(data$variance$p[i])
      if (is.na(p))
        sprintf("- **%s**: F(%g, %g) = %.3f, %s.", v, df1, df2, Fv, fmt_p(p))
      else if (p < 0.05)
        sprintf("- **%s**: F(%g, %g) = %.3f, %s \u2014 equal variances rejected (heteroscedasticity).", v, df1, df2, Fv, fmt_p(p))
      else
        sprintf("- **%s**: F(%g, %g) = %.3f, %s \u2014 equal variances tenable.", v, df1, df2, Fv, fmt_p(p))
    }, character(1))
    parts <- c(parts,
      sprintf("**Equality of variances (%s):**", flow$eq_var_label),
      paste(lines, collapse = "\n"))
  }

  rat <- vapply(seq_len(nrow(decisions)), function(i) {
    v      <- decisions$variable[i]
    reason <- decisions$reason[i]
    expl <- switch(reason,
      "normality violated" = "normality was violated, so the non-parametric Mann-Whitney U test is primary (it does not assume normal residuals)",
      "variances unequal"  = "normality held but variances were unequal, so Welch's t-test is primary (robust to heteroscedasticity)",
      "assumptions met"    = "normality and equal variances both held, so Student's t-test is primary (most powerful)")
    sprintf("- **%s**: %s.", v, expl)
  }, character(1))
  parts <- c(parts, "**Test selection rationale:**", paste(rat, collapse = "\n"))

  paste(parts, collapse = "\n\n")
}

# Per-variable inferential prose: primary analysis + supplementary results.
build_var_inference <- function(v, data, decisions) {
  dec    <- decisions[decisions$variable == v, , drop = FALSE]
  rec    <- dec$recommended[1]
  reason <- dec$reason[1]
  m      <- data$main

  # Cite the driving assumption statistic in the selection reason.
  reason_phrase <- switch(reason,
    "normality violated" = "normality was violated",
    "variances unequal"  = "variances were unequal",
    "assumptions met"    = "all assumptions were met")
  reason_cite <- ""
  if (identical(reason, "normality violated") && !is.null(data$normality)) {
    nr <- data$normality[data$normality$dep == v, , drop = FALSE]
    if (nrow(nr) > 0)
      reason_cite <- sprintf(", W = %.3f, %s", num(nr$W[1]), fmt_p(num(nr$p[1])))
  } else if (identical(reason, "variances unequal") && !is.null(data$variance)) {
    vr <- data$variance[data$variance$variable == v, , drop = FALSE]
    if (nrow(vr) > 0)
      reason_cite <- sprintf(", F(%g, %g) = %.3f, %s",
                             num(vr$dfOne[1]), num(vr$dfTwo[1]), num(vr$fStat[1]), fmt_p(num(vr$p[1])))
  }

  primary_row <- m[m$v == v & m$test == rec, , drop = FALSE]
  primary_sentence <- if (nrow(primary_row) > 0) format_result_sentence(primary_row, rec) else ""

  supp_tests <- setdiff(c("Student", "Welch", "Mann-Whitney"), rec)
  supp_briefs <- character()
  for (tn in supp_tests) {
    r <- m[m$v == v & m$test == tn, , drop = FALSE]
    if (nrow(r) > 0) supp_briefs <- c(supp_briefs, format_brief(r, tn))
  }

  parts <- sprintf("#### %s", v)
  parts <- c(parts, sprintf(
    "**Primary analysis \u2014 %s** (selected because %s%s): %s",
    test_display(rec), reason_phrase, reason_cite, primary_sentence))
  if (length(supp_briefs) > 0)
    parts <- c(parts, sprintf(
      "**Supplementary** (reported for completeness): %s.",
      paste(supp_briefs, collapse = "; ")))

  paste(parts, collapse = "\n\n")
}

build_conclusion <- function(data, decisions, flow) {
  m      <- data$main
  desc   <- data$descriptives
  groups <- unique(desc$group)

  recs <- unique(decisions$recommended)
  lead <- if (length(recs) == 1)
    sprintf("The primary analysis was the %s, selected via the assumption checks above.", test_display(recs[1]))
  else
    "The primary analysis varied by variable, selected via the assumption checks above."

  sentences <- character()
  for (i in seq_len(nrow(decisions))) {
    v   <- decisions$variable[i]
    rec <- decisions$recommended[i]
    row <- m[m$v == v & m$test == rec, , drop = FALSE]
    if (nrow(row) == 0) next

    pv   <- num(row$p[1])
    d    <- num(row$d[1])
    md   <- num(row$md[1])
    mag  <- es_magnitude(d)
    mag_txt <- if (is.na(mag)) "" else sprintf(", %s effect", mag)
    stat_str <- stat_string(row, rec)
    sig <- !is.na(pv) && pv < 0.05

    if (sig) {
      dir_txt <- ""
      if (length(groups) >= 2 && !is.na(md)) {
        dir_txt <- if (md < 0) sprintf(", with %s lower than %s", groups[1], groups[2])
                   else sprintf(", with %s higher than %s", groups[1], groups[2])
      }
      sentences <- c(sentences, sprintf(
        "A statistically significant group difference was found for **%s** (%s, %s%s%s).",
        v, stat_str, fmt_p(pv), mag_txt, dir_txt))
    } else {
      sentences <- c(sentences, sprintf(
        "No significant group difference was detected for **%s** (%s, %s%s).",
        v, stat_str, fmt_p(pv), mag_txt))
    }
  }

  paste(c("## Conclusion", lead, paste(sentences, collapse = " ")), collapse = "\n\n")
}

# ------------------------------------------------------------------------------
# 5. Build: compose annotation elements
# ------------------------------------------------------------------------------

build_elements <- function(data, opts, flow) {
  decisions <- decide_tests(data)
  elements  <- list()

  # Abstract
  elements <- c(elements, list(el_md(build_abstract(data, decisions, flow))))

  # Descriptives: table + raincloud plots (collection renders its own headings)
  if (!is.null(data$descriptives))
    elements <- c(elements, list(el_ref("ttestDescriptives")))

  # Descriptive statistics prose
  elements <- c(elements, list(el_md(build_descriptives_prose(data, flow))))

  # Assumption check tables
  if (!is.null(data$normality) || !is.null(data$variance))
    elements <- c(elements, list(el_ref("AssumptionChecks")))

  # Assumption checks & test selection prose
  elements <- c(elements, list(el_md(build_assumption_prose(data, decisions, flow))))

  # Inferential tests: heading + main table
  elements <- c(elements, list(el_md("### Inferential Tests")))
  elements <- c(elements, list(el_ref("ttest")))

  # Per-variable primary/supplementary prose
  for (v in decisions$variable)
    elements <- c(elements, list(el_md(build_var_inference(v, data, decisions))))

  # Conclusion
  elements <- c(elements, list(el_md(build_conclusion(data, decisions, flow))))

  elements
}

# ------------------------------------------------------------------------------
# 6. Entry point - required by the RoboReport contract
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
