context("Informed Bayesian T-Test Integration")

test_that("Negligible likelihood tails do not abort informed one-sided tests", {
  # At t = -2, direct noncentral-t integration is an independent reference.
  # Narrow centered and shifted priors triggered divergence in a subnormal Q
  # tail; negative locations also exercise normalization of a truncated prior.
  for (prior.df in c(1, 3)) {
    for (location in c(-0.5, 0, 0.5)) {
      scale <- if (location == 0) 0.01 else 0.707
      reference <- integrate(function(delta) {
        suppressWarnings(dt(-2, df = 1998, ncp = sqrt(500) * delta)) *
          dt((delta - location)/scale, df = prior.df) /
          (scale * pt(location/scale, df = prior.df) * dt(-2, df = 1998))
      }, 0, Inf, rel.tol = 1e-9, abs.tol = 0, subdivisions = 1000L)$value

      for (direction in c(-1, 1)) {
        result <- jaspTTests::.bf10_t(
          t = -2 * direction, n1 = 1000, n2 = 1000,
          oneSided = if (direction == 1) "right" else "left",
          independentSamples = TRUE, prior.location = direction * location,
          prior.scale = scale, prior.df = prior.df)
        # R's noncentral-t density loses some precision in the far tail.
        expect_equal(result$bf, reference, tolerance = 1e-7)
        expect_true(is.finite(result$error) && result$error >= 0 && result$error < 1e-6)
      }
    }
  }
})

test_that("Extreme opposing effects retain the full likelihood mass", {
  # Independent one-dimensional reference for a centered Student-t prior:
  # delta | v ~ N(0, df*scale^2/v), v ~ chi-squared(df). Integrating delta
  # analytically gives a scaled-t density and a t CDF for its positive sign.
  mixtureReference <- function(t, neff, nu, scale, df) {
    integrate(function(v) {
      ng <- neff * df * scale^2/v
      exp(log(2) - 0.5*log1p(ng) +
            dt(t/sqrt(1 + ng), df = nu, log = TRUE) - dt(t, df = nu, log = TRUE) +
            pt(t*sqrt(ng)*sqrt((nu + 1)/(nu*(1 + ng) + t^2)),
               df = nu + 1, log.p = TRUE) +
            dchisq(v, df = df, log = TRUE))
    }, 0, Inf, rel.tol = 1e-8, abs.tol = 0, subdivisions = 1000L)$value
  }

  for (t in c(-100, -1000)) {
    for (scale in c(2, 5)) {
      for (prior.df in c(1, 3)) {
        reference <- mixtureReference(t, 5000, 19998, scale, prior.df)
        result <- jaspTTests::.bf10_t(
          t = t, n1 = 10000, n2 = 10000, oneSided = "right",
          independentSamples = TRUE, prior.location = 0,
          prior.scale = scale, prior.df = prior.df)
        # Compare relative errors: these Bayes factors are much smaller than 1.
        expect_equal(result$bf/reference, 1, tolerance = 1e-8)
        expect_true(is.finite(result$error) && result$error >= 0 && result$error < 1e-6)
      }
    }
  }
})
