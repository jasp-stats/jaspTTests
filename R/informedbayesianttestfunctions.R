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
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#-------------------------------------------------------------------------------
# HELPER FUNCTIONS INFORMED BAYESIAN T-TESTS
#-------------------------------------------------------------------------------


.integrand_t <- function(delta, t, n, nu, mu.delta, gamma, kappa) {

  suppressWarnings(
    dt(x = t, df = nu, ncp = sqrt(n) * delta) *
    1 / gamma * dt( (delta - mu.delta) / gamma, df = kappa)
  )
}

.dtss <- function(delta, mu.delta, r, kappa, log = FALSE) {

  out <- - log(r) + lgamma((kappa + 1)/2) - .5*(log(pi) + log(kappa)) -
    lgamma(kappa/2) - (kappa + 1)/2 * log(1 + ((delta - mu.delta)/r)^2/kappa)

  if ( ! log)
    out <- exp(out)

  return(out)
}

.dprior_informative <- function(delta, oneSided = FALSE, options) {
  isDefault <- options[["effectSizeStandardized"]] == "default"

  if(isDefault) {
    cauchyLocation <- 0
    cauchyScale    <- options[["priorWidth"]]
  } else {
    cauchyLocation <- options[["informativeCauchyLocation"]]
    cauchyScale    <- options[["informativeCauchyScale"]]
  }

  if (options[["informativeStandardizedEffectSize"]] == "cauchy" || isDefault) {
    out <- .dtss(delta, mu.delta = cauchyLocation, r = cauchyScale, kappa = 1)

    if (oneSided == "right") {
      out <- ifelse(delta < 0, 0, out/integrate(.dtss, 0, Inf,
                                                mu.delta = cauchyLocation,
                                                r = cauchyScale,
                                                kappa = 1)$value)
    } else if (oneSided == "left") {
      out <- ifelse(delta > 0, 0, out/integrate(.dtss, -Inf, 0,
                                                mu.delta = cauchyLocation,
                                                r = cauchyScale,
                                                kappa = 1)$value)
    }
  } else if (options[["informativeStandardizedEffectSize"]] == "t") {
    out <- .dtss(delta,
                 mu.delta = options[["informativeTLocation"]],
                 r = options[["informativeTScale"]],
                 kappa = options[["informativeTDf"]])
    if (oneSided == "right") {
      out <- ifelse(delta < 0, 0, out/integrate(.dtss, 0, Inf,
                                                mu.delta = options[["informativeTLocation"]],
                                                r = options[["informativeTScale"]],
                                                kappa = options[["informativeTDf"]])$value)
    } else if (oneSided == "left") {
      out <- ifelse(delta > 0, 0, out/integrate(.dtss, -Inf, 0,
                                                mu.delta = options[["informativeTLocation"]],
                                                r = options[["informativeTScale"]],
                                                kappa = options[["informativeTDf"]])$value)
    }
  } else if (options[["informativeStandardizedEffectSize"]] == "normal") {
    out <- dnorm(delta, mean = options[["informativeNormalMean"]],
                 sd = options[["informativeNormalStd"]])
    if (oneSided == "right") {
      out <- ifelse(delta < 0, 0, out/pnorm(0, mean = options[["informativeNormalMean"]],
                                            sd = options[["informativeNormalStd"]],
                                            lower.tail = FALSE))
    } else if (oneSided == "left") {
      out <- ifelse(delta > 0, 0, out/pnorm(0, mean = options[["informativeNormalMean"]],
                                            sd = options[["informativeNormalStd"]],
                                            lower.tail = TRUE))
    }
  }

  out[out < 0] <- 0
  return(out)

}

.posterior_t <- function(delta, t, n1, n2 = NULL, independentSamples = FALSE, prior.location,
                         prior.scale, prior.df, rel.tol = .Machine$double.eps^0.25) {

  neff <- ifelse(independentSamples, n1 * n2 / (n1 + n2), n1)
  nu   <- ifelse(independentSamples, n1 + n2 - 2, n1 - 1)

  mu.delta  <- prior.location
  gamma     <- prior.scale
  kappa     <- prior.df

  numerator   <- suppressWarnings(
    dt(x = t, df = nu, ncp = sqrt(neff) * delta) *
    1 / gamma * dt( (delta - mu.delta) / gamma, df = kappa)
  )
  denominator <- integrate(.integrand_t, lower = -Inf, upper = Inf, t = t, n = neff, nu = nu,
                           mu.delta = mu.delta, gamma = gamma, kappa = kappa, rel.tol = rel.tol)$value
  out         <- numerator / denominator
  out[is.na(out)] <- 0

  return(out)
}

.cdf_t <- function(x, t, n1, n2 = NULL, independentSamples = FALSE, prior.location, prior.scale, prior.df) {

  integrate(.posterior_t, lower = -Inf, upper = x, t = t, n1 = n1, n2 = n2,
            independentSamples = independentSamples, prior.location = prior.location,
            prior.scale = prior.scale, prior.df = prior.df)$value
}

.quantile_t <- function(q, t, n1, n2 = NULL, independentSamples = FALSE, prior.location, prior.scale,
                        prior.df, tol = 0.0001, max.iter = 100) {
  # Computes quantiles via Newton-Raphson method

  x.cur <- Inf
  # get reasonable starting value
  delta <- seq(-2, 2, length.out = 400)
  dens  <- .posterior_t(delta, t = t, n1 = n1, n2 = n2, independentSamples = independentSamples,
                        prior.location = prior.location, prior.scale = prior.scale, prior.df = prior.df)
  x.new <- delta[which.max(dens)]

  i <- 1
  while (abs(x.cur - x.new) > tol && i < max.iter) {
    x.cur <- x.new
    x.new <- x.cur - (.cdf_t(x.cur, t = t, n1 = n1, n2 = n2, independentSamples = independentSamples,
                             prior.location = prior.location, prior.scale = prior.scale, prior.df = prior.df) - q) /
                      .posterior_t(x.cur, t = t, n1 = n1, n2 = n2, independentSamples = independentSamples,
                                   prior.location = prior.location, prior.scale = prior.scale, prior.df = prior.df)

    if(!is.finite(x.new)) # possibly due to dividing by posterior density = 0
      stop("Cannot plot the posterior - possibly too concentrated near 0.")
    i <- i + 1
  }

  return(x.new)
}

.ciPlusMedian_t <- function(t, n1, n2 = NULL, independentSamples = FALSE, prior.location, prior.scale,
                            prior.df, ci = .95, tol = 0.0001, max.iter = 100, oneSided) {
  lower <- (1 - ci) / 2
  upper <- ci + (1 - ci) / 2
  med   <- .5

  postAreaSmaller0 <- .cdf_t(x = 0, t = t, n1 = n1, n2 = n2, independentSamples = independentSamples,
                             prior.location = prior.location, prior.scale = prior.scale, prior.df = prior.df)

  if (oneSided == "right") {

    lower <- postAreaSmaller0 + (1 - postAreaSmaller0) * lower
    upper <- postAreaSmaller0 + (1 - postAreaSmaller0) * upper
    med   <- postAreaSmaller0 + (1 - postAreaSmaller0) * med
  } else if (oneSided == "left") {

    lower <- postAreaSmaller0 * lower
    upper <- postAreaSmaller0 * upper
    med   <- postAreaSmaller0 * med
  }

  ciLower <- .quantile_t(lower, t = t, n1 = n1, n2 = n2, independentSamples = independentSamples,
                         prior.location = prior.location, prior.scale = prior.scale, prior.df = prior.df)
  ciUpper <- .quantile_t(upper, t = t, n1 = n1, n2 = n2, independentSamples = independentSamples,
                         prior.location = prior.location, prior.scale = prior.scale, prior.df = prior.df)
  median  <- .quantile_t(med,   t = t, n1 = n1, n2 = n2, independentSamples = independentSamples,
                         prior.location = prior.location, prior.scale = prior.scale, prior.df = prior.df)

  return(list(ciLower = ciLower, median = median, ciUpper = ciUpper))
}

.posterior_normal <- function(delta, t, n1, n2 = NULL, independentSamples = FALSE, prior.mean,
                              prior.variance, rel.tol = .Machine$double.eps^0.25) {

  neff        <- ifelse(independentSamples, n1 * n2 / (n1 + n2), n1)
  nu          <- ifelse(independentSamples, n1 + n2 - 2, n1 - 1)
  mu.delta    <- prior.mean
  g           <- prior.variance

  numerator   <- dt(x = t, df = nu, ncp = sqrt(neff) * delta) * dnorm(x = delta, mean = mu.delta, sd = sqrt(g))

  denominator <- 1 / sqrt(1 + neff * g) * dt(x = t / sqrt(1 + neff * g),
                                             df = nu,
                                             ncp = sqrt(neff / (1 + neff * g)) * mu.delta)

  out <- numerator / denominator
  out[is.na(out)] <- 0

  return(out)
}


.cdf_normal <- function(x, t, n1, n2 = NULL, independentSamples = FALSE, prior.mean, prior.variance) {

  integrate(.posterior_normal, lower = -Inf, upper = x, t = t, n1 = n1, n2 = n2,
            independentSamples = independentSamples, prior.mean = prior.mean, prior.variance = prior.variance)$value
}

.quantile_normal <- function(q, t, n1, n2 = NULL, independentSamples = FALSE, prior.mean,
                             prior.variance, tol = 0.0001, max.iter = 100) {
  # Compute quantiles via Newton-Raphson method

  x.cur <- Inf
  # get reasonable start value
  delta <- seq(-2, 2, length.out = 400)
  dens  <- .posterior_normal(delta, t = t, n1 = n1, n2 = n2, independentSamples = independentSamples,
                             prior.mean = prior.mean, prior.variance = prior.variance)
  x.new <- delta[which.max(dens)]
  i     <- 1

  while (abs(x.cur - x.new) > tol && i < max.iter) {

    x.cur <- x.new
    x.new <- x.cur - (.cdf_normal(x.cur, t = t, n1 = n1, n2 = n2, independentSamples = independentSamples,
                                  prior.mean = prior.mean, prior.variance = prior.variance) - q) /
                      .posterior_normal(x.cur, t = t, n1 = n1, n2 = n2, independentSamples = independentSamples,
                                        prior.mean = prior.mean, prior.variance = prior.variance)

    if(is.infinite(x.new)) # possibly due to dividing by posterior density = 0
      stop("Cannot plot the posterior - possibly too concentrated near 0.")
    i <- i + 1
  }

  return(x.new)
}

.ciPlusMedian_normal <- function(t, n1, n2 = NULL, independentSamples = FALSE, prior.mean,
                                 prior.variance, ci = .95, oneSided = FALSE, tol = 0.0001,
                                 max.iter = 100) {

  lower <- (1 - ci)/2
  upper <- ci + (1 - ci)/2
  med   <- .5

  postAreaSmaller0 <- .cdf_normal(x = 0, t = t, n1 = n1, n2 = n2, independentSamples = independentSamples,
                                 prior.mean = prior.mean, prior.variance = prior.variance)


  if (oneSided == "right") {

    lower <- postAreaSmaller0 + (1 - postAreaSmaller0) * lower
    upper <- postAreaSmaller0 + (1 - postAreaSmaller0) * upper
    med   <- postAreaSmaller0 + (1 - postAreaSmaller0) * med
  } else if (oneSided == "left") {

    lower <- postAreaSmaller0 * lower
    upper <- postAreaSmaller0 * upper
    med   <- postAreaSmaller0 * med
  }

  ciLower <- .quantile_normal(lower, t = t, n1 = n1, n2 = n2, independentSamples = independentSamples,
                              prior.mean = prior.mean, prior.variance = prior.variance)
  ciUpper <- .quantile_normal(upper, t = t, n1 = n1, n2 = n2, independentSamples = independentSamples,
                              prior.mean = prior.mean, prior.variance = prior.variance)
  median  <- .quantile_normal(med,   t = t, n1 = n1, n2 = n2, independentSamples = independentSamples,
                              prior.mean = prior.mean, prior.variance = prior.variance)

  return(list(ciLower = ciLower, median = median, ciUpper = ciUpper))
}

.dposterior_informative <- function(delta, t, n1, n2 = NULL, paired = FALSE, oneSided = FALSE, options) {
  isDefault <- options[["effectSizeStandardized"]] == "default"

  if(isDefault) {
    cauchyLocation <- 0
    cauchyScale    <- options[["priorWidth"]]
  } else {
    cauchyLocation <- options[["informativeCauchyLocation"]]
    cauchyScale    <- options[["informativeCauchyScale"]]
  }


  if (options[["informativeStandardizedEffectSize"]] == "cauchy" || isDefault) {
    out <- .posterior_t(delta, t = t, n1 = n1, n2 = n2, independentSamples = ! paired && !is.null(n2),
                        prior.location = cauchyLocation,
                        prior.scale    = cauchyScale,
                        prior.df = 1)
    if (oneSided == "right") {

      out <- ifelse(delta < 0, 0, out / (1 - .cdf_t(0, t = t, n1 = n1, n2 = n2, independentSamples = ! paired && !is.null(n2),
                                                    prior.location = cauchyLocation,
                                                    prior.scale    = cauchyScale,
                                                    prior.df = 1)))
    } else if (oneSided == "left") {

      out <- ifelse(delta > 0, 0, out / .cdf_t(0, t = t, n1 = n1, n2 = n2, independentSamples = ! paired && !is.null(n2),
                                               prior.location = cauchyLocation,
                                               prior.scale    = cauchyScale,
                                               prior.df = 1))
    }
  } else if (options[["informativeStandardizedEffectSize"]] == "t") {

    out <- .posterior_t(delta, t = t, n1 = n1, n2 = n2, independentSamples = ! paired && !is.null(n2),
                        prior.location = options[["informativeTLocation"]],
                        prior.scale = options[["informativeTScale"]],
                        prior.df = options[["informativeTDf"]])

    if (oneSided == "right") {
      out <- ifelse(delta < 0, 0, out / (1 - .cdf_t(0, t = t, n1 = n1, n2 = n2, independentSamples = ! paired && !is.null(n2),
                                                    prior.location = options[["informativeTLocation"]],
                                                    prior.scale = options[["informativeTScale"]],
                                                    prior.df = options[["informativeTDf"]])))

    } else if (oneSided == "left") {
      out <- ifelse(delta > 0, 0, out / .cdf_t(0, t = t, n1 = n1, n2 = n2, independentSamples = ! paired && !is.null(n2),
                                               prior.location = options[["informativeTLocation"]],
                                               prior.scale = options[["informativeTScale"]],
                                               prior.df = options[["informativeTDf"]]))
    }
  } else if (options[["informativeStandardizedEffectSize"]] == "normal") {
    out <- .posterior_normal(delta, t = t, n1 = n1, n2 = n2, independentSamples = ! paired && !is.null(n2),
                             prior.mean = options[["informativeNormalMean"]],
                             prior.variance = options[["informativeNormalStd"]]^2)

    if (oneSided == "right") {
      out <- ifelse(delta < 0, 0, out / (1 - .cdf_normal(0, t = t, n1 = n1, n2 = n2, independentSamples = ! paired && !is.null(n2),
                                                         prior.mean = options[["informativeNormalMean"]],
                                                         prior.variance = options[["informativeNormalStd"]]^2)))

    } else if (oneSided == "left") {
      out <- ifelse(delta > 0, 0, out / .cdf_normal(0, t = t, n1 = n1, n2 = n2, independentSamples = ! paired && !is.null(n2),
                                                    prior.mean = options[["informativeNormalMean"]],
                                                    prior.variance = options[["informativeNormalStd"]]^2))
    }
  }

  out[out < 0] <- 0
  return(out)
}

.bf10_t_halfspace <- function(t, neff, nu, direction, prior.location, prior.scale,
                              prior.df, rel.tol) {
  # Conditional on T=t under H0, Q=sqrt(V/nu) has density proportional to
  # q^nu exp(-(nu+t^2)q^2/2). The likelihood ratio at delta is therefore
  # E[exp(-neff*delta^2/2 + t*sqrt(neff)*delta*Q)]. Integrating this positive
  # quantity avoids subtracting a posterior probability close to one.
  a <- nu + t^2
  logQConstant <- log(2) + (nu + 1)/2 * log(a/2) - lgamma((nu + 1)/2)
  logPriorMass <- pt(direction * prior.location/prior.scale, df = prior.df, log.p = TRUE)
  tolerance <- max(100 * .Machine$double.eps, min(rel.tol, 1e-9))
  innerRelativeError <- 0

  integratePieces <- function(fun, cuts) {
    pieces <- lapply(seq_len(length(cuts) - 1L), function(i) {
      integrate(fun, cuts[i], cuts[i + 1L], abs.tol = 0, rel.tol = tolerance,
                subdivisions = 1000L)
    })
    list(value = sum(vapply(pieces, function(x) x$value, numeric(1))),
         abs.error = sum(vapply(pieces, function(x) x$abs.error, numeric(1))))
  }

  # Split at the concentration of Q and at the prior's location/scale, so
  # adaptive integration also sees narrow likelihoods and narrow priors.
  qMode <- sqrt(nu/a)
  qWidth <- 1/sqrt(2*a)
  qCuts <- sort(unique(c(0, max(0, qMode - 8*qWidth), qMode, qMode + 8*qWidth, Inf)))
  location <- direction * prior.location
  deltaCuts <- sort(unique(c(0, 1/(sqrt(neff)*(1 + abs(t))),
                            max(0, location - 8*prior.scale), max(0, location),
                            max(0, location + 8*prior.scale), Inf)))
  integrand <- function(magnitudes) {
    vapply(magnitudes, function(magnitude) {
      likelihood <- integratePieces(function(q) {
        exp(logQConstant + nu*log(q) - a*q^2/2 - neff*magnitude^2/2 +
              direction*t*sqrt(neff)*magnitude*q)
      }, qCuts)
      if (likelihood$value > 0)
        innerRelativeError <<- max(innerRelativeError, likelihood$abs.error/likelihood$value)
      prior <- exp(dt((direction*magnitude - prior.location)/prior.scale,
                      df = prior.df, log = TRUE) - log(prior.scale) - logPriorMass)
      prior * likelihood$value
    }, numeric(1))
  }
  result <- integratePieces(integrand, deltaCuts)
  # Positive weights propagate the maximum observed inner relative error.
  # These are quadrature error estimates, not rigorous total error bounds.
  error <- if (result$value > 0) result$abs.error/result$value + innerRelativeError else NA_real_
  list(bf = result$value, error = error)
}

.bf10_t <- function(t, n1, n2 = NULL, oneSided, independentSamples = FALSE, prior.location,
                    prior.scale, prior.df, rel.tol = .Machine$double.eps^0.25) {

  neff <- ifelse(independentSamples, n1 * n2 / (n1 + n2), n1)
  nu   <- ifelse(independentSamples, n1 + n2 - 2, n1 - 1)

  oppositeDirection <- (oneSided == "right" && t < 0) || (oneSided == "left" && t > 0)
  if (oppositeDirection) {
    direction <- if (oneSided == "right") 1 else -1
    return(.bf10_t_halfspace(t, neff, nu, direction, prior.location, prior.scale,
                             prior.df, rel.tol))
  }

  mu.delta    <- prior.location
  gamma       <- prior.scale
  kappa       <- prior.df

  int         <- integrate(.integrand_t, lower = -Inf, upper = Inf, t = t, n = neff, nu = nu,
                           mu.delta = mu.delta, gamma = gamma, kappa = kappa, rel.tol = rel.tol)
  numerator   <- int$value
  denominator <- dt(x = t, df = nu)
  BF10        <- numerator / denominator
  error       <- exp(log(int[[2]]) - log(BF10))

  if (oneSided == FALSE) {
    return(list(bf = BF10, error = error))
  }

  priorAreaSmaller0 <- pt(q = - mu.delta / gamma, df = kappa)
  postAreaSmaller0  <- .cdf_t(x = 0, t = t, n1 = n1, n2 = n2, independentSamples = independentSamples,
                              prior.location = prior.location, prior.scale = prior.scale, prior.df = prior.df)

  # This is required in case there are numerical issues.
  if (postAreaSmaller0 > 1) {
    postAreaSmaller0 <- 1
  } else if(postAreaSmaller0 < 0) {
    postAreaSmaller0 <- 0
  }

  bf <- 'NA'
  if (oneSided == "left") {
    BFmin1  <- postAreaSmaller0 / priorAreaSmaller0
    BFmin0  <- BFmin1  * BF10

    # TODO: Verify this
    bf <- BFmin0
  } else if (oneSided  == "right") {
    BFplus1 <- (1 - postAreaSmaller0) / (1 - priorAreaSmaller0)

    BFplus0 <- BFplus1 * BF10
    # TODO: Verify this
    bf <- BFplus0
  }

  return(list(bf = bf, error = error))
}

.bf10_normal <- function(t, n1, n2 = NULL, oneSided, independentSamples = FALSE, prior.mean, prior.variance) {

  neff <- ifelse(independentSamples, n1 * n2 / (n1 + n2), n1)
  nu   <- ifelse(independentSamples, n1 + n2 - 2, n1 - 1)

  mu.delta    <- prior.mean
  g           <- prior.variance
  numerator   <- 1 / sqrt(1 + neff * g) * dt(x = t / sqrt(1 + neff * g), df = nu,
                                             ncp = sqrt(neff / (1 + neff * g)) * mu.delta)
  denominator <- dt(x = t, df = nu)
  BF10        <- numerator / denominator

  if (oneSided == FALSE) {
      return (BF10)
  }

  priorAreaSmaller0 <- pnorm(0, mean = prior.mean, sd = sqrt(prior.variance))
  postAreaSmaller0  <- .cdf_normal(x = 0, t = t, n1 = n1, n2 = n2, independentSamples = independentSamples,
                                   prior.mean = prior.mean, prior.variance = prior.variance)

  BFmin1  <- postAreaSmaller0 / priorAreaSmaller0
  BFplus1 <- (1 - postAreaSmaller0) / (1 - priorAreaSmaller0)
  BFmin0  <- BFmin1  * BF10
  BFplus0 <- BFplus1 * BF10

  bf <- 'NA'
  if (oneSided == "left") {
    BFmin1  <- postAreaSmaller0 / priorAreaSmaller0
    bf  <- BFmin1  * BF10
  } else if (oneSided  == "right") {

    BFplus1 <- (1 - postAreaSmaller0) / (1 - priorAreaSmaller0)
    bf      <- BFplus1 * BF10
  }

  return (bf)
}
