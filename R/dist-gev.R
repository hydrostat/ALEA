# Generalized extreme-value distribution internals.
# Parameterization follows Hosking's lmom package:
# para = c(xi, alpha, k), where xi is location, alpha is scale, and k is shape.
# Important: k uses the Hosking/Jenkinson sign convention used by lmom.

check_gev_params <- function(para) {
  check_parameter_vector(para, c("xi", "alpha", "k"))
  check_positive_value(para[["alpha"]], "alpha")
  invisible(TRUE)
}

d_gev_internal <- function(x, para, log = FALSE) {
  check_gev_params(para)

  x <- as.numeric(x)
  xi <- para[["xi"]]
  alpha <- para[["alpha"]]
  k <- para[["k"]]

  log_density <- rep(-Inf, length(x))

  if (abs(k) < .Machine$double.eps^0.5) {
    y <- (x - xi) / alpha
    log_density <- -log(alpha) - y - exp(-y)
  } else {
    z <- 1 - k * (x - xi) / alpha
    ok <- z > 0
    log_density[ok] <- -log(alpha) +
      (1 / k - 1) * log(z[ok]) -
      z[ok]^(1 / k)
  }

  if (log) log_density else exp(log_density)
}

p_gev_internal <- function(q, para) {
  check_gev_params(para)
  lmom::cdfgev(q, para)
}

q_gev_internal <- function(p, para) {
  check_probability(p, allow_zero_one = FALSE)
  check_gev_params(para)
  lmom::quagev(p, para)
}

r_gev_internal <- function(n, para) {
  check_non_negative_count(n)
  q_gev_internal(stats::runif(n), para)
}

gev_skewness_from_k_internal <- function(k) {
  if (!is.numeric(k) || length(k) != 1L || !is.finite(k)) {
    return(NA_real_)
  }

  if (abs(k) < 1e-7) {
    return(12 * sqrt(6) * 1.202056903159594 / pi^3)
  }

  if (1 + 3 * k <= 0) {
    return(NA_real_)
  }

  g1 <- gamma(1 + k)
  g2 <- gamma(1 + 2 * k)
  g3 <- gamma(1 + 3 * k)

  variance_component <- g2 - g1^2
  if (!is.finite(variance_component) || variance_component <= 0) {
    return(NA_real_)
  }

  third_component <- -(g3 - 3 * g1 * g2 + 2 * g1^3)

  # The Hosking/Jenkinson GEV parameterization is
  # x(F) = xi + alpha / k * {1 - (-log(F))^k}.
  # The standardized third central moment therefore includes
  # sign(k), because the scale multiplier alpha / k changes sign
  # when k is negative. Omitting this factor makes the skewness
  # discontinuous around k = 0 and can falsely reject valid samples.
  sign(k) * third_component / variance_component^(3 / 2)
}

gev_k_from_skewness_internal <- function(skewness) {
  if (!is.finite(skewness)) {
    stop("Sample skewness must be finite for GEV MOM fitting.", call. = FALSE)
  }

  f <- function(k) gev_skewness_from_k_internal(k) - skewness

  lower <- -0.32
  upper <- 5

  f_lower <- f(lower)
  f_upper <- f(upper)

  if (!is.finite(f_lower) || !is.finite(f_upper) || f_lower * f_upper > 0) {
    stop(
      "GEV MOM fitting failed because the sample skewness is outside the supported numerical range.",
      call. = FALSE
    )
  }

  stats::uniroot(f, lower = lower, upper = upper, tol = 1e-10)$root
}

fit_gev_lmom <- function(x, ...) {
  x <- check_numeric_vector(x, "x", min_length = 3L)

  lmom <- lmom::samlmu(x, nmom = 3L)
  para <- lmom::pelgev(lmom)
  names(para) <- c("xi", "alpha", "k")

  check_gev_params(para)

  list(
    parameters = para,
    convergence = list(
      converged = TRUE,
      method = "lmom",
      engine = "lmom::pelgev",
      message = "GEV parameters estimated by L-moments using lmom::pelgev()."
    ),
    covariance = NULL,
    diagnostics = NULL,
    warnings = character()
  )
}

fit_gev_mom <- function(x, ...) {
  st <- sample_moments_internal(x, min_length = 3L)

  k <- gev_k_from_skewness_internal(st$skewness)

  if (abs(k) < 1e-7) {
    alpha <- sqrt(6) * st$sd / pi
    xi <- st$mean - 0.5772156649015329 * alpha
    k <- 0
  } else {
    g1 <- gamma(1 + k)
    g2 <- gamma(1 + 2 * k)

    variance_component <- g2 - g1^2
    if (!is.finite(variance_component) || variance_component <= 0) {
      stop("GEV MOM fitting failed to compute a positive variance component.", call. = FALSE)
    }

    alpha <- sqrt(st$variance * k^2 / variance_component)
    xi <- st$mean - (alpha / k) * (1 - g1)
  }

  para <- c(xi = xi, alpha = alpha, k = k)
  check_gev_params(para)

  make_fit_result_internal(
    para = para,
    distribution = "gev",
    method = "mom",
    engine = "ALEA method-of-moments translated from legacy ALEA",
    message = "GEV parameters estimated by method of moments."
  )
}

fit_gev_mle <- function(x, ...) {
  x <- check_numeric_vector(x, "x", min_length = 3L)
  start <- tryCatch(
    fit_gev_lmom(x)$parameters,
    error = function(e) fit_gev_mom(x)$parameters
  )

  objective <- function(theta) {
    para <- c(xi = theta[[1L]], alpha = exp(theta[[2L]]), k = theta[[3L]])
    safe_negloglik_internal(x, para, d_gev_internal)
  }

  opt <- mle_optim_internal(
    par = c(unname(start[["xi"]]), log(unname(start[["alpha"]])), unname(start[["k"]])),
    fn = objective,
    lower = c(-Inf, log(.Machine$double.eps), -5),
    upper = c(Inf, Inf, 5)
  )

  para <- c(xi = opt$par[[1L]], alpha = exp(opt$par[[2L]]), k = opt$par[[3L]])
  check_gev_params(para)

  make_mle_fit_result_internal(para, "gev", opt)
}

return_level_gev_internal <- function(return_period, para, ...) {
  check_return_period(return_period)
  q_gev_internal(1 - 1 / return_period, para)
}
