# Generalized Pareto distribution internals.
# Parameterization follows Hosking's lmom package:
# para = c(xi, alpha, k), where xi is location/lower threshold, alpha is scale,
# and k is shape. Important: k uses the Hosking sign convention used by lmom.

check_gpa_params <- function(para) {
  check_parameter_vector(para, c("xi", "alpha", "k"))
  check_positive_value(para[["alpha"]], "alpha")
  invisible(TRUE)
}

d_gpa_internal <- function(x, para, log = FALSE) {
  check_gpa_params(para)

  x <- as.numeric(x)
  xi <- para[["xi"]]
  alpha <- para[["alpha"]]
  k <- para[["k"]]

  log_density <- rep(-Inf, length(x))

  if (abs(k) < .Machine$double.eps^0.5) {
    ok <- x >= xi
    y <- (x[ok] - xi) / alpha
    log_density[ok] <- -log(alpha) - y
  } else {
    z <- 1 - k * (x - xi) / alpha
    ok <- x >= xi & z > 0
    log_density[ok] <- -log(alpha) + (1 / k - 1) * log(z[ok])
  }

  if (log) log_density else exp(log_density)
}

p_gpa_internal <- function(q, para) {
  check_gpa_params(para)
  lmom::cdfgpa(q, para)
}

q_gpa_internal <- function(p, para) {
  check_probability(p, allow_zero_one = FALSE)
  check_gpa_params(para)
  lmom::quagpa(p, para)
}

r_gpa_internal <- function(n, para) {
  check_non_negative_count(n)
  q_gpa_internal(stats::runif(n), para)
}

gpa_skewness_from_k_internal <- function(k) {
  if (!is.numeric(k) || length(k) != 1L || !is.finite(k)) {
    return(NA_real_)
  }

  if (1 + 2 * k <= 0 || 1 + 3 * k <= 0) {
    return(NA_real_)
  }

  2 * (1 - k) * sqrt(1 + 2 * k) / (1 + 3 * k)
}

gpa_k_from_skewness_internal <- function(skewness) {
  if (!is.finite(skewness)) {
    stop("Sample skewness must be finite for GPA MOM fitting.", call. = FALSE)
  }

  f <- function(k) gpa_skewness_from_k_internal(k) - skewness

  lower <- -0.32
  upper <- 20

  f_lower <- f(lower)
  f_upper <- f(upper)

  if (!is.finite(f_lower) || !is.finite(f_upper) || f_lower * f_upper > 0) {
    stop(
      "GPA MOM fitting failed because the sample skewness is outside the supported numerical range.",
      call. = FALSE
    )
  }

  stats::uniroot(f, lower = lower, upper = upper, tol = 1e-10)$root
}

fit_gpa_lmom <- function(x, bound = NULL, ...) {
  x <- check_numeric_vector(x, "x", min_length = 3L)

  lmom <- lmom::samlmu(x, nmom = 3L)
  para <- lmom::pelgpa(lmom, bound = bound)
  names(para) <- c("xi", "alpha", "k")

  check_gpa_params(para)

  list(
    parameters = para,
    convergence = list(
      converged = TRUE,
      method = "lmom",
      engine = "lmom::pelgpa",
      message = "GPA parameters estimated by L-moments using lmom::pelgpa()."
    ),
    covariance = NULL,
    diagnostics = NULL,
    warnings = character()
  )
}

fit_gpa_mom <- function(x, ...) {
  st <- sample_moments_internal(x, min_length = 3L)

  k <- gpa_k_from_skewness_internal(st$skewness)

  if (1 + 2 * k <= 0) {
    stop("GPA MOM fitting produced an invalid shape parameter.", call. = FALSE)
  }

  alpha <- st$sd * (1 + k) * sqrt(1 + 2 * k)
  xi <- st$mean - alpha / (1 + k)

  para <- c(xi = xi, alpha = alpha, k = k)
  check_gpa_params(para)

  make_fit_result_internal(
    para = para,
    distribution = "gpa",
    method = "mom",
    engine = "ALEA method-of-moments",
    message = "GPA parameters estimated by method of moments."
  )
}

fit_gpa_mle <- function(x, ...) {
  x <- check_numeric_vector(x, "x", min_length = 3L)
  start <- tryCatch(
    fit_gpa_lmom(x)$parameters,
    error = function(e) fit_gpa_mom(x)$parameters
  )

  min_x <- min(x)
  xi_start <- min(unname(start[["xi"]]), min_x - .Machine$double.eps^0.25)
  delta_start <- max(min_x - xi_start, .Machine$double.eps^0.25)

  objective <- function(theta) {
    para <- c(
      xi = min_x - exp(theta[[1L]]),
      alpha = exp(theta[[2L]]),
      k = theta[[3L]]
    )
    safe_negloglik_internal(x, para, d_gpa_internal)
  }

  opt <- mle_optim_internal(
    par = c(log(delta_start), log(unname(start[["alpha"]])), unname(start[["k"]])),
    fn = objective,
    lower = c(log(.Machine$double.eps), log(.Machine$double.eps), -5),
    upper = c(Inf, Inf, 5)
  )

  para <- c(
    xi = min_x - exp(opt$par[[1L]]),
    alpha = exp(opt$par[[2L]]),
    k = opt$par[[3L]]
  )
  check_gpa_params(para)

  make_mle_fit_result_internal(para, "gpa", opt)
}

return_level_gpa_internal <- function(return_period, para, exceedance_rate = 1, ...) {
  check_return_period(return_period)
  check_positive_value(exceedance_rate, "exceedance_rate")

  p <- 1 - 1 / (return_period * exceedance_rate)
  q_gpa_internal(p, para)
}
