# Gumbel distribution internals.
# Parameterization follows Hosking's lmom package:
# para = c(xi, alpha), where xi is location and alpha is scale.

check_gum_params <- function(para) {
  check_parameter_vector(para, c("xi", "alpha"))
  check_positive_value(para[["alpha"]], "alpha")
  invisible(TRUE)
}

d_gum_internal <- function(x, para, log = FALSE) {
  check_gum_params(para)

  x <- as.numeric(x)
  xi <- para[["xi"]]
  alpha <- para[["alpha"]]
  y <- (x - xi) / alpha

  log_density <- -log(alpha) - y - exp(-y)

  if (log) log_density else exp(log_density)
}

p_gum_internal <- function(q, para) {
  check_gum_params(para)
  lmom::cdfgum(q, para)
}

q_gum_internal <- function(p, para) {
  check_probability(p, allow_zero_one = FALSE)
  check_gum_params(para)
  lmom::quagum(p, para)
}

r_gum_internal <- function(n, para) {
  check_non_negative_count(n)
  q_gum_internal(stats::runif(n), para)
}

fit_gum_lmom <- function(x, ...) {
  x <- check_numeric_vector(x, "x", min_length = 2L)

  lmom <- lmom::samlmu(x, nmom = 2L)
  para <- lmom::pelgum(lmom)
  names(para) <- c("xi", "alpha")

  check_gum_params(para)

  list(
    parameters = para,
    convergence = list(
      converged = TRUE,
      method = "lmom",
      engine = "lmom::pelgum",
      message = "Gumbel parameters estimated by L-moments using lmom::pelgum()."
    ),
    covariance = NULL,
    diagnostics = NULL,
    warnings = character()
  )
}

fit_gum_mom <- function(x, ...) {
  st <- sample_moments_internal(x, min_length = 2L)

  alpha <- sqrt(6) * st$sd / pi
  xi <- st$mean - 0.5772156649015329 * alpha

  para <- c(xi = xi, alpha = alpha)
  check_gum_params(para)

  make_fit_result_internal(
    para = para,
    distribution = "gum",
    method = "mom",
    engine = "ALEA method-of-moments",
    message = "Gumbel parameters estimated by method of moments."
  )
}

fit_gum_mle <- function(x, ...) {
  x <- check_numeric_vector(x, "x", min_length = 2L)
  start <- fit_gum_lmom(x)$parameters

  objective <- function(theta) {
    para <- c(xi = theta[[1L]], alpha = exp(theta[[2L]]))
    safe_negloglik_internal(x, para, d_gum_internal)
  }

  opt <- mle_optim_internal(
    par = c(unname(start[["xi"]]), log(unname(start[["alpha"]]))),
    fn = objective
  )

  para <- c(xi = opt$par[[1L]], alpha = exp(opt$par[[2L]]))
  check_gum_params(para)

  make_mle_fit_result_internal(para, "gum", opt)
}

quantile_gum_internal <- function(return_period, para, ...) {
  check_return_period(return_period)
  q_gum_internal(1 - 1 / return_period, para)
}
