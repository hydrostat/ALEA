# Pearson type III distribution internals.
# Parameterization follows Hosking's lmom package:
# para = c(mu, sigma, gamma), where mu is mean, sigma is standard deviation,
# and gamma is skewness.

check_pe3_params <- function(para) {
  check_parameter_vector(para, c("mu", "sigma", "gamma"))
  check_positive_value(para[["sigma"]], "sigma")
  invisible(TRUE)
}

d_pe3_internal <- function(x, para, log = FALSE) {
  check_pe3_params(para)

  x <- as.numeric(x)
  mu <- para[["mu"]]
  sigma <- para[["sigma"]]
  gamma <- para[["gamma"]]

  if (abs(gamma) < .Machine$double.eps^0.5) {
    return(stats::dnorm(x, mean = mu, sd = sigma, log = log))
  }

  alpha <- 4 / gamma^2
  beta <- 0.5 * sigma * abs(gamma)
  xi <- mu - 2 * sigma / gamma

  log_density <- rep(-Inf, length(x))

  if (gamma > 0) {
    y <- x - xi
    ok <- y > 0
  } else {
    y <- xi - x
    ok <- y > 0
  }

  log_density[ok] <- stats::dgamma(
    y[ok],
    shape = alpha,
    scale = beta,
    log = TRUE
  )

  if (log) log_density else exp(log_density)
}

p_pe3_internal <- function(q, para) {
  check_pe3_params(para)
  lmom::cdfpe3(q, para)
}

q_pe3_internal <- function(p, para) {
  check_probability(p, allow_zero_one = FALSE)
  check_pe3_params(para)
  lmom::quape3(p, para)
}

r_pe3_internal <- function(n, para) {
  check_non_negative_count(n)
  q_pe3_internal(stats::runif(n), para)
}

fit_pe3_lmom <- function(x, ...) {
  x <- check_numeric_vector(x, "x", min_length = 3L)

  lmom <- lmom::samlmu(x, nmom = 3L)
  para <- lmom::pelpe3(lmom)
  names(para) <- c("mu", "sigma", "gamma")

  check_pe3_params(para)

  list(
    parameters = para,
    convergence = list(
      converged = TRUE,
      method = "lmom",
      engine = "lmom::pelpe3",
      message = "PE3 parameters estimated by L-moments using lmom::pelpe3()."
    ),
    covariance = NULL,
    diagnostics = NULL,
    warnings = character()
  )
}

fit_pe3_mom <- function(x, ...) {
  st <- sample_moments_internal(x, min_length = 3L)

  para <- c(
    mu = st$mean,
    sigma = st$sd,
    gamma = st$skewness
  )

  check_pe3_params(para)

  make_fit_result_internal(
    para = para,
    distribution = "pe3",
    method = "mom",
    engine = "ALEA method-of-moments translated to lmom PE3 parameterization",
    message = "PE3 parameters estimated by method of moments."
  )
}

fit_pe3_mle <- function(x, ...) {
  x <- check_numeric_vector(x, "x", min_length = 3L)
  start <- tryCatch(
    fit_pe3_lmom(x)$parameters,
    error = function(e) fit_pe3_mom(x)$parameters
  )

  objective <- function(theta) {
    para <- c(mu = theta[[1L]], sigma = exp(theta[[2L]]), gamma = theta[[3L]])
    safe_negloglik_internal(x, para, d_pe3_internal)
  }

  opt <- mle_optim_internal(
    par = c(unname(start[["mu"]]), log(unname(start[["sigma"]])), unname(start[["gamma"]])),
    fn = objective,
    lower = c(-Inf, log(.Machine$double.eps), -10),
    upper = c(Inf, Inf, 10)
  )

  para <- c(mu = opt$par[[1L]], sigma = exp(opt$par[[2L]]), gamma = opt$par[[3L]])
  check_pe3_params(para)

  make_mle_fit_result_internal(para, "pe3", opt)
}

quantile_pe3_internal <- function(return_period, para, ...) {
  check_return_period(return_period)
  q_pe3_internal(1 - 1 / return_period, para)
}
