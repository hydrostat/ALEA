# Three-parameter lognormal distribution internals.
# Parameterization follows Hosking's lmom package:
# para = c(zeta, mu, sigma), where zeta is lower bound, mu is meanlog,
# and sigma is sdlog.

check_ln3_params <- function(para) {
  check_parameter_vector(para, c("zeta", "mu", "sigma"))
  check_positive_value(para[["sigma"]], "sigma")
  invisible(TRUE)
}

d_ln3_internal <- function(x, para, log = FALSE) {
  check_ln3_params(para)

  x <- as.numeric(x)
  y <- x - para[["zeta"]]
  log_density <- rep(-Inf, length(x))

  ok <- y > 0
  log_density[ok] <- stats::dlnorm(
    y[ok],
    meanlog = para[["mu"]],
    sdlog = para[["sigma"]],
    log = TRUE
  )

  if (log) log_density else exp(log_density)
}

p_ln3_internal <- function(q, para) {
  check_ln3_params(para)
  lmom::cdfln3(q, para)
}

q_ln3_internal <- function(p, para) {
  check_probability(p, allow_zero_one = FALSE)
  check_ln3_params(para)
  lmom::qualn3(p, para)
}

r_ln3_internal <- function(n, para) {
  check_non_negative_count(n)
  q_ln3_internal(stats::runif(n), para)
}

fit_ln3_lmom <- function(x, bound = NULL, ...) {
  x <- check_numeric_vector(x, "x", min_length = 3L)

  lmom <- lmom::samlmu(x, nmom = 3L)
  para <- lmom::pelln3(lmom, bound = bound)
  names(para) <- c("zeta", "mu", "sigma")

  check_ln3_params(para)

  list(
    parameters = para,
    convergence = list(
      converged = TRUE,
      method = "lmom",
      engine = "lmom::pelln3",
      message = "LN3 parameters estimated by L-moments using lmom::pelln3()."
    ),
    covariance = NULL,
    diagnostics = NULL,
    warnings = character()
  )
}

fit_ln3_mom <- function(x, ...) {
  st <- sample_moments_internal(x, min_length = 3L)

  if (!is.finite(st$skewness) || st$skewness <= 0) {
    stop(
      "LN3 MOM fitting requires positive sample skewness.",
      call. = FALSE
    )
  }

  g <- st$skewness
  w <- (-g + sqrt(g^2 + 4)) / 2
  z2 <- (1 - w^(2 / 3)) / w^(1 / 3)

  if (!is.finite(z2) || z2 <= 0) {
    stop("LN3 MOM fitting failed to compute a valid shape transformation.", call. = FALSE)
  }

  zeta <- st$mean - st$sd / z2
  sigma <- sqrt(log1p(z2^2))
  mu <- log(st$sd / z2) - 0.5 * log1p(z2^2)

  para <- c(zeta = zeta, mu = mu, sigma = sigma)
  check_ln3_params(para)

  if (any(x <= para[["zeta"]])) {
    stop("LN3 MOM fitting produced a lower bound that is not below all observations.", call. = FALSE)
  }

  make_fit_result_internal(
    para = para,
    distribution = "ln3",
    method = "mom",
    engine = "ALEA method-of-moments translated from legacy ALEA",
    message = "LN3 parameters estimated by method of moments."
  )
}

fit_ln3_mle <- function(x, ...) {
  x <- check_numeric_vector(x, "x", min_length = 3L)
  start <- tryCatch(
    fit_ln3_lmom(x)$parameters,
    error = function(e) fit_ln3_mom(x)$parameters
  )

  min_x <- min(x)
  zeta_start <- min(unname(start[["zeta"]]), min_x - .Machine$double.eps^0.25)
  delta_start <- max(min_x - zeta_start, .Machine$double.eps^0.25)

  objective <- function(theta) {
    zeta <- min_x - exp(theta[[1L]])
    para <- c(zeta = zeta, mu = theta[[2L]], sigma = exp(theta[[3L]]))
    safe_negloglik_internal(x, para, d_ln3_internal)
  }

  opt <- mle_optim_internal(
    par = c(log(delta_start), unname(start[["mu"]]), log(unname(start[["sigma"]]))),
    fn = objective
  )

  para <- c(
    zeta = min_x - exp(opt$par[[1L]]),
    mu = opt$par[[2L]],
    sigma = exp(opt$par[[3L]])
  )
  check_ln3_params(para)

  make_mle_fit_result_internal(para, "ln3", opt)
}

quantile_ln3_internal <- function(return_period, para, ...) {
  check_return_period(return_period)
  q_ln3_internal(1 - 1 / return_period, para)
}
