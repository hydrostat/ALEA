# Two-parameter lognormal distribution internals.
# Parameterization follows Hosking's lmom package through LN3 with fixed lower
# bound zeta = 0:
# para = c(mu, sigma), where mu is meanlog and sigma is sdlog.

check_ln2_params <- function(para) {
  check_parameter_vector(para, c("mu", "sigma"))
  check_positive_value(para[["sigma"]], "sigma")
  invisible(TRUE)
}

ln2_to_ln3_para <- function(para) {
  check_ln2_params(para)
  c(zeta = 0, mu = para[["mu"]], sigma = para[["sigma"]])
}

d_ln2_internal <- function(x, para, log = FALSE) {
  check_ln2_params(para)

  stats::dlnorm(
    x,
    meanlog = para[["mu"]],
    sdlog = para[["sigma"]],
    log = log
  )
}

p_ln2_internal <- function(q, para) {
  check_ln2_params(para)
  lmom::cdfln3(q, ln2_to_ln3_para(para))
}

q_ln2_internal <- function(p, para) {
  check_probability(p, allow_zero_one = FALSE)
  check_ln2_params(para)
  lmom::qualn3(p, ln2_to_ln3_para(para))
}

r_ln2_internal <- function(n, para) {
  check_non_negative_count(n)
  q_ln2_internal(stats::runif(n), para)
}

fit_ln2_lmom <- function(x, ...) {
  x <- check_numeric_vector(x, "x", min_length = 3L)

  if (any(x <= 0)) {
    stop("`x` must contain only positive values for LN2 fitting.", call. = FALSE)
  }

  lmom <- lmom::samlmu(x, nmom = 3L)
  para3 <- lmom::pelln3(lmom, bound = 0)

  para <- c(mu = unname(para3[[2L]]), sigma = unname(para3[[3L]]))
  check_ln2_params(para)

  list(
    parameters = para,
    convergence = list(
      converged = TRUE,
      method = "lmom",
      engine = "lmom::pelln3(bound = 0)",
      message = "LN2 parameters estimated as LN3 with fixed lower bound zero using lmom::pelln3()."
    ),
    covariance = NULL,
    diagnostics = NULL,
    warnings = character()
  )
}

fit_ln2_mom <- function(x, ...) {
  x <- check_numeric_vector(x, "x", min_length = 3L)

  if (any(x <= 0)) {
    stop("`x` must contain only positive values for LN2 fitting.", call. = FALSE)
  }

  st <- sample_moments_internal(x, min_length = 3L)
  cv2 <- st$variance / st$mean^2

  if (!is.finite(cv2) || cv2 <= 0) {
    stop("The coefficient of variation must be positive for LN2 MOM fitting.", call. = FALSE)
  }

  sigma <- sqrt(log1p(cv2))
  mu <- log(st$mean) - 0.5 * sigma^2

  para <- c(mu = mu, sigma = sigma)
  check_ln2_params(para)

  make_fit_result_internal(
    para = para,
    distribution = "ln2",
    method = "mom",
    engine = "ALEA method-of-moments",
    message = "LN2 parameters estimated by method of moments."
  )
}

fit_ln2_mle <- function(x, ...) {
  x <- check_numeric_vector(x, "x", min_length = 3L)

  if (any(x <= 0)) {
    stop("`x` must contain only positive values for LN2 fitting.", call. = FALSE)
  }

  # Closed-form MLE for the two-parameter lognormal distribution.
  log_x <- log(x)
  para <- c(mu = mean(log_x), sigma = sqrt(mean((log_x - mean(log_x))^2)))
  check_ln2_params(para)

  loglik <- sum(d_ln2_internal(x, para, log = TRUE))

  list(
    parameters = para,
    convergence = list(
      converged = TRUE,
      method = "mle",
      engine = "closed-form lognormal MLE",
      message = "LN2 parameters estimated by maximum likelihood.",
      code = 0L,
      value = -loglik
    ),
    covariance = NULL,
    diagnostics = NULL,
    warnings = character()
  )
}

return_level_ln2_internal <- function(return_period, para, ...) {
  check_return_period(return_period)
  q_ln2_internal(1 - 1 / return_period, para)
}
