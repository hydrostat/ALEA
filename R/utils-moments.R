# Internal sample moment helpers for method-of-moments estimation.

sample_moments_internal <- function(x, min_length = 3L) {
  x <- check_numeric_vector(x, "x", min_length = min_length)

  n <- length(x)
  mean_x <- mean(x)
  centered <- x - mean_x

  # Ordinary central moments for method-of-moments matching.
  m2 <- mean(centered^2)
  if (!is.finite(m2) || m2 <= 0) {
    stop("`x` must have positive variance.", call. = FALSE)
  }

  sd_x <- sqrt(m2)
  m3 <- mean(centered^3)
  skewness <- m3 / sd_x^3

  list(
    n = n,
    mean = mean_x,
    variance = m2,
    sd = sd_x,
    skewness = skewness,
    min = min(x),
    max = max(x),
    data = x
  )
}

check_finite_parameter_result <- function(para, distribution, method) {
  if (any(!is.finite(para))) {
    stop(
      "Method `", method, "` produced non-finite parameters for distribution `",
      distribution, "`.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

make_fit_result_internal <- function(para, distribution, method, engine, message) {
  check_finite_parameter_result(para, distribution, method)

  list(
    parameters = para,
    convergence = list(
      converged = TRUE,
      method = method,
      engine = engine,
      message = message
    ),
    covariance = NULL,
    diagnostics = NULL,
    warnings = character()
  )
}
