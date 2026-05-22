#' Fit a probability distribution to a hydrological sample
#'
#' `alea_fit()` fits one supported probability distribution to a numeric
#' hydrological sample using one estimation method.
#'
#' @param x A numeric vector with the observed sample.
#' @param distribution Character scalar. Distribution to fit. Supported values
#'   are `"gev"`, `"gpa"`, `"pe3"`, `"ln2"`, `"ln3"`, and `"gum"`.
#' @param method Character scalar. Estimation method. Supported values are
#'   `"lmom"`, `"mom"`, and `"mle"`. The default is `"lmom"`.
#' @param return_period Optional numeric vector of return periods. If supplied,
#'   return levels are computed and stored in the fitted object for convenience.
#'   Return periods must be greater than 1.
#' @param ... Additional arguments passed to internal fitting routines or
#'   reserved for future extensions.
#'
#' @return An object of class `alea_fit`.
#'
#' @details
#' The fitted object stores the original numeric sample in its `data` field.
#' This sample is used by downstream workflows such as return-level bootstrap
#' confidence intervals, goodness-of-fit assessment, diagnostics,
#' AI-assisted distribution selection, and plotting.
#'
#' ALEA-R uses the parameterization adopted by Hosking's `lmom` package. For
#' example, GEV and GPA use `xi`, `alpha`, and `k`.
#'
#' @examples
#' x <- c(42.1, 38.5, 51.3, 47.0, 62.4, 55.2, 49.8, 58.1)
#'
#' fit <- alea_fit(x, distribution = "gev", method = "lmom")
#' fit
#' coef(fit)
#'
#' fit_with_return_levels <- alea_fit(
#'   x,
#'   distribution = "gev",
#'   method = "lmom",
#'   return_period = c(10, 25, 50, 100)
#' )
#'
#' fit_with_return_levels
#'
#' @export
alea_fit <- function(x,
                     distribution,
                     method = "lmom",
                     return_period = NULL,
                     ...) {
  x <- check_numeric_vector(x, "x", min_length = 2L)
  distribution <- check_distribution(distribution)
  method <- check_method(method)

  fit_fun <- get_fit_function_internal(distribution, method)
  fit_result <- fit_fun(x = x, ...)

  return_levels <- NULL

  if (!is.null(return_period)) {
    return_levels <- alea_return_level_from_params_internal(
      distribution = distribution,
      parameters = fit_result$parameters,
      return_period = return_period,
      ...
    )
  }

  new_alea_fit(
    data = x,
    distribution = distribution,
    method = method,
    parameters = fit_result$parameters,
    convergence = fit_result$convergence,
    return_levels = return_levels,
    covariance = fit_result$covariance %||% NULL,
    diagnostics = fit_result$diagnostics %||% NULL,
    warnings = fit_result$warnings %||% character()
  )
}

get_fit_function_internal <- function(distribution, method) {
  name <- paste0("fit_", distribution, "_", method)
  fun <- get0(name, mode = "function", inherits = TRUE)

  if (is.null(fun)) {
    stop(
      "Method `", method, "` is not implemented yet for distribution `",
      distribution,
      "`.",
      call. = FALSE
    )
  }

  fun
}

alea_return_level_from_params_internal <- function(distribution,
                                                   parameters,
                                                   return_period,
                                                   ...) {
  name <- paste0("return_level_", distribution, "_internal")
  fun <- get0(name, mode = "function", inherits = TRUE)

  if (is.null(fun)) {
    stop(
      "Return-level calculation is not implemented yet for distribution `",
      distribution,
      "`.",
      call. = FALSE
    )
  }

  values <- fun(return_period = return_period, para = parameters, ...)
  names(values) <- paste0("T", return_period)
  values
}

new_alea_fit <- function(data,
                         distribution,
                         method,
                         parameters,
                         convergence,
                         return_levels = NULL,
                         covariance = NULL,
                         diagnostics = NULL,
                         warnings = character()) {
  obj <- list(
    data = data,
    distribution = distribution,
    method = method,
    parameters = parameters,
    convergence = convergence,
    return_levels = return_levels,
    covariance = covariance,
    diagnostics = diagnostics,
    warnings = warnings,
    n = length(data)
  )

  class(obj) <- "alea_fit"
  validate_alea_fit(obj)
}

validate_alea_fit <- function(x) {
  if (!inherits(x, "alea_fit")) {
    stop("`x` must be an `alea_fit` object.", call. = FALSE)
  }

  check_distribution(x$distribution)
  check_method(x$method)

  if (!is.numeric(x$parameters) || is.null(names(x$parameters))) {
    stop("`parameters` must be a named numeric vector.", call. = FALSE)
  }

  if (!is.list(x$convergence)) {
    stop("`convergence` must be a list.", call. = FALSE)
  }

  if (!is.numeric(x$n) || length(x$n) != 1L || x$n < 1L) {
    stop("`n` must be a positive sample size.", call. = FALSE)
  }

  x
}

#' Print an ALEA fitted model
#'
#' @param x An `alea_fit` object.
#' @param ... Further arguments ignored.
#'
#' @return The input object, invisibly.
#'
#' @export
print.alea_fit <- function(x, ...) {
  cat("<alea_fit>\n")
  cat("Distribution:", x$distribution, "\n")
  cat("Method:", x$method, "\n")
  cat("Sample size:", x$n, "\n")
  cat("Parameters:\n")
  print(x$parameters)

  if (!is.null(x$return_levels)) {
    cat("Return levels:\n")
    print(x$return_levels)
  }

  invisible(x)
}

#' Extract fitted model coefficients
#'
#' @param object An `alea_fit` object.
#' @param ... Further arguments ignored.
#'
#' @return A named numeric vector of fitted parameters.
#'
#' @export
coef.alea_fit <- function(object, ...) {
  object$parameters
}
