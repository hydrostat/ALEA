#' Fit ALEA models
#'
#' `alea_fit()` is the main user-facing entry point for ALEA-R fitting
#' workflows. It dispatches according to the supplied data and arguments:
#'
#' * one numeric series and one distribution-method combination returns
#'   an `alea_fit` object;
#' * one numeric series and multiple distributions or methods returns
#'   an `alea_compare` object;
#' * a data frame with `station` and `value` columns returns an `alea_batch`
#'   object through the batch workflow.
#'
#' @param x A numeric vector with one hydrological series, or a data frame for
#'   batch analysis.
#' @param distribution Character scalar or vector. Distribution(s) to fit.
#'   Supported values are `"gev"`, `"gpa"`, `"pe3"`, `"ln2"`, `"ln3"`, and
#'   `"gum"`. For data-frame input, the default is all supported distributions.
#' @param method Character scalar or vector. Estimation method(s). Supported
#'   values are `"lmom"`, `"mom"`, and `"mle"`. The default is `"lmom"`.
#' @param return_period Optional numeric vector of return periods. If supplied,
#'   quantiles are computed and stored in the fitted object for convenience.
#' @param station,value,time Optional column names used when `x` is a data
#'   frame. Supplying `station` and `value` triggers the batch workflow.
#' @param gof Logical. For batch workflows, compute goodness-of-fit tables.
#' @param diagnostics Logical. For batch workflows, compute diagnostics tables.
#' @param select For batch workflows, use `"none"` or `"ai"`.
#' @param ai_model Optional pre-loaded FADS_AI light model for batch workflows.
#' @param ai_model_path Optional path to a FADS_AI light model file for batch
#'   workflows.
#' @param method_priority Character vector used by batch AI selection to choose
#'   the preferred fitted method.
#' @param quiet Logical. If `TRUE`, suppresses non-essential messages in
#'   multi-model or batch workflows.
#' @param ... Additional arguments passed to fitting routines.
#'
#' @return An `alea_fit`, `alea_compare`, or `alea_batch` object.
#'
#' @details
#' Fitted objects store the original numeric sample in the `data` field. This
#' sample is used by downstream workflows such as bootstrap quantile confidence
#' intervals, goodness-of-fit assessment, diagnostics, AI-assisted distribution
#' selection, and plotting.
#'
#' User-facing coefficient and quantile tables use standardized parameter
#' columns: `location`, `scale`, and `shape`. Use `alea_dist()` to see how
#' these standardized columns map to the internal distribution parameters.
#'
#' @examples
#' x <- c(42.1, 38.5, 51.3, 47.0, 62.4, 55.2, 49.8, 58.1)
#'
#' fit <- alea_fit(x, distribution = "gev", method = "lmom")
#' fit
#' coef(fit)
#'
#' cmp <- alea_fit(x, distribution = c("gev", "gum"), method = c("lmom", "mle"))
#' cmp
#'
#' q <- alea_quantile(fit, return_period = c(10, 25, 50, 100))
#' q
#'
#' @export
alea_fit <- function(x,
                     distribution,
                     method = "lmom",
                     return_period = NULL,
                     station = NULL,
                     value = NULL,
                     time = NULL,
                     gof = FALSE,
                     diagnostics = FALSE,
                     select = c("none", "ai"),
                     ai_model = NULL,
                     ai_model_path = NULL,
                     method_priority = c("lmom", "mle", "mom"),
                     quiet = FALSE,
                     ...) {
  distribution_was_missing <- missing(distribution)

  if (is.data.frame(x) || !is.null(station) || !is.null(value)) {
    if (!is.data.frame(x)) {
      stop("`x` must be a data frame when `station` or `value` is supplied.", call. = FALSE)
    }
    if (is.null(station) || is.null(value)) {
      stop("`station` and `value` must be supplied for batch workflows.", call. = FALSE)
    }
    distributions <- if (distribution_was_missing) {
      c("gev", "gpa", "pe3", "ln2", "ln3", "gum")
    } else {
      distribution
    }
    select <- match.arg(select)
    return(alea_batch_fit(
      data = x,
      station = station,
      value = value,
      time = time,
      distributions = distributions,
      methods = method,
      return_period = return_period,
      gof = gof,
      diagnostics = diagnostics,
      select = select,
      ai_model = ai_model,
      ai_model_path = ai_model_path,
      method_priority = method_priority,
      quiet = quiet,
      ...
    ))
  }

  if (distribution_was_missing) {
    stop("`distribution` must be supplied for single-series workflows.", call. = FALSE)
  }

  x <- check_numeric_vector(x, "x", min_length = 2L)

  if (length(distribution) != 1L || length(method) != 1L) {
    return(alea_compare(
      x = x,
      distributions = distribution,
      methods = method,
      return_period = return_period,
      quiet = quiet,
      ...
    ))
  }

  distribution <- check_distribution(distribution)
  method <- check_method(method)

  fit_fun <- get_fit_function_internal(distribution, method)
  fit_result <- fit_fun(x = x, ...)

  quantiles <- NULL

  if (!is.null(return_period)) {
    quantiles <- alea_quantile_from_params_internal(
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
    quantiles = quantiles,
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
      "Method `", method, "` is not available for distribution `",
      distribution,
      "`.",
      call. = FALSE
    )
  }

  fun
}

alea_quantile_from_params_internal <- function(distribution,
                                           parameters,
                                           return_period,
                                           ...) {
  name <- paste0("quantile_", distribution, "_internal")
  fun <- get0(name, mode = "function", inherits = TRUE)

  if (is.null(fun)) {
    stop(
      "Quantile calculation is not available for distribution `",
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
                         quantiles = NULL,
                         covariance = NULL,
                         diagnostics = NULL,
                         warnings = character()) {
  obj <- list(
    data = data,
    distribution = distribution,
    method = method,
    parameters = parameters,
    convergence = convergence,
    quantiles = quantiles,
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
  print(coef(x))

  if (!is.null(x$quantiles)) {
    cat("Quantiles:\n")
    print(x$quantiles)
  }

  invisible(x)
}

#' Extract fitted model coefficients
#'
#' @param object An `alea_fit` object.
#' @param type Character scalar. Use `"standard"` to return user-facing
#'   parameter names (`location`, `scale`, and `shape`). Use `"internal"` to
#'   return distribution-specific internal parameter names.
#' @param ... Further arguments ignored.
#'
#' @return A named numeric vector of fitted parameters.
#'
#' @export
coef.alea_fit <- function(object, type = c("standard", "internal"), ...) {
  type <- match.arg(type)
  if (type == "internal") {
    return(object$parameters)
  }
  standardize_parameters_internal(object$distribution, object$parameters)
}
