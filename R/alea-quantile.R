# Quantiles for ALEA fitted models.

#' Quantiles for ALEA fitted models
#'
#' Computes hydrological quantiles associated with return periods for fitted
#' ALEA models.
#'
#' @param object An object returned by `alea_fit()` or `alea_compare()`.
#' @param return_period Numeric vector of return periods. Values must be finite
#'   and greater than 1.
#' @param ... Additional arguments passed to methods.
#'
#' @return A data frame with class `alea_quantile`. The table includes
#'   `distribution`, `method`, `return_period`, `probability`, `quantile`, and
#'   standardized parameter columns (`location`, `scale`, and `shape`).
#'
#' @details
#' The probability associated with a return period is computed as
#' `probability = 1 - 1 / return_period`.
#'
#' @examples
#' x <- c(42.1, 38.5, 51.3, 47.0, 62.4, 55.2, 49.8, 58.1,
#'        60.3, 45.9)
#' fit <- alea_fit(x, distribution = "gev", method = "lmom")
#' q <- alea_quantile(fit, return_period = c(10, 25, 50))
#' q
#' plot(q)
#'
#' @export
alea_quantile <- function(object, return_period, ...) {
  UseMethod("alea_quantile")
}

#' @export
alea_quantile.alea_fit <- function(object, return_period, ...) {
  validate_alea_fit(object)
  check_return_period(return_period)
  return_period <- as.numeric(return_period)

  distribution <- object$distribution
  method <- object$method
  parameters <- object$parameters

  probability <- 1 - 1 / return_period

  q <- quantile_from_fit_internal(
    distribution = distribution,
    return_period = return_period,
    parameters = parameters
  )

  out <- data.frame(
    distribution = rep(distribution, length(return_period)),
    method = rep(method, length(return_period)),
    return_period = return_period,
    probability = probability,
    quantile = as.numeric(q),
    stringsAsFactors = FALSE
  )

  param_df <- standardized_parameter_columns_internal(
    distribution = distribution,
    parameters = parameters,
    n = nrow(out)
  )
  out <- cbind(out, param_df)

  attr(out, "data") <- object$data
  attr(out, "observed_data") <- object$data
  class(out) <- c("alea_quantile", "data.frame")
  out
}

#' @export
print.alea_quantile <- function(x, digits = 4, max_rows = 20, ...) {
  df <- as.data.frame(x)

  cat("ALEA quantiles\n")

  distributions <- unique(df$distribution)
  methods <- unique(df$method)
  model_keys <- unique(paste(df$distribution, df$method, sep = "|"))
  n_models <- length(model_keys)

  if (n_models == 1L) {
    cat("Distribution:", distributions, "\n")
    cat("Method:", methods, "\n\n")

    out <- df[, c("return_period", "probability", "quantile"), drop = FALSE]
    out <- round_numeric_columns_internal(out, digits = digits)
    print.data.frame(out, row.names = FALSE, ...)

    parameter_cols <- intersect(c("location", "scale", "shape"), names(df))
    if (length(parameter_cols) > 0L) {
      parameters <- unique(df[parameter_cols])
      parameters <- round_numeric_columns_internal(parameters, digits = digits)
      cat("\nParameters:\n")
      print.data.frame(parameters, row.names = FALSE, ...)
    }
  } else {
    cat("Distributions:", paste(distributions, collapse = ", "), "\n")
    cat("Methods:", paste(methods, collapse = ", "), "\n")
    cat("Models:", n_models, "\n")
    cat("Rows:", nrow(df), "\n\n")

    out <- df[, c("distribution", "method", "return_period", "quantile"), drop = FALSE]
    out <- round_numeric_columns_internal(out, digits = digits)
    out <- compact_print_rows_internal(out, max_rows = max_rows)
    print.data.frame(out, row.names = FALSE, ...)
  }

  cat("\nUse as.data.frame(x) for the full quantile table.\n")

  invisible(x)
}

round_numeric_columns_internal <- function(x, digits = 4) {
  numeric_cols <- vapply(x, is.numeric, logical(1))
  x[numeric_cols] <- lapply(x[numeric_cols], round, digits = digits)
  x
}

compact_print_rows_internal <- function(x, max_rows = 20) {
  if (is.null(max_rows) || !is.finite(max_rows) || max_rows <= 0L) {
    return(x)
  }

  max_rows <- as.integer(max_rows)

  if (nrow(x) <= max_rows) {
    return(x)
  }

  n_head <- max(1L, floor(max_rows / 2))
  n_tail <- max(1L, max_rows - n_head)

  ellipsis <- as.data.frame(
    as.list(rep("...", ncol(x))),
    stringsAsFactors = FALSE
  )
  names(ellipsis) <- names(x)

  rbind(
    utils::head(x, n_head),
    ellipsis,
    utils::tail(x, n_tail)
  )
}

#' @export
as.data.frame.alea_quantile <- function(x, ...) {
  class(x) <- "data.frame"
  x
}


check_return_period <- function(return_period) {
  msg <- "`return_period` must contain finite values greater than 1."

  if (missing(return_period)) {
    stop("`return_period` must be provided.", call. = FALSE)
  }

  if (!is.numeric(return_period)) {
    stop("`return_period` must be numeric.", call. = FALSE)
  }

  if (length(return_period) == 0L) {
    stop("`return_period` must not be empty.", call. = FALSE)
  }

  if (anyNA(return_period) || any(!is.finite(return_period)) || any(return_period <= 1)) {
    stop(msg, call. = FALSE)
  }

  return_period
}

quantile_from_fit_internal <- function(distribution, return_period, parameters) {
  distribution <- tolower(distribution)

  valid_distributions <- c("gev", "gpa", "pe3", "ln2", "ln3", "gum")

  if (!distribution %in% valid_distributions) {
    stop("Unsupported distribution.", call. = FALSE)
  }

  qfun <- switch(
    distribution,
    gev = quantile_gev_internal,
    gpa = quantile_gpa_internal,
    pe3 = quantile_pe3_internal,
    ln2 = quantile_ln2_internal,
    ln3 = quantile_ln3_internal,
    gum = quantile_gum_internal
  )

  qfun(return_period = return_period, para = parameters)
}

