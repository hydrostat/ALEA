
#' Return levels for ALEA fitted models
#'
#' Computes return levels for fitted ALEA models.
#'
#' @param object An object returned by `alea_fit`.
#' @param return_period Numeric vector of return periods. Values must be finite
#'   and greater than 1.
#' @param ... Additional arguments reserved for future methods.
#'
#' @return A data frame with class `alea_return_level`.
#'
#' @export
alea_return_level <- function(object, return_period, ...) {
  UseMethod("alea_return_level")
}


#' @export
alea_return_level.alea_fit <- function(object, return_period, ...) {
  validate_alea_fit(object)
  check_return_period(return_period)
  return_period <- as.numeric(return_period)
  
  distribution <- object$distribution
  method <- object$method
  parameters <- coef(object)
  
  probability <- 1 - 1 / return_period
  
  return_level <- return_level_from_fit_internal(
    distribution = distribution,
    return_period = return_period,
    parameters = parameters
  )
  
  out <- data.frame(
    distribution = rep(distribution, length(return_period)),
    method = rep(method, length(return_period)),
    return_period = return_period,
    probability = probability,
    return_level = as.numeric(return_level),
    stringsAsFactors = FALSE
  )
  
  param_df <- parameters_to_columns_internal(parameters, n = nrow(out))
  out <- cbind(out, param_df)
  
  class(out) <- c("alea_return_level", "data.frame")
  out
}


#' @export
print.alea_return_level <- function(x, ...) {
  cat("ALEA return levels\n")
  cat("Distribution:", unique(x$distribution), "\n")
  cat("Method:", unique(x$method), "\n\n")
  print.data.frame(x, row.names = FALSE, ...)
  invisible(x)
}


#' @export
as.data.frame.alea_return_level <- function(x, ...) {
  class(x) <- "data.frame"
  x
}


check_return_period <- function(return_period) {
  if (missing(return_period)) {
    stop("`return_period` must be provided.", call. = FALSE)
  }
  
  if (!is.numeric(return_period)) {
    stop("`return_period` must be numeric.", call. = FALSE)
  }
  
  if (length(return_period) == 0L) {
    stop("`return_period` must not be empty.", call. = FALSE)
  }
  
  if (anyNA(return_period)) {
    stop("`return_period` must not contain missing values.", call. = FALSE)
  }
  
  if (any(!is.finite(return_period))) {
    stop("`return_period` must contain only finite values.", call. = FALSE)
  }
  
  if (any(return_period <= 1)) {
    stop("`return_period` must contain values greater than 1.", call. = FALSE)
  }
  
  return_period
}


return_level_from_fit_internal <- function(distribution, return_period, parameters) {
  distribution <- tolower(distribution)
  
  valid_distributions <- c("gev", "gpa", "pe3", "ln2", "ln3", "gum")
  
  if (!distribution %in% valid_distributions) {
    stop("Unsupported distribution.", call. = FALSE)
  }
  
  rlfun <- switch(
    distribution,
    gev = return_level_gev_internal,
    gpa = return_level_gpa_internal,
    pe3 = return_level_pe3_internal,
    ln2 = return_level_ln2_internal,
    ln3 = return_level_ln3_internal,
    gum = return_level_gum_internal
  )
  
  rlfun(return_period = return_period, para = parameters)
}

parameters_to_columns_internal <- function(parameters, n) {
  param_list <- as.list(parameters)
  
  param_df <- as.data.frame(
    lapply(param_list, function(value) rep(as.numeric(value), n)),
    stringsAsFactors = FALSE
  )
  
  names(param_df) <- names(param_list)
  param_df
}
