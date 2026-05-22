#' Diagnostics for hydrological frequency analysis samples
#'
#' Computes diagnostic checks for samples used in ALEA frequency analysis
#' workflows.
#'
#' Diagnostics are intended to flag potential data issues. They do not replace
#' formal hydrological judgment and do not automatically invalidate a fitted
#' model.
#'
#' The initial diagnostics include simple sample checks and hypothesis-based
#' diagnostics implemented through the CRAN package `trend` when available.
#'
#' @param object A numeric vector or an object returned by `alea_fit`.
#' @param diagnostics Character vector with diagnostics to compute. Supported
#'   values are `"sample_size"`, `"missing"`, `"ties"`, `"range"`,
#'   `"skewness"`, `"randomness"`, `"independence"`, `"homogeneity"`, and
#'   `"stationarity"`. Use `"all"` to compute all supported diagnostics.
#' @param min_n Minimum sample size used by the sample-size diagnostic.
#' @param alpha Significance level used by hypothesis-test diagnostics.
#' @param ... Additional arguments passed to methods. Currently unused.
#'
#' @return A data frame with S3 class `alea_diagnostics`.
#'
#' @export
alea_diagnostics <- function(
    object,
    diagnostics = c(
      "sample_size", "missing", "ties", "range", "skewness",
      "randomness", "independence", "homogeneity", "stationarity"
    ),
    min_n = 10L,
    alpha = 0.05,
    ...
) {
  UseMethod("alea_diagnostics")
}


#' @rdname alea_diagnostics
#' @export
alea_diagnostics.numeric <- function(
    object,
    diagnostics = c(
      "sample_size", "missing", "ties", "range", "skewness",
      "randomness", "independence", "homogeneity", "stationarity"
    ),
    min_n = 10L,
    alpha = 0.05,
    ...
) {
  diagnostics <- check_diagnostics_names(diagnostics)
  min_n <- check_diagnostics_min_n(min_n)
  alpha <- check_diagnostics_alpha(alpha)
  
  out <- compute_sample_diagnostics(
    x = object,
    diagnostics = diagnostics,
    min_n = min_n,
    alpha = alpha,
    distribution = NA_character_,
    method = NA_character_
  )
  
  class(out) <- c("alea_diagnostics", "data.frame")
  out
}


#' @rdname alea_diagnostics
#' @export
alea_diagnostics.alea_fit <- function(
    object,
    diagnostics = c(
      "sample_size", "missing", "ties", "range", "skewness",
      "randomness", "independence", "homogeneity", "stationarity"
    ),
    min_n = 10L,
    alpha = 0.05,
    ...
) {
  check_diagnostics_fit_object(object)
  
  diagnostics <- check_diagnostics_names(diagnostics)
  min_n <- check_diagnostics_min_n(min_n)
  alpha <- check_diagnostics_alpha(alpha)
  
  out <- compute_sample_diagnostics(
    x = object$data,
    diagnostics = diagnostics,
    min_n = min_n,
    alpha = alpha,
    distribution = object$distribution,
    method = object$method
  )
  
  class(out) <- c("alea_diagnostics", "data.frame")
  out
}


#' @export
print.alea_diagnostics <- function(x, ...) {
  cat("ALEA diagnostics\n")
  
  distribution <- unique(x$distribution)
  method <- unique(x$method)
  
  if (length(distribution) == 1L && !is.na(distribution)) {
    cat("Distribution:", distribution, "\n")
  }
  
  if (length(method) == 1L && !is.na(method)) {
    cat("Estimation method:", method, "\n")
  }
  
  cat("Number of observations:", unique(x$n), "\n\n")
  
  print.data.frame(as.data.frame(x), row.names = FALSE, ...)
  invisible(x)
}


#' @export
as.data.frame.alea_diagnostics <- function(x, ...) {
  class(x) <- "data.frame"
  x
}


check_diagnostics_names <- function(diagnostics) {
  allowed <- c(
    "sample_size",
    "missing",
    "ties",
    "range",
    "skewness",
    "randomness",
    "independence",
    "homogeneity",
    "stationarity"
  )
  
  if (is.null(diagnostics) || length(diagnostics) == 0L) {
    stop("`diagnostics` must contain at least one diagnostic.", call. = FALSE)
  }
  
  if (!is.character(diagnostics)) {
    stop("`diagnostics` must be a character vector.", call. = FALSE)
  }
  
  diagnostics <- tolower(diagnostics)
  
  if (any(diagnostics == "all")) {
    diagnostics <- allowed
  }
  
  unknown <- setdiff(diagnostics, allowed)
  
  if (length(unknown) > 0L) {
    stop(
      "Unsupported diagnostic(s): ",
      paste(unknown, collapse = ", "),
      ". Supported values are: ",
      paste(allowed, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  unique(diagnostics)
}


check_diagnostics_min_n <- function(min_n) {
  if (!is.numeric(min_n) || length(min_n) != 1L || !is.finite(min_n)) {
    stop("`min_n` must be a single finite numeric value.", call. = FALSE)
  }
  
  if (min_n < 2) {
    stop("`min_n` must be greater than or equal to 2.", call. = FALSE)
  }
  
  as.integer(min_n)
}


check_diagnostics_alpha <- function(alpha) {
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha)) {
    stop("`alpha` must be a single finite numeric value.", call. = FALSE)
  }
  
  if (alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be in the open interval (0, 1).", call. = FALSE)
  }
  
  as.numeric(alpha)
}


check_diagnostics_fit_object <- function(object) {
  if (!inherits(object, "alea_fit")) {
    stop("`object` must be an object of class 'alea_fit'.", call. = FALSE)
  }
  
  required_fields <- c("data", "distribution", "method", "parameters", "n")
  missing_fields <- setdiff(required_fields, names(object))
  
  if (length(missing_fields) > 0L) {
    stop(
      "`object` is missing required field(s): ",
      paste(missing_fields, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if (!is.numeric(object$data) || length(object$data) == 0L) {
    stop("`object$data` must be a non-empty numeric vector.", call. = FALSE)
  }
  
  invisible(TRUE)
}


compute_sample_diagnostics <- function(
    x,
    diagnostics,
    min_n,
    alpha,
    distribution = NA_character_,
    method = NA_character_
) {
  if (!is.numeric(x)) {
    stop("`x` must be numeric.", call. = FALSE)
  }
  
  n_total <- length(x)
  n_missing <- sum(is.na(x))
  n_nonfinite <- sum(!is.na(x) & !is.finite(x))
  x_valid <- x[is.finite(x)]
  
  rows <- list()
  
  if ("sample_size" %in% diagnostics) {
    rows[[length(rows) + 1L]] <- diagnostic_sample_size_row(
      x_valid = x_valid,
      min_n = min_n,
      distribution = distribution,
      method = method,
      n_total = n_total
    )
  }
  
  if ("missing" %in% diagnostics) {
    rows[[length(rows) + 1L]] <- diagnostic_missing_row(
      n_missing = n_missing,
      n_nonfinite = n_nonfinite,
      distribution = distribution,
      method = method,
      n_total = n_total,
      n_valid = length(x_valid)
    )
  }
  
  if ("ties" %in% diagnostics) {
    rows[[length(rows) + 1L]] <- diagnostic_ties_row(
      x_valid = x_valid,
      distribution = distribution,
      method = method,
      n_total = n_total
    )
  }
  
  if ("range" %in% diagnostics) {
    rows[[length(rows) + 1L]] <- diagnostic_range_row(
      x_valid = x_valid,
      distribution = distribution,
      method = method,
      n_total = n_total
    )
  }
  
  if ("skewness" %in% diagnostics) {
    rows[[length(rows) + 1L]] <- diagnostic_skewness_row(
      x_valid = x_valid,
      distribution = distribution,
      method = method,
      n_total = n_total
    )
  }
  
  if ("randomness" %in% diagnostics) {
    rows[[length(rows) + 1L]] <- diagnostic_randomness_row(
      x_valid = x_valid,
      alpha = alpha,
      distribution = distribution,
      method = method,
      n_total = n_total
    )
  }
  
  if ("independence" %in% diagnostics) {
    rows[[length(rows) + 1L]] <- diagnostic_independence_row(
      x_valid = x_valid,
      alpha = alpha,
      distribution = distribution,
      method = method,
      n_total = n_total
    )
  }
  
  if ("homogeneity" %in% diagnostics) {
    rows[[length(rows) + 1L]] <- diagnostic_homogeneity_row(
      x_valid = x_valid,
      alpha = alpha,
      distribution = distribution,
      method = method,
      n_total = n_total
    )
  }
  
  if ("stationarity" %in% diagnostics) {
    rows[[length(rows) + 1L]] <- diagnostic_stationarity_row(
      x_valid = x_valid,
      alpha = alpha,
      distribution = distribution,
      method = method,
      n_total = n_total
    )
  }
  
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


new_diagnostic_row <- function(
    distribution,
    method,
    diagnostic,
    statistic,
    value,
    p_value = NA_real_,
    alpha = NA_real_,
    reject = NA,
    threshold,
    status,
    message,
    n,
    n_valid
) {
  data.frame(
    distribution = as.character(distribution),
    method = as.character(method),
    diagnostic = diagnostic,
    statistic = statistic,
    value = as.numeric(value),
    p_value = as.numeric(p_value),
    alpha = as.numeric(alpha),
    reject = as.logical(reject),
    threshold = threshold,
    status = status,
    message = message,
    n = as.integer(n),
    n_valid = as.integer(n_valid),
    stringsAsFactors = FALSE
  )
}


diagnostic_sample_size_row <- function(
    x_valid,
    min_n,
    distribution,
    method,
    n_total
) {
  n_valid <- length(x_valid)
  
  if (n_valid >= min_n) {
    status <- "ok"
    message <- "Sample size is acceptable for initial frequency analysis."
  } else {
    status <- "warning"
    message <- "Sample size is smaller than the recommended minimum for initial frequency analysis."
  }
  
  new_diagnostic_row(
    distribution = distribution,
    method = method,
    diagnostic = "sample_size",
    statistic = "n_valid",
    value = n_valid,
    threshold = paste0(">= ", min_n),
    status = status,
    message = message,
    n = n_total,
    n_valid = n_valid
  )
}


diagnostic_missing_row <- function(
    n_missing,
    n_nonfinite,
    distribution,
    method,
    n_total,
    n_valid
) {
  n_problem <- n_missing + n_nonfinite
  
  if (n_problem == 0L) {
    status <- "ok"
    message <- "No missing or non-finite values detected."
  } else {
    status <- "warning"
    message <- "Missing or non-finite values were detected and should be reviewed."
  }
  
  new_diagnostic_row(
    distribution = distribution,
    method = method,
    diagnostic = "missing",
    statistic = "n_missing_or_nonfinite",
    value = n_problem,
    threshold = "== 0",
    status = status,
    message = message,
    n = n_total,
    n_valid = n_valid
  )
}


diagnostic_ties_row <- function(
    x_valid,
    distribution,
    method,
    n_total
) {
  n_valid <- length(x_valid)
  
  if (n_valid == 0L) {
    n_ties <- NA_real_
    status <- "fail"
    message <- "No finite observations are available to evaluate ties."
  } else {
    tab <- table(x_valid)
    n_ties <- sum(tab[tab > 1L]) - sum(tab > 1L)
    
    if (n_ties == 0L) {
      status <- "ok"
      message <- "No tied values detected."
    } else {
      status <- "warning"
      message <- "Tied values were detected and should be reviewed."
    }
  }
  
  new_diagnostic_row(
    distribution = distribution,
    method = method,
    diagnostic = "ties",
    statistic = "n_ties",
    value = n_ties,
    threshold = "== 0",
    status = status,
    message = message,
    n = n_total,
    n_valid = n_valid
  )
}


diagnostic_range_row <- function(
    x_valid,
    distribution,
    method,
    n_total
) {
  n_valid <- length(x_valid)
  n_unique <- length(unique(x_valid))
  
  if (n_valid == 0L) {
    value <- NA_real_
    status <- "fail"
    message <- "No finite observations are available to evaluate sample range."
  } else if (n_unique <= 1L) {
    value <- n_unique
    status <- "fail"
    message <- "Sample has one or fewer unique finite values."
  } else {
    value <- n_unique
    status <- "ok"
    message <- "Sample has more than one unique finite value."
  }
  
  new_diagnostic_row(
    distribution = distribution,
    method = method,
    diagnostic = "range",
    statistic = "n_unique",
    value = value,
    threshold = "> 1",
    status = status,
    message = message,
    n = n_total,
    n_valid = n_valid
  )
}


diagnostic_skewness_row <- function(
    x_valid,
    distribution,
    method,
    n_total
) {
  n_valid <- length(x_valid)
  
  if (n_valid < 3L) {
    skew <- NA_real_
    status <- "warning"
    message <- "At least three finite observations are required to evaluate sample skewness."
  } else {
    skew <- sample_skewness_diagnostic(x_valid)
    
    if (is.finite(skew)) {
      status <- "ok"
      message <- "Sample skewness is finite."
    } else {
      status <- "warning"
      message <- "Sample skewness is not finite and should be reviewed."
    }
  }
  
  new_diagnostic_row(
    distribution = distribution,
    method = method,
    diagnostic = "skewness",
    statistic = "sample_skewness",
    value = skew,
    threshold = "finite",
    status = status,
    message = message,
    n = n_total,
    n_valid = n_valid
  )
}


diagnostic_randomness_row <- function(
    x_valid,
    alpha,
    distribution,
    method,
    n_total
) {
  n_valid <- length(x_valid)
  
  if (n_valid < 5L) {
    return(new_hypothesis_diagnostic_unavailable_row(
      distribution = distribution,
      method = method,
      diagnostic = "randomness",
      statistic = "bartels_rank_von_neumann",
      alpha = alpha,
      n = n_total,
      n_valid = n_valid,
      message = "At least five finite observations are recommended for the Bartels randomness test."
    ))
  }
  
  result <- run_trend_test(
    test_fun = "bartels.test",
    x = x_valid
  )
  
  hypothesis_diagnostic_row_from_htest(
    result = result,
    distribution = distribution,
    method = method,
    diagnostic = "randomness",
    statistic = "bartels_rank_von_neumann",
    alpha = alpha,
    n = n_total,
    n_valid = n_valid,
    ok_message = "The Bartels test does not indicate non-randomness.",
    warning_message = "The Bartels test suggests possible non-randomness.",
    unavailable_message = "The Bartels randomness test requires the suggested package 'trend'."
  )
}


diagnostic_independence_row <- function(
    x_valid,
    alpha,
    distribution,
    method,
    n_total
) {
  n_valid <- length(x_valid)
  
  if (n_valid < 4L) {
    return(new_hypothesis_diagnostic_unavailable_row(
      distribution = distribution,
      method = method,
      diagnostic = "independence",
      statistic = "wald_wolfowitz",
      alpha = alpha,
      n = n_total,
      n_valid = n_valid,
      message = "At least four finite observations are required for the Wald-Wolfowitz independence test."
    ))
  }
  
  result <- run_trend_test(
    test_fun = "ww.test",
    x = x_valid
  )
  
  hypothesis_diagnostic_row_from_htest(
    result = result,
    distribution = distribution,
    method = method,
    diagnostic = "independence",
    statistic = "wald_wolfowitz",
    alpha = alpha,
    n = n_total,
    n_valid = n_valid,
    ok_message = "The Wald-Wolfowitz test does not indicate serial dependence.",
    warning_message = "The Wald-Wolfowitz test suggests possible serial dependence.",
    unavailable_message = "The Wald-Wolfowitz independence test requires the suggested package 'trend'."
  )
}


diagnostic_homogeneity_row <- function(
    x_valid,
    alpha,
    distribution,
    method,
    n_total
) {
  n_valid <- length(x_valid)
  
  if (n_valid < 6L) {
    return(new_hypothesis_diagnostic_unavailable_row(
      distribution = distribution,
      method = method,
      diagnostic = "homogeneity",
      statistic = "pettitt_change_point",
      alpha = alpha,
      n = n_total,
      n_valid = n_valid,
      message = "At least six finite observations are recommended for the Pettitt homogeneity test."
    ))
  }
  
  result <- run_trend_test(
    test_fun = "pettitt.test",
    x = x_valid
  )
  
  hypothesis_diagnostic_row_from_htest(
    result = result,
    distribution = distribution,
    method = method,
    diagnostic = "homogeneity",
    statistic = "pettitt_change_point",
    alpha = alpha,
    n = n_total,
    n_valid = n_valid,
    ok_message = "The Pettitt test does not indicate a change point.",
    warning_message = "The Pettitt test suggests a possible change point.",
    unavailable_message = "The Pettitt homogeneity test requires the suggested package 'trend'."
  )
}


diagnostic_stationarity_row <- function(
    x_valid,
    alpha,
    distribution,
    method,
    n_total
) {
  n_valid <- length(x_valid)
  
  if (n_valid < 4L) {
    return(new_hypothesis_diagnostic_unavailable_row(
      distribution = distribution,
      method = method,
      diagnostic = "stationarity",
      statistic = "mann_kendall",
      alpha = alpha,
      n = n_total,
      n_valid = n_valid,
      message = "At least four finite observations are required for the Mann-Kendall stationarity test."
    ))
  }
  
  result <- run_trend_test(
    test_fun = "mk.test",
    x = x_valid
  )
  
  hypothesis_diagnostic_row_from_htest(
    result = result,
    distribution = distribution,
    method = method,
    diagnostic = "stationarity",
    statistic = "mann_kendall",
    alpha = alpha,
    n = n_total,
    n_valid = n_valid,
    ok_message = "The Mann-Kendall test does not indicate a monotonic trend.",
    warning_message = "The Mann-Kendall test suggests a possible monotonic trend.",
    unavailable_message = "The Mann-Kendall stationarity test requires the suggested package 'trend'."
  )
}


run_trend_test <- function(test_fun, x) {
  if (!requireNamespace("trend", quietly = TRUE)) {
    return(list(
      available = FALSE,
      statistic = NA_real_,
      p_value = NA_real_,
      message = "The suggested package 'trend' is not installed."
    ))
  }
  
  fun <- get(test_fun, envir = asNamespace("trend"))
  
  result <- tryCatch(
    fun(x),
    error = function(e) e
  )
  
  if (inherits(result, "error")) {
    return(list(
      available = TRUE,
      statistic = NA_real_,
      p_value = NA_real_,
      message = conditionMessage(result)
    ))
  }
  
  list(
    available = TRUE,
    statistic = extract_htest_statistic(result),
    p_value = extract_htest_p_value(result),
    message = NA_character_
  )
}


extract_htest_statistic <- function(result) {
  statistic <- result$statistic
  
  if (is.null(statistic) || length(statistic) == 0L) {
    return(NA_real_)
  }
  
  statistic <- as.numeric(statistic[1L])
  
  if (!is.finite(statistic)) {
    return(NA_real_)
  }
  
  statistic
}


extract_htest_p_value <- function(result) {
  p_value <- result$p.value
  
  if (is.null(p_value) || length(p_value) == 0L) {
    return(NA_real_)
  }
  
  p_value <- as.numeric(p_value[1L])
  
  if (!is.finite(p_value)) {
    return(NA_real_)
  }
  
  p_value
}


hypothesis_diagnostic_row_from_htest <- function(
    result,
    distribution,
    method,
    diagnostic,
    statistic,
    alpha,
    n,
    n_valid,
    ok_message,
    warning_message,
    unavailable_message
) {
  if (!isTRUE(result$available)) {
    return(new_hypothesis_diagnostic_unavailable_row(
      distribution = distribution,
      method = method,
      diagnostic = diagnostic,
      statistic = statistic,
      alpha = alpha,
      n = n,
      n_valid = n_valid,
      message = unavailable_message
    ))
  }
  
  if (is.na(result$p_value)) {
    return(new_diagnostic_row(
      distribution = distribution,
      method = method,
      diagnostic = diagnostic,
      statistic = statistic,
      value = result$statistic,
      p_value = NA_real_,
      alpha = alpha,
      reject = NA,
      threshold = paste0("p >= ", alpha),
      status = "warning",
      message = result$message,
      n = n,
      n_valid = n_valid
    ))
  }
  
  reject <- result$p_value < alpha
  status <- if (reject) "warning" else "ok"
  message <- if (reject) warning_message else ok_message
  
  new_diagnostic_row(
    distribution = distribution,
    method = method,
    diagnostic = diagnostic,
    statistic = statistic,
    value = result$statistic,
    p_value = result$p_value,
    alpha = alpha,
    reject = reject,
    threshold = paste0("p >= ", alpha),
    status = status,
    message = message,
    n = n,
    n_valid = n_valid
  )
}


new_hypothesis_diagnostic_unavailable_row <- function(
    distribution,
    method,
    diagnostic,
    statistic,
    alpha,
    n,
    n_valid,
    message
) {
  new_diagnostic_row(
    distribution = distribution,
    method = method,
    diagnostic = diagnostic,
    statistic = statistic,
    value = NA_real_,
    p_value = NA_real_,
    alpha = alpha,
    reject = NA,
    threshold = paste0("p >= ", alpha),
    status = "warning",
    message = message,
    n = n,
    n_valid = n_valid
  )
}


sample_skewness_diagnostic <- function(x) {
  x <- as.numeric(x)
  n <- length(x)
  
  if (n < 3L) {
    return(NA_real_)
  }
  
  s <- stats::sd(x)
  
  if (!is.finite(s) || s == 0) {
    return(NA_real_)
  }
  
  m <- mean(x)
  sum(((x - m) / s)^3) * n / ((n - 1) * (n - 2))
}