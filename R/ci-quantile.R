# Quantile confidence intervals.


#' Confidence intervals for ALEA fitted models
#'
#' Computes percentile bootstrap confidence intervals for quantiles from ALEA
#' fitted models.
#'
#' @param object An object returned by `alea_fit()` or `alea_compare()`.
#' @param parm Character scalar. Use `"quantile"`.
#' @param level Confidence level. Must be a single number between 0 and 1.
#' @param return_period Numeric vector of return periods. Values must be finite
#'   and greater than 1.
#' @param method Character scalar. Use `"bootstrap"`.
#' @param n_boot Number of bootstrap resamples.
#' @param seed Optional integer seed for reproducibility. If `NULL`, the current
#'   random-number state is used.
#' @param ... Additional arguments passed to methods. Not used by the current
#'   methods.
#'
#' @return A data frame with class `alea_quantile_ci`. The table includes
#'   fitted quantiles, confidence limits, bootstrap counts, and standardized
#'   parameter columns.
#'
#' @export
confint.alea_fit <- function(object,
                             parm = "quantile",
                             level = 0.95,
                             return_period,
                             method = "bootstrap",
                             n_boot = 500,
                             seed = NULL,
                             ...) {
  validate_alea_fit(object)
  
  if (!identical(parm, "quantile")) {
    stop("Only parm = 'quantile' is currently supported.", call. = FALSE)
  }
  
  method <- match.arg(method, choices = "bootstrap")
  
  ci_bootstrap_quantile(
    object = object,
    return_period = return_period,
    level = level,
    n_boot = n_boot,
    seed = seed,
    ...
  )
}


ci_bootstrap_quantile <- function(object,
                                      return_period,
                                      level = 0.95,
                                      n_boot = 500,
                                      seed = NULL,
                                      ...) {
  validate_alea_fit(object)
  check_conf_level(level)
  check_n_boot(n_boot)
  
  check_return_period(return_period)
  return_period <- as.numeric(return_period)
  
  x <- extract_alea_fit_data(object)
  distribution <- object$distribution
  method <- object$method
  
  point <- alea_quantile(object, return_period = return_period)
  
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) || !is.finite(seed)) {
      stop("`seed` must be a single finite number or NULL.", call. = FALSE)
    }
    
    old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (old_seed_exists) {
      old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    }
    
    on.exit({
      if (old_seed_exists) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    
    set.seed(seed)
  }
  
  boot_matrix <- matrix(
    NA_real_,
    nrow = n_boot,
    ncol = length(return_period)
  )
  
  failed <- character()
  
  n <- length(x)
  
  for (i in seq_len(n_boot)) {
    xb <- sample(x, size = n, replace = TRUE)
    
    fit_b <- tryCatch(
      alea_fit(
        xb,
        distribution = distribution,
        method = method,
        ...
      ),
      error = function(e) e
    )
    
    if (inherits(fit_b, "error")) {
      failed <- c(failed, conditionMessage(fit_b))
      next
    }
    
    rl_b <- tryCatch(
      alea_quantile(fit_b, return_period = return_period),
      error = function(e) e
    )
    
    if (inherits(rl_b, "error")) {
      failed <- c(failed, conditionMessage(rl_b))
      next
    }
    
    boot_matrix[i, ] <- rl_b$quantile
  }
  
  n_success <- colSums(is.finite(boot_matrix))
  n_failed <- n_boot - n_success
  
  alpha <- 1 - level
  lower_prob <- alpha / 2
  upper_prob <- 1 - alpha / 2
  
  lower <- apply(
    boot_matrix,
    2L,
    stats::quantile,
    probs = lower_prob,
    na.rm = TRUE,
    names = FALSE,
    type = 7
  )
  
  upper <- apply(
    boot_matrix,
    2L,
    stats::quantile,
    probs = upper_prob,
    na.rm = TRUE,
    names = FALSE,
    type = 7
  )
  
  out <- data.frame(
    distribution = point$distribution,
    method = point$method,
    return_period = point$return_period,
    probability = point$probability,
    quantile = point$quantile,
    conf_level = rep(level, nrow(point)),
    conf_method = rep("bootstrap", nrow(point)),
    lower = as.numeric(lower),
    upper = as.numeric(upper),
    n_boot = rep(n_boot, nrow(point)),
    n_success = as.integer(n_success),
    n_failed = as.integer(n_failed),
    stringsAsFactors = FALSE
  )
  
  param_cols <- setdiff(
    names(point),
    c("distribution", "method", "return_period", "probability", "quantile")
  )
  
  if (length(param_cols) > 0L) {
    out <- cbind(out, point[param_cols])
  }
  
  if (any(n_success == 0L)) {
    warning(
      "All bootstrap resamples failed for at least one return period.",
      call. = FALSE
    )
  } else if (any(n_failed > 0L)) {
    warning(
      "Some bootstrap resamples failed. See `n_success` and `n_failed`.",
      call. = FALSE
    )
  }
  
  attr(out, "data") <- object$data
  attr(out, "observed_data") <- object$data
  attr(out, "bootstrap_failures") <- failed
  class(out) <- c("alea_quantile_ci", "data.frame")
  out
}


#' @export
print.alea_quantile_ci <- function(x, digits = 4, max_rows = 20, ...) {
  df <- as.data.frame(x)

  cat("ALEA quantile confidence intervals\n")

  distributions <- unique(df$distribution)
  methods <- unique(df$method)
  conf_methods <- unique(df$conf_method)
  conf_levels <- unique(df$conf_level)
  model_keys <- unique(paste(df$distribution, df$method, sep = "|"))
  n_models <- length(model_keys)

  conf_method_label <- paste(conf_methods, collapse = ", ")
  conf_level_label <- paste(round(conf_levels, digits = digits), collapse = ", ")

  if (n_models == 1L) {
    cat("Distribution:", distributions, "\n")
    cat("Method:", methods, "\n")
    cat("Confidence method:", conf_method_label, "\n")
    cat("Confidence level:", conf_level_label, "\n")

    n_success <- unique(df$n_success)
    n_failed <- unique(df$n_failed)
    if (length(n_success) == 1L && length(n_failed) == 1L) {
      cat(
        "Bootstrap replicates:",
        n_success,
        "successful,",
        n_failed,
        "failed\n"
      )
    }

    cat("\n")

    out <- df[, c("return_period", "probability", "quantile", "lower", "upper"), drop = FALSE]
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
    cat("Confidence method:", conf_method_label, "\n")
    cat("Confidence level:", conf_level_label, "\n")
    cat("Models:", n_models, "\n")
    cat("Rows:", nrow(df), "\n\n")

    out <- df[, c("distribution", "method", "return_period", "quantile", "lower", "upper"), drop = FALSE]
    out <- round_numeric_columns_internal(out, digits = digits)
    out <- compact_print_rows_internal(out, max_rows = max_rows)
    print.data.frame(out, row.names = FALSE, ...)
  }

  cat("\nUse as.data.frame(x) for the full confidence-interval table.\n")

  invisible(x)
}


#' @export
as.data.frame.alea_quantile_ci <- function(x, ...) {
  class(x) <- "data.frame"
  x
}


check_conf_level <- function(level) {
  if (!is.numeric(level) || length(level) != 1L || is.na(level) || !is.finite(level)) {
    stop("`level` must be a single finite number.", call. = FALSE)
  }
  
  if (level <= 0 || level >= 1) {
    stop("`level` must be greater than 0 and less than 1.", call. = FALSE)
  }
  
  invisible(TRUE)
}


check_n_boot <- function(n_boot) {
  if (!is.numeric(n_boot) || length(n_boot) != 1L || is.na(n_boot) || !is.finite(n_boot)) {
    stop("`n_boot` must be a single finite number.", call. = FALSE)
  }
  
  if (n_boot < 1) {
    stop("`n_boot` must be greater than or equal to 1.", call. = FALSE)
  }
  
  if (n_boot != as.integer(n_boot)) {
    stop("`n_boot` must be an integer.", call. = FALSE)
  }
  
  invisible(TRUE)
}


extract_alea_fit_data <- function(object) {
  candidates <- c("x", "data", "input")
  
  for (candidate in candidates) {
    if (!is.null(object[[candidate]]) && is.numeric(object[[candidate]])) {
      x <- object[[candidate]]
      x <- x[is.finite(x)]
      
      if (length(x) >= 2L) {
        return(as.numeric(x))
      }
    }
  }
  
  stop(
    "The `alea_fit` object does not contain the original numeric data. ",
    "Store the input series in the fitted object, preferably as `x`.",
    call. = FALSE
  )
}
