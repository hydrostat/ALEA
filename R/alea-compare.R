#' Compare multiple ALEA fitted models for one hydrological sample
#'
#' `alea_compare()` fits several distribution-method combinations to one
#' numeric hydrological sample. It is the single-site multi-model workflow in
#' ALEA-R. Use `alea_fit()` for one model, `alea_compare()` for several models
#' applied to one series, and `alea_batch_fit()` for several stations or sites.
#'
#' @param x A numeric vector with the observed sample.
#' @param distributions Character vector of distributions to fit. Supported
#'   values are `"gev"`, `"gpa"`, `"pe3"`, `"ln2"`, `"ln3"`, and `"gum"`.
#' @param methods Character vector of estimation methods. Supported values are
#'   `"lmom"`, `"mom"`, and `"mle"`. The default is `"lmom"`.
#' @param return_period Optional numeric vector of return periods. If supplied,
#'   quantiles are computed for successful models and stored in the object
#'   for convenience.
#' @param quiet Logical. If `TRUE`, suppresses non-essential messages.
#' @param ... Additional arguments passed to `alea_fit()` and downstream
#'   fitting routines.
#'
#' @return An object of class `alea_compare`.
#'
#' @details
#' Fitting failures for one distribution-method combination are recorded in the
#' `errors` table and do not stop successful models from being returned.
#'
#' The object stores successful `alea_fit` objects in `fit_objects` and a compact
#' attempted-model table in `fits`. Downstream methods such as
#' `alea_quantile()`, `alea_gof()`, and `confint()` combine results across
#' successful models and align distribution-specific parameter columns
#' automatically.
#'
#' @examples
#' x <- c(42.1, 38.5, 51.3, 47.0, 62.4, 55.2, 49.8, 58.1, 60.3, 45.9)
#'
#' cmp <- alea_compare(
#'   x,
#'   distributions = c("gev", "gum", "pe3"),
#'   methods = c("lmom", "mle")
#' )
#'
#' cmp
#' as.data.frame(cmp)
#' coef(cmp)
#'
#' quantiles <- alea_quantile(cmp, return_period = c(10, 25, 50, 100))
#' quantiles
#'
#' @export
alea_compare <- function(x,
                         distributions = c("gev", "gpa", "pe3", "ln2", "ln3", "gum"),
                         methods = "lmom",
                         return_period = NULL,
                         quiet = FALSE,
                         ...) {
  call <- match.call()
  x <- check_numeric_vector(x, "x", min_length = 2L)
  distributions <- check_distribution_vector_internal(distributions, "distributions")
  methods <- check_method_vector_internal(methods, "methods")

  if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet)) {
    stop("`quiet` must be `TRUE` or `FALSE`.", call. = FALSE)
  }

  if (!is.null(return_period)) {
    check_return_period(return_period)
    return_period <- as.numeric(return_period)
  }

  model_grid <- unique(expand.grid(
    distribution = distributions,
    method = methods,
    stringsAsFactors = FALSE
  ))
  model_grid <- model_grid[order(model_grid$distribution, model_grid$method), , drop = FALSE]
  row.names(model_grid) <- NULL

  fits <- vector("list", nrow(model_grid))
  fit_objects <- list()
  errors <- list()
  quantiles <- list()
  fit_index <- 0L

  for (i in seq_len(nrow(model_grid))) {
    distribution <- model_grid$distribution[i]
    method <- model_grid$method[i]

    fit_result <- tryCatch(
      alea_fit(
        x,
        distribution = distribution,
        method = method,
        ...
      ),
      error = function(e) e
    )

    if (inherits(fit_result, "error")) {
      fits[[i]] <- make_compare_fit_row(
        distribution = distribution,
        method = method,
        status = "error",
        n = length(x),
        fit_index = NA_integer_
      )

      errors[[length(errors) + 1L]] <- make_compare_error_row(
        distribution = distribution,
        method = method,
        step = "fit",
        message = conditionMessage(fit_result),
        class = class(fit_result)[1L]
      )
      next
    }

    fit_index <- fit_index + 1L
    fit_objects[[fit_index]] <- fit_result

    fits[[i]] <- make_compare_fit_row(
      distribution = distribution,
      method = method,
      status = "ok",
      n = length(x),
      fit_index = fit_index
    )

    if (!is.null(return_period)) {
      rl_result <- tryCatch(
        alea_quantile(fit_result, return_period = return_period),
        error = function(e) e
      )

      if (inherits(rl_result, "error")) {
        errors[[length(errors) + 1L]] <- make_compare_error_row(
          distribution = distribution,
          method = method,
          step = "quantile",
          message = conditionMessage(rl_result),
          class = class(rl_result)[1L]
        )
      } else {
        quantiles[[length(quantiles) + 1L]] <- as.data.frame(rl_result)
      }
    }
  }

  names(fit_objects) <- make_compare_fit_object_names(fit_objects)

  object <- list(
    data = x,
    fits = bind_alea_rows_internal(fits),
    fit_objects = fit_objects,
    quantiles = bind_alea_rows_internal(quantiles),
    errors = bind_compare_error_rows(errors),
    settings = list(
      distributions = distributions,
      methods = methods,
      return_period = return_period
    ),
    call = call
  )

  new_alea_compare(object)
}

new_alea_compare <- function(x) {
  class(x) <- c("alea_compare", "list")
  validate_alea_compare(x)
}

validate_alea_compare <- function(x) {
  if (!inherits(x, "alea_compare")) {
    stop("`x` must be an `alea_compare` object.", call. = FALSE)
  }

  required <- c("data", "fits", "fit_objects", "errors", "settings", "call")
  missing <- setdiff(required, names(x))

  if (length(missing) > 0L) {
    stop(
      "Invalid `alea_compare` object. Missing fields: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!is.numeric(x$data) || length(x$data) < 2L) {
    stop("`data` must be a numeric vector with at least two observations.", call. = FALSE)
  }

  if (!is.data.frame(x$fits)) {
    stop("`fits` must be a data frame.", call. = FALSE)
  }

  required_fit_cols <- c("distribution", "method", "status", "n", "fit_index")
  missing_fit_cols <- setdiff(required_fit_cols, names(x$fits))

  if (length(missing_fit_cols) > 0L) {
    stop(
      "`fits` is missing required column(s): ",
      paste(missing_fit_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!is.list(x$fit_objects)) {
    stop("`fit_objects` must be a list.", call. = FALSE)
  }

  if (length(x$fit_objects) > 0L) {
    ok <- vapply(x$fit_objects, inherits, logical(1L), what = "alea_fit")
    if (!all(ok)) {
      stop("All entries in `fit_objects` must be `alea_fit` objects.", call. = FALSE)
    }
  }

  x$errors <- normalize_compare_errors(x$errors)
  x
}

#' @export
print.alea_compare <- function(x, ...) {
  validate_alea_compare(x)

  n_attempted <- nrow(x$fits)
  n_success <- sum(x$fits$status == "ok", na.rm = TRUE)
  n_failed <- sum(x$fits$status != "ok", na.rm = TRUE)

  cat("<alea_compare>\n")
  cat("Sample size:", length(x$data), "\n")
  cat("Attempted models:", n_attempted, "\n")
  cat("Successful models:", n_success, "\n")
  cat("Failed models:", n_failed, "\n")

  if (n_attempted > 0L) {
    cat("\nModels:\n")
    print.data.frame(as.data.frame(x), row.names = FALSE)
  }

  invisible(x)
}

#' @export
summary.alea_compare <- function(object, ...) {
  validate_alea_compare(object)

  out <- as.data.frame(object)
  coef_df <- coef(object)

  if (nrow(coef_df) > 0L) {
    coef_key <- coef_df[, c("fit_index"), drop = FALSE]
    coef_cols <- setdiff(names(coef_df), c("distribution", "method", "fit_index"))
    coef_df <- cbind(coef_key, coef_df[, coef_cols, drop = FALSE])
    out <- merge(out, coef_df, by = "fit_index", all.x = TRUE, sort = FALSE)

    preferred <- c("distribution", "method", "status", "n", "fit_index")
    out <- out[, c(preferred, setdiff(names(out), preferred)), drop = FALSE]
  }

  class(out) <- c("summary.alea_compare", "data.frame")
  out
}

#' @export
print.summary.alea_compare <- function(x, ...) {
  cat("ALEA multi-model comparison summary\n\n")
  print.data.frame(as.data.frame(x), row.names = FALSE, ...)
  invisible(x)
}

#' @export
as.data.frame.alea_compare <- function(x, ...) {
  validate_alea_compare(x)
  order_alea_model_table_internal(x$fits)
}

#' @export
coef.alea_compare <- function(object, ...) {
  validate_alea_compare(object)

  rows <- lapply(seq_along(object$fit_objects), function(i) {
    fit <- object$fit_objects[[i]]
    params <- as.list(coef(fit))

    data.frame(
      distribution = fit$distribution,
      method = fit$method,
      fit_index = i,
      params,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })

  order_alea_model_table_internal(bind_alea_rows_internal(rows))
}

#' @rdname alea_quantile
#' @export
alea_quantile.alea_compare <- function(object, return_period, ...) {
  validate_alea_compare(object)
  check_return_period(return_period)
  return_period <- as.numeric(return_period)

  rows <- list()
  errors <- list()

  for (i in seq_along(object$fit_objects)) {
    fit <- object$fit_objects[[i]]

    result <- tryCatch(
      alea_quantile(fit, return_period = return_period, ...),
      error = function(e) e
    )

    if (inherits(result, "error")) {
      errors[[length(errors) + 1L]] <- make_compare_error_row(
        distribution = fit$distribution,
        method = fit$method,
        step = "quantile",
        message = conditionMessage(result),
        class = class(result)[1L]
      )
    } else {
      rows[[length(rows) + 1L]] <- as.data.frame(result)
    }
  }

  out <- bind_alea_rows_internal(rows)

  if (nrow(out) == 0L) {
    stop("No quantile results could be computed for the `alea_compare` object.", call. = FALSE)
  }

  out <- order_alea_model_table_internal(out)
  attr(out, "data") <- object$data
  attr(out, "observed_data") <- object$data
  attr(out, "alea_errors") <- bind_compare_error_rows(errors)
  class(out) <- c("alea_quantile", "data.frame")
  out
}

#' @rdname alea_gof
#' @export
alea_gof.alea_compare <- function(object,
                                  statistics = c("ks", "cvm", "ad", "loglik", "aic", "bic"),
                                  ...) {
  validate_alea_compare(object)
  statistics <- check_gof_statistics(statistics)

  rows <- list()
  errors <- list()

  for (i in seq_along(object$fit_objects)) {
    fit <- object$fit_objects[[i]]

    result <- tryCatch(
      alea_gof(fit, statistics = statistics, ...),
      error = function(e) e
    )

    if (inherits(result, "error")) {
      errors[[length(errors) + 1L]] <- make_compare_error_row(
        distribution = fit$distribution,
        method = fit$method,
        step = "gof",
        message = conditionMessage(result),
        class = class(result)[1L]
      )
    } else {
      rows[[length(rows) + 1L]] <- as.data.frame(result)
    }
  }

  out <- bind_alea_rows_internal(rows)

  if (nrow(out) == 0L) {
    stop("No goodness-of-fit results could be computed for the `alea_compare` object.", call. = FALSE)
  }

  out <- order_alea_model_table_internal(out)
  attr(out, "alea_errors") <- bind_compare_error_rows(errors)
  class(out) <- c("alea_gof", "data.frame")
  out
}

#' @rdname confint.alea_fit
#' @export
confint.alea_compare <- function(object,
                                 parm = "quantile",
                                 level = 0.95,
                                 return_period,
                                 method = "bootstrap",
                                 n_boot = 500,
                                 seed = NULL,
                                 ...) {
  validate_alea_compare(object)

  if (!identical(parm, "quantile")) {
    stop("Only parm = 'quantile' is currently supported.", call. = FALSE)
  }

  method <- match.arg(method, choices = "bootstrap")
  check_conf_level(level)
  check_n_boot(n_boot)
  check_return_period(return_period)
  return_period <- as.numeric(return_period)

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

  rows <- list()
  errors <- list()
  bootstrap_failures <- list()

  for (i in seq_along(object$fit_objects)) {
    fit <- object$fit_objects[[i]]

    result <- tryCatch(
      ci_bootstrap_quantile(
        object = fit,
        return_period = return_period,
        level = level,
        n_boot = n_boot,
        seed = NULL,
        ...
      ),
      error = function(e) e
    )

    if (inherits(result, "error")) {
      errors[[length(errors) + 1L]] <- make_compare_error_row(
        distribution = fit$distribution,
        method = fit$method,
        step = "confint",
        message = conditionMessage(result),
        class = class(result)[1L]
      )
    } else {
      rows[[length(rows) + 1L]] <- as.data.frame(result)
      bootstrap_failures[[make_compare_fit_label(fit)]] <- attr(result, "bootstrap_failures") %||% character()
    }
  }

  out <- bind_alea_rows_internal(rows)

  if (nrow(out) == 0L) {
    stop("No confidence intervals could be computed for the `alea_compare` object.", call. = FALSE)
  }

  out <- order_alea_model_table_internal(out)
  attr(out, "data") <- object$data
  attr(out, "observed_data") <- object$data
  attr(out, "alea_errors") <- bind_compare_error_rows(errors)
  attr(out, "bootstrap_failures") <- bootstrap_failures
  class(out) <- c("alea_quantile_ci", "data.frame")
  out
}

#' @export
plot.alea_compare <- function(x,
                              type = c("quantile", "gof"),
                              return_period = c(2, 5, 10, 25, 50, 100),
                              return_period_scale = c("gumbel", "log", "linear"),
                              statistic = "aic",
                              plot_observed = TRUE,
                              plotting_position_a = 0.44,
                              ...) {
  type <- match.arg(type)
  return_period_scale <- match.arg(return_period_scale)


  if (type == "quantile") {
    rl <- alea_quantile(x, return_period = return_period, ...)
    return(plot(
      rl,
      return_period_scale = return_period_scale,
      plot_observed = plot_observed,
      plotting_position_a = plotting_position_a
    ))
  }

  if (type == "gof") {
    gof <- alea_gof(x, statistics = "all", ...)
    return(plot(gof, type = "statistic", ...))
  }

  stop("Unsupported plot type.", call. = FALSE)
}



check_distribution_vector_internal <- function(distributions, name = "distributions") {
  if (!is.character(distributions) || length(distributions) == 0L || anyNA(distributions)) {
    stop("`", name, "` must be a non-empty character vector.", call. = FALSE)
  }

  unique(vapply(distributions, check_distribution, character(1L), USE.NAMES = FALSE))
}

check_method_vector_internal <- function(methods, name = "methods") {
  if (!is.character(methods) || length(methods) == 0L || anyNA(methods)) {
    stop("`", name, "` must be a non-empty character vector.", call. = FALSE)
  }

  unique(vapply(methods, check_method, character(1L), USE.NAMES = FALSE))
}

make_compare_fit_row <- function(distribution, method, status, n, fit_index) {
  data.frame(
    distribution = distribution,
    method = method,
    status = status,
    n = n,
    fit_index = fit_index,
    stringsAsFactors = FALSE
  )
}

make_compare_error_row <- function(distribution, method, step, message, class) {
  data.frame(
    distribution = distribution,
    method = method,
    step = step,
    message = message,
    class = class,
    stringsAsFactors = FALSE
  )
}

empty_compare_errors <- function() {
  data.frame(
    distribution = character(),
    method = character(),
    step = character(),
    message = character(),
    class = character(),
    stringsAsFactors = FALSE
  )
}

bind_compare_error_rows <- function(rows) {
  if (length(rows) == 0L) {
    return(empty_compare_errors())
  }

  normalize_compare_errors(bind_alea_rows_internal(rows))
}

normalize_compare_errors <- function(errors) {
  if (is.null(errors)) {
    return(empty_compare_errors())
  }

  errors <- as.data.frame(errors, stringsAsFactors = FALSE)
  required <- c("distribution", "method", "step", "message", "class")

  if (nrow(errors) == 0L) {
    return(empty_compare_errors())
  }

  missing <- setdiff(required, names(errors))
  for (column in missing) {
    errors[[column]] <- NA_character_
  }

  errors <- errors[, required, drop = FALSE]
  for (column in required) {
    errors[[column]] <- as.character(errors[[column]])
  }

  rownames(errors) <- NULL
  errors
}

bind_alea_rows_internal <- function(rows) {
  rows <- Filter(Negate(is.null), rows)

  if (length(rows) == 0L) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  rows <- lapply(rows, as.data.frame, stringsAsFactors = FALSE)
  all_names <- unique(unlist(lapply(rows, names), use.names = FALSE))

  rows <- lapply(rows, function(row) {
    missing <- setdiff(all_names, names(row))
    for (name in missing) {
      row[[name]] <- NA
    }
    row[, all_names, drop = FALSE]
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

make_compare_fit_label <- function(fit) {
  paste(fit$distribution, fit$method, sep = "/")
}

make_compare_fit_object_names <- function(fit_objects) {
  if (length(fit_objects) == 0L) {
    return(character())
  }

  labels <- vapply(fit_objects, make_compare_fit_label, character(1L))
  make.unique(labels, sep = "_")
}
