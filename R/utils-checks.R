# Common validation helpers for ALEA-R.

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

check_distribution <- function(distribution) {
  allowed <- c("gev", "gpa", "pe3", "ln2", "ln3", "gum")

  if (!is.character(distribution) || length(distribution) != 1L || is.na(distribution)) {
    stop("`distribution` must be a single character value.", call. = FALSE)
  }

  distribution <- tolower(distribution)

  if (!distribution %in% allowed) {
    stop(
      "`distribution` must be one of: ",
      paste(allowed, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  distribution
}

check_method <- function(method) {
  allowed <- c("mom", "lmom", "mle")

  if (!is.character(method) || length(method) != 1L || is.na(method)) {
    stop("`method` must be a single character value.", call. = FALSE)
  }

  method <- tolower(method)

  if (!method %in% allowed) {
    stop(
      "`method` must be one of: ",
      paste(allowed, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  method
}

check_numeric_vector <- function(x, name = "x", min_length = 1L) {
  if (!is.numeric(x)) {
    stop("`", name, "` must be numeric.", call. = FALSE)
  }

  finite <- is.finite(x)

  if (!all(finite)) {
    warning(
      "Non-finite values were removed from `", name, "`.",
      call. = FALSE
    )
    x <- x[finite]
  }

  if (length(x) < min_length) {
    stop(
      "`", name, "` must contain at least ",
      min_length,
      " finite observations.",
      call. = FALSE
    )
  }

  if (stats::sd(x) == 0) {
    stop("`", name, "` must not be constant.", call. = FALSE)
  }

  x
}

check_probability <- function(p, allow_zero_one = TRUE) {
  if (!is.numeric(p)) {
    stop("`p` must be numeric.", call. = FALSE)
  }

  if (any(!is.finite(p))) {
    stop("`p` must contain only finite values.", call. = FALSE)
  }

  ok <- if (allow_zero_one) {
    p >= 0 & p <= 1
  } else {
    p > 0 & p < 1
  }

  if (any(!ok)) {
    stop("`p` must be inside the valid probability range.", call. = FALSE)
  }

  invisible(TRUE)
}

check_return_period <- function(return_period) {
  if (!is.numeric(return_period)) {
    stop("`return_period` must be numeric.", call. = FALSE)
  }

  if (any(!is.finite(return_period)) || any(return_period <= 1)) {
    stop("`return_period` must contain finite values greater than 1.", call. = FALSE)
  }

  invisible(TRUE)
}

check_parameter_vector <- function(para, expected_names) {
  if (!is.numeric(para)) {
    stop("`para` must be a numeric vector.", call. = FALSE)
  }

  if (length(para) != length(expected_names)) {
    stop(
      "`para` must have length ",
      length(expected_names),
      ".",
      call. = FALSE
    )
  }

  if (is.null(names(para))) {
    stop("`para` must be a named numeric vector.", call. = FALSE)
  }

  if (!identical(names(para), expected_names)) {
    stop(
      "`para` names must be: ",
      paste(expected_names, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (any(!is.finite(para))) {
    stop("`para` must contain only finite values.", call. = FALSE)
  }

  invisible(TRUE)
}

check_positive_value <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0) {
    stop("`", name, "` must be a positive finite number.", call. = FALSE)
  }

  invisible(TRUE)
}

check_non_negative_count <- function(n) {
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 0 || n != floor(n)) {
    stop("`n` must be a non-negative whole number.", call. = FALSE)
  }

  invisible(TRUE)
}
