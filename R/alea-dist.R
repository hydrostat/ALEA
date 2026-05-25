#' Supported ALEA distributions and parameter roles
#'
#' `alea_dist()` returns user-facing information about the distributions
#' supported by ALEA-R and how their internal parameter names map to the
#' standardized output columns `location`, `scale`, and `shape`.
#'
#' @param distribution Optional character vector of distribution codes. If
#'   `NULL`, information for all supported distributions is returned.
#'
#' @return A data frame with class `alea_dist`.
#'
#' @examples
#' alea_dist()
#' alea_dist("gev")
#' alea_dist(c("ln3", "pe3"))
#'
#' @export
alea_dist <- function(distribution = NULL) {
  supported <- c("gev", "gpa", "pe3", "ln2", "ln3", "gum")

  if (is.null(distribution)) {
    distribution <- supported
  } else {
    if (!is.character(distribution) || length(distribution) == 0L || anyNA(distribution)) {
      stop("`distribution` must be NULL or a non-empty character vector.", call. = FALSE)
    }
    distribution <- unique(vapply(distribution, check_distribution, character(1L), USE.NAMES = FALSE))
  }

  rows <- lapply(distribution, distribution_parameter_info_internal)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  class(out) <- c("alea_dist", "data.frame")
  out
}

#' @export
print.alea_dist <- function(x, ...) {
  cat("ALEA supported distributions and parameter roles\n\n")
  print.data.frame(as.data.frame(x), row.names = FALSE, ...)
  invisible(x)
}

#' @export
as.data.frame.alea_dist <- function(x, ...) {
  class(x) <- "data.frame"
  x
}

distribution_parameter_info_internal <- function(distribution) {
  distribution <- check_distribution(distribution)

  switch(
    distribution,
    gev = data.frame(
      distribution = "gev",
      name = "Generalized Extreme Value",
      internal_parameter = c("xi", "alpha", "k"),
      output_column = c("location", "scale", "shape"),
      description = c("location parameter", "scale parameter", "shape parameter"),
      stringsAsFactors = FALSE
    ),
    gpa = data.frame(
      distribution = "gpa",
      name = "Generalized Pareto",
      internal_parameter = c("xi", "alpha", "k"),
      output_column = c("location", "scale", "shape"),
      description = c("location/threshold parameter", "scale parameter", "shape parameter"),
      stringsAsFactors = FALSE
    ),
    gum = data.frame(
      distribution = "gum",
      name = "Gumbel",
      internal_parameter = c("xi", "alpha", NA_character_),
      output_column = c("location", "scale", "shape"),
      description = c("location parameter", "scale parameter", "not used"),
      stringsAsFactors = FALSE
    ),
    pe3 = data.frame(
      distribution = "pe3",
      name = "Pearson type III",
      internal_parameter = c("mu", "sigma", "gamma"),
      output_column = c("location", "scale", "shape"),
      description = c("mean parameter", "standard-deviation parameter", "skewness parameter"),
      stringsAsFactors = FALSE
    ),
    ln2 = data.frame(
      distribution = "ln2",
      name = "Two-parameter lognormal",
      internal_parameter = c("mu", "sigma", NA_character_),
      output_column = c("location", "scale", "shape"),
      description = c("log-scale mean", "log-scale standard deviation", "not used"),
      stringsAsFactors = FALSE
    ),
    ln3 = data.frame(
      distribution = "ln3",
      name = "Three-parameter lognormal",
      internal_parameter = c("zeta", "sigma", "mu"),
      output_column = c("location", "scale", "shape"),
      description = c("lower-bound/location parameter", "log-scale standard deviation", "log-scale mean"),
      stringsAsFactors = FALSE
    )
  )
}

standardize_parameters_internal <- function(distribution, parameters) {
  distribution <- check_distribution(distribution)

  get_value <- function(name) {
    if (is.na(name) || !name %in% names(parameters)) {
      return(NA_real_)
    }
    as.numeric(parameters[[name]])
  }

  info <- distribution_parameter_info_internal(distribution)
  values <- vapply(info$internal_parameter, get_value, numeric(1L))
  names(values) <- info$output_column
  values
}

standardized_parameter_columns_internal <- function(distribution, parameters, n) {
  values <- standardize_parameters_internal(distribution, parameters)
  as.data.frame(
    lapply(values, function(value) rep(as.numeric(value), n)),
    stringsAsFactors = FALSE
  )
}
