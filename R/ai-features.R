# AI feature extraction -----------------------------------------------------
#
# Internal utilities for computing the application-ready FADS_AI feature set.
#
# The deployed FADS_AI model uses the classical scenario with sample
# L-moment descriptors only. These helpers intentionally compute only the
# features required by the trained model.

#' Return the AI feature names used by the deployed FADS_AI model
#'
#' @keywords internal
#' @noRd
extract_ai_feature_names <- function(feature_set = "fads_ai_classical_v1") {
  feature_set <- match.arg(feature_set, "fads_ai_classical_v1")

  c(
    "lmom_l1",
    "lmom_l2",
    "lmom_l3",
    "lmom_l4",
    "lmom_t3",
    "lmom_t4"
  )
}

#' Extract FADS_AI features from a numeric sample
#'
#' @keywords internal
#' @noRd
extract_ai_features <- function(x, feature_set = "fads_ai_classical_v1") {
  feature_set <- match.arg(feature_set, "fads_ai_classical_v1")

  if (!is.numeric(x)) {
    stop("`x` must be a numeric vector.", call. = FALSE)
  }

  x <- x[is.finite(x)]

  if (length(x) < 4L) {
    stop("At least 4 finite observations are required to compute AI features.", call. = FALSE)
  }

  if (!requireNamespace("lmom", quietly = TRUE)) {
    stop("Package `lmom` is required to compute AI features.", call. = FALSE)
  }

  lm <- tryCatch(
    lmom::samlmu(x, nmom = 4L),
    error = function(e) {
      stop("Could not compute sample L-moments: ", conditionMessage(e), call. = FALSE)
    }
  )

  l1 <- as.numeric(lm[1L])
  l2 <- as.numeric(lm[2L])
  l3 <- as.numeric(lm[3L])
  l4 <- as.numeric(lm[4L])

  out <- data.frame(
    lmom_l1 = l1,
    lmom_l2 = l2,
    lmom_l3 = l3,
    lmom_l4 = l4,
    lmom_t3 = if (isTRUE(l2 == 0)) NA_real_ else as.numeric(l3 / l2),
    lmom_t4 = if (isTRUE(l2 == 0)) NA_real_ else as.numeric(l4 / l2),
    stringsAsFactors = FALSE
  )

  out[, extract_ai_feature_names(feature_set), drop = FALSE]
}

#' Build a FADS_AI application row
#'
#' @keywords internal
#' @noRd
build_ai_application_row <- function(
  x,
  sample_id = "observed_sample",
  param_id = "observed_sample",
  feature_set = "fads_ai_classical_v1"
) {
  if (!is.character(sample_id) || length(sample_id) != 1L || is.na(sample_id)) {
    stop("`sample_id` must be a single non-missing character string.", call. = FALSE)
  }

  if (!is.character(param_id) || length(param_id) != 1L || is.na(param_id)) {
    stop("`param_id` must be a single non-missing character string.", call. = FALSE)
  }

  if (!is.numeric(x)) {
    stop("`x` must be a numeric vector.", call. = FALSE)
  }

  x_finite <- x[is.finite(x)]
  features <- extract_ai_features(x_finite, feature_set = feature_set)

  metadata <- data.frame(
    sample_id = sample_id,
    param_id = param_id,
    n = as.integer(length(x_finite)),
    replicate_id = NA_integer_,
    seed = NA_integer_,
    par1 = NA_real_,
    par2 = NA_real_,
    par3 = NA_real_,
    stringsAsFactors = FALSE
  )

  cbind(metadata, features)
}

#' Validate that a feature row contains the model-required columns
#'
#' @keywords internal
#' @noRd
validate_ai_feature_row <- function(feature_row, required_features = extract_ai_feature_names()) {
  if (!is.data.frame(feature_row)) {
    stop("`feature_row` must be a data frame.", call. = FALSE)
  }

  missing_features <- setdiff(required_features, names(feature_row))

  if (length(missing_features) > 0L) {
    stop(
      "The AI feature row is missing required feature columns: ",
      paste(missing_features, collapse = ", "),
      call. = FALSE
    )
  }

  bad_features <- required_features[
    !vapply(feature_row[required_features], is.numeric, logical(1))
  ]

  if (length(bad_features) > 0L) {
    stop(
      "The following AI feature columns must be numeric: ",
      paste(bad_features, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(feature_row)
}
