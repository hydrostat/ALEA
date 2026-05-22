# AI model metadata ---------------------------------------------------------

#' FADS_AI model metadata
#'
#' Return lightweight metadata about the FADS_AI application model bundled
#' with ALEA-R or supplied by the user.
#'
#' @param model_path Optional path to a FADS_AI light model file.
#' @param validation_path Optional path to the light-model validation CSV.
#' @param quiet Logical. If `FALSE`, print messages when optional files are not found.
#'
#' @return An object of class `alea_ai_model_info`.
#'
#' @export
alea_ai_model_info <- function(
  model_path = NULL,
  validation_path = NULL,
  quiet = FALSE
) {
  if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet)) {
    stop("`quiet` must be a single logical value.", call. = FALSE)
  }

  if (is.null(model_path)) {
    model_path <- system.file(
      "extdata", "fads_ai", "fads_ai_application_model_light.rds",
      package = "ALEA",
      mustWork = FALSE
    )
  }

  if (is.null(validation_path)) {
    validation_path <- system.file(
      "extdata", "fads_ai", "fads_ai_application_model_light_validation.csv",
      package = "ALEA",
      mustWork = FALSE
    )
  }

  model_available <- nzchar(model_path) && file.exists(model_path)
  validation_available <- nzchar(validation_path) && file.exists(validation_path)

  validation <- list(
    n_validation_rows = NA_integer_,
    max_abs_probability_difference = NA_real_,
    class_agreement = NA_real_
  )

  if (validation_available) {
    validation_df <- tryCatch(
      utils::read.csv(validation_path, stringsAsFactors = FALSE),
      error = function(e) NULL
    )

    if (!is.null(validation_df) && nrow(validation_df) >= 1L) {
      validation$n_validation_rows <- as.integer(validation_df$n_validation_rows[[1L]])
      validation$max_abs_probability_difference <- as.numeric(validation_df$max_abs_probability_difference[[1L]])
      validation$class_agreement <- as.numeric(validation_df$class_agreement[[1L]])
    }
  } else if (!quiet) {
    message("FADS_AI light-model validation CSV was not found.")
  }

  out <- list(
    model_name = "FADS_AI lightweight operational application model",
    model_version = "1.0.0-light",
    parent_model_name = "FADS_AI final application model",
    parent_model_version = "1.0.0",
    scenario = "classical",
    algorithm = "xgb",
    candidate_distributions = c("gev", "gpa", "pe3", "ln2", "ln3", "gum"),
    original_candidate_labels = c("GEV", "GPA", "PE3", "LN2", "LN3", "GUM"),
    feature_set = "fads_ai_classical_v1",
    feature_columns = c("lmom_l1", "lmom_l2", "lmom_l3", "lmom_l4", "lmom_t3", "lmom_t4"),
    model_file = if (nzchar(model_path)) model_path else NA_character_,
    model_available = model_available,
    validation_file = if (nzchar(validation_path)) validation_path else NA_character_,
    validation_available = validation_available,
    validation = validation,
    model_size_note = paste(
      "The bundled light model is an operational representation of the FADS_AI",
      "XGBoost application classifier and was validated to reproduce the full",
      "tidymodels workflow predictions on the validation check."
    ),
    interpretation_note = paste(
      "FADS_AI probabilities are model-based support for candidate distribution",
      "families. They are decision-support evidence and should not be interpreted",
      "as proof of the true generating distribution."
    )
  )

  class(out) <- c("alea_ai_model_info", "list")
  out
}

#' @export
print.alea_ai_model_info <- function(x, ...) {
  cat("ALEA-R AI model metadata\n")
  cat("Model:", x$model_name, "\n")
  cat("Model version:", x$model_version, "\n")
  cat("Parent model:", x$parent_model_name, "\n")
  cat("Parent model version:", x$parent_model_version, "\n")
  cat("Scenario:", x$scenario, "\n")
  cat("Algorithm:", x$algorithm, "\n")
  cat("Feature set:", x$feature_set, "\n")
  cat("Candidate distributions:", paste(x$candidate_distributions, collapse = ", "), "\n")
  cat("Model available:", x$model_available, "\n")

  if (isTRUE(x$validation_available)) {
    cat("Validation rows:", x$validation$n_validation_rows, "\n")
    cat("Max abs probability difference:", x$validation$max_abs_probability_difference, "\n")
    cat("Class agreement:", x$validation$class_agreement, "\n")
  }

  cat("\n", x$interpretation_note, "\n", sep = "")

  invisible(x)
}
