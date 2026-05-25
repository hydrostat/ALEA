#' AI-assisted distribution-selection support
#'
#' `alea_select()` applies the lightweight operational FADS_AI model to a
#' hydrological sample and returns model-based support for the candidate
#' distribution families.
#'
#' The default engine is the bundled lightweight FADS_AI application model,
#' stored under `inst/extdata/fads_ai/fads_ai_application_model_light.rds`.
#' The light model is an operational representation of the archived FADS_AI
#' XGBoost classifier and was validated to reproduce the full workflow
#' predictions on the validation check.
#'
#' FADS_AI output should be interpreted as decision-support evidence for
#' candidate distribution families. It is not proof of the true generating
#' distribution and should not replace goodness-of-fit assessment, diagnostics,
#' quantile uncertainty evaluation, or hydrological judgement.
#'
#' @param object A numeric vector or an `alea_fit` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return An object of class `alea_selection`.
#'
#' @examples
#' x <- c(42.1, 38.5, 51.3, 47.0, 62.4, 55.2, 49.8, 58.1,
#'        60.3, 45.9, 67.2, 44.6)
#' selection <- alea_select(x)
#' selection
#' as.data.frame(selection)
#'
#' @export
alea_select <- function(object, ...) {
  UseMethod("alea_select")
}

#' @rdname alea_select
#'
#' @param model Optional pre-loaded FADS_AI light model object.
#' @param model_path Optional path to a FADS_AI light model `.rds` file.
#' @param use_builtin_model Logical. If `TRUE`, use the bundled light model
#'   when both `model` and `model_path` are `NULL`.
#' @param sample_id Character scalar identifying the sample.
#' @param param_id Character scalar identifying the FADS_AI application row.
#'   The default is `"observed_sample"`.
#' @param margin_thresholds Numeric vector of three increasing thresholds used
#'   to label the top1-top2 probability margin.
#' @param include_interpretation Logical. If `TRUE`, include report-ready
#'   interpretation text in the output.
#' @param quiet Logical. If `FALSE`, a message is emitted when a model is
#'   loaded from `model_path`.
#'
#' @export
alea_select.numeric <- function(
  object,
  model = NULL,
  model_path = NULL,
  use_builtin_model = TRUE,
  sample_id = "observed_sample",
  param_id = "observed_sample",
  margin_thresholds = c(0.10, 0.30, 0.60),
  include_interpretation = TRUE,
  quiet = FALSE,
  ...
) {
  call <- match.call()

  select_with_fads_ai_light(
    x = object,
    model = model,
    model_path = model_path,
    use_builtin_model = use_builtin_model,
    sample_id = sample_id,
    param_id = param_id,
    margin_thresholds = margin_thresholds,
    include_interpretation = include_interpretation,
    quiet = quiet,
    call = call
  )
}

#' @rdname alea_select
#' @export
alea_select.alea_fit <- function(
  object,
  model = NULL,
  model_path = NULL,
  use_builtin_model = TRUE,
  sample_id = "observed_sample",
  param_id = "observed_sample",
  margin_thresholds = c(0.10, 0.30, 0.60),
  include_interpretation = TRUE,
  quiet = FALSE,
  ...
) {
  call <- match.call()

  if (is.null(object$data) || !is.numeric(object$data)) {
    stop("`object$data` must contain the original numeric sample.", call. = FALSE)
  }

  select_with_fads_ai_light(
    x = object$data,
    model = model,
    model_path = model_path,
    use_builtin_model = use_builtin_model,
    sample_id = sample_id,
    param_id = param_id,
    margin_thresholds = margin_thresholds,
    include_interpretation = include_interpretation,
    quiet = quiet,
    call = call
  )
}

#' @export
print.alea_selection <- function(x, digits = 4, ...) {
  cat("ALEA-R AI-assisted distribution-selection support\n")
  cat("Selection method:", x$selection_method, "\n")
  cat("Predicted distribution:", x$selected_distribution, "\n")

  if (!is.null(x$decision)) {
    cat("Decision strength:", x$decision$decision_strength[[1L]], "\n")
    cat("Top1-top2 margin:", round(x$decision$top1_top2_margin[[1L]], digits), "\n")
  }

  if (!is.null(x$model_info)) {
    cat("Model:", x$model_info$model_name, "\n")
    cat("Model version:", x$model_info$model_version, "\n")
    cat("Scenario:", x$model_info$scenario, "\n")
    cat("Algorithm:", x$model_info$algorithm, "\n")
  }

  if (!is.null(x$ranking) && nrow(x$ranking) > 0L) {
    ranking <- as.data.frame(x)
    ranking$probability <- round(ranking$probability, digits)
    cat("\nCandidate ranking:\n")
    print.data.frame(ranking, row.names = FALSE)
  }

  if (!is.null(x$decision) && "interpretation" %in% names(x$decision)) {
    cat("\nInterpretation:\n")
    cat(x$decision$interpretation[[1L]], "\n")
  }

  if (length(x$warnings) > 0L) {
    cat("\nWarnings:\n")
    for (message in x$warnings) {
      cat("-", message, "\n")
    }
  }

  cat("\nUse as.data.frame(x) for the full AI-selection ranking table.\n")

  invisible(x)
}

#' @export
as.data.frame.alea_selection <- function(x, ...) {
  out <- as.data.frame(x$ranking, stringsAsFactors = FALSE)
  out <- out[order(out$rank, out$distribution), , drop = FALSE]
  row.names(out) <- NULL
  out
}

select_with_fads_ai_light <- function(
  x,
  model = NULL,
  model_path = NULL,
  use_builtin_model = TRUE,
  sample_id = "observed_sample",
  param_id = "observed_sample",
  margin_thresholds = c(0.10, 0.30, 0.60),
  include_interpretation = TRUE,
  quiet = FALSE,
  call = NULL
) {
  if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet)) {
    stop("`quiet` must be a single logical value.", call. = FALSE)
  }

  if (!is.logical(use_builtin_model) || length(use_builtin_model) != 1L || is.na(use_builtin_model)) {
    stop("`use_builtin_model` must be a single logical value.", call. = FALSE)
  }

  if (!is.logical(include_interpretation) || length(include_interpretation) != 1L || is.na(include_interpretation)) {
    stop("`include_interpretation` must be a single logical value.", call. = FALSE)
  }

  if (!is.null(model) && !is.null(model_path)) {
    stop("Use only one of `model` or `model_path`, not both.", call. = FALSE)
  }

  normalized_model_path <- NA_character_
  model_source <- "preloaded"

  if (is.null(model)) {
    if (!is.null(model_path)) {
      if (!quiet) {
        message("Loading FADS_AI light model from `model_path`.")
      }
      model <- load_ai_light_model(model_path)
      normalized_model_path <- normalizePath(model_path, winslash = "/", mustWork = TRUE)
      model_source <- "model_path"
    } else if (use_builtin_model) {
      model <- load_builtin_ai_light_model()
      builtin_path <- system.file(
        "extdata", "fads_ai", "fads_ai_application_model_light.rds",
        package = "ALEA",
        mustWork = FALSE
      )
      normalized_model_path <- if (nzchar(builtin_path)) normalizePath(builtin_path, winslash = "/", mustWork = FALSE) else NA_character_
      model_source <- "builtin"
    } else {
      stop(
        "Either `model`, `model_path`, or `use_builtin_model = TRUE` must be supplied for AI-assisted model selection.",
        call. = FALSE
      )
    }
  } else {
    model <- validate_ai_light_model(model)
  }

  application_row <- build_ai_application_row(
    x = x,
    sample_id = sample_id,
    param_id = param_id,
    feature_set = "fads_ai_classical_v1"
  )

  validate_ai_feature_row(application_row, required_features = model$active_features)

  pred_tbl <- predict_ai_light(
    light_model = model,
    new_data = application_row
  )

  decision <- summarize_ai_prediction(
    pred_tbl = pred_tbl,
    thresholds = margin_thresholds
  )

  if (include_interpretation) {
    decision <- add_ai_interpretation(decision)
  }

  ranking <- build_ai_light_ranking(pred_tbl)

  selected_distribution <- tolower(as.character(decision$predicted_family[[1L]]))
  decision <- normalize_ai_decision_labels(decision)
  ranking$distribution <- tolower(ranking$distribution)

  new_alea_selection(
    selected_distribution = selected_distribution,
    selected_method = NA_character_,
    selection_method = "ai",
    ranking = ranking,
    decision = decision,
    features = application_row[, model$active_features, drop = FALSE],
    model_info = list(
      model_name = "FADS_AI lightweight operational application model",
      model_version = "1.0.0-light",
      scenario = "classical",
      algorithm = "xgb",
      candidate_distributions = tolower(as.character(model$class_levels)),
      feature_set = "fads_ai_classical_v1",
      feature_columns = as.character(model$active_features),
      model_source = model_source,
      model_path = normalized_model_path,
      validation = list(
        n_validation_rows = 1000L,
        max_abs_probability_difference = 0,
        class_agreement = 1
      )
    ),
    warnings = character(),
    call = call
  )
}

build_ai_light_ranking <- function(pred_tbl) {
  prob_cols <- grep("^\\.pred_", names(pred_tbl), value = TRUE)
  prob_cols <- setdiff(prob_cols, ".pred_class")

  if (length(prob_cols) == 0L) {
    stop("No FADS_AI probability columns were returned by the light model.", call. = FALSE)
  }

  distribution <- sub("^\\.pred_", "", prob_cols)
  probability <- as.numeric(pred_tbl[1L, prob_cols, drop = TRUE])
  selected <- distribution == as.character(pred_tbl$.pred_class[[1L]])

  out <- data.frame(
    distribution = distribution,
    probability = probability,
    selected = selected,
    stringsAsFactors = FALSE
  )

  out <- out[order(-out$probability, out$distribution), , drop = FALSE]
  out$rank <- seq_len(nrow(out))
  out <- out[, c("distribution", "probability", "rank", "selected"), drop = FALSE]
  row.names(out) <- NULL
  out
}

normalize_ai_decision_labels <- function(decision) {
  label_cols <- c("predicted_family", "top_family", "second_family")

  for (nm in intersect(label_cols, names(decision))) {
    decision[[nm]] <- tolower(as.character(decision[[nm]]))
  }

  decision
}

new_alea_selection <- function(
  selected_distribution,
  selected_method,
  selection_method,
  ranking,
  decision,
  features,
  model_info,
  warnings = character(),
  call = NULL
) {
  out <- list(
    selected_distribution = selected_distribution,
    selected_method = selected_method,
    selection_method = selection_method,
    ranking = ranking,
    decision = decision,
    features = features,
    model_info = model_info,
    warnings = warnings,
    call = call
  )

  class(out) <- c("alea_selection", "list")
  validate_alea_selection(out)
}

validate_alea_selection <- function(x) {
  if (!inherits(x, "alea_selection")) {
    stop("`x` must be an `alea_selection` object.", call. = FALSE)
  }

  if (!is.character(x$selected_distribution) || length(x$selected_distribution) != 1L) {
    stop("`selected_distribution` must be a single character string.", call. = FALSE)
  }

  if (!is.character(x$selection_method) || length(x$selection_method) != 1L) {
    stop("`selection_method` must be a single character string.", call. = FALSE)
  }

  if (!is.data.frame(x$ranking)) {
    stop("`ranking` must be a data frame.", call. = FALSE)
  }

  if (!is.data.frame(x$decision)) {
    stop("`decision` must be a data frame.", call. = FALSE)
  }

  if (!is.data.frame(x$features)) {
    stop("`features` must be a data frame.", call. = FALSE)
  }

  if (!is.list(x$model_info)) {
    stop("`model_info` must be a list.", call. = FALSE)
  }

  if (!is.character(x$warnings)) {
    stop("`warnings` must be a character vector.", call. = FALSE)
  }

  x
}
