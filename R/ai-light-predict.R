# Lightweight FADS_AI prediction helpers -----------------------------------
#
# Internal ALEA-R helpers for the lightweight operational FADS_AI model.
# The light model is a compact representation of the same FADS_AI XGBoost
# application classifier and is expected to contain:
#   active_features, impute_median, feature_center, feature_scale,
#   booster_raw, and class_levels.
#
# These helpers intentionally return base data frames to keep ALEA-R's
# dependency surface small. The only required prediction dependency is xgboost.

load_builtin_ai_light_model <- function() {
  model_path <- system.file(
    "extdata", "fads_ai", "fads_ai_application_model_light.rds",
    package = "ALEA",
    mustWork = FALSE
  )

  if (!nzchar(model_path) || !file.exists(model_path)) {
    stop(
      "The built-in FADS_AI light model file was not found. ",
      "Expected file: inst/extdata/fads_ai/fads_ai_application_model_light.rds.",
      call. = FALSE
    )
  }

  load_ai_light_model(model_path)
}

load_ai_light_model <- function(model_path) {
  if (!is.character(model_path) || length(model_path) != 1L ||
      is.na(model_path) || !nzchar(model_path)) {
    stop("`model_path` must be a single non-empty character string.", call. = FALSE)
  }

  if (!file.exists(model_path)) {
    stop("`model_path` does not exist: ", model_path, call. = FALSE)
  }

  model <- tryCatch(
    readRDS(model_path),
    error = function(e) {
      stop("Could not read the FADS_AI light model file: ", conditionMessage(e), call. = FALSE)
    }
  )

  validate_ai_light_model(model)
}

validate_ai_light_model <- function(light_model) {
  if (!is.list(light_model)) {
    stop("The FADS_AI light model must be a list object.", call. = FALSE)
  }

  required_fields <- c(
    "active_features",
    "impute_median",
    "feature_center",
    "feature_scale",
    "booster_raw",
    "class_levels"
  )

  missing_fields <- setdiff(required_fields, names(light_model))

  if (length(missing_fields) > 0L) {
    stop(
      "The FADS_AI light model is missing required fields: ",
      paste(missing_fields, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.character(light_model$active_features) || length(light_model$active_features) == 0L) {
    stop("`active_features` in the FADS_AI light model must be a non-empty character vector.", call. = FALSE)
  }

  if (!is.character(light_model$class_levels) || length(light_model$class_levels) == 0L) {
    stop("`class_levels` in the FADS_AI light model must be a non-empty character vector.", call. = FALSE)
  }

  missing_impute <- setdiff(light_model$active_features, names(light_model$impute_median))
  missing_center <- setdiff(light_model$active_features, names(light_model$feature_center))
  missing_scale <- setdiff(light_model$active_features, names(light_model$feature_scale))

  if (length(missing_impute) > 0L) {
    stop("The FADS_AI light model is missing imputation medians for: ", paste(missing_impute, collapse = ", "), call. = FALSE)
  }

  if (length(missing_center) > 0L) {
    stop("The FADS_AI light model is missing feature centers for: ", paste(missing_center, collapse = ", "), call. = FALSE)
  }

  if (length(missing_scale) > 0L) {
    stop("The FADS_AI light model is missing feature scales for: ", paste(missing_scale, collapse = ", "), call. = FALSE)
  }

  expected_distributions <- c("gev", "gpa", "pe3", "ln2", "ln3", "gum")
  model_distributions <- tolower(as.character(light_model$class_levels))
  unknown_distributions <- setdiff(model_distributions, expected_distributions)

  if (length(unknown_distributions) > 0L) {
    stop(
      "The FADS_AI light model contains candidate distributions not supported by ALEA-R: ",
      paste(unknown_distributions, collapse = ", "),
      call. = FALSE
    )
  }

  light_model
}

preprocess_ai_light_model <- function(light_model, new_data) {
  light_model <- validate_ai_light_model(light_model)

  if (!is.data.frame(new_data)) {
    stop("`new_data` must be a data frame.", call. = FALSE)
  }

  required <- light_model$active_features
  missing_features <- setdiff(required, names(new_data))

  if (length(missing_features) > 0L) {
    stop(
      "New data is missing required FADS_AI features: ",
      paste(missing_features, collapse = ", "),
      call. = FALSE
    )
  }

  xdf <- new_data[, required, drop = FALSE]

  for (nm in required) {
    x <- xdf[[nm]]

    if (!is.numeric(x)) {
      stop("FADS_AI feature `", nm, "` must be numeric.", call. = FALSE)
    }

    x[is.na(x)] <- as.numeric(light_model$impute_median[[nm]])

    scale_value <- as.numeric(light_model$feature_scale[[nm]])
    if (!is.finite(scale_value) || scale_value == 0) {
      stop("Invalid scaling value for FADS_AI feature `", nm, "`.", call. = FALSE)
    }

    center_value <- as.numeric(light_model$feature_center[[nm]])
    xdf[[nm]] <- (x - center_value) / scale_value
  }

  mat <- as.matrix(xdf)
  storage.mode(mat) <- "numeric"
  mat
}

predict_ai_light_prob <- function(light_model, new_data) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("Package `xgboost` is required for FADS_AI light-model prediction.", call. = FALSE)
  }

  light_model <- validate_ai_light_model(light_model)

  booster <- xgboost::xgb.load.raw(light_model$booster_raw)

  mat <- preprocess_ai_light_model(
    light_model = light_model,
    new_data = new_data
  )

  dmat <- xgboost::xgb.DMatrix(mat)
  raw_pred <- predict(booster, dmat)

  n <- nrow(mat)
  k <- length(light_model$class_levels)

  # Validation against the full tidymodels workflow showed that byrow = FALSE
  # reproduces the original workflow probabilities.
  prob_mat <- matrix(raw_pred, nrow = n, ncol = k, byrow = FALSE)
  colnames(prob_mat) <- paste0(".pred_", light_model$class_levels)

  as.data.frame(prob_mat, stringsAsFactors = FALSE)
}

predict_ai_light_class <- function(light_model, new_data) {
  light_model <- validate_ai_light_model(light_model)

  pred_prob <- predict_ai_light_prob(
    light_model = light_model,
    new_data = new_data
  )

  prob_mat <- as.matrix(pred_prob)
  idx <- max.col(prob_mat, ties.method = "first")

  data.frame(
    .pred_class = factor(
      light_model$class_levels[idx],
      levels = light_model$class_levels
    ),
    stringsAsFactors = FALSE
  )
}

predict_ai_light <- function(light_model, new_data) {
  pred_class <- predict_ai_light_class(
    light_model = light_model,
    new_data = new_data
  )

  pred_prob <- predict_ai_light_prob(
    light_model = light_model,
    new_data = new_data
  )

  cbind(pred_class, pred_prob)
}
