# Tests for AI-assisted model selection with the FADS_AI light model --------

sample_for_selection <- function() {
  c(12.1, 33.0, 15.7, 65.2, 22.3, 44.0, 51.6, 28.4)
}

fads_light_model_path <- function() {
  system.file(
    "extdata",
    "fads_ai",
    "fads_ai_application_model_light.rds",
    package = "ALEA",
    mustWork = TRUE
  )
}

fads_light_validation_path <- function() {
  system.file(
    "extdata",
    "fads_ai",
    "fads_ai_application_model_light_validation.csv",
    package = "ALEA",
    mustWork = TRUE
  )
}

test_that("bundled FADS_AI light model file is available", {
  expect_true(file.exists(fads_light_model_path()))
  expect_true(file.exists(fads_light_validation_path()))
})

test_that("alea_ai_model_info reports bundled FADS_AI light model metadata", {
  info <- alea_ai_model_info()
  
  expect_s3_class(info, "alea_ai_model_info")
  expect_equal(info$model_name, "FADS_AI lightweight operational application model")
  expect_equal(info$model_version, "1.0.0-light")
  expect_equal(info$parent_model_name, "FADS_AI final application model")
  expect_equal(info$parent_model_version, "1.0.0")
  expect_equal(info$scenario, "classical")
  expect_equal(info$algorithm, "xgb")
  expect_equal(info$feature_set, "fads_ai_classical_v1")
  expect_equal(info$candidate_distributions, c("gev", "gpa", "pe3", "ln2", "ln3", "gum"))
  expect_true(info$model_available)
  expect_equal(info$validation$n_validation_rows, 1000)
  expect_equal(info$validation$max_abs_probability_difference, 0)
  expect_equal(info$validation$class_agreement, 1)
})

test_that("print.alea_ai_model_info returns the object invisibly", {
  info <- alea_ai_model_info()
  
  expect_output(out <- print(info), "ALEA-R AI model metadata")
  expect_s3_class(out, "alea_ai_model_info")
})

test_that("alea_select works with the bundled FADS_AI light model", {
  selection <- alea_select(sample_for_selection())
  
  expect_s3_class(selection, "alea_selection")
  expect_type(selection, "list")
  
  expect_equal(selection$selection_method, "ai")
  expect_true(selection$selected_distribution %in% c("gev", "gpa", "pe3", "ln2", "ln3", "gum"))
  expect_true(is.na(selection$selected_method))
  
  expect_s3_class(selection$ranking, "data.frame")
  expect_identical(
    names(selection$ranking),
    c("distribution", "probability", "rank", "selected")
  )
  expect_equal(nrow(selection$ranking), 6L)
  expect_true(all(selection$ranking$distribution %in% c("gev", "gpa", "pe3", "ln2", "ln3", "gum")))
  expect_true(all(is.finite(selection$ranking$probability)))
  expect_equal(sum(selection$ranking$probability), 1, tolerance = 1e-6)
  expect_equal(selection$ranking$rank, seq_len(6L))
  expect_equal(sum(selection$ranking$selected), 1L)
  expect_equal(
    selection$ranking$distribution[selection$ranking$selected],
    selection$selected_distribution
  )
  
  expect_s3_class(selection$features, "data.frame")
  expect_identical(
    names(selection$features),
    c("lmom_l1", "lmom_l2", "lmom_l3", "lmom_l4", "lmom_t3", "lmom_t4")
  )
  expect_equal(nrow(selection$features), 1L)
  expect_true(all(vapply(selection$features, is.numeric, logical(1))))
  
  expect_s3_class(selection$decision, "data.frame")
  expect_identical(
    names(selection$decision),
    c(
      "row_id",
      "predicted_family",
      "top_family",
      "top_support",
      "second_family",
      "second_support",
      "top1_top2_margin",
      "decision_strength",
      "interpretation"
    )
  )
  expect_equal(nrow(selection$decision), 1L)
  expect_equal(selection$decision$predicted_family, selection$selected_distribution)
  expect_equal(selection$decision$top_family, selection$selected_distribution)
  expect_true(is.finite(selection$decision$top_support))
  expect_true(is.finite(selection$decision$second_support))
  expect_true(is.finite(selection$decision$top1_top2_margin))
  expect_true(selection$decision$decision_strength %in% c(
    "ambiguous support",
    "weak to moderate support",
    "moderate support",
    "strong support"
  ))
  expect_true(nzchar(selection$decision$interpretation))
  
  expect_equal(selection$model_info$model_name, "FADS_AI lightweight operational application model")
  expect_equal(selection$model_info$model_version, "1.0.0-light")
  expect_equal(selection$model_info$scenario, "classical")
  expect_equal(selection$model_info$algorithm, "xgb")
})

test_that("alea_select works with a pre-loaded FADS_AI light model", {
  light_model <- readRDS(fads_light_model_path())
  
  selection <- alea_select(
    sample_for_selection(),
    model = light_model
  )
  
  expect_s3_class(selection, "alea_selection")
  expect_true(selection$selected_distribution %in% c("gev", "gpa", "pe3", "ln2", "ln3", "gum"))
  expect_equal(nrow(selection$ranking), 6L)
  expect_true(all(is.finite(selection$ranking$probability)))
})

test_that("alea_select works when the light model is supplied through model_path", {
  expect_message(
    selection <- alea_select(
      sample_for_selection(),
      model_path = fads_light_model_path(),
      quiet = FALSE
    ),
    "Loading FADS_AI light model from `model_path`.",
    fixed = TRUE
  )
  
  expect_s3_class(selection, "alea_selection")
  expect_true(selection$selected_distribution %in% c("gev", "gpa", "pe3", "ln2", "ln3", "gum"))
})

test_that("alea_select can suppress model_path loading messages", {
  expect_silent(
    selection <- alea_select(
      sample_for_selection(),
      model_path = fads_light_model_path(),
      quiet = TRUE
    )
  )
  
  expect_s3_class(selection, "alea_selection")
})

test_that("alea_select rejects ambiguous model inputs", {
  light_model <- readRDS(fads_light_model_path())
  
  expect_error(
    alea_select(
      sample_for_selection(),
      model = light_model,
      model_path = fads_light_model_path()
    ),
    "Use only one of `model` or `model_path`, not both.",
    fixed = TRUE
  )
})

test_that("alea_select validates model_path", {
  expect_error(
    alea_select(sample_for_selection(), model_path = "missing-model-file.rds"),
    "`model_path` does not exist:",
    fixed = TRUE
  )
})

test_that("alea_select.alea_fit uses the fitted object's stored data", {
  fit <- list(data = sample_for_selection())
  class(fit) <- "alea_fit"
  
  selection <- alea_select(fit)
  
  expect_s3_class(selection, "alea_selection")
  expect_true(selection$selected_distribution %in% c("gev", "gpa", "pe3", "ln2", "ln3", "gum"))
})

test_that("alea_select.alea_fit requires numeric object$data", {
  fit <- list(data = letters[1:5])
  class(fit) <- "alea_fit"
  
  expect_error(
    alea_select(fit),
    "`object$data` must contain the original numeric sample.",
    fixed = TRUE
  )
})

test_that("as.data.frame.alea_selection returns the ranking table", {
  selection <- alea_select(sample_for_selection())
  
  out <- as.data.frame(selection)
  
  expect_s3_class(out, "data.frame")
  expect_identical(out, selection$ranking)
})

test_that("print.alea_selection returns the object invisibly", {
  selection <- alea_select(sample_for_selection())
  
  expect_output(out <- print(selection), "ALEA-R AI-assisted distribution-selection support")
  expect_s3_class(out, "alea_selection")
})

test_that("alea_select returns a documented decision-strength label", {
  selection <- alea_select(sample_for_selection())
  
  expect_true(selection$decision$decision_strength %in% c(
    "ambiguous support",
    "weak to moderate support",
    "moderate support",
    "strong support"
  ))
  
  expect_true(is.finite(selection$decision$top1_top2_margin))
  expect_gte(selection$decision$top1_top2_margin, 0)
})

test_that("alea_select validates unsupported model distributions after normalizing case", {
  light_model <- readRDS(fads_light_model_path())
  light_model$class_levels <- c("GEV", "LP3")
  
  expect_error(
    alea_select(
      sample_for_selection(),
      model = light_model
    ),
    "The FADS_AI light model contains candidate distributions not supported by ALEA-R: lp3",
    fixed = TRUE
  )
})