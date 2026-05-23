test_that("Phase 13 FADS_AI light model metadata and prediction remain valid", {
  model_path <- system.file(
    "extdata",
    "fads_ai",
    "fads_ai_application_model_light.rds",
    package = "ALEA",
    mustWork = TRUE
  )
  
  validation_path <- system.file(
    "extdata",
    "fads_ai",
    "fads_ai_application_model_light_validation.csv",
    package = "ALEA",
    mustWork = TRUE
  )
  
  info <- alea_ai_model_info()
  
  expect_s3_class(info, "alea_ai_model_info")
  
  required_fields <- c(
    "model_name",
    "model_version",
    "parent_model_name",
    "parent_model_version",
    "scenario",
    "algorithm",
    "candidate_distributions",
    "original_candidate_labels",
    "feature_set",
    "feature_columns",
    "model_file",
    "model_available",
    "validation_file",
    "validation_available",
    "validation",
    "model_size_note",
    "interpretation_note"
  )
  
  expect_setequal(
    names(info),
    required_fields
  )
  
  expect_identical(info$model_version, "1.0.0-light")
  expect_identical(info$parent_model_version, "1.0.0")
  expect_identical(info$scenario, "classical")
  expect_identical(info$algorithm, "xgb")
  
  expect_identical(
    info$candidate_distributions,
    c("gev", "gpa", "pe3", "ln2", "ln3", "gum")
  )
  
  expect_identical(
    info$original_candidate_labels,
    c("GEV", "GPA", "PE3", "LN2", "LN3", "GUM")
  )
  
  expect_identical(
    info$feature_columns,
    c("lmom_l1", "lmom_l2", "lmom_l3", "lmom_l4", "lmom_t3", "lmom_t4")
  )
  
  expect_true(file.exists(model_path))
  expect_true(file.exists(validation_path))
  expect_true(isTRUE(info$model_available))
  expect_true(isTRUE(info$validation_available))
  
  validation <- read.csv(validation_path, stringsAsFactors = FALSE)
  
  expect_true(all(c(
    "n_validation_rows",
    "max_abs_probability_difference",
    "class_agreement"
  ) %in% names(validation)))
  
  n_validation_rows <- unique(validation$n_validation_rows)
  max_abs_probability_difference <- unique(validation$max_abs_probability_difference)
  class_agreement <- unique(validation$class_agreement)
  
  expect_length(n_validation_rows, 1)
  expect_length(max_abs_probability_difference, 1)
  expect_length(class_agreement, 1)
  
  expect_identical(as.integer(n_validation_rows), 1000L)
  expect_identical(as.numeric(max_abs_probability_difference), 0)
  expect_identical(as.numeric(class_agreement), 1)
  
  x <- c(
    42.1, 39.4, 51.7, 48.3, 55.2,
    60.1, 46.8, 53.9, 58.4, 62.7,
    49.5, 57.8, 64.3, 52.6, 59.9,
    61.5, 67.2, 54.8, 63.1, 69.4
  )
  
  selection <- alea_select(x, quiet = TRUE)
  
  expect_s3_class(selection, "alea_selection")
  
  expect_true(
    selection$selected_distribution %in% c("gev", "gpa", "pe3", "ln2", "ln3", "gum")
  )
  
  ranking <- selection$ranking
  
  expect_true(is.data.frame(ranking))
  expect_equal(nrow(ranking), 6)
  
  expect_setequal(
    ranking$distribution,
    c("gev", "gpa", "pe3", "ln2", "ln3", "gum")
  )
  
  expect_true(all(is.finite(ranking$probability)))
  expect_true(all(ranking$probability >= 0))
  expect_true(all(ranking$probability <= 1))
  
  expect_equal(
    sum(ranking$probability),
    1,
    tolerance = 1e-6,
    ignore_attr = TRUE
  )
  
  expect_identical(
    sort(ranking$rank),
    1:6
  )
  
  expect_equal(
    sum(ranking$selected),
    1,
    tolerance = 0
  )
  
  expect_identical(
    ranking$distribution[ranking$rank == 1],
    selection$selected_distribution
  )
})