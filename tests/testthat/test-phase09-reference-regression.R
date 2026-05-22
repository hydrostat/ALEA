# Phase 09 reference regression tests
#
# These tests add lightweight regression coverage for release validation.
#
# Scope:
# - bundled FADS_AI light-model files are discoverable;
# - validation CSV has the documented size and stable basic structure;
# - `alea_ai_model_info()` remains available as an S3 metadata object;
# - `alea_select()` remains stable at the user-facing level;
# - GOF and return-level reference workflows remain reproducible for a
#   deterministic sample.
#
# These tests intentionally avoid checking private implementation details
# more deeply than needed for release validation.

phase09_supported_distributions <- function() {
  c("gev", "gpa", "pe3", "ln2", "ln3", "gum")
}

phase09_reference_sample <- function() {
  c(
    78.2, 81.4, 84.7, 88.1, 91.5, 95.2, 97.8, 101.3, 104.6, 109.1,
    112.4, 116.8, 119.7, 123.5, 128.2, 132.9, 137.4, 143.1, 148.6,
    155.2, 161.9, 168.4, 176.3, 184.7, 193.5
  )
}

phase09_get_default_ai_model_file <- function() {
  system.file(
    "extdata",
    "fads_ai",
    "fads_ai_application_model_light.rds",
    package = "ALEA",
    mustWork = TRUE
  )
}

phase09_get_default_ai_validation_file <- function() {
  system.file(
    "extdata",
    "fads_ai",
    "fads_ai_application_model_light_validation.csv",
    package = "ALEA",
    mustWork = TRUE
  )
}

phase09_has_names <- function(object, required) {
  all(required %in% names(object))
}

testthat::test_that("Bundled FADS_AI light-model files are available", {
  model_file <- phase09_get_default_ai_model_file()
  validation_file <- phase09_get_default_ai_validation_file()
  
  testthat::expect_true(file.exists(model_file))
  testthat::expect_true(file.exists(validation_file))
  
  testthat::expect_match(basename(model_file), "light[.]rds$")
  testthat::expect_match(basename(validation_file), "light_validation[.]csv$")
})

testthat::test_that("FADS_AI light validation CSV keeps documented regression size", {
  validation_file <- phase09_get_default_ai_validation_file()
  validation <- utils::read.csv(validation_file, stringsAsFactors = FALSE)
  
  testthat::expect_s3_class(validation, "data.frame")
  testthat::expect_gte(nrow(validation), 1L)
  
  if ("n_validation_rows" %in% names(validation)) {
    testthat::expect_equal(validation$n_validation_rows[1], 1000L)
  }
  
  if ("max_abs_probability_difference" %in% names(validation)) {
    testthat::expect_equal(
      validation$max_abs_probability_difference[1],
      0,
      tolerance = 1e-12
    )
  }
  
  if ("class_agreement" %in% names(validation)) {
    testthat::expect_equal(
      validation$class_agreement[1],
      1,
      tolerance = 1e-12
    )
  }
  
  testthat::expect_gt(ncol(validation), 0L)
  
  testthat::expect_false(anyDuplicated(names(validation)) > 0)
  
  expected_feature_columns <- c(
    "lmom_l1", "lmom_l2", "lmom_l3",
    "lmom_l4", "lmom_t3", "lmom_t4"
  )
  
  if (all(expected_feature_columns %in% names(validation))) {
    feature_block <- validation[expected_feature_columns]
    
    testthat::expect_true(all(vapply(feature_block, is.numeric, logical(1))))
    testthat::expect_true(all(stats::complete.cases(feature_block)))
  }
  
  probability_like_columns <- grep(
    "^(GEV|GPA|PE3|LN2|LN3|GUM)$|prob",
    names(validation),
    value = TRUE,
    ignore.case = TRUE
  )
  
  if (length(probability_like_columns) > 0L) {
    probability_block <- validation[probability_like_columns]
    numeric_probability_columns <- vapply(
      probability_block,
      is.numeric,
      logical(1)
    )
    
    if (any(numeric_probability_columns)) {
      numeric_probability_block <- probability_block[numeric_probability_columns]
      
      testthat::expect_true(
        all(is.finite(as.matrix(numeric_probability_block)))
      )
    }
  }
})

testthat::test_that("alea_ai_model_info returns a stable user-facing metadata object", {
  info <- alea_ai_model_info(quiet = TRUE)
  
  testthat::expect_s3_class(info, "alea_ai_model_info")
  testthat::expect_type(info, "list")
  testthat::expect_gt(length(info), 0L)
  
  info_text <- paste(utils::capture.output(print(info)), collapse = "\n")
  
  testthat::expect_match(info_text, "FADS", ignore.case = TRUE)
  testthat::expect_match(info_text, "light", ignore.case = TRUE)
  testthat::expect_match(info_text, "decision", ignore.case = TRUE)
})

testthat::test_that("alea_select default workflow returns stable decision-support structure", {
  x <- phase09_reference_sample()
  
  selection <- alea_select(x, quiet = TRUE)
  
  testthat::expect_s3_class(selection, "alea_selection")
  testthat::expect_type(selection, "list")
  
  testthat::expect_true("selected_distribution" %in% names(selection))
  testthat::expect_true("ranking" %in% names(selection))
  testthat::expect_true("decision" %in% names(selection))
  testthat::expect_true("features" %in% names(selection))
  testthat::expect_true("model_info" %in% names(selection))
  
  testthat::expect_true(
    selection$selected_distribution %in% phase09_supported_distributions()
  )
  
  ranking <- as.data.frame(selection)
  
  testthat::expect_s3_class(ranking, "data.frame")
  testthat::expect_equal(nrow(ranking), 6L)
  testthat::expect_true(
    phase09_has_names(
      ranking,
      c("distribution", "probability", "rank", "selected")
    )
  )
  
  testthat::expect_setequal(
    ranking$distribution,
    phase09_supported_distributions()
  )
  testthat::expect_true(all(is.finite(ranking$probability)))
  testthat::expect_true(all(ranking$probability >= 0))
  testthat::expect_true(all(ranking$probability <= 1))
  testthat::expect_equal(sum(ranking$probability), 1, tolerance = 1e-6)
  
  ranking_by_rank <- ranking[order(ranking$rank), , drop = FALSE]
  
  testthat::expect_equal(ranking_by_rank$rank, seq_len(nrow(ranking_by_rank)))
  testthat::expect_true(
    all(diff(ranking_by_rank$probability) <= sqrt(.Machine$double.eps))
  )
  testthat::expect_equal(sum(ranking$selected), 1L)
  testthat::expect_identical(
    ranking$distribution[ranking$selected][1],
    selection$selected_distribution
  )
  
  if (is.data.frame(selection$decision)) {
    testthat::expect_equal(nrow(selection$decision), 1L)
    testthat::expect_true(
      phase09_has_names(
        selection$decision,
        c(
          "top_family",
          "top_support",
          "second_family",
          "second_support",
          "top1_top2_margin",
          "decision_strength",
          "interpretation"
        )
      )
    )
    testthat::expect_true(
      selection$decision$top_family %in% phase09_supported_distributions()
    )
    testthat::expect_true(is.finite(selection$decision$top1_top2_margin))
    testthat::expect_true(nzchar(selection$decision$interpretation))
  }
  
  expected_feature_columns <- c(
    "lmom_l1", "lmom_l2", "lmom_l3",
    "lmom_l4", "lmom_t3", "lmom_t4"
  )
  
  testthat::expect_s3_class(selection$features, "data.frame")
  testthat::expect_true(
    all(expected_feature_columns %in% names(selection$features))
  )
})

testthat::test_that("alea_select is stable for explicit bundled model_path", {
  x <- phase09_reference_sample()
  model_file <- phase09_get_default_ai_model_file()
  
  default_selection <- alea_select(x, quiet = TRUE)
  path_selection <- alea_select(x, model_path = model_file, quiet = TRUE)
  
  default_ranking <- as.data.frame(default_selection)
  path_ranking <- as.data.frame(path_selection)
  
  default_ranking <- default_ranking[order(default_ranking$distribution), ]
  path_ranking <- path_ranking[order(path_ranking$distribution), ]
  
  row.names(default_ranking) <- NULL
  row.names(path_ranking) <- NULL
  
  testthat::expect_identical(
    default_selection$selected_distribution,
    path_selection$selected_distribution
  )
  testthat::expect_equal(
    default_ranking$probability,
    path_ranking$probability,
    tolerance = 1e-12
  )
  testthat::expect_identical(
    default_ranking$distribution,
    path_ranking$distribution
  )
})

testthat::test_that("Deterministic GUM fit keeps stable return-level and GOF structure", {
  x <- phase09_reference_sample()
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  
  testthat::expect_s3_class(fit, "alea_fit")
  testthat::expect_identical(fit$distribution, "gum")
  testthat::expect_identical(fit$method, "lmom")
  
  return_period <- c(2, 5, 10, 25, 50, 100)
  rl <- alea_return_level(fit, return_period = return_period)
  
  testthat::expect_s3_class(rl, "alea_return_level")
  testthat::expect_equal(nrow(rl), length(return_period))
  testthat::expect_true(
    phase09_has_names(
      rl,
      c("distribution", "method", "return_period", "probability", "return_level")
    )
  )
  testthat::expect_equal(rl$return_period, return_period)
  testthat::expect_equal(rl$probability, 1 - 1 / return_period)
  testthat::expect_true(all(is.finite(rl$return_level)))
  testthat::expect_true(all(diff(rl$return_level) > 0))
  
  gof <- alea_gof(fit, statistics = "all")
  
  testthat::expect_s3_class(gof, "alea_gof")
  testthat::expect_true(
    phase09_has_names(
      gof,
      c(
        "distribution",
        "method",
        "statistic",
        "estimate",
        "p_value",
        "p_value_method",
        "n",
        "n_parameters",
        "higher_is_better",
        "description"
      )
    )
  )
  
  testthat::expect_setequal(
    gof$statistic,
    c("ks", "cvm", "ad", "loglik", "aic", "bic")
  )
  testthat::expect_true(all(is.finite(gof$estimate)))
  testthat::expect_true(all(gof$distribution == "gum"))
  testthat::expect_true(all(gof$method == "lmom"))
})

testthat::test_that("Reference bootstrap CI remains reproducible with a fixed seed", {
  x <- phase09_reference_sample()
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  return_period <- c(10, 50)
  
  ci_1 <- confint(
    fit,
    parm = "return_level",
    return_period = return_period,
    method = "bootstrap",
    n_boot = 25,
    seed = 909
  )
  
  ci_2 <- confint(
    fit,
    parm = "return_level",
    return_period = return_period,
    method = "bootstrap",
    n_boot = 25,
    seed = 909
  )
  
  testthat::expect_s3_class(ci_1, "alea_return_level_ci")
  testthat::expect_s3_class(ci_2, "alea_return_level_ci")
  
  testthat::expect_equal(ci_1, ci_2, tolerance = 1e-12)
  testthat::expect_equal(ci_1$return_period, return_period)
  testthat::expect_true(all(is.finite(ci_1$lower)))
  testthat::expect_true(all(is.finite(ci_1$upper)))
  testthat::expect_true(all(ci_1$lower <= ci_1$return_level))
  testthat::expect_true(all(ci_1$return_level <= ci_1$upper))
  testthat::expect_true(all(ci_1$n_success > 0))
  testthat::expect_true(all(ci_1$n_boot == 25))
})