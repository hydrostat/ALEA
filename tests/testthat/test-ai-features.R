test_that("extract_ai_feature_names returns the deployed FADS_AI feature order", {
  expected <- c(
    "lmom_l1",
    "lmom_l2",
    "lmom_l3",
    "lmom_l4",
    "lmom_t3",
    "lmom_t4"
  )

  expect_identical(extract_ai_feature_names(), expected)
  expect_identical(extract_ai_feature_names("fads_ai_classical_v1"), expected)
})

test_that("extract_ai_features returns one row with the expected columns", {
  x <- c(421.3, 358.7, 512.4, 466.1, 590.8, 533.6, 608.2, 487.9)

  features <- extract_ai_features(x)

  expect_s3_class(features, "data.frame")
  expect_equal(nrow(features), 1L)
  expect_identical(names(features), extract_ai_feature_names())
  expect_true(all(vapply(features, is.numeric, logical(1))))
})

test_that("extract_ai_features agrees with direct lmom::samlmu calculations", {
  skip_if_not_installed("lmom")

  x <- c(421.3, 358.7, 512.4, 466.1, 590.8, 533.6, 608.2, 487.9)
  lm <- as.numeric(lmom::samlmu(x, nmom = 4L))

  features <- extract_ai_features(x)

  expect_equal(features$lmom_l1, lm[1L])
  expect_equal(features$lmom_l2, lm[2L])
  expect_equal(features$lmom_l3, lm[3L])
  expect_equal(features$lmom_l4, lm[4L])
  expect_equal(features$lmom_t3, lm[3L] / lm[2L])
  expect_equal(features$lmom_t4, lm[4L] / lm[2L])
})

test_that("extract_ai_features is deterministic", {
  x <- c(421.3, 358.7, 512.4, 466.1, 590.8, 533.6, 608.2, 487.9)

  features_1 <- extract_ai_features(x)
  features_2 <- extract_ai_features(x)

  expect_equal(features_1, features_2)
})

test_that("extract_ai_features removes non-finite values before computing features", {
  skip_if_not_installed("lmom")

  x <- c(421.3, 358.7, NA, 512.4, Inf, 466.1, 590.8, 533.6, 608.2, 487.9)
  x_finite <- x[is.finite(x)]

  features <- extract_ai_features(x)
  expected <- as.numeric(lmom::samlmu(x_finite, nmom = 4L))

  expect_equal(features$lmom_l1, expected[1L])
  expect_equal(features$lmom_l2, expected[2L])
  expect_equal(features$lmom_l3, expected[3L])
  expect_equal(features$lmom_l4, expected[4L])
})

test_that("extract_ai_features validates numeric input", {
  expect_error(
    extract_ai_features(letters[1:8]),
    "`x` must be a numeric vector.",
    fixed = TRUE
  )
})

test_that("extract_ai_features requires at least four finite observations", {
  expect_error(
    extract_ai_features(c(1, 2, 3, NA, Inf)),
    "At least 4 finite observations are required to compute AI features.",
    fixed = TRUE
  )
})

test_that("build_ai_application_row returns metadata plus FADS_AI features", {
  x <- c(421.3, 358.7, 512.4, 466.1, 590.8, 533.6, 608.2, 487.9)

  row <- build_ai_application_row(
    x,
    sample_id = "station_001",
    param_id = "observed"
  )

  expected_names <- c(
    "sample_id",
    "param_id",
    "n",
    "replicate_id",
    "seed",
    "par1",
    "par2",
    "par3",
    extract_ai_feature_names()
  )

  expect_s3_class(row, "data.frame")
  expect_equal(nrow(row), 1L)
  expect_identical(names(row), expected_names)
  expect_equal(row$sample_id, "station_001")
  expect_equal(row$param_id, "observed")
  expect_equal(row$n, length(x))
})

test_that("build_ai_application_row validates sample_id and param_id", {
  x <- c(421.3, 358.7, 512.4, 466.1, 590.8, 533.6, 608.2, 487.9)

  expect_error(
    build_ai_application_row(x, sample_id = NA_character_),
    "`sample_id` must be a single non-missing character string.",
    fixed = TRUE
  )

  expect_error(
    build_ai_application_row(x, param_id = NA_character_),
    "`param_id` must be a single non-missing character string.",
    fixed = TRUE
  )
})

test_that("validate_ai_feature_row accepts valid feature rows invisibly", {
  x <- c(421.3, 358.7, 512.4, 466.1, 590.8, 533.6, 608.2, 487.9)
  features <- extract_ai_features(x)

  expect_invisible(validate_ai_feature_row(features))
})

test_that("validate_ai_feature_row rejects non-data-frame input", {
  expect_error(
    validate_ai_feature_row(list(lmom_l1 = 1)),
    "`feature_row` must be a data frame.",
    fixed = TRUE
  )
})

test_that("validate_ai_feature_row detects missing required columns", {
  feature_row <- data.frame(
    lmom_l1 = 1,
    lmom_l2 = 2,
    stringsAsFactors = FALSE
  )

  expect_error(
    validate_ai_feature_row(feature_row),
    "The AI feature row is missing required feature columns:",
    fixed = TRUE
  )
})

test_that("validate_ai_feature_row detects non-numeric required columns", {
  feature_row <- data.frame(
    lmom_l1 = 1,
    lmom_l2 = 2,
    lmom_l3 = 3,
    lmom_l4 = 4,
    lmom_t3 = "0.1",
    lmom_t4 = 0.2,
    stringsAsFactors = FALSE
  )

  expect_error(
    validate_ai_feature_row(feature_row),
    "The following AI feature columns must be numeric:",
    fixed = TRUE
  )
})
