test_that("alea_dist reports standardized parameter mapping", {
  info <- alea_dist(c("gev", "ln3"))

  expect_s3_class(info, "alea_dist")
  expect_true(all(c("distribution", "internal_parameter", "output_column", "description") %in% names(info)))
  expect_true(all(c("location", "scale", "shape") %in% info$output_column))
})

test_that("alea_quantile uses standardized parameter columns", {
  x <- c(32, 41, 38, 45, 51, 48, 57, 62, 59, 66, 70, 74)
  fit <- alea_fit(x, distribution = "gum", method = "lmom")

  q <- alea_quantile(fit, return_period = c(10, 50))

  expect_s3_class(q, "alea_quantile")
  expect_true(all(c("distribution", "method", "return_period", "probability", "quantile", "location", "scale", "shape") %in% names(q)))
  expect_false(paste0("return", "_", "level") %in% names(q))
})

test_that("alea_fit dispatches data-frame input to alea_batch", {
  dat <- data.frame(
    station = rep(c("A", "B"), each = 8),
    year = rep(2001:2008, 2),
    value = c(42, 44, 48, 51, 55, 57, 62, 66, 31, 35, 37, 40, 43, 45, 48, 52)
  )

  batch <- alea_fit(
    dat,
    station = "station",
    value = "value",
    time = "year",
    distribution = c("gum"),
    method = "lmom",
    return_period = c(10, 25)
  )

  expect_s3_class(batch, "alea_batch")
  expect_true("quantiles" %in% names(batch))
  expect_true(nrow(batch$quantiles) > 0)
  expect_false(paste0("return", "_", "level") %in% names(batch$quantiles))
})
