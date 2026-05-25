test_that("Phase 13 LN2 quantiles match LN3 zero-threshold convention", {
  x <- c(
    42.1, 39.4, 51.7, 48.3, 55.2,
    60.1, 46.8, 53.9, 58.4, 62.7,
    49.5, 57.8, 64.3, 52.6, 59.9,
    61.5, 67.2, 54.8, 63.1, 69.4
  )
  
  return_period <- c(2, 5, 10, 25, 50, 100)
  probability <- 1 - 1 / return_period
  
  fit <- alea_fit(
    x,
    distribution = "ln2",
    method = "lmom"
  )
  
  rl <- alea_quantile(
    fit,
    return_period = return_period
  )
  
  params <- coef(fit, type = "internal")
  
  mu <- unname(params[["mu"]])
  sigma <- unname(params[["sigma"]])
  
  reference <- exp(mu + sigma * stats::qnorm(probability))
  
  expect_s3_class(fit, "alea_fit")
  expect_s3_class(rl, "alea_quantile")
  
  expect_identical(
    as.character(rl$distribution),
    rep("ln2", length(return_period))
  )
  
  expect_identical(
    as.character(rl$method),
    rep("lmom", length(return_period))
  )
  
  expect_equal(
    rl$return_period,
    return_period,
    tolerance = 1e-12,
    ignore_attr = TRUE
  )
  
  expect_equal(
    rl$probability,
    probability,
    tolerance = 1e-12,
    ignore_attr = TRUE
  )
  
  expect_true(is.finite(mu))
  expect_true(is.finite(sigma))
  expect_gt(sigma, 0)
  
  expect_equal(
    rl$quantile,
    reference,
    tolerance = 1e-10,
    ignore_attr = TRUE
  )
  
  expect_true(all(is.finite(rl$quantile)))
  expect_true(all(diff(rl$quantile) > 0))
})