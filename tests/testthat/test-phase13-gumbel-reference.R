test_that("Phase 13 Gumbel quantiles match closed-form reference", {
  x <- c(
    42.1, 39.4, 51.7, 48.3, 55.2,
    60.1, 46.8, 53.9, 58.4, 62.7,
    49.5, 57.8, 64.3, 52.6, 59.9,
    61.5, 67.2, 54.8, 63.1, 69.4
  )
  
  return_period <- c(2, 5, 10, 25, 50, 100)
  
  fit <- alea_fit(
    x,
    distribution = "gum",
    method = "lmom"
  )
  
  rl <- alea_quantile(
    fit,
    return_period = return_period
  )
  
  params <- coef(fit, type = "internal")
  
  xi <- unname(params[["xi"]])
  alpha <- unname(params[["alpha"]])
  
  probability_reference <- 1 - 1 / return_period
  quantile_reference <- xi - alpha * log(-log(probability_reference))
  
  expect_s3_class(fit, "alea_fit")
  expect_s3_class(rl, "alea_quantile")
  
  expect_identical(
    as.character(rl$distribution),
    rep("gum", length(return_period))
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
    probability_reference,
    tolerance = 1e-12,
    ignore_attr = TRUE
  )
  
  expect_equal(
    rl$quantile,
    quantile_reference,
    tolerance = 1e-10,
    ignore_attr = TRUE
  )
  
  expect_true(all(diff(rl$quantile) > 0))
})