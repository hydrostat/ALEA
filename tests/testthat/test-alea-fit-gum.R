test_that("alea_fit returns an alea_fit object for GUM L-moments", {
  set.seed(123)
  x <- ALEA:::r_gum_internal(100, c(xi = 10, alpha = 2))

  fit <- alea_fit(x, distribution = "gum", method = "lmom")

  expect_s3_class(fit, "alea_fit")
  expect_equal(fit$distribution, "gum")
  expect_equal(fit$method, "lmom")
  expect_named(fit$parameters, c("xi", "alpha"))
  expect_true(fit$convergence$converged)
  expect_equal(fit$n, length(x))
})

test_that("alea_fit computes GUM quantiles", {
  set.seed(123)
  x <- ALEA:::r_gum_internal(100, c(xi = 10, alpha = 2))
  return_period <- c(10, 25, 50)

  fit <- alea_fit(
    x,
    distribution = "gum",
    method = "lmom",
    return_period = return_period
  )

  expected <- ALEA:::quantile_gum_internal(return_period, fit$parameters)
  names(expected) <- paste0("T", return_period)

  expect_equal(fit$quantiles, expected)
})

test_that("alea_fit accepts all supported GUM estimation methods", {
  set.seed(321)
  x <- ALEA:::r_gum_internal(100, c(xi = 10, alpha = 2))

  for (method in c("lmom", "mom", "mle")) {
    fit <- alea_fit(x, distribution = "gum", method = method)

    expect_s3_class(fit, "alea_fit")
    expect_equal(fit$distribution, "gum")
    expect_equal(fit$method, method)
    expect_named(fit$parameters, c("xi", "alpha"))
  }
})

test_that("alea_fit rejects invalid methods with controlled errors", {
  x <- c(10, 12, 15, 18, 20, 22)

  expect_error(
    alea_fit(x, distribution = "gum", method = "invalid"),
    "method"
  )
})

test_that("coef.alea_fit returns fitted parameters", {
  x <- c(10, 12, 15, 18, 20, 22)
  fit <- alea_fit(x, distribution = "gum", method = "lmom")

  expect_equal(coef(fit, type = "internal"), fit$parameters)
  expect_named(coef(fit), c("location", "scale", "shape"))
})
