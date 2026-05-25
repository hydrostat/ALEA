test_that("MOM fitting works through alea_fit for all distributions", {
  set.seed(3001)

  cases <- list(
    gum = r_gum_internal(500, c(xi = 10, alpha = 2)),
    ln2 = r_ln2_internal(500, c(mu = 2, sigma = 0.35)),
    ln3 = r_ln3_internal(500, c(zeta = 1, mu = 2, sigma = 0.35)),
    gev = r_gev_internal(500, c(xi = 10, alpha = 2, k = 0.1)),
    gpa = r_gpa_internal(500, c(xi = 0, alpha = 2, k = 0.2)),
    pe3 = r_pe3_internal(500, c(mu = 10, sigma = 2, gamma = 0.8))
  )

  expected_names <- list(
    gum = c("xi", "alpha"),
    ln2 = c("mu", "sigma"),
    ln3 = c("zeta", "mu", "sigma"),
    gev = c("xi", "alpha", "k"),
    gpa = c("xi", "alpha", "k"),
    pe3 = c("mu", "sigma", "gamma")
  )

  for (distribution in names(cases)) {
    fit <- alea_fit(
      cases[[distribution]],
      distribution = distribution,
      method = "mom",
      return_period = c(10, 25)
    )

    expect_s3_class(fit, "alea_fit")
    expect_identical(fit$distribution, distribution)
    expect_identical(fit$method, "mom")
    expect_named(fit$parameters, expected_names[[distribution]])
    expect_true(all(is.finite(fit$parameters)))
    expect_true(isTRUE(fit$convergence$converged))
    expect_named(fit$quantiles, c("T10", "T25"))
    expect_true(all(is.finite(fit$quantiles)))
  }
})

test_that("MOM fitting rejects invalid LN2 input", {
  expect_error(
    alea_fit(c(-1, 1, 2, 3), distribution = "ln2", method = "mom"),
    "positive values"
  )
})

test_that("MOM fitting rejects non-positive LN3 skewness", {
  expect_error(
    alea_fit(c(-2, -1, 0, 1, 2), distribution = "ln3", method = "mom"),
    "positive sample skewness"
  )
})

test_that("PE3 MOM parameters match sample moments in ALEA-R parameterization", {
  x <- c(2, 4, 7, 8, 10, 15, 20)
  fit <- alea_fit(x, distribution = "pe3", method = "mom")

  st <- sample_moments_internal(x, min_length = 3L)

  expect_equal(unname(fit$parameters["mu"]), st$mean)
  expect_equal(unname(fit$parameters["sigma"]), st$sd)
  expect_equal(unname(fit$parameters["gamma"]), st$skewness)
})

test_that("GUM MOM agrees with classical closed-form estimates", {
  x <- c(10, 11, 13, 16, 20, 25)
  fit <- alea_fit(x, distribution = "gum", method = "mom")
  st <- sample_moments_internal(x, min_length = 2L)

  alpha <- sqrt(6) * st$sd / pi
  xi <- st$mean - 0.5772156649015329 * alpha

  expect_equal(unname(fit$parameters["xi"]), xi)
  expect_equal(unname(fit$parameters["alpha"]), alpha)
})
