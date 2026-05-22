test_that("PE3 CDF and quantile are inverse operations", {
  para <- c(mu = 10, sigma = 2, gamma = 0.8)
  p <- c(0.01, 0.1, 0.5, 0.9, 0.99)

  x <- ALEA:::q_pe3_internal(p, para)
  p2 <- ALEA:::p_pe3_internal(x, para)

  expect_equal(p2, p, tolerance = 1e-8)
})

test_that("PE3 wrappers agree with lmom", {
  para <- c(mu = 10, sigma = 2, gamma = 0.8)
  p <- c(0.2, 0.5, 0.8)
  x <- c(8, 10, 15)

  expect_equal(ALEA:::q_pe3_internal(p, para), lmom::quape3(p, para))
  expect_equal(ALEA:::p_pe3_internal(x, para), lmom::cdfpe3(x, para))
})

test_that("PE3 density is positive and finite on support", {
  para <- c(mu = 10, sigma = 2, gamma = 0.8)
  x <- ALEA:::q_pe3_internal(c(0.2, 0.5, 0.8), para)
  dens <- ALEA:::d_pe3_internal(x, para)

  expect_true(all(is.finite(dens)))
  expect_true(all(dens > 0))
})

test_that("PE3 density uses normal limit for zero skewness", {
  para <- c(mu = 10, sigma = 2, gamma = 0)
  x <- c(8, 10, 15)

  expect_equal(
    ALEA:::d_pe3_internal(x, para),
    stats::dnorm(x, mean = para[["mu"]], sd = para[["sigma"]])
  )
})

test_that("PE3 L-moment fitting returns expected structure", {
  set.seed(123)
  x <- ALEA:::r_pe3_internal(100, c(mu = 10, sigma = 2, gamma = 0.8))
  fit <- ALEA:::fit_pe3_lmom(x)

  expect_named(fit$parameters, c("mu", "sigma", "gamma"))
  expect_true(fit$convergence$converged)
})

test_that("PE3 return levels match quantiles", {
  para <- c(mu = 10, sigma = 2, gamma = 0.8)
  return_period <- c(2, 10, 100)

  expect_equal(
    ALEA:::return_level_pe3_internal(return_period, para),
    ALEA:::q_pe3_internal(1 - 1 / return_period, para)
  )
})
