test_that("LN2 CDF and quantile are inverse operations", {
  para <- c(mu = 1, sigma = 0.5)
  p <- c(0.01, 0.1, 0.5, 0.9, 0.99)

  x <- ALEA:::q_ln2_internal(p, para)
  p2 <- ALEA:::p_ln2_internal(x, para)

  expect_equal(p2, p, tolerance = 1e-10)
})

test_that("LN2 wrappers agree with lmom LN3 with zeta zero", {
  para <- c(mu = 1, sigma = 0.5)
  para3 <- c(zeta = 0, mu = 1, sigma = 0.5)
  p <- c(0.2, 0.5, 0.8)
  x <- c(1, 3, 8)

  expect_equal(ALEA:::q_ln2_internal(p, para), lmom::qualn3(p, para3))
  expect_equal(ALEA:::p_ln2_internal(x, para), lmom::cdfln3(x, para3))
})

test_that("LN2 density agrees with dlnorm", {
  para <- c(mu = 1, sigma = 0.5)
  x <- c(1, 3, 8)

  expect_equal(
    ALEA:::d_ln2_internal(x, para),
    stats::dlnorm(x, meanlog = para[["mu"]], sdlog = para[["sigma"]])
  )
})

test_that("LN2 L-moment fitting returns expected structure", {
  set.seed(123)
  x <- ALEA:::r_ln2_internal(100, c(mu = 1, sigma = 0.5))
  fit <- ALEA:::fit_ln2_lmom(x)

  expect_named(fit$parameters, c("mu", "sigma"))
  expect_true(fit$convergence$converged)
  expect_equal(fit$convergence$engine, "lmom::pelln3(bound = 0)")
})

test_that("LN2 rejects non-positive observations during fitting", {
  expect_error(ALEA:::fit_ln2_lmom(c(0, 1, 2, 3)), "positive")
})

test_that("LN2 quantiles match quantiles", {
  para <- c(mu = 1, sigma = 0.5)
  return_period <- c(2, 10, 100)

  expect_equal(
    ALEA:::quantile_ln2_internal(return_period, para),
    ALEA:::q_ln2_internal(1 - 1 / return_period, para)
  )
})
