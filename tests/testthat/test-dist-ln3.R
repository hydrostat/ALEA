test_that("LN3 CDF and quantile are inverse operations", {
  para <- c(zeta = 2, mu = 1, sigma = 0.5)
  p <- c(0.01, 0.1, 0.5, 0.9, 0.99)

  x <- ALEA:::q_ln3_internal(p, para)
  p2 <- ALEA:::p_ln3_internal(x, para)

  expect_equal(p2, p, tolerance = 1e-10)
})

test_that("LN3 wrappers agree with lmom", {
  para <- c(zeta = 2, mu = 1, sigma = 0.5)
  p <- c(0.2, 0.5, 0.8)
  x <- c(3, 5, 10)

  expect_equal(ALEA:::q_ln3_internal(p, para), lmom::qualn3(p, para))
  expect_equal(ALEA:::p_ln3_internal(x, para), lmom::cdfln3(x, para))
})

test_that("LN3 density agrees with shifted dlnorm", {
  para <- c(zeta = 2, mu = 1, sigma = 0.5)
  x <- c(3, 5, 10)

  expect_equal(
    ALEA:::d_ln3_internal(x, para),
    stats::dlnorm(x - para[["zeta"]], meanlog = para[["mu"]], sdlog = para[["sigma"]])
  )
})

test_that("LN3 density is zero below lower bound", {
  para <- c(zeta = 2, mu = 1, sigma = 0.5)
  expect_equal(ALEA:::d_ln3_internal(c(1, 2), para), c(0, 0))
})

test_that("LN3 L-moment fitting returns expected structure", {
  set.seed(123)
  x <- ALEA:::r_ln3_internal(100, c(zeta = 2, mu = 1, sigma = 0.5))
  fit <- ALEA:::fit_ln3_lmom(x)

  expect_named(fit$parameters, c("zeta", "mu", "sigma"))
  expect_true(fit$convergence$converged)
})

test_that("LN3 quantiles match quantiles", {
  para <- c(zeta = 2, mu = 1, sigma = 0.5)
  return_period <- c(2, 10, 100)

  expect_equal(
    ALEA:::quantile_ln3_internal(return_period, para),
    ALEA:::q_ln3_internal(1 - 1 / return_period, para)
  )
})
