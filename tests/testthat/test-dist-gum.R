test_that("GUM CDF and quantile are inverse operations", {
  para <- c(xi = 10, alpha = 2)
  p <- c(0.01, 0.1, 0.5, 0.9, 0.99)

  x <- ALEA:::q_gum_internal(p, para)
  p2 <- ALEA:::p_gum_internal(x, para)

  expect_equal(p2, p, tolerance = 1e-10)
})

test_that("GUM wrappers agree with lmom", {
  para <- c(xi = 10, alpha = 2)
  p <- c(0.2, 0.5, 0.8)
  x <- c(8, 10, 15)

  expect_equal(ALEA:::q_gum_internal(p, para), lmom::quagum(p, para))
  expect_equal(ALEA:::p_gum_internal(x, para), lmom::cdfgum(x, para))
})

test_that("GUM density is positive and finite", {
  para <- c(xi = 10, alpha = 2)
  x <- c(8, 10, 15)

  dens <- ALEA:::d_gum_internal(x, para)

  expect_true(all(is.finite(dens)))
  expect_true(all(dens > 0))
})

test_that("GUM rejects invalid parameter vectors", {
  expect_error(ALEA:::p_gum_internal(1, c(xi = 0, alpha = 0)), "alpha")
  expect_error(ALEA:::p_gum_internal(1, c(location = 0, scale = 1)), "names")
  expect_error(ALEA:::q_gum_internal(c(0, 0.5), c(xi = 0, alpha = 1)), "probability")
})

test_that("GUM return levels match quantiles", {
  para <- c(xi = 10, alpha = 2)
  return_period <- c(2, 10, 100)

  rl <- ALEA:::return_level_gum_internal(return_period, para)
  q <- ALEA:::q_gum_internal(1 - 1 / return_period, para)

  expect_equal(rl, q, tolerance = 1e-10)
})

test_that("GUM random generation returns expected length", {
  para <- c(xi = 10, alpha = 2)
  set.seed(123)
  x <- ALEA:::r_gum_internal(20, para)

  expect_length(x, 20)
  expect_true(all(is.finite(x)))
})
