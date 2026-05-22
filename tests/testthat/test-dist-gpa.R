test_that("GPA CDF and quantile are inverse operations", {
  para <- c(xi = 0, alpha = 2, k = -0.1)
  p <- c(0.01, 0.1, 0.5, 0.9, 0.99)

  x <- ALEA:::q_gpa_internal(p, para)
  p2 <- ALEA:::p_gpa_internal(x, para)

  expect_equal(p2, p, tolerance = 1e-10)
})

test_that("GPA wrappers agree with lmom", {
  para <- c(xi = 0, alpha = 2, k = -0.1)
  p <- c(0.2, 0.5, 0.8)
  x <- c(0, 1, 5)

  expect_equal(ALEA:::q_gpa_internal(p, para), lmom::quagpa(p, para))
  expect_equal(ALEA:::p_gpa_internal(x, para), lmom::cdfgpa(x, para))
})

test_that("GPA density is positive and finite on support", {
  para <- c(xi = 0, alpha = 2, k = -0.1)
  x <- ALEA:::q_gpa_internal(c(0.2, 0.5, 0.8), para)
  dens <- ALEA:::d_gpa_internal(x, para)

  expect_true(all(is.finite(dens)))
  expect_true(all(dens > 0))
})

test_that("GPA L-moment fitting returns expected structure", {
  set.seed(123)
  x <- ALEA:::r_gpa_internal(100, c(xi = 0, alpha = 2, k = -0.1))
  fit <- ALEA:::fit_gpa_lmom(x)

  expect_named(fit$parameters, c("xi", "alpha", "k"))
  expect_true(fit$convergence$converged)
})

test_that("GPA rejects invalid parameter vectors", {
  expect_error(ALEA:::p_gpa_internal(1, c(xi = 0, alpha = 0, k = 0)), "alpha")
  expect_error(ALEA:::p_gpa_internal(1, c(location = 0, scale = 1, shape = 0)), "names")
})

test_that("GPA return levels match quantiles when exceedance rate is one", {
  para <- c(xi = 0, alpha = 2, k = -0.1)
  return_period <- c(2, 10, 100)

  expect_equal(
    ALEA:::return_level_gpa_internal(return_period, para, exceedance_rate = 1),
    ALEA:::q_gpa_internal(1 - 1 / return_period, para)
  )
})
