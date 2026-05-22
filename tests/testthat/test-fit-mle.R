test_that("MLE fitting works through alea_fit for all distributions", {
  set.seed(4001)

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
      method = "mle",
      return_period = c(10, 25)
    )

    expect_s3_class(fit, "alea_fit")
    expect_identical(fit$distribution, distribution)
    expect_identical(fit$method, "mle")
    expect_named(fit$parameters, expected_names[[distribution]])
    expect_true(all(is.finite(fit$parameters)))
    expect_true(is.list(fit$convergence))
    expect_identical(fit$convergence$method, "mle")
    expect_true(is.finite(fit$convergence$value))
    expect_named(fit$return_levels, c("T10", "T25"))
    expect_true(all(is.finite(fit$return_levels)))
  }
})

test_that("LN2 MLE agrees with closed-form lognormal estimates", {
  x <- c(5, 7, 8, 10, 12, 15, 20)
  fit <- alea_fit(x, distribution = "ln2", method = "mle")

  log_x <- log(x)
  expect_equal(unname(fit$parameters["mu"]), mean(log_x))
  expect_equal(
    unname(fit$parameters["sigma"]),
    sqrt(mean((log_x - mean(log_x))^2))
  )
})

test_that("MLE fitting rejects invalid LN2 input", {
  expect_error(
    alea_fit(c(-1, 1, 2, 3), distribution = "ln2", method = "mle"),
    "positive values"
  )
})

test_that("MLE estimates improve or match log-likelihood relative to L-moments", {
  set.seed(4002)

  cases <- list(
    gum = r_gum_internal(250, c(xi = 10, alpha = 2)),
    ln2 = r_ln2_internal(250, c(mu = 2, sigma = 0.35)),
    ln3 = r_ln3_internal(250, c(zeta = 1, mu = 2, sigma = 0.35)),
    gev = r_gev_internal(250, c(xi = 10, alpha = 2, k = 0.1)),
    gpa = r_gpa_internal(250, c(xi = 0, alpha = 2, k = 0.2)),
    pe3 = r_pe3_internal(250, c(mu = 10, sigma = 2, gamma = 0.8))
  )

  density_functions <- list(
    gum = d_gum_internal,
    ln2 = d_ln2_internal,
    ln3 = d_ln3_internal,
    gev = d_gev_internal,
    gpa = d_gpa_internal,
    pe3 = d_pe3_internal
  )

  for (distribution in names(cases)) {
    x <- cases[[distribution]]
    fit_lmom <- alea_fit(x, distribution = distribution, method = "lmom")
    fit_mle <- alea_fit(x, distribution = distribution, method = "mle")

    ll_lmom <- sum(density_functions[[distribution]](x, fit_lmom$parameters, log = TRUE))
    ll_mle <- sum(density_functions[[distribution]](x, fit_mle$parameters, log = TRUE))

    expect_true(is.finite(ll_lmom))
    expect_true(is.finite(ll_mle))
    expect_gte(ll_mle + 1e-6, ll_lmom)
  }
})
