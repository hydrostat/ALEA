test_that("alea_fit works for all implemented L-moment distributions", {
  set.seed(123)

  examples <- list(
    gum = list(x = ALEA:::r_gum_internal(120, c(xi = 10, alpha = 2)), names = c("xi", "alpha")),
    ln2 = list(x = ALEA:::r_ln2_internal(120, c(mu = 1, sigma = 0.5)), names = c("mu", "sigma")),
    ln3 = list(x = ALEA:::r_ln3_internal(120, c(zeta = 2, mu = 1, sigma = 0.5)), names = c("zeta", "mu", "sigma")),
    gev = list(x = ALEA:::r_gev_internal(120, c(xi = 10, alpha = 2, k = -0.1)), names = c("xi", "alpha", "k")),
    gpa = list(x = ALEA:::r_gpa_internal(120, c(xi = 0, alpha = 2, k = -0.1)), names = c("xi", "alpha", "k")),
    pe3 = list(x = ALEA:::r_pe3_internal(120, c(mu = 10, sigma = 2, gamma = 0.8)), names = c("mu", "sigma", "gamma"))
  )

  for (dist in names(examples)) {
    fit <- alea_fit(
      examples[[dist]]$x,
      distribution = dist,
      method = "lmom",
      return_period = c(10, 25, 50)
    )

    expect_s3_class(fit, "alea_fit")
    expect_equal(fit$distribution, dist)
    expect_equal(fit$method, "lmom")
    expect_named(fit$parameters, examples[[dist]]$names)
    expect_named(fit$return_levels, c("T10", "T25", "T50"))
    expect_true(fit$convergence$converged)
  }
})

test_that("alea_fit works for all implemented MOM and MLE distributions", {
  set.seed(456)

  examples <- list(
    gum = list(x = ALEA:::r_gum_internal(120, c(xi = 10, alpha = 2)), names = c("xi", "alpha")),
    ln2 = list(x = ALEA:::r_ln2_internal(120, c(mu = 1, sigma = 0.5)), names = c("mu", "sigma")),
    ln3 = list(x = ALEA:::r_ln3_internal(120, c(zeta = 2, mu = 1, sigma = 0.5)), names = c("zeta", "mu", "sigma")),
    gev = list(x = ALEA:::r_gev_internal(120, c(xi = 10, alpha = 2, k = -0.1)), names = c("xi", "alpha", "k")),
    gpa = list(x = ALEA:::r_gpa_internal(120, c(xi = 0, alpha = 2, k = -0.1)), names = c("xi", "alpha", "k")),
    pe3 = list(x = ALEA:::r_pe3_internal(120, c(mu = 10, sigma = 2, gamma = 0.8)), names = c("mu", "sigma", "gamma"))
  )

  for (method in c("mom", "mle")) {
    for (dist in names(examples)) {
      fit <- alea_fit(
        examples[[dist]]$x,
        distribution = dist,
        method = method,
        return_period = c(10, 25, 50)
      )

      expect_s3_class(fit, "alea_fit")
      expect_equal(fit$distribution, dist)
      expect_equal(fit$method, method)
      expect_named(fit$parameters, examples[[dist]]$names)
      expect_named(fit$return_levels, c("T10", "T25", "T50"))
      expect_true(is.logical(fit$convergence$converged))
    }
  }
})
