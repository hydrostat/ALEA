test_that("alea_gof() returns expected structure for an alea_fit object", {
  x <- c(
    18.2, 21.5, 24.1, 26.8, 29.4, 33.7, 37.9, 42.6,
    48.3, 55.1, 63.8, 74.5, 88.9, 106.4, 128.7,
    156.2, 191.5, 238.9, 302.4, 391.8
  )
  
  fit <- alea_fit(x, distribution = "gev", method = "lmom")
  gof <- alea_gof(fit)
  
  expect_s3_class(gof, "alea_gof")
  expect_s3_class(gof, "data.frame")
  
  expect_named(
    gof,
    c(
      "distribution",
      "method",
      "statistic",
      "estimate",
      "p_value",
      "p_value_method",
      "n",
      "n_parameters",
      "higher_is_better",
      "description"
    )
  )
  
  expect_equal(nrow(gof), 6L)
  expect_equal(gof$distribution, rep("gev", 6L))
  expect_equal(gof$method, rep("lmom", 6L))
  expect_equal(
    gof$statistic,
    c("ks", "cvm", "ad", "loglik", "aic", "bic")
  )
  
  expect_true(all(is.finite(gof$estimate)))
  expect_true(all(is.na(gof$p_value)))
  expect_true(all(is.na(gof$p_value_method)))
  expect_equal(gof$n, rep(length(x), 6L))
  expect_equal(gof$n_parameters, rep(3L, 6L))
})


test_that("alea_gof() can compute a selected subset of statistics", {
  x <- c(
    18.2, 21.5, 24.1, 26.8, 29.4, 33.7, 37.9, 42.6,
    48.3, 55.1, 63.8, 74.5, 88.9, 106.4, 128.7,
    156.2, 191.5, 238.9, 302.4, 391.8
  )
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  gof <- alea_gof(fit, statistics = c("ks", "aic"))
  
  expect_s3_class(gof, "alea_gof")
  expect_equal(nrow(gof), 2L)
  expect_equal(gof$statistic, c("ks", "aic"))
  expect_true(all(is.finite(gof$estimate)))
})


test_that("alea_gof() accepts statistics = 'all'", {
  x <- c(
    18.2, 21.5, 24.1, 26.8, 29.4, 33.7, 37.9, 42.6,
    48.3, 55.1, 63.8, 74.5, 88.9, 106.4, 128.7,
    156.2, 191.5, 238.9, 302.4, 391.8
  )
  
  fit <- alea_fit(x, distribution = "ln2", method = "lmom")
  gof <- alea_gof(fit, statistics = "all")
  
  expect_equal(
    gof$statistic,
    c("ks", "cvm", "ad", "loglik", "aic", "bic")
  )
})


test_that("alea_gof() validates statistic names", {
  x <- c(
    18.2, 21.5, 24.1, 26.8, 29.4, 33.7, 37.9, 42.6,
    48.3, 55.1, 63.8, 74.5, 88.9, 106.4, 128.7,
    156.2, 191.5, 238.9, 302.4, 391.8
  )
  
  fit <- alea_fit(x, distribution = "gev", method = "lmom")
  
  expect_error(
    alea_gof(fit, statistics = "chi_square"),
    "Unsupported GOF statistic"
  )
  
  expect_error(
    alea_gof(fit, statistics = numeric(0)),
    "`statistics` must contain at least one statistic",
    fixed = TRUE
  )
  
  expect_error(
    alea_gof(fit, statistics = 1),
    "`statistics` must be a character vector",
    fixed = TRUE
  )
})


test_that("alea_gof() validates input object class", {
  expect_error(
    alea_gof(1:10),
    "no applicable method"
  )
})


test_that("alea_gof() works for all supported distributions with L-moments", {
  x <- c(
    18.2, 21.5, 24.1, 26.8, 29.4, 33.7, 37.9, 42.6,
    48.3, 55.1, 63.8, 74.5, 88.9, 106.4, 128.7,
    156.2, 191.5, 238.9, 302.4, 391.8
  )
  
  distributions <- c("gev", "gpa", "pe3", "ln2", "ln3", "gum")
  
  for (distribution in distributions) {
    fit <- alea_fit(x, distribution = distribution, method = "lmom")
    gof <- alea_gof(fit)
    
    expect_s3_class(gof, "alea_gof")
    expect_equal(nrow(gof), 6L)
    expect_equal(unique(gof$distribution), distribution)
    expect_equal(unique(gof$method), "lmom")
    expect_true(all(is.finite(gof$estimate)))
  }
})


test_that("alea_gof() works for all supported estimation methods", {
  x <- c(
    18.2, 21.5, 24.1, 26.8, 29.4, 33.7, 37.9, 42.6,
    48.3, 55.1, 63.8, 74.5, 88.9, 106.4, 128.7,
    156.2, 191.5, 238.9, 302.4, 391.8
  )
  
  methods <- c("lmom", "mom", "mle")
  
  for (method in methods) {
    fit <- alea_fit(x, distribution = "gev", method = method)
    gof <- alea_gof(fit)
    
    expect_s3_class(gof, "alea_gof")
    expect_equal(nrow(gof), 6L)
    expect_equal(unique(gof$distribution), "gev")
    expect_equal(unique(gof$method), method)
    expect_true(all(is.finite(gof$estimate)))
  }
})


test_that("alea_gof() works for all supported distributions and methods", {
  x <- c(
    18.2, 21.5, 24.1, 26.8, 29.4, 33.7, 37.9, 42.6,
    48.3, 55.1, 63.8, 74.5, 88.9, 106.4, 128.7,
    156.2, 191.5, 238.9, 302.4, 391.8
  )
  
  distributions <- c("gev", "gpa", "pe3", "ln2", "ln3", "gum")
  methods <- c("lmom", "mom", "mle")
  
  for (distribution in distributions) {
    for (method in methods) {
      fit <- alea_fit(x, distribution = distribution, method = method)
      gof <- alea_gof(fit)
      
      expect_s3_class(gof, "alea_gof")
      expect_equal(nrow(gof), 6L)
      expect_equal(unique(gof$distribution), distribution)
      expect_equal(unique(gof$method), method)
      expect_true(all(is.finite(gof$estimate)))
      expect_true(all(gof$n == length(x)))
      expect_true(all(gof$n_parameters == length(fit$parameters)))
    }
  }
})


test_that("EDF statistics match direct internal calculation", {
  x <- c(
    18.2, 21.5, 24.1, 26.8, 29.4, 33.7, 37.9, 42.6,
    48.3, 55.1, 63.8, 74.5, 88.9, 106.4, 128.7,
    156.2, 191.5, 238.9, 302.4, 391.8
  )
  
  fit <- alea_fit(x, distribution = "gev", method = "lmom")
  gof <- alea_gof(fit, statistics = c("ks", "cvm", "ad"))
  
  u <- gof_cdf_values(
    x = fit$data,
    distribution = fit$distribution,
    para = fit$parameters
  )
  
  edf <- gof_edf_statistics_from_u(u)
  
  expect_equal(gof$estimate[gof$statistic == "ks"], edf$ks)
  expect_equal(gof$estimate[gof$statistic == "cvm"], edf$cvm)
  expect_equal(gof$estimate[gof$statistic == "ad"], edf$ad)
})


test_that("information criteria match direct internal calculation", {
  x <- c(
    18.2, 21.5, 24.1, 26.8, 29.4, 33.7, 37.9, 42.6,
    48.3, 55.1, 63.8, 74.5, 88.9, 106.4, 128.7,
    156.2, 191.5, 238.9, 302.4, 391.8
  )
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  gof <- alea_gof(fit, statistics = c("loglik", "aic", "bic"))
  
  loglik <- gof_loglik(
    x = fit$data,
    distribution = fit$distribution,
    para = fit$parameters
  )
  
  aic <- gof_aic(
    loglik = loglik,
    n_parameters = length(fit$parameters)
  )
  
  bic <- gof_bic(
    loglik = loglik,
    n_parameters = length(fit$parameters),
    n = length(fit$data)
  )
  
  expect_equal(gof$estimate[gof$statistic == "loglik"], loglik)
  expect_equal(gof$estimate[gof$statistic == "aic"], aic)
  expect_equal(gof$estimate[gof$statistic == "bic"], bic)
})


test_that("gof_cdf_values() returns probabilities in the open unit interval", {
  x <- c(
    18.2, 21.5, 24.1, 26.8, 29.4, 33.7, 37.9, 42.6,
    48.3, 55.1, 63.8, 74.5, 88.9, 106.4, 128.7,
    156.2, 191.5, 238.9, 302.4, 391.8
  )
  
  fit <- alea_fit(x, distribution = "ln3", method = "lmom")
  
  u <- gof_cdf_values(
    x = fit$data,
    distribution = fit$distribution,
    para = fit$parameters
  )
  
  expect_type(u, "double")
  expect_equal(length(u), length(x))
  expect_true(all(is.finite(u)))
  expect_true(all(u > 0))
  expect_true(all(u < 1))
})


test_that("gof_clamp_probability() clamps boundary probabilities", {
  p <- c(0, 1e-20, 0.5, 1 - 1e-20, 1)
  
  out <- gof_clamp_probability(p, eps = 1e-12)
  
  expect_true(all(out >= 1e-12))
  expect_true(all(out <= 1 - 1e-12))
  expect_equal(out[3], 0.5)
})


test_that("print.alea_gof() prints and returns invisibly", {
  x <- c(
    18.2, 21.5, 24.1, 26.8, 29.4, 33.7, 37.9, 42.6,
    48.3, 55.1, 63.8, 74.5, 88.9, 106.4, 128.7,
    156.2, 191.5, 238.9, 302.4, 391.8
  )
  
  fit <- alea_fit(x, distribution = "gev", method = "lmom")
  gof <- alea_gof(fit)
  
  expect_output(print(gof), "Goodness-of-fit results")
  expect_output(print(gof), "Distribution: gev")
  expect_output(print(gof), "Method: lmom")
})


test_that("as.data.frame.alea_gof() drops alea_gof class", {
  x <- c(
    18.2, 21.5, 24.1, 26.8, 29.4, 33.7, 37.9, 42.6,
    48.3, 55.1, 63.8, 74.5, 88.9, 106.4, 128.7,
    156.2, 191.5, 238.9, 302.4, 391.8
  )
  
  fit <- alea_fit(x, distribution = "gev", method = "lmom")
  gof <- alea_gof(fit)
  
  out <- as.data.frame(gof)
  
  expect_s3_class(out, "data.frame")
  expect_false(inherits(out, "alea_gof"))
  expect_equal(nrow(out), nrow(gof))
  expect_equal(names(out), names(gof))
})