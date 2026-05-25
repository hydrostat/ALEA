# Phase 09 numerical benchmark tests
#
# Lightweight deterministic numerical benchmarks for the Phase 09 validation
# layer.
#
# Confirmed internal signatures:
# - q_<dist>_internal(p, para)
# - p_<dist>_internal(q, para)
# - quantile_<dist>_internal(return_period, para, ...)
#
# ALEA-R uses Hosking/lmom-style numeric parameter vectors in the internal
# distribution wrappers.

phase09_supported_distributions <- function() {
  c("gev", "gpa", "pe3", "ln2", "ln3", "gum")
}

phase09_fixed_parameters <- function() {
  list(
    gev = c(xi = 100, alpha = 20, k = 0.10),
    gpa = c(xi = 0, alpha = 15, k = -0.10),
    pe3 = c(mu = 100, sigma = 20, gamma = 0.50),
    ln2 = c(mu = 4.50, sigma = 0.25),
    ln3 = c(zeta = 20, mu = 4.30, sigma = 0.20),
    gum = c(xi = 100, alpha = 20)
  )
}

phase09_p <- function(distribution, q, para) {
  fun <- get(sprintf("p_%s_internal", distribution), mode = "function")
  fun(q = q, para = para)
}

phase09_q <- function(distribution, p, para) {
  fun <- get(sprintf("q_%s_internal", distribution), mode = "function")
  fun(p = p, para = para)
}

phase09_quantile <- function(distribution, return_period, para) {
  fun <- get(sprintf("quantile_%s_internal", distribution), mode = "function")
  fun(return_period = return_period, para = para)
}

testthat::test_that("Phase 09 benchmark helper assumptions are valid", {
  supported <- phase09_supported_distributions()
  
  testthat::expect_setequal(
    supported,
    c("gev", "gpa", "pe3", "ln2", "ln3", "gum")
  )
  
  for (distribution in supported) {
    testthat::expect_true(
      exists(sprintf("p_%s_internal", distribution), mode = "function"),
      info = paste("distribution:", distribution)
    )
    
    testthat::expect_true(
      exists(sprintf("q_%s_internal", distribution), mode = "function"),
      info = paste("distribution:", distribution)
    )
    
    testthat::expect_true(
      exists(sprintf("quantile_%s_internal", distribution), mode = "function"),
      info = paste("distribution:", distribution)
    )
  }
})

testthat::test_that("GUM quantiles match the closed-form Gumbel benchmark", {
  para <- c(xi = 10, alpha = 2)
  return_period <- c(2, 5, 10, 25, 50, 100)
  
  expected <- para[["xi"]] - para[["alpha"]] *
    log(-log(1 - 1 / return_period))
  
  actual <- phase09_quantile(
    distribution = "gum",
    return_period = return_period,
    para = para
  )
  
  testthat::expect_type(actual, "double")
  testthat::expect_length(actual, length(return_period))
  testthat::expect_equal(as.numeric(actual), expected, tolerance = 1e-10)
})

testthat::test_that("Quantile wrappers use the quantile probability convention consistently", {
  parameters <- phase09_fixed_parameters()
  return_period <- c(2, 5, 10, 25, 50, 100)
  
  for (distribution in names(parameters)) {
    benchmark_probability <- 1 - 1 / return_period
    
    q_expected <- phase09_q(
      distribution = distribution,
      p = benchmark_probability,
      para = parameters[[distribution]]
    )
    
    rl_actual <- phase09_quantile(
      distribution = distribution,
      return_period = return_period,
      para = parameters[[distribution]]
    )
    
    testthat::expect_length(
      rl_actual,
      length(return_period)
    )
    
    testthat::expect_true(
      all(is.finite(rl_actual)),
      info = paste("distribution:", distribution)
    )
    
    testthat::expect_equal(
      as.numeric(rl_actual),
      as.numeric(q_expected),
      tolerance = 1e-8,
      info = paste("distribution:", distribution)
    )
  }
})

testthat::test_that("CDF and quantile wrappers invert each other for fixed benchmark parameters", {
  parameters <- phase09_fixed_parameters()
  probabilities <- c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)
  
  for (distribution in names(parameters)) {
    q <- phase09_q(
      distribution = distribution,
      p = probabilities,
      para = parameters[[distribution]]
    )
    
    p_back <- phase09_p(
      distribution = distribution,
      q = q,
      para = parameters[[distribution]]
    )
    
    testthat::expect_length(
      q,
      length(probabilities)
    )
    
    testthat::expect_true(
      all(is.finite(q)),
      info = paste("distribution:", distribution)
    )
    
    testthat::expect_equal(
      as.numeric(p_back),
      probabilities,
      tolerance = 1e-7,
      info = paste("distribution:", distribution)
    )
  }
})

testthat::test_that("LN2 wrapper agrees with the LN3 zero-threshold convention", {
  probabilities <- c(0.10, 0.25, 0.50, 0.75, 0.90)
  
  para_ln2 <- c(mu = 4.50, sigma = 0.25)
  
  para_ln3_zero_threshold <- c(
    zeta = 0,
    mu = para_ln2[["mu"]],
    sigma = para_ln2[["sigma"]]
  )
  
  expected <- phase09_q(
    distribution = "ln3",
    p = probabilities,
    para = para_ln3_zero_threshold
  )
  
  actual <- phase09_q(
    distribution = "ln2",
    p = probabilities,
    para = para_ln2
  )
  
  testthat::expect_equal(
    as.numeric(actual),
    as.numeric(expected),
    tolerance = 1e-8
  )
})

testthat::test_that("Fixed benchmark parameters produce monotone quantiles", {
  parameters <- phase09_fixed_parameters()
  return_period <- c(2, 5, 10, 25, 50, 100)
  
  for (distribution in names(parameters)) {
    rl <- phase09_quantile(
      distribution = distribution,
      return_period = return_period,
      para = parameters[[distribution]]
    )
    
    testthat::expect_true(
      all(diff(as.numeric(rl)) > 0),
      info = paste("distribution:", distribution)
    )
  }
})

testthat::test_that("Selected wrappers agree with direct lmom reference quantiles where available", {
  testthat::skip_if_not_installed("lmom")
  
  probabilities <- c(0.10, 0.25, 0.50, 0.75, 0.90)
  
  reference_cases <- list(
    gev = list(
      para = c(xi = 100, alpha = 20, k = 0.10),
      fun = lmom::quagev
    ),
    gpa = list(
      para = c(xi = 0, alpha = 15, k = -0.10),
      fun = lmom::quagpa
    ),
    gum = list(
      para = c(xi = 100, alpha = 20),
      fun = lmom::quagum
    )
  )
  
  for (distribution in names(reference_cases)) {
    case <- reference_cases[[distribution]]
    
    expected <- case$fun(
      probabilities,
      para = unname(case$para)
    )
    
    actual <- phase09_q(
      distribution = distribution,
      p = probabilities,
      para = case$para
    )
    
    testthat::expect_equal(
      as.numeric(actual),
      as.numeric(expected),
      tolerance = 1e-8,
      info = paste("distribution:", distribution)
    )
  }
})
