test_that("alea_return_level returns expected structure", {
  x <- c(32, 41, 38, 45, 51, 48, 57, 62, 59, 66, 70, 74)
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  rl <- alea_return_level(fit, return_period = c(10, 50, 100))
  
  expect_s3_class(rl, "alea_return_level")
  expect_s3_class(rl, "data.frame")
  
  expect_named(
    rl,
    c("distribution", "method", "return_period", "probability",
      "return_level", "xi", "alpha"),
    ignore.order = FALSE
  )
  
  expect_equal(nrow(rl), 3)
  expect_equal(rl$distribution, rep("gum", 3))
  expect_equal(rl$method, rep("lmom", 3))
  expect_equal(rl$return_period, c(10, 50, 100))
  expect_equal(rl$probability, 1 - 1 / c(10, 50, 100))
  expect_true(all(is.finite(rl$return_level)))
})


test_that("alea_return_level matches internal quantile functions", {
  x <- c(
    18.2, 21.5, 24.1, 26.8, 29.4, 33.7, 37.9, 42.6,
    48.3, 55.1, 63.8, 74.5, 88.9, 106.4, 128.7,
    156.2, 191.5, 238.9, 302.4, 391.8
  )
  return_period <- c(2, 10, 25)
  probability <- 1 - 1 / return_period
  
  distributions <- c("gev", "gpa", "pe3", "ln2", "ln3", "gum")
  methods <- c("lmom", "mom", "mle")
  
  for (distribution in distributions) {
    for (method in methods) {
      fit <- alea_fit(x, distribution = distribution, method = method)
      rl <- alea_return_level(fit, return_period = return_period)
      
      parameters <- coef(fit)
      
      expected <- switch(
        distribution,
        gev = return_level_gev_internal(return_period, parameters),
        gpa = return_level_gpa_internal(return_period, parameters),
        pe3 = return_level_pe3_internal(return_period, parameters),
        ln2 = return_level_ln2_internal(return_period, parameters),
        ln3 = return_level_ln3_internal(return_period, parameters),
        gum = return_level_gum_internal(return_period, parameters)
      )
      
      expect_equal(rl$return_level, as.numeric(expected), tolerance = 1e-10)
    }
  }
})


test_that("alea_return_level validates return_period", {
  x <- c(32, 41, 38, 45, 51, 48, 57, 62, 59, 66, 70, 74)
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  
  expect_error(
    alea_return_level(fit, return_period = 1),
    "`return_period` must contain finite values greater than 1",
    fixed = TRUE
  )
  
  expect_error(
    alea_return_level(fit, return_period = 0),
    "`return_period` must contain finite values greater than 1",
    fixed = TRUE
  )
  
  expect_error(
    alea_return_level(fit, return_period = -10),
    "`return_period` must contain finite values greater than 1",
    fixed = TRUE
  )
  
  expect_error(
    alea_return_level(fit, return_period = NA_real_),
    "`return_period` must contain finite values greater than 1",
    fixed = TRUE
  )
  
  expect_error(
    alea_return_level(fit, return_period = Inf),
    "`return_period` must contain finite values greater than 1",
    fixed = TRUE
  )
  
  expect_error(
    alea_return_level(fit, return_period = "100"),
    "`return_period` must be numeric",
    fixed = TRUE
  )
})

test_that("alea_return_level works for all distributions and methods", {
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
      rl <- alea_return_level(fit, return_period = c(10, 25, 50, 100))
      
      expect_s3_class(rl, "alea_return_level")
      expect_equal(nrow(rl), 4)
      expect_true(all(is.finite(rl$return_level)))
      expect_true(all(rl$return_period > 1))
      expect_true(all(rl$probability > 0 & rl$probability < 1))
    }
  }
})


test_that("print.alea_return_level returns invisibly", {
  x <- c(32, 41, 38, 45, 51, 48, 57, 62, 59, 66, 70, 74)
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  rl <- alea_return_level(fit, return_period = c(10, 50))
  
  expect_output(
    out <- print(rl),
    "ALEA return levels"
  )
  
  expect_identical(out, rl)
})


test_that("as.data.frame.alea_return_level drops custom class", {
  x <- c(32, 41, 38, 45, 51, 48, 57, 62, 59, 66, 70, 74)
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  rl <- alea_return_level(fit, return_period = c(10, 50))
  
  df <- as.data.frame(rl)
  
  expect_s3_class(df, "data.frame")
  expect_false(inherits(df, "alea_return_level"))
})