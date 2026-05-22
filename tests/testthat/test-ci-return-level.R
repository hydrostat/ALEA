test_that("confint.alea_fit returns bootstrap return-level CI structure", {
  x <- c(
    18.2, 21.5, 24.1, 26.8, 29.4, 33.7, 37.9, 42.6,
    48.3, 55.1, 63.8, 74.5, 88.9, 106.4, 128.7,
    156.2, 191.5, 238.9, 302.4, 391.8
  )
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  
  ci <- confint(
    fit,
    return_period = c(10, 50),
    level = 0.95,
    method = "bootstrap",
    n_boot = 20,
    seed = 123
  )
  
  expect_s3_class(ci, "alea_return_level_ci")
  expect_s3_class(ci, "data.frame")
  
  expect_true(all(c(
    "distribution", "method", "return_period", "probability",
    "return_level", "conf_level", "conf_method", "lower", "upper",
    "n_boot", "n_success", "n_failed"
  ) %in% names(ci)))
  
  expect_equal(nrow(ci), 2)
  expect_equal(ci$distribution, rep("gum", 2))
  expect_equal(ci$method, rep("lmom", 2))
  expect_equal(ci$conf_level, rep(0.95, 2))
  expect_equal(ci$conf_method, rep("bootstrap", 2))
  expect_equal(ci$n_boot, rep(20, 2))
  expect_true(all(ci$n_success > 0))
  expect_true(all(ci$n_failed >= 0))
  expect_true(all(is.finite(ci$lower)))
  expect_true(all(is.finite(ci$upper)))
  expect_true(all(ci$lower <= ci$upper))
})


test_that("bootstrap confidence intervals are reproducible with seed", {
  x <- c(
    18.2, 21.5, 24.1, 26.8, 29.4, 33.7, 37.9, 42.6,
    48.3, 55.1, 63.8, 74.5, 88.9, 106.4, 128.7,
    156.2, 191.5, 238.9, 302.4, 391.8
  )
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  
  ci1 <- confint(
    fit,
    return_period = c(10, 50),
    level = 0.95,
    n_boot = 30,
    seed = 123
  )
  
  ci2 <- confint(
    fit,
    return_period = c(10, 50),
    level = 0.95,
    n_boot = 30,
    seed = 123
  )
  
  expect_equal(ci1$lower, ci2$lower)
  expect_equal(ci1$upper, ci2$upper)
  expect_equal(ci1$n_success, ci2$n_success)
  expect_equal(ci1$n_failed, ci2$n_failed)
})


test_that("confint.alea_fit validates confidence level", {
  x <- c(
    18.2, 21.5, 24.1, 26.8, 29.4, 33.7, 37.9, 42.6,
    48.3, 55.1, 63.8, 74.5, 88.9, 106.4, 128.7,
    156.2, 191.5, 238.9, 302.4, 391.8
  )
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  
  expect_error(
    confint(fit, return_period = 10, level = 1, n_boot = 10),
    "`level` must be greater than 0 and less than 1",
    fixed = TRUE
  )
  
  expect_error(
    confint(fit, return_period = 10, level = 0, n_boot = 10),
    "`level` must be greater than 0 and less than 1",
    fixed = TRUE
  )
  
  expect_error(
    confint(fit, return_period = 10, level = NA_real_, n_boot = 10),
    "`level` must be a single finite number",
    fixed = TRUE
  )
})


test_that("confint.alea_fit validates n_boot", {
  x <- c(
    18.2, 21.5, 24.1, 26.8, 29.4, 33.7, 37.9, 42.6,
    48.3, 55.1, 63.8, 74.5, 88.9, 106.4, 128.7,
    156.2, 191.5, 238.9, 302.4, 391.8
  )
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  
  expect_error(
    confint(fit, return_period = 10, n_boot = 0),
    "`n_boot` must be greater than or equal to 1",
    fixed = TRUE
  )
  
  expect_error(
    confint(fit, return_period = 10, n_boot = 10.5),
    "`n_boot` must be an integer",
    fixed = TRUE
  )
  
  expect_error(
    confint(fit, return_period = 10, n_boot = NA_real_),
    "`n_boot` must be a single finite number",
    fixed = TRUE
  )
})


test_that("confint.alea_fit rejects unsupported parm", {
  x <- c(
    18.2, 21.5, 24.1, 26.8, 29.4, 33.7, 37.9, 42.6,
    48.3, 55.1, 63.8, 74.5, 88.9, 106.4, 128.7,
    156.2, 191.5, 238.9, 302.4, 391.8
  )
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  
  expect_error(
    confint(fit, parm = "parameters", return_period = 10, n_boot = 10),
    "Only parm = 'return_level' is currently supported.",
    fixed = TRUE
  )
})


test_that("confint.alea_fit rejects unsupported CI methods", {
  x <- c(
    18.2, 21.5, 24.1, 26.8, 29.4, 33.7, 37.9, 42.6,
    48.3, 55.1, 63.8, 74.5, 88.9, 106.4, 128.7,
    156.2, 191.5, 238.9, 302.4, 391.8
  )
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  
  expect_error(
    confint(fit, return_period = 10, method = "delta", n_boot = 10)
  )
  
  expect_error(
    confint(fit, return_period = 10, method = "asymptotic", n_boot = 10)
  )
})


test_that("confint.alea_fit works for all distributions and methods with small bootstrap", {
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
      
      ci <- suppressWarnings(confint(
        fit,
        return_period = c(10, 25),
        level = 0.90,
        method = "bootstrap",
        n_boot = 10,
        seed = 123
      ))
      
      expect_s3_class(ci, "alea_return_level_ci")
      expect_equal(nrow(ci), 2)
      expect_true(all(ci$n_boot == 10))
      expect_true(all(ci$n_success >= 1))
      expect_true(all(ci$n_failed >= 0))
      expect_true(all(is.finite(ci$lower)))
      expect_true(all(is.finite(ci$upper)))
    }
  }
})


test_that("print.alea_return_level_ci prints expected header", {
  x <- c(
    18.2, 21.5, 24.1, 26.8, 29.4, 33.7, 37.9, 42.6,
    48.3, 55.1, 63.8, 74.5, 88.9, 106.4, 128.7,
    156.2, 191.5, 238.9, 302.4, 391.8
  )
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  
  ci <- confint(
    fit,
    return_period = c(10, 50),
    n_boot = 10,
    seed = 123
  )
  
  expect_output(
    print(ci),
    "ALEA return-level confidence intervals"
  )
})

test_that("as.data.frame.alea_return_level_ci drops custom class", {
  x <- c(
    18.2, 21.5, 24.1, 26.8, 29.4, 33.7, 37.9, 42.6,
    48.3, 55.1, 63.8, 74.5, 88.9, 106.4, 128.7,
    156.2, 191.5, 238.9, 302.4, 391.8
  )
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  
  ci <- confint(
    fit,
    return_period = c(10, 50),
    n_boot = 10,
    seed = 123
  )
  
  df <- as.data.frame(ci)
  
  expect_s3_class(df, "data.frame")
  expect_false(inherits(df, "alea_return_level_ci"))
})