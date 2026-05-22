ggplot_label <- function(p, name) {
  labels <- p$labels
  
  if (!is.null(labels[[name]])) {
    return(labels[[name]])
  }
  
  if (identical(name, "color") && !is.null(labels[["colour"]])) {
    return(labels[["colour"]])
  }
  
  if (identical(name, "colour") && !is.null(labels[["color"]])) {
    return(labels[["color"]])
  }
  
  NULL
}

test_that("plot.alea_return_level returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- stats::rnorm(60, mean = 100, sd = 15)
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  rl <- alea_return_level(fit, return_period = c(2, 5, 10, 25, 50, 100))
  
  p <- plot(rl)
  
  expect_s3_class(p, "ggplot")
})


test_that("plot.alea_return_level_ci returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- stats::rnorm(60, mean = 100, sd = 15)
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  
  ci <- confint(
    fit,
    parm = "return_level",
    return_period = c(2, 5, 10, 25),
    n_boot = 20,
    seed = 123
  )
  
  p <- plot(ci)
  
  expect_s3_class(p, "ggplot")
})


test_that("plot.alea_return_level orders return periods before plotting", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- stats::rnorm(60, mean = 100, sd = 15)
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  rl <- alea_return_level(fit, return_period = c(50, 2, 25, 10, 5))
  
  p <- plot(rl)
  
  expect_s3_class(p, "ggplot")
  
  plot_data <- p$data
  expect_equal(plot_data$return_period, sort(plot_data$return_period))
})


test_that("plot.alea_return_level validates required columns", {
  skip_if_not_installed("ggplot2")
  
  bad <- data.frame(
    distribution = "gum",
    method = "lmom",
    return_period = c(2, 5, 10)
  )
  
  class(bad) <- c("alea_return_level", "data.frame")
  
  expect_error(
    plot(bad),
    "missing required column"
  )
})


test_that("plot.alea_return_level validates numeric return-level columns", {
  skip_if_not_installed("ggplot2")
  
  bad <- data.frame(
    distribution = "gum",
    method = "lmom",
    return_period = c(2, 5, 10),
    return_level = c("a", "b", "c")
  )
  
  class(bad) <- c("alea_return_level", "data.frame")
  
  expect_error(
    plot(bad),
    "`return_period` and `return_level` columns must be numeric"
  )
})


test_that("plot.alea_return_level validates return periods", {
  skip_if_not_installed("ggplot2")
  
  bad <- data.frame(
    distribution = "gum",
    method = "lmom",
    return_period = c(1, 5, 10),
    return_level = c(100, 120, 140)
  )
  
  class(bad) <- c("alea_return_level", "data.frame")
  
  expect_error(
    plot(bad),
    "greater than 1"
  )
})


test_that("plot.alea_return_level_ci validates confidence interval columns", {
  skip_if_not_installed("ggplot2")
  
  bad <- data.frame(
    distribution = "gum",
    method = "lmom",
    return_period = c(2, 5, 10),
    return_level = c(100, 120, 140),
    lower = c(90, 110, 130),
    upper = c(110, 130, 150)
  )
  
  class(bad) <- c("alea_return_level_ci", "data.frame")
  
  expect_error(
    plot(bad),
    "missing required confidence-interval column"
  )
})


test_that("plot.alea_return_level_ci validates confidence interval limits", {
  skip_if_not_installed("ggplot2")
  
  bad <- data.frame(
    distribution = "gum",
    method = "lmom",
    return_period = c(2, 5, 10),
    return_level = c(100, 120, 140),
    lower = c(90, 140, 130),
    upper = c(110, 130, 150),
    conf_level = c(0.95, 0.95, 0.95)
  )
  
  class(bad) <- c("alea_return_level_ci", "data.frame")
  
  expect_error(
    plot(bad),
    "lower limits must not exceed upper limits"
  )
})


test_that("plot.alea_return_level_ci supports multiple confidence levels", {
  skip_if_not_installed("ggplot2")
  
  ci <- data.frame(
    distribution = "gum",
    method = "lmom",
    return_period = c(2, 5, 10),
    probability = c(0.5, 0.8, 0.9),
    return_level = c(100, 120, 140),
    conf_level = c(0.90, 0.95, 0.99),
    conf_method = "bootstrap",
    lower = c(90, 110, 130),
    upper = c(110, 130, 150),
    n_boot = 100,
    n_success = 100,
    n_failed = 0
  )
  
  class(ci) <- c("alea_return_level_ci", "data.frame")
  
  p <- plot(ci)
  
  expect_s3_class(p, "ggplot")
})

test_that("plot.alea_return_level supports return-period axis scales", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- stats::rnorm(60, mean = 100, sd = 15)
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  rl <- alea_return_level(fit, return_period = c(2, 5, 10, 25, 50, 100))
  
  p_gumbel <- plot(rl, return_period_scale = "gumbel")
  p_log <- plot(rl, return_period_scale = "log")
  p_linear <- plot(rl, return_period_scale = "linear")
  
  expect_s3_class(p_gumbel, "ggplot")
  expect_s3_class(p_log, "ggplot")
  expect_s3_class(p_linear, "ggplot")
  expect_identical(p_gumbel$labels$x, "Return period")
  expect_identical(p_gumbel$labels$y, "Quantile")
  expect_error(plot(rl, return_period_scale = "unsupported"), "should be one of")
})


test_that("plot.alea_return_level supports multiple models without mixing groups", {
  skip_if_not_installed("ggplot2")
  
  rl <- data.frame(
    distribution = rep(c("gum", "gev"), each = 3),
    method = "lmom",
    return_period = rep(c(2, 5, 10), times = 2),
    probability = rep(c(0.5, 0.8, 0.9), times = 2),
    return_level = c(100, 120, 140, 105, 130, 160)
  )
  class(rl) <- c("alea_return_level", "data.frame")
  
  p <- plot(rl, return_period_scale = "gumbel")
  
  expect_s3_class(p, "ggplot")
  expect_true("model_label" %in% names(p$data))
  expect_equal(length(unique(p$data$model_label)), 2L)
  expect_identical(ggplot_label(p, "color"), "Model")
})


test_that("plot.alea_return_level_ci supports multiple models without connecting intervals", {
  skip_if_not_installed("ggplot2")
  
  ci <- data.frame(
    distribution = rep(c("gum", "gev"), each = 3),
    method = "lmom",
    return_period = rep(c(2, 5, 10), times = 2),
    probability = rep(c(0.5, 0.8, 0.9), times = 2),
    return_level = c(100, 120, 140, 105, 130, 160),
    conf_level = 0.95,
    conf_method = "bootstrap",
    lower = c(90, 110, 130, 95, 120, 150),
    upper = c(110, 130, 150, 115, 140, 170),
    n_boot = 100,
    n_success = 100,
    n_failed = 0
  )
  class(ci) <- c("alea_return_level_ci", "data.frame")
  
  p <- plot(ci, return_period_scale = "gumbel")
  
  expect_s3_class(p, "ggplot")
  expect_true("model_label" %in% names(p$data))
  expect_equal(length(unique(p$data$model_label)), 2L)
  expect_identical(ggplot_label(p, "color"), "Model")
  expect_identical(p$labels$fill, "Model")
  expect_identical(p$labels$y, "Quantile")
})
