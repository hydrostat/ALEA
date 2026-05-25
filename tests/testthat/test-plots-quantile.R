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

test_that("plot.alea_quantile returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- stats::rnorm(60, mean = 100, sd = 15)
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  rl <- alea_quantile(fit, return_period = c(2, 5, 10, 25, 50, 100))
  
  p <- plot(rl)
  
  expect_s3_class(p, "ggplot")
})


test_that("plot.alea_quantile_ci returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- stats::rnorm(60, mean = 100, sd = 15)
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  
  ci <- confint(
    fit,
    parm = "quantile",
    return_period = c(2, 5, 10, 25),
    n_boot = 20,
    seed = 123
  )
  
  p <- plot(ci)
  
  expect_s3_class(p, "ggplot")
})


test_that("plot.alea_quantile orders return periods before plotting", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- stats::rnorm(60, mean = 100, sd = 15)
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  rl <- alea_quantile(fit, return_period = c(50, 2, 25, 10, 5))
  
  p <- plot(rl)
  
  expect_s3_class(p, "ggplot")
  
  plot_data <- p$data
  expect_equal(plot_data$return_period, sort(plot_data$return_period))
})


test_that("plot.alea_quantile validates required columns", {
  skip_if_not_installed("ggplot2")
  
  bad <- data.frame(
    distribution = "gum",
    method = "lmom",
    return_period = c(2, 5, 10)
  )
  
  class(bad) <- c("alea_quantile", "data.frame")
  
  expect_error(
    plot(bad),
    "missing required column"
  )
})


test_that("plot.alea_quantile validates numeric quantile columns", {
  skip_if_not_installed("ggplot2")
  
  bad <- data.frame(
    distribution = "gum",
    method = "lmom",
    return_period = c(2, 5, 10),
    quantile = c("a", "b", "c")
  )
  
  class(bad) <- c("alea_quantile", "data.frame")
  
  expect_error(
    plot(bad),
    "`return_period` and `quantile` columns must be numeric"
  )
})


test_that("plot.alea_quantile validates return periods", {
  skip_if_not_installed("ggplot2")
  
  bad <- data.frame(
    distribution = "gum",
    method = "lmom",
    return_period = c(1, 5, 10),
    quantile = c(100, 120, 140)
  )
  
  class(bad) <- c("alea_quantile", "data.frame")
  
  expect_error(
    plot(bad),
    "greater than 1"
  )
})


test_that("plot.alea_quantile_ci validates confidence interval columns", {
  skip_if_not_installed("ggplot2")
  
  bad <- data.frame(
    distribution = "gum",
    method = "lmom",
    return_period = c(2, 5, 10),
    quantile = c(100, 120, 140),
    lower = c(90, 110, 130),
    upper = c(110, 130, 150)
  )
  
  class(bad) <- c("alea_quantile_ci", "data.frame")
  
  expect_error(
    plot(bad),
    "missing required confidence-interval column"
  )
})


test_that("plot.alea_quantile_ci validates confidence interval limits", {
  skip_if_not_installed("ggplot2")
  
  bad <- data.frame(
    distribution = "gum",
    method = "lmom",
    return_period = c(2, 5, 10),
    quantile = c(100, 120, 140),
    lower = c(90, 140, 130),
    upper = c(110, 130, 150),
    conf_level = c(0.95, 0.95, 0.95)
  )
  
  class(bad) <- c("alea_quantile_ci", "data.frame")
  
  expect_error(
    plot(bad),
    "lower limits must not exceed upper limits"
  )
})


test_that("plot.alea_quantile_ci supports multiple confidence levels", {
  skip_if_not_installed("ggplot2")
  
  ci <- data.frame(
    distribution = "gum",
    method = "lmom",
    return_period = c(2, 5, 10),
    probability = c(0.5, 0.8, 0.9),
    quantile = c(100, 120, 140),
    conf_level = c(0.90, 0.95, 0.99),
    conf_method = "bootstrap",
    lower = c(90, 110, 130),
    upper = c(110, 130, 150),
    n_boot = 100,
    n_success = 100,
    n_failed = 0
  )
  
  class(ci) <- c("alea_quantile_ci", "data.frame")
  
  p <- plot(ci)
  
  expect_s3_class(p, "ggplot")
})

test_that("plot.alea_quantile supports return-period axis scales", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- stats::rnorm(60, mean = 100, sd = 15)
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  rl <- alea_quantile(fit, return_period = c(2, 5, 10, 25, 50, 100))
  
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


test_that("plot.alea_quantile supports multiple models without mixing groups", {
  skip_if_not_installed("ggplot2")
  
  rl <- data.frame(
    distribution = rep(c("gum", "gev"), each = 3),
    method = "lmom",
    return_period = rep(c(2, 5, 10), times = 2),
    probability = rep(c(0.5, 0.8, 0.9), times = 2),
    quantile = c(100, 120, 140, 105, 130, 160)
  )
  class(rl) <- c("alea_quantile", "data.frame")
  
  p <- plot(rl, return_period_scale = "gumbel")
  
  expect_s3_class(p, "ggplot")
  expect_true("model_label" %in% names(p$data))
  expect_equal(length(unique(p$data$model_label)), 2L)
  expect_identical(ggplot_label(p, "color"), "Model")
})


test_that("plot.alea_quantile_ci supports multiple models without connecting intervals", {
  skip_if_not_installed("ggplot2")
  
  ci <- data.frame(
    distribution = rep(c("gum", "gev"), each = 3),
    method = "lmom",
    return_period = rep(c(2, 5, 10), times = 2),
    probability = rep(c(0.5, 0.8, 0.9), times = 2),
    quantile = c(100, 120, 140, 105, 130, 160),
    conf_level = 0.95,
    conf_method = "bootstrap",
    lower = c(90, 110, 130, 95, 120, 150),
    upper = c(110, 130, 150, 115, 140, 170),
    n_boot = 100,
    n_success = 100,
    n_failed = 0
  )
  class(ci) <- c("alea_quantile_ci", "data.frame")
  
  p <- plot(ci, return_period_scale = "gumbel")
  
  expect_s3_class(p, "ggplot")
  expect_true("model_label" %in% names(p$data))
  expect_equal(length(unique(p$data$model_label)), 2L)
  expect_identical(ggplot_label(p, "color"), "Model")
  expect_identical(p$labels$fill, "Model")
  expect_identical(p$labels$y, "Quantile")
})

has_quantile_ggplot_geom <- function(p, geom_class) {
  any(vapply(p$layers, function(layer) inherits(layer$geom, geom_class), logical(1)))
}

test_that("plot.alea_quantile can include or omit observed points", {
  skip_if_not_installed("ggplot2")

  set.seed(123)
  x <- stats::rnorm(60, mean = 100, sd = 15)
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  q <- alea_quantile(fit, return_period = c(2, 5, 10, 25, 50, 100, 200))

  p_observed <- plot(q, plot_observed = TRUE)
  p_without_observed <- plot(q, plot_observed = FALSE)

  expect_s3_class(p_observed, "ggplot")
  expect_s3_class(p_without_observed, "ggplot")
  expect_true(has_quantile_ggplot_geom(p_observed, "GeomPoint"))
  expect_false(has_quantile_ggplot_geom(p_without_observed, "GeomPoint"))
})

test_that("plot.alea_quantile_ci can include or omit observed points", {
  skip_if_not_installed("ggplot2")

  set.seed(123)
  x <- stats::rnorm(60, mean = 100, sd = 15)
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  ci <- confint(
    fit,
    parm = "quantile",
    return_period = c(2, 5, 10, 25, 50, 100, 200),
    n_boot = 20,
    seed = 123
  )

  p_observed <- plot(ci, plot_observed = TRUE)
  p_without_observed <- plot(ci, plot_observed = FALSE)

  expect_s3_class(p_observed, "ggplot")
  expect_s3_class(p_without_observed, "ggplot")
  expect_true(has_quantile_ggplot_geom(p_observed, "GeomPoint"))
  expect_false(has_quantile_ggplot_geom(p_without_observed, "GeomPoint"))
})

test_that("quantile plots validate observed plotting arguments", {
  skip_if_not_installed("ggplot2")

  set.seed(123)
  x <- stats::rnorm(60, mean = 100, sd = 15)
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  q <- alea_quantile(fit, return_period = c(2, 5, 10, 25))

  expect_error(
    plot(q, plot_observed = NA),
    "`plot_observed` must be `TRUE` or `FALSE`"
  )
  expect_error(
    plot(q, plotting_position_a = -0.1),
    "`plotting_position_a` must be greater than or equal to 0 and less than 1"
  )
})
