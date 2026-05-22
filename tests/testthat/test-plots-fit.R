test_that("plot.alea_fit returns ggplot objects for all supported plot types", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- stats::rnorm(60, mean = 100, sd = 15)
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  
  plot_types <- c("density", "cdf", "qq", "pp", "return_level")
  
  for (plot_type in plot_types) {
    p <- plot(fit, type = plot_type)
    expect_s3_class(p, "ggplot")
  }
})


test_that("plot.alea_fit supports all distributions with density plots", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- exp(stats::rnorm(80, mean = 4.5, sd = 0.25))
  
  distributions <- c("gev", "gpa", "pe3", "ln2", "ln3", "gum")
  
  for (distribution in distributions) {
    fit <- alea_fit(x, distribution = distribution, method = "lmom")
    p <- plot(fit, type = "density")
    
    expect_s3_class(p, "ggplot")
  }
})


test_that("plot.alea_fit supports all estimation methods with Q-Q plots", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- stats::rgamma(80, shape = 8, scale = 10)
  
  methods <- c("lmom", "mom", "mle")
  
  for (method in methods) {
    fit <- alea_fit(x, distribution = "gum", method = method)
    p <- plot(fit, type = "qq")
    
    expect_s3_class(p, "ggplot")
  }
})


test_that("plot.alea_fit validates plot type and plotting arguments", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- stats::rnorm(60, mean = 100, sd = 15)
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  
  expect_error(
    plot(fit, type = "unsupported"),
    "should be one of"
  )
  
  expect_error(
    plot(fit, type = "density", n_grid = 10),
    "`n_grid` must be at least 20"
  )
  
  expect_error(
    plot(fit, type = "density", bins = 0),
    "`bins` must be positive"
  )
  
  expect_error(
    plot(fit, type = "return_level", return_period = c(1, 10)),
    "greater than 1"
  )
})


test_that("plot.alea_fit rejects non-plottable fitted data", {
  skip_if_not_installed("ggplot2")
  
  fit <- alea_fit(stats::rnorm(30), distribution = "gum", method = "lmom")
  
  fit_one_value <- fit
  fit_one_value$data <- c(1, NA, Inf)
  
  expect_error(
    plot(fit_one_value, type = "density"),
    "At least two finite observations"
  )
  
  fit_constant <- fit
  fit_constant$data <- rep(1, 10)
  
  expect_error(
    plot(fit_constant, type = "density"),
    "At least two unique finite observations"
  )
})

test_that("plot.alea_fit return-level plot supports automatic ticks and scales", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- stats::rnorm(60, mean = 100, sd = 15)
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  
  p_default <- plot(fit, type = "return_level")
  p_na <- plot(fit, type = "return_level", return_period = NA_real_)
  p_log <- plot(fit, type = "return_level", return_period_scale = "log")
  p_linear <- plot(fit, type = "return_level", return_period_scale = "linear")
  
  expect_s3_class(p_default, "ggplot")
  expect_s3_class(p_na, "ggplot")
  expect_s3_class(p_log, "ggplot")
  expect_s3_class(p_linear, "ggplot")
  expect_identical(p_default$labels$x, "Return period")
  expect_identical(p_default$labels$y, "Quantile")
})


test_that("plot.alea_fit return-level plot validates return-period plotting arguments", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- stats::rnorm(60, mean = 100, sd = 15)
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  
  expect_error(
    plot(fit, type = "return_level", return_period_scale = "unsupported"),
    "should be one of"
  )
  
  expect_error(
    plot(fit, type = "return_level", return_period_grid_n = 10),
    "`return_period_grid_n` must be at least 20"
  )
  
  expect_error(
    plot(fit, type = "return_level", plotting_position_a = -0.1),
    "`plotting_position_a` must be greater than or equal to 0 and less than 1"
  )
  
  expect_error(
    plot(fit, type = "return_level", plotting_position_a = 1),
    "`plotting_position_a` must be greater than or equal to 0 and less than 1"
  )
})


test_that("plot.alea_fit warns when requested return periods hide observed plotting positions", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- stats::rnorm(60, mean = 100, sd = 15)
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  
  expect_warning(
    plot(
      fit,
      type = "return_level",
      return_period = c(2, 5),
      return_period_scale = "gumbel"
    ),
    "Some observed plotting positions"
  )
})
