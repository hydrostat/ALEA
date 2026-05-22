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

test_that("plot.alea_gof returns a ggplot object for statistic plot", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- stats::rnorm(60, mean = 100, sd = 15)
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  gof <- alea_gof(fit)
  
  p <- plot(gof, type = "statistic")
  
  expect_s3_class(p, "ggplot")
})


test_that("plot.alea_gof returns a ggplot object for rank plot", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- stats::rnorm(60, mean = 100, sd = 15)
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  gof <- alea_gof(fit)
  
  p <- plot(gof, type = "rank")
  
  expect_s3_class(p, "ggplot")
})


test_that("plot.alea_gof supports all distributions", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- exp(stats::rnorm(80, mean = 4.5, sd = 0.25))
  
  distributions <- c("gev", "gpa", "pe3", "ln2", "ln3", "gum")
  
  for (distribution in distributions) {
    fit <- alea_fit(x, distribution = distribution, method = "lmom")
    gof <- alea_gof(fit)
    
    p <- plot(gof, type = "statistic")
    
    expect_s3_class(p, "ggplot")
  }
})


test_that("plot.alea_gof supports all estimation methods", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- stats::rgamma(80, shape = 8, scale = 10)
  
  methods <- c("lmom", "mom", "mle")
  
  for (method in methods) {
    fit <- alea_fit(x, distribution = "gum", method = method)
    gof <- alea_gof(fit)
    
    p <- plot(gof, type = "statistic")
    
    expect_s3_class(p, "ggplot")
  }
})


test_that("plot.alea_gof rejects unsupported plot type", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- stats::rnorm(60, mean = 100, sd = 15)
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  gof <- alea_gof(fit)
  
  expect_error(
    plot(gof, type = "unsupported"),
    "should be one of"
  )
})


test_that("plot.alea_gof validates required columns", {
  skip_if_not_installed("ggplot2")
  
  bad <- data.frame(
    distribution = "gum",
    method = "lmom",
    statistic = "aic",
    estimate = 100,
    higher_is_better = FALSE
  )
  
  class(bad) <- c("alea_gof", "data.frame")
  
  expect_error(
    plot(bad),
    "missing required column"
  )
})


test_that("plot.alea_gof validates statistic column type", {
  skip_if_not_installed("ggplot2")
  
  bad <- data.frame(
    distribution = "gum",
    method = "lmom",
    statistic = 1,
    estimate = 100,
    higher_is_better = FALSE,
    description = "AIC"
  )
  
  class(bad) <- c("alea_gof", "data.frame")
  
  expect_error(
    plot(bad),
    "`statistic` column must be character"
  )
})


test_that("plot.alea_gof validates estimate column type", {
  skip_if_not_installed("ggplot2")
  
  bad <- data.frame(
    distribution = "gum",
    method = "lmom",
    statistic = "aic",
    estimate = "100",
    higher_is_better = FALSE,
    description = "AIC"
  )
  
  class(bad) <- c("alea_gof", "data.frame")
  
  expect_error(
    plot(bad),
    "`estimate` column must be numeric"
  )
})


test_that("plot.alea_gof validates higher_is_better column type", {
  skip_if_not_installed("ggplot2")
  
  bad <- data.frame(
    distribution = "gum",
    method = "lmom",
    statistic = "aic",
    estimate = 100,
    higher_is_better = "FALSE",
    description = "AIC"
  )
  
  class(bad) <- c("alea_gof", "data.frame")
  
  expect_error(
    plot(bad),
    "`higher_is_better` column must be logical"
  )
})


test_that("plot.alea_gof rejects all non-finite estimates", {
  skip_if_not_installed("ggplot2")
  
  bad <- data.frame(
    distribution = "gum",
    method = "lmom",
    statistic = c("aic", "bic"),
    estimate = c(NA_real_, Inf),
    higher_is_better = c(FALSE, FALSE),
    description = c("AIC", "BIC")
  )
  
  class(bad) <- c("alea_gof", "data.frame")
  
  expect_error(
    plot(bad),
    "At least one finite GOF estimate"
  )
})


test_that("plot.alea_gof drops non-finite estimates when finite estimates are available", {
  skip_if_not_installed("ggplot2")
  
  gof <- data.frame(
    distribution = "gum",
    method = "lmom",
    statistic = c("ks", "aic", "bic"),
    estimate = c(0.10, NA_real_, 300),
    higher_is_better = c(FALSE, FALSE, FALSE),
    description = c("KS", "AIC", "BIC")
  )
  
  class(gof) <- c("alea_gof", "data.frame")
  
  p <- plot(gof, type = "statistic")
  
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 2L)
})


test_that("plot.alea_gof rank plot ranks lower and higher better statistics correctly", {
  skip_if_not_installed("ggplot2")
  
  gof <- data.frame(
    distribution = c("gum", "gev", "gum", "gev"),
    method = c("lmom", "lmom", "lmom", "lmom"),
    statistic = c("aic", "aic", "loglik", "loglik"),
    estimate = c(100, 90, -50, -40),
    higher_is_better = c(FALSE, FALSE, TRUE, TRUE),
    description = c("AIC", "AIC", "log-likelihood", "log-likelihood")
  )
  
  class(gof) <- c("alea_gof", "data.frame")
  
  p <- plot(gof, type = "rank")
  
  expect_s3_class(p, "ggplot")
  
  rank_data <- p$data
  
  aic_rows <- rank_data[rank_data$statistic == "aic", , drop = FALSE]
  loglik_rows <- rank_data[rank_data$statistic == "loglik", , drop = FALSE]
  
  expect_equal(
    aic_rows$distribution[aic_rows$rank_value == 1],
    "gev"
  )
  
  expect_equal(
    loglik_rows$distribution[loglik_rows$rank_value == 1],
    "gev"
  )
})

test_that("plot.alea_gof statistic plot facets statistics with free y scales", {
  skip_if_not_installed("ggplot2")
  
  gof <- data.frame(
    distribution = rep(c("gum", "gev"), times = 2),
    method = "lmom",
    statistic = rep(c("ks", "aic"), each = 2),
    estimate = c(0.1, 0.08, 100, 90),
    higher_is_better = c(FALSE, FALSE, FALSE, FALSE),
    description = c("KS", "KS", "AIC", "AIC")
  )
  class(gof) <- c("alea_gof", "data.frame")
  
  p <- plot(gof, type = "statistic")
  
  expect_s3_class(p, "ggplot")
  expect_true("model_label" %in% names(p$data))
  expect_true(inherits(p$facet, "FacetWrap"))
})


test_that("plot.alea_gof rank plot exposes distribution labels for the legend", {
  skip_if_not_installed("ggplot2")
  
  gof <- data.frame(
    distribution = c("gum", "gev", "gum", "gev"),
    method = c("lmom", "lmom", "lmom", "lmom"),
    statistic = c("aic", "aic", "loglik", "loglik"),
    estimate = c(100, 90, -50, -40),
    higher_is_better = c(FALSE, FALSE, TRUE, TRUE),
    description = c("AIC", "AIC", "log-likelihood", "log-likelihood")
  )
  class(gof) <- c("alea_gof", "data.frame")
  
  p <- plot(gof, type = "rank")
  
  expect_s3_class(p, "ggplot")
  expect_true("distribution_label" %in% names(p$data))
  expect_identical(ggplot_label(p, "color"), "Distribution")
})
