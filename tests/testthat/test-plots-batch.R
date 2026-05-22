make_plot_batch_example <- function() {
  set.seed(123)
  
  data.frame(
    station = rep(c("A", "B", "C"), each = 45),
    year = rep(seq_len(45), times = 3),
    value = c(
      stats::rnorm(45, mean = 100, sd = 15),
      stats::rnorm(45, mean = 130, sd = 20),
      stats::rnorm(45, mean = 85, sd = 10)
    )
  )
}


test_that("plot.alea_batch returns a ggplot object for selected models", {
  skip_if_not_installed("ggplot2")
  
  batch <- alea_batch_fit(
    data = make_plot_batch_example(),
    station = "station",
    value = "value",
    time = "year",
    distributions = c("gum", "gev"),
    methods = "lmom",
    select = "ai",
    quiet = TRUE
  )
  
  p <- plot(batch, type = "selected_models")
  
  expect_s3_class(p, "ggplot")
})


test_that("plot.alea_batch returns a ggplot object for return levels", {
  skip_if_not_installed("ggplot2")
  
  batch <- alea_batch_fit(
    data = make_plot_batch_example(),
    station = "station",
    value = "value",
    time = "year",
    distributions = c("gum", "gev"),
    methods = "lmom",
    return_period = c(2, 5, 10),
    quiet = TRUE
  )
  
  p <- plot(batch, type = "return_levels")
  
  expect_s3_class(p, "ggplot")
})


test_that("plot.alea_batch returns a ggplot object for GOF results", {
  skip_if_not_installed("ggplot2")
  
  batch <- alea_batch_fit(
    data = make_plot_batch_example(),
    station = "station",
    value = "value",
    time = "year",
    distributions = c("gum", "gev"),
    methods = "lmom",
    gof = TRUE,
    quiet = TRUE
  )
  
  p <- plot(batch, type = "gof", statistic = "aic")
  
  expect_s3_class(p, "ggplot")
})


test_that("plot.alea_batch returns a ggplot object for diagnostics status summary", {
  skip_if_not_installed("ggplot2")
  
  batch <- alea_batch_fit(
    data = make_plot_batch_example(),
    station = "station",
    value = "value",
    time = "year",
    distributions = "gum",
    methods = "lmom",
    diagnostics = TRUE,
    quiet = TRUE
  )
  
  p <- plot(batch, type = "diagnostics")
  
  expect_s3_class(p, "ggplot")
})


test_that("plot.alea_batch returns a ggplot object for one diagnostic p-value plot", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("trend")
  
  batch <- alea_batch_fit(
    data = make_plot_batch_example(),
    station = "station",
    value = "value",
    time = "year",
    distributions = "gum",
    methods = "lmom",
    diagnostics = TRUE,
    quiet = TRUE
  )
  
  p <- plot(batch, type = "diagnostics", diagnostic = "stationarity")
  
  expect_s3_class(p, "ggplot")
})


test_that("plot.alea_batch rejects unsupported plot type", {
  skip_if_not_installed("ggplot2")
  
  batch <- alea_batch_fit(
    data = make_plot_batch_example(),
    station = "station",
    value = "value",
    distributions = "gum",
    methods = "lmom",
    quiet = TRUE
  )
  
  expect_error(
    plot(batch, type = "unsupported"),
    "should be one of"
  )
})


test_that("plot.alea_batch validates object class", {
  skip_if_not_installed("ggplot2")
  
  bad <- list()
  
  expect_error(
    plot.alea_batch(bad),
    "`x` must be an object of class 'alea_batch'"
  )
})


test_that("plot.alea_batch selected models requires selected_models output", {
  skip_if_not_installed("ggplot2")
  
  batch <- alea_batch_fit(
    data = make_plot_batch_example(),
    station = "station",
    value = "value",
    distributions = "gum",
    methods = "lmom",
    quiet = TRUE
  )
  
  expect_error(
    plot(batch, type = "selected_models"),
    "`x\\$selected_models` is empty"
  )
})


test_that("plot.alea_batch return levels requires return_levels output", {
  skip_if_not_installed("ggplot2")
  
  batch <- alea_batch_fit(
    data = make_plot_batch_example(),
    station = "station",
    value = "value",
    distributions = "gum",
    methods = "lmom",
    quiet = TRUE
  )
  
  expect_error(
    plot(batch, type = "return_levels"),
    "`x\\$return_levels` is empty"
  )
})


test_that("plot.alea_batch GOF requires GOF output", {
  skip_if_not_installed("ggplot2")
  
  batch <- alea_batch_fit(
    data = make_plot_batch_example(),
    station = "station",
    value = "value",
    distributions = "gum",
    methods = "lmom",
    quiet = TRUE
  )
  
  expect_error(
    plot(batch, type = "gof"),
    "`x\\$gof` is empty"
  )
})


test_that("plot.alea_batch diagnostics requires diagnostics output", {
  skip_if_not_installed("ggplot2")
  
  batch <- alea_batch_fit(
    data = make_plot_batch_example(),
    station = "station",
    value = "value",
    distributions = "gum",
    methods = "lmom",
    quiet = TRUE
  )
  
  expect_error(
    plot(batch, type = "diagnostics"),
    "`x\\$diagnostics` is empty"
  )
})


test_that("plot.alea_batch validates selected_models columns", {
  skip_if_not_installed("ggplot2")
  
  batch <- list(
    selected_models = data.frame(
      selected_distribution = "gum"
    )
  )
  
  class(batch) <- c("alea_batch", "list")
  
  expect_error(
    plot(batch, type = "selected_models"),
    "missing required column"
  )
})


test_that("plot.alea_batch validates selected_models distribution type", {
  skip_if_not_installed("ggplot2")
  
  batch <- list(
    selected_models = data.frame(
      station = "A",
      selected_distribution = 1,
      selected_method = "lmom"
    )
  )
  
  class(batch) <- c("alea_batch", "list")
  
  expect_error(
    plot(batch, type = "selected_models"),
    "must be character"
  )
})


test_that("plot.alea_batch validates return_levels columns", {
  skip_if_not_installed("ggplot2")
  
  batch <- list(
    return_levels = data.frame(
      station = "A",
      distribution = "gum",
      method = "lmom",
      return_period = 10
    )
  )
  
  class(batch) <- c("alea_batch", "list")
  
  expect_error(
    plot(batch, type = "return_levels"),
    "missing required column"
  )
})


test_that("plot.alea_batch validates return_levels numeric columns", {
  skip_if_not_installed("ggplot2")
  
  batch <- list(
    return_levels = data.frame(
      station = "A",
      distribution = "gum",
      method = "lmom",
      return_period = "10",
      return_level = 120
    )
  )
  
  class(batch) <- c("alea_batch", "list")
  
  expect_error(
    plot(batch, type = "return_levels"),
    "`x\\$return_levels\\$return_period` and `x\\$return_levels\\$return_level` must be numeric"
  )
})


test_that("plot.alea_batch validates return_period values", {
  skip_if_not_installed("ggplot2")
  
  batch <- list(
    return_levels = data.frame(
      station = "A",
      distribution = "gum",
      method = "lmom",
      return_period = 1,
      return_level = 120
    )
  )
  
  class(batch) <- c("alea_batch", "list")
  
  expect_error(
    plot(batch, type = "return_levels"),
    "greater than 1"
  )
})


test_that("plot.alea_batch rejects non-finite return levels", {
  skip_if_not_installed("ggplot2")
  
  batch <- list(
    return_levels = data.frame(
      station = "A",
      distribution = "gum",
      method = "lmom",
      return_period = c(2, 10),
      return_level = c(NA_real_, Inf)
    )
  )
  
  class(batch) <- c("alea_batch", "list")
  
  expect_error(
    plot(batch, type = "return_levels"),
    "At least one finite batch return level"
  )
})


test_that("plot.alea_batch drops non-finite return levels when finite values exist", {
  skip_if_not_installed("ggplot2")
  
  batch <- list(
    return_levels = data.frame(
      station = "A",
      distribution = "gum",
      method = "lmom",
      return_period = c(2, 5, 10),
      return_level = c(100, NA_real_, 130)
    )
  )
  
  class(batch) <- c("alea_batch", "list")
  
  p <- plot(batch, type = "return_levels")
  
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 2L)
})


test_that("plot.alea_batch validates GOF statistic argument", {
  skip_if_not_installed("ggplot2")
  
  batch <- list(
    gof = data.frame(
      station = "A",
      distribution = "gum",
      method = "lmom",
      statistic = "aic",
      estimate = 100
    )
  )
  
  class(batch) <- c("alea_batch", "list")
  
  expect_error(
    plot(batch, type = "gof", statistic = ""),
    "`statistic` must be a non-empty character scalar"
  )
})


test_that("plot.alea_batch validates GOF columns", {
  skip_if_not_installed("ggplot2")
  
  batch <- list(
    gof = data.frame(
      station = "A",
      distribution = "gum",
      method = "lmom",
      statistic = "aic"
    )
  )
  
  class(batch) <- c("alea_batch", "list")
  
  expect_error(
    plot(batch, type = "gof"),
    "missing required column"
  )
})


test_that("plot.alea_batch validates GOF statistic column type", {
  skip_if_not_installed("ggplot2")
  
  batch <- list(
    gof = data.frame(
      station = "A",
      distribution = "gum",
      method = "lmom",
      statistic = 1,
      estimate = 100
    )
  )
  
  class(batch) <- c("alea_batch", "list")
  
  expect_error(
    plot(batch, type = "gof"),
    "`x\\$gof\\$statistic` must be character"
  )
})


test_that("plot.alea_batch validates GOF estimate column type", {
  skip_if_not_installed("ggplot2")
  
  batch <- list(
    gof = data.frame(
      station = "A",
      distribution = "gum",
      method = "lmom",
      statistic = "aic",
      estimate = "100"
    )
  )
  
  class(batch) <- c("alea_batch", "list")
  
  expect_error(
    plot(batch, type = "gof"),
    "`x\\$gof\\$estimate` must be numeric"
  )
})


test_that("plot.alea_batch rejects missing GOF statistic", {
  skip_if_not_installed("ggplot2")
  
  batch <- list(
    gof = data.frame(
      station = "A",
      distribution = "gum",
      method = "lmom",
      statistic = "bic",
      estimate = 100
    )
  )
  
  class(batch) <- c("alea_batch", "list")
  
  expect_error(
    plot(batch, type = "gof", statistic = "aic"),
    "Statistic 'aic' was not found"
  )
})


test_that("plot.alea_batch rejects non-finite GOF estimates", {
  skip_if_not_installed("ggplot2")
  
  batch <- list(
    gof = data.frame(
      station = "A",
      distribution = "gum",
      method = "lmom",
      statistic = c("aic", "aic"),
      estimate = c(NA_real_, Inf)
    )
  )
  
  class(batch) <- c("alea_batch", "list")
  
  expect_error(
    plot(batch, type = "gof", statistic = "aic"),
    "At least one finite batch GOF estimate"
  )
})


test_that("plot.alea_batch validates diagnostics argument", {
  skip_if_not_installed("ggplot2")
  
  batch <- list(
    diagnostics = data.frame(
      station = "A",
      diagnostic = "stationarity",
      status = "ok",
      p_value = 0.50,
      alpha = 0.05,
      reject = FALSE
    )
  )
  
  class(batch) <- c("alea_batch", "list")
  
  expect_error(
    plot(batch, type = "diagnostics", diagnostic = ""),
    "`diagnostic` must be `NULL` or a non-empty character scalar"
  )
})


test_that("plot.alea_batch validates diagnostics columns", {
  skip_if_not_installed("ggplot2")
  
  batch <- list(
    diagnostics = data.frame(
      station = "A",
      diagnostic = "stationarity",
      status = "ok",
      p_value = 0.50,
      alpha = 0.05
    )
  )
  
  class(batch) <- c("alea_batch", "list")
  
  expect_error(
    plot(batch, type = "diagnostics"),
    "missing required column"
  )
})


test_that("plot.alea_batch validates diagnostics column types", {
  skip_if_not_installed("ggplot2")
  
  batch <- list(
    diagnostics = data.frame(
      station = "A",
      diagnostic = 1,
      status = "ok",
      p_value = 0.50,
      alpha = 0.05,
      reject = FALSE
    )
  )
  
  class(batch) <- c("alea_batch", "list")
  
  expect_error(
    plot(batch, type = "diagnostics"),
    "`x\\$diagnostics\\$diagnostic` must be character"
  )
  
  batch$diagnostics$diagnostic <- "stationarity"
  batch$diagnostics$status <- 1
  
  expect_error(
    plot(batch, type = "diagnostics"),
    "`x\\$diagnostics\\$status` must be character"
  )
  
  batch$diagnostics$status <- "ok"
  batch$diagnostics$p_value <- "0.50"
  
  expect_error(
    plot(batch, type = "diagnostics"),
    "`x\\$diagnostics\\$p_value` must be numeric"
  )
  
  batch$diagnostics$p_value <- 0.50
  batch$diagnostics$alpha <- "0.05"
  
  expect_error(
    plot(batch, type = "diagnostics"),
    "`x\\$diagnostics\\$alpha` must be numeric"
  )
  
  batch$diagnostics$alpha <- 0.05
  batch$diagnostics$reject <- "FALSE"
  
  expect_error(
    plot(batch, type = "diagnostics"),
    "`x\\$diagnostics\\$reject` must be logical"
  )
})


test_that("plot.alea_batch rejects missing diagnostic", {
  skip_if_not_installed("ggplot2")
  
  batch <- list(
    diagnostics = data.frame(
      station = "A",
      diagnostic = "stationarity",
      status = "ok",
      p_value = 0.50,
      alpha = 0.05,
      reject = FALSE
    )
  )
  
  class(batch) <- c("alea_batch", "list")
  
  expect_error(
    plot(batch, type = "diagnostics", diagnostic = "homogeneity"),
    "Diagnostic 'homogeneity' was not found"
  )
})


test_that("plot.alea_batch single diagnostic requires finite p-values", {
  skip_if_not_installed("ggplot2")
  
  batch <- list(
    diagnostics = data.frame(
      station = c("A", "B"),
      diagnostic = c("stationarity", "stationarity"),
      status = c("warning", "warning"),
      p_value = c(NA_real_, NA_real_),
      alpha = c(0.05, 0.05),
      reject = c(NA, NA)
    )
  )
  
  class(batch) <- c("alea_batch", "list")
  
  expect_error(
    plot(batch, type = "diagnostics", diagnostic = "stationarity"),
    "At least one finite p-value"
  )
})


test_that("plot.alea_batch single diagnostic supports multiple alpha values", {
  skip_if_not_installed("ggplot2")
  
  batch <- list(
    diagnostics = data.frame(
      station = c("A", "B"),
      diagnostic = c("stationarity", "stationarity"),
      status = c("ok", "ok"),
      p_value = c(0.20, 0.01),
      alpha = c(0.05, 0.10),
      reject = c(FALSE, TRUE)
    )
  )
  
  class(batch) <- c("alea_batch", "list")
  
  p <- plot(batch, type = "diagnostics", diagnostic = "stationarity")
  
  expect_s3_class(p, "ggplot")
})


test_that("plot.alea_batch diagnostics summary counts statuses", {
  skip_if_not_installed("ggplot2")
  
  batch <- list(
    diagnostics = data.frame(
      station = c("A", "B", "C"),
      diagnostic = c("sample_size", "sample_size", "missing"),
      status = c("ok", "warning", "ok"),
      p_value = c(NA_real_, NA_real_, NA_real_),
      alpha = c(0.05, 0.05, 0.05),
      reject = c(NA, NA, NA)
    )
  )
  
  class(batch) <- c("alea_batch", "list")
  
  p <- plot(batch, type = "diagnostics")
  
  expect_s3_class(p, "ggplot")
  expect_equal(sum(p$data$count), 3L)
})

test_that("plot.alea_batch selected models uses integer count data", {
  skip_if_not_installed("ggplot2")
  
  batch <- alea_batch_fit(
    data = make_plot_batch_example(),
    station = "station",
    value = "value",
    time = "year",
    distributions = c("gum", "gev"),
    methods = "lmom",
    select = "ai",
    quiet = TRUE
  )
  
  p <- plot(batch, type = "selected_models")
  
  expect_s3_class(p, "ggplot")
  expect_identical(p$labels$y, "Number of stations")
})


test_that("plot.alea_batch return-level plot facets by station and supports scales", {
  skip_if_not_installed("ggplot2")
  
  batch <- alea_batch_fit(
    data = make_plot_batch_example(),
    station = "station",
    value = "value",
    time = "year",
    distributions = c("gum", "gev"),
    methods = "lmom",
    return_period = c(2, 5, 10),
    quiet = TRUE
  )
  
  p_gumbel <- plot(batch, type = "return_levels", return_period_scale = "gumbel")
  p_log <- plot(batch, type = "return_levels", return_period_scale = "log")
  p_linear <- plot(batch, type = "return_levels", return_period_scale = "linear")
  
  expect_s3_class(p_gumbel, "ggplot")
  expect_s3_class(p_log, "ggplot")
  expect_s3_class(p_linear, "ggplot")
  expect_true("model_label" %in% names(p_gumbel$data))
  expect_identical(p_gumbel$labels$y, "Quantile")
  expect_true(inherits(p_gumbel$facet, "FacetWrap"))
  expect_error(
    plot(batch, type = "return_levels", return_period_scale = "unsupported"),
    "should be one of"
  )
})


test_that("plot.alea_batch diagnostics status uses complete status levels", {
  skip_if_not_installed("ggplot2")
  
  batch <- alea_batch_fit(
    data = make_plot_batch_example(),
    station = "station",
    value = "value",
    time = "year",
    distributions = "gum",
    methods = "lmom",
    diagnostics = TRUE,
    quiet = TRUE
  )
  
  p <- plot(batch, type = "diagnostics")
  
  expect_s3_class(p, "ggplot")
  expect_true(is.factor(p$data$status))
  expect_identical(levels(p$data$status), c("ok", "warning", "fail"))
  expect_identical(p$labels$y, "Count")
})


test_that("plot.alea_batch single diagnostic uses complete rejection levels", {
  skip_if_not_installed("ggplot2")
  
  batch <- alea_batch_fit(
    data = make_plot_batch_example(),
    station = "station",
    value = "value",
    time = "year",
    distributions = "gum",
    methods = "lmom",
    diagnostics = TRUE,
    quiet = TRUE
  )
  
  p <- plot(batch, type = "diagnostics", diagnostic = "stationarity")
  
  expect_s3_class(p, "ggplot")
  expect_true(is.factor(p$data$reject))
  expect_identical(levels(p$data$reject), c("FALSE", "TRUE"))
  expect_identical(p$labels$shape, "Reject null hypothesis")
})
