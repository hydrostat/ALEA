test_that("plot.alea_diagnostics returns a ggplot object for status plot", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- stats::rnorm(60, mean = 100, sd = 15)
  
  diagnostics <- alea_diagnostics(x)
  
  p <- plot(diagnostics, type = "status")
  
  expect_s3_class(p, "ggplot")
})


test_that("plot.alea_diagnostics returns a ggplot object for p-value plot", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("trend")
  
  set.seed(123)
  x <- stats::rnorm(60, mean = 100, sd = 15)
  
  diagnostics <- alea_diagnostics(x)
  
  p <- plot(diagnostics, type = "p_value")
  
  expect_s3_class(p, "ggplot")
})


test_that("plot.alea_diagnostics works for alea_fit diagnostics", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- stats::rnorm(60, mean = 100, sd = 15)
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  diagnostics <- alea_diagnostics(fit)
  
  p <- plot(diagnostics, type = "status")
  
  expect_s3_class(p, "ggplot")
})


test_that("plot.alea_diagnostics rejects unsupported plot type", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- stats::rnorm(60, mean = 100, sd = 15)
  
  diagnostics <- alea_diagnostics(x)
  
  expect_error(
    plot(diagnostics, type = "unsupported"),
    "should be one of"
  )
})


test_that("plot.alea_diagnostics validates required columns", {
  skip_if_not_installed("ggplot2")
  
  bad <- data.frame(
    diagnostic = "sample_size",
    status = "ok",
    message = "Sample size is adequate.",
    p_value = NA_real_,
    alpha = 0.05,
    reject = NA
  )
  
  class(bad) <- c("alea_diagnostics", "data.frame")
  
  expect_error(
    plot(bad),
    "missing required column"
  )
})


test_that("plot.alea_diagnostics validates diagnostic column type", {
  skip_if_not_installed("ggplot2")
  
  bad <- data.frame(
    diagnostic = 1,
    status = "ok",
    message = "Sample size is adequate.",
    p_value = NA_real_,
    alpha = 0.05,
    reject = NA,
    n = 30,
    n_valid = 30
  )
  
  class(bad) <- c("alea_diagnostics", "data.frame")
  
  expect_error(
    plot(bad),
    "`diagnostic` column must be character"
  )
})


test_that("plot.alea_diagnostics validates status column type", {
  skip_if_not_installed("ggplot2")
  
  bad <- data.frame(
    diagnostic = "sample_size",
    status = 1,
    message = "Sample size is adequate.",
    p_value = NA_real_,
    alpha = 0.05,
    reject = NA,
    n = 30,
    n_valid = 30
  )
  
  class(bad) <- c("alea_diagnostics", "data.frame")
  
  expect_error(
    plot(bad),
    "`status` column must be character"
  )
})


test_that("plot.alea_diagnostics validates p_value column type", {
  skip_if_not_installed("ggplot2")
  
  bad <- data.frame(
    diagnostic = "stationarity",
    status = "ok",
    message = "Mann-Kendall test completed.",
    p_value = "0.50",
    alpha = 0.05,
    reject = FALSE,
    n = 30,
    n_valid = 30
  )
  
  class(bad) <- c("alea_diagnostics", "data.frame")
  
  expect_error(
    plot(bad),
    "`p_value` column must be numeric"
  )
})


test_that("plot.alea_diagnostics validates alpha column type", {
  skip_if_not_installed("ggplot2")
  
  bad <- data.frame(
    diagnostic = "stationarity",
    status = "ok",
    message = "Mann-Kendall test completed.",
    p_value = 0.50,
    alpha = "0.05",
    reject = FALSE,
    n = 30,
    n_valid = 30
  )
  
  class(bad) <- c("alea_diagnostics", "data.frame")
  
  expect_error(
    plot(bad),
    "`alpha` column must be numeric"
  )
})


test_that("plot.alea_diagnostics validates reject column type", {
  skip_if_not_installed("ggplot2")
  
  bad <- data.frame(
    diagnostic = "stationarity",
    status = "ok",
    message = "Mann-Kendall test completed.",
    p_value = 0.50,
    alpha = 0.05,
    reject = "FALSE",
    n = 30,
    n_valid = 30
  )
  
  class(bad) <- c("alea_diagnostics", "data.frame")
  
  expect_error(
    plot(bad),
    "`reject` column must be logical"
  )
})


test_that("plot.alea_diagnostics validates n and n_valid column types", {
  skip_if_not_installed("ggplot2")
  
  bad <- data.frame(
    diagnostic = "sample_size",
    status = "ok",
    message = "Sample size is adequate.",
    p_value = NA_real_,
    alpha = 0.05,
    reject = NA,
    n = "30",
    n_valid = 30
  )
  
  class(bad) <- c("alea_diagnostics", "data.frame")
  
  expect_error(
    plot(bad),
    "`n` and `n_valid` columns must be numeric"
  )
})


test_that("plot.alea_diagnostics rejects empty diagnostics tables", {
  skip_if_not_installed("ggplot2")
  
  bad <- data.frame(
    diagnostic = character(),
    status = character(),
    message = character(),
    p_value = numeric(),
    alpha = numeric(),
    reject = logical(),
    n = numeric(),
    n_valid = numeric()
  )
  
  class(bad) <- c("alea_diagnostics", "data.frame")
  
  expect_error(
    plot(bad),
    "At least one diagnostic row"
  )
})


test_that("plot.alea_diagnostics p-value plot requires finite p-values", {
  skip_if_not_installed("ggplot2")
  
  diagnostics <- data.frame(
    diagnostic = c("sample_size", "missing"),
    status = c("ok", "ok"),
    message = c("Sample size is adequate.", "No missing values."),
    p_value = c(NA_real_, NA_real_),
    alpha = c(0.05, 0.05),
    reject = c(NA, NA),
    n = c(30, 30),
    n_valid = c(30, 30)
  )
  
  class(diagnostics) <- c("alea_diagnostics", "data.frame")
  
  expect_error(
    plot(diagnostics, type = "p_value"),
    "At least one finite diagnostic p-value"
  )
})


test_that("plot.alea_diagnostics p-value plot supports multiple alpha values", {
  skip_if_not_installed("ggplot2")
  
  diagnostics <- data.frame(
    diagnostic = c("stationarity", "homogeneity"),
    status = c("ok", "ok"),
    message = c("Mann-Kendall test completed.", "Pettitt test completed."),
    p_value = c(0.20, 0.01),
    alpha = c(0.05, 0.10),
    reject = c(FALSE, TRUE),
    n = c(40, 40),
    n_valid = c(40, 40)
  )
  
  class(diagnostics) <- c("alea_diagnostics", "data.frame")
  
  p <- plot(diagnostics, type = "p_value")
  
  expect_s3_class(p, "ggplot")
})


test_that("plot.alea_diagnostics status plot counts statuses", {
  skip_if_not_installed("ggplot2")
  
  diagnostics <- data.frame(
    diagnostic = c("sample_size", "sample_size", "missing"),
    status = c("ok", "warning", "ok"),
    message = c(
      "Sample size is adequate.",
      "Sample size is short.",
      "No missing values."
    ),
    p_value = c(NA_real_, NA_real_, NA_real_),
    alpha = c(0.05, 0.05, 0.05),
    reject = c(NA, NA, NA),
    n = c(30, 8, 30),
    n_valid = c(30, 8, 30)
  )
  
  class(diagnostics) <- c("alea_diagnostics", "data.frame")
  
  p <- plot(diagnostics, type = "status")
  
  expect_s3_class(p, "ggplot")
  expect_equal(sum(p$data$count), 3L)
})

test_that("plot.alea_diagnostics status plot keeps complete status levels", {
  skip_if_not_installed("ggplot2")
  
  diagnostics <- data.frame(
    diagnostic = c("sample_size", "ties"),
    status = c("ok", "warning"),
    message = c("Sample size is adequate.", "Ties detected."),
    p_value = c(NA_real_, NA_real_),
    alpha = c(NA_real_, NA_real_),
    reject = c(NA, NA),
    n = c(30, 30),
    n_valid = c(30, 30)
  )
  class(diagnostics) <- c("alea_diagnostics", "data.frame")
  
  p <- plot(diagnostics, type = "status")
  
  expect_s3_class(p, "ggplot")
  expect_true(is.factor(p$data$status))
  expect_identical(levels(p$data$status), c("ok", "warning", "fail"))
})


test_that("plot.alea_diagnostics p-value plot keeps complete rejection levels", {
  skip_if_not_installed("ggplot2")
  
  diagnostics <- data.frame(
    diagnostic = c("stationarity", "homogeneity"),
    status = c("ok", "ok"),
    message = c("Mann-Kendall test completed.", "Pettitt test completed."),
    p_value = c(0.20, 0.30),
    alpha = c(0.05, 0.05),
    reject = c(FALSE, FALSE),
    n = c(40, 40),
    n_valid = c(40, 40)
  )
  class(diagnostics) <- c("alea_diagnostics", "data.frame")
  
  p <- plot(diagnostics, type = "p_value")
  
  expect_s3_class(p, "ggplot")
  expect_true(is.factor(p$data$reject))
  expect_identical(levels(p$data$reject), c("FALSE", "TRUE"))
  expect_identical(p$labels$shape, "Reject null hypothesis")
  expect_true("Reject null hypothesis" %in% unlist(p$labels))
})
