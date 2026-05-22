expected_diagnostics <- c(
  "sample_size",
  "missing",
  "ties",
  "range",
  "skewness",
  "randomness",
  "independence",
  "homogeneity",
  "stationarity"
)

expected_diagnostics_columns <- c(
  "distribution",
  "method",
  "diagnostic",
  "statistic",
  "value",
  "p_value",
  "alpha",
  "reject",
  "threshold",
  "status",
  "message",
  "n",
  "n_valid"
)

diagnostics_test_sample <- c(
  18.2, 21.5, 24.1, 26.8, 29.4, 33.7, 37.9, 42.6,
  48.3, 55.1, 63.8, 74.5, 88.9, 106.4, 128.7,
  156.2, 191.5, 238.9, 302.4, 391.8
)

basic_diagnostics <- c(
  "sample_size",
  "missing",
  "ties",
  "range",
  "skewness"
)

hypothesis_diagnostics <- c(
  "randomness",
  "independence",
  "homogeneity",
  "stationarity"
)


test_that("alea_diagnostics() returns expected structure for numeric input", {
  x <- diagnostics_test_sample
  
  diag <- alea_diagnostics(x)
  
  expect_s3_class(diag, "alea_diagnostics")
  expect_s3_class(diag, "data.frame")
  expect_named(diag, expected_diagnostics_columns)
  
  expect_equal(nrow(diag), 9L)
  expect_equal(diag$diagnostic, expected_diagnostics)
  
  expect_true(all(is.na(diag$distribution)))
  expect_true(all(is.na(diag$method)))
  expect_equal(diag$n, rep(length(x), 9L))
  expect_equal(diag$n_valid, rep(length(x), 9L))
  
  expect_true(all(diag$status %in% c("ok", "warning")))
  expect_true(all(diag$status[diag$diagnostic %in% basic_diagnostics] == "ok"))
  
  expect_true(all(is.na(diag$p_value[diag$diagnostic %in% basic_diagnostics])))
  expect_true(all(is.na(diag$alpha[diag$diagnostic %in% basic_diagnostics])))
  expect_true(all(is.na(diag$reject[diag$diagnostic %in% basic_diagnostics])))
  
  expect_equal(
    diag$alpha[diag$diagnostic %in% hypothesis_diagnostics],
    rep(0.05, 4L)
  )
})


test_that("alea_diagnostics() returns expected structure for alea_fit input", {
  x <- diagnostics_test_sample
  
  fit <- alea_fit(x, distribution = "gev", method = "lmom")
  diag <- alea_diagnostics(fit)
  
  expect_s3_class(diag, "alea_diagnostics")
  expect_s3_class(diag, "data.frame")
  expect_named(diag, expected_diagnostics_columns)
  
  expect_equal(nrow(diag), 9L)
  expect_equal(diag$diagnostic, expected_diagnostics)
  expect_equal(unique(diag$distribution), "gev")
  expect_equal(unique(diag$method), "lmom")
  expect_equal(diag$n, rep(length(x), 9L))
  expect_equal(diag$n_valid, rep(length(x), 9L))
  
  expect_true(all(diag$status %in% c("ok", "warning")))
  expect_true(all(diag$status[diag$diagnostic %in% basic_diagnostics] == "ok"))
})


test_that("alea_diagnostics() accepts diagnostics = 'all'", {
  x <- diagnostics_test_sample
  
  diag <- alea_diagnostics(x, diagnostics = "all")
  
  expect_equal(diag$diagnostic, expected_diagnostics)
  expect_equal(nrow(diag), 9L)
})


test_that("alea_diagnostics() can compute a selected subset of diagnostics", {
  x <- diagnostics_test_sample
  
  diag <- alea_diagnostics(x, diagnostics = c("sample_size", "skewness"))
  
  expect_s3_class(diag, "alea_diagnostics")
  expect_equal(nrow(diag), 2L)
  expect_equal(diag$diagnostic, c("sample_size", "skewness"))
  expect_true(all(diag$status == "ok"))
})


test_that("alea_diagnostics() validates diagnostic names", {
  x <- c(1, 2, 3, 4, 5)
  
  expect_error(
    alea_diagnostics(x, diagnostics = "invalid_diagnostic"),
    "Unsupported diagnostic"
  )
  
  expect_error(
    alea_diagnostics(x, diagnostics = numeric(0)),
    "`diagnostics` must contain at least one diagnostic",
    fixed = TRUE
  )
  
  expect_error(
    alea_diagnostics(x, diagnostics = 1),
    "`diagnostics` must be a character vector",
    fixed = TRUE
  )
})


test_that("alea_diagnostics() validates min_n", {
  x <- c(1, 2, 3, 4, 5)
  
  expect_error(
    alea_diagnostics(x, min_n = NA_real_),
    "`min_n` must be a single finite numeric value",
    fixed = TRUE
  )
  
  expect_error(
    alea_diagnostics(x, min_n = c(5, 10)),
    "`min_n` must be a single finite numeric value",
    fixed = TRUE
  )
  
  expect_error(
    alea_diagnostics(x, min_n = 1),
    "`min_n` must be greater than or equal to 2",
    fixed = TRUE
  )
})


test_that("alea_diagnostics() validates alpha", {
  x <- c(1, 2, 3, 4, 5, 6)
  
  expect_error(
    alea_diagnostics(x, alpha = NA_real_),
    "`alpha` must be a single finite numeric value",
    fixed = TRUE
  )
  
  expect_error(
    alea_diagnostics(x, alpha = c(0.05, 0.10)),
    "`alpha` must be a single finite numeric value",
    fixed = TRUE
  )
  
  expect_error(
    alea_diagnostics(x, alpha = 0),
    "`alpha` must be in the open interval \\(0, 1\\)"
  )
  
  expect_error(
    alea_diagnostics(x, alpha = 1),
    "`alpha` must be in the open interval \\(0, 1\\)"
  )
})


test_that("sample_size diagnostic flags small samples", {
  x <- c(10, 20, 30, 40, 50)
  
  diag <- alea_diagnostics(x, diagnostics = "sample_size", min_n = 10)
  
  expect_equal(diag$diagnostic, "sample_size")
  expect_equal(diag$statistic, "n_valid")
  expect_equal(diag$value, 5)
  expect_equal(diag$threshold, ">= 10")
  expect_equal(diag$status, "warning")
})


test_that("missing diagnostic flags missing and non-finite values", {
  x <- c(10, 20, NA, 30, Inf, NaN, 40)
  
  diag <- alea_diagnostics(x, diagnostics = "missing")
  
  expect_equal(diag$diagnostic, "missing")
  expect_equal(diag$statistic, "n_missing_or_nonfinite")
  expect_equal(diag$value, 3)
  expect_equal(diag$threshold, "== 0")
  expect_equal(diag$status, "warning")
  expect_equal(diag$n, length(x))
  expect_equal(diag$n_valid, 4L)
})


test_that("ties diagnostic flags repeated finite values", {
  x <- c(10, 20, 20, 30, 30, 30, 40)
  
  diag <- alea_diagnostics(x, diagnostics = "ties")
  
  expect_equal(diag$diagnostic, "ties")
  expect_equal(diag$statistic, "n_ties")
  expect_equal(diag$value, 3)
  expect_equal(diag$threshold, "== 0")
  expect_equal(diag$status, "warning")
})


test_that("ties diagnostic handles no finite observations", {
  x <- c(NA, Inf, -Inf, NaN)
  
  diag <- alea_diagnostics(x, diagnostics = "ties")
  
  expect_equal(diag$diagnostic, "ties")
  expect_equal(diag$statistic, "n_ties")
  expect_true(is.na(diag$value))
  expect_equal(diag$status, "fail")
  expect_equal(diag$n_valid, 0L)
})


test_that("range diagnostic flags constant samples", {
  x <- c(10, 10, 10, 10, 10)
  
  diag <- alea_diagnostics(x, diagnostics = "range")
  
  expect_equal(diag$diagnostic, "range")
  expect_equal(diag$statistic, "n_unique")
  expect_equal(diag$value, 1)
  expect_equal(diag$threshold, "> 1")
  expect_equal(diag$status, "fail")
})


test_that("range diagnostic handles no finite observations", {
  x <- c(NA, Inf, -Inf, NaN)
  
  diag <- alea_diagnostics(x, diagnostics = "range")
  
  expect_equal(diag$diagnostic, "range")
  expect_equal(diag$statistic, "n_unique")
  expect_true(is.na(diag$value))
  expect_equal(diag$status, "fail")
  expect_equal(diag$n_valid, 0L)
})


test_that("skewness diagnostic returns finite skewness for valid samples", {
  x <- diagnostics_test_sample
  
  diag <- alea_diagnostics(x, diagnostics = "skewness")
  
  expect_equal(diag$diagnostic, "skewness")
  expect_equal(diag$statistic, "sample_skewness")
  expect_true(is.finite(diag$value))
  expect_equal(diag$threshold, "finite")
  expect_equal(diag$status, "ok")
})


test_that("skewness diagnostic flags samples with fewer than three finite values", {
  x <- c(10, 20, NA, Inf)
  
  diag <- alea_diagnostics(x, diagnostics = "skewness")
  
  expect_equal(diag$diagnostic, "skewness")
  expect_equal(diag$statistic, "sample_skewness")
  expect_true(is.na(diag$value))
  expect_equal(diag$status, "warning")
  expect_equal(diag$n_valid, 2L)
})


test_that("skewness diagnostic flags zero-variance samples", {
  x <- c(10, 10, 10, 10, 10)
  
  diag <- alea_diagnostics(x, diagnostics = "skewness")
  
  expect_equal(diag$diagnostic, "skewness")
  expect_equal(diag$statistic, "sample_skewness")
  expect_true(is.na(diag$value))
  expect_equal(diag$status, "warning")
})


test_that("hypothesis diagnostics return structured rows", {
  x <- diagnostics_test_sample
  
  diag <- alea_diagnostics(
    x,
    diagnostics = hypothesis_diagnostics
  )
  
  expect_s3_class(diag, "alea_diagnostics")
  expect_named(diag, expected_diagnostics_columns)
  
  expect_equal(diag$diagnostic, hypothesis_diagnostics)
  expect_equal(
    diag$statistic,
    c(
      "bartels_rank_von_neumann",
      "wald_wolfowitz",
      "pettitt_change_point",
      "mann_kendall"
    )
  )
  
  expect_true(all(diag$status %in% c("ok", "warning")))
  expect_true(all(diag$threshold == "p >= 0.05"))
  expect_equal(diag$n, rep(length(x), 4L))
  expect_equal(diag$n_valid, rep(length(x), 4L))
  expect_equal(diag$alpha, rep(0.05, 4L))
})


test_that("hypothesis diagnostics use requested alpha", {
  x <- diagnostics_test_sample
  
  diag <- alea_diagnostics(
    x,
    diagnostics = hypothesis_diagnostics,
    alpha = 0.10
  )
  
  expect_equal(diag$alpha, rep(0.10, 4L))
  expect_equal(diag$threshold, rep("p >= 0.1", 4L))
})


test_that("hypothesis diagnostics handle samples that are too short", {
  x <- c(1, 2, 3)
  
  diag <- alea_diagnostics(
    x,
    diagnostics = hypothesis_diagnostics
  )
  
  expect_equal(diag$diagnostic, hypothesis_diagnostics)
  expect_true(all(diag$status == "warning"))
  expect_true(all(is.na(diag$value)))
  expect_true(all(is.na(diag$p_value)))
  expect_true(all(is.na(diag$reject)))
})


test_that("hypothesis diagnostics return p-values when trend is available", {
  skip_if_not_installed("trend")
  
  x <- diagnostics_test_sample
  
  diag <- alea_diagnostics(
    x,
    diagnostics = hypothesis_diagnostics
  )
  
  expect_true(all(is.finite(diag$value)))
  expect_true(all(is.finite(diag$p_value)))
  expect_true(all(diag$p_value >= 0))
  expect_true(all(diag$p_value <= 1))
  expect_true(all(!is.na(diag$reject)))
})


test_that("hypothesis diagnostics return warning rows when trend tests fail internally", {
  result <- hypothesis_diagnostic_row_from_htest(
    result = list(
      available = TRUE,
      statistic = NA_real_,
      p_value = NA_real_,
      message = "internal test failure"
    ),
    distribution = NA_character_,
    method = NA_character_,
    diagnostic = "stationarity",
    statistic = "mann_kendall",
    alpha = 0.05,
    n = 10L,
    n_valid = 10L,
    ok_message = "ok",
    warning_message = "warning",
    unavailable_message = "unavailable"
  )
  
  expect_equal(result$diagnostic, "stationarity")
  expect_equal(result$statistic, "mann_kendall")
  expect_true(is.na(result$value))
  expect_true(is.na(result$p_value))
  expect_true(is.na(result$reject))
  expect_equal(result$status, "warning")
  expect_equal(result$message, "internal test failure")
})


test_that("alea_diagnostics.numeric() rejects non-numeric input through S3 dispatch", {
  expect_error(
    alea_diagnostics("not numeric")
  )
})


test_that("alea_diagnostics() works for all supported fitted distributions", {
  x <- diagnostics_test_sample
  
  distributions <- c("gev", "gpa", "pe3", "ln2", "ln3", "gum")
  
  for (distribution in distributions) {
    fit <- alea_fit(x, distribution = distribution, method = "lmom")
    diag <- alea_diagnostics(fit)
    
    expect_s3_class(diag, "alea_diagnostics")
    expect_equal(nrow(diag), 9L)
    expect_equal(diag$diagnostic, expected_diagnostics)
    expect_equal(unique(diag$distribution), distribution)
    expect_equal(unique(diag$method), "lmom")
    expect_true(all(diag$n == length(x)))
    expect_true(all(diag$n_valid == length(x)))
  }
})


test_that("alea_diagnostics() works for all supported estimation methods", {
  x <- diagnostics_test_sample
  
  methods <- c("lmom", "mom", "mle")
  
  for (method in methods) {
    fit <- alea_fit(x, distribution = "gev", method = method)
    diag <- alea_diagnostics(fit)
    
    expect_s3_class(diag, "alea_diagnostics")
    expect_equal(nrow(diag), 9L)
    expect_equal(diag$diagnostic, expected_diagnostics)
    expect_equal(unique(diag$distribution), "gev")
    expect_equal(unique(diag$method), method)
    expect_true(all(diag$n == length(x)))
    expect_true(all(diag$n_valid == length(x)))
  }
})


test_that("print.alea_diagnostics() prints expected header for numeric diagnostics", {
  x <- diagnostics_test_sample
  
  diag <- alea_diagnostics(x)
  
  expect_output(
    print(diag),
    "ALEA diagnostics"
  )
  
  expect_output(
    print(diag),
    "Number of observations: 20"
  )
})


test_that("print.alea_diagnostics() prints fitted model information", {
  x <- diagnostics_test_sample
  
  fit <- alea_fit(x, distribution = "gev", method = "lmom")
  diag <- alea_diagnostics(fit)
  
  expect_output(
    print(diag),
    "Distribution: gev"
  )
  
  expect_output(
    print(diag),
    "Estimation method: lmom"
  )
})


test_that("as.data.frame.alea_diagnostics() drops alea_diagnostics class", {
  x <- diagnostics_test_sample
  
  diag <- alea_diagnostics(x)
  out <- as.data.frame(diag)
  
  expect_s3_class(out, "data.frame")
  expect_false(inherits(out, "alea_diagnostics"))
  expect_equal(nrow(out), nrow(diag))
  expect_equal(names(out), names(diag))
})


test_that("sample_skewness_diagnostic() agrees with direct calculation", {
  x <- c(1, 2, 3, 4, 10)
  
  n <- length(x)
  s <- stats::sd(x)
  m <- mean(x)
  
  expected <- sum(((x - m) / s)^3) * n / ((n - 1) * (n - 2))
  
  expect_equal(sample_skewness_diagnostic(x), expected)
})


test_that("unavailable hypothesis diagnostics return structured warning rows", {
  row <- new_hypothesis_diagnostic_unavailable_row(
    distribution = NA_character_,
    method = NA_character_,
    diagnostic = "stationarity",
    statistic = "mann_kendall",
    alpha = 0.05,
    n = 10L,
    n_valid = 10L,
    message = "The Mann-Kendall stationarity test requires the suggested package 'trend'."
  )
  
  expect_equal(row$diagnostic, "stationarity")
  expect_equal(row$statistic, "mann_kendall")
  expect_true(is.na(row$value))
  expect_true(is.na(row$p_value))
  expect_true(is.na(row$reject))
  expect_equal(row$alpha, 0.05)
  expect_equal(row$threshold, "p >= 0.05")
  expect_equal(row$status, "warning")
  expect_equal(
    row$message,
    "The Mann-Kendall stationarity test requires the suggested package 'trend'."
  )
  expect_equal(row$n, 10L)
  expect_equal(row$n_valid, 10L)
})