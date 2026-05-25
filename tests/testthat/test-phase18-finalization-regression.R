# Phase 18 finalization regression tests
#
# These tests cover the final public-facing changes introduced after the
# quantile terminology refactor, compact batch-result printing, and observed
# plotting-position support in quantile plots.

phase18_sample <- function() {
  c(
    74.2, 81.6, 86.1, 91.4, 95.3, 99.7, 103.5, 107.9,
    113.2, 118.8, 124.1, 130.6, 138.4, 146.9, 157.5,
    170.2, 185.6, 203.1, 225.4, 252.7, 286.3, 331.8
  )
}

phase18_batch_data <- function(x = phase18_sample()) {
  data.frame(
    station = rep(c("B", "A"), each = length(x)),
    year = rep(seq_along(x), times = 2),
    value = c(x * 1.20, x),
    stringsAsFactors = FALSE
  )
}

phase18_has_geom <- function(p, geom_class) {
  any(vapply(p$layers, function(layer) inherits(layer$geom, geom_class), logical(1)))
}

phase18_is_sorted <- function(df, cols) {
  ord <- do.call(base::order, unname(df[cols]))
  identical(seq_len(nrow(df)), ord)
}

test_that("Phase 18 public API exposes quantiles and rejects old return-level API", {
  x <- phase18_sample()
  fit <- alea_fit(x, distribution = "gum", method = "lmom")

  expect_false(exists("alea_return_level", mode = "function"))

  q <- alea_quantile(fit, return_period = c(10, 25))
  expect_s3_class(q, "alea_quantile")
  expect_true("quantile" %in% names(q))
  expect_false("return_level" %in% names(q))

  expect_error(
    confint(fit, parm = "return_level", return_period = 10, n_boot = 10),
    "Only parm = 'quantile' is currently supported",
    fixed = TRUE
  )

  batch <- alea_fit(
    phase18_batch_data(x),
    station = "station",
    time = "year",
    value = "value",
    distribution = "gum",
    method = "lmom",
    return_period = c(10, 25)
  )

  expect_s3_class(batch, "alea_batch")
  expect_true("quantiles" %in% names(batch))
  expect_false("return_levels" %in% names(batch))
  expect_error(alea_results(batch, "return_levels"))
})


test_that("multi-model quantile and CI objects preserve observed sample for plotting", {
  x <- phase18_sample()
  cmp <- alea_fit(
    x,
    distribution = c("gev", "gum", "pe3"),
    method = c("lmom", "mle")
  )

  q <- alea_quantile(cmp, return_period = c(10, 25, 50, 100))
  ci <- confint(
    cmp,
    parm = "quantile",
    return_period = c(10, 25, 50),
    n_boot = 10,
    seed = 123
  )

  expect_s3_class(q, "alea_quantile")
  expect_s3_class(ci, "alea_quantile_ci")
  expect_equal(attr(q, "observed_data", exact = TRUE), x)
  expect_equal(attr(ci, "observed_data", exact = TRUE), x)

  expect_true(phase18_is_sorted(
    as.data.frame(q),
    c("distribution", "method", "return_period")
  ))
  expect_true(phase18_is_sorted(
    as.data.frame(ci),
    c("distribution", "method", "return_period")
  ))
})


test_that("quantile and CI plots can include or omit observed plotting positions", {
  skip_if_not_installed("ggplot2")

  x <- phase18_sample()
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  cmp <- alea_fit(
    x,
    distribution = c("gev", "gum"),
    method = c("lmom", "mle")
  )

  q_fit <- alea_quantile(fit, return_period = c(10, 25, 50, 100, 200))
  q_cmp <- alea_quantile(cmp, return_period = c(10, 25, 50, 100, 200))
  ci_fit <- confint(
    fit,
    parm = "quantile",
    return_period = c(10, 25, 50, 100),
    n_boot = 10,
    seed = 123
  )
  ci_cmp <- confint(
    cmp,
    parm = "quantile",
    return_period = c(10, 25, 50),
    n_boot = 10,
    seed = 123
  )

  plot_objects <- list(
    q_fit = q_fit,
    q_cmp = q_cmp,
    ci_fit = ci_fit,
    ci_cmp = ci_cmp
  )

  for (nm in names(plot_objects)) {
    p_observed <- plot(plot_objects[[nm]], plot_observed = TRUE)
    p_hidden <- plot(plot_objects[[nm]], plot_observed = FALSE)

    expect_s3_class(p_observed, "ggplot")
    expect_s3_class(p_hidden, "ggplot")
    expect_true(phase18_has_geom(p_observed, "GeomPoint"), info = nm)
    expect_false(phase18_has_geom(p_hidden, "GeomPoint"), info = nm)
  }
})


test_that("batch quantile plots include observed points and hide them on request", {
  skip_if_not_installed("ggplot2")

  batch <- alea_fit(
    phase18_batch_data(),
    station = "station",
    time = "year",
    value = "value",
    distribution = c("gev", "gum"),
    method = "lmom",
    return_period = c(10, 25, 50, 100, 200)
  )

  p_observed <- plot(batch, type = "quantiles", plot_observed = TRUE)
  p_hidden <- plot(batch, type = "quantiles", plot_observed = FALSE)

  expect_s3_class(p_observed, "ggplot")
  expect_s3_class(p_hidden, "ggplot")
  expect_true(phase18_has_geom(p_observed, "GeomPoint"))
  expect_false(phase18_has_geom(p_hidden, "GeomPoint"))

  expect_error(
    plot(batch, type = "quantiles", plot_observed = NA),
    "`plot_observed` must be `TRUE` or `FALSE`",
    fixed = TRUE
  )
})


test_that("batch result printers are compact while as.data.frame remains complete", {
  batch <- alea_fit(
    phase18_batch_data(),
    station = "station",
    time = "year",
    value = "value",
    distribution = c("gev", "gum"),
    method = "lmom",
    return_period = c(10, 25),
    gof = TRUE,
    diagnostics = TRUE,
    select = "none"
  )

  quantiles <- alea_results(batch, "quantiles")
  gof <- alea_results(batch, "gof")
  diagnostics <- alea_results(batch, "diagnostics")
  selection <- alea_results(batch, "selection")
  selected_models <- alea_results(batch, "selected_models")
  errors <- alea_results(batch, "errors")

  expect_s3_class(quantiles, "alea_batch_quantiles")
  expect_s3_class(gof, "alea_batch_gof")
  expect_s3_class(diagnostics, "alea_batch_diagnostics")
  expect_s3_class(selection, "alea_batch_selection")
  expect_s3_class(selected_models, "alea_batch_selected_models")
  expect_s3_class(errors, "alea_batch_errors")

  expect_true("probability" %in% names(as.data.frame(quantiles)))
  expect_true("description" %in% names(as.data.frame(gof)))
  expect_true("message" %in% names(as.data.frame(diagnostics)))

  expect_output(print(selection), "No AI-selection results are available", fixed = TRUE)
  expect_output(print(selected_models), "No selected-model results are available", fixed = TRUE)
  expect_output(print(errors), "No errors were recorded", fixed = TRUE)
})


test_that("batch result tables preserve final public sorting", {
  batch <- alea_fit(
    phase18_batch_data(),
    station = "station",
    time = "year",
    value = "value",
    distribution = c("pe3", "gev", "gum"),
    method = c("mle", "lmom"),
    return_period = c(50, 10),
    gof = TRUE,
    diagnostics = TRUE
  )

  expect_true(phase18_is_sorted(
    as.data.frame(alea_results(batch, "fits")),
    c("station", "distribution", "method")
  ))
  expect_true(phase18_is_sorted(
    as.data.frame(alea_results(batch, "quantiles")),
    c("station", "distribution", "method", "return_period")
  ))

  gof <- as.data.frame(alea_results(batch, "gof"))
  gof$.statistic_order <- match(gof$statistic, c("ks", "cvm", "ad", "loglik", "aic", "bic"))
  expect_true(phase18_is_sorted(gof, c("station", "distribution", "method", ".statistic_order")))

  diagnostics <- as.data.frame(alea_results(batch, "diagnostics"))
  diagnostics$.diagnostic_order <- match(
    diagnostics$diagnostic,
    c(
      "sample_size", "missing", "ties", "range", "skewness",
      "randomness", "independence", "homogeneity", "stationarity"
    )
  )
  expect_true(phase18_is_sorted(diagnostics, c("station", "distribution", "method", ".diagnostic_order")))
})
