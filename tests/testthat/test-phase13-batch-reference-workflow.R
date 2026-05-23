test_that("Phase 13 batch reference workflow returns stable integrated outputs", {
  station_a <- c(
    42.1, 39.4, 51.7, 48.3, 55.2,
    60.1, 46.8, 53.9, 58.4, 62.7,
    49.5, 57.8, 64.3, 52.6, 59.9,
    61.5, 67.2, 54.8, 63.1, 69.4
  )
  
  station_b <- c(
    75.2, 81.4, 79.8, 88.1, 91.5,
    84.7, 95.3, 89.6, 93.2, 97.8,
    86.5, 92.1, 99.4, 94.6, 101.2,
    90.7, 96.8, 103.5, 98.9, 105.1
  )
  
  station_c <- c(
    28.5, 32.1, 35.4, 31.8, 37.6,
    40.2, 36.5, 39.7, 42.3, 38.9,
    41.1, 44.6, 43.2, 45.8, 47.5,
    46.1, 49.3, 48.7, 50.5, 52.2
  )
  
  batch_data <- data.frame(
    station = rep(c("A", "B", "C"), each = 20),
    year = rep(2001:2020, times = 3),
    value = c(station_a, station_b, station_c)
  )
  
  return_period <- c(10, 50, 100)
  
  batch <- alea_batch_fit(
    data = batch_data,
    station = "station",
    time = "year",
    value = "value",
    distributions = c("gev", "gum"),
    methods = c("lmom"),
    return_period = return_period,
    gof = TRUE,
    diagnostics = TRUE,
    select = "ai",
    quiet = TRUE
  )
  
  stations <- alea_results(batch, "stations")
  fits <- alea_results(batch, "fits")
  return_levels <- alea_results(batch, "return_levels")
  gof <- alea_results(batch, "gof")
  diagnostics <- alea_results(batch, "diagnostics")
  selection <- alea_results(batch, "selection")
  selected_models <- alea_results(batch, "selected_models")
  errors <- alea_results(batch, "errors")
  
  expect_s3_class(batch, "alea_batch")
  
  expect_true(is.data.frame(stations))
  expect_true(is.data.frame(fits))
  expect_true(is.data.frame(return_levels))
  expect_true(is.data.frame(gof))
  expect_true(is.data.frame(diagnostics))
  expect_true(is.data.frame(selection))
  expect_true(is.data.frame(selected_models))
  expect_true(is.data.frame(errors))
  
  expect_equal(nrow(stations), 3)
  expect_setequal(stations$station, c("A", "B", "C"))
  
  # 3 stations x 2 distributions x 1 method
  expect_equal(nrow(fits), 6)
  expect_true(all(fits$status == "ok"))
  expect_setequal(fits$station, c("A", "B", "C"))
  expect_setequal(fits$distribution, c("gev", "gum"))
  expect_setequal(fits$method, "lmom")
  
  # 3 stations x 2 fitted models x 3 return periods
  expect_equal(nrow(return_levels), 18)
  expect_setequal(return_levels$return_period, return_period)
  expect_true(all(is.finite(return_levels$return_level)))
  
  # 3 stations x 2 fitted models x 6 GOF statistics
  expect_equal(nrow(gof), 36)
  expect_setequal(gof$statistic, c("ks", "cvm", "ad", "loglik", "aic", "bic"))
  expect_true(all(is.finite(gof$estimate)))
  
  # Diagnostics should be non-empty and station-tagged.
  expect_gt(nrow(diagnostics), 0)
  expect_setequal(diagnostics$station, c("A", "B", "C"))
  
  # AI selection is station-level.
  expect_equal(nrow(selection), 3)
  expect_setequal(selection$station, c("A", "B", "C"))
  
  expect_true(all(
    selection$selected_distribution %in% c("gev", "gpa", "pe3", "ln2", "ln3", "gum")
  ))
  
  # Selected models are available only when the AI-selected distribution was
  # among the fitted candidate distributions. This workflow fits only GEV/GUM,
  # so selected_models may have fewer than 3 rows.
  expect_true(all(selected_models$station %in% c("A", "B", "C")))
  
  if (nrow(selected_models) > 0) {
    expect_true(all(selected_models$distribution %in% c("gev", "gum")))
    expect_true(all(selected_models$method == "lmom"))
  }
  
  # No errors are expected for this controlled synthetic case.
  expect_equal(nrow(errors), 0)
  
  expect_identical(
    names(errors),
    c("station", "step", "distribution", "method", "message", "class")
  )
})