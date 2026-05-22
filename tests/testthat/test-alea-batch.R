test_that("alea_batch_fit returns an alea_batch object", {
  set.seed(123)
  
  dat <- data.frame(
    station = rep(c("A", "B"), each = 40),
    year = rep(seq_len(40), times = 2),
    value = c(rnorm(40, mean = 100, sd = 15), rnorm(40, mean = 120, sd = 20))
  )
  
  batch <- alea_batch_fit(
    data = dat,
    station = "station",
    time = "year",
    value = "value",
    distributions = c("gum"),
    methods = c("lmom"),
    return_period = c(10, 50)
  )
  
  expect_s3_class(batch, "alea_batch")
  expect_equal(nrow(alea_results(batch, "stations")), 2)
  expect_equal(nrow(alea_results(batch, "fits")), 2)
  expect_equal(nrow(alea_results(batch, "return_levels")), 4)
  expect_equal(nrow(alea_results(batch, "errors")), 0)
})


test_that("alea_batch_fit records fit errors without stopping", {
  dat <- data.frame(
    station = rep(c("good", "bad"), each = 30),
    year = rep(seq_len(30), times = 2),
    value = c(rnorm(30, mean = 100, sd = 10), rep(100, 30))
  )
  
  batch <- alea_batch_fit(
    data = dat,
    station = "station",
    time = "year",
    value = "value",
    distributions = c("gum", "gev"),
    methods = c("lmom", "mle")
  )
  
  expect_s3_class(batch, "alea_batch")
  expect_true(nrow(alea_results(batch, "fits")) > 0)
  expect_true(nrow(alea_results(batch, "errors")) >= 0)
  
  errors <- alea_results(batch, "errors")
  if (nrow(errors) > 0) {
    expect_true(all(c("station", "step", "distribution", "method", "message") %in% names(errors)))
  }
})


test_that("alea_results validates object and type", {
  dat <- data.frame(
    station = rep("A", 40),
    value = rnorm(40)
  )
  
  batch <- alea_batch_fit(
    data = dat,
    station = "station",
    value = "value",
    distributions = "gum",
    methods = "lmom"
  )
  
  expect_error(alea_results(list(), "fits"), "alea_batch")
  expect_error(alea_results(batch, "unknown"))
})


test_that("alea_batch_fit validates input columns", {
  dat <- data.frame(
    station = "A",
    value = 1
  )
  
  expect_error(
    alea_batch_fit(dat, station = "missing", value = "value"),
    "column"
  )
  
  expect_error(
    alea_batch_fit(dat, station = "station", value = "missing"),
    "column"
  )
})


test_that("alea_batch_fit rejects unsupported distributions and methods", {
  dat <- data.frame(
    station = rep("A", 20),
    value = rnorm(20)
  )
  
  expect_error(
    alea_batch_fit(dat, station = "station", value = "value", distributions = "lp3"),
    "Unsupported distributions"
  )
  
  expect_error(
    alea_batch_fit(dat, station = "station", value = "value", methods = "pwm"),
    "Unsupported methods"
  )
})


test_that("alea_batch_fit supports AI selection in batch mode", {
  set.seed(123)
  
  dat <- data.frame(
    station = rep(c("A", "B"), each = 40),
    value = c(rnorm(40, mean = 100, sd = 15), rnorm(40, mean = 120, sd = 20))
  )
  
  batch <- alea_batch_fit(
    data = dat,
    station = "station",
    value = "value",
    distributions = c("gev", "gpa", "pe3", "ln2", "ln3", "gum"),
    methods = c("lmom"),
    select = "ai"
  )
  
  expect_s3_class(batch, "alea_batch")
  
  selection <- alea_results(batch, "selection")
  expect_equal(nrow(selection), 2)
  expect_true(all(c(
    "station",
    "selected_distribution",
    "top_support",
    "top1_top2_margin",
    "decision_strength"
  ) %in% names(selection)))
  
  selected_models <- alea_results(batch, "selected_models")
  expect_equal(nrow(selected_models), 2)
  expect_true(all(selected_models$selected_distribution %in% c(
    "gev", "gpa", "pe3", "ln2", "ln3", "gum"
  )))
})


test_that("alea_batch_fit rejects ambiguous AI model inputs", {
  dat <- data.frame(
    station = rep("A", 20),
    value = rnorm(20)
  )
  
  fake_path <- tempfile(fileext = ".rds")
  saveRDS(list(), fake_path)
  
  expect_error(
    alea_batch_fit(
      data = dat,
      station = "station",
      value = "value",
      select = "ai",
      ai_model = list(),
      ai_model_path = fake_path
    ),
    "Only one of"
  )
})

test_that("alea_batch_fit can compute GOF and diagnostics tables", {
  set.seed(123)
  
  dat <- data.frame(
    station = rep(c("A", "B"), each = 40),
    year = rep(seq_len(40), times = 2),
    value = c(
      rnorm(40, mean = 100, sd = 15),
      rnorm(40, mean = 120, sd = 20)
    )
  )
  
  batch <- alea_batch_fit(
    data = dat,
    station = "station",
    time = "year",
    value = "value",
    distributions = c("gum"),
    methods = c("lmom"),
    gof = TRUE,
    diagnostics = TRUE
  )
  
  expect_s3_class(batch, "alea_batch")
  
  gof <- alea_results(batch, "gof")
  diagnostics <- alea_results(batch, "diagnostics")
  
  expect_true(nrow(gof) > 0)
  expect_true(nrow(diagnostics) > 0)
  
  expect_true(all(c(
    "station",
    "distribution",
    "method",
    "statistic",
    "estimate"
  ) %in% names(gof)))
  
  expect_true(all(c(
    "station",
    "distribution",
    "method",
    "diagnostic",
    "status",
    "message"
  ) %in% names(diagnostics)))
})

test_that("alea_batch_fit validates GOF and diagnostics flags", {
  dat <- data.frame(
    station = rep("A", 30),
    value = rnorm(30)
  )
  
  expect_error(
    alea_batch_fit(
      data = dat,
      station = "station",
      value = "value",
      gof = NA
    ),
    "`gof` must be"
  )
  
  expect_error(
    alea_batch_fit(
      data = dat,
      station = "station",
      value = "value",
      diagnostics = NA
    ),
    "`diagnostics` must be"
  )
})

test_that("alea_batch object has the final expected structure", {
  set.seed(123)
  
  dat <- data.frame(
    station = rep(c("A", "B"), each = 40),
    year = rep(seq_len(40), times = 2),
    value = c(
      rnorm(40, mean = 100, sd = 15),
      rnorm(40, mean = 120, sd = 20)
    )
  )
  
  batch <- alea_batch_fit(
    data = dat,
    station = "station",
    time = "year",
    value = "value",
    distributions = c("gum"),
    methods = c("lmom"),
    return_period = c(10, 50),
    gof = TRUE,
    diagnostics = TRUE
  )
  
  expect_s3_class(batch, "alea_batch")
  
  expected_names <- c(
    "stations",
    "fits",
    "fit_objects",
    "return_levels",
    "gof",
    "diagnostics",
    "selection",
    "selection_objects",
    "selected_models",
    "errors",
    "settings",
    "call"
  )
  
  expect_named(batch, expected_names)
  
  expect_true(is.data.frame(batch$stations))
  expect_true(is.data.frame(batch$fits))
  expect_true(is.list(batch$fit_objects))
  expect_true(is.data.frame(batch$return_levels))
  expect_true(is.data.frame(batch$gof))
  expect_true(is.data.frame(batch$diagnostics))
  expect_true(is.data.frame(batch$selection))
  expect_true(is.list(batch$selection_objects))
  expect_true(is.data.frame(batch$selected_models))
  expect_true(is.data.frame(batch$errors))
  expect_true(is.list(batch$settings))
})

test_that("alea_results extracts every final alea_batch component", {
  set.seed(123)
  
  dat <- data.frame(
    station = rep(c("A", "B"), each = 35),
    year = rep(seq_len(35), times = 2),
    value = c(
      rnorm(35, mean = 100, sd = 10),
      rnorm(35, mean = 130, sd = 12)
    )
  )
  
  batch <- alea_batch_fit(
    data = dat,
    station = "station",
    time = "year",
    value = "value",
    distributions = c("gum"),
    methods = c("lmom"),
    return_period = c(10),
    gof = TRUE,
    diagnostics = TRUE,
    select = "ai"
  )
  
  expect_s3_class(batch, "alea_batch")
  
  expect_true(is.data.frame(alea_results(batch, "stations")))
  expect_true(is.data.frame(alea_results(batch, "fits")))
  expect_true(is.list(alea_results(batch, "fit_objects")))
  expect_true(is.data.frame(alea_results(batch, "return_levels")))
  expect_true(is.data.frame(alea_results(batch, "gof")))
  expect_true(is.data.frame(alea_results(batch, "diagnostics")))
  expect_true(is.data.frame(alea_results(batch, "selection")))
  expect_true(is.list(alea_results(batch, "selection_objects")))
  expect_true(is.data.frame(alea_results(batch, "selected_models")))
  expect_true(is.data.frame(alea_results(batch, "errors")))
  
  expect_equal(length(alea_results(batch, "fit_objects")), 2)
  expect_equal(length(alea_results(batch, "selection_objects")), 2)
  
  expect_s3_class(alea_results(batch, "fit_objects")[[1]], "alea_fit")
  expect_s3_class(alea_results(batch, "selection_objects")[[1]], "alea_selection")
})

test_that("alea_batch_fit continues when one station has no finite data", {
  set.seed(123)
  
  dat <- data.frame(
    station = c(rep("good", 40), rep("bad", 20)),
    year = c(seq_len(40), seq_len(20)),
    value = c(rnorm(40, mean = 100, sd = 10), rep(NA_real_, 20))
  )
  
  batch <- alea_batch_fit(
    data = dat,
    station = "station",
    time = "year",
    value = "value",
    distributions = c("gum"),
    methods = c("lmom"),
    return_period = c(10),
    gof = TRUE,
    diagnostics = TRUE
  )
  
  expect_s3_class(batch, "alea_batch")
  
  stations <- alea_results(batch, "stations")
  fits <- alea_results(batch, "fits")
  return_levels <- alea_results(batch, "return_levels")
  errors <- alea_results(batch, "errors")
  
  expect_equal(nrow(stations), 2)
  
  expect_true("good" %in% stations$station)
  expect_true("bad" %in% stations$station)
  
  expect_true(any(fits$station == "good" & fits$status == "ok"))
  expect_false(any(fits$station == "bad" & fits$status == "ok"))
  
  expect_true(nrow(return_levels) > 0)
  
  expect_true(nrow(errors) >= 1)
  expect_true(any(errors$station == "bad"))
  expect_true(any(errors$step == "data"))
  expect_true(any(grepl("No finite observations", errors$message)))
})

test_that("alea_batch_fit records fit-level failures without stopping successful fits", {
  set.seed(123)
  
  dat <- data.frame(
    station = c(rep("good", 40), rep("constant", 40)),
    year = c(seq_len(40), seq_len(40)),
    value = c(rnorm(40, mean = 100, sd = 10), rep(100, 40))
  )
  
  batch <- alea_batch_fit(
    data = dat,
    station = "station",
    time = "year",
    value = "value",
    distributions = c("gum", "gev"),
    methods = c("lmom", "mle"),
    return_period = c(10),
    gof = TRUE,
    diagnostics = TRUE
  )
  
  expect_s3_class(batch, "alea_batch")
  
  fits <- alea_results(batch, "fits")
  errors <- alea_results(batch, "errors")
  
  expect_true(any(fits$station == "good" & fits$status == "ok"))
  
  expect_true(all(c(
    "station",
    "step",
    "distribution",
    "method",
    "message",
    "class"
  ) %in% names(errors)))
  
  if (nrow(errors) > 0) {
    expect_true(all(errors$step %in% c(
      "data",
      "fit",
      "return_level",
      "gof",
      "diagnostics",
      "selection"
    )))
  }
})

test_that("alea_batch selected_models table is consistent with AI selection and fits", {
  set.seed(123)
  
  dat <- data.frame(
    station = rep(c("A", "B"), each = 45),
    year = rep(seq_len(45), times = 2),
    value = c(
      rnorm(45, mean = 100, sd = 10),
      rnorm(45, mean = 120, sd = 15)
    )
  )
  
  batch <- alea_batch_fit(
    data = dat,
    station = "station",
    time = "year",
    value = "value",
    distributions = c("gev", "gpa", "pe3", "ln2", "ln3", "gum"),
    methods = c("lmom", "mle"),
    select = "ai",
    method_priority = c("lmom", "mle", "mom")
  )
  
  selection <- alea_results(batch, "selection")
  selected_models <- alea_results(batch, "selected_models")
  fits <- alea_results(batch, "fits")
  
  expect_equal(nrow(selection), 2)
  expect_equal(nrow(selected_models), 2)
  
  expect_true(all(selected_models$station %in% selection$station))
  expect_true(all(selected_models$selected_distribution %in% selection$selected_distribution))
  expect_true(all(selected_models$status == "ok"))
  
  for (i in seq_len(nrow(selected_models))) {
    row <- selected_models[i, ]
    
    matching_fit <- fits[
      fits$station == row$station &
        fits$distribution == row$selected_distribution &
        fits$method == row$selected_method &
        fits$status == "ok",
      ,
      drop = FALSE
    ]
    
    expect_equal(nrow(matching_fit), 1)
    expect_equal(matching_fit$fit_index[1], row$fit_index)
  }
})

test_that("alea_batch print and as.data.frame methods are stable", {
  set.seed(123)
  
  dat <- data.frame(
    station = rep(c("A", "B"), each = 30),
    value = c(
      rnorm(30, mean = 100, sd = 10),
      rnorm(30, mean = 120, sd = 10)
    )
  )
  
  batch <- alea_batch_fit(
    data = dat,
    station = "station",
    value = "value",
    distributions = "gum",
    methods = "lmom",
    gof = TRUE,
    diagnostics = TRUE
  )
  
  expect_output(print(batch), "ALEA-R batch analysis")
  expect_output(print(batch), "Stations:")
  expect_output(print(batch), "Successful fits:")
  
  df <- as.data.frame(batch)
  
  expect_true(is.data.frame(df))
  expect_identical(df, alea_results(batch, "fits"))
})

test_that("alea_batch stores empty errors with stable columns", {
  x <- c(10, 12, 15, 20, 25, 30, 40, 55, 70, 90)
  
  data <- data.frame(
    station = rep("A", length(x)),
    value = x
  )
  
  batch <- alea_batch_fit(
    data = data,
    station = "station",
    value = "value",
    distributions = "gum",
    methods = "lmom",
    quiet = TRUE
  )
  
  errors <- alea_results(batch, "errors")
  
  expect_s3_class(batch, "alea_batch")
  expect_s3_class(errors, "data.frame")
  expect_identical(
    names(errors),
    c("station", "step", "distribution", "method", "message", "class")
  )
  expect_equal(nrow(errors), 0L)
})

test_that("alea_batch errors table has stable columns when errors are present", {
  set.seed(123)
  
  dat <- data.frame(
    station = c(rep("good", 30), rep("bad", 20)),
    value = c(stats::rnorm(30, mean = 100, sd = 10), rep(NA_real_, 20))
  )
  
  batch <- alea_batch_fit(
    data = dat,
    station = "station",
    value = "value",
    distributions = "gum",
    methods = "lmom",
    quiet = TRUE
  )
  
  errors <- alea_results(batch, "errors")
  
  expect_s3_class(errors, "data.frame")
  expect_identical(
    names(errors),
    c("station", "step", "distribution", "method", "message", "class")
  )
  expect_true(nrow(errors) > 0L)
})
