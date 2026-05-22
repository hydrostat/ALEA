test_that("plot.alea_selection returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- stats::rnorm(80, mean = 100, sd = 15)
  
  selection <- alea_select(x)
  p <- plot(selection)
  
  expect_s3_class(p, "ggplot")
})


test_that("plot.alea_selection uses ranking data", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- stats::rnorm(80, mean = 100, sd = 15)
  
  selection <- alea_select(x)
  p <- plot(selection)
  
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), nrow(selection$ranking))
  expect_true(all(as.character(p$data$distribution) %in% selection$ranking$distribution))
})


test_that("plot.alea_selection works when decision data is missing", {
  skip_if_not_installed("ggplot2")
  
  selection <- list(
    selected_distribution = "gum",
    selected_method = NA_character_,
    selection_method = "fads_ai_light",
    ranking = data.frame(
      distribution = c("gum", "gev", "pe3"),
      probability = c(0.60, 0.25, 0.15),
      rank = c(1, 2, 3),
      selected = c(TRUE, FALSE, FALSE)
    ),
    decision = NULL,
    features = data.frame(),
    model_info = list(),
    warnings = character(),
    call = quote(alea_select(x))
  )
  
  class(selection) <- c("alea_selection", "list")
  
  p <- plot(selection)
  
  expect_s3_class(p, "ggplot")
})


test_that("plot.alea_selection validates object class", {
  skip_if_not_installed("ggplot2")
  
  bad <- list(
    ranking = data.frame(
      distribution = "gum",
      probability = 1,
      rank = 1,
      selected = TRUE
    )
  )
  
  expect_error(
    plot.alea_selection(bad),
    "`x` must be an object of class 'alea_selection'"
  )
})


test_that("plot.alea_selection validates ranking availability", {
  skip_if_not_installed("ggplot2")
  
  bad <- list(
    selected_distribution = "gum"
  )
  
  class(bad) <- c("alea_selection", "list")
  
  expect_error(
    plot(bad),
    "`x\\$ranking` is missing"
  )
})


test_that("plot.alea_selection validates required ranking columns", {
  skip_if_not_installed("ggplot2")
  
  bad <- list(
    ranking = data.frame(
      distribution = "gum",
      probability = 1,
      rank = 1
    )
  )
  
  class(bad) <- c("alea_selection", "list")
  
  expect_error(
    plot(bad),
    "missing required column"
  )
})


test_that("plot.alea_selection validates distribution column type", {
  skip_if_not_installed("ggplot2")
  
  bad <- list(
    ranking = data.frame(
      distribution = 1,
      probability = 1,
      rank = 1,
      selected = TRUE
    )
  )
  
  class(bad) <- c("alea_selection", "list")
  
  expect_error(
    plot(bad),
    "`distribution` column must be character"
  )
})


test_that("plot.alea_selection validates probability column type", {
  skip_if_not_installed("ggplot2")
  
  bad <- list(
    ranking = data.frame(
      distribution = "gum",
      probability = "1",
      rank = 1,
      selected = TRUE
    )
  )
  
  class(bad) <- c("alea_selection", "list")
  
  expect_error(
    plot(bad),
    "`probability` column must be numeric"
  )
})


test_that("plot.alea_selection validates rank column type", {
  skip_if_not_installed("ggplot2")
  
  bad <- list(
    ranking = data.frame(
      distribution = "gum",
      probability = 1,
      rank = "1",
      selected = TRUE
    )
  )
  
  class(bad) <- c("alea_selection", "list")
  
  expect_error(
    plot(bad),
    "`rank` column must be numeric or integer"
  )
})


test_that("plot.alea_selection validates selected column type", {
  skip_if_not_installed("ggplot2")
  
  bad <- list(
    ranking = data.frame(
      distribution = "gum",
      probability = 1,
      rank = 1,
      selected = "TRUE"
    )
  )
  
  class(bad) <- c("alea_selection", "list")
  
  expect_error(
    plot(bad),
    "`selected` column must be logical"
  )
})


test_that("plot.alea_selection rejects empty ranking tables", {
  skip_if_not_installed("ggplot2")
  
  bad <- list(
    ranking = data.frame(
      distribution = character(),
      probability = numeric(),
      rank = numeric(),
      selected = logical()
    )
  )
  
  class(bad) <- c("alea_selection", "list")
  
  expect_error(
    plot(bad),
    "At least one ranking row"
  )
})


test_that("plot.alea_selection validates finite probabilities", {
  skip_if_not_installed("ggplot2")
  
  bad <- list(
    ranking = data.frame(
      distribution = c("gum", "gev"),
      probability = c(0.5, NA_real_),
      rank = c(1, 2),
      selected = c(TRUE, FALSE)
    )
  )
  
  class(bad) <- c("alea_selection", "list")
  
  expect_error(
    plot(bad),
    "All `probability` values must be finite"
  )
})


test_that("plot.alea_selection validates probability range", {
  skip_if_not_installed("ggplot2")
  
  bad <- list(
    ranking = data.frame(
      distribution = c("gum", "gev"),
      probability = c(1.1, -0.1),
      rank = c(1, 2),
      selected = c(TRUE, FALSE)
    )
  )
  
  class(bad) <- c("alea_selection", "list")
  
  expect_error(
    plot(bad),
    "between 0 and 1"
  )
})


test_that("plot.alea_selection validates finite ranks", {
  skip_if_not_installed("ggplot2")
  
  bad <- list(
    ranking = data.frame(
      distribution = c("gum", "gev"),
      probability = c(0.6, 0.4),
      rank = c(1, NA_real_),
      selected = c(TRUE, FALSE)
    )
  )
  
  class(bad) <- c("alea_selection", "list")
  
  expect_error(
    plot(bad),
    "All `rank` values must be finite"
  )
})


test_that("plot.alea_selection validates positive ranks", {
  skip_if_not_installed("ggplot2")
  
  bad <- list(
    ranking = data.frame(
      distribution = c("gum", "gev"),
      probability = c(0.6, 0.4),
      rank = c(0, 1),
      selected = c(TRUE, FALSE)
    )
  )
  
  class(bad) <- c("alea_selection", "list")
  
  expect_error(
    plot(bad),
    "All `rank` values must be positive"
  )
})


test_that("plot.alea_selection orders ranking by rank", {
  skip_if_not_installed("ggplot2")
  
  selection <- list(
    selected_distribution = "gum",
    selected_method = NA_character_,
    selection_method = "fads_ai_light",
    ranking = data.frame(
      distribution = c("pe3", "gum", "gev"),
      probability = c(0.15, 0.60, 0.25),
      rank = c(3, 1, 2),
      selected = c(FALSE, TRUE, FALSE)
    ),
    decision = NULL,
    features = data.frame(),
    model_info = list(),
    warnings = character(),
    call = quote(alea_select(x))
  )
  
  class(selection) <- c("alea_selection", "list")
  
  p <- plot(selection)
  
  expect_s3_class(p, "ggplot")
  expect_equal(as.integer(p$data$rank), c(1L, 2L, 3L))
})


test_that("plot.alea_selection handles decision subtitle data", {
  skip_if_not_installed("ggplot2")
  
  selection <- list(
    selected_distribution = "gum",
    selected_method = NA_character_,
    selection_method = "fads_ai_light",
    ranking = data.frame(
      distribution = c("gum", "gev", "pe3"),
      probability = c(0.60, 0.25, 0.15),
      rank = c(1, 2, 3),
      selected = c(TRUE, FALSE, FALSE)
    ),
    decision = data.frame(
      row_id = "observed_sample",
      predicted_family = "gum",
      top_family = "gum",
      top_support = 0.60,
      second_family = "gev",
      second_support = 0.25,
      top1_top2_margin = 0.35,
      decision_strength = "moderate support",
      interpretation = "Example interpretation."
    ),
    features = data.frame(),
    model_info = list(),
    warnings = character(),
    call = quote(alea_select(x))
  )
  
  class(selection) <- c("alea_selection", "list")
  
  p <- plot(selection)
  
  expect_s3_class(p, "ggplot")
})


test_that("plot.alea_selection handles incomplete decision data", {
  skip_if_not_installed("ggplot2")
  
  selection <- list(
    selected_distribution = "gum",
    selected_method = NA_character_,
    selection_method = "fads_ai_light",
    ranking = data.frame(
      distribution = c("gum", "gev", "pe3"),
      probability = c(0.60, 0.25, 0.15),
      rank = c(1, 2, 3),
      selected = c(TRUE, FALSE, FALSE)
    ),
    decision = data.frame(
      top_family = "gum"
    ),
    features = data.frame(),
    model_info = list(),
    warnings = character(),
    call = quote(alea_select(x))
  )
  
  class(selection) <- c("alea_selection", "list")
  
  p <- plot(selection)
  
  expect_s3_class(p, "ggplot")
})