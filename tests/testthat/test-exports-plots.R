make_export_test_plot <- function() {
  ggplot2::ggplot(
    data.frame(x = 1:3, y = 1:3),
    ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::geom_point()
}


make_export_test_batch <- function() {
  set.seed(123)
  
  data <- data.frame(
    station = rep(c("A", "B"), each = 35),
    year = rep(seq_len(35), times = 2),
    value = c(
      stats::rnorm(35, mean = 100, sd = 15),
      stats::rnorm(35, mean = 130, sd = 20)
    )
  )
  
  alea_batch_fit(
    data = data,
    station = "station",
    value = "value",
    time = "year",
    distributions = "gum",
    methods = "lmom",
    return_period = c(2, 5, 10),
    gof = TRUE,
    diagnostics = TRUE,
    select = "none",
    quiet = TRUE
  )
}


test_that("alea_save_plot saves a ggplot object to supported file formats", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- stats::rnorm(60, mean = 100, sd = 15)
  
  fit <- alea_fit(x, distribution = "gum", method = "lmom")
  p <- plot(fit, type = "density")
  
  extensions <- c("pdf", "png", "tif", "tiff")
  
  for (extension in extensions) {
    filename <- tempfile(fileext = paste0(".", extension))
    
    result <- alea_save_plot(
      plot = p,
      filename = filename,
      width = 4,
      height = 3,
      dpi = 72
    )
    
    expect_true(file.exists(filename))
    expect_true(file.info(filename)$size > 0)
    expect_identical(result, normalizePath(filename, winslash = "/", mustWork = FALSE))
  }
})


test_that("alea_save_plot supports svg when svglite is installed", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("svglite")
  
  p <- make_export_test_plot()
  filename <- tempfile(fileext = ".svg")
  
  result <- alea_save_plot(
    plot = p,
    filename = filename,
    width = 4,
    height = 3,
    dpi = 72
  )
  
  expect_true(file.exists(filename))
  expect_true(file.info(filename)$size > 0)
  expect_identical(result, normalizePath(filename, winslash = "/", mustWork = FALSE))
})


test_that("alea_save_plot accepts svg as a supported extension", {
  skip_if_not_installed("ggplot2")
  
  p <- make_export_test_plot()
  
  expect_silent(
    validate_alea_save_plot_input(
      plot = p,
      filename = tempfile(fileext = ".svg"),
      width = 4,
      height = 3,
      units = "in",
      dpi = 72
    )
  )
})


test_that("alea_save_plot returns output path invisibly", {
  skip_if_not_installed("ggplot2")
  
  p <- make_export_test_plot()
  filename <- tempfile(fileext = ".png")
  
  result <- withVisible(
    alea_save_plot(
      plot = p,
      filename = filename,
      width = 4,
      height = 3,
      dpi = 72
    )
  )
  
  expect_false(result$visible)
  expect_identical(
    result$value,
    normalizePath(filename, winslash = "/", mustWork = FALSE)
  )
  expect_true(file.exists(filename))
})


test_that("alea_save_plot rejects non-ggplot objects", {
  skip_if_not_installed("ggplot2")
  
  filename <- tempfile(fileext = ".png")
  
  expect_error(
    alea_save_plot(
      plot = data.frame(x = 1:3),
      filename = filename
    ),
    "`plot` must be a ggplot object"
  )
})


test_that("alea_save_plot validates filename", {
  skip_if_not_installed("ggplot2")
  
  p <- make_export_test_plot()
  
  expect_error(
    alea_save_plot(plot = p, filename = NA_character_),
    "`filename` must be a non-missing character scalar"
  )
  
  expect_error(
    alea_save_plot(plot = p, filename = ""),
    "`filename` must not be empty"
  )
  
  expect_error(
    alea_save_plot(plot = p, filename = tempfile(fileext = ".jpg")),
    "`filename` must have one of these extensions"
  )
})


test_that("alea_save_plot validates output directory", {
  skip_if_not_installed("ggplot2")
  
  p <- make_export_test_plot()
  
  filename <- file.path(
    tempdir(),
    "directory-that-does-not-exist",
    "plot.png"
  )
  
  expect_error(
    alea_save_plot(plot = p, filename = filename),
    "Output directory does not exist"
  )
})


test_that("alea_save_plot validates size and resolution arguments", {
  skip_if_not_installed("ggplot2")
  
  p <- make_export_test_plot()
  filename <- tempfile(fileext = ".png")
  
  expect_error(
    alea_save_plot(plot = p, filename = filename, width = 0),
    "`width` must be a positive finite numeric scalar"
  )
  
  expect_error(
    alea_save_plot(plot = p, filename = filename, height = 0),
    "`height` must be a positive finite numeric scalar"
  )
  
  expect_error(
    alea_save_plot(plot = p, filename = filename, dpi = 0),
    "`dpi` must be a positive finite numeric scalar"
  )
})


test_that("alea_save_plot validates units", {
  skip_if_not_installed("ggplot2")
  
  p <- make_export_test_plot()
  filename <- tempfile(fileext = ".png")
  
  expect_error(
    alea_save_plot(plot = p, filename = filename, units = "meters"),
    "`units` must be one of"
  )
})


test_that("alea_save_plots saves a named list of ggplot objects", {
  skip_if_not_installed("ggplot2")
  
  plots <- list(
    density = make_export_test_plot(),
    return_level = make_export_test_plot()
  )
  
  directory <- tempfile()
  dir.create(directory)
  
  result <- alea_save_plots(
    plots = plots,
    directory = directory,
    extension = "png",
    width = 4,
    height = 3,
    dpi = 72
  )
  
  expected_files <- file.path(directory, c("density.png", "return_level.png"))
  
  expect_true(all(file.exists(expected_files)))
  expect_true(all(file.info(expected_files)$size > 0))
  expect_identical(
    unname(result),
    normalizePath(expected_files, winslash = "/", mustWork = FALSE)
  )
  expect_identical(names(result), c("density", "return_level"))
})


test_that("alea_save_plots saves unnamed plots using prefix", {
  skip_if_not_installed("ggplot2")
  
  plots <- list(
    make_export_test_plot(),
    make_export_test_plot()
  )
  
  directory <- tempfile()
  dir.create(directory)
  
  result <- alea_save_plots(
    plots = plots,
    directory = directory,
    prefix = "batch_plot",
    extension = "png",
    width = 4,
    height = 3,
    dpi = 72
  )
  
  expected_files <- file.path(directory, c("batch_plot_1.png", "batch_plot_2.png"))
  
  expect_true(all(file.exists(expected_files)))
  expect_identical(
    unname(result),
    normalizePath(expected_files, winslash = "/", mustWork = FALSE)
  )
  expect_identical(names(result), c("batch_plot_1", "batch_plot_2"))
})


test_that("alea_save_plots returns output paths invisibly", {
  skip_if_not_installed("ggplot2")
  
  plots <- list(first = make_export_test_plot())
  
  directory <- tempfile()
  dir.create(directory)
  
  result <- withVisible(
    alea_save_plots(
      plots = plots,
      directory = directory,
      extension = "png",
      width = 4,
      height = 3,
      dpi = 72
    )
  )
  
  expected_file <- file.path(directory, "first.png")
  
  expect_false(result$visible)
  expect_identical(
    unname(result$value),
    normalizePath(expected_file, winslash = "/", mustWork = FALSE)
  )
  expect_true(file.exists(expected_file))
})


test_that("alea_save_plots protects existing files unless overwrite is TRUE", {
  skip_if_not_installed("ggplot2")
  
  plots <- list(first = make_export_test_plot())
  
  directory <- tempfile()
  dir.create(directory)
  
  first_file <- file.path(directory, "first.png")
  file.create(first_file)
  
  expect_error(
    alea_save_plots(
      plots = plots,
      directory = directory,
      extension = "png",
      width = 4,
      height = 3,
      dpi = 72
    ),
    "already exist and `overwrite = FALSE`"
  )
  
  expect_silent(
    alea_save_plots(
      plots = plots,
      directory = directory,
      extension = "png",
      width = 4,
      height = 3,
      dpi = 72,
      overwrite = TRUE
    )
  )
  
  expect_true(file.exists(first_file))
  expect_true(file.info(first_file)$size > 0)
})


test_that("alea_save_plots rejects invalid plots input", {
  skip_if_not_installed("ggplot2")
  
  directory <- tempfile()
  dir.create(directory)
  
  expect_error(
    alea_save_plots(
      plots = list(),
      directory = directory
    ),
    "`plots` must be a non-empty list of ggplot objects"
  )
  
  expect_error(
    alea_save_plots(
      plots = list(good = make_export_test_plot(), bad = data.frame(x = 1)),
      directory = directory
    ),
    "All elements of `plots` must be ggplot objects"
  )
})


test_that("alea_save_plots validates directory", {
  skip_if_not_installed("ggplot2")
  
  plots <- list(first = make_export_test_plot())
  
  expect_error(
    alea_save_plots(
      plots = plots,
      directory = NA_character_
    ),
    "`directory` must be a non-missing character scalar"
  )
  
  expect_error(
    alea_save_plots(
      plots = plots,
      directory = ""
    ),
    "`directory` must not be empty"
  )
  
  expect_error(
    alea_save_plots(
      plots = plots,
      directory = file.path(tempdir(), "missing-directory")
    ),
    "Output directory does not exist"
  )
})


test_that("alea_save_plots validates prefix and extension", {
  skip_if_not_installed("ggplot2")
  
  plots <- list(make_export_test_plot())
  
  directory <- tempfile()
  dir.create(directory)
  
  expect_error(
    alea_save_plots(
      plots = plots,
      directory = directory,
      prefix = ""
    ),
    "`prefix` must be a non-empty character scalar"
  )
  
  expect_error(
    alea_save_plots(
      plots = plots,
      directory = directory,
      extension = NA_character_
    ),
    "`extension` must be a non-missing character scalar"
  )
  
  expect_error(
    alea_save_plots(
      plots = plots,
      directory = directory,
      extension = "jpg"
    ),
    "`extension` must be one of"
  )
})


test_that("alea_save_plots validates size, units, dpi, and overwrite", {
  skip_if_not_installed("ggplot2")
  
  plots <- list(first = make_export_test_plot())
  
  directory <- tempfile()
  dir.create(directory)
  
  expect_error(
    alea_save_plots(
      plots = plots,
      directory = directory,
      width = 0
    ),
    "`width` must be a positive finite numeric scalar"
  )
  
  expect_error(
    alea_save_plots(
      plots = plots,
      directory = directory,
      height = 0
    ),
    "`height` must be a positive finite numeric scalar"
  )
  
  expect_error(
    alea_save_plots(
      plots = plots,
      directory = directory,
      units = "meters"
    ),
    "`units` must be one of"
  )
  
  expect_error(
    alea_save_plots(
      plots = plots,
      directory = directory,
      dpi = 0
    ),
    "`dpi` must be a positive finite numeric scalar"
  )
  
  expect_error(
    alea_save_plots(
      plots = plots,
      directory = directory,
      overwrite = NA
    ),
    "`overwrite` must be a non-missing logical scalar"
  )
})


test_that("alea_save_plots makes safe filenames from plot names", {
  skip_if_not_installed("ggplot2")
  
  plots <- list(
    "Density plot" = make_export_test_plot(),
    "Return/level plot!" = make_export_test_plot()
  )
  
  directory <- tempfile()
  dir.create(directory)
  
  result <- alea_save_plots(
    plots = plots,
    directory = directory,
    extension = "png",
    width = 4,
    height = 3,
    dpi = 72
  )
  
  expected_names <- c("Density_plot", "Return_level_plot")
  expected_files <- file.path(directory, paste0(expected_names, ".png"))
  
  expect_true(all(file.exists(expected_files)))
  expect_identical(names(result), expected_names)
})


test_that("alea_save_plots rejects names that produce duplicate filenames", {
  skip_if_not_installed("ggplot2")
  
  plots <- list(
    "a b" = make_export_test_plot(),
    "a/b" = make_export_test_plot()
  )
  
  directory <- tempfile()
  dir.create(directory)
  
  expect_error(
    alea_save_plots(
      plots = plots,
      directory = directory,
      extension = "png",
      width = 4,
      height = 3,
      dpi = 72
    ),
    "unique output filenames"
  )
})


test_that("alea_save_plots accepts svg as a supported extension", {
  skip_if_not_installed("ggplot2")
  
  plots <- list(first = make_export_test_plot())
  
  directory <- tempfile()
  dir.create(directory)
  
  expect_silent(
    validate_alea_save_plots_input(
      plots = plots,
      directory = directory,
      prefix = "plot",
      extension = "svg",
      width = 4,
      height = 3,
      units = "in",
      dpi = 72,
      overwrite = FALSE
    )
  )
})


test_that("alea_save_plots saves svg when svglite is installed", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("svglite")
  
  plots <- list(first = make_export_test_plot())
  
  directory <- tempfile()
  dir.create(directory)
  
  result <- alea_save_plots(
    plots = plots,
    directory = directory,
    extension = "svg",
    width = 4,
    height = 3,
    dpi = 72
  )
  
  expected_file <- file.path(directory, "first.svg")
  
  expect_true(file.exists(expected_file))
  expect_true(file.info(expected_file)$size > 0)
  expect_identical(
    unname(result),
    normalizePath(expected_file, winslash = "/", mustWork = FALSE)
  )
})


test_that("alea_export saves a ggplot object", {
  skip_if_not_installed("ggplot2")
  
  p <- make_export_test_plot()
  filename <- tempfile(fileext = ".png")
  
  result <- alea_export(
    object = p,
    path = filename,
    width = 4,
    height = 3,
    dpi = 72
  )
  
  expect_true(file.exists(filename))
  expect_true(file.info(filename)$size > 0)
  expect_identical(result, normalizePath(filename, winslash = "/", mustWork = FALSE))
})


test_that("alea_export saves a list of ggplot objects", {
  skip_if_not_installed("ggplot2")
  
  plots <- list(
    first = make_export_test_plot(),
    second = make_export_test_plot()
  )
  
  directory <- tempfile()
  dir.create(directory)
  
  result <- alea_export(
    object = plots,
    path = directory,
    extension = "png",
    width = 4,
    height = 3,
    dpi = 72
  )
  
  expected_files <- file.path(directory, c("first.png", "second.png"))
  
  expect_true(all(file.exists(expected_files)))
  expect_identical(
    unname(result),
    normalizePath(expected_files, winslash = "/", mustWork = FALSE)
  )
})


test_that("alea_export saves a data frame as CSV", {
  data <- data.frame(
    station = c("A", "B"),
    value = c(100, 120)
  )
  
  filename <- tempfile(fileext = ".csv")
  
  result <- alea_export(
    object = data,
    path = filename
  )
  
  expect_true(file.exists(filename))
  expect_true(file.info(filename)$size > 0)
  expect_identical(result, normalizePath(filename, winslash = "/", mustWork = FALSE))
  
  imported <- utils::read.csv(filename)
  expect_equal(imported$station, data$station)
  expect_equal(imported$value, data$value)
})


test_that("alea_export saves selected alea_batch tables", {
  batch <- make_export_test_batch()
  
  directory <- tempfile()
  dir.create(directory)
  
  result <- alea_export(
    object = batch,
    path = directory,
    type = c("stations", "fits")
  )
  
  expected_files <- file.path(directory, c("stations.csv", "fits.csv"))
  
  expect_true(all(file.exists(expected_files)))
  expect_identical(
    unname(result),
    normalizePath(expected_files, winslash = "/", mustWork = FALSE)
  )
  expect_identical(names(result), c("stations", "fits"))
})


test_that("alea_export saves all non-empty alea_batch tables", {
  batch <- make_export_test_batch()
  
  directory <- tempfile()
  dir.create(directory)
  
  result <- alea_export(
    object = batch,
    path = directory,
    type = "all"
  )
  
  expect_true(length(result) >= 4L)
  expect_true(all(file.exists(unname(result))))
  expect_true("stations" %in% names(result))
  expect_true("fits" %in% names(result))
  expect_true("return_levels" %in% names(result))
  expect_true("gof" %in% names(result))
  expect_true("diagnostics" %in% names(result))
})


test_that("alea_export validates path and overwrite", {
  p <- make_export_test_plot()
  
  expect_error(
    alea_export(
      object = p,
      path = NA_character_
    ),
    "`path` must be a non-missing character scalar"
  )
  
  expect_error(
    alea_export(
      object = p,
      path = ""
    ),
    "`path` must not be empty"
  )
  
  expect_error(
    alea_export(
      object = p,
      path = tempfile(fileext = ".png"),
      overwrite = NA
    ),
    "`overwrite` must be a non-missing logical scalar"
  )
})


test_that("alea_export rejects unsupported objects", {
  expect_error(
    alea_export(
      object = 1:3,
      path = tempfile(fileext = ".csv")
    ),
    "`object` must be an alea_batch object"
  )
})


test_that("alea_export protects existing plot files unless overwrite is TRUE", {
  skip_if_not_installed("ggplot2")
  
  p <- make_export_test_plot()
  filename <- tempfile(fileext = ".png")
  
  file.create(filename)
  
  expect_error(
    alea_export(
      object = p,
      path = filename,
      width = 4,
      height = 3,
      dpi = 72
    ),
    "already exists and `overwrite = FALSE`"
  )
  
  expect_silent(
    alea_export(
      object = p,
      path = filename,
      width = 4,
      height = 3,
      dpi = 72,
      overwrite = TRUE
    )
  )
  
  expect_true(file.exists(filename))
  expect_true(file.info(filename)$size > 0)
})


test_that("alea_export protects existing data-frame CSV files unless overwrite is TRUE", {
  data <- data.frame(x = 1:3)
  filename <- tempfile(fileext = ".csv")
  
  file.create(filename)
  
  expect_error(
    alea_export(
      object = data,
      path = filename
    ),
    "already exists and `overwrite = FALSE`"
  )
  
  expect_silent(
    alea_export(
      object = data,
      path = filename,
      overwrite = TRUE
    )
  )
  
  expect_true(file.exists(filename))
  expect_true(file.info(filename)$size > 0)
})


test_that("alea_export requires csv path for data frames", {
  data <- data.frame(x = 1:3)
  
  expect_error(
    alea_export(
      object = data,
      path = tempfile(fileext = ".txt")
    ),
    "Data-frame exports require a `.csv` file path"
  )
})


test_that("alea_export validates output directory for data frames", {
  data <- data.frame(x = 1:3)
  
  filename <- file.path(
    tempdir(),
    "missing-directory",
    "table.csv"
  )
  
  expect_error(
    alea_export(
      object = data,
      path = filename
    ),
    "Output directory does not exist"
  )
})


test_that("alea_export validates batch export directory", {
  batch <- make_export_test_batch()
  
  expect_error(
    alea_export(
      object = batch,
      path = file.path(tempdir(), "missing-directory")
    ),
    "Output directory does not exist"
  )
})


test_that("alea_export validates batch export type", {
  batch <- make_export_test_batch()
  
  directory <- tempfile()
  dir.create(directory)
  
  expect_error(
    alea_export(
      object = batch,
      path = directory,
      type = NA_character_
    ),
    "`type` must be a non-missing character vector"
  )
  
  expect_error(
    alea_export(
      object = batch,
      path = directory,
      type = "unsupported"
    ),
    "`type` contains unsupported value"
  )
})


test_that("alea_export protects existing batch CSV files unless overwrite is TRUE", {
  batch <- make_export_test_batch()
  
  directory <- tempfile()
  dir.create(directory)
  
  stations_file <- file.path(directory, "stations.csv")
  file.create(stations_file)
  
  expect_error(
    alea_export(
      object = batch,
      path = directory,
      type = "stations"
    ),
    "already exists and `overwrite = FALSE`"
  )
  
  expect_silent(
    alea_export(
      object = batch,
      path = directory,
      type = "stations",
      overwrite = TRUE
    )
  )
  
  expect_true(file.exists(stations_file))
  expect_true(file.info(stations_file)$size > 0)
})


test_that("alea_export reports when no non-empty batch tables are available", {
  batch <- list(
    stations = data.frame(),
    fits = data.frame(),
    return_levels = data.frame(),
    gof = data.frame(),
    diagnostics = data.frame(),
    selection = data.frame(),
    selected_models = data.frame(),
    errors = data.frame()
  )
  
  class(batch) <- c("alea_batch", "list")
  
  directory <- tempfile()
  dir.create(directory)
  
  expect_error(
    alea_export(
      object = batch,
      path = directory,
      type = "all"
    ),
    "No non-empty exportable batch tables"
  )
})