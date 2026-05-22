# Plot export helpers

#' Save an ALEA plot
#'
#' Saves a `ggplot` object to disk using `ggplot2::ggsave()`.
#'
#' @param plot A `ggplot` object.
#' @param filename Character scalar. Output file path.
#' @param width Numeric scalar. Plot width.
#' @param height Numeric scalar. Plot height.
#' @param units Character scalar. Units passed to `ggplot2::ggsave()`.
#' @param dpi Numeric scalar. Resolution passed to `ggplot2::ggsave()`.
#' @param ... Additional arguments passed to `ggplot2::ggsave()`.
#'
#' @return The normalized output path, invisibly.
#'
#' @export
alea_save_plot <- function(
    plot,
    filename,
    width = 7,
    height = 5,
    units = "in",
    dpi = 300,
    ...
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required to save ALEA plots.", call. = FALSE)
  }
  
  validate_alea_save_plot_input(
    plot = plot,
    filename = filename,
    width = width,
    height = height,
    units = units,
    dpi = dpi
  )
  
  directory <- dirname(filename)
  
  if (!dir.exists(directory)) {
    stop("Output directory does not exist: ", directory, call. = FALSE)
  }
  
  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    ...
  )
  
  invisible(normalizePath(filename, winslash = "/", mustWork = FALSE))
}


#' Save multiple ALEA plots
#'
#' Saves a list of `ggplot` objects to disk using `alea_save_plot()`.
#'
#' @param plots A list of `ggplot` objects.
#' @param directory Character scalar. Output directory.
#' @param prefix Character scalar. Prefix used for unnamed plots.
#' @param extension Character scalar. Output file extension. One of `"pdf"`,
#'   `"png"`, `"tif"`, `"tiff"`, or `"svg"`.
#' @param width Numeric scalar. Plot width.
#' @param height Numeric scalar. Plot height.
#' @param units Character scalar. Units passed to `ggplot2::ggsave()`.
#' @param dpi Numeric scalar. Resolution passed to `ggplot2::ggsave()`.
#' @param overwrite Logical scalar. If `FALSE`, existing files are not
#'   overwritten.
#' @param ... Additional arguments passed to `alea_save_plot()`.
#'
#' @return A character vector with the normalized output paths, invisibly.
#'
#' @export
alea_save_plots <- function(
    plots,
    directory,
    prefix = "alea_plot",
    extension = "png",
    width = 7,
    height = 5,
    units = "in",
    dpi = 300,
    overwrite = FALSE,
    ...
) {
  validate_alea_save_plots_input(
    plots = plots,
    directory = directory,
    prefix = prefix,
    extension = extension,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    overwrite = overwrite
  )
  
  plot_names <- names(plots)
  
  if (is.null(plot_names)) {
    plot_names <- rep("", length(plots))
  }
  
  empty_names <- !nzchar(plot_names)
  plot_names[empty_names] <- paste0(prefix, "_", seq_along(plots)[empty_names])
  
  plot_names <- make_safe_plot_filenames(plot_names)
  
  filenames <- file.path(
    directory,
    paste0(plot_names, ".", tolower(extension))
  )
  
  duplicated_filenames <- duplicated(filenames) | duplicated(filenames, fromLast = TRUE)
  
  if (any(duplicated_filenames)) {
    stop("Plot names must produce unique output filenames.", call. = FALSE)
  }
  
  existing_files <- file.exists(filenames)
  
  if (any(existing_files) && !isTRUE(overwrite)) {
    stop(
      "Output file(s) already exist and `overwrite = FALSE`: ",
      paste(basename(filenames[existing_files]), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  output_paths <- character(length(plots))
  
  for (i in seq_along(plots)) {
    output_paths[i] <- alea_save_plot(
      plot = plots[[i]],
      filename = filenames[i],
      width = width,
      height = height,
      units = units,
      dpi = dpi,
      ...
    )
  }
  
  names(output_paths) <- plot_names
  
  invisible(output_paths)
}

#' Export ALEA objects
#'
#' Exports ALEA objects to disk.
#'
#' Currently supported objects are:
#' - `ggplot` objects, saved with `alea_save_plot()`;
#' - lists of `ggplot` objects, saved with `alea_save_plots()`;
#' - `alea_batch` objects, exported as CSV tables;
#' - data frames, exported as a CSV table.
#'
#' @param object Object to export.
#' @param path Character scalar. Output file path for single-file exports or
#'   output directory for multi-file exports.
#' @param type Character vector. For `alea_batch` objects, result tables to
#'   export. Use `"all"` to export all supported non-empty tables.
#' @param overwrite Logical scalar. If `FALSE`, existing files are not
#'   overwritten.
#' @param ... Additional arguments passed to lower-level export helpers.
#'
#' @return A character vector with normalized output paths, invisibly.
#'
#' @export
alea_export <- function(
    object,
    path,
    type = "all",
    overwrite = FALSE,
    ...
) {
  validate_alea_export_common_input(
    path = path,
    overwrite = overwrite
  )
  
  if (inherits(object, "alea_batch")) {
    return(alea_export_batch(
      object = object,
      directory = path,
      type = type,
      overwrite = overwrite,
      ...
    ))
  }
  
  if (inherits(object, "ggplot")) {
    return(alea_export_plot(
      object = object,
      filename = path,
      overwrite = overwrite,
      ...
    ))
  }
  
  if (is.list(object) && all(vapply(object, inherits, logical(1L), what = "ggplot"))) {
    return(alea_save_plots(
      plots = object,
      directory = path,
      overwrite = overwrite,
      ...
    ))
  }
  
  if (is.data.frame(object)) {
    return(alea_export_data_frame(
      object = object,
      filename = path,
      overwrite = overwrite,
      ...
    ))
  }
  
  stop(
    "`object` must be an alea_batch object, a ggplot object, a list of ggplot objects, or a data frame.",
    call. = FALSE
  )
}


validate_alea_export_common_input <- function(path, overwrite) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    stop("`path` must be a non-missing character scalar.", call. = FALSE)
  }
  
  if (!nzchar(path)) {
    stop("`path` must not be empty.", call. = FALSE)
  }
  
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("`overwrite` must be a non-missing logical scalar.", call. = FALSE)
  }
  
  invisible(TRUE)
}


alea_export_plot <- function(object, filename, overwrite, ...) {
  if (file.exists(filename) && !isTRUE(overwrite)) {
    stop(
      "Output file already exists and `overwrite = FALSE`: ",
      basename(filename),
      ".",
      call. = FALSE
    )
  }
  
  path <- alea_save_plot(
    plot = object,
    filename = filename,
    ...
  )
  
  invisible(path)
}


alea_export_data_frame <- function(object, filename, overwrite, row.names = FALSE, ...) {
  validate_alea_export_csv_filename(filename)
  
  directory <- dirname(filename)
  
  if (!dir.exists(directory)) {
    stop("Output directory does not exist: ", directory, call. = FALSE)
  }
  
  if (file.exists(filename) && !isTRUE(overwrite)) {
    stop(
      "Output file already exists and `overwrite = FALSE`: ",
      basename(filename),
      ".",
      call. = FALSE
    )
  }
  
  utils::write.csv(
    x = object,
    file = filename,
    row.names = row.names,
    ...
  )
  
  invisible(normalizePath(filename, winslash = "/", mustWork = FALSE))
}


alea_export_batch <- function(object, directory, type = "all", overwrite = FALSE, ...) {
  if (!dir.exists(directory)) {
    stop("Output directory does not exist: ", directory, call. = FALSE)
  }
  
  types <- resolve_alea_batch_export_types(type)
  
  output_paths <- character(0L)
  
  for (current_type in types) {
    table <- object[[current_type]]
    
    if (!is_exportable_batch_table(table)) {
      next
    }
    
    filename <- file.path(directory, paste0(current_type, ".csv"))
    
    if (file.exists(filename) && !isTRUE(overwrite)) {
      stop(
        "Output file already exists and `overwrite = FALSE`: ",
        basename(filename),
        ".",
        call. = FALSE
      )
    }
    
    utils::write.csv(
      x = as.data.frame(table),
      file = filename,
      row.names = FALSE,
      ...
    )
    
    output_paths[current_type] <- normalizePath(
      filename,
      winslash = "/",
      mustWork = FALSE
    )
  }
  
  if (length(output_paths) < 1L) {
    stop("No non-empty exportable batch tables were found.", call. = FALSE)
  }
  
  invisible(output_paths)
}


resolve_alea_batch_export_types <- function(type) {
  supported_types <- c(
    "stations",
    "fits",
    "return_levels",
    "gof",
    "diagnostics",
    "selection",
    "selected_models",
    "errors"
  )
  
  if (!is.character(type) || length(type) < 1L || anyNA(type)) {
    stop("`type` must be a non-missing character vector.", call. = FALSE)
  }
  
  if (length(type) == 1L && identical(type, "all")) {
    return(supported_types)
  }
  
  invalid_types <- setdiff(type, supported_types)
  
  if (length(invalid_types) > 0L) {
    stop(
      "`type` contains unsupported value(s): ",
      paste(invalid_types, collapse = ", "),
      ". Supported values are: all, ",
      paste(supported_types, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  unique(type)
}


is_exportable_batch_table <- function(x) {
  if (is.null(x)) {
    return(FALSE)
  }
  
  x <- as.data.frame(x)
  
  nrow(x) > 0L && ncol(x) > 0L
}


validate_alea_export_csv_filename <- function(filename) {
  if (!is.character(filename) || length(filename) != 1L || is.na(filename)) {
    stop("`path` must be a non-missing character scalar.", call. = FALSE)
  }
  
  if (!nzchar(filename)) {
    stop("`path` must not be empty.", call. = FALSE)
  }
  
  extension <- tolower(tools::file_ext(filename))
  
  if (!identical(extension, "csv")) {
    stop("Data-frame exports require a `.csv` file path.", call. = FALSE)
  }
  
  invisible(TRUE)
}

validate_alea_save_plot_input <- function(plot, filename, width, height, units, dpi) {
  if (!inherits(plot, "ggplot")) {
    stop("`plot` must be a ggplot object.", call. = FALSE)
  }
  
  if (!is.character(filename) || length(filename) != 1L || is.na(filename)) {
    stop("`filename` must be a non-missing character scalar.", call. = FALSE)
  }
  
  if (!nzchar(filename)) {
    stop("`filename` must not be empty.", call. = FALSE)
  }
  
  extension <- tolower(tools::file_ext(filename))
  allowed_extensions <- c("pdf", "png", "tif", "tiff", "svg")
  
  if (!extension %in% allowed_extensions) {
    stop(
      "`filename` must have one of these extensions: ",
      paste(allowed_extensions, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if (!is.numeric(width) || length(width) != 1L || !is.finite(width) || width <= 0) {
    stop("`width` must be a positive finite numeric scalar.", call. = FALSE)
  }
  
  if (!is.numeric(height) || length(height) != 1L || !is.finite(height) || height <= 0) {
    stop("`height` must be a positive finite numeric scalar.", call. = FALSE)
  }
  
  allowed_units <- c("in", "cm", "mm", "px")
  
  if (!is.character(units) || length(units) != 1L || is.na(units)) {
    stop("`units` must be a non-missing character scalar.", call. = FALSE)
  }
  
  if (!units %in% allowed_units) {
    stop(
      "`units` must be one of: ",
      paste(allowed_units, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if (!is.numeric(dpi) || length(dpi) != 1L || !is.finite(dpi) || dpi <= 0) {
    stop("`dpi` must be a positive finite numeric scalar.", call. = FALSE)
  }
  
  invisible(TRUE)
}


validate_alea_save_plots_input <- function(
    plots,
    directory,
    prefix,
    extension,
    width,
    height,
    units,
    dpi,
    overwrite
) {
  if (!is.list(plots) || length(plots) < 1L) {
    stop("`plots` must be a non-empty list of ggplot objects.", call. = FALSE)
  }
  
  is_plot <- vapply(plots, inherits, logical(1L), what = "ggplot")
  
  if (!all(is_plot)) {
    stop("All elements of `plots` must be ggplot objects.", call. = FALSE)
  }
  
  if (!is.character(directory) || length(directory) != 1L || is.na(directory)) {
    stop("`directory` must be a non-missing character scalar.", call. = FALSE)
  }
  
  if (!nzchar(directory)) {
    stop("`directory` must not be empty.", call. = FALSE)
  }
  
  if (!dir.exists(directory)) {
    stop("Output directory does not exist: ", directory, call. = FALSE)
  }
  
  if (!is.character(prefix) || length(prefix) != 1L || is.na(prefix) || !nzchar(prefix)) {
    stop("`prefix` must be a non-empty character scalar.", call. = FALSE)
  }
  
  if (!is.character(extension) || length(extension) != 1L || is.na(extension)) {
    stop("`extension` must be a non-missing character scalar.", call. = FALSE)
  }
  
  extension <- tolower(extension)
  allowed_extensions <- c("pdf", "png", "tif", "tiff", "svg")
  
  if (!extension %in% allowed_extensions) {
    stop(
      "`extension` must be one of: ",
      paste(allowed_extensions, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if (!is.numeric(width) || length(width) != 1L || !is.finite(width) || width <= 0) {
    stop("`width` must be a positive finite numeric scalar.", call. = FALSE)
  }
  
  if (!is.numeric(height) || length(height) != 1L || !is.finite(height) || height <= 0) {
    stop("`height` must be a positive finite numeric scalar.", call. = FALSE)
  }
  
  allowed_units <- c("in", "cm", "mm", "px")
  
  if (!is.character(units) || length(units) != 1L || is.na(units)) {
    stop("`units` must be a non-missing character scalar.", call. = FALSE)
  }
  
  if (!units %in% allowed_units) {
    stop(
      "`units` must be one of: ",
      paste(allowed_units, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if (!is.numeric(dpi) || length(dpi) != 1L || !is.finite(dpi) || dpi <= 0) {
    stop("`dpi` must be a positive finite numeric scalar.", call. = FALSE)
  }
  
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("`overwrite` must be a non-missing logical scalar.", call. = FALSE)
  }
  
  invisible(TRUE)
}


make_safe_plot_filenames <- function(x) {
  x <- trimws(as.character(x))
  x[!nzchar(x)] <- "alea_plot"
  
  x <- gsub("[^A-Za-z0-9_-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  
  x[!nzchar(x)] <- "alea_plot"
  
  x
}