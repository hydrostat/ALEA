#' Batch frequency analysis for multiple stations
#'
#' Fits one or more probability distributions to multiple stations or sites.
#' Batch processing is robust: failures for one station, distribution, method,
#' quantile, goodness-of-fit, diagnostics, or optional AI-selection step are
#' recorded in the errors table and do not stop the full workflow.
#'
#' @param data A data frame containing station and value columns.
#' @param station Character scalar. Name of the station/site identifier column.
#' @param value Character scalar. Name of the numeric value column.
#' @param time Optional character scalar. Name of the time column.
#' @param distributions Character vector of distributions to fit.
#' @param methods Character vector of estimation methods to use.
#' @param return_period Optional numeric vector of return periods.
#' @param gof Logical. If `TRUE`, computes goodness-of-fit tables for each
#'   successful fitted model.
#' @param diagnostics Logical. If `TRUE`, computes diagnostics tables for each
#'   successful fitted model.
#' @param select Selection workflow. Use `"none"` or `"ai"`.
#' @param ai_model Optional pre-loaded FADS_AI light model object.
#' @param ai_model_path Optional path to a FADS_AI light model file.
#' @param method_priority Character vector used to choose the preferred fitted
#'   method when AI selects a distribution.
#' @param quiet Logical. If `TRUE`, suppresses non-essential messages.
#' @param ... Additional arguments passed to fitting or selection helpers.
#'
#' @return An object of class `alea_batch`.
#' @examples
#' data <- data.frame(
#'   station = rep(c("A", "B"), each = 10),
#'   year = rep(2001:2010, times = 2),
#'   value = c(42, 39, 51, 47, 62, 55, 50, 58, 60, 46,
#'             50, 47, 61, 56, 75, 66, 60, 70, 72, 55)
#' )
#' batch <- alea_batch_fit(
#'   data,
#'   station = "station",
#'   time = "year",
#'   value = "value",
#'   distributions = c("gev", "gum"),
#'   methods = "lmom",
#'   return_period = c(10, 25),
#'   gof = TRUE
#' )
#' batch
#' alea_results(batch, "quantiles")
#'
#' @export
alea_batch_fit <- function(
    data,
    station,
    value,
    time = NULL,
    distributions = c("gev", "gpa", "pe3", "ln2", "ln3", "gum"),
    methods = c("lmom"),
    return_period = NULL,
    gof = FALSE,
    diagnostics = FALSE,
    select = c("none", "ai"),
    ai_model = NULL,
    ai_model_path = NULL,
    method_priority = c("lmom", "mle", "mom"),
    quiet = FALSE,
    ...
) {
  call <- match.call()
  select <- match.arg(select)
  
  validate_batch_input(
    data = data,
    station = station,
    value = value,
    time = time,
    distributions = distributions,
    methods = methods,
    return_period = return_period,
    gof = gof,
    diagnostics = diagnostics,
    select = select,
    ai_model = ai_model,
    ai_model_path = ai_model_path,
    method_priority = method_priority,
    quiet = quiet
  )
  
  ai_model_object <- prepare_batch_ai_model(
    select = select,
    ai_model = ai_model,
    ai_model_path = ai_model_path
  )
  
  station_ids <- unique(as.character(data[[station]]))
  
  stations <- list()
  fits <- list()
  fit_objects <- list()
  quantiles <- list()
  gof_results <- list()
  diagnostics_results <- list()
  selection <- list()
  selection_objects <- list()
  selected_models <- list()
  errors <- list()
  
  fit_index <- 0L
  selection_index <- 0L
  
  for (station_id in station_ids) {
    station_data <- data[as.character(data[[station]]) == station_id, , drop = FALSE]
    x_raw <- station_data[[value]]
    x <- as.numeric(x_raw)
    x_finite <- x[is.finite(x)]
    
    stations[[length(stations) + 1L]] <- make_batch_station_row(
      station_id = station_id,
      x = x,
      x_finite = x_finite,
      time_values = if (!is.null(time)) station_data[[time]] else NULL
    )
    
    if (length(x_finite) == 0L) {
      errors[[length(errors) + 1L]] <- make_batch_error_row(
        station = station_id,
        step = "data",
        distribution = NA_character_,
        method = NA_character_,
        message = "No finite observations are available for this station.",
        class = "alea_batch_data_error"
      )
      next
    }
    
    if (select == "ai") {
      ai_result <- tryCatch(
        alea_select(
          x_finite,
          model = ai_model_object,
          sample_id = station_id,
          param_id = station_id,
          quiet = TRUE,
          ...
        ),
        error = function(e) e
      )
      
      if (inherits(ai_result, "error")) {
        errors[[length(errors) + 1L]] <- make_batch_error_row(
          station = station_id,
          step = "selection",
          distribution = NA_character_,
          method = NA_character_,
          message = conditionMessage(ai_result),
          class = class(ai_result)[1L]
        )
      } else {
        selection_index <- selection_index + 1L
        selection_objects[[selection_index]] <- ai_result
        
        selection[[length(selection) + 1L]] <- make_batch_selection_row(
          station_id = station_id,
          selection = ai_result,
          selection_index = selection_index
        )
      }
    }
    
    for (distribution in distributions) {
      for (method in methods) {
        fit_result <- tryCatch(
          alea_fit(
            x_finite,
            distribution = distribution,
            method = method,
            ...
          ),
          error = function(e) e
        )
        
        if (inherits(fit_result, "error")) {
          errors[[length(errors) + 1L]] <- make_batch_error_row(
            station = station_id,
            step = "fit",
            distribution = distribution,
            method = method,
            message = conditionMessage(fit_result),
            class = class(fit_result)[1L]
          )
          
          fits[[length(fits) + 1L]] <- make_batch_fit_row(
            station_id = station_id,
            distribution = distribution,
            method = method,
            status = "error",
            n = length(x_finite),
            fit_index = NA_integer_
          )
          
          next
        }
        
        fit_index <- fit_index + 1L
        fit_objects[[fit_index]] <- fit_result
        
        fits[[length(fits) + 1L]] <- make_batch_fit_row(
          station_id = station_id,
          distribution = distribution,
          method = method,
          status = "ok",
          n = length(x_finite),
          fit_index = fit_index
        )
        
        if (!is.null(return_period)) {
          rl_result <- tryCatch(
            alea_quantile(fit_result, return_period = return_period),
            error = function(e) e
          )
          
          if (inherits(rl_result, "error")) {
            errors[[length(errors) + 1L]] <- make_batch_error_row(
              station = station_id,
              step = "quantile",
              distribution = distribution,
              method = method,
              message = conditionMessage(rl_result),
              class = class(rl_result)[1L]
            )
          } else {
            rl_df <- as.data.frame(rl_result)
            rl_df <- cbind(
              data.frame(station = station_id, stringsAsFactors = FALSE),
              rl_df
            )
            quantiles[[length(quantiles) + 1L]] <- rl_df
          }
        }
        
        if (isTRUE(gof)) {
          gof_result <- tryCatch(
            alea_gof(fit_result),
            error = function(e) e
          )
          
          if (inherits(gof_result, "error")) {
            errors[[length(errors) + 1L]] <- make_batch_error_row(
              station = station_id,
              step = "gof",
              distribution = distribution,
              method = method,
              message = conditionMessage(gof_result),
              class = class(gof_result)[1L]
            )
          } else {
            gof_df <- as.data.frame(gof_result)
            gof_df <- cbind(
              data.frame(station = station_id, stringsAsFactors = FALSE),
              gof_df
            )
            gof_results[[length(gof_results) + 1L]] <- gof_df
          }
        }
        
        if (isTRUE(diagnostics)) {
          diagnostics_result <- tryCatch(
            alea_diagnostics(fit_result),
            error = function(e) e
          )
          
          if (inherits(diagnostics_result, "error")) {
            errors[[length(errors) + 1L]] <- make_batch_error_row(
              station = station_id,
              step = "diagnostics",
              distribution = distribution,
              method = method,
              message = conditionMessage(diagnostics_result),
              class = class(diagnostics_result)[1L]
            )
          } else {
            diagnostics_df <- as.data.frame(diagnostics_result)
            diagnostics_df <- cbind(
              data.frame(station = station_id, stringsAsFactors = FALSE),
              diagnostics_df
            )
            diagnostics_results[[length(diagnostics_results) + 1L]] <- diagnostics_df
          }
        }
      }
    }
    
    if (select == "ai") {
      station_selected <- make_batch_selected_model_row(
        station_id = station_id,
        selection_rows = selection,
        fits_rows = fits,
        method_priority = method_priority
      )
      
      if (!is.null(station_selected)) {
        selected_models[[length(selected_models) + 1L]] <- station_selected
      }
    }
  }
  
  object <- list(
    stations = order_alea_model_table_internal(bind_batch_rows(stations)),
    fits = order_alea_model_table_internal(bind_batch_rows(fits)),
    fit_objects = fit_objects,
    quantiles = order_alea_model_table_internal(bind_batch_rows(quantiles)),
    gof = order_alea_model_table_internal(bind_batch_rows(gof_results)),
    diagnostics = order_alea_model_table_internal(bind_batch_rows(diagnostics_results)),
    selection = order_alea_model_table_internal(bind_batch_rows(selection)),
    selection_objects = selection_objects,
    selected_models = order_alea_model_table_internal(bind_batch_rows(selected_models)),
    errors = order_alea_model_table_internal(bind_batch_error_rows(errors)),
    settings = list(
      station = station,
      value = value,
      time = time,
      distributions = distributions,
      methods = methods,
      return_period = return_period,
      gof = gof,
      diagnostics = diagnostics,
      select = select,
      method_priority = method_priority
    ),
    call = call
  )
  
  new_alea_batch(object)
}


#' Extract results from an ALEA batch object
#'
#' Extracts station metadata, fit summaries, quantiles, goodness-of-fit tables,
#' diagnostics, AI-selection summaries, selected models, stored fit objects, or
#' structured errors from an `alea_batch` object.
#'
#' @param object An object of class `alea_batch`.
#' @param type Character scalar. Result component to extract. Supported values
#'   are `"stations"`, `"fits"`, `"fit_objects"`, `"quantiles"`, `"gof"`,
#'   `"diagnostics"`, `"selection"`, `"selection_objects"`,
#'   `"selected_models"`, and `"errors"`.
#' @param ... Additional arguments.
#'
#' @return A data frame with a lightweight S3 class for compact console
#'   printing, or a list for object-list components. Use `as.data.frame()` on
#'   returned tables to access the complete underlying data frame.
#'
#' @details
#' Tables returned by `alea_results()` are ordered for reporting. Quantile,
#' goodness-of-fit, diagnostics, and error tables are ordered by station,
#' distribution, and estimation method, followed by the relevant result field.
#'
#' @examples
#' data <- data.frame(
#'   station = rep(c("A", "B"), each = 10),
#'   value = c(42, 39, 51, 47, 62, 55, 50, 58, 60, 46,
#'             50, 47, 61, 56, 75, 66, 60, 70, 72, 55)
#' )
#' batch <- alea_batch_fit(
#'   data, station = "station", value = "value",
#'   distributions = c("gev", "gum"),
#'   methods = "lmom",
#'   return_period = c(10, 25),
#'   gof = TRUE
#' )
#' alea_results(batch, "quantiles")
#' as.data.frame(alea_results(batch, "quantiles"))
#'
#' @export
alea_results <- function(
    object,
    type = c(
      "stations",
      "fits",
      "fit_objects",
      "quantiles",
      "gof",
      "diagnostics",
      "selection",
      "selection_objects",
      "selected_models",
      "errors"
    ),
    ...
) {
  if (!inherits(object, "alea_batch")) {
    stop("`object` must be an object of class 'alea_batch'.", call. = FALSE)
  }

  type <- match.arg(type)

  result <- switch(
    type,
    stations = order_alea_model_table_internal(object$stations),
    fits = order_alea_model_table_internal(object$fits),
    fit_objects = object$fit_objects,
    quantiles = order_alea_model_table_internal(object$quantiles),
    gof = order_alea_model_table_internal(object$gof),
    diagnostics = order_alea_model_table_internal(object$diagnostics),
    selection = order_alea_model_table_internal(object$selection),
    selection_objects = object$selection_objects,
    selected_models = order_alea_model_table_internal(object$selected_models),
    errors = order_alea_model_table_internal(normalize_batch_errors(object$errors))
  )

  if (is.data.frame(result)) {
    result <- new_alea_batch_result(result, type)
  }

  result
}


new_alea_batch_result <- function(x, type) {
  if (!is.data.frame(x)) {
    return(x)
  }

  type_class <- switch(
    type,
    stations = "alea_batch_stations",
    fits = "alea_batch_fits",
    quantiles = "alea_batch_quantiles",
    gof = "alea_batch_gof",
    diagnostics = "alea_batch_diagnostics",
    selection = "alea_batch_selection",
    selected_models = "alea_batch_selected_models",
    errors = "alea_batch_errors",
    paste0("alea_batch_", type)
  )

  class(x) <- unique(c(type_class, "alea_batch_result", class(x)))
  x
}


#' @export
as.data.frame.alea_batch_result <- function(x, ...) {
  class(x) <- setdiff(class(x), c(
    "alea_batch_stations",
    "alea_batch_fits",
    "alea_batch_quantiles",
    "alea_batch_gof",
    "alea_batch_diagnostics",
    "alea_batch_selection",
    "alea_batch_selected_models",
    "alea_batch_errors",
    "alea_batch_result"
  ))

  as.data.frame(x, ...)
}


#' @export
print.alea_batch_result <- function(x, digits = 4, max_rows = 20, ...) {
  type <- batch_result_type(x)
  df <- as.data.frame(x)

  switch(
    type,
    stations = print_alea_batch_stations(df, digits = digits, max_rows = max_rows),
    fits = print_alea_batch_fits(df, digits = digits, max_rows = max_rows),
    quantiles = print_alea_batch_quantiles(df, digits = digits, max_rows = max_rows),
    gof = print_alea_batch_gof(df, digits = digits, max_rows = max_rows),
    diagnostics = print_alea_batch_diagnostics(df, digits = digits, max_rows = max_rows),
    selection = print_alea_batch_selection(df, digits = digits, max_rows = max_rows),
    selected_models = print_alea_batch_selected_models(df, digits = digits, max_rows = max_rows),
    errors = print_alea_batch_errors(df, digits = digits, max_rows = max_rows),
    print.data.frame(df, row.names = FALSE, ...)
  )

  invisible(x)
}


batch_result_type <- function(x) {
  cls <- class(x)

  if ("alea_batch_stations" %in% cls) {
    return("stations")
  }
  if ("alea_batch_fits" %in% cls) {
    return("fits")
  }
  if ("alea_batch_quantiles" %in% cls) {
    return("quantiles")
  }
  if ("alea_batch_gof" %in% cls) {
    return("gof")
  }
  if ("alea_batch_diagnostics" %in% cls) {
    return("diagnostics")
  }
  if ("alea_batch_selection" %in% cls) {
    return("selection")
  }
  if ("alea_batch_selected_models" %in% cls) {
    return("selected_models")
  }
  if ("alea_batch_errors" %in% cls) {
    return("errors")
  }

  "unknown"
}



print_empty_batch_result <- function(x, title, full_table_label) {
  cat(title, "\n", sep = "")
  cat("Rows: 0\n\n")
  print.data.frame(as.data.frame(x), row.names = FALSE)
  cat("\nUse as.data.frame(x) for the full ", full_table_label, ".\n", sep = "")
  invisible(x)
}


print_alea_batch_stations <- function(x, digits = 4, max_rows = 20) {
  if (nrow(x) == 0L) {
    return(print_empty_batch_result(x, "ALEA batch stations", "station table"))
  }

  cat("ALEA batch stations\n")
  cat("Stations:", nrow(x), "\n\n")

  cols <- intersect(
    c("station", "n", "n_valid", "n_missing", "min_value", "max_value", "first_time", "last_time"),
    names(x)
  )

  out <- x[, cols, drop = FALSE]
  out <- round_numeric_columns(out, digits = digits)
  print_batch_compact_table(out, max_rows = max_rows)
  cat("\nUse as.data.frame(x) for the full station table.\n")
}


print_alea_batch_fits <- function(x, digits = 4, max_rows = 20) {
  if (nrow(x) == 0L) {
    return(print_empty_batch_result(x, "ALEA batch fit summary", "fit table"))
  }

  cat("ALEA batch fit summary\n")
  cat("Stations:", count_unique_non_missing(x$station), "\n")
  cat("Distributions:", collapse_unique(x$distribution), "\n")
  cat("Methods:", collapse_unique(x$method), "\n")
  cat("Rows:", nrow(x), "\n\n")

  cols <- intersect(c("station", "distribution", "method", "status", "n", "fit_index"), names(x))
  out <- x[, cols, drop = FALSE]
  print_batch_compact_table(out, max_rows = max_rows)
  cat("\nUse as.data.frame(x) for the full fit table.\n")
}


print_alea_batch_quantiles <- function(x, digits = 4, max_rows = 20) {
  if (nrow(x) == 0L) {
    return(print_empty_batch_result(x, "ALEA batch quantiles", "quantile table"))
  }

  cat("ALEA batch quantiles\n")
  cat("Stations:", count_unique_non_missing(x$station), "\n")
  cat("Distributions:", collapse_unique(x$distribution), "\n")
  cat("Methods:", collapse_unique(x$method), "\n")
  cat("Rows:", nrow(x), "\n\n")

  cols <- intersect(c("station", "distribution", "method", "return_period", "quantile"), names(x))
  out <- x[, cols, drop = FALSE]
  out <- round_numeric_columns(out, digits = digits)
  print_batch_compact_table(out, max_rows = max_rows)
  cat("\nUse as.data.frame(x) for the full quantile table.\n")
}


print_alea_batch_gof <- function(x, digits = 4, max_rows = 20) {
  if (nrow(x) == 0L) {
    return(print_empty_batch_result(x, "ALEA batch goodness-of-fit results", "goodness-of-fit table"))
  }

  cat("ALEA batch goodness-of-fit results\n")
  cat("Stations:", count_unique_non_missing(x$station), "\n")
  cat("Distributions:", collapse_unique(x$distribution), "\n")
  cat("Methods:", collapse_unique(x$method), "\n")
  cat("Rows:", nrow(x), "\n\n")

  out <- data.frame(
    station = x$station,
    distribution = x$distribution,
    method = x$method,
    statistic = x$statistic,
    estimate = round(x$estimate, digits),
    better = ifelse(isTRUE(x$higher_is_better), "higher", "lower"),
    stringsAsFactors = FALSE
  )

  if (length(x$higher_is_better) > 1L) {
    out$better <- ifelse(x$higher_is_better, "higher", "lower")
  }

  print_batch_compact_table(out, max_rows = max_rows)
  cat("\nCalibrated p-values are not implemented.\n")
  cat("Use as.data.frame(x) for the full goodness-of-fit table.\n")
}


print_alea_batch_diagnostics <- function(x, digits = 4, max_rows = 20) {
  if (nrow(x) == 0L) {
    return(print_empty_batch_result(x, "ALEA batch diagnostics", "diagnostics table"))
  }

  cat("ALEA batch diagnostics\n")
  cat("Stations:", count_unique_non_missing(x$station), "\n")
  cat("Distributions:", collapse_unique(x$distribution), "\n")
  cat("Methods:", collapse_unique(x$method), "\n")
  cat("Rows:", nrow(x), "\n\n")

  cols <- intersect(
    c("station", "distribution", "method", "diagnostic", "value", "p_value", "status", "reject"),
    names(x)
  )
  out <- x[, cols, drop = FALSE]
  out <- round_numeric_columns(out, digits = digits)
  print_batch_compact_table(out, max_rows = max_rows)
  cat("\nUse as.data.frame(x) for the full diagnostics table.\n")
}


print_alea_batch_selection <- function(x, digits = 4, max_rows = 20) {
  if (nrow(x) == 0L) {
    cat("ALEA batch AI selection\n")
    cat("Rows: 0\n\n")
    cat("No AI-selection results are available.\n")
    return(invisible(x))
  }

  cat("ALEA batch AI selection\n")
  cat("Stations:", count_unique_non_missing(x$station), "\n")
  cat("Rows:", nrow(x), "\n\n")

  cols <- intersect(
    c("station", "selected_distribution", "top_support", "second_family", "second_support", "top1_top2_margin", "decision_strength"),
    names(x)
  )
  out <- x[, cols, drop = FALSE]
  out <- round_numeric_columns(out, digits = digits)
  print_batch_compact_table(out, max_rows = max_rows)
  cat("\nUse as.data.frame(x) for the full AI-selection table.\n")
}


print_alea_batch_selected_models <- function(x, digits = 4, max_rows = 20) {
  if (nrow(x) == 0L) {
    cat("ALEA batch selected models\n")
    cat("Rows: 0\n\n")
    cat("No selected-model results are available.\n")
    return(invisible(x))
  }

  cat("ALEA batch selected models\n")
  cat("Stations:", count_unique_non_missing(x$station), "\n")
  cat("Rows:", nrow(x), "\n\n")

  cols <- intersect(
    c("station", "selected_distribution", "selected_method", "fit_index", "selection_index", "status"),
    names(x)
  )
  out <- x[, cols, drop = FALSE]
  print_batch_compact_table(out, max_rows = max_rows)
  cat("\nUse as.data.frame(x) for the full selected-model table.\n")
}


print_alea_batch_errors <- function(x, digits = 4, max_rows = 20) {
  cat("ALEA batch errors\n")
  cat("Rows:", nrow(x), "\n\n")

  if (nrow(x) == 0L) {
    cat("No errors were recorded.\n")
    return(invisible(x))
  }

  cols <- intersect(c("station", "step", "distribution", "method", "message", "class"), names(x))
  out <- x[, cols, drop = FALSE]
  print_batch_compact_table(out, max_rows = max_rows)
  cat("\nUse as.data.frame(x) for the full error table.\n")
}


print_batch_compact_table <- function(x, max_rows = 20) {
  if (nrow(x) == 0L) {
    print.data.frame(x, row.names = FALSE)
    return(invisible(x))
  }

  out <- compact_rows_for_print(x, max_rows = max_rows)
  print.data.frame(out, row.names = FALSE)

  if (nrow(x) > max_rows) {
    cat("\nShowing first and last rows of", nrow(x), "total rows.\n")
  }

  invisible(x)
}


compact_rows_for_print <- function(x, max_rows = 20) {
  if (nrow(x) <= max_rows) {
    return(x)
  }

  n_head <- max(1L, floor(max_rows / 2))
  n_tail <- max(1L, max_rows - n_head)

  ellipsis <- as.data.frame(
    as.list(rep("...", ncol(x))),
    stringsAsFactors = FALSE
  )
  names(ellipsis) <- names(x)

  rbind(
    utils::head(x, n_head),
    ellipsis,
    utils::tail(x, n_tail)
  )
}


round_numeric_columns <- function(x, digits = 4) {
  for (nm in names(x)) {
    if (is.numeric(x[[nm]])) {
      x[[nm]] <- round(x[[nm]], digits)
    }
  }

  x
}


collapse_unique <- function(x) {
  x <- unique(as.character(x[!is.na(x)]))

  if (length(x) == 0L) {
    return("none")
  }

  paste(x, collapse = ", ")
}


count_unique_non_missing <- function(x) {
  length(unique(x[!is.na(x)]))
}


#' @export
print.alea_batch <- function(x, ...) {
  n_stations <- nrow(x$stations)
  n_fits <- sum(x$fits$status == "ok", na.rm = TRUE)
  n_fit_errors <- sum(x$errors$step == "fit", na.rm = TRUE)
  n_errors <- nrow(x$errors)
  
  cat("ALEA-R batch analysis\n")
  cat("Stations:", n_stations, "\n")
  cat("Successful fits:", n_fits, "\n")
  cat("Fit errors:", n_fit_errors, "\n")
  cat("Total recorded errors:", n_errors, "\n")
  
  if (!is.null(x$settings$gof) && isTRUE(x$settings$gof)) {
    cat("GOF rows:", nrow(x$gof), "\n")
  }
  
  if (!is.null(x$settings$diagnostics) && isTRUE(x$settings$diagnostics)) {
    cat("Diagnostics rows:", nrow(x$diagnostics), "\n")
  }
  
  if (!is.null(x$settings$select) && identical(x$settings$select, "ai")) {
    cat("Selection:", "ai", "\n")
    cat("Selected models:", nrow(x$selected_models), "\n")
  }
  
  invisible(x)
}


#' @export
as.data.frame.alea_batch <- function(x, ...) {
  order_alea_model_table_internal(x$fits)
}


new_alea_batch <- function(x) {
  class(x) <- c("alea_batch", "list")
  validate_alea_batch(x)
}


validate_alea_batch <- function(x) {
  required <- c(
    "stations",
    "fits",
    "fit_objects",
    "quantiles",
    "gof",
    "diagnostics",
    "selection",
    "selection_objects",
    "selected_models",
    "errors",
    "settings",
    "call"
  )
  
  missing <- setdiff(required, names(x))
  if (length(missing) > 0L) {
    stop(
      "Invalid 'alea_batch' object. Missing fields: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  x$errors <- normalize_batch_errors(x$errors)
  
  x
}


validate_batch_input <- function(
    data,
    station,
    value,
    time,
    distributions,
    methods,
    return_period,
    gof,
    diagnostics,
    select,
    ai_model,
    ai_model_path,
    method_priority,
    quiet
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  
  check_string_column <- function(column, argument) {
    if (!is.character(column) || length(column) != 1L || is.na(column)) {
      stop("`", argument, "` must be a character scalar.", call. = FALSE)
    }
    
    if (!column %in% names(data)) {
      stop(
        "`", argument, "` must identify a column in `data`.",
        call. = FALSE
      )
    }
    
    invisible(TRUE)
  }
  
  check_string_column(station, "station")
  check_string_column(value, "value")
  
  if (!is.null(time)) {
    check_string_column(time, "time")
  }
  
  if (!is.numeric(data[[value]]) && !is.integer(data[[value]])) {
    stop("`value` must identify a numeric column in `data`.", call. = FALSE)
  }
  
  allowed_distributions <- c("gev", "gpa", "pe3", "ln2", "ln3", "gum")
  invalid_distributions <- setdiff(distributions, allowed_distributions)
  
  if (length(invalid_distributions) > 0L) {
    stop(
      "Unsupported distributions: ",
      paste(invalid_distributions, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  allowed_methods <- c("mom", "lmom", "mle")
  invalid_methods <- setdiff(methods, allowed_methods)
  
  if (length(invalid_methods) > 0L) {
    stop(
      "Unsupported methods: ",
      paste(invalid_methods, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if (!is.null(return_period)) {
    if (
      !is.numeric(return_period) ||
      any(!is.finite(return_period)) ||
      any(return_period <= 1)
    ) {
      stop(
        "`return_period` must be a numeric vector with values greater than 1.",
        call. = FALSE
      )
    }
  }
  
  if (!is.logical(gof) || length(gof) != 1L || is.na(gof)) {
    stop("`gof` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  
  if (!is.logical(diagnostics) || length(diagnostics) != 1L || is.na(diagnostics)) {
    stop("`diagnostics` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  
  if (!identical(select, "none") && !identical(select, "ai")) {
    stop("`select` must be either 'none' or 'ai'.", call. = FALSE)
  }
  
  if (!is.null(ai_model) && !is.null(ai_model_path)) {
    stop(
      "Only one of `ai_model` or `ai_model_path` may be supplied.",
      call. = FALSE
    )
  }
  
  if (!is.null(ai_model_path)) {
    if (!is.character(ai_model_path) || length(ai_model_path) != 1L ||
        is.na(ai_model_path)) {
      stop("`ai_model_path` must be a character scalar.", call. = FALSE)
    }
    
    if (!file.exists(ai_model_path)) {
      stop("`ai_model_path` does not exist.", call. = FALSE)
    }
  }
  
  if (!is.character(method_priority) || length(method_priority) == 0L) {
    stop("`method_priority` must be a non-empty character vector.", call. = FALSE)
  }
  
  invalid_priority <- setdiff(method_priority, allowed_methods)
  
  if (length(invalid_priority) > 0L) {
    stop(
      "`method_priority` contains unsupported methods: ",
      paste(invalid_priority, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet)) {
    stop("`quiet` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  
  invisible(TRUE)
}


prepare_batch_ai_model <- function(select, ai_model, ai_model_path) {
  if (!identical(select, "ai")) {
    return(NULL)
  }
  
  if (!is.null(ai_model)) {
    return(ai_model)
  }
  
  if (!is.null(ai_model_path)) {
    return(readRDS(ai_model_path))
  }
  
  NULL
}


make_batch_station_row <- function(station_id, x, x_finite, time_values = NULL) {
  data.frame(
    station = station_id,
    n = length(x),
    n_valid = length(x_finite),
    n_missing = sum(!is.finite(x)),
    min_value = if (length(x_finite) > 0L) min(x_finite) else NA_real_,
    max_value = if (length(x_finite) > 0L) max(x_finite) else NA_real_,
    first_time = if (!is.null(time_values)) as.character(min(time_values, na.rm = TRUE)) else NA_character_,
    last_time = if (!is.null(time_values)) as.character(max(time_values, na.rm = TRUE)) else NA_character_,
    stringsAsFactors = FALSE
  )
}


make_batch_fit_row <- function(
    station_id,
    distribution,
    method,
    status,
    n,
    fit_index
) {
  data.frame(
    station = station_id,
    distribution = distribution,
    method = method,
    status = status,
    n = n,
    fit_index = fit_index,
    stringsAsFactors = FALSE
  )
}


make_batch_selection_row <- function(station_id, selection, selection_index) {
  decision <- selection$decision
  
  data.frame(
    station = station_id,
    selected_distribution = selection$selected_distribution,
    selection_method = selection$selection_method,
    top_family = decision$top_family[1L],
    top_support = decision$top_support[1L],
    second_family = decision$second_family[1L],
    second_support = decision$second_support[1L],
    top1_top2_margin = decision$top1_top2_margin[1L],
    decision_strength = decision$decision_strength[1L],
    selection_index = selection_index,
    stringsAsFactors = FALSE
  )
}


make_batch_selected_model_row <- function(
    station_id,
    selection_rows,
    fits_rows,
    method_priority
) {
  selection_table <- bind_batch_rows(selection_rows)
  fits_table <- bind_batch_rows(fits_rows)
  
  if (nrow(selection_table) == 0L || nrow(fits_table) == 0L) {
    return(NULL)
  }
  
  station_selection <- selection_table[
    selection_table$station == station_id,
    ,
    drop = FALSE
  ]
  
  if (nrow(station_selection) == 0L) {
    return(NULL)
  }
  
  selected_distribution <- station_selection$selected_distribution[1L]
  
  station_fits <- fits_table[
    fits_table$station == station_id &
      fits_table$distribution == selected_distribution &
      fits_table$status == "ok",
    ,
    drop = FALSE
  ]
  
  if (nrow(station_fits) == 0L) {
    return(data.frame(
      station = station_id,
      selected_distribution = selected_distribution,
      selected_method = NA_character_,
      fit_index = NA_integer_,
      selection_index = station_selection$selection_index[1L],
      status = "selected_distribution_not_fitted",
      stringsAsFactors = FALSE
    ))
  }
  
  priority <- match(station_fits$method, method_priority)
  priority[is.na(priority)] <- length(method_priority) + 1L
  station_fits <- station_fits[order(priority), , drop = FALSE]
  
  data.frame(
    station = station_id,
    selected_distribution = selected_distribution,
    selected_method = station_fits$method[1L],
    fit_index = station_fits$fit_index[1L],
    selection_index = station_selection$selection_index[1L],
    status = "ok",
    stringsAsFactors = FALSE
  )
}


make_batch_error_row <- function(
    station,
    step,
    distribution,
    method,
    message,
    class
) {
  data.frame(
    station = station,
    step = step,
    distribution = distribution,
    method = method,
    message = message,
    class = class,
    stringsAsFactors = FALSE
  )
}


bind_batch_rows <- function(rows) {
  if (length(rows) == 0L) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  
  all_names <- unique(unlist(lapply(rows, names), use.names = FALSE))
  
  rows <- lapply(rows, function(row) {
    missing <- setdiff(all_names, names(row))
    
    for (name in missing) {
      row[[name]] <- NA
    }
    
    row[all_names]
  })
  
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

empty_batch_errors <- function() {
  out <- data.frame(
    station = character(),
    step = character(),
    distribution = character(),
    method = character(),
    message = character(),
    class = character(),
    stringsAsFactors = FALSE
  )
  
  out[, batch_error_columns(), drop = FALSE]
}


bind_batch_error_rows <- function(rows) {
  if (length(rows) == 0L) {
    return(empty_batch_errors())
  }
  
  normalize_batch_errors(bind_batch_rows(rows))
}


normalize_batch_errors <- function(errors) {
  if (is.null(errors)) {
    return(empty_batch_errors())
  }
  
  errors <- as.data.frame(errors, stringsAsFactors = FALSE)
  required_columns <- batch_error_columns()
  
  if (nrow(errors) == 0L) {
    return(empty_batch_errors())
  }
  
  missing_columns <- setdiff(required_columns, names(errors))
  
  for (column in missing_columns) {
    errors[[column]] <- NA_character_
  }
  
  errors <- errors[, required_columns, drop = FALSE]
  
  for (column in required_columns) {
    errors[[column]] <- as.character(errors[[column]])
  }
  
  rownames(errors) <- NULL
  errors
}


batch_error_columns <- function() {
  c("station", "step", "distribution", "method", "message", "class")
}
