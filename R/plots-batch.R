# Plot methods for ALEA batch objects

#' Plot ALEA batch-analysis results
#'
#' Produces summary plots from an `alea_batch` object.
#'
#' @param x An object of class `alea_batch`.
#' @param type Character scalar. Plot type. One of `"selected_models"`,
#'   `"quantiles"`, `"gof"`, or `"diagnostics"`.
#' @param statistic Character scalar. Goodness-of-fit statistic used when
#'   `type = "gof"`.
#' @param diagnostic Character scalar or `NULL`. Diagnostic used when
#'   `type = "diagnostics"`. If `NULL`, all diagnostics are summarized by
#'   status.
#' @param return_period_scale Character scalar. Return-period axis scale when
#'   `type = "quantiles"`. One of `"gumbel"`, `"log"`, or `"linear"`.
#' @param plot_observed Logical scalar. If `TRUE`, observed plotting-position
#'   points are added to batch quantile plots.
#' @param plotting_position_a Numeric scalar. Plotting-position parameter `a`
#'   used for observed quantile points, with plotting positions
#'   `p_i = (i - a) / (n + 1 - 2 * a)` and empirical return periods
#'   `T_i = 1 / (1 - p_i)`.
#' @param ... Additional arguments passed to methods.
#'
#' @return A `ggplot` object.
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
#' plot(batch, type = "quantiles")
#' plot(batch, type = "quantiles", plot_observed = FALSE)
#'
#' @method plot alea_batch
#' @export
plot.alea_batch <- function(
    x,
    type = c("selected_models", "quantiles", "gof", "diagnostics"),
    statistic = "aic",
    diagnostic = NULL,
    return_period_scale = c("gumbel", "log", "linear"),
    plot_observed = TRUE,
    plotting_position_a = 0.44,
    ...
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required to plot ALEA objects.", call. = FALSE)
  }
  
  type <- match.arg(type)
  return_period_scale <- match.arg(return_period_scale)
  
  validate_batch_plot_object(x)
  
  if (type == "selected_models") {
    return(plot_alea_batch_selected_models(x))
  }
  
  if (type == "quantiles") {
    return(plot_alea_batch_quantiles(
      x,
      return_period_scale = return_period_scale,
      plot_observed = plot_observed,
      plotting_position_a = plotting_position_a
    ))
  }
  
  if (type == "gof") {
    return(plot_alea_batch_gof(x, statistic = statistic))
  }
  
  if (type == "diagnostics") {
    return(plot_alea_batch_diagnostics(x, diagnostic = diagnostic))
  }
  
  stop("Unsupported batch plot type.", call. = FALSE)
}


validate_batch_plot_object <- function(x) {
  if (!inherits(x, "alea_batch")) {
    stop("`x` must be an object of class 'alea_batch'.", call. = FALSE)
  }
  
  invisible(TRUE)
}

plot_alea_batch_selected_models <- function(x) {
  data <- validate_batch_selected_models_plot_data(x$selected_models)
  
  max_count <- max(as.numeric(table(data$distribution)), na.rm = TRUE)
  
  ggplot2::ggplot(
    data,
    ggplot2::aes(x = distribution)
  ) +
    ggplot2::geom_bar(
      width = 0.72,
      fill = alea_plot_color("primary"),
      alpha = 0.9
    ) +
    ggplot2::scale_x_discrete(
      labels = alea_plot_distribution_label
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, max_count, by = 1)
    ) +
    ggplot2::labs(
      title = "Batch selected models",
      subtitle = "Number of stations by selected distribution",
      x = "Selected distribution",
      y = "Number of stations"
    ) +
    alea_plot_theme()
}

plot_alea_batch_quantiles <- function(
    x,
    return_period_scale = c("gumbel", "log", "linear"),
    plot_observed = TRUE,
    plotting_position_a = 0.44
) {
  return_period_scale <- match.arg(return_period_scale)
  validate_quantile_observed_plot_args(
    plot_observed = plot_observed,
    plotting_position_a = plotting_position_a
  )

  data <- validate_batch_quantiles_plot_data(x$quantiles)

  return_period_ticks <- sort(unique(data$return_period))
  return_period_tick_positions <- transform_return_period_axis(
    return_period = return_period_ticks,
    scale = return_period_scale
  )

  observed <- make_observed_batch_quantile_plot_data(
    x = x,
    return_period_scale = return_period_scale,
    max_return_period = max(return_period_ticks, na.rm = TRUE),
    plotting_position_a = plotting_position_a
  )

  model <- make_batch_quantile_model_plot_data(
    x = x,
    return_period_ticks = return_period_ticks,
    return_period_scale = return_period_scale
  )

  if (nrow(model) < 1L) {
    model <- data
    model$return_period_axis <- transform_return_period_axis(
      return_period = model$return_period,
      scale = return_period_scale
    )
    model$model_label <- make_quantile_model_label(model)
  }

  multiple_models <- length(unique(model$model_label)) > 1L

  p <- ggplot2::ggplot(
    model,
    ggplot2::aes(x = return_period_axis, y = quantile)
  ) +
    ggplot2::geom_line(
      ggplot2::aes(
        group = interaction(station, model_label, drop = TRUE),
        color = model_label
      ),
      linewidth = 0.5
    )

  if (isTRUE(plot_observed) && nrow(observed) > 0L) {
    p <- p +
      ggplot2::geom_point(
        data = observed,
        ggplot2::aes(x = return_period_axis, y = quantile),
        inherit.aes = FALSE,
        shape = 21,
        size = 1.0,
        stroke = 0.35,
        color = alea_plot_color("observed"),
        fill = "black",
        alpha = 0.5
      )
  }

  p <- p +
    ggplot2::scale_x_continuous(
      breaks = return_period_tick_positions,
      labels = return_period_ticks
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(station),
      scales = "free_y"
    ) +
    ggplot2::labs(
      title = "Batch quantile plot",
      subtitle = "One panel per station",
      x = "Return period",
      y = "Quantile"
    ) +
    alea_plot_theme()

  if (multiple_models) {
    p <- p +
      ggplot2::labs(
        color = "Model"
      )
  } else {
    p <- p +
      ggplot2::scale_color_manual(
        values = stats::setNames(
          alea_plot_color("fitted"),
          unique(data$model_label)
        ),
        guide = "none"
      )
  }

  p
}

make_batch_quantile_model_plot_data <- function(
    x,
    return_period_ticks,
    return_period_scale
) {
  fits <- x$fits
  fit_objects <- x$fit_objects

  if (!is.data.frame(fits) || length(fit_objects) < 1L) {
    return(empty_batch_quantile_model_plot_data())
  }

  ok_fits <- fits[fits$status == "ok" & is.finite(fits$fit_index), , drop = FALSE]
  if (nrow(ok_fits) < 1L) {
    return(empty_batch_quantile_model_plot_data())
  }

  ok_fits <- ok_fits[order(ok_fits$station, ok_fits$distribution, ok_fits$method), , drop = FALSE]

  model_return_period <- make_model_return_period_grid(
    return_period_ticks = return_period_ticks,
    n_grid = 200
  )

  rows <- lapply(seq_len(nrow(ok_fits)), function(i) {
    fit_index <- as.integer(ok_fits$fit_index[i])
    if (!is.finite(fit_index) || fit_index < 1L || fit_index > length(fit_objects)) {
      return(NULL)
    }

    fit <- fit_objects[[fit_index]]

    model <- tryCatch(
      alea_quantile(fit, return_period = model_return_period),
      error = function(e) NULL
    )

    if (is.null(model)) {
      return(NULL)
    }

    model <- as.data.frame(model)
    if (nrow(model) < 1L) {
      return(NULL)
    }

    model$station <- ok_fits$station[i]
    model
  })

  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) < 1L) {
    return(empty_batch_quantile_model_plot_data())
  }

  model <- do.call(rbind, rows)
  model$return_period_axis <- transform_return_period_axis(
    return_period = model$return_period,
    scale = return_period_scale
  )
  model$model_label <- make_quantile_model_label(model)

  model
}

empty_batch_quantile_model_plot_data <- function() {
  data.frame(
    station = character(),
    distribution = character(),
    method = character(),
    return_period = numeric(),
    probability = numeric(),
    quantile = numeric(),
    location = numeric(),
    scale = numeric(),
    shape = numeric(),
    return_period_axis = numeric(),
    model_label = character(),
    stringsAsFactors = FALSE
  )
}

make_observed_batch_quantile_plot_data <- function(
    x,
    return_period_scale,
    max_return_period,
    plotting_position_a = 0.44
) {
  fits <- x$fits
  fit_objects <- x$fit_objects

  if (!is.data.frame(fits) || length(fit_objects) < 1L) {
    return(empty_observed_batch_quantile_plot_data())
  }

  ok_fits <- fits[fits$status == "ok" & is.finite(fits$fit_index), , drop = FALSE]
  if (nrow(ok_fits) < 1L) {
    return(empty_observed_batch_quantile_plot_data())
  }

  ok_fits <- ok_fits[order(ok_fits$station, ok_fits$distribution, ok_fits$method), , drop = FALSE]
  ok_fits <- ok_fits[!duplicated(ok_fits$station), , drop = FALSE]

  rows <- lapply(seq_len(nrow(ok_fits)), function(i) {
    fit_index <- as.integer(ok_fits$fit_index[i])
    if (!is.finite(fit_index) || fit_index < 1L || fit_index > length(fit_objects)) {
      return(NULL)
    }

    fit <- fit_objects[[fit_index]]
    if (is.null(fit$data)) {
      return(NULL)
    }

    data <- as.numeric(fit$data)
    data <- data[is.finite(data)]

    if (length(data) < 2L || length(unique(data)) < 2L) {
      return(NULL)
    }

    observed <- make_observed_quantile_data(
      data = data,
      plotting_position_a = plotting_position_a
    )

    observed <- observed[observed$return_period <= max_return_period, , drop = FALSE]
    if (nrow(observed) < 1L) {
      return(NULL)
    }

    observed$station <- ok_fits$station[i]
    observed
  })

  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) < 1L) {
    return(empty_observed_batch_quantile_plot_data())
  }

  observed <- do.call(rbind, rows)
  observed$return_period_axis <- transform_return_period_axis(
    return_period = observed$return_period,
    scale = return_period_scale
  )

  observed
}

empty_observed_batch_quantile_plot_data <- function() {
  data.frame(
    station = character(),
    return_period = numeric(),
    quantile = numeric(),
    return_period_axis = numeric(),
    stringsAsFactors = FALSE
  )
}

plot_alea_batch_gof <- function(x, statistic = "aic") {
  data <- validate_batch_gof_plot_data(x$gof, statistic = statistic)
  
  ggplot2::ggplot(
    data,
    ggplot2::aes(x = distribution, y = estimate)
  ) +
    ggplot2::geom_boxplot(
      width = 0.65,
      fill = alea_plot_color("light"),
      color = alea_plot_color("primary"),
      outlier.alpha = 0.6
    ) +
    ggplot2::geom_point(
      size = 1.0,
      alpha = 0.65,
      color = alea_plot_color("observed")
    ) +
    ggplot2::scale_x_discrete(
      labels = alea_plot_distribution_label
    ) +
    ggplot2::labs(
      title = "Batch goodness-of-fit",
      subtitle = paste0("Statistic: ", toupper(statistic)),
      x = "Distribution",
      y = "Estimate"
    ) +
    alea_plot_theme()
}


plot_alea_batch_diagnostics <- function(x, diagnostic = NULL) {
  data <- validate_batch_diagnostics_plot_data(
    diagnostics = x$diagnostics,
    diagnostic = diagnostic
  )
  
  if (is.null(diagnostic)) {
    return(plot_alea_batch_diagnostics_status_summary(data))
  }
  
  plot_alea_batch_single_diagnostic(data, diagnostic = diagnostic)
}


validate_batch_selected_models_plot_data <- function(data) {
  if (is.null(data) || nrow(as.data.frame(data)) < 1L) {
    stop(
      "`x$selected_models` is empty. Run `alea_batch_fit(..., select = \"ai\")` before plotting selected models.",
      call. = FALSE
    )
  }
  
  data <- as.data.frame(data)
  
  if (!"station" %in% names(data)) {
    stop(
      "`x$selected_models` is missing required column(s): station.",
      call. = FALSE
    )
  }
  
  if ("distribution" %in% names(data)) {
    data$distribution <- data$distribution
  } else if ("selected_distribution" %in% names(data)) {
    data$distribution <- data$selected_distribution
  } else {
    stop(
      "`x$selected_models` is missing required column(s): distribution or selected_distribution.",
      call. = FALSE
    )
  }
  
  if ("method" %in% names(data)) {
    data$method <- data$method
  } else if ("selected_method" %in% names(data)) {
    data$method <- data$selected_method
  } else {
    data$method <- NA_character_
  }
  
  if (!is.character(data$station)) {
    data$station <- as.character(data$station)
  }
  
  if (!is.character(data$distribution)) {
    stop(
      "`x$selected_models$distribution` or `x$selected_models$selected_distribution` must be character.",
      call. = FALSE
    )
  }
  
  if (!is.character(data$method)) {
    data$method <- as.character(data$method)
  }
  
  data <- data[!is.na(data$distribution) & nzchar(data$distribution), , drop = FALSE]
  
  if (nrow(data) < 1L) {
    stop("At least one selected model is required for plotting.", call. = FALSE)
  }
  
  data
}


validate_batch_quantiles_plot_data <- function(data) {
  if (is.null(data) || nrow(as.data.frame(data)) < 1L) {
    stop(
      "`x$quantiles` is empty. Supply `return_period` in `alea_batch_fit()` before plotting quantiles.",
      call. = FALSE
    )
  }
  
  data <- as.data.frame(data)
  
  required_columns <- c(
    "station",
    "distribution",
    "method",
    "return_period",
    "quantile"
  )
  
  missing_columns <- setdiff(required_columns, names(data))
  
  if (length(missing_columns) > 0L) {
    stop(
      "`x$quantiles` is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if (!is.numeric(data$return_period) || !is.numeric(data$quantile)) {
    stop(
      "`x$quantiles$return_period` and `x$quantiles$quantile` must be numeric.",
      call. = FALSE
    )
  }
  
  if (any(!is.finite(data$return_period)) || any(data$return_period <= 1)) {
    stop(
      "All batch `return_period` values must be finite and greater than 1.",
      call. = FALSE
    )
  }
  
  finite_rows <- is.finite(data$quantile)
  
  if (!any(finite_rows)) {
    stop("At least one finite batch quantile is required for plotting.", call. = FALSE)
  }
  
  data <- data[finite_rows, , drop = FALSE]
  
  data$station <- as.character(data$station)
  data$distribution <- as.character(data$distribution)
  data$method <- as.character(data$method)
  
  data$model_label <- make_quantile_model_label(data)
  
  data[order(data$station, data$distribution, data$method, data$return_period), , drop = FALSE]
}


validate_batch_gof_plot_data <- function(data, statistic) {
  if (!is.character(statistic) || length(statistic) != 1L || is.na(statistic) || !nzchar(statistic)) {
    stop("`statistic` must be a non-empty character scalar.", call. = FALSE)
  }
  
  if (is.null(data) || nrow(as.data.frame(data)) < 1L) {
    stop(
      "`x$gof` is empty. Run `alea_batch_fit(..., gof = TRUE)` before plotting GOF results.",
      call. = FALSE
    )
  }
  
  data <- as.data.frame(data)
  
  required_columns <- c(
    "station",
    "distribution",
    "method",
    "statistic",
    "estimate"
  )
  
  missing_columns <- setdiff(required_columns, names(data))
  
  if (length(missing_columns) > 0L) {
    stop(
      "`x$gof` is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if (!is.character(data$statistic)) {
    stop("`x$gof$statistic` must be character.", call. = FALSE)
  }
  
  if (!is.numeric(data$estimate)) {
    stop("`x$gof$estimate` must be numeric.", call. = FALSE)
  }
  
  data <- data[data$statistic == statistic, , drop = FALSE]
  
  if (nrow(data) < 1L) {
    stop("Statistic '", statistic, "' was not found in `x$gof`.", call. = FALSE)
  }
  
  finite_rows <- is.finite(data$estimate)
  
  if (!any(finite_rows)) {
    stop("At least one finite batch GOF estimate is required for plotting.", call. = FALSE)
  }
  
  data <- data[finite_rows, , drop = FALSE]
  
  data$station <- as.character(data$station)
  data$distribution <- as.character(data$distribution)
  data$method <- as.character(data$method)
  
  data
}


validate_batch_diagnostics_plot_data <- function(diagnostics, diagnostic = NULL) {
  if (!is.null(diagnostic)) {
    if (!is.character(diagnostic) || length(diagnostic) != 1L || is.na(diagnostic) || !nzchar(diagnostic)) {
      stop("`diagnostic` must be `NULL` or a non-empty character scalar.", call. = FALSE)
    }
  }
  
  if (is.null(diagnostics) || nrow(as.data.frame(diagnostics)) < 1L) {
    stop(
      "`x$diagnostics` is empty. Run `alea_batch_fit(..., diagnostics = TRUE)` before plotting diagnostics.",
      call. = FALSE
    )
  }
  
  data <- as.data.frame(diagnostics)
  
  required_columns <- c(
    "station",
    "diagnostic",
    "status",
    "p_value",
    "alpha",
    "reject"
  )
  
  missing_columns <- setdiff(required_columns, names(data))
  
  if (length(missing_columns) > 0L) {
    stop(
      "`x$diagnostics` is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if (!is.character(data$diagnostic)) {
    stop("`x$diagnostics$diagnostic` must be character.", call. = FALSE)
  }
  
  if (!is.character(data$status)) {
    stop("`x$diagnostics$status` must be character.", call. = FALSE)
  }
  
  if (!is.numeric(data$p_value)) {
    stop("`x$diagnostics$p_value` must be numeric.", call. = FALSE)
  }
  
  if (!is.numeric(data$alpha)) {
    stop("`x$diagnostics$alpha` must be numeric.", call. = FALSE)
  }
  
  if (!is.logical(data$reject)) {
    stop("`x$diagnostics$reject` must be logical.", call. = FALSE)
  }
  
  if (!is.null(diagnostic)) {
    data <- data[data$diagnostic == diagnostic, , drop = FALSE]
    
    if (nrow(data) < 1L) {
      stop("Diagnostic '", diagnostic, "' was not found in `x$diagnostics`.", call. = FALSE)
    }
  }
  
  data$station <- as.character(data$station)
  data$diagnostic <- as.character(data$diagnostic)
  data$status <- as.character(data$status)
  
  data
}


plot_alea_batch_diagnostics_status_summary <- function(data) {
  summary_data <- stats::aggregate(
    x = list(count = rep(1L, nrow(data))),
    by = list(
      diagnostic = data$diagnostic,
      status = data$status
    ),
    FUN = sum
  )
  
  summary_data$diagnostic <- factor(
    summary_data$diagnostic,
    levels = unique(summary_data$diagnostic)
  )
  
  summary_data$status <- factor(
    summary_data$status,
    levels = c("ok", "warning", "fail")
  )
  
  max_count <- max(summary_data$count, na.rm = TRUE)
  
  ggplot2::ggplot(
    summary_data,
    ggplot2::aes(x = diagnostic, y = count, fill = status)
  ) +
    ggplot2::geom_col(
      width = 0.72,
      alpha = 0.9,
      show.legend = TRUE
    ) +
    ggplot2::scale_x_discrete(
      labels = alea_plot_title_case
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, max_count, by = 1)
    ) +
    alea_plot_fill_status_scale(drop = FALSE) +
    ggplot2::labs(
      title = "Batch diagnostics status",
      subtitle = "Diagnostic status counts across stations and fitted models",
      x = "Diagnostic",
      y = "Count",
      fill = "Status"
    ) +
    alea_plot_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}


plot_alea_batch_single_diagnostic <- function(data, diagnostic) {
  p_data <- data[is.finite(data$p_value), , drop = FALSE]
  
  if (nrow(p_data) < 1L) {
    stop(
      "At least one finite p-value is required to plot diagnostic '",
      diagnostic,
      "'.",
      call. = FALSE
    )
  }
  
  p_data$reject <- factor(
    as.character(p_data$reject),
    levels = c("FALSE", "TRUE")
  )
  
  alpha_values <- unique(p_data$alpha[is.finite(p_data$alpha)])
  
  reject_colors <- c(
    "FALSE" = alea_plot_color("primary"),
    "TRUE" = alea_plot_color("fail")
  )
  
  p <- ggplot2::ggplot(
    p_data,
    ggplot2::aes(x = station, y = p_value)
  ) +
    ggplot2::geom_point(
      ggplot2::aes(shape = reject, color = reject),
      size = 1.0,
      alpha = 0.9,
      show.legend = TRUE
    ) +
    ggplot2::scale_shape_manual(
      name = "Reject null hypothesis",
      values = c(
        "FALSE" = 16,
        "TRUE" = 17
      ),
      limits = c("FALSE", "TRUE"),
      breaks = c("FALSE", "TRUE"),
      labels = c(
        "FALSE" = "No",
        "TRUE" = "Yes"
      ),
      drop = FALSE
    ) +
    ggplot2::scale_color_manual(
      name = "Reject null hypothesis",
      values = reject_colors,
      limits = c("FALSE", "TRUE"),
      breaks = c("FALSE", "TRUE"),
      labels = c(
        "FALSE" = "No",
        "TRUE" = "Yes"
      ),
      drop = FALSE,
      guide = ggplot2::guide_legend(
        override.aes = list(
          shape = c(16, 17),
          color = unname(reject_colors),
          alpha = 0.9,
          size = 1.5
        )
      )
    ) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(
      title = "Batch diagnostic p-values",
      subtitle = paste0("Diagnostic: ", alea_plot_title_case(diagnostic)),
      x = "Station",
      y = "p-value",
      shape = "Reject null hypothesis",
      color = "Reject null hypothesis"
    ) +
    alea_plot_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  if (length(alpha_values) == 1L) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = alpha_values,
        linetype = "dashed",
        linewidth = 0.3,
        color = alea_plot_color("neutral")
      )
  }
  
  p
}