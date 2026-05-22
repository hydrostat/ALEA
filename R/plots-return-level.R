# Plot methods for ALEA return-level objects

#' Plot ALEA return levels
#'
#' Produces a return-level plot from an `alea_return_level` object.
#'
#' @param x An object of class `alea_return_level`.
#' @param return_period_scale Character scalar. Scale used for the
#'   return-period axis. Supported values are `"gumbel"`, `"log"`, and
#'   `"linear"`. The default is `"gumbel"`.
#' @param ... Additional arguments passed to methods. Currently unused.
#'
#' @return A `ggplot` object.
#'
#' @method plot alea_return_level
#' @export
plot.alea_return_level <- function(
    x,
    return_period_scale = c("gumbel", "log", "linear"),
    ...
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required to plot ALEA objects.", call. = FALSE)
  }
  
  return_period_scale <- match.arg(return_period_scale)
  
  data <- validate_return_level_plot_data(x, class = "alea_return_level")
  
  data$return_period_axis <- transform_return_period_axis(
    return_period = data$return_period,
    scale = return_period_scale
  )
  
  data$model_label <- make_return_level_model_label(data)
  
  return_period_ticks <- sort(unique(data$return_period))
  return_period_tick_positions <- transform_return_period_axis(
    return_period = return_period_ticks,
    scale = return_period_scale
  )
  
  multiple_models <- length(unique(data$model_label)) > 1L
  
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = return_period_axis, y = return_level)
  ) +
    ggplot2::geom_line(
      ggplot2::aes(
        group = model_label,
        color = model_label
      ),
      linewidth = 0.5
    ) +
    ggplot2::scale_x_continuous(
      breaks = return_period_tick_positions,
      labels = return_period_ticks
    ) +
    ggplot2::labs(
      title = "Return-level plot",
      subtitle = alea_plot_model_subtitle(data$distribution, data$method),
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

#' Plot ALEA return-level confidence intervals
#'
#' Produces a return-level plot with confidence intervals from an
#' `alea_return_level_ci` object.
#'
#' @param x An object of class `alea_return_level_ci`.
#' @param return_period_scale Character scalar. Scale used for the
#'   return-period axis. Supported values are `"gumbel"`, `"log"`, and
#'   `"linear"`. The default is `"gumbel"`.
#' @param ... Additional arguments passed to methods. Currently unused.
#'
#' @return A `ggplot` object.
#'
#' @method plot alea_return_level_ci
#' @export
plot.alea_return_level_ci <- function(
    x,
    return_period_scale = c("gumbel", "log", "linear"),
    ...
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required to plot ALEA objects.", call. = FALSE)
  }
  
  return_period_scale <- match.arg(return_period_scale)
  
  data <- validate_return_level_plot_data(x, class = "alea_return_level_ci")
  
  conf_level <- unique(data$conf_level)
  conf_level <- conf_level[is.finite(conf_level)]
  
  if (length(conf_level) == 1L) {
    interval_label <- paste0(round(100 * conf_level), "% confidence interval")
  } else {
    interval_label <- "Confidence interval"
  }
  
  data$return_period_axis <- transform_return_period_axis(
    return_period = data$return_period,
    scale = return_period_scale
  )
  
  data$model_label <- make_return_level_model_label(data)
  
  return_period_ticks <- sort(unique(data$return_period))
  return_period_tick_positions <- transform_return_period_axis(
    return_period = return_period_ticks,
    scale = return_period_scale
  )
  
  multiple_models <- length(unique(data$model_label)) > 1L
  
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = return_period_axis, y = return_level)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = lower,
        ymax = upper,
        group = model_label,
        fill = model_label
      ),
      alpha = 0.10,
      color = NA
    ) +
    ggplot2::geom_line(
      ggplot2::aes(
        y = lower,
        group = model_label,
        color = model_label
      ),
      linewidth = 0.2,
      alpha = 0.4
    ) +
    ggplot2::geom_line(
      ggplot2::aes(
        y = upper,
        group = model_label,
        color = model_label
      ),
      linewidth = 0.2,
      alpha = 0.4
    ) +
    ggplot2::geom_line(
      ggplot2::aes(
        group = model_label,
        color = model_label
      ),
      linewidth = 0.5
    ) +
    ggplot2::scale_x_continuous(
      breaks = return_period_tick_positions,
      labels = return_period_ticks
    ) +
    ggplot2::labs(
      title = "Return-level confidence interval plot",
      subtitle = paste(
        alea_plot_model_subtitle(data$distribution, data$method),
        interval_label,
        sep = " | "
      ),
      x = "Return period",
      y = "Quantile"
    ) +
    alea_plot_theme()
  
  if (multiple_models) {
    p <- p +
      ggplot2::labs(
        color = "Model",
        fill = "Model"
      )
  } else {
    p <- p +
      ggplot2::scale_color_manual(
        values = stats::setNames(
          alea_plot_color("fitted"),
          unique(data$model_label)
        ),
        guide = "none"
      ) +
      ggplot2::scale_fill_manual(
        values = stats::setNames(
          alea_plot_color("fitted"),
          unique(data$model_label)
        ),
        guide = "none"
      )
  }
  
  p
}

make_return_level_model_label <- function(data) {
  distribution <- alea_plot_distribution_label(data$distribution)
  method <- alea_plot_method_label(data$method)
  
  paste(distribution, method, sep = " / ")
}

validate_return_level_plot_data <- function(x, class) {
  if (!inherits(x, class)) {
    stop("`x` must be an object of class '", class, "'.", call. = FALSE)
  }
  
  data <- as.data.frame(x)
  
  required_columns <- c(
    "distribution",
    "method",
    "return_period",
    "return_level"
  )
  
  missing_columns <- setdiff(required_columns, names(data))
  
  if (length(missing_columns) > 0L) {
    stop(
      "`x` is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if (!is.numeric(data$return_period) || !is.numeric(data$return_level)) {
    stop(
      "`return_period` and `return_level` columns must be numeric.",
      call. = FALSE
    )
  }
  
  if (any(!is.finite(data$return_period)) || any(data$return_period <= 1)) {
    stop(
      "All `return_period` values must be finite and greater than 1.",
      call. = FALSE
    )
  }
  
  if (any(!is.finite(data$return_level))) {
    stop("All `return_level` values must be finite.", call. = FALSE)
  }
  
  if (class == "alea_return_level_ci") {
    required_ci_columns <- c("lower", "upper", "conf_level")
    missing_ci_columns <- setdiff(required_ci_columns, names(data))
    
    if (length(missing_ci_columns) > 0L) {
      stop(
        "`x` is missing required confidence-interval column(s): ",
        paste(missing_ci_columns, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    
    if (!is.numeric(data$lower) || !is.numeric(data$upper)) {
      stop("`lower` and `upper` columns must be numeric.", call. = FALSE)
    }
    
    if (any(!is.finite(data$lower)) || any(!is.finite(data$upper))) {
      stop("Confidence-interval limits must be finite.", call. = FALSE)
    }
    
    if (any(data$lower > data$upper)) {
      stop("Confidence-interval lower limits must not exceed upper limits.", call. = FALSE)
    }
    
    if (!is.numeric(data$conf_level)) {
      stop("`conf_level` column must be numeric.", call. = FALSE)
    }
  }
  
  data[order(data$return_period), , drop = FALSE]
}


