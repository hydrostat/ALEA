# Plot methods for ALEA quantile objects

#' Plot ALEA quantiles
#'
#' Produces a quantile plot from an `alea_quantile` object.
#'
#' @param x An object of class `alea_quantile`.
#' @param return_period_scale Character scalar. Scale used for the
#'   return-period axis. Supported values are `"gumbel"`, `"log"`, and
#'   `"linear"`. The default is `"gumbel"`.
#' @param plot_observed Logical scalar. If `TRUE`, observed plotting-position
#'   points are added when the observed sample is available in the object.
#' @param plotting_position_a Numeric scalar. Plotting-position parameter `a`
#'   used for observed quantile points, with plotting positions
#'   `p_i = (i - a) / (n + 1 - 2 * a)` and empirical return periods
#'   `T_i = 1 / (1 - p_i)`.
#' @param ... Additional arguments passed to methods.
#'
#' @return A `ggplot` object.
#'
#' @method plot alea_quantile
#' @examples
#' x <- c(42.1, 38.5, 51.3, 47.0, 62.4, 55.2, 49.8, 58.1,
#'        60.3, 45.9)
#' fit <- alea_fit(x, distribution = "gev", method = "lmom")
#' q <- alea_quantile(fit, return_period = c(10, 25, 50))
#' plot(q)
#'
#' @export
plot.alea_quantile <- function(
    x,
    return_period_scale = c("gumbel", "log", "linear"),
    plot_observed = TRUE,
    plotting_position_a = 0.44,
    ...
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required to plot ALEA objects.", call. = FALSE)
  }

  return_period_scale <- match.arg(return_period_scale)
  validate_quantile_observed_plot_args(
    plot_observed = plot_observed,
    plotting_position_a = plotting_position_a
  )

  data <- validate_quantile_plot_data(x, class = "alea_quantile")

  data$return_period_axis <- transform_return_period_axis(
    return_period = data$return_period,
    scale = return_period_scale
  )

  data$model_label <- make_quantile_model_label(data)

  return_period_ticks <- sort(unique(data$return_period))
  return_period_tick_positions <- transform_return_period_axis(
    return_period = return_period_ticks,
    scale = return_period_scale
  )

  observed <- make_observed_quantile_plot_data_from_object(
    x = x,
    return_period_scale = return_period_scale,
    max_return_period = max(return_period_ticks, na.rm = TRUE),
    plotting_position_a = plotting_position_a
  )

  multiple_models <- length(unique(data$model_label)) > 1L

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = return_period_axis, y = quantile)
  ) +
    ggplot2::geom_line(
      ggplot2::aes(
        group = model_label,
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
        stroke = 0.6,
        color = alea_plot_color("observed"),
        fill = "white",
        alpha = 0.9
      )
  }

  p <- p +
    ggplot2::scale_x_continuous(
      breaks = return_period_tick_positions,
      labels = return_period_ticks
    ) +
    ggplot2::labs(
      title = "Quantile plot",
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

#' Plot ALEA quantile confidence intervals
#'
#' Produces a quantile plot with confidence intervals from an
#' `alea_quantile_ci` object.
#'
#' @param x An object of class `alea_quantile_ci`.
#' @param return_period_scale Character scalar. Scale used for the
#'   return-period axis. Supported values are `"gumbel"`, `"log"`, and
#'   `"linear"`. The default is `"gumbel"`.
#' @param plot_observed Logical scalar. If `TRUE`, observed plotting-position
#'   points are added when the observed sample is available in the object.
#' @param plotting_position_a Numeric scalar. Plotting-position parameter `a`
#'   used for observed quantile points, with plotting positions
#'   `p_i = (i - a) / (n + 1 - 2 * a)` and empirical return periods
#'   `T_i = 1 / (1 - p_i)`.
#' @param ... Additional arguments passed to methods.
#'
#' @return A `ggplot` object.
#'
#' @method plot alea_quantile_ci
#' @examples
#' x <- c(42.1, 38.5, 51.3, 47.0, 62.4, 55.2, 49.8, 58.1,
#'        60.3, 45.9)
#' fit <- alea_fit(x, distribution = "gev", method = "lmom")
#' ci <- confint(
#'   fit,
#'   parm = "quantile",
#'   return_period = c(10, 25),
#'   n_boot = 20,
#'   seed = 123
#' )
#' plot(ci)
#'
#' @export
plot.alea_quantile_ci <- function(
    x,
    return_period_scale = c("gumbel", "log", "linear"),
    plot_observed = TRUE,
    plotting_position_a = 0.44,
    ...
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required to plot ALEA objects.", call. = FALSE)
  }

  return_period_scale <- match.arg(return_period_scale)
  validate_quantile_observed_plot_args(
    plot_observed = plot_observed,
    plotting_position_a = plotting_position_a
  )

  data <- validate_quantile_plot_data(x, class = "alea_quantile_ci")

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

  data$model_label <- make_quantile_model_label(data)

  return_period_ticks <- sort(unique(data$return_period))
  return_period_tick_positions <- transform_return_period_axis(
    return_period = return_period_ticks,
    scale = return_period_scale
  )

  observed <- make_observed_quantile_plot_data_from_object(
    x = x,
    return_period_scale = return_period_scale,
    max_return_period = max(return_period_ticks, na.rm = TRUE),
    plotting_position_a = plotting_position_a
  )

  multiple_models <- length(unique(data$model_label)) > 1L

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = return_period_axis, y = quantile)
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
    )

  if (isTRUE(plot_observed) && nrow(observed) > 0L) {
    p <- p +
      ggplot2::geom_point(
        data = observed,
        ggplot2::aes(x = return_period_axis, y = quantile),
        inherit.aes = FALSE,
        shape = 21,
        size = 1.0,
        stroke = 0.6,
        color = alea_plot_color("observed"),
        fill = "white",
        alpha = 0.9
      )
  }

  p <- p +
    ggplot2::scale_x_continuous(
      breaks = return_period_tick_positions,
      labels = return_period_ticks
    ) +
    ggplot2::labs(
      title = "Quantile confidence interval plot",
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

make_quantile_model_label <- function(data) {
  distribution <- alea_plot_distribution_label(data$distribution)
  method <- alea_plot_method_label(data$method)

  paste(distribution, method, sep = " / ")
}

validate_quantile_observed_plot_args <- function(plot_observed, plotting_position_a) {
  if (!is.logical(plot_observed) || length(plot_observed) != 1L || is.na(plot_observed)) {
    stop("`plot_observed` must be `TRUE` or `FALSE`.", call. = FALSE)
  }

  if (
    !is.numeric(plotting_position_a) ||
    length(plotting_position_a) != 1L ||
    !is.finite(plotting_position_a)
  ) {
    stop("`plotting_position_a` must be a finite numeric scalar.", call. = FALSE)
  }

  if (plotting_position_a < 0 || plotting_position_a >= 1) {
    stop("`plotting_position_a` must be greater than or equal to 0 and less than 1.", call. = FALSE)
  }

  invisible(TRUE)
}

make_observed_quantile_plot_data_from_object <- function(
    x,
    return_period_scale,
    max_return_period,
    plotting_position_a = 0.44
) {
  data <- attr(x, "data", exact = TRUE)
  if (is.null(data)) {
    data <- attr(x, "observed_data", exact = TRUE)
  }

  if (is.null(data)) {
    return(empty_observed_quantile_plot_data())
  }

  data <- as.numeric(data)
  data <- data[is.finite(data)]

  if (length(data) < 2L || length(unique(data)) < 2L) {
    return(empty_observed_quantile_plot_data())
  }

  observed <- make_observed_quantile_data(
    data = data,
    plotting_position_a = plotting_position_a
  )

  observed <- observed[observed$return_period <= max_return_period, , drop = FALSE]

  if (nrow(observed) < 1L) {
    return(empty_observed_quantile_plot_data())
  }

  observed$return_period_axis <- transform_return_period_axis(
    return_period = observed$return_period,
    scale = return_period_scale
  )

  observed
}

empty_observed_quantile_plot_data <- function() {
  data.frame(
    return_period = numeric(),
    quantile = numeric(),
    return_period_axis = numeric()
  )
}

validate_quantile_plot_data <- function(x, class) {
  if (!inherits(x, class)) {
    stop("`x` must be an object of class '", class, "'.", call. = FALSE)
  }

  data <- as.data.frame(x)

  required_columns <- c(
    "distribution",
    "method",
    "return_period",
    "quantile"
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

  if (!is.numeric(data$return_period) || !is.numeric(data$quantile)) {
    stop(
      "`return_period` and `quantile` columns must be numeric.",
      call. = FALSE
    )
  }

  if (any(!is.finite(data$return_period)) || any(data$return_period <= 1)) {
    stop(
      "All `return_period` values must be finite and greater than 1.",
      call. = FALSE
    )
  }

  if (any(!is.finite(data$quantile))) {
    stop("All `quantile` values must be finite.", call. = FALSE)
  }

  if (class == "alea_quantile_ci") {
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

  data[order(data$distribution, data$method, data$return_period), , drop = FALSE]
}
