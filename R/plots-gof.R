# Plot methods for ALEA goodness-of-fit objects

#' Plot ALEA goodness-of-fit results
#'
#' Produces a goodness-of-fit plot from an `alea_gof` object.
#'
#' @param x An object of class `alea_gof`.
#' @param type Character scalar. Plot type. One of `"statistic"` or `"rank"`.
#' @param ... Additional arguments passed to methods. Currently unused.
#'
#' @return A `ggplot` object.
#'
#' @method plot alea_gof
#' @export
plot.alea_gof <- function(
    x,
    type = c("statistic", "rank"),
    ...
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required to plot ALEA objects.", call. = FALSE)
  }
  
  type <- match.arg(type)
  
  data <- validate_gof_plot_data(x)
  
  if (type == "statistic") {
    return(plot_alea_gof_statistic(data))
  }
  
  if (type == "rank") {
    return(plot_alea_gof_rank(data))
  }
  
  stop("Unsupported GOF plot type.", call. = FALSE)
}


validate_gof_plot_data <- function(x) {
  if (!inherits(x, "alea_gof")) {
    stop("`x` must be an object of class 'alea_gof'.", call. = FALSE)
  }
  
  data <- as.data.frame(x)
  
  required_columns <- c(
    "distribution",
    "method",
    "statistic",
    "estimate",
    "higher_is_better",
    "description"
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
  
  if (!is.character(data$statistic)) {
    stop("`statistic` column must be character.", call. = FALSE)
  }
  
  if (!is.numeric(data$estimate)) {
    stop("`estimate` column must be numeric.", call. = FALSE)
  }
  
  if (!is.logical(data$higher_is_better)) {
    stop("`higher_is_better` column must be logical.", call. = FALSE)
  }
  
  finite_rows <- is.finite(data$estimate)
  
  if (!any(finite_rows)) {
    stop("At least one finite GOF estimate is required for plotting.", call. = FALSE)
  }
  
  data <- data[finite_rows, , drop = FALSE]
  
  data$statistic <- as.character(data$statistic)
  data$distribution <- as.character(data$distribution)
  data$method <- as.character(data$method)
  data$label <- make_gof_plot_label(data)
  
  data
}


plot_alea_gof_statistic <- function(data) {
  plot_data <- data
  
  plot_data$statistic <- factor(
    plot_data$statistic,
    levels = unique(plot_data$statistic)
  )
  
  plot_data$distribution <- factor(
    plot_data$distribution,
    levels = unique(plot_data$distribution)
  )
  
  plot_data$model_label <- alea_plot_distribution_label(plot_data$distribution)
  
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = model_label, y = estimate)
  ) +
    ggplot2::geom_col(
      width = 0.72,
      fill = alea_plot_color("primary"),
      alpha = 0.85
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(statistic),
      scales = "free_y"
    ) +
    ggplot2::labs(
      title = "Goodness-of-fit statistics",
      subtitle = alea_plot_model_subtitle(data$distribution, data$method),
      x = "Distribution",
      y = "Estimate"
    ) +
    alea_plot_theme()
}


plot_alea_gof_rank <- function(data) {
  rank_data <- data
  
  rank_data$rank_value <- NA_real_
  
  lower_better <- !rank_data$higher_is_better
  higher_better <- rank_data$higher_is_better
  
  if (any(lower_better)) {
    rank_data$rank_value[lower_better] <- ave(
      rank_data$estimate[lower_better],
      rank_data$statistic[lower_better],
      FUN = function(z) rank(z, ties.method = "min")
    )
  }
  
  if (any(higher_better)) {
    rank_data$rank_value[higher_better] <- ave(
      -rank_data$estimate[higher_better],
      rank_data$statistic[higher_better],
      FUN = function(z) rank(z, ties.method = "min")
    )
  }
  
  rank_data$statistic <- factor(
    rank_data$statistic,
    levels = unique(rank_data$statistic)
  )
  
  rank_data$distribution_label <- alea_plot_distribution_label(rank_data$distribution)
  
  ggplot2::ggplot(
    rank_data,
    ggplot2::aes(
      x = statistic,
      y = rank_value,
      group = distribution_label,
      color = distribution_label
    )
  ) +
    ggplot2::geom_line(
      linewidth = 0.3,
      alpha = 0.75
    ) +
    ggplot2::geom_point(
      size = 1.0,
      alpha = 0.95
    ) +
    ggplot2::scale_y_reverse(
      breaks = sort(unique(rank_data$rank_value))
    ) +
    ggplot2::labs(
      title = "Goodness-of-fit rank plot",
      subtitle = "Rank 1 indicates the best distribution for each statistic",
      x = "Statistic",
      y = "Rank",
      color = "Distribution"
    ) +
    alea_plot_theme()
}


make_gof_plot_label <- function(data) {
  paste(data$distribution, data$method, sep = " / ")
}

