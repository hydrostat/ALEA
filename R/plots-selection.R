# Plot methods for ALEA AI-selection objects

#' Plot ALEA AI-assisted model selection
#'
#' Produces a probability ranking plot from an `alea_selection` object.
#'
#' @param x An object of class `alea_selection`.
#' @param ... Additional arguments passed to methods.
#'
#' @return A `ggplot` object.
#'
#' @method plot alea_selection
#' @examples
#' x <- c(42.1, 38.5, 51.3, 47.0, 62.4, 55.2, 49.8, 58.1,
#'        60.3, 45.9, 67.2, 44.6)
#' selection <- alea_select(x)
#' plot(selection)
#'
#' @export
plot.alea_selection <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required to plot ALEA objects.", call. = FALSE)
  }
  
  data <- validate_selection_plot_data(x)
  decision <- selection_plot_decision_data(x)
  
  plot_alea_selection_ranking(
    data = data,
    decision = decision
  )
}


validate_selection_plot_data <- function(x) {
  if (!inherits(x, "alea_selection")) {
    stop("`x` must be an object of class 'alea_selection'.", call. = FALSE)
  }
  
  if (is.null(x$ranking)) {
    stop("`x$ranking` is missing.", call. = FALSE)
  }
  
  data <- as.data.frame(x$ranking)
  
  required_columns <- c(
    "distribution",
    "probability",
    "rank",
    "selected"
  )
  
  missing_columns <- setdiff(required_columns, names(data))
  
  if (length(missing_columns) > 0L) {
    stop(
      "`x$ranking` is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if (!is.character(data$distribution)) {
    stop("`distribution` column must be character.", call. = FALSE)
  }
  
  if (!is.numeric(data$probability)) {
    stop("`probability` column must be numeric.", call. = FALSE)
  }
  
  if (!is.numeric(data$rank) && !is.integer(data$rank)) {
    stop("`rank` column must be numeric or integer.", call. = FALSE)
  }
  
  if (!is.logical(data$selected)) {
    stop("`selected` column must be logical.", call. = FALSE)
  }
  
  if (nrow(data) < 1L) {
    stop("At least one ranking row is required for plotting.", call. = FALSE)
  }
  
  if (any(!is.finite(data$probability))) {
    stop("All `probability` values must be finite.", call. = FALSE)
  }
  
  if (any(data$probability < 0 | data$probability > 1)) {
    stop("All `probability` values must be between 0 and 1.", call. = FALSE)
  }
  
  if (any(!is.finite(data$rank))) {
    stop("All `rank` values must be finite.", call. = FALSE)
  }
  
  if (any(data$rank < 1)) {
    stop("All `rank` values must be positive.", call. = FALSE)
  }
  
  data <- data[order(data$rank, data$distribution), , drop = FALSE]
  
  data$distribution <- factor(
    data$distribution,
    levels = rev(data$distribution)
  )
  
  data
}


selection_plot_decision_data <- function(x) {
  if (is.null(x$decision)) {
    return(NULL)
  }
  
  decision <- as.data.frame(x$decision)
  
  if (nrow(decision) < 1L) {
    return(NULL)
  }
  
  decision
}


plot_alea_selection_ranking <- function(data, decision = NULL) {
  subtitle <- selection_plot_subtitle(decision)
  
  ggplot2::ggplot(
    data,
    ggplot2::aes(x = probability, y = distribution)
  ) +
    ggplot2::geom_col(
      ggplot2::aes(fill = selected),
      width = 0.72,
      alpha = 0.95
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = format_selection_probability(probability)),
      hjust = -0.10,
      size = 3.3,
      color = alea_plot_color("observed")
    ) +
    ggplot2::coord_cartesian(
      xlim = c(0, selection_probability_axis_limit(data)),
      clip = "off"
    ) +
    ggplot2::scale_x_continuous(
      labels = alea_plot_percent_labels(digits = 0)
    ) +
    ggplot2::scale_y_discrete(
      labels = alea_plot_distribution_label
    ) +
    alea_plot_fill_selection_scale(drop = FALSE) +
    ggplot2::labs(
      title = "AI-assisted distribution selection",
      subtitle = subtitle,
      x = "Model-based support",
      y = "Distribution",
      fill = NULL
    ) +
    alea_plot_theme() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 5.5, r = 20, b = 5.5, l = 5.5)
    )
}


selection_probability_axis_limit <- function(data) {
  upper <- max(data$probability, na.rm = TRUE)
  upper <- upper + 0.10
  
  min(max(upper, 0.20), 1.00)
}


format_selection_probability <- function(probability) {
  paste0(round(100 * probability, 1), "%")
}


selection_plot_subtitle <- function(decision) {
  if (is.null(decision)) {
    return("Candidate distribution ranking")
  }
  
  required_columns <- c(
    "top_family",
    "top_support",
    "second_family",
    "second_support",
    "top1_top2_margin",
    "decision_strength"
  )
  
  if (!all(required_columns %in% names(decision))) {
    return("Candidate distribution ranking")
  }
  
  top_family <- as.character(decision$top_family[1L])
  top_support <- decision$top_support[1L]
  second_family <- as.character(decision$second_family[1L])
  second_support <- decision$second_support[1L]
  margin <- decision$top1_top2_margin[1L]
  strength <- as.character(decision$decision_strength[1L])
  
  if (
    anyNA(c(top_family, second_family, strength)) ||
    any(!is.finite(c(top_support, second_support, margin)))
  ) {
    return("Candidate distribution ranking")
  }
  
  paste0(
    "Top: ",
    top_family,
    " (",
    round(100 * top_support, 1),
    "%); second: ",
    second_family,
    " (",
    round(100 * second_support, 1),
    "%); margin: ",
    round(100 * margin, 1),
    "%; ",
    strength
  )
}