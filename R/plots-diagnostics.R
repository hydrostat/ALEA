# Plot methods for ALEA diagnostics objects

#' Plot ALEA diagnostics
#'
#' Produces diagnostic plots from an `alea_diagnostics` object.
#'
#' @param x An object of class `alea_diagnostics`.
#' @param type Character scalar. Plot type. One of `"status"` or `"p_value"`.
#' @param ... Additional arguments passed to methods.
#'
#' @return A `ggplot` object.
#'
#' @method plot alea_diagnostics
#' @examples
#' x <- c(42.1, 38.5, 51.3, 47.0, 62.4, 55.2, 49.8, 58.1,
#'        60.3, 45.9)
#' diagnostics <- alea_diagnostics(x)
#' plot(diagnostics, type = "status")
#'
#' @export
plot.alea_diagnostics <- function(
    x,
    type = c("status", "p_value"),
    ...
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required to plot ALEA objects.", call. = FALSE)
  }
  
  type <- match.arg(type)
  
  data <- validate_diagnostics_plot_data(x)
  
  if (type == "status") {
    return(plot_alea_diagnostics_status(data))
  }
  
  if (type == "p_value") {
    return(plot_alea_diagnostics_p_value(data))
  }
  
  stop("Unsupported diagnostics plot type.", call. = FALSE)
}


validate_diagnostics_plot_data <- function(x) {
  if (!inherits(x, "alea_diagnostics")) {
    stop("`x` must be an object of class 'alea_diagnostics'.", call. = FALSE)
  }
  
  data <- as.data.frame(x)
  
  required_columns <- c(
    "diagnostic",
    "status",
    "message",
    "p_value",
    "alpha",
    "reject",
    "n",
    "n_valid"
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
  
  if (!is.character(data$diagnostic)) {
    stop("`diagnostic` column must be character.", call. = FALSE)
  }
  
  if (!is.character(data$status)) {
    stop("`status` column must be character.", call. = FALSE)
  }
  
  if (!is.numeric(data$p_value)) {
    stop("`p_value` column must be numeric.", call. = FALSE)
  }
  
  if (!is.numeric(data$alpha)) {
    stop("`alpha` column must be numeric.", call. = FALSE)
  }
  
  if (!is.logical(data$reject)) {
    stop("`reject` column must be logical.", call. = FALSE)
  }
  
  if (!is.numeric(data$n) || !is.numeric(data$n_valid)) {
    stop("`n` and `n_valid` columns must be numeric.", call. = FALSE)
  }
  
  if (nrow(data) < 1L) {
    stop("At least one diagnostic row is required for plotting.", call. = FALSE)
  }
  
  data$diagnostic <- as.character(data$diagnostic)
  data$status <- as.character(data$status)
  data$message <- as.character(data$message)
  
  if ("distribution" %in% names(data)) {
    data$distribution <- as.character(data$distribution)
  } else {
    data$distribution <- NA_character_
  }
  
  if ("method" %in% names(data)) {
    data$method <- as.character(data$method)
  } else {
    data$method <- NA_character_
  }
  
  data$diagnostic <- factor(data$diagnostic, levels = unique(data$diagnostic))
  
  data
}


plot_alea_diagnostics_status <- function(data) {
  status_counts <- stats::aggregate(
    x = list(count = rep(1L, nrow(data))),
    by = list(
      diagnostic = data$diagnostic,
      status = data$status
    ),
    FUN = sum
  )
  
  status_counts$status <- factor(
    status_counts$status,
    levels = c("ok", "warning", "fail")
  )
  
  max_count <- max(status_counts$count, na.rm = TRUE)
  
  ggplot2::ggplot(
    status_counts,
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
      title = "Diagnostics status",
      subtitle = diagnostics_plot_subtitle(data),
      x = "Diagnostic",
      y = "Count",
      fill = "Status"
    ) +
    alea_plot_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}

plot_alea_diagnostics_p_value <- function(data) {
  p_data <- data[is.finite(data$p_value), , drop = FALSE]
  
  if (nrow(p_data) < 1L) {
    stop("At least one finite diagnostic p-value is required for this plot.", call. = FALSE)
  }
  
  p_data$diagnostic <- factor(
    p_data$diagnostic,
    levels = unique(p_data$diagnostic)
  )
  
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
    ggplot2::aes(x = diagnostic, y = p_value)
  ) +
    ggplot2::geom_point(
      ggplot2::aes(shape = reject, color = reject),
      size = 2.5,
      alpha = 0.9,
      show.legend = TRUE
    ) +
    ggplot2::scale_x_discrete(
      labels = alea_plot_title_case
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
          size = 2.5
        )
      )
    ) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(
      title = "Diagnostics p-values",
      subtitle = diagnostics_plot_subtitle(data),
      x = "Diagnostic",
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

diagnostics_plot_subtitle <- function(data) {
  if (
    all(c("distribution", "method") %in% names(data)) &&
    any(!is.na(data$distribution)) &&
    any(!is.na(data$method))
  ) {
    return(alea_plot_model_subtitle(data$distribution, data$method))
  }
  
  "Sample diagnostics"
}