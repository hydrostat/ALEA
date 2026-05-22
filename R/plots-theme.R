# Common plotting helpers for ALEA graphics

alea_plot_theme <- function(base_size = 11, base_family = "") {
  ggplot2::theme_bw(
    base_size = base_size,
    base_family = base_family
  ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = ggplot2::rel(1.15),
        margin = ggplot2::margin(b = 4)
      ),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(0.95),
        color = "grey30",
        margin = ggplot2::margin(b = 8)
      ),
      axis.title = ggplot2::element_text(
        face = "bold"
      ),
      axis.text = ggplot2::element_text(
        color = "grey20"
      ),
      panel.grid.major = ggplot2::element_line(
        linewidth = 0.25,
        color = "grey85"
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        linewidth = 0.45,
        color = "grey40",
        fill = NA
      ),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(
        face = "bold"
      ),
      legend.key = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(
        fill = "grey92",
        color = "grey50",
        linewidth = 0.35
      ),
      strip.text = ggplot2::element_text(
        face = "bold"
      )
    )
}


alea_plot_colors <- function() {
  c(
    primary = "#1B4F72",
    secondary = "#D35400",
    tertiary = "#117A65",
    quaternary = "#7D3C98",
    neutral = "#6C757D",
    light = "#D6EAF8",
    lighter = "#EBF5FB",
    histogram = "#5DADE2",
    observed = "#2C3E50",
    fitted = "#C0392B",
    selected = "#1B4F72",
    candidate = "#B0B7C3",
    ok = "#117A65",
    warning = "#D68910",
    fail = "#A93226"
  )
}


alea_plot_color <- function(name) {
  colors <- alea_plot_colors()
  
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
    stop("`name` must be a non-empty character scalar.", call. = FALSE)
  }
  
  if (!name %in% names(colors)) {
    stop(
      "Unknown ALEA plot color: ",
      name,
      ".",
      call. = FALSE
    )
  }
  
  unname(colors[[name]])
}


alea_plot_fill_status_scale <- function(drop = FALSE) {
  colors <- alea_plot_colors()
  
  values <- c(
    ok = colors[["ok"]],
    warning = colors[["warning"]],
    fail = colors[["fail"]]
  )
  
  ggplot2::scale_fill_manual(
    values = values,
    limits = c("ok", "warning", "fail"),
    breaks = c("ok", "warning", "fail"),
    labels = c(
      ok = "ok",
      warning = "warning",
      fail = "fail"
    ),
    drop = drop,
    guide = ggplot2::guide_legend(
      override.aes = list(
        fill = unname(values),
        alpha = 0.9
      )
    )
  )
}


alea_plot_fill_selection_scale <- function(drop = FALSE) {
  colors <- alea_plot_colors()
  
  ggplot2::scale_fill_manual(
    values = c(
      "FALSE" = colors[["candidate"]],
      "TRUE" = colors[["selected"]]
    ),
    labels = c(
      "FALSE" = "Candidate",
      "TRUE" = "Selected"
    ),
    drop = drop
  )
}


alea_plot_percent_labels <- function(digits = 0) {
  force(digits)
  
  function(x) {
    paste0(round(100 * x, digits), "%")
  }
}


alea_plot_title_case <- function(x) {
  x <- gsub("_", " ", as.character(x), fixed = TRUE)
  x <- trimws(x)
  
  words <- strsplit(x, "\\s+")
  vapply(
    words,
    function(w) {
      w <- tolower(w)
      w <- paste0(toupper(substr(w, 1L, 1L)), substring(w, 2L))
      paste(w, collapse = " ")
    },
    character(1L)
  )
}


alea_plot_distribution_label <- function(x) {
  toupper(as.character(x))
}


alea_plot_method_label <- function(x) {
  toupper(as.character(x))
}


alea_plot_model_subtitle <- function(distribution, method) {
  distribution <- unique(stats::na.omit(as.character(distribution)))
  method <- unique(stats::na.omit(as.character(method)))
  
  if (length(distribution) == 1L && length(method) == 1L) {
    return(paste0(
      "Distribution: ",
      alea_plot_distribution_label(distribution),
      " | Method: ",
      alea_plot_method_label(method)
    ))
  }
  
  if (length(distribution) == 1L) {
    return(paste0(
      "Distribution: ",
      alea_plot_distribution_label(distribution)
    ))
  }
  
  if (length(method) == 1L) {
    return(paste0(
      "Method: ",
      alea_plot_method_label(method)
    ))
  }
  
  "Multiple fitted models"
}