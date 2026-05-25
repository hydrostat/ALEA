# Plot methods for alea_fit objects

#' Plot ALEA fitted models
#'
#' Produces diagnostic and fitted-distribution plots from an `alea_fit` object.
#'
#' @param x An object of class `alea_fit`.
#' @param type Character scalar. Plot type. One of `"density"`, `"cdf"`,
#'   `"qq"`, `"pp"`, or `"quantile"`.
#' @param return_period Numeric vector, `NULL`, or `NA`. Return periods used as
#'   x-axis ticks when `type = "quantile"`. If `NULL` or `NA`, ticks are
#'   chosen automatically from standard hydrological return periods.
#' @param return_period_scale Character scalar. Return-period axis scale when
#'   `type = "quantile"`. One of `"gumbel"`, `"log"`, or `"linear"`.
#' @param return_period_grid_n Integer scalar. Number of internal grid points
#'   used to draw a smooth fitted quantile curve.
#' @param plot_observed Logical scalar. If `TRUE`, observed plotting-position
#'   points are added to quantile plots.
#' @param plotting_position_a Numeric scalar. Plotting-position parameter `a`
#'   used for observed quantile points, with plotting positions
#'   `p_i = (i - a) / (n + 1 - 2 * a)` and empirical return periods
#'   `T_i = 1 / (1 - p_i)`.
#' @param n_grid Integer scalar. Number of grid points for fitted density and
#'   CDF curves.
#' @param bins Integer scalar. Number of histogram bins for `type = "density"`.
#' @param ... Additional arguments passed to methods.
#'
#' @return A `ggplot` object.
#'
#' @method plot alea_fit
#' @examples
#' x <- c(42.1, 38.5, 51.3, 47.0, 62.4, 55.2, 49.8, 58.1,
#'        60.3, 45.9)
#' fit <- alea_fit(x, distribution = "gev", method = "lmom")
#' plot(fit, type = "density")
#' plot(fit, type = "quantile")
#' plot(fit, type = "quantile", plot_observed = FALSE)
#'
#' @export
plot.alea_fit <- function(
    x,
    type = c("density", "cdf", "qq", "pp", "quantile"),
    return_period = NULL,
    return_period_scale = c("gumbel", "log", "linear"),
    return_period_grid_n = 200,
    plot_observed = TRUE,
    plotting_position_a = 0.44,
    n_grid = 200,
    bins = 30,
    ...
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required to plot ALEA objects.", call. = FALSE)
  }
  
  if (!inherits(x, "alea_fit")) {
    stop("`x` must be an object of class 'alea_fit'.", call. = FALSE)
  }
  
  type <- match.arg(type)
  return_period_scale <- match.arg(return_period_scale)
  
  data <- validate_fit_plot_data(x)
  parameters <- fit_parameters_for_plot(x)
  
  validate_alea_fit_plot_input(
    n_grid = n_grid,
    bins = bins,
    return_period_grid_n = return_period_grid_n,
    plot_observed = plot_observed,
    plotting_position_a = plotting_position_a
  )
  
  if (type == "density") {
    return(plot_alea_fit_density(
      data = data,
      distribution = x$distribution,
      method = x$method,
      parameters = parameters,
      n_grid = n_grid,
      bins = bins
    ))
  }
  
  if (type == "cdf") {
    return(plot_alea_fit_cdf(
      data = data,
      distribution = x$distribution,
      method = x$method,
      parameters = parameters,
      n_grid = n_grid
    ))
  }
  
  if (type == "qq") {
    return(plot_alea_fit_qq(
      data = data,
      distribution = x$distribution,
      method = x$method,
      parameters = parameters
    ))
  }
  
  if (type == "pp") {
    return(plot_alea_fit_pp(
      data = data,
      distribution = x$distribution,
      method = x$method,
      parameters = parameters
    ))
  }
  
  if (type == "quantile") {
    type <- "quantile"
  }

  if (type == "quantile") {
    return(plot_alea_fit_quantile(
      x = x,
      return_period = return_period,
      return_period_scale = return_period_scale,
      return_period_grid_n = return_period_grid_n,
      plot_observed = plot_observed,
      plotting_position_a = plotting_position_a
    ))
  }
  
  stop("Unsupported plot type.", call. = FALSE)
}


validate_alea_fit_plot_input <- function(
    n_grid,
    bins,
    return_period_grid_n,
    plot_observed,
    plotting_position_a
) {
  if (!is.numeric(n_grid) || length(n_grid) != 1L || !is.finite(n_grid)) {
    stop("`n_grid` must be a finite numeric scalar.", call. = FALSE)
  }
  
  if (n_grid < 20L) {
    stop("`n_grid` must be at least 20.", call. = FALSE)
  }
  
  if (!is.numeric(bins) || length(bins) != 1L || !is.finite(bins)) {
    stop("`bins` must be a finite numeric scalar.", call. = FALSE)
  }
  
  if (bins < 1L) {
    stop("`bins` must be positive.", call. = FALSE)
  }
  
  if (
    !is.numeric(return_period_grid_n) ||
    length(return_period_grid_n) != 1L ||
    !is.finite(return_period_grid_n)
  ) {
    stop("`return_period_grid_n` must be a finite numeric scalar.", call. = FALSE)
  }
  
  if (return_period_grid_n < 20L) {
    stop("`return_period_grid_n` must be at least 20.", call. = FALSE)
  }
  

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


validate_fit_plot_data <- function(x) {
  if (is.null(x$data)) {
    stop("`x$data` is missing. Cannot create plot.", call. = FALSE)
  }
  
  data <- x$data
  
  if (!is.numeric(data)) {
    stop("`x$data` must be numeric.", call. = FALSE)
  }
  
  data <- data[is.finite(data)]
  
  if (length(data) < 2L) {
    stop("At least two finite observations are required to create a plot.", call. = FALSE)
  }
  
  if (length(unique(data)) < 2L) {
    stop("At least two unique finite observations are required to create a plot.", call. = FALSE)
  }
  
  data
}


fit_parameters_for_plot <- function(x) {
  parameters <- NULL
  
  if (!is.null(x$parameters)) {
    parameters <- x$parameters
  } else {
    parameters <- stats::coef(x)
  }
  
  if (is.null(parameters)) {
    stop("Fitted parameters are missing from the 'alea_fit' object.", call. = FALSE)
  }
  
  parameters <- unlist(parameters, recursive = FALSE, use.names = TRUE)
  
  if (!is.numeric(parameters) || any(!is.finite(parameters))) {
    stop("Fitted parameters must be finite numeric values.", call. = FALSE)
  }
  
  parameters
}


plot_alea_fit_density <- function(data, distribution, method, parameters, n_grid, bins) {
  x_grid <- regular_data_grid(data, n_grid = n_grid)
  
  density <- evaluate_fit_density(
    distribution = distribution,
    x = x_grid,
    parameters = parameters
  )
  
  observed <- data.frame(value = data)
  fitted <- data.frame(value = x_grid, density = density)
  
  ggplot2::ggplot(observed, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      bins = as.integer(bins),
      fill = alea_plot_color("histogram"),
      color = "white",
      linewidth = 0.25,
      alpha = 0.55
    ) +
    ggplot2::geom_line(
      data = fitted,
      ggplot2::aes(x = value, y = density),
      linewidth = 0.5,
      color = alea_plot_color("fitted"),
      inherit.aes = FALSE
    ) +
    ggplot2::labs(
      title = "Fitted density",
      subtitle = alea_plot_model_subtitle(distribution, method),
      x = "Observed value",
      y = "Density"
    ) +
    alea_plot_theme()
}


plot_alea_fit_cdf <- function(data, distribution, method, parameters, n_grid) {
  x_grid <- regular_data_grid(data, n_grid = n_grid)
  
  fitted_cdf <- evaluate_fit_cdf(
    distribution = distribution,
    q = x_grid,
    parameters = parameters
  )
  
  data_sorted <- sort(data)
  empirical <- stats::ecdf(data_sorted)
  
  observed <- data.frame(
    value = data_sorted,
    probability = empirical(data_sorted)
  )
  
  fitted <- data.frame(
    value = x_grid,
    probability = fitted_cdf
  )
  
  ggplot2::ggplot() +
    ggplot2::geom_step(
      data = observed,
      ggplot2::aes(x = value, y = probability),
      direction = "hv",
      linewidth = 0.3,
      color = alea_plot_color("observed"),
      alpha = 0.9
    ) +
    ggplot2::geom_line(
      data = fitted,
      ggplot2::aes(x = value, y = probability),
      linewidth = 0.5,
      color = alea_plot_color("fitted")
    ) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(
      title = "Fitted distribution function",
      subtitle = alea_plot_model_subtitle(distribution, method),
      x = "Observed value",
      y = "Cumulative probability"
    ) +
    alea_plot_theme()
}


plot_alea_fit_qq <- function(data, distribution, method, parameters) {
  data_sorted <- sort(data)
  probabilities <- stats::ppoints(length(data_sorted))
  
  theoretical <- evaluate_fit_quantile(
    distribution = distribution,
    p = probabilities,
    parameters = parameters
  )
  
  plot_data <- data.frame(
    theoretical = theoretical,
    observed = data_sorted
  )
  
  finite_rows <- is.finite(plot_data$theoretical) & is.finite(plot_data$observed)
  plot_data <- plot_data[finite_rows, , drop = FALSE]
  
  if (nrow(plot_data) < 2L) {
    stop("Could not compute enough finite fitted quantiles for a Q-Q plot.", call. = FALSE)
  }
  
  reference_range <- range(c(plot_data$theoretical, plot_data$observed), finite = TRUE)
  
  ggplot2::ggplot(plot_data, ggplot2::aes(x = theoretical, y = observed)) +
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      linewidth = 0.5,
      color = alea_plot_color("neutral")
    ) +
    ggplot2::geom_point(
      size = 1.0,
      color = alea_plot_color("observed"),
      alpha = 0.85
    ) +
    ggplot2::coord_equal(xlim = reference_range, ylim = reference_range) +
    ggplot2::labs(
      title = "Q-Q plot",
      subtitle = alea_plot_model_subtitle(distribution, method),
      x = "Theoretical quantile",
      y = "Observed quantile"
    ) +
    alea_plot_theme()
}


plot_alea_fit_pp <- function(data, distribution, method, parameters) {
  data_sorted <- sort(data)
  empirical_probability <- stats::ppoints(length(data_sorted))
  
  fitted_probability <- evaluate_fit_cdf(
    distribution = distribution,
    q = data_sorted,
    parameters = parameters
  )
  
  plot_data <- data.frame(
    fitted_probability = fitted_probability,
    empirical_probability = empirical_probability
  )
  
  finite_rows <- is.finite(plot_data$fitted_probability) &
    is.finite(plot_data$empirical_probability)
  
  plot_data <- plot_data[finite_rows, , drop = FALSE]
  
  if (nrow(plot_data) < 2L) {
    stop("Could not compute enough finite fitted probabilities for a P-P plot.", call. = FALSE)
  }
  
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = fitted_probability, y = empirical_probability)
  ) +
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      linewidth = 0.5,
      color = alea_plot_color("neutral")
    ) +
    ggplot2::geom_point(
      size = 1.0,
      color = alea_plot_color("observed"),
      alpha = 0.85
    ) +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(
      title = "P-P plot",
      subtitle = alea_plot_model_subtitle(distribution, method),
      x = "Fitted probability",
      y = "Empirical probability"
    ) +
    alea_plot_theme()
}


plot_alea_fit_quantile <- function(
    x,
    return_period = NULL,
    return_period_scale = c("gumbel", "log", "linear"),
    return_period_grid_n = 200,
    plot_observed = TRUE,
    plotting_position_a = 0.44
) {
  return_period_scale <- match.arg(return_period_scale)
  
  data <- validate_fit_plot_data(x)
  
  observed <- make_observed_quantile_data(
    data = data,
    plotting_position_a = plotting_position_a
  )
  
  return_period_ticks <- resolve_return_period_ticks(
    return_period = return_period,
    observed_return_period = observed$return_period,
    return_period_scale = return_period_scale
  )
  
  max_requested_return_period <- max(return_period_ticks, na.rm = TRUE)
  
  observed <- filter_observed_quantiles(
    observed = observed,
    max_return_period = max_requested_return_period
  )
  
  model_return_period <- make_model_return_period_grid(
    return_period_ticks = return_period_ticks,
    n_grid = return_period_grid_n
  )
  
  model <- alea_quantile(
    object = x,
    return_period = model_return_period
  )
  
  model <- as.data.frame(model)
  
  observed$return_period_axis <- transform_return_period_axis(
    return_period = observed$return_period,
    scale = return_period_scale
  )
  
  model$return_period_axis <- transform_return_period_axis(
    return_period = model$return_period,
    scale = return_period_scale
  )
  
  return_period_tick_positions <- transform_return_period_axis(
    return_period = return_period_ticks,
    scale = return_period_scale
  )
  
  p <- ggplot2::ggplot()

  if (isTRUE(plot_observed)) {
    p <- p +
      ggplot2::geom_point(
        data = observed,
        ggplot2::aes(x = return_period_axis, y = quantile),
        shape = 21,
        size = 1.0,
        stroke = 0.6,
        color = alea_plot_color("observed"),
        fill = "white",
        alpha = 0.9
      )
  }

  p +
    ggplot2::geom_line(
      data = model,
      ggplot2::aes(x = return_period_axis, y = quantile),
      linewidth = 0.5,
      color = alea_plot_color("fitted")
    ) +
    ggplot2::scale_x_continuous(
      breaks = return_period_tick_positions,
      labels = return_period_ticks
    ) +
    ggplot2::labs(
      title = "Quantile plot",
      subtitle = alea_plot_model_subtitle(x$distribution, x$method),
      x = "Return period",
      y = "Quantile"
    ) +
    alea_plot_theme()
}


regular_data_grid <- function(data, n_grid) {
  data_range <- range(data, finite = TRUE)
  span <- diff(data_range)
  
  if (!is.finite(span) || span <= 0) {
    stop("Cannot create a plotting grid from a zero-range sample.", call. = FALSE)
  }
  
  padding <- 0.05 * span
  
  seq(
    from = data_range[1L] - padding,
    to = data_range[2L] + padding,
    length.out = as.integer(n_grid)
  )
}


evaluate_fit_density <- function(distribution, x, parameters) {
  function_name <- paste0("d_", distribution, "_internal")
  fun <- get_internal_distribution_function(function_name)
  
  values <- call_distribution_function(
    fun = fun,
    value_name = "x",
    value = x,
    parameters = parameters
  )
  
  values <- as.numeric(values)
  values[!is.finite(values)] <- NA_real_
  
  values
}


evaluate_fit_cdf <- function(distribution, q, parameters) {
  function_name <- paste0("p_", distribution, "_internal")
  fun <- get_internal_distribution_function(function_name)
  
  values <- call_distribution_function(
    fun = fun,
    value_name = "q",
    value = q,
    parameters = parameters
  )
  
  values <- as.numeric(values)
  values[!is.finite(values)] <- NA_real_
  
  pmin(pmax(values, 0), 1)
}


evaluate_fit_quantile <- function(distribution, p, parameters) {
  function_name <- paste0("q_", distribution, "_internal")
  fun <- get_internal_distribution_function(function_name)
  
  values <- call_distribution_function(
    fun = fun,
    value_name = "p",
    value = p,
    parameters = parameters
  )
  
  as.numeric(values)
}


get_internal_distribution_function <- function(function_name) {
  if (!exists(function_name, mode = "function")) {
    stop(
      "Internal distribution function '", function_name, "' was not found.",
      call. = FALSE
    )
  }
  
  get(function_name, mode = "function")
}


call_distribution_function <- function(fun, value_name, value, parameters) {
  parameters_list <- as.list(parameters)
  
  attempts <- list(
    function() do.call(fun, c(stats::setNames(list(value), value_name), parameters_list)),
    function() do.call(fun, c(list(value), parameters_list)),
    function() fun(value, parameters),
    function() fun(value, parameters_list),
    function() fun(value, par = parameters),
    function() fun(value, par = parameters_list),
    function() fun(value, params = parameters),
    function() fun(value, params = parameters_list)
  )
  
  last_error <- NULL
  
  for (attempt in attempts) {
    result <- tryCatch(
      attempt(),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )
    
    if (!is.null(result)) {
      return(result)
    }
  }
  
  stop(
    "Could not evaluate the fitted distribution. Last error: ",
    conditionMessage(last_error),
    call. = FALSE
  )
}


default_return_period_ticks <- function() {
  c(2, 5, 10, 25, 50, 100, 200, 500, 1000, 2000, 5000, 10000)
}


resolve_return_period_ticks <- function(
    return_period,
    observed_return_period,
    return_period_scale = c("gumbel", "log", "linear")
) {
  return_period_scale <- match.arg(return_period_scale)
  reference_ticks <- default_return_period_ticks()
  
  automatic <- is.null(return_period) ||
    length(return_period) < 1L ||
    all(is.na(return_period))
  
  if (automatic) {
    max_observed <- max(observed_return_period, na.rm = TRUE)
    
    if (!is.finite(max_observed)) {
      stop("Could not determine observed return periods.", call. = FALSE)
    }
    
    if (return_period_scale == "linear") {
      max_tick <- ceiling(max_observed / 10) * 10
      max_tick <- max(max_tick, 10)
      
      ticks <- pretty(c(0, max_tick), n = 6)
      ticks <- ticks[ticks > 1 & ticks <= max_tick]
      
      if (!max_tick %in% ticks) {
        ticks <- sort(unique(c(ticks, max_tick)))
      }
      
      return(ticks)
    }
    
    selected_ticks <- reference_ticks[reference_ticks <= max_observed]
    next_tick <- reference_ticks[reference_ticks > max_observed][1L]
    
    if (length(selected_ticks) < 1L) {
      selected_ticks <- reference_ticks[1L]
    }
    
    if (!is.na(next_tick)) {
      selected_ticks <- unique(c(selected_ticks, next_tick))
    }
    
    return(selected_ticks)
  }
  
  if (!is.numeric(return_period)) {
    stop("`return_period` must be numeric, `NULL`, or `NA`.", call. = FALSE)
  }
  
  if (any(!is.finite(return_period)) || any(return_period <= 1)) {
    stop("All `return_period` values must be finite and greater than 1.", call. = FALSE)
  }
  
  sort(unique(return_period))
}


transform_return_period_axis <- function(
    return_period,
    scale = c("gumbel", "log", "linear")
) {
  scale <- match.arg(scale)
  
  if (scale == "gumbel") {
    return(-log(-log(1 - 1 / return_period)))
  }
  
  if (scale == "log") {
    return(log10(return_period))
  }
  
  return_period
}


make_observed_quantile_data <- function(data, plotting_position_a = 0.44) {
  data <- sort(data)
  
  probability <- stats::ppoints(
    n = length(data),
    a = plotting_position_a
  )
  
  return_period <- 1 / (1 - probability)
  
  data.frame(
    return_period = return_period,
    quantile = data
  )
}


make_model_return_period_grid <- function(
    return_period_ticks,
    n_grid = 200,
    minimum_return_period = 1.01
) {
  max_return_period <- max(return_period_ticks, na.rm = TRUE)
  
  if (!is.finite(max_return_period) || max_return_period <= minimum_return_period) {
    stop("Could not define a valid return-period grid.", call. = FALSE)
  }
  
  seq(
    from = minimum_return_period,
    to = max_return_period,
    length.out = as.integer(n_grid)
  )
}


filter_observed_quantiles <- function(observed, max_return_period) {
  keep <- observed$return_period <= max_return_period
  
  if (any(!keep)) {
    warning(
      "Some observed plotting positions have return periods larger than the largest requested `return_period` and were not plotted.",
      call. = FALSE
    )
  }
  
  observed[keep, , drop = FALSE]
}