#' Goodness-of-fit statistics for ALEA fitted models
#'
#' Computes goodness-of-fit statistics and information criteria for an ALEA
#' fitted model.
#'
#' The initial implementation supports objects of class `alea_fit`.
#'
#' @param object An object returned by `alea_fit`.
#' @param statistics Character vector with statistics to compute. Supported
#'   values are `"ks"`, `"cvm"`, `"ad"`, `"loglik"`, `"aic"`, and `"bic"`.
#'   Use `"all"` to compute all supported statistics.
#' @param ... Additional arguments passed to methods. Currently unused.
#'
#' @return A data frame with S3 class `alea_gof`.
#'
#' @export
alea_gof <- function(object, statistics = c("ks", "cvm", "ad", "loglik", "aic", "bic"), ...) {
  UseMethod("alea_gof")
}


#' @rdname alea_gof
#' @export
alea_gof.alea_fit <- function(
    object,
    statistics = c("ks", "cvm", "ad", "loglik", "aic", "bic"),
    ...
) {
  check_alea_gof_fit_object(object)
  
  statistics <- check_gof_statistics(statistics)
  
  x <- as.numeric(object$data)
  distribution <- as.character(object$distribution)
  method <- as.character(object$method)
  para <- object$parameters
  n <- length(x)
  n_parameters <- length(para)
  
  out <- list()
  
  if (any(statistics %in% c("ks", "cvm", "ad"))) {
    u <- gof_cdf_values(
      x = x,
      distribution = distribution,
      para = para
    )
    
    edf <- gof_edf_statistics_from_u(u)
    
    if ("ks" %in% statistics) {
      out[[length(out) + 1L]] <- new_gof_row(
        distribution = distribution,
        method = method,
        statistic = "ks",
        estimate = edf$ks,
        p_value = NA_real_,
        p_value_method = NA_character_,
        n = n,
        n_parameters = n_parameters,
        higher_is_better = FALSE,
        description = "Kolmogorov-Smirnov statistic"
      )
    }
    
    if ("cvm" %in% statistics) {
      out[[length(out) + 1L]] <- new_gof_row(
        distribution = distribution,
        method = method,
        statistic = "cvm",
        estimate = edf$cvm,
        p_value = NA_real_,
        p_value_method = NA_character_,
        n = n,
        n_parameters = n_parameters,
        higher_is_better = FALSE,
        description = "Cramer-von Mises statistic"
      )
    }
    
    if ("ad" %in% statistics) {
      out[[length(out) + 1L]] <- new_gof_row(
        distribution = distribution,
        method = method,
        statistic = "ad",
        estimate = edf$ad,
        p_value = NA_real_,
        p_value_method = NA_character_,
        n = n,
        n_parameters = n_parameters,
        higher_is_better = FALSE,
        description = "Anderson-Darling statistic"
      )
    }
  }
  
  if (any(statistics %in% c("loglik", "aic", "bic"))) {
    loglik <- gof_loglik(
      x = x,
      distribution = distribution,
      para = para
    )
    
    if ("loglik" %in% statistics) {
      out[[length(out) + 1L]] <- new_gof_row(
        distribution = distribution,
        method = method,
        statistic = "loglik",
        estimate = loglik,
        p_value = NA_real_,
        p_value_method = NA_character_,
        n = n,
        n_parameters = n_parameters,
        higher_is_better = TRUE,
        description = "Log-likelihood"
      )
    }
    
    if ("aic" %in% statistics) {
      out[[length(out) + 1L]] <- new_gof_row(
        distribution = distribution,
        method = method,
        statistic = "aic",
        estimate = gof_aic(loglik = loglik, n_parameters = n_parameters),
        p_value = NA_real_,
        p_value_method = NA_character_,
        n = n,
        n_parameters = n_parameters,
        higher_is_better = FALSE,
        description = "Akaike information criterion"
      )
    }
    
    if ("bic" %in% statistics) {
      out[[length(out) + 1L]] <- new_gof_row(
        distribution = distribution,
        method = method,
        statistic = "bic",
        estimate = gof_bic(loglik = loglik, n_parameters = n_parameters, n = n),
        p_value = NA_real_,
        p_value_method = NA_character_,
        n = n,
        n_parameters = n_parameters,
        higher_is_better = FALSE,
        description = "Bayesian information criterion"
      )
    }
  }
  
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  
  class(out) <- c("alea_gof", "data.frame")
  out
}


#' @export
print.alea_gof <- function(x, ...) {
  cat("Goodness-of-fit results\n")
  cat("Distribution:", unique(x$distribution), "\n")
  cat("Method:", unique(x$method), "\n")
  cat("Number of observations:", unique(x$n), "\n\n")
  
  print.data.frame(as.data.frame(x), row.names = FALSE, ...)
  invisible(x)
}


#' @export
as.data.frame.alea_gof <- function(x, ...) {
  class(x) <- "data.frame"
  x
}


check_alea_gof_fit_object <- function(object) {
  if (!inherits(object, "alea_fit")) {
    stop("`object` must be an object of class 'alea_fit'.", call. = FALSE)
  }
  
  required_fields <- c("data", "distribution", "method", "parameters", "n")
  missing_fields <- setdiff(required_fields, names(object))
  
  if (length(missing_fields) > 0L) {
    stop(
      "`object` is missing required field(s): ",
      paste(missing_fields, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if (!is.numeric(object$data) || length(object$data) == 0L) {
    stop("`object$data` must be a non-empty numeric vector.", call. = FALSE)
  }
  
  if (any(!is.finite(object$data))) {
    stop("`object$data` must contain only finite values.", call. = FALSE)
  }
  
  if (!is.character(object$distribution) || length(object$distribution) != 1L) {
    stop("`object$distribution` must be a single character value.", call. = FALSE)
  }
  
  if (!is.character(object$method) || length(object$method) != 1L) {
    stop("`object$method` must be a single character value.", call. = FALSE)
  }
  
  if (!is.numeric(object$parameters) || length(object$parameters) == 0L) {
    stop("`object$parameters` must be a non-empty numeric vector.", call. = FALSE)
  }
  
  invisible(TRUE)
}


check_gof_statistics <- function(statistics) {
  allowed <- c("ks", "cvm", "ad", "loglik", "aic", "bic")
  
  if (is.null(statistics) || length(statistics) == 0L) {
    stop("`statistics` must contain at least one statistic.", call. = FALSE)
  }
  
  if (!is.character(statistics)) {
    stop("`statistics` must be a character vector.", call. = FALSE)
  }
  
  statistics <- tolower(statistics)
  
  if (any(statistics == "all")) {
    statistics <- allowed
  }
  
  unknown <- setdiff(statistics, allowed)
  
  if (length(unknown) > 0L) {
    stop(
      "Unsupported GOF statistic(s): ",
      paste(unknown, collapse = ", "),
      ". Supported values are: ",
      paste(allowed, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  unique(statistics)
}


new_gof_row <- function(
    distribution,
    method,
    statistic,
    estimate,
    p_value,
    p_value_method,
    n,
    n_parameters,
    higher_is_better,
    description
) {
  data.frame(
    distribution = distribution,
    method = method,
    statistic = statistic,
    estimate = as.numeric(estimate),
    p_value = as.numeric(p_value),
    p_value_method = as.character(p_value_method),
    n = as.integer(n),
    n_parameters = as.integer(n_parameters),
    higher_is_better = as.logical(higher_is_better),
    description = description,
    stringsAsFactors = FALSE
  )
}


gof_clamp_probability <- function(p, eps = 1e-12) {
  if (!is.numeric(p)) {
    stop("`p` must be numeric.", call. = FALSE)
  }
  
  if (!is.numeric(eps) || length(eps) != 1L || !is.finite(eps) || eps <= 0 || eps >= 0.5) {
    stop("`eps` must be a single finite number in (0, 0.5).", call. = FALSE)
  }
  
  pmin(pmax(p, eps), 1 - eps)
}


gof_cdf_values <- function(x, distribution, para) {
  distribution <- tolower(distribution)
  
  p <- switch(
    distribution,
    gev = p_gev_internal(x, para),
    gpa = p_gpa_internal(x, para),
    pe3 = p_pe3_internal(x, para),
    ln2 = p_ln2_internal(x, para),
    ln3 = p_ln3_internal(x, para),
    gum = p_gum_internal(x, para),
    stop("Unsupported distribution: ", distribution, ".", call. = FALSE)
  )
  
  if (!is.numeric(p) || length(p) != length(x)) {
    stop("Internal CDF returned an invalid result.", call. = FALSE)
  }
  
  if (any(!is.finite(p))) {
    stop("Internal CDF returned non-finite values.", call. = FALSE)
  }
  
  gof_clamp_probability(as.numeric(p))
}


gof_density_values <- function(x, distribution, para) {
  distribution <- tolower(distribution)
  
  dens <- switch(
    distribution,
    gev = d_gev_internal(x, para),
    gpa = d_gpa_internal(x, para),
    pe3 = d_pe3_internal(x, para),
    ln2 = d_ln2_internal(x, para),
    ln3 = d_ln3_internal(x, para),
    gum = d_gum_internal(x, para),
    stop("Unsupported distribution: ", distribution, ".", call. = FALSE)
  )
  
  if (!is.numeric(dens) || length(dens) != length(x)) {
    stop("Internal density returned an invalid result.", call. = FALSE)
  }
  
  as.numeric(dens)
}


gof_edf_statistics_from_u <- function(u) {
  if (!is.numeric(u)) {
    stop("`u` must be numeric.", call. = FALSE)
  }
  
  u <- sort(gof_clamp_probability(as.numeric(u)))
  
  if (any(!is.finite(u))) {
    stop("`u` must contain only finite values.", call. = FALSE)
  }
  
  n <- length(u)
  
  if (n == 0L) {
    stop("`u` must contain at least one value.", call. = FALSE)
  }
  
  i <- seq_len(n)
  
  d_plus <- max(i / n - u)
  d_minus <- max(u - (i - 1) / n)
  ks <- max(d_plus, d_minus)
  
  cvm <- 1 / (12 * n) + sum((u - (2 * i - 1) / (2 * n))^2)
  
  ad_sum <- sum((2 * i - 1) * (log(u) + log(1 - rev(u))))
  ad <- -n - ad_sum / n
  
  list(
    ks = as.numeric(ks),
    cvm = as.numeric(cvm),
    ad = as.numeric(ad)
  )
}


gof_loglik <- function(x, distribution, para, small = .Machine$double.xmin) {
  dens <- gof_density_values(
    x = x,
    distribution = distribution,
    para = para
  )
  
  dens[!is.finite(dens) | dens <= 0] <- small
  
  as.numeric(sum(log(dens)))
}


gof_aic <- function(loglik, n_parameters) {
  if (!is.finite(loglik)) {
    return(NA_real_)
  }
  
  -2 * loglik + 2 * n_parameters
}


gof_bic <- function(loglik, n_parameters, n) {
  if (!is.finite(loglik)) {
    return(NA_real_)
  }
  
  -2 * loglik + log(n) * n_parameters
}