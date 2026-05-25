# Internal helpers for stable ordering of ALEA user-facing tables.

order_alea_model_table_internal <- function(x) {
  if (!is.data.frame(x) || nrow(x) == 0L) {
    return(x)
  }

  order_parts <- list()

  add_order_part <- function(value) {
    order_parts[[length(order_parts) + 1L]] <<- value
  }

  if ("station" %in% names(x)) {
    add_order_part(as.character(x$station))
  }

  if ("distribution" %in% names(x)) {
    add_order_part(as.character(x$distribution))
  }

  if ("method" %in% names(x)) {
    add_order_part(as.character(x$method))
  }

  if ("return_period" %in% names(x)) {
    add_order_part(x$return_period)
  }

  if ("statistic" %in% names(x)) {
    statistic_order <- c("ks", "cvm", "ad", "loglik", "aic", "bic")
    idx <- match(as.character(x$statistic), statistic_order)
    idx[is.na(idx)] <- length(statistic_order) + seq_len(sum(is.na(idx)))
    add_order_part(idx)
  }

  if ("diagnostic" %in% names(x)) {
    diagnostic_order <- c(
      "sample_size", "missing", "ties", "range", "skewness",
      "randomness", "independence", "homogeneity", "stationarity"
    )
    idx <- match(as.character(x$diagnostic), diagnostic_order)
    idx[is.na(idx)] <- length(diagnostic_order) + seq_len(sum(is.na(idx)))
    add_order_part(idx)
  }

  if ("rank" %in% names(x)) {
    add_order_part(x$rank)
  }

  if ("fit_index" %in% names(x)) {
    add_order_part(x$fit_index)
  }

  if (length(order_parts) == 0L) {
    return(x)
  }

  ord <- do.call(base::order, c(unname(order_parts), list(na.last = TRUE)))
  out <- x[ord, , drop = FALSE]
  row.names(out) <- NULL
  out
}
