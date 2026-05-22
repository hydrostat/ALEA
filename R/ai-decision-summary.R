# AI decision-support summaries --------------------------------------------
#
# Internal ALEA-R helpers for expanding FADS_AI model predictions into
# decision-support summaries. These summaries are heuristic and should be
# interpreted together with GOF, diagnostics, quantile uncertainty, and
# hydrological judgement.

classify_ai_margin <- function(margin, thresholds = c(0.10, 0.30, 0.60)) {
  if (!is.numeric(thresholds) || length(thresholds) != 3L ||
      any(!is.finite(thresholds)) || is.unsorted(thresholds, strictly = TRUE)) {
    stop("`thresholds` must be three strictly increasing finite numeric values.", call. = FALSE)
  }

  if (!is.finite(margin)) {
    return(NA_character_)
  }

  if (margin < thresholds[[1L]]) {
    return("ambiguous support")
  }

  if (margin < thresholds[[2L]]) {
    return("weak to moderate support")
  }

  if (margin < thresholds[[3L]]) {
    return("moderate support")
  }

  "strong support"
}

summarize_ai_prediction <- function(pred_tbl, thresholds = c(0.10, 0.30, 0.60)) {
  if (!is.data.frame(pred_tbl)) {
    stop("`pred_tbl` must be a data frame.", call. = FALSE)
  }

  if (!".pred_class" %in% names(pred_tbl)) {
    stop("`pred_tbl` must contain `.pred_class`.", call. = FALSE)
  }

  prob_cols <- grep("^\\.pred_", names(pred_tbl), value = TRUE)
  prob_cols <- setdiff(prob_cols, ".pred_class")

  if (length(prob_cols) == 0L) {
    stop("No probability columns found in `pred_tbl`.", call. = FALSE)
  }

  out <- vector("list", nrow(pred_tbl))

  for (i in seq_len(nrow(pred_tbl))) {
    prob_values <- as.numeric(pred_tbl[i, prob_cols, drop = TRUE])
    families <- sub("^\\.pred_", "", prob_cols)

    ord <- order(prob_values, decreasing = TRUE)
    families <- families[ord]
    prob_values <- prob_values[ord]

    top_family <- families[[1L]]
    top_support <- prob_values[[1L]]

    second_family <- if (length(families) >= 2L) families[[2L]] else NA_character_
    second_support <- if (length(prob_values) >= 2L) prob_values[[2L]] else NA_real_

    margin <- top_support - second_support
    decision_strength <- classify_ai_margin(margin, thresholds = thresholds)

    out[[i]] <- data.frame(
      row_id = i,
      predicted_family = as.character(pred_tbl$.pred_class[[i]]),
      top_family = top_family,
      top_support = top_support,
      second_family = second_family,
      second_support = second_support,
      top1_top2_margin = margin,
      decision_strength = decision_strength,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, out)
}

make_ai_interpretation_text <- function(summary_row) {
  if (!is.data.frame(summary_row) || nrow(summary_row) != 1L) {
    stop("`summary_row` must be a one-row data frame.", call. = FALSE)
  }

  top_family <- summary_row$top_family[[1L]]
  top_support <- summary_row$top_support[[1L]]
  second_family <- summary_row$second_family[[1L]]
  second_support <- summary_row$second_support[[1L]]
  margin <- summary_row$top1_top2_margin[[1L]]
  strength <- summary_row$decision_strength[[1L]]

  if (!is.finite(margin)) {
    return(
      "FADS_AI produced a distribution-selection output, but the decision margin could not be computed."
    )
  }

  if (identical(strength, "ambiguous support")) {
    return(
      paste0(
        "FADS_AI assigned the highest model-based support to ",
        top_family,
        " (", round(top_support, 3), "), followed closely by ",
        second_family,
        " (", round(second_support, 3), "). The top1-top2 margin was ",
        round(margin, 3),
        ", indicating an ambiguous distribution-selection signal. ",
        "Both leading families should be retained for further diagnostic comparison, ",
        "quantile-sensitivity analysis, and hydrological judgement."
      )
    )
  }

  paste0(
    "FADS_AI assigned the highest model-based support to ",
    top_family,
    " (", round(top_support, 3), "). The second-ranked family was ",
    second_family,
    " (", round(second_support, 3), "). The top1-top2 margin was ",
    round(margin, 3),
    ", corresponding to ",
    strength,
    ". This result should be interpreted as decision-support evidence and evaluated ",
    "together with goodness-of-fit diagnostics, L-moment behavior, quantile uncertainty, ",
    "and hydrological judgement."
  )
}

add_ai_interpretation <- function(summary_tbl) {
  summary_tbl$interpretation <- vapply(
    seq_len(nrow(summary_tbl)),
    function(i) make_ai_interpretation_text(summary_tbl[i, , drop = FALSE]),
    character(1)
  )

  summary_tbl
}
