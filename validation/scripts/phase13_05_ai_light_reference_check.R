# Phase 13 — FADS_AI light-model reference check
#
# Purpose:
# Validate bundled FADS_AI light-model availability, metadata, and
# validation-file consistency.
#
# This script is a validation artifact, not a user-facing package feature.
# It does not change the public API or package scope.

suppressPackageStartupMessages({
  library(ALEA)
})

# ---- Setup ------------------------------------------------------------------

output_dir <- file.path("validation", "reference_outputs")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

output_file <- file.path(output_dir, "phase13_ai_light_reference_check.csv")

# ---- Locate bundled files ----------------------------------------------------

model_path <- system.file(
  "extdata",
  "fads_ai",
  "fads_ai_application_model_light.rds",
  package = "ALEA",
  mustWork = TRUE
)

validation_path <- system.file(
  "extdata",
  "fads_ai",
  "fads_ai_application_model_light_validation.csv",
  package = "ALEA",
  mustWork = TRUE
)

# ---- Read public metadata ----------------------------------------------------

info <- alea_ai_model_info()

# ---- Validate model info structure ------------------------------------------

stopifnot(inherits(info, "alea_ai_model_info"))

# The object is intentionally checked through stable user-facing fields only.
info_names <- names(info)

required_fields <- c(
  "model_name",
  "model_version",
  "parent_model_name",
  "parent_model_version",
  "scenario",
  "algorithm",
  "candidate_distributions",
  "original_candidate_labels",
  "feature_set",
  "feature_columns",
  "model_file",
  "model_available",
  "validation_file",
  "validation_available",
  "validation",
  "model_size_note",
  "interpretation_note"
)

missing_fields <- setdiff(required_fields, info_names)
stopifnot(length(missing_fields) == 0)

# ---- Validate expected metadata ---------------------------------------------

stopifnot(identical(info$model_version, "1.0.0-light"))
stopifnot(identical(info$parent_model_version, "1.0.0"))
stopifnot(identical(info$scenario, "classical"))
stopifnot(identical(info$algorithm, "xgb"))

stopifnot(identical(
  info$candidate_distributions,
  c("gev", "gpa", "pe3", "ln2", "ln3", "gum")
))

stopifnot(identical(
  info$feature_columns,
  c("lmom_l1", "lmom_l2", "lmom_l3", "lmom_l4", "lmom_t3", "lmom_t4")
))

stopifnot(file.exists(model_path))
stopifnot(file.exists(validation_path))

# ---- Validate bundled validation file ---------------------------------------

validation <- read.csv(validation_path, stringsAsFactors = FALSE)

validation_names <- names(validation)

expected_validation_columns <- c(
  "n_validation_rows",
  "max_abs_probability_difference",
  "class_agreement"
)

missing_validation_columns <- setdiff(
  expected_validation_columns,
  validation_names
)

stopifnot(length(missing_validation_columns) == 0)

n_validation_rows <- unique(validation$n_validation_rows)
max_abs_probability_difference <- unique(validation$max_abs_probability_difference)
class_agreement <- unique(validation$class_agreement)

stopifnot(length(n_validation_rows) == 1)
stopifnot(length(max_abs_probability_difference) == 1)
stopifnot(length(class_agreement) == 1)

stopifnot(identical(as.integer(n_validation_rows), 1000L))
stopifnot(identical(as.numeric(max_abs_probability_difference), 0))
stopifnot(identical(as.numeric(class_agreement), 1))

# ---- Validate ordinary AI-selection workflow --------------------------------

x <- c(
  42.1, 39.4, 51.7, 48.3, 55.2,
  60.1, 46.8, 53.9, 58.4, 62.7,
  49.5, 57.8, 64.3, 52.6, 59.9,
  61.5, 67.2, 54.8, 63.1, 69.4
)

selection <- alea_select(x, quiet = TRUE)

stopifnot(inherits(selection, "alea_selection"))
stopifnot(selection$selected_distribution %in% c("gev", "gpa", "pe3", "ln2", "ln3", "gum"))

ranking <- selection$ranking

stopifnot(is.data.frame(ranking))
stopifnot(nrow(ranking) == 6)
stopifnot(all(ranking$distribution %in% c("gev", "gpa", "pe3", "ln2", "ln3", "gum")))
stopifnot(all(is.finite(ranking$probability)))

probability_sum <- sum(ranking$probability)

if (abs(probability_sum - 1) >= 1e-6) {
  message("Ranking probability sum is not 1.")
  message("Observed sum: ", probability_sum)
  print(ranking)
  stop("Phase 13 AI light validation needs ranking-probability adjustment.")
}

stopifnot(identical(sort(ranking$rank), 1:6))

# ---- Write compact reference output -----------------------------------------

reference <- data.frame(
  case_id = "P13-AI-LIGHT-001",
  check = c(
    "model_file_exists",
    "validation_file_exists",
    "model_version",
    "parent_model_version",
    "scenario",
    "algorithm",
    "candidate_distributions",
    "feature_columns",
    "n_validation_rows",
    "max_abs_probability_difference",
    "class_agreement",
    "alea_select_output",
    "ranking_probability_sum"
  ),
  observed = c(
    file.exists(model_path),
    file.exists(validation_path),
    info$model_version,
    info$parent_model_version,
    info$scenario,
    info$algorithm,
    paste(info$candidate_distributions, collapse = ","),
    paste(info$feature_columns, collapse = ","),
    as.character(n_validation_rows),
    as.character(max_abs_probability_difference),
    as.character(class_agreement),
    inherits(selection, "alea_selection"),
    as.character(sum(ranking$probability))
  ),
  expected = c(
    "TRUE",
    "TRUE",
    "1.0.0-light",
    "1.0.0",
    "classical",
    "xgb",
    "gev,gpa,pe3,ln2,ln3,gum",
    "lmom_l1,lmom_l2,lmom_l3,lmom_l4,lmom_t3,lmom_t4",
    "1000",
    "0",
    "1",
    "TRUE",
    "1"
  ),
  passed = c(
    file.exists(model_path),
    file.exists(validation_path),
    identical(info$model_version, "1.0.0-light"),
    identical(info$parent_model_version, "1.0.0"),
    identical(info$scenario, "classical"),
    identical(info$algorithm, "xgb"),
    identical(info$candidate_distributions, c("gev", "gpa", "pe3", "ln2", "ln3", "gum")),
    identical(info$feature_columns, c("lmom_l1", "lmom_l2", "lmom_l3", "lmom_l4", "lmom_t3", "lmom_t4")),
    identical(as.integer(n_validation_rows), 1000L),
    identical(as.numeric(max_abs_probability_difference), 0),
    identical(as.numeric(class_agreement), 1),
    inherits(selection, "alea_selection"),
    abs(sum(ranking$probability) - 1) < 1e-6
  )
)

stopifnot(all(reference$passed))

write.csv(
  reference,
  file = output_file,
  row.names = FALSE
)

message("Phase 13 FADS_AI light-model reference check completed successfully.")
message("Reference output written to: ", output_file)

print(reference)