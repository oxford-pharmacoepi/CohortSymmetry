getComparedResult <- function(x) {
  x <- x |>
    dplyr::mutate(
      result_type = "sequence_symmetry",
      package_name = "CohortSymmetry",
      package_version = as.character(utils::packageVersion("CohortSymmetry")),
      group_name_reference = "index_cohort_name",
      group_name_comparator = "marker_cohort_name",
      strata_name_reference = "overall", # to update
      strata_level_reference = "overall", # to update
      strata_name_comparator = "overall", # to update
      strata_level_comparator = "overall" # to update
    ) |>
    dplyr::rename(
      "group_level_reference" = "index_name",
      "group_level_comparator" = "marker_name",
      "index_first_count" = "index_first",
      "marker_first_count" = "marker_first"
    ) |>
    tidyr::pivot_longer(
      cols = c("index_first_count", "index_first_percentage",
               "marker_first_count", "marker_first_percentage",
               "csr", "asr", "lowerCSR_CI", "upperCSR_CI", "lowerASR_CI",
               "upperASR_CI"),
      names_to = "variable_name",
      values_to = "estimate_value"
    ) |>
    dplyr::mutate(
      variable_level = dplyr::case_when(
        grepl("csr", .data$variable_name, ignore.case = TRUE) ~ "crude",
        grepl("asr", .data$variable_name, ignore.case = TRUE) ~ "adjusted",
        grepl("index", .data$variable_name) ~ "index",
        grepl("marker", .data$variable_name) ~ "marker"
      ),
      estimate_name = dplyr::case_when(
        .data$variable_name %in% c("csr", "asr") ~ "sr",
        grepl("lower", .data$variable_name) ~ "lower_CI",
        grepl("upper", .data$variable_name) ~ "upper_CI",
        grepl("count", .data$variable_name) ~ "count",
        grepl("percentage", .data$variable_name) ~ "percentage"
      ),
      estimate_type = dplyr::if_else(
        grepl("count", .data$variable_name),
        "integer", "numeric"),
      variable_name = dplyr::if_else(
        .data$variable_name %in%  c("csr", "asr", "lowerCSR_CI", "upperCSR_CI",
                                    "lowerASR_CI", "upperASR_CI"),
        "sequence_ratio", "first_pharmac"
      ),
      estimate_value = as.character(.data$estimate_value)
    ) |>
    visOmopResults::uniteNameLevel(
      cols = c("days_prior_observation", "washout_window", "index_marker_gap",
               "combination_window"),
      name = "additional_name_reference",
      level = "additional_level_reference"
    ) |>
    dplyr::mutate(
      "additional_name_comparator" = "additional_name_reference",
      "additional_level_comparator" = "additional_level_reference"
    ) |>
    dplyr::select(omopgenerics::resultColumns("compared_result")) |>
    omopgenerics::newComparedResult()
  return(x)
}
