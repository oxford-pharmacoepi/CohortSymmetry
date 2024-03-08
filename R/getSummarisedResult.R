getSummarisedResult <- function(x) {
  settings <- c("days_prior_observation", "washout_window", "index_marker_gap",
                "combination_window", "confidence_interval", "moving_average_restriction")
  x_sum <- x |>
    dplyr::mutate(
      result_type = "sequence_ratios",
      package_name = "CohortSymmetry",
      package_version = as.character(utils::packageVersion("CohortSymmetry")),
      group_name = "index_cohort_name and marker_cohort_name",
      group_level = paste0(.data$index_name, " and ", .data$marker_name),
      strata_name = "overall", # to update
      strata_level = "overall", # to update
    ) |>
    dplyr::rename(
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
        .data$variable_name %in% c("csr", "asr") ~ "point_estimate",
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
      estimate_value = as.character(.data$estimate_value),
      additional_name = "overall",
      additional_level = "overall"
    )

  x_res <- x |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(settings, "cdm_name")))) |>
    dplyr::mutate(result_id = as.character(dplyr::row_number()))

  x_sum <- x_sum |>
    dplyr::left_join(x_res) |>
    dplyr::select(dplyr::all_of(omopgenerics::resultColumns("summarised_result"))) |>
    dplyr::union_all(
      x_res |>
        dplyr::mutate(
          result_type = "sequence_ratios",
          package_name = "CohortSymmetry",
          package_version = as.character(utils::packageVersion("CohortSymmetry")),
          group_name = "overall",
          group_level = "overall",
          strata_name = "overall",
          strata_level = "overall",
          additional_name = "overall",
          additional_level = "overall",
          variable_name = "settings",
          variable_level = NA_character_,
          dplyr::across(dplyr::all_of(settings), ~ as.character(.x))
        ) |>
        tidyr::pivot_longer(cols = settings,
                            names_to = "estimate_name",
                            values_to = "estimate_value") |>
        dplyr::mutate(estimate_type = dplyr::case_when(
          .data$estimate_name == "combination_window" ~ "character",
          .data$estimate_name == "confidence_interval" ~ "numeric",
          .default = "integer"
        )) |>
        dplyr::select(omopgenerics::resultColumns("summarised_result"))
    ) |>
    omopgenerics::newSummarisedResult()

  return(x_sum)
}
