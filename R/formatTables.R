#' A formatted visualization of sequence_symmetry objects.
#'
#' @description
#' It provides a formatted table with the contents of the getSequenceRatios
#' output.
#'
#' @param result A sequence_symmetry object.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param estimateNameFormat .
#' @param header .
#' @param style .
#' @param splitGroup .
#' @param .options # decimals
#'
#' @return A table with a formatted version of the sequence_symmetry object.
#'
#'
#' @examples
#' {
#' }
#'
#'
formatSequenceSymmetry <- function(result,
                                   type = "gt",
                                   estimateNameFormat =
                                     c("N (%)" = "<count> (<percentage> %)",
                                       "SR (CI)" = "<point_estimate> (<lower_CI> - <upper_CI>)"),
                                   style = "default",
                                   crude = TRUE,
                                   adjusted = TRUE,
                                   studyPopulation = TRUE,
                                   indexName = TRUE,
                                   markerName = TRUE,
                                   cdmName = TRUE,
                                   .options = NULL) {
  # checks
  checkSequenceSymmetry(result)
  checksFormatSequenceSymmetry(type, crude, adjusted, studyPopulation, indexName,
                               markerName, cdmName, .options)

  # Fill .options argument
  .options = defaultOptions(.options)

  # get CI
  ci <- (1-2*(result |> visOmopResults::splitAdditional() |>
    dplyr::pull("confidence_interval_level") |> unique() |> as.numeric()))*100

  # get study population
  if (studyPopulation) {
    total_participants <- result |>
      dplyr::mutate(
        estimate_value = as.numeric(.data$estimate_value)
        ) |>
      dplyr::filter(.data$variable_name == "first_pharmac") |>
      dplyr::filter(.data$estimate_name == "count") |>
      tidyr::pivot_wider(names_from = "variable_level",
                         values_from = "estimate_value") |>
      dplyr::mutate(estimate_value = as.character(index + marker),
                    estimate_name = "Study population") |>
      visOmopResults::splitGroup() |>
      dplyr::select(!c("result_type", "package_name", "package_version",
                       "estimate_type", dplyr::starts_with("additional"),
                       dplyr::starts_with("strata"), "index", "marker")) |>
      dplyr::select(-"variable_name")
  }

  # columns to export
  order_columns <- c("Database name", "Index", "Marker", "Study population",
                     "Index first, N (%)", "Marker first, N (%)",
                     paste0("CSR (", ci, "% CI)"),
                     paste0("ASR (", ci, "% CI)"))
  order_columns <- order_columns[c(cdmName, indexName, markerName,
                                   studyPopulation, TRUE, TRUE, TRUE, TRUE)]

  # format table
  format_result <- result |>
    visOmopResults::formatEstimateValue(
      decimals = .options$decimals,
      decimalMark = .options$decimalMark,
      bigMark = .options$bigMark
    ) |>
    visOmopResults::formatEstimateName(
      estimateNameFormat = estimateNameFormat,
      keepNotFormatted = .options$keepNotFormatted,
      useFormatOrder = .options$useFormatOrder
    ) |>
    visOmopResults::splitGroup() |>
    dplyr::select(!c("result_type", "package_name", "package_version",
                     "estimate_type", dplyr::starts_with("additional"),
                     dplyr::starts_with("strata"))) %>%
    dplyr::mutate(
      estimate_name = dplyr::case_when(
        .data$variable_level == "crude" ~ paste0("CSR (", ci, "% CI)"),
        .data$variable_level == "adjusted" ~ paste0("ASR (", ci, "% CI)"),
        .default = .data$estimate_name
      ),
      estimate_name = dplyr::case_when(
        .data$variable_level == "crude" ~ paste0("CSR (", ci, "% CI)"),
        .data$variable_level == "adjusted" ~ paste0("ASR (", ci, "% CI)"),
        .data$variable_level == "index" ~ "Index first, N (%)",
        .data$variable_level == "marker" ~ "Marker first, N (%)"
      )
    ) |>
    dplyr::select(-dplyr::all_of(c("variable_name", "variable_level"))) %>%
    {if (studyPopulation) {
      dplyr::union_all(., total_participants)
    } else .} %>%
    dplyr::rename(
      "Database name" = "cdm_name",
      "Index" = "index_cohort_name",
      "Marker" = "marker_cohort_name"
    ) %>%
    dplyr::mutate(
      Index = stringr::str_to_sentence(gsub("_", " ", .data$Index)),
      Marker = stringr::str_to_sentence(gsub("_", " ", .data$Marker))
    ) %>%
    {if (!indexName) {
      dplyr::select(., -"Index")
    } else .} %>%
    {if (!markerName) {
      dplyr::select(., -"Marker")
    } else .} %>%
    tidyr::pivot_wider(names_from = estimate_name, values_from = estimate_value) %>%
    dplyr::select(dplyr::all_of(order_columns))


  # output type
  if (type == "tibble") {
    return(format_result)
  }

  if (type == "gt") {
    return(
      visOmopResults::gtTable(
        format_result,
        style = style,
        na = .options$na,
        title = .options$title,
        subtitle = .options$subtitle,
        caption = .options$caption,
        groupNameCol = .options$groupNameCol,
        groupNameAsColumn = .options$groupNameAsColumn,
        groupOrder = .options$groupOrder,
        colsToMergeRows = .options$colsToMergeRows
      )
    )
  }

  if (type == "fx") {
    return(
      visOmopResults::fxTable(
        format_result,
        style = style,
        na = .options$na,
        title = .options$title,
        subtitle = .options$subtitle,
        caption = .options$caption,
        groupNameCol = .options$groupNameCol,
        groupNameAsColumn = .options$groupNameAsColumn,
        groupOrder = .options$groupOrder,
        colsToMergeRows = .options$colsToMergeRows
      )
    )
  }

}

defaultOptions <- function(userOptions) {
  defaultOpts <- list(
    decimals = c(integer = 0, numeric = 2, percentage = 1, proportion = 3),
    decimalMark = ".",
    bigMark = ",",
    keepNotFormatted = TRUE,
    useFormatOrder = TRUE,
    header = NULL,
    includeHeaderName = FALSE,
    includeHeaderKey = TRUE,
    na = "-",
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    groupNameCol = NULL,
    groupNameAsColumn = FALSE,
    groupOrder = NULL,
    colsToMergeRows = "all_columns"
  )

  for (opt in names(defaultOpts)) {
    if (!opt %in% names(userOptions)) {
      userOptions[[opt]] <- defaultOpts[[opt]]
    }
  }

  return(userOptions)
}
