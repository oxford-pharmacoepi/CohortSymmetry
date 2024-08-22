#' Intersecting the index and marker cohorts prior to calculating Sequence Symmetry Ratios
#' @description
#' Join two tables in the CDM (one for index and the other for marker cohorts)
#' into a new table in the cdm taking into account the maximum time interval between events.
#' Index and marker cohorts should be instantiated in advance by the user.
#' @param cdm A CDM reference.
#' @param indexTable A table in the CDM that the index cohorts should come from.
#' @param markerTable A table in the CDM that the marker cohorts should come from.
#' @param name The name within the cdm that the output is called. Default is joined_cohorts.
#' @param cohortDateRange Two dates indicating study period and the sequences that the user wants
#' to restrict to.
#' @param indexId Cohort definition IDs in indexTable to be considered for the analysis.
#' Change to NULL if all indices are wished to be included.
#' @param markerId Cohort definition IDs in markerTable to be considered for the analysis.
#' Change to NULL if all markers are wished to be included.
#' @param daysPriorObservation The minimum amount of prior observation required on both the index
#' and marker cohorts per person.
#' @param washoutWindow A washout window to be applied on both the index cohort event and marker cohort.
#' @param indexMarkerGap The maximum allowable gap between the end of the first episode
#' and the start of the second episode in a sequence/combination.
#' @param combinationWindow A constrain to be placed on the gap between two initiations.
#' Default c(0,365), meaning the gap should be larger than 0 but less than or equal to 365.
#'
#' @return
#' A table within the cdm reference.
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortSymmetry)
#' cdm <- mockCohortSymmetry()
#' cdm <- generateSequenceCohortSet(
#'   cdm = cdm,
#'   name = "joined_cohorts",
#'   indexTable = "cohort_1",
#'   markerTable = "cohort_2"
#' )
#'  cdm$joined_cohorts
#'  CDMConnector::cdmDisconnect(cdm = cdm)
#' }
generateSequenceCohortSet <- function(cdm,
                              indexTable,
                              markerTable,
                              name,
                              cohortDateRange = as.Date(c(NA, NA)),
                              indexId = NULL,
                              markerId = NULL,
                              daysPriorObservation = 0,
                              washoutWindow = 0,
                              indexMarkerGap = NULL,
                              combinationWindow = c(0,365)) {
  # checks
  checkInputgenerateSequenceCohortSet(
    cdm = cdm,
    indexTable = indexTable,
    markerTable = markerTable,
    name = name,
    cohortDateRange = cohortDateRange,
    indexId = indexId,
    markerId = markerId,
    daysPriorObservation = daysPriorObservation,
    washoutWindow = washoutWindow,
    indexMarkerGap = indexMarkerGap,
    combinationWindow = combinationWindow
  )
  comb_export_1 <- as.character(combinationWindow[1])
  comb_export_2 <- as.character(combinationWindow[2])

  if(!is.finite(combinationWindow[2])){
    combinationWindow[2] <- as.integer(99999)
  }

  if (is.null(indexMarkerGap)) {
    indexMarkerGap <- combinationWindow[2]
    indexMarkerGap_export <- comb_export_2
  } else {
    indexMarkerGap_export <- indexMarkerGap
  }

  # modify cohortDateRange if necessary
  if (any(is.na(cohortDateRange))) {
    cohortDateRange <- getcohortDateRange(
      cdm = cdm,
      cohortDateRange = cohortDateRange
    )
  }

  # Preprocess both cohorts
  indexPreprocessed <- preprocessCohort(cdm = cdm, cohortName = indexTable,
                                        cohortId = indexId, cohortDateRange = cohortDateRange) |>
    dplyr::rename("index_id" = "cohort_definition_id",
                  "index_name" = "cohort_name",
      "index_date" = "cohort_start_date",
      "index_end_date" = "cohort_end_date",
      "gap_to_prior_index" = "gap_to_prior"
    )
  markerPreprocessed <- preprocessCohort(cdm = cdm, cohortName = markerTable,
                                         cohortId = markerId, cohortDateRange = cohortDateRange) |>
    dplyr::rename("marker_id" = "cohort_definition_id",
                  "marker_name" = "cohort_name",
                  "marker_date" = "cohort_start_date",
                  "marker_end_date" = "cohort_end_date",
                  "gap_to_prior_marker" = "gap_to_prior")

  time_1 <- combinationWindow[1]
  time_2 <- combinationWindow[2]

  joinedData <- indexPreprocessed |>
    dplyr::inner_join(
      markerPreprocessed,
      by = "subject_id"
    ) |>
    dplyr::mutate(
      first_date = dplyr::if_else(.data$index_date <= .data$marker_date,
                                                    .data$index_date,
                                                    .data$marker_date
    ),
    second_date = dplyr::if_else(.data$index_date >= .data$marker_date,
                                 .data$index_date,
                                 .data$marker_date)
    ) |>
    dplyr::inner_join(
      cdm[["observation_period"]], by = c("subject_id" = "person_id")
    ) |>
    dplyr::filter(.data$first_date >= .data$observation_period_start_date,
                  .data$second_date <= .data$observation_period_end_date) |>
    dplyr::select("index_id", "subject_id", "index_date",
                  "index_end_date", "gap_to_prior_index",
                  "index_name", "marker_id", "marker_date",
                  "marker_end_date", "gap_to_prior_marker",
                  "marker_name", "first_date", "second_date")

  # Post-join processing
  cdm[[name]] <- joinedData %>%
    dplyr::mutate(
      gap = as.numeric(!!CDMConnector::datediff("index_date", "marker_date",
                                                interval = "day")),
      gap_index_marker = as.numeric(!!CDMConnector::datediff("index_end_date", "marker_date",
                                        interval = "day")),
      gap_marker_index = as.numeric(!!CDMConnector::datediff("marker_end_date", "index_date",
                                                             interval = "day"))) |>
    dplyr::mutate(
      cei = dplyr::if_else(.data$index_date < .data$marker_date,
                           .data$gap_index_marker, .data$gap_marker_index)
    ) |>
    dplyr::select("index_id", "index_name",
                  "marker_id", "marker_name",
                  "subject_id", "index_date",
                  "marker_date", "first_date", "second_date",
                  "cei",
                  "gap_to_prior_index",
                  "gap_to_prior_marker", "gap")  |>
    dplyr::compute(name = name,
                   temporary = FALSE)

  cdm[["ids"]] <- cdm[[name]] |>
    dplyr::select(.data$index_id, .data$marker_id) |>
    dplyr::distinct() |>
    dplyr::arrange(.data$index_id, .data$marker_id) |>
    dplyr::mutate(cohort_definition_id = as.integer(dplyr::row_number())) |>
    dplyr::compute(name = "ids", temporary = FALSE)

  cdm[[name]] <- cdm[[name]] |>
    dplyr::left_join(cdm[["ids"]], by = c("index_id", "marker_id")) |>
    dplyr::mutate(cohort_start_date = .data$first_date,
                  cohort_end_date = .data$second_date,
                  cohort_name = paste0("index_", .data$index_name,
                                       "_marker_", .data$marker_name)) |>
    dplyr::select("cohort_definition_id", "index_id", "marker_id",
                  "cohort_name", "index_name", "marker_name",
                  "subject_id",
                  "cohort_start_date",
                  "cohort_end_date",
                  "index_date",
                  "marker_date",
                  "cei",
                  "gap_to_prior_index", "gap_to_prior_marker", "gap") |>
    PatientProfiles::addPriorObservation() |>
    dplyr::compute(name = name,
                   temporary = FALSE)

  cohortSetRef <- cdm[[name]]  |>
    dplyr::select("cohort_definition_id", "cohort_name", "index_id",
                  "index_name", "marker_id", "marker_name") |>
    dplyr::group_by(.data$cohort_definition_id, .data$cohort_name, .data$index_id,
                    .data$index_name, .data$marker_id, .data$marker_name) |>
    dplyr::distinct() |>
    dplyr::mutate(days_prior_observation = .env$daysPriorObservation,
                  washout_window = .env$washoutWindow,
                  index_marker_gap = .env$indexMarkerGap_export,
                  combination_window = paste0("(",.env$comb_export_1, ",",
                                              .env$comb_export_2, ")"))

  cdm[[name]] <- cdm[[name]] |>
    dplyr::select(!c("cohort_name", "index_name", "marker_name"))

  cdm[[name]] <- cdm[[name]] |>
    dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date",
                  "cohort_end_date", "index_id", "marker_id", "index_date", "marker_date",
                  "cei", "gap_to_prior_index", "gap_to_prior_marker", "gap", "prior_observation") |>
    omopgenerics::newCohortTable(cohortSetRef = cohortSetRef,
                                 cohortAttritionRef = NULL)

  # exclusion criteria - where attrition starts
  # 1) within combination window
  cdm[[name]] <- cdm[[name]] |>
    dplyr::filter(abs(.data$gap) > .env$time_1 &
                  abs(.data$gap) <= .env$time_2) |>
  dplyr::compute(name = name, temporary = FALSE) |>
  omopgenerics::recordCohortAttrition(reason="Events excluded due to the prespecified combination window")

  # 2) indexMarkerGap
  cdm[[name]] <- cdm[[name]] |>
    dplyr::filter(.data$cei <= .env$indexMarkerGap) |>
  dplyr::compute(name = name, temporary = FALSE) |>
  omopgenerics::recordCohortAttrition(reason="Events excluded due to the prespecified index marker gap")

  # 3) days prior observation
  cdm[[name]] <- cdm[[name]] |>
    dplyr::filter(
      .data$prior_observation >= .env$daysPriorObservation
    ) |>
    dplyr::compute(name = name, temporary = FALSE) |>
  omopgenerics::recordCohortAttrition(reason="Events excluded due to insufficient prior history")

  # 4) washoutWindow
  cdm[[name]] <- cdm[[name]] |>
    dplyr::filter(
      .data$gap_to_prior_index >= .env$washoutWindow | is.na(.data$gap_to_prior_index),
      .data$gap_to_prior_marker >= .env$washoutWindow | is.na(.data$gap_to_prior_marker)
    ) |>
    dplyr::compute(name = name, temporary = FALSE) |>
  omopgenerics::recordCohortAttrition(reason="Events excluded due to insufficient washout window")

  # final output table
  cdm[[name]] <- cdm[[name]] |>
    dplyr::select("cohort_definition_id", "subject_id",
                  "cohort_start_date", "cohort_end_date",
                  "index_date", "marker_date")  |>
    dplyr::compute(name = name,
                   temporary = FALSE)

  cdm <- CDMConnector::dropTable(cdm = cdm, name = "ids")
  return(cdm)
}

### extra functions
# If the user doesn't specify date range
# range to min and max of obs period
getcohortDateRange <- function(cdm, cohortDateRange) {
  if (is.na(cohortDateRange[1])) {
    cohortDateRange[1] <- as.Date(cdm[["observation_period"]] |>
      dplyr::summarise(
        min = min(.data$observation_period_start_date,
          na.rm = TRUE
        )
      ) |>
      dplyr::collect() |>
      dplyr::pull("min"))
  }
  if (is.na(cohortDateRange[2])) {
    cohortDateRange[2] <- as.Date(cdm[["observation_period"]] |>
      dplyr::summarise(
        max = max(.data$observation_period_end_date,
          na.rm = TRUE
        )
      ) |>
      dplyr::collect() |>
      dplyr::pull("max"))
  }
  return(cohortDateRange)
}

preprocessCohort <- function(cdm, cohortName, cohortId, cohortDateRange) {
  cohort <- cdm[[cohortName]]
  if (!is.null(cohortId)) {
    cohort <- cohort |>
      dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)
  }
  id <- "tmp_id_12345"
  nm <- paste0("tmp_001_",  omopgenerics::uniqueTableName())
  cohort <- cohort |>
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
    dplyr::arrange(.data$cohort_start_date) |>
    dplyr::mutate(!!id := dplyr::row_number()) |>
    dplyr::compute(name = nm, temporary = FALSE)
  cohort <- cohort |>
    dplyr::left_join(
      cohort |>
        dplyr::select(dplyr::all_of(
          c("previous_exposure" = "cohort_start_date", id, "cohort_definition_id", "subject_id")
        )) |>
        dplyr::mutate(!!id := .data[[id]] + 1),
      by = c(id, "cohort_definition_id", "subject_id")
    ) %>%
    dplyr::mutate(gap_to_prior = as.numeric(!!CDMConnector::datediff(
      "previous_exposure", "cohort_start_date"
    ))) |>
    dplyr::filter(
      .data$cohort_start_date <= !!cohortDateRange[[2]] &
        .data$cohort_start_date >= !!cohortDateRange[[1]]
    ) |>
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
    dplyr::filter(.data[[id]] == min(.data[[id]], na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(!dplyr::all_of(c(id, "previous_exposure"))) |>
    dplyr::compute(name = nm, temporary = FALSE) |>
    PatientProfiles::addCohortName() |>
    dplyr::compute()
  cdm <- omopgenerics::dropTable(cdm = cdm, name = nm)
  return(cohort)
}
