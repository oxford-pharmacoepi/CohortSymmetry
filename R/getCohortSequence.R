#' Intersecting the index and marker cohorts prior to calculating Sequence Symmetry Ratios
#' @description
#' Join two tables in the CDM (one for index and the other for marker cohorts)
#' into a new table in the cdm taking into account the maximum time interval between events.
#' Index and marker cohorts should be instantiated in advance by the user.
#' @param cdm A CDM reference.
#' @param indexTable A table in the CDM that the index cohorts should come from.
#' @param markerTable A table in the CDM that the marker cohorts should come from.
#' @param name the name within the cdm that the output is called. Default is joined_cohorts.
#' @param dateRange Two dates indicating study period and the sequences that the user wants
#' to restrict to.
#' @param indexId Cohort definition IDs in indexTable to be considered for the analysis.
#' Change to NULL if all indices are wished to be included.
#' @param markerId Cohort definition IDs in markerTable to be considered for the analysis.
#' Change to NULL if all markers are wished to be included.
#' @param daysPriorObservation The minimum amount of prior observation required on both the index
#' and marker cohorts per person.
#' @param washoutWindow A washout window to be applied on both the index cohort event and marker cohort.
#' @param indexMarkerGap The time before the start of the second episode
#' of the drug (could be either marker or index) and the time after the end of the first
#' episode (could be either marker or index).
#' @param combinationWindow a constrain to be placed on the gap between two iniations.
#' Default c(0,365), meaning the gap should be larger than 0 but less than or equal to 365.
#'
#' @return
#' A table in the cdm reference.
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- mockPatientProfiles()
#' cdm <- CohortSymmetry::getCohortSequence(
#'   cdm = cdm,
#'   indexTable = "cohort1",
#'   markerTable = "cohort2"
#' )
#' ## default name - joined_cohorts
#'  cdm$joined_cohorts
#' }
getCohortSequence <- function(cdm,
                              indexTable,
                              markerTable,
                              name = "joined_cohorts",
                              dateRange = as.Date(c(NA, NA)),
                              indexId = NULL,
                              markerId = NULL,
                              daysPriorObservation = 0,
                              washoutWindow = 0,
                              indexMarkerGap = NULL,
                              combinationWindow = c(0,365)) {
  # checks
  checkInputGetCohortSequence(
    cdm = cdm,
    indexTable = indexTable,
    markerTable = markerTable,
    name = name,
    dateRange = dateRange,
    indexId = indexId,
    markerId = markerId,
    daysPriorObservation = daysPriorObservation,
    washoutWindow = washoutWindow,
    indexMarkerGap = indexMarkerGap,
    combinationWindow = combinationWindow
  )
  temp <- list()

  comb_export_1 <- as.character(combinationWindow[1])
  comb_export_2 <- as.character(combinationWindow[2])


  if(!is.finite(combinationWindow[2])){
    combinationWindow[2] <- 99999999999
  }

  if (is.null(indexMarkerGap)) {
    indexMarkerGap <- combinationWindow[2]
    indexMarkerGap_export <- comb_export_2
  } else {
    indexMarkerGap <- indexMarkerGap
  }


  # modify dateRange if necessary
  if (any(is.na(dateRange))) {
    dateRange <- getDateRange(
      cdm = cdm,
      dateRange = dateRange
    )
  }

  preprocessCohort <- function(cdm, cohortName, cohortId, dateRange) {
    cohort <- cdm[[cohortName]]
    if (!is.null(cohortId)) {
      cohort <- cohort |>
        dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)
    }
    id <- "tmp_id_12345"
    nm <- paste0("tmp_001_",  omopgenerics::uniqueTableName())
    cohort <- cohort |>
      dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
      dplyr::arrange(.data$cohort_start_date) %>%
      dplyr::mutate(!!id := dplyr::row_number()) |>
      dplyr::compute(name = nm, temporary = FALSE)
    cohort <- cohort |>
      dplyr::left_join(
        cohort |>
          dplyr::select(dplyr::all_of(
            c("previous_exposure" = "cohort_start_date", id)
          )) |>
          dplyr::mutate(!!id := .data[[id]] + 1),
        by = id
      ) %>%
      dplyr::mutate(gap_to_prior = as.numeric(!!CDMConnector::datediff(
        "previous_exposure", "cohort_start_date"
      ))) %>%
      dplyr::filter(
        .data$cohort_start_date <= !!dateRange[[2]] &
          .data$cohort_start_date >= !!dateRange[[1]]
      ) %>%
      dplyr::filter(.data[[id]] == min(.data[[id]], na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::select(!dplyr::all_of(c(id, "previous_exposure"))) |>
      dplyr::compute(name = nm, temporary = FALSE) |>
      PatientProfiles::addPriorObservation()
    cdm <- omopgenerics::dropTable(cdm = cdm, name = nm)
    return(cohort)
  }

  # Preprocess both cohorts
  indexPreprocessed <- preprocessCohort(cdm, indexTable, indexId, dateRange)
  markerPreprocessed <- preprocessCohort(cdm, markerTable, markerId, dateRange)

  time_1 <- combinationWindow[1]
  time_2 <- combinationWindow[2]

  index_name <- CDMConnector::settings(cdm[[indexTable]]) %>%
    dplyr::select("cohort_definition_id", "cohort_name") %>%
    dplyr::rename("index_id" = "cohort_definition_id",
                  "index_name" = "cohort_name")

  #prefix <- omopgenerics::tmpPrefix() - prefix
  cdm <- omopgenerics::insertTable(cdm = cdm, name = "index_name", table = index_name)

  marker_name <- CDMConnector::settings(cdm[[markerTable]]) %>%
    dplyr::select("cohort_definition_id", "cohort_name") %>%
    dplyr::rename("marker_id" = "cohort_definition_id",
                  "marker_name" = "cohort_name")

  cdm <- omopgenerics::insertTable(cdm = cdm, name = "marker_name", table = marker_name)

  joinedData <- indexPreprocessed %>%
    dplyr::rename(
      "index_id" = "cohort_definition_id", "index_date" = "cohort_start_date",
      "index_end_date" = "cohort_end_date", "prior_observation_index" = "prior_observation",
      "gap_to_prior_index" = "gap_to_prior"
    ) %>%
    dplyr::inner_join(
      markerPreprocessed %>%
        dplyr::rename(
          "marker_id" = "cohort_definition_id", "marker_date" = "cohort_start_date",
          "marker_end_date" = "cohort_end_date", "prior_observation_marker" = "prior_observation",
          "gap_to_prior_marker" = "gap_to_prior"
        ),
      by = "subject_id"
    )


  # Post-join processing
  cdm[[name]] <- joinedData %>%
    dplyr::mutate(
      gap = CDMConnector::datediff("index_date", "marker_date", interval = "day"),
      cei = dplyr::if_else(.data$index_date < .data$marker_date,
        .data$marker_date - .data$index_end_date, #
        .data$index_date - .data$marker_end_date
      ),
      first_date = dplyr::if_else(.data$index_date <= .data$marker_date,
        .data$index_date,
        .data$marker_date
      ),
      second_date = dplyr::if_else(.data$index_date >= .data$marker_date,
        .data$index_date,
        .data$marker_date
      )
    ) %>%
    dplyr::filter(
      abs(.data$gap) > .env$time_1 & abs(.data$gap) <= .env$time_2,
      .data$cei <= .env$indexMarkerGap,
      .data$prior_observation_marker >= .env$daysPriorObservation &
        .data$prior_observation_index >= .env$daysPriorObservation,
      .data$gap_to_prior_index >= .env$washoutWindow | is.na(.data$gap_to_prior_index),
      .data$gap_to_prior_marker >= .env$washoutWindow | is.na(.data$gap_to_prior_marker)
    ) %>%
    dplyr::mutate(days_prior_observation = .env$daysPriorObservation,
                  washout_window = .env$washoutWindow,
                  index_marker_gap = .env$indexMarkerGap_export,
                  combination_window = paste0("(",.env$comb_export_1, ",", .env$comb_export_2, ")")) %>%
    dplyr::select("index_id", "marker_id", "subject_id", "index_date", "marker_date", "first_date", "second_date", "days_prior_observation", "washout_window", "index_marker_gap", "combination_window")  %>%
    dplyr::left_join(cdm[["index_name"]], by = "index_id") %>%
    dplyr::left_join(cdm[["marker_name"]], by = "marker_id") %>%
    dplyr::compute(name = name,
                   temporary = FALSE)

  cdm <- CDMConnector::dropTable(cdm = cdm, name = "index_name")
  cdm <- CDMConnector::dropTable(cdm = cdm, name = "marker_name")
  return(cdm)
}
### extra functions
# If the user doesn't specify date range
# range to min and max of obs period
getDateRange <- function(cdm, dateRange) {
  if (is.na(dateRange[1])) {
    dateRange[1] <- as.Date(cdm[["observation_period"]] %>%
      dplyr::summarise(
        min(.data$observation_period_start_date,
          na.rm = TRUE
        )
      ) %>%
      dplyr::collect() %>%
      dplyr::pull())
  }
  if (is.na(dateRange[2])) {
    dateRange[2] <- as.Date(cdm[["observation_period"]] %>%
      dplyr::summarise(
        max(.data$observation_period_end_date,
          na.rm = TRUE
        )
      ) %>%
      dplyr::collect() %>%
      dplyr::pull())
  }
  return(dateRange)
}
