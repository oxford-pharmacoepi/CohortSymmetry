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
#' @param combineIndex A parameter to combine certain cohorts of the indexTable together.
#' Default is Null, meaning every cohort_definition_id is considered separately.
#' @param markerId Cohort definition IDs in markerTable to be considered for the analysis.
#' Change to NULL if all markers are wished to be included.
#' @param combineMarker A parameter to combine certain cohorts of the markerTable together.
#' Default is Null, meaning every cohort_definition_id is considered separately.
#' @param daysPriorObservation The minimum amount of prior observation required on both the index
#' and marker cohorts per person.
#' @param indexWashout Washout period to be applied to the index cohort event.
#' @param markerWashout Washout period to be applied to the marker cohort event.
#' @param continuedExposureInterval The time before the start of the second episode
#' of the drug (could be either marker or index) and the time after the end of the first
#' episode (could be either marker or index).
#' @param blackOutPeriod The minimum time the ADR is expected to take place.
#' Default is 0, meaning excluding the cases that see both dates on the same day.
#' @param timeGap The time between two initiations of index and marker. Default is 365.
#' Change to Inf if no constrains are imposed.
#'
#' @return
#' A table in the cdm reference with subject_id, index_id, marker_id, index_date, marker_date, first_date and cdm_name.
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
#' cdm$joined_cohorts
#' }
getCohortSequence <- function(cdm,
                              indexTable,
                              markerTable,
                              name = "joined_cohorts",
                              dateRange = as.Date(c(NA, NA)),
                              indexId = NULL,
                              combineIndex = NULL,
                              markerId = NULL,
                              combineMarker = NULL,
                              daysPriorObservation = 0,
                              indexWashout = 0,
                              markerWashout = 0,
                              continuedExposureInterval = NULL,
                              blackOutPeriod = 0,
                              timeGap = 365) {
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
    indexWashout = indexWashout,
    markerWashout = markerWashout,
    blackOutPeriod = blackOutPeriod,
    continuedExposureInterval = continuedExposureInterval,
    timeGap = timeGap
  )
  temp <- list()

  if (!is.finite(timeGap)) {
    timeGap <- 99999999999
  }

  if (is.null(continuedExposureInterval)) {
    continuedExposureInterval <- timeGap
  }

  # modify dateRange if necessary
  if (any(is.na(dateRange))) {
    dateRange <- getDateRange(
      cdm = cdm,
      dateRange = dateRange
    )
  }

  if (is.null(indexId)) {
    if (is.null(markerId)) {
      indexCohort <- cdm[[indexTable]]
      markerCohort <- cdm[[markerTable]]
    } else {
      indexCohort <- cdm[[indexTable]]
      markerCohort <- cdm[[markerTable]] %>% dplyr::filter(.data$cohort_definition_id %in% markerId)
    }
  } else {
    if (is.null(markerId)) {
      indexCohort <- cdm[[indexTable]] %>% dplyr::filter(.data$cohort_definition_id %in% indexId)
      markerCohort <- cdm[[markerTable]]
    } else {
      indexCohort <- cdm[[indexTable]] %>% dplyr::filter(.data$cohort_definition_id %in% indexId)
      markerCohort <- cdm[[markerTable]] %>% dplyr::filter(.data$cohort_definition_id %in% markerId)
    }
  }

  if (is.null(combineIndex)) {
    indexCohort <- indexCohort
  } else if (identical(combineIndex, c("All"))) {
    indexCohort <- indexCohort %>% dplyr::mutate(cohort_definition_id = 1)
  } else if (is.list(combineIndex)) {
    checkcombineIndexList(combineIndex = combineIndex)
    input_ids <- c()
    for (i in (1:length(combineIndex))) {
      input_ids <- c(input_ids, combineIndex[[i]]) %>% unique()
    }
    current_ids <- indexCohort %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::pull("cohort_definition_id")
    if (identical(input_ids, current_ids) == FALSE) {
      cli::cli_abort("your inputted ids are not the same as cohort_definition_id in the indexTable, please double check")
    }
    for (k in 1:length(combineIndex)) {
      rq_id <- combineIndex[[k]]
      indexCohort <- indexCohort %>% dplyr::mutate(cohort_definition_id2 = dplyr::case_when(as.integer(.data$cohort_definition_id) %in% test ~ k))
    }
    indexCohort <- indexCohort %>%
      dplyr::select(-"cohort_definition_id") %>%
      dplyr::rename("cohort_definition_id" = "cohort_definition_id2") %>%
      dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")
  } else {
    cli::cli_abort("combineIndex has to be either NULL or 'ALL' or a list")
  }

  if (is.null(combineMarker)) {
    markerCohort <- markerCohort
  } else if (identical(combineMarker, c("All"))) {
    markerCohort <- markerCohort %>% dplyr::mutate(cohort_definition_id = 1)
  } else if (is.list(combineMarker)) {
    checkcombineMarkerList(combineMarker = combineMarker)
    input_ids <- c()
    for (i in (1:length(combineMarker))) {
      input_ids <- c(input_ids, combineMarker[[i]]) %>% unique()
    }
    current_ids <- markerCohort %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::pull("cohort_definition_id")
    if (identical(input_ids, current_ids) == FALSE) {
      cli::cli_abort("your inputted ids are not the same as cohort_definition_id in the markerTable, please double check")
    }
    for (k in 1:length(combineMarker)) {
      rq_id <- combineMarker[[k]]
      markerCohort <- markerCohort %>% dplyr::mutate(cohort_definition_id2 = dplyr::case_when(as.integer(.data$cohort_definition_id) %in% test ~ k))
    }
    markerCohort <- markerCohort %>%
      dplyr::select(-"cohort_definition_id") %>%
      dplyr::rename("cohort_definition_id" = "cohort_definition_id2") %>%
      dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")
  } else {
    cli::cli_abort("combineMarker has to be either NULL or 'ALL' or a list")
  }



  preprocessCohort <- function(cohort, dateRange) {
    cohort %>%
      dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
      dbplyr::window_order(.data$cohort_start_date) %>%
      dplyr::mutate(gap_to_prior = .data$cohort_start_date - dplyr::lag(.data$cohort_start_date)) %>%
      dplyr::filter(.data$cohort_start_date <= !!dateRange[[2]] & .data$cohort_start_date >= !!dateRange[[1]]) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup() %>%
      PatientProfiles::addPriorObservation(indexDate = "cohort_start_date", priorObservationName = "prior_observation") %>%
      dplyr::compute()
  }

  # Preprocess both cohorts
  indexPreprocessed <- preprocessCohort(indexCohort, dateRange)
  markerPreprocessed <- preprocessCohort(markerCohort, dateRange)


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
      abs(.data$gap) > .env$blackOutPeriod & abs(.data$gap) <= .env$timeGap,
      .data$cei <= .env$continuedExposureInterval,
      .data$prior_observation_marker >= .env$daysPriorObservation &
        .data$prior_observation_index >= .env$daysPriorObservation,
      .data$gap_to_prior_index >= .env$indexWashout | is.na(.data$gap_to_prior_index),
      .data$gap_to_prior_marker >= .env$markerWashout | is.na(.data$gap_to_prior_marker)
    ) %>%
    dplyr::select("index_id", "marker_id", "subject_id", "index_date", "marker_date", "first_date", "second_date")


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
