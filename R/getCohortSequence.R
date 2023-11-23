#' Intersecting the index and marker cohorts prior to calculating Sequence Symmetry Ratios
#' @description
#' Join two tables in the CDM (one for index and the other for marker cohorts)
#' into a new table in the cdm taking into account the maximum time interval between events.
#' Index and marker cohorts should be instantiated in advance by the user.
#' @param cdm A CDM reference.
#' @param name the name within the cdm that the output is called. Default is joined_cohorts.
#' @param dateRange Two dates indicating study period and the sequences that the user wants
#' to restrict to.
#' @param indexTable A table in the CDM that the index cohorts should come from.
#' @param indexId Cohort definition IDs in indexTable to be considered for the analysis.
#' Change to NULL if all indices are wished to be included.
#' @param markerTable A table in the CDM that the marker cohorts should come from.
#' @param markerId Cohort definition IDs in markerTable to be considered for the analysis.
#' Change to NULL if all markers are wished to be included.
#' @param daysPriorObservation The minimum amount of prior observation required on both the index
#' and marker cohorts per person.
#' @param indexWashout Washout period to be applied to the index cohort event.
#' @param markerWashout Washout period to be applied to the marker cohort event.
#' @param timeGap The time between two initiations of index and marker. Default is 365.
#' Change to Inf if no constrains are imposed.
#' @param firstEver If TRUE then the first sequence is considered followed by assessing its eligibility.
#' If false then all eligible sequences are assessed and then the first one is picked.
#' @return
#' A table in the cdm reference with subject_id, index_id, marker_id, index_date, marker_date, first_date and cdm_name.
#' @export
#'
#' @examples
getCohortSequence <- function(cdm,
                              name = "joined_cohorts",
                              dateRange = as.Date(c(NA, NA)),
                              indexTable,
                              indexId = NULL,
                              markerTable,
                              markerId = NULL,
                              daysPriorObservation = 0,
                              indexWashout = 0,
                              markerWashout = 0,
                              timeGap = 365,
                              firstEver = F){

  # change daysPriorObservation in the event of Inf

  if(isTRUE(firstEver)){
    cli::cli_abort("error")
  }

  if(!is.finite(timeGap)){
    timeGap <- 999999999
  }

  # checks
  checkInputGetCohortSequence(cdm = cdm,
                              name = name,
                              dateRange = dateRange,
                              indexTable = indexTable,
                              indexId = indexId,
                              markerTable = markerTable,
                              markerId = markerId,
                              daysPriorObservation = daysPriorObservation,
                              indexWashout = indexWashout,
                              markerWashout = markerWashout,
                              timeGap = timeGap,
                              firstEver = firstEver)

  # modify dateRange if necessary
  if(any(is.na(dateRange))){
    dateRange <- getDateRange(cdm = cdm,
                              dateRange = dateRange)
  }

  temp <- list()
  if (is.null(indexId)){
    if (is.null(markerId)){
      indexCohort <- cdm[[indexTable]]
      markerCohort <- cdm[[markerTable]]
    } else{
      indexCohort <- cdm[[indexTable]]
      markerCohort <- cdm[[markerTable]] %>% dplyr::filter(.data$cohort_definition_id %in% markerId)
    }
  } else{
    if (is.null(markerId)){
      indexCohort <- cdm[[indexTable]] %>% dplyr::filter(.data$cohort_definition_id %in% indexId)
      markerCohort <- cdm[[markerTable]]
    } else {
      indexCohort <- cdm[[indexTable]] %>% dplyr::filter(.data$cohort_definition_id %in% indexId)
      markerCohort <- cdm[[markerTable]] %>% dplyr::filter(.data$cohort_definition_id %in% markerId)
    }
  }

  for (j in (markerCohort %>% dplyr::select(.data$cohort_definition_id) %>% dplyr::distinct() %>% dplyr::pull())){
    for (i in (indexCohort %>% dplyr::select(.data$cohort_definition_id) %>% dplyr::distinct() %>% dplyr::pull())){
      temp[[(2^i)*(3^j)]] <-
        indexCohort %>%
        dplyr::filter(.data$cohort_definition_id == i) %>%
        dplyr::group_by(.data$subject_id) %>%
        dbplyr::window_order(.data$cohort_start_date) %>%
        dplyr::mutate(gap_to_prior = .data$cohort_start_date - dplyr::lag(.data$cohort_start_date)) %>%
        dplyr::select(.data$cohort_definition_id, .data$subject_id, .data$cohort_start_date, .data$gap_to_prior) %>%
        dplyr::rename(index_id = .data$cohort_definition_id,
                      index_date = .data$cohort_start_date,
                      gap_to_prior_index = .data$gap_to_prior) %>%
        dplyr::filter(.data$index_date <= !!dateRange[[2]] & .data$index_date >= !!dateRange[[1]]) %>%
        dplyr::filter(dplyr::row_number()==1) %>%
        dplyr::ungroup() %>%
        dbplyr::window_order() %>%
        PatientProfiles::addPriorObservation(indexDate = "index_date",
                                             priorObservationName = "prior_observation_index") %>%
        CDMConnector::computeQuery()%>%
        dplyr::inner_join(markerCohort %>%
                            dplyr::filter(.data$cohort_definition_id == j) %>%
                            dplyr::group_by(.data$subject_id) %>%
                            dbplyr::window_order(.data$cohort_start_date) %>%
                            dplyr::mutate(gap_to_prior = .data$cohort_start_date - dplyr::lag(.data$cohort_start_date)) %>%
                            dplyr::select(.data$cohort_definition_id, .data$subject_id, .data$cohort_start_date, .data$gap_to_prior) %>%
                            dplyr::rename(marker_id = .data$cohort_definition_id,
                                          marker_date = .data$cohort_start_date,
                                          gap_to_prior_marker = .data$gap_to_prior) %>%
                            dplyr::filter(.data$marker_date <= !!dateRange[[2]] & .data$marker_date >= !!dateRange[[1]]) %>%
                            dplyr::filter(dplyr::row_number()==1) %>%
                            dplyr::ungroup() %>%
                            dbplyr::window_order() %>%
                            PatientProfiles::addPriorObservation(indexDate = "marker_date",
                                                                 priorObservationName = "prior_observation_marker") %>%
                            CDMConnector::computeQuery(),
                          by = "subject_id") %>%
        CDMConnector::computeQuery()
    }
  }
  temp <- temp[!sapply(temp, is.null)]
  cdm[[name]] <- Reduce(dplyr::union_all, temp) %>%
    PatientProfiles::addCdmName() %>%
    dplyr::mutate(gap = !!CDMConnector::datediff("index_date", "marker_date",
                                                 interval = "day")) %>%
    dplyr::filter(!.data$gap==0) %>%
    dplyr::filter(abs(.data$gap)<.env$timeGap) %>%
    dplyr::select(-.data$gap) %>%
    dplyr::mutate(first_date = dplyr::if_else(.data$index_date<=.data$marker_date,
                                              .data$index_date, .data$marker_date),
                  second_date = dplyr::if_else(.data$index_date>=.data$marker_date,
                                              .data$index_date, .data$marker_date)) %>%
    dplyr::filter(.data$prior_observation_marker >= .env$daysPriorObservation & .data$prior_observation_index >= .env$daysPriorObservation)%>%
    dplyr::filter(.data$gap_to_prior_index >= .env$indexWashout | is.na(.data$gap_to_prior_index)) %>%
    dplyr::filter(.data$gap_to_prior_marker >= .env$markerWashout | is.na(.data$gap_to_prior_marker)) %>%
    dplyr::select(.data$index_id, .data$marker_id, .data$subject_id, .data$index_date, .data$marker_date, .data$first_date, .data$second_date, .data$cdm_name)  %>%
    CDMConnector::computeQuery(name = name,
                               temporary = FALSE,
                               schema = attr(cdm, "write_schema"),
                               overwrite = TRUE)
  return(cdm)
}

### extra functions
# If the user doesn't specify date range
# range to min and max of obs period
getDateRange <- function(cdm, dateRange){
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
