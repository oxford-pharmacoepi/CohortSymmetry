# getCohortSequence3 <- function(cdm,
#                                name = "joined_cohorts",
#                                dateRange = as.Date(c(NA, NA)),
#                                indexTable,
#                                indexId = NULL,
#                                markerTable,
#                                markerId = NULL,
#                                ageGroup = list(c(0, 150)),
#                                sex = "both",
#                                daysPriorObservation = 0,
#                                indexWashout = 0,
#                                markerWashout = 0,
#                                timeGap = 365,
#                                firstEver = T){
#
# }

getCohortSequence2 <- function(cdm,
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
                              firstEver = T){

  # change daysPriorObservation in the event of Inf

  if(!isTRUE(firstEver)){
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
        dplyr::rename(index_id = .data$cohort_definition_id,
                      index_date = .data$cohort_start_date) %>%
        dplyr::group_by(.data$subject_id) %>%
        dbplyr::window_order(.data$index_date) %>%
        dplyr::mutate(gap_to_prior_index = .data$index_date - dplyr::lag(.data$index_date)) %>%
        dplyr::ungroup() %>%
        dbplyr::window_order() %>%
        PatientProfiles::addPriorObservation(indexDate = "index_date",
                                             priorObservationName = "prior_observation_index") %>%
        dplyr::filter(.data$prior_observation_index >= .env$daysPriorObservation) %>%
        dplyr::filter(.data$gap_to_prior_index >= .env$indexWashout | is.na(.data$gap_to_prior_index)) %>%
        dplyr::select(.data$index_id, .data$subject_id, .data$index_date) %>%
        CDMConnector::computeQuery() %>%
        dplyr::inner_join(markerCohort %>%
                            dplyr::filter(.data$cohort_definition_id == j) %>%
                            dplyr::rename(marker_id = .data$cohort_definition_id,
                                          marker_date = .data$cohort_start_date) %>%
                            dplyr::group_by(.data$subject_id) %>%
                            dbplyr::window_order(.data$marker_date) %>%
                            dplyr::mutate(gap_to_prior_marker = .data$marker_date - dplyr::lag(.data$marker_date)) %>%
                            dplyr::ungroup() %>%
                            dbplyr::window_order() %>%
                            PatientProfiles::addPriorObservation(indexDate = "marker_date",
                                                                 priorObservationName = "prior_observation_marker") %>%
                            dplyr::filter(.data$prior_observation_marker >= daysPriorObservation) %>%
                            dplyr::filter(.data$gap_to_prior_marker >= markerWashout | is.na(.data$gap_to_prior_marker)) %>%
                            dplyr::select(.data$marker_id, .data$subject_id, .data$marker_date) %>%
                            CDMConnector::computeQuery(),
                          by = "subject_id") %>%
        dplyr::mutate(gap = !!CDMConnector::datediff("index_date", "marker_date",
                                                     interval = "day")) %>%
        dplyr::filter(!.data$gap==0) %>%
        dplyr::filter(abs(.data$gap)<.env$timeGap) %>%
        dplyr::select(-.data$gap) %>%
        dplyr::mutate(first_date = dplyr::if_else(.data$index_date<=.data$marker_date,
                                                  .data$index_date, .data$marker_date)) %>%
        dplyr::mutate(second_date = dplyr::if_else(.data$index_date>=.data$marker_date,
                                                   .data$index_date, .data$marker_date)) %>%
        dplyr::group_by(.data$subject_id) %>%
        dbplyr::window_order(.data$first_date, .data$second_date) %>%
        dplyr::filter(dplyr::row_number()==1) %>%
        dplyr::ungroup() %>%
        dbplyr::window_order() %>%
        CDMConnector::computeQuery()
    }
  }
  temp <- temp[!sapply(temp, is.null)]
  cdm[[name]] <- Reduce(dplyr::union_all, temp) %>%
    PatientProfiles::addCdmName() %>%
    dplyr::filter(.data$index_date <= !!dateRange[[2]] & .data$index_date >= !!dateRange[[1]]) %>%
    dplyr::filter(.data$marker_date <= !!dateRange[[2]] & .data$marker_date >= !!dateRange[[1]]) %>%
    dplyr::select(.data$index_id, .data$marker_id, .data$subject_id, .data$index_date, .data$marker_date, .data$first_date, .data$second_date, .data$cdm_name) %>%
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
