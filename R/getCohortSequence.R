#' Intersecting the index and marker cohorts prior to calculating Sequence Symmetry Ratios
#' @description
#' Join two tables in the CDM (one for index and the other for marker cohorts)
#' into a new table in the cdm taking into account the maximum time interval between events.
#' Index and marker cohorts should be instantiated in advance by the user.
#' @param cdm A CDM reference.
#' @param indexTable A table in the CDM that the index cohorts should come from.
#' @param indexId Cohort definition IDs in indexTable to be considered for the analysis.
#' Change to NULL if all indices are wished to be included.
#' @param markerTable A table in the CDM that the marker cohorts should come from.
#' @param markerId Cohort definition IDs in markerTable to be considered for the analysis.
#' Change to NULL if all markers are wished to be included.
#' @param timeGap The time between two initiations of index and marker. Default is 365.
#' Change to Inf if no constrains are imposed.
#'
#' @return
#' A table in the cdm reference with subject_id, index_id, marker_id, index_date, marker_date, first_date, time_gap and cdm_name.
#' @export
#'
#' @examples
getCohortSequence <- function(cdm, indexTable, indexId = NULL, markerTable, markerId = NULL, timeGap = 365){

  # Check cdm objects, writing schema and index/marker tables
  checkCdm(cdm, tables=c(indexTable, markerTable))
  assertWriteSchema(cdm)


  # Check markerId and indexId
  check_marker_id <- is.numeric(markerId)
  if(!is.null(markerId)){
    if(!isTRUE(check_marker_id)){
      cli::cli_abort("markerId must be of type 'numeric'")
    }
  }

  check_index_id <- is.numeric(indexId)
  if(!is.null(indexId)){
    if(!isTRUE(check_index_id)){
      cli::cli_abort("indexId must be of type 'numeric'")
    }
  }

  # Checks that Index and Marker ids exist in Index and Marker tables
  checkCohortIds(cdm,indexTable,indexId)
  checkCohortIds(cdm,markerTable,markerId)

  # Checks columns in Index and Marker tables
  checkColumns(cdm,indexTable)
  checkColumns(cdm,markerTable)

  # Check timeGapx
  checktimeGap(timeGap)


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
  if(is.finite(timeGap)){
    for (j in (markerCohort %>% dplyr::select(.data$cohort_definition_id) %>% dplyr::distinct() %>% dplyr::pull())){
      for (i in (indexCohort %>% dplyr::select(.data$cohort_definition_id) %>% dplyr::distinct() %>% dplyr::pull())){
        temp[[(2^i)*(3^j)]] <-
          indexCohort %>%
          dplyr::select(.data$cohort_definition_id, .data$subject_id, .data$cohort_start_date) %>%
          dplyr::filter(.data$cohort_definition_id == i) %>%
          dplyr::rename(index_id = .data$cohort_definition_id,
                        index_date = .data$cohort_start_date,
                        subject_id = .data$subject_id) %>%
          dplyr::inner_join(markerCohort %>%
                              dplyr::select(.data$cohort_definition_id, .data$subject_id, .data$cohort_start_date) %>%
                              dplyr::filter(.data$cohort_definition_id == j) %>%
                              dplyr::rename(marker_id = .data$cohort_definition_id,
                                            marker_date = .data$cohort_start_date,
                                            subject_id = .data$subject_id),
                            by = "subject_id") %>%
          dplyr::select(.data$subject_id, .data$index_id, .data$marker_id, .data$index_date, .data$marker_date) %>%
          dplyr::mutate(gap = .data$marker_date - .data$index_date) %>%
          dplyr::filter(!.data$gap==0) %>%
          dplyr::filter(-.env$timeGap <= .data$gap & .data$gap <= .env$timeGap) %>%
          dplyr::select(-.data$gap) %>%
          dplyr::mutate(first_date = pmin(.data$index_date, .data$marker_date, na.rm = T))%>%
          dplyr::compute()
      }
    }
  } else {
    for (j in (markerCohort %>% dplyr::select(.data$cohort_definition_id) %>% dplyr::distinct() %>% dplyr::pull())){
      for (i in (indexCohort %>% dplyr::select(.data$cohort_definition_id) %>% dplyr::distinct() %>% dplyr::pull())){
        temp[[(2^i)*(3^j)]] <-
          indexCohort %>%
          dplyr::select(.data$cohort_definition_id, .data$subject_id, .data$cohort_start_date) %>%
          dplyr::filter(.data$cohort_definition_id == i) %>%
          dplyr::rename(index_id = .data$cohort_definition_id,
                        index_date = .data$cohort_start_date,
                        subject_id = .data$subject_id) %>%
          dplyr::inner_join(markerCohort %>%
                              dplyr::select(.data$cohort_definition_id, .data$subject_id, .data$cohort_start_date) %>%
                              dplyr::filter(.data$cohort_definition_id == j) %>%
                              dplyr::rename(marker_id = .data$cohort_definition_id,
                                            marker_date = .data$cohort_start_date,
                                            subject_id = .data$subject_id),
                            by = "subject_id") %>%
          dplyr::select(.data$subject_id, .data$index_id, .data$marker_id, .data$index_date, .data$marker_date) %>%
          dplyr::mutate(gap = .data$marker_date - .data$index_date) %>%
          dplyr::filter(!.data$gap==0) %>%
          dplyr::select(-.data$gap) %>%
          dplyr::mutate(first_date = pmin(.data$index_date, .data$marker_date, na.rm = T))%>%
          dplyr::compute()
      }
    }
  }
  temp <- temp[!sapply(temp, is.null)]
  data <- Reduce(dplyr::union_all, temp)
  cdm_name <- attr(cdm, "cdm_name")
  cdm[["joined_cohorts"]] <- data %>%
    dplyr::mutate(time_gap = .env$timeGap,
                  cdm_name = cdm_name
    )
  return(cdm)
}
