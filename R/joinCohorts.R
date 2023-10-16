#' Table joining prior to calculating Sequence Symmetry Ratios
#' @description
#' Join two tables in the CDM (one for index and the other for marker cohorts)
#' into a new table in the cdm taking into account the maximum time interval between events.
#' Index and marker cohorts should be instantiated in advance
#' by the user and should only contain first-ever events.
#' @param cdm A CDM reference.
#' @param indexTable A table in the CDM that the index cohorts should come from.
#' @param indexId Cohort definition IDs in indexTable to be considered for the analysis.
#' Change to NULL if all indices are wished to be included.
#' @param markerTable A table in the CDM that the marker cohorts should come from.
#' @param markerId Cohort definition IDs in markerTable to be considered for the analysis.
#' Change to NULL if all markers are wished to be included.
#' @param timeGap The time between two initiations of index and marker. Default is 365.
#' Change to NULL if no constrains are imposed.
#'
#' @return
#' A local table with subjectId, indexId, markerId, indexDate, markerDate and firstDate.
#' @export
#'
#' @examples
joinCohorts <- function(cdm, indexTable, indexId, markerTable, markerId, timeGap = 365){
  data <- data.frame()
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
  if(is.null(timeGap)){
    for (j in (1:length(markerCohort %>% dplyr::select(.data$cohort_definition_id) %>% dplyr::distinct() %>% dplyr::pull()))){
      for (i in (1:length(indexCohort %>% dplyr::select(.data$cohort_definition_id) %>% dplyr::distinct() %>% dplyr::pull()))){
        temp <-
          indexCohort %>%
          dplyr::select(.data$cohort_definition_id, .data$subject_id, .data$cohort_start_date) %>%
          dplyr::filter(.data$cohort_definition_id == i) %>%
          dplyr::rename(indexId = .data$cohort_definition_id,
                        indexDate = .data$cohort_start_date,
                        subjectId = .data$subject_id) %>%
          dplyr::inner_join(markerCohort %>%
                              dplyr::select(.data$cohort_definition_id, .data$subject_id, .data$cohort_start_date) %>%
                              dplyr::filter(.data$cohort_definition_id == j) %>%
                              dplyr::rename(markerId = .data$cohort_definition_id,
                                            markerDate = .data$cohort_start_date,
                                            subjectId = .data$subject_id),
                            by = "subjectId") %>%
          dplyr::select(.data$subjectId, .data$indexId, .data$markerId, .data$indexDate, .data$markerDate) %>%
          dplyr::mutate(gap = .data$markerDate - .data$indexDate) %>%
          dplyr::filter(!.data$gap==0) %>%
          dplyr::filter(-.env$study_time <= .data$gap & .data$gap <= .env$study_time) %>%
          dplyr::select(-.data$gap) %>%
          dplyr::mutate(firstDate = pmin(.data$indexDate, .data$markerDate))

        data <- rbind(data, temp)
      }
    }
  } else {
    for (j in (1:length(markerCohort %>% dplyr::select(.data$cohort_definition_id) %>% dplyr::distinct() %>% dplyr::pull()))){
      for (i in (1:length(indexCohort %>% dplyr::select(.data$cohort_definition_id) %>% dplyr::distinct() %>% dplyr::pull()))){
        temp <-
          indexCohort %>%
          dplyr::select(.data$cohort_definition_id, .data$subject_id, .data$cohort_start_date) %>%
          dplyr::filter(.data$cohort_definition_id == i) %>%
          dplyr::rename(indexId = .data$cohort_definition_id,
                        indexDate = .data$cohort_start_date,
                        subjectId = .data$subject_id) %>%
          dplyr::inner_join(markerCohort %>%
                              dplyr::select(.data$cohort_definition_id, .data$subject_id, .data$cohort_start_date) %>%
                              dplyr::filter(.data$cohort_definition_id == j) %>%
                              dplyr::rename(markerId = .data$cohort_definition_id,
                                            markerDate = .data$cohort_start_date,
                                            subjectId = .data$subject_id),
                            by = "subjectId") %>%
          dplyr::select(.data$subjectId, .data$indexId, .data$markerId, .data$indexDate, .data$markerDate) %>%
          dplyr::mutate(gap = .data$markerDate - .data$indexDate) %>%
          dplyr::filter(!.data$gap==0) %>%
          dplyr::select(-.data$gap) %>%
          dplyr::mutate(firstDate = pmin(.data$indexDate, .data$markerDate))

        data <- rbind(data, temp)
      }
    }
  }
  cdm[["joined_cohorts"]] <- data %>%
    dplyr::mutate(cdm_name = attr(cdm, "cdm_name"),
                  timeGap = .env$timeGap)
  return(cdm)
}
