#' Sequence ratio calculations
#'
#' @description
#' Using getCohortSequence to obtain sequence ratios for the desired outcomes.
#'
#' @param cdm A CDM reference.
#' @param outcomeTable A table in the CDM that the output of getCohortSequence resides.
#' @param confidenceIntervalLevel Default is 0.025, which is the central 95% confidence interval.
#' @param restriction The moving window when calculating nSR, default is 548.
#'
#' @return
#' A local table with all the analyses.
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cohort1 <-
#' dplyr::tibble("cohort_definition_id" = c(1,1,1,1,1),
#'               "subject_id" = c(1,2,1,2,1),
#'               "cohort_start_date" = seq(as.Date("2020-05-23"), as.Date("2020-05-27"), by = "day"),
#'               "cohort_end_date" = seq(as.Date("2020-05-24"), as.Date("2020-05-28"), by = "day"))
#'
#'  cdm <- mockPatientProfiles(cohort1 = cohort1,
#'                             patient_size = 10)
#'  cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
#'                                           indexTable = "cohort1",
#'                                           markerTable = "cohort2")
#'  pssa_result <- CohortSymmetry::getSequenceRatios (cdm = cdm,
#'                                                    outcomeTable = "joined_cohorts")
#'
#'  pssa_result
#'  CDMConnector::cdmDisconnect(cdm)
#' }
#'
getSequenceRatios <- function(cdm,
                              outcomeTable,
                              confidenceIntervalLevel = 0.025,
                              restriction = 548){

  # checks
  checkInputGetSequenceRatios(cdm = cdm,
                              outcomeTable = outcomeTable,
                              confidenceIntervalLevel = confidenceIntervalLevel,
                              restriction = restriction)

  temp <- list()
  results <- list()
  for (i in (cdm[[outcomeTable]] %>% dplyr::distinct(.data$index_id) %>% dplyr::pull())){
    for (j in (cdm[[outcomeTable]] %>% dplyr::filter(.data$index_id == i) %>% dplyr::distinct(.data$marker_id) %>% dplyr::pull())){
      temp[[paste0("(",i,",",j,")")]] <-
        cdm[[outcomeTable]] %>%
        dplyr::filter(.data$index_id == i & .data$marker_id == j) %>%
        dplyr::collect()

      date_start <- min(temp[[paste0("(",i,",",j,")")]]$index_date, temp[[paste0("(",i,",",j,")")]]$marker_date)

      temp[[paste0("(",i,",",j,")")]] <-
        temp[[paste0("(",i,",",j,")")]] %>%
        dplyr::mutate(
          orderBA = .data$index_date >= .data$marker_date,
          days_first = as.integer((lubridate::interval(date_start, .data$first_date)) / lubridate::days(1)), # gap between the first drug of a person and the first drug of the whole population
          days_second = as.integer((lubridate::interval(.data$first_date, .data$second_date)) / lubridate::days(1))) %>%  # gap between two drugs of a person
        dplyr::arrange(.data$days_first) %>%
        dplyr::group_by(.data$days_first) %>%
        dplyr::summarise(marker_first = sum(.data$orderBA), index_first = sum(!.data$orderBA), .groups = "drop") %>%
        dplyr::mutate(index_id = i, marker_id = j) %>%
        dplyr::ungroup()

      csr<-crudeSequenceRatio(temp[[paste0("(",i,",",j,")")]])
      nsr<-nullSequenceRatio(temp[[paste0("(",i,",",j,")")]], restriction = restriction)
      asr<-adjustedSequenceRatio(temp[[paste0("(",i,",",j,")")]], restriction = restriction)
      counts <- getConfidenceInterval(temp[[paste0("(",i,",",j,")")]], nsr, confidenceIntervalLevel = confidenceIntervalLevel)

      results[[paste0("(",i,",",j,")")]] <- cbind(tibble::tibble(csr = csr,
                                      asr = asr),
                                      counts) %>%
        dplyr::mutate(index_id = i, marker_id = j)
    }
  }

  output <- Reduce(dplyr::union_all, results) %>%
    PatientProfiles::addCdmName(cdm) %>%
    dplyr::select("index_id", "marker_id", "index_first", "marker_first", "csr", "asr", "lowerCSR_CI", "upperCSR_CI", "lowerASR_CI", "upperASR_CI", "cdm_name")

  return(output)
}
