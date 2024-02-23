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
#' cdm <- mockPatientProfiles(cohort1 = cohort1,
#'                             patient_size = 10)
#' cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
#'                                           indexTable = "cohort1",
#'                                           markerTable = "cohort2")
#'  pssa_result <- CohortSymmetry::getSequenceRatios (cdm = cdm,
#'                                                    outcomeTable = "joined_cohorts")
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
  temp2<-list()
  results <- list()
  for (i in (cdm[[outcomeTable]] %>% dplyr::distinct(.data$index_id) %>% dplyr::pull())){
    for (j in (cdm[[outcomeTable]] %>% dplyr::filter(.data$index_id == i) %>% dplyr::distinct(.data$marker_id) %>% dplyr::pull())){
      temp[[paste0("index_",i, "_marker_", j)]] <-
        cdm[[outcomeTable]] %>%
        dplyr::filter(.data$index_id == i & .data$marker_id == j) %>%
        dplyr::left_join(
          cdm[[outcomeTable]] %>%
            dplyr::filter(.data$index_id == i & .data$marker_id == j) %>%
            dplyr::group_by(.data$index_id, .data$marker_id) %>%
            dplyr::summarise(date_start = min(.data$first_date, na.rm = T),
                             .groups = "drop") %>%
            dplyr::ungroup(),
          by  = c("index_id", "marker_id")
        ) %>%
        dplyr::mutate(
          orderBA = .data$index_date >= .data$marker_date,
          days_first = as.numeric(!!CDMConnector::datediff(
            "date_start", "first_date"
          )), # gap between the first drug of a person and the first drug of the whole population
          days_second = as.numeric(!!CDMConnector::datediff(
            "first_date", "second_date"
          ))) %>%
        dplyr::collect() %>%
        dplyr::group_by(.data$days_first, .data$index_id, .data$index_name, .data$marker_id, .data$marker_name, .data$days_prior_observation, .data$washout_window, .data$index_marker_gap, .data$combination_window) %>%
        dplyr::summarise(marker_first = sum(.data$orderBA, na.rm = T), index_first = sum((!.data$orderBA), na.rm = T), .groups = "drop") %>%
        dplyr::ungroup()

      temp2[[paste0("index_",i, "_marker_", j)]] <-
        temp[[paste0("index_",i, "_marker_", j)]] %>%
        dplyr::group_by(.data$index_id, .data$index_name, .data$marker_id, .data$marker_name, .data$days_prior_observation, .data$washout_window, .data$index_marker_gap, .data$combination_window) %>%
        dplyr::summarise(marker_first = sum(.data$marker_first), index_first = sum(.data$index_first), .groups = "drop") %>%
        dplyr::ungroup()

      csr<-crudeSequenceRatio(temp[[paste0("index_",i, "_marker_", j)]])
      nsr<-nullSequenceRatio(temp[[paste0("index_",i, "_marker_", j)]], restriction = restriction)
      asr<-adjustedSequenceRatio(temp[[paste0("index_",i, "_marker_", j)]], restriction = restriction)
      counts <- getConfidenceInterval(temp[[paste0("index_",i, "_marker_", j)]], nsr, confidenceIntervalLevel = confidenceIntervalLevel) %>%
        dplyr::select(-"index_first_by_nsr", -"marker_first_by_nsr", -"index_first", -"marker_first")

      results[[paste0("index_",i, "_marker_", j)]] <- cbind(temp2[[paste0("index_",i, "_marker_", j)]],
                                                            cbind(tibble::tibble(csr = csr,asr = asr),
                                                                  counts)) %>%
        dplyr::mutate(marker_first_percentage = round(.data$marker_first/(.data$marker_first + .data$index_first)*100, digits = 1),
                      index_first_percentage = round(.data$index_first/(.data$marker_first + .data$index_first)*100, digits = 1)) %>%
        dplyr::select("index_id", "index_name", "marker_id", "marker_name",
                      "index_first", "marker_first", "index_first_percentage", "marker_first_percentage",
                      "csr", "lowerCSR_CI", "upperCSR_CI",
                      "asr", "lowerASR_CI", "upperASR_CI",
                      "days_prior_observation", "washout_window", "index_marker_gap", "combination_window")
    }
  }

  output <- Reduce(dplyr::union_all, results) %>%
    PatientProfiles::addCdmName(cdm) %>%
    getComparedResult()

  return(output)
}
