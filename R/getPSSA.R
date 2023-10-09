##### getPSSA (complete approach)
##### either give both index and marker names or
##### cohort_table (latter most preferably being generated from generateDrugCohortPSSA())

#' PSSA Calculations including CSR, ASR and CI.
#'
#' @description
#' Using a cohort table with two different cohort_definition_id (coded 1 and 2)
#' to compute the necessary PSSA calculations.
#' Either, using the cohort table produced by generateDrugCohort function,
#' or simply specify index and marker drug(s) and leave cohort_table NULL.
#' The outputs include csr (Crude Sequence Ratio), asr (Adjusted sequence ratio),
#' confidence intervals and relevant counts (marker_first and index_first).
#'
#' @param cdm A CDM reference.
#' @param index Index drug(s), need to specify the name and the level in the ATC format.
#' @param marker Marker drug(s), need to specify the name and the level in the ATC format.
#' @param cohort_table Cohort table produced using generateDrugCohort() function. Default NA.
#' @param table_name Table name in CDM of user's choice.
#' @param study_time Study time is defined to be the gap between the initiation of index and the initiation of marker. Default is NULL, meaning no restriction is imposed.
#' @param confidence_interval_level Default 0.025, meaning 95% interval. If the user wants N% interval (N between 0 and 100) then change the default to (1-N/100)/2.
#' @param prior_obs Prior observation that the user would like to impose on both index drug(s) and marker drug(s).
#' @param start_date Start date that the user would impose so that both index drug(s) and marker drug(s) would be initiated after this day. Set NA if this is not necessary.
#' @param end_date End date that the user would impose so that both index drug(s) and marker drug(s) would be initiated before this day. Set NA if this is not necessary.
#'
#' @return
#' @export
#'
#' @examples
getPSSA <- function(cdm,
                    index,
                    marker,
                    cohort_table = NULL,
                    table_name = "pssa",
                    study_time = NULL,
                    confidence_interval_level = 0.025,
                    prior_obs = 365,
                    start_date = NA,
                    end_date = NA # set both as NA for full
){
  if (!is.null(cohort_table)){
    colChecks(cohort_table, c("cohort_definition_id", "subject_id", "cohort_start_date"))
    table <- cohort_table
  } else {
    table <- generateDrugCohortPSSA(cdm = cdm, index = index, marker = marker, table_name = table_name, prior_obs = prior_obs, start_date = start_date, end_date = end_date)
  }
  table_cleaned <- tableCleaning(table = table, study_time = study_time)
  csr<-crudeSequenceRatio(table_cleaned[[2]])
  asr<-adjustedSequenceRatio(table_cleaned[[2]])
  counts <- getConfidenceInterval(table_cleaned[[2]], confidence_interval_level = confidence_interval_level)

  results <- tibble::tibble(name = table_name,
                    csr = csr,
                    asr = asr)

  results <- cbind(results, counts)

  result <- list(table_cleaned[1], results)

  return(result)
}
