##### getPSSA (complete approach)
##### either give both index and marker names or
##### cohort_table (latter most preferably being generated from generateDrugCohortPSSA())

#' Get PPSA
#'
#' @param cdm A cdm reference
#' @param index Index drug
#' @param marker Marker drug
#' @param cohort_table Cohort table
#' @param table_name Table name
#' @param study_time Study time
#' @param confidence_interval_level Confidence interval level
#' @param prior_obs Prior observation
#' @param start_date Start date
#' @param end_date End date
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
  csr<-crudeSequenceRatio(summaryTable(table_cleaned))
  asr<-adjustedSequenceRatio(summaryTable(table_cleaned))
  counts <- getConfidenceInterval(summaryTable(table_cleaned), confidence_interval_level = confidence_interval_level)

  results <- tibble::tibble(name = table_name,
                    csr = csr,
                    asr = asr)

  results <- cbind(results, counts)

  result <- list(table_cleaned, results)

  return(result)
}
