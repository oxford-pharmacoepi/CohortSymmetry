##### getPSSA (complete approach)
##### either give both index and marker names or
##### cohort_table (latter most preferably being generated from generateDrugCohortPSSA())

getPSSA <- function(cdm,
                    index,
                    marker,
                    cohort_table = NULL,
                    table_name = "pssa",
                    study_time = NULL,
                    confidenceIntervalLevel = 0.025,
                    prior_obs = 365,
                    start_date = NA,
                    end_date = NA # set both as NA for full
){
  if (!is.null(cohort_table)){
    table <- cohort_table
  } else {
    table <- generateDrugCohortPSSA(cdm = cdm, index = index, marker = marker, table_name = table_name, prior_obs = prior_obs, start_date = start_date, end_date = end_date)
  }
  table_cleaned <- tableCleaning(table = table, study_time = study_time)
  csr<-crudeSequenceRatio(table_cleaned[[2]])
  asr<-adjustedSequenceRatio(table_cleaned[[2]])
  counts <- getConfidenceInterval(table_cleaned[[2]], confidenceIntervalLevel = confidenceIntervalLevel)

  results <- tibble::tibble(name = table_name,
                    csr = csr,
                    asr = asr)

  results <- cbind(results, counts)

  result <- list(table_cleaned[[1]], results)

  return(result)
}
