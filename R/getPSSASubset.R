getPSSASubset <- function(cdm, index, marker, subset_name, subset_id, table_name = "pssa", study_time = NULL, confidenceIntervalLevel = 0.025){
  cdm[["subset"]] <- cdm[[subset_name]] %>% dplyr::filter(.data$cohort_definition_id == subset_id)
  subset_cdm <- CDMConnector::cdmSubsetCohort(cdm, "subset")
  subset_result <- getPSSA(cdm = subset_cdm,
                           index = index,
                           marker = marker,
                           table_name = table_name,
                           study_time = study_time,
                           confidenceIntervalLevel = confidenceIntervalLevel)
  return(subset_result)
}
