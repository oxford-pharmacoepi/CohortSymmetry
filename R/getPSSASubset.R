# pssa based on a subset of a condition - requires predefined jsons to be instantiated
# inspired by IncidencePrevalence
getPSSASubset <- function(cdm, index, marker, subset_name, subset_id, table_name = "pssa", study_time = NULL, confidence_interval_level = 0.025){
  cdm[["subset"]] <- cdm[[subset_name]] %>% filter(cohort_definition_id == subset_id)
  subset_cdm <- cdmSubsetCohort(cdm, "subset")
  subset_result <- getPSSA(cdm = subset_cdm,
                           index = index,
                           marker = marker,
                           table_name = table_name,
                           study_time = study_time,
                           confidence_interval_level = confidence_interval_level)
  return(subset_result)
}
