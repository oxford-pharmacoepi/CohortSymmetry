# pssa based on a subset of a condition - requires predefined jsons to be instantiated
# inspired by IncidencePrevalence
#' PSSA calculations with the underlying population restricted by a predefined JSON file.
#'
#' @param cdm A CDM reference.
#' @param index Index drug(s), need to specify the name and the level in the ATC format.
#' @param marker Marker drug(s), need to specify the name and the level in the ATC format.
#' @param subset_name A table in the CDM that the user wants to subset the underlying by.
#' @param subset_id The cohort definition ID of the denominator cohort of interest.
#' @param table_name Table name in CDM of user's choice.
#' @param study_time Study time is defined to be the gap between the initiation of index and the initiation of marker. Default is NULL, meaning no restriction is imposed.
#' @param confidence_interval_level Default 0.025, meaning 95% interval. If the user wants N% interval (N between 0 and 100) then change the default to (1-N/100)/2.
#'
#' @return
#' @export
#'
#' @examples
getPSSASubset <- function(cdm, index, marker, subset_name, subset_id, table_name = "pssa", study_time = NULL, confidence_interval_level = 0.025){
  cdm[["subset"]] <- cdm[[subset_name]] %>% dplyr::filter(.data$cohort_definition_id == subset_id)
  subset_cdm <- CDMConnector::cdmSubsetCohort(cdm, "subset")
  subset_result <- getPSSA(cdm = subset_cdm,
                           index = index,
                           marker = marker,
                           table_name = table_name,
                           study_time = study_time,
                           confidence_interval_level = confidence_interval_level)
  return(subset_result)
}
