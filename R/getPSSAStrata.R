#' PSSA in age and sex strata
#' @description
#' PSSA calculations with different age and sex strata
#'
#' @param cdm A CDM reference.
#' @param ageGroup Age groups, for example: list(c(0,17), c(18,30))
#' @param sex Sex groups, for example: list(c("Male", "Female", "Both"))
#' @param index Index drug(s), need to specify the name and the level in the ATC format.
#' @param marker Marker drug(s), need to specify the name and the level in the ATC format.
#' @param prior_obs Prior observation that the user would like to impose on both index drug(s) and marker drug(s).
#' @param start_date Start date that the user would impose so that both index drug(s) and marker drug(s) would be initiated after this day. Set NA if this is not necessary.
#' @param end_date End date that the user would impose so that both index drug(s) and marker drug(s) would be initiated before this day. Set NA if this is not necessary.
#' @param table_name Table name in CDM of user's choice.
#' @param study_time Study time is defined to be the gap between the initiation of index and the initiation of marker. Default is NULL, meaning no restriction is imposed.
#' @param confidence_interval_level Default 0.025, meaning 95% interval. If the user wants N% interval (N between 0 and 100) then change the default to (1-N/100)/2.
#'
#' @return
#' @export
#'
#' @examples
getPSSAStrata <- function(cdm,
                          ageGroup, #e.g., list(c(0,50), c(50,150), c(0,150))
                          sex, #e.g., sex = c("Male", "Female", "Both")
                          index,
                          marker,
                          prior_obs = 365,
                          start_date = NA,
                          end_date = NA,
                          table_name = "pssa",
                          study_time = NULL,
                          confidence_interval_level = 0.025){

  cdm <- IncidencePrevalence::generateDenominatorCohortSet(cdm = cdm,
                                                           ageGroup = ageGroup,
                                                           sex = sex)
  strata_results <- list()
  for (i in (1:nrow(CDMConnector::cohortSet(cdm$denominator)))){
    subject_ids <- cdm$denominator %>% dplyr::filter(.data$cohort_definition_id == i) %>% dplyr::pull("subject_id")

    drug_cohort <- generateDrugCohortPSSA(cdm = cdm,
                                          index = index,
                                          marker = marker,
                                          prior_obs = prior_obs,
                                          table_name = table_name,
                                          start_date = start_date,
                                          end_date = end_date) %>%
      dplyr::filter(.data$subject_id %in% subject_ids)

    cohort_groups <- CDMConnector::cohortSet(cdm$denominator) %>% dplyr::mutate(group = paste(.data$age_group, " ", sex))

    strata_results[[cohort_groups %>% dplyr::filter(.data$cohort_definition_id == i) %>% dplyr::pull("group")]]<-getPSSA(cohort_table = drug_cohort, study_time = study_time, confidence_interval_level = confidence_interval_level)

  }
  return(strata_results)
}
