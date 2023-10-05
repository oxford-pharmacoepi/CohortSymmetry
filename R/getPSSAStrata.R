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
