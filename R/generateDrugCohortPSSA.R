#generate drug cohort using DrugUtilisation - to be fed into getPSSA()
generateDrugCohortPSSA <- function(cdm, index, marker, table_name = "pssa", prior_obs = 365, start_date, end_date){
  index_drug <- list()
  marker_drug <- list()

  for (i in (1: length(index))){
    if (index[[i]][2] == "ingredient"){
      index_drug[[i]] <- getDrugIngredientCodes(cdm = cdm, name = index[[i]][1])
    } else {
      index_drug[[i]] <- getATCCodes(cdm = cdm, name = index[[i]][1], level = c(index[[i]][2]))
    }
  }

  for (i in (1: length(marker))){
    if (marker[[i]][2] == "ingredient"){
      marker_drug[[i]] <- getDrugIngredientCodes(cdm = cdm, name = marker[[i]][1])
    } else {
      marker_drug[[i]] <- getATCCodes(cdm = cdm, name = marker[[i]][1], level = c(marker[[i]][2]))
    }
  }

  conceptSetList <- c()
  for (i in (1:length(index_drug))){
    conceptSetList <- c(conceptSetList, index_drug[[i]])
  }

  for (i in (1: length(marker_drug))){
    conceptSetList <- c(conceptSetList, marker_drug[[i]])
  }

  cdm <- generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = table_name,
    conceptSetList = conceptSetList,
    summariseMode = "FirstEra",
    daysPriorObservation = prior_obs,
    cohortDateRange = as.Date(c(start_date, end_date))
  )

  index_length <- 0
  for (i in (1:length(index_drug))){
    index_length <- index_length + length(index_drug[[i]])
  }

  cdm[[table_name]] <- cdm[[table_name]] %>%
    collect() %>%
    dplyr::mutate(cohort_definition_id = case_when(cohort_definition_id <= index_length ~ 1,
                                                   cohort_definition_id > index_length ~ 2)) %>%
    dplyr::mutate(cohort_definition_id = as.integer(cohort_definition_id)) %>%
    group_by(cohort_definition_id, subject_id) %>%
    arrange(cohort_start_date, .by_group =T) %>%
    filter(row_number()==1) %>%
    ungroup() %>%
    compute()

  raw_table <- cdm[[table_name]]

  return(raw_table)
}
