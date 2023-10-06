### Waiting Time Distribution
# Generate a single drug cohort, similar to generateDrugCohortPSSA()
generateSingleDrugCohort <- function(cdm, drug, table_name = "wtd", start_date, end_date, prior_obs = 365){
  drug_name <- list()

  for (i in (1: length(drug))){
    if (drug[[i]][2] == "ingredient"){
      drug_name[[i]] <- CodelistGenerator::getDrugIngredientCodes(cdm = cdm, name = drug[[i]][1])
    } else {
      drug_name[[i]] <- CodelistGenerator::getATCCodes(cdm = cdm, name = drug[[i]][1], level = c(drug[[i]][2]))
    }
  }

  conceptSetList <- c()
  for (i in (1:length(drug_name))){
    conceptSetList <- c(conceptSetList, drug_name[[i]])
  }

  cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = table_name,
    conceptSetList = conceptSetList,
    summariseMode = "FirstEra",
    daysPriorObservation = .env$prior_obs,
    cohortDateRange = as.Date(c(.env$start_date, .env$end_date))
  )

  raw_table <- cdm[[table_name]] %>%
    dplyr::collect()

  return(raw_table)
}
