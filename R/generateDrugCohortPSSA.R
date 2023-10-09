#generate drug cohort using DrugUtilisation - to be fed into getPSSA()
#' Generate PSSA Drug Cohort
#' @description
#' Generate the necessary drug cohorts based on the index drug(s) and
#' marker drug(s) specified. The output of this function will subsequently
#' be fed into, for exmaple, getPSSA().
#'
#' @param cdm A CDM reference.
#' @param index Index drug(s), need to specify the name and the level in the ATC format.
#' @param marker Marker drug(s), need to specify the name and the level in the ATC format.
#' @param table_name Table name in CDM of user's choice.
#' @param prior_obs Prior observation that the user would like to impose on both index drug(s) and marker drug(s).
#' @param start_date Start date that the user would impose so that both index drug(s) and marker drug(s) would be initiated after this day. Set NA if this is not necessary.
#' @param end_date End date that the user would impose so that both index drug(s) and marker drug(s) would be initiated before this day. Set NA if this is not necessary.
#'
#' @return
#' @export
#'
#' @examples
generateDrugCohortPSSA <- function(cdm, index, marker, table_name = "pssa", prior_obs = 365, start_date = NA, end_date = NA){
  index_drug <- list()
  marker_drug <- list()

  for (i in (1: length(index))){
    if (index[[i]][2] == "ingredient"){
      index_drug[[i]] <- CodelistGenerator::getDrugIngredientCodes(cdm = cdm, name = index[[i]][1])
    } else {
      index_drug[[i]] <- CodelistGenerator::getATCCodes(cdm = cdm, name = index[[i]][1], level = c(index[[i]][2]))
    }
  }

  for (i in (1: length(marker))){
    if (marker[[i]][2] == "ingredient"){
      marker_drug[[i]] <- CodelistGenerator::getDrugIngredientCodes(cdm = cdm, name = marker[[i]][1])
    } else {
      marker_drug[[i]] <- CodelistGenerator::getATCCodes(cdm = cdm, name = marker[[i]][1], level = c(marker[[i]][2]))
    }
  }

  conceptSetList <- c()
  for (i in (1:length(index_drug))){
    conceptSetList <- c(conceptSetList, index_drug[[i]])
  }

  for (i in (1: length(marker_drug))){
    conceptSetList <- c(conceptSetList, marker_drug[[i]])
  }

  cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
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
    dplyr::collect() %>%
    dplyr::mutate(cohort_definition_id = dplyr::case_when(.data$cohort_definition_id <= index_length ~ 1,
                                                          .data$cohort_definition_id > index_length ~ 2)) %>%
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id)) %>%
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
    dplyr::arrange(.data$cohort_start_date, .by_group =T) %>%
    dplyr::filter(dplyr::row_number()==1) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  raw_table <- cdm[[table_name]]

  return(raw_table)
}
