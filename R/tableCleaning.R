### Input a cdm table and a study period and output a cleaned version of table, so that it can be used in asr
tableCleaning <- function(table, study_time = NULL){
  colChecks(table, c("cohort_definition_id", "cohort_start_date"))
  if (!setequal((table %>% dplyr::pull(cohort_definition_id) %>% unique()), c(1,2)))
    stop("table doesn't have the right format, cohort_definition_id should have both 1 and 2 and only 1 and 2.")
  if (is.null(study_time)){
    dat <-
      table %>%
      dplyr::select(cohort_definition_id, subject_id, cohort_start_date) %>%
      tidyr::pivot_wider(names_from = cohort_definition_id, values_from = cohort_start_date) %>%
      dplyr::rename("dateIndexDrug" = `1`, "dateMarkerDrug" = `2`) %>%
      dplyr::mutate(gap = dateMarkerDrug - dateIndexDrug) %>%
      dplyr::filter(!is.na(gap)) %>%
      dplyr::filter(!gap==0) %>%
      dplyr::select(-gap) %>%
      dplyr::collect() %>%
      dplyr::select(-subject_id)
  }
  else{
    dat <-
      table %>%
      dplyr::select(cohort_definition_id, subject_id, cohort_start_date) %>%
      tidyr::pivot_wider(names_from = cohort_definition_id, values_from = cohort_start_date) %>%
      dplyr::rename("dateIndexDrug" = `1`, "dateMarkerDrug" = `2`) %>%
      dplyr::mutate(gap = dateMarkerDrug - dateIndexDrug) %>%
      dplyr::filter(!is.na(gap)) %>%
      dplyr::filter(!gap==0) %>%
      dplyr::filter(-study_time<= gap & gap <= study_time) %>%
      dplyr::select(-gap) %>%
      dplyr::collect() %>%
      dplyr::select(-subject_id)
  }
  return(dat)
}
