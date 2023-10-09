### Input a cdm table and a study period and output a cleaned version of table, so that it can be used in asr
tableCleaning <- function(table, study_time = NULL){
  colChecks(table, c("cohort_definition_id", "cohort_start_date"))
  if (!setequal((table %>% dplyr::pull(.data$cohort_definition_id) %>% unique()), c(1,2)))
    stop("table doesn't have the right format, cohort_definition_id should have both 1 and 2 and only 1 and 2.")
  if (is.null(study_time)){
    dat <-
      table %>%
      dplyr::select(.data$cohort_definition_id, .data$subject_id, .data$cohort_start_date) %>%
      tidyr::pivot_wider(names_from = .data$cohort_definition_id, values_from = .data$cohort_start_date) %>%
      dplyr::rename("dateIndexDrug" = .data$`1`, "dateMarkerDrug" = .data$`2`) %>%
      dplyr::mutate(gap = .data$dateMarkerDrug - .data$dateIndexDrug) %>%
      dplyr::filter(!is.na(.data$gap)) %>%
      dplyr::filter(!.data$gap==0) %>%
      dplyr::select(-.data$gap) %>%
      dplyr::collect() %>%
      dplyr::select(-.data$subject_id)
  }
  else{
    dat <-
      table %>%
      dplyr::select(.data$cohort_definition_id, .data$subject_id, .data$cohort_start_date) %>%
      tidyr::pivot_wider(names_from = .data$cohort_definition_id, values_from = .data$cohort_start_date) %>%
      dplyr::rename("dateIndexDrug" = .data$`1`, "dateMarkerDrug" = .data$`2`) %>%
      dplyr::mutate(gap = .data$dateMarkerDrug - .data$dateIndexDrug) %>%
      dplyr::filter(!is.na(.data$gap)) %>%
      dplyr::filter(!.data$gap==0) %>%
      dplyr::filter(-study_time<= .data$gap & .data$gap <= study_time) %>%
      dplyr::select(-.data$gap, - .data$subject_id) %>%
      dplyr::collect()
  }

  date_start <- min(dat %>% dplyr::pull(.data$dateIndexDrug), table %>% dplyr::pull(.data$dateMarkerDrug))

  dat <-
    dat %>%
    dplyr::filter((!is.na(.data$dateIndexDrug)) & (!is.na(.data$dateMarkerDrug))) %>%
    dplyr::mutate(orderBA = .data$dateIndexDrug >= .data$dateMarkerDrug) %>%
    dplyr::mutate(
      date_first = lubridate::as_date(ifelse(.data$orderBA, .data$dateMarkerDrug, .data$dateIndexDrug)), # setting which date is first and which is second
      date_second = lubridate::as_date(ifelse(.data$orderBA, .data$dateIndexDrug, .data$dateMarkerDrug)),
      days_first = as.integer((lubridate::interval(.data$date_start, .data$date_first)) / lubridate::days(1)), # gap between the first drug of a person and the first drug of the whole population
      days_second = as.integer((lubridate::interval(.data$date_first, .data$date_second)) / lubridate::days(1)) # gap between two drugs of a person
    ) %>%
    dplyr::arrange(.data$days_first) %>%
    dplyr::group_by(.data$days_first) %>%
    dplyr::summarise(marker_first = sum(.data$orderBA), index_first = sum(!.data$orderBA), .groups = "drop") %>%
    dplyr::ungroup()

  return(dat)
}
