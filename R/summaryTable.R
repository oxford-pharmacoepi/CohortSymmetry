### A summary table required to compute asr, csr and nsr
# and produces a summary table with one column indicating how many days it has been since the the very first drug
# and the number of cases where the marker was prescribed first and the index was prescribed first.

summaryTable <- function(table) {

  colChecks(table, c("dateIndexDrug", "dateMarkerDrug"))

  # creating order, orderBA is TRUE if index is AFTER marker
  table <-
    table %>%
    dplyr::filter((!is.na(.env$dateIndexDrug)) & (!is.na(.env$dateMarkerDrug))) %>%
    dplyr::mutate(orderBA = .env$dateIndexDrug >= .env$dateMarkerDrug)

  # min date of any drug start
  date_start <- min(table %>% dplyr::pull(.env$dateIndexDrug), table %>% dplyr::pull(.env$dateMarkerDrug))

  table <-
    table %>%
    dplyr::mutate(
      date_first = lubridate::as_date(ifelse(.env$orderBA, .env$dateMarkerDrug, .env$dateIndexDrug)), # setting which date is first and which is second
      date_second = lubridate::as_date(ifelse(.env$orderBA, .env$dateIndexDrug, .env$dateMarkerDrug)),
      days_first = as.integer((lubridate::interval(.env$date_start, .env$date_first)) / lubridate::days(1)), # gap between the first drug of a person and the first drug of the whole population
      days_second = as.integer((lubridate::interval(.env$date_first, .env$date_second)) / lubridate::days(1)) # gap between two drugs of a person
    )

  table <-
    table %>%
    dplyr::arrange(.data$days_first)

  ### final output, a dataframe with four columns, days_first_ch_format, days_first, marker_first and index_first.
  # days_first_ch_format: days_first in ch format
  # days_first: gap between the first drug date of an individual to the first drug date of everyone
  # marker_first: for a given days_first, how many were marker first
  # index_first: for a given days_first, how many were index first
  dat <-
    table %>%
    dplyr::group_by(.data$days_first) %>%
    dplyr::summarise(marker_first = sum(.data$orderBA), index_first = sum(!.data$orderBA), .groups = "drop") %>%
    dplyr::ungroup()

  return(dat)
}
