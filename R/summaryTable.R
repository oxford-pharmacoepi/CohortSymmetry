### A summary table required to compute asr, csr and nsr
# and produces a summary table with one column indicating how many days it has been since the the very first drug
# and the number of cases where the marker was prescribed first and the index was prescribed first.

summaryTable <- function(table) {

  colChecks(table, c("dateIndexDrug", "dateMarkerDrug"))

  # creating order, orderBA is TRUE if index is AFTER marker
  table <-
    table %>%
    dplyr::filter((!is.na(.data$dateIndexDrug)) & (!is.na(.data$dateMarkerDrug))) %>%
    dplyr::mutate(orderBA = .data$dateIndexDrug >= .data$dateMarkerDrug)

  # min date of any drug start
  date_start <- min(dplyr::pull(.data$table, .data$dateIndexDrug), dplyr::pull(.data$table, .data$dateMarkerDrug))

  # lubridate package used, days(1), as_date() and %--%.
  table <-
    table %>%
    dplyr::mutate(
      date_first = lubridate::as_date(ifelse(.data$orderBA, .data$dateMarkerDrug, .data$dateIndexDrug)), # setting which date is first and which is second
      date_second = lubridate::as_date(ifelse(.data$orderBA, .data$dateIndexDrug, .data$dateMarkerDrug)),
      days_first = as.integer((lubridate::interval(.data$date_start, .data$date_first)) / lubridate::days(1)), # gap between the first drug of a person and the first drug of the whole population
      days_second = as.integer((lubridate::interval(.data$date_first, .data$date_second)) / lubridate::days(1)) # gap between two drugs of a person
    )

  #max number of digits in days_first
  # max_dig <- nchar(as.character(max(dplyr::pull("table", "days_first"))))
  # ch_format <- paste0("%0", max_dig, ".0f")

  table <-
    table %>%
    # dplyr::mutate(
    #   days_first_ch_format = sprintf(ch_format, days_first) # make a column with days_first having the same number of digits - Why?
    # ) %>%
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
