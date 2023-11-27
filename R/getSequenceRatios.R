#' Sequence ratio calculations
#'
#' @description
#' Using getCohortSequence to obtain sequence ratios for the desired outcomes.
#'
#' @param cdm A CDM reference.
#' @param outcomeTable A table in the CDM that the output of getCohortSequence resides.
#' @param confidenceIntervalLevel Default is 0.025, which is the central 95% confidence interval.
#'
#' @return
#' A local table with all analyses with
#' @export
#'
#' @examples
getSequenceRatios <- function(cdm,
                              outcomeTable,
                              confidenceIntervalLevel = 0.025){

  # Check cdm objects, writing schema and index/marker tables
  checkCdm(cdm, tables=c(outcomeTable))
  assertWriteSchema(cdm)

  errorMessage <- checkmate::makeAssertCollection()
  # check relevant formats of the arguments
  checkmate::assertCharacter(outcomeTable, len = 1, any.missing = FALSE, add = errorMessage)

  temp <- list()
  results <- list()
  for (i in (cdm[[outcomeTable]] %>% dplyr::distinct(.data$index_id) %>% dplyr::pull())){
    for (j in (cdm[[outcomeTable]] %>% dplyr::filter(.data$index_id == i) %>% dplyr::distinct(.data$marker_id) %>% dplyr::pull())){
      temp[[(2^i)*(3^j)]] <-
        cdm[[outcomeTable]] %>%
        dplyr::filter(.data$index_id == i & .data$marker_id == j) %>%
        dplyr::collect()

      date_start <- min(temp[[(2^i)*(3^j)]]$index_date, temp[[(2^i)*(3^j)]]$marker_date)

      temp[[(2^i)*(3^j)]] <-
        temp[[(2^i)*(3^j)]] %>%
        dplyr::mutate(
          orderBA = .data$index_date >= .data$marker_date,
          days_first = as.integer((lubridate::interval(date_start, .data$first_date)) / lubridate::days(1)), # gap between the first drug of a person and the first drug of the whole population
          days_second = as.integer((lubridate::interval(.data$first_date, .data$second_date)) / lubridate::days(1))) %>%  # gap between two drugs of a person
        dplyr::arrange(.data$days_first) %>%
        dplyr::group_by(.data$days_first) %>%
        dplyr::summarise(marker_first = sum(.data$orderBA), index_first = sum(!.data$orderBA), .groups = "drop") %>%
        dplyr::mutate(index_id = i, marker_id = j) %>%
        dplyr::ungroup()

      csr<-crudeSequenceRatio(temp[[(2^i)*(3^j)]])
      asr<-adjustedSequenceRatio(temp[[(2^i)*(3^j)]])
      counts <- getConfidenceInterval(temp[[(2^i)*(3^j)]], confidence_interval_level = confidenceIntervalLevel)

      results[[(2^i)*(3^j)]] <- cbind(tibble::tibble(csr = csr,
                                      asr = asr),
                                      counts) %>%
        dplyr::mutate(index_id = i, marker_id = j)
    }
  }

  results <- results[!sapply(results, is.null)]
  output <- Reduce(dplyr::union_all, results) %>%
    PatientProfiles::addCdmName(cdm) %>%
    dplyr::select(.data$index_id, .data$marker_id, .data$index_first, .data$marker_first, .data$csr, .data$asr, .data$lowerCI, .data$upperCI, .data$cdm_name)

  return(output)
}
