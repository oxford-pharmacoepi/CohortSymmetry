### NSR (uses tableCleaning, days_first, marker_first and index_first)
nullSequenceRatio <- function(table, restriction = 548) {

  n_index_before_marker <- table %>% dplyr::pull(.data$index_first) %>% sum()
  n_marker_before_index <- table %>% dplyr::pull(.data$marker_first) %>% sum()

  numer <- 0
  denom <- 0

  # The case restriction is finite:
  if (is.finite(restriction)) {

    table <-
      table %>%
      dplyr::mutate(
        marker_cumsum_fwd = deltaCumulativeSum(.data$marker_first, .data$days_first, restriction, backwards = FALSE), # For each days_first, look back 548 (restriction) days and see how many marker_first are there
        marker_cumsum_bwd = deltaCumulativeSum(.data$marker_first, .data$days_first, restriction, backwards = TRUE), # For each days_first, look forward 548 (restriction days) and see how many marker_first are there
        numerator = .data$index_first * .data$marker_cumsum_fwd,
        denominator = .data$index_first * (.data$marker_cumsum_bwd + .data$marker_cumsum_fwd - .data$marker_first), # why the minus - mistake?
      )

    numer <- table %>% dplyr::pull(.data$numerator) %>% sum()
    denom <- table %>% dplyr::pull(.data$denominator) %>% sum()

  } else {
    # The case restriction is infinite:
    # Hallas 1996 Appendix
    numer <-
      table %>%
      dplyr::mutate(
        marker_cumsum = .data$n_marker_before_index - cumsum(.data$marker_first),
        numerator = .data$index_first * .data$marker_cumsum
      ) %>%
      dplyr::pull(.data$numerator)

    numer <- sum(numer)
    denom <- .data$n_marker_before_index * .data$n_index_before_marker

  }

  if (numer < 1)
    warning("NSR numerator is 0, which results in a NSR = 0, proceed with caution")

  if (denom < 1){
    warning("NSR denominator is 0, suggesting no Marker Drug -> Index Drug or Index Drug -> Marker Drug events")
    nullSequenceRatio <- NA
  } else {
    a <- numer / denom

    nullSequenceRatio <- a / (1 - a)
  }

  return(nullSequenceRatio)

}
