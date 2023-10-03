### NSR (uses summary table, days_first, marker_first and index_first)
nullSequenceRatio <- function(table, restriction = 548) {

  colChecks(table, c("days_first", "marker_first", "index_first"))

  n_index_before_marker <- table %>% dplyr::pull(index_first) %>% sum(.)
  n_marker_before_index <- table %>% dplyr::pull(marker_first) %>% sum(.)

  numer <- 0
  denom <- 0

  # The case restriction is finite:
  if (is.finite(restriction)) {

    table <-
      table %>%
      mutate(
        marker_cumsum_fwd = deltaCumulativeSum(marker_first, days_first, restriction, backwards = FALSE), # For each days_first, look back 548 (restriction) days and see how many marker_first are there
        marker_cumsum_bwd = deltaCumulativeSum(marker_first, days_first, restriction, backwards = TRUE), # For each days_first, look forward 548 (restriction days) and see how many marker_first are there
        numerator = index_first * marker_cumsum_fwd,
        denominator = index_first * (marker_cumsum_bwd + marker_cumsum_fwd - marker_first), # why the minus - mistake?
      )

    numer <- table %>% dplyr::pull(numerator) %>% sum(.)
    denom <- table %>% dplyr::pull(denominator) %>% sum(.)

  } else {
    # The case restriction is infinite:
    # Hallas 1996 Appendix
    numer <-
      table %>%
      mutate(
        marker_cumsum = n_marker_before_index - cumsum(marker_first),
        numerator = index_first * marker_cumsum
      ) %>%
      dplyr::pull(numerator)

    numer <- sum(numer)
    denom <- n_marker_before_index * n_index_before_marker

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
