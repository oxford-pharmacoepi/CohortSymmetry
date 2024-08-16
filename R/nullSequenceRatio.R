### NSR (uses tableCleaning, days_first, marker_first and index_first)
nullSequenceRatio <- function(table, movingAverageRestriction = 548) {

  n_index_before_marker <- table |> dplyr::pull("index_first") |> sum()
  n_marker_before_index <- table |> dplyr::pull("marker_first") |> sum()

  numer <- 0
  denom <- 0

  # The case restriction is finite:
  if (is.finite(movingAverageRestriction)) {

    table <-
      table |>
      dplyr::mutate(
        marker_cumsum_fwd = deltaCumulativeSum(.data$marker_first, .data$days_first, movingAverageRestriction, backwards = FALSE), # For each days_first, look back 548 (restriction) days and see how many marker_first are there
        marker_cumsum_bwd = deltaCumulativeSum(.data$marker_first, .data$days_first, movingAverageRestriction, backwards = TRUE), # For each days_first, look forward 548 (restriction days) and see how many marker_first are there
        numerator = .data$index_first * .data$marker_cumsum_fwd,
        denominator = .data$index_first * (.data$marker_cumsum_bwd + .data$marker_cumsum_fwd)
      )

    numer <- table |> dplyr::pull("numerator") |> sum()
    denom <- table |> dplyr::pull("denominator") |> sum()

  } else {
    # The case movingAverageRestriction is infinite:
    # Hallas 1996 Appendix
    numer <-
      table |>
      dplyr::mutate(
        marker_cumsum = .env$n_marker_before_index - cumsum(.data$marker_first),
        numerator = .data$index_first * .data$marker_cumsum
      ) |>
      dplyr::pull("numerator")

    numer <- sum(numer)
    denom <- n_marker_before_index * n_index_before_marker

  }

  # if (numer < 1)
  #   warning("NSR numerator is 0, which results in a NSR = 0, proceed with caution")

  if (denom < 1){
    nullSequenceRatio <- NA
  } else {
    a <- numer / denom

    nullSequenceRatio <- a / (1 - a)
  }

  return(nullSequenceRatio)

}

indexDelta <- function(x, delta, direction){
  ### For each i in 1 through length(x), it finds the max j such that x[j]-x[i]<=delta.
  # x needs to be non decreasing.
  # i.e., y[i] = max{j: x[j]-x[i]<=delta}
  if (direction == "Forward"){
    n <- length(x)
    y <- rep(0, n)

    j <- 2
    i <- 1

    while (j <= n) {

      if ((x[j] - x[i]) > delta) {
        y[i] <- j - 1
        i <- i + 1
      } else {
        j <- j + 1
      }

    }

    y[i:n] <- n

    return(y)
  }
  ### For each i in 1 through length(x), it finds the minimum j such that x[i]-x[j]<=delta.
  # x needs to be non decreasing.
  # i.e., y[i] = min{j: x[i]-x[j]<=delta}
  if (direction == "Backward"){
    n <- length(x)
    y <- rep(0, n)

    y[1] <- 1

    j <- 1
    i <- 2

    while (i <= n) {

      if ((x[i] - x[j]) <= delta) {
        y[i] <- j
        i <- i + 1
      } else {
        j <- j + 1
      }

    }

    return(y)

  }
}

#### Backward:
# y and t are vectors of the same length
# t is again non decreasing
# this is calculating the sum of y depending on t.
# for ith position, it looks at the ith value of t. Look back in t to find the minimum index
# of t such that the difference is less than or equal to delta. Say this index is j
# then for the ith position, it is summing y from j to i-1. By default/design the 1st entry is 0.

#### Forward:
# y and t are vectors of the same length
# t is again non decreasing
# this is calculating the sum of y depending on t.
# for ith position, it looks at the ith value of t. Look forward in t to find the maximum index
# of t such that the difference is less than or equal to delta. Say this index is j
# then for the ith position, it is summing y from i+1 to j. By default/design the last entry is 0.

deltaCumulativeSum <- function(y, t, delta, backwards = TRUE) {

  y_cumsum <- cumsum(y)

  if (backwards) {

    n_y <- length(y)
    prior_cumsum <- c(0, y_cumsum[-n_y])
    bwd_cumsum <- rep(0, n_y)
    t_delta_bwd <- indexDelta(t, delta, "Backward") - 1
    look_bwds <- t_delta_bwd > 0
    bwd_cumsum[look_bwds] <- y_cumsum[t_delta_bwd[look_bwds]]

    return(prior_cumsum - bwd_cumsum)

  } else {
    t_delta_fwd <- indexDelta(t, delta, "Forward")
    fwd_cumsum <- y_cumsum[t_delta_fwd]

    return(fwd_cumsum - y_cumsum)

  }

}
