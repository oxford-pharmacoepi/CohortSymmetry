### CSR
crudeSequenceRatio <- function(table) {

  n_index_before_marker <- table |>
    dplyr::pull("index_first") |>
    sum()
  n_marker_before_index <- table |>
    dplyr::pull("marker_first") |>
    sum()

  crudeSequenceRatio <- n_index_before_marker / n_marker_before_index

  return(crudeSequenceRatio)

}
