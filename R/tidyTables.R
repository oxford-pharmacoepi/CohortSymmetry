#' A tidy visualization of sequence_symmetry objects.
#'
#' @description
#' It provides a tidy dataframe with the contents of the getSequenceRatios
#' output.
#'
#' @param result A sequence_symmetry object.
#'
#' @return A tibble with a tidy version of the sequence_symmetry
#' object.
#'
#' @export
#'

tidySequenceSymmetry <- function(result) {
  # checks
  checkSequenceSymmetry(result)

  result <- result |>
    visOmopResults::splitAll() |>
    dplyr::select(!c("result_type", "package_name", "package_version",
                     "estimate_type")) |>
    tidyr::pivot_wider(
      names_from = c("variable_level", "variable_name", "estimate_name"),
      values_from = "estimate_value"
    )

  return(result)
}
