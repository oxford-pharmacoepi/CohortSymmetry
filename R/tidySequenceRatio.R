#' A tidy visualization of sequence_symmetry objects.
#'
#' @description
#' It provides a tidy dataframe with the contents of the getSequenceRatios
#' output.
#'
#' @param result A sequence_symmetry object.
#' @param minCellCount Minimum number of counts to report.
#'
#' @return A tibble with a tidy version of the sequence_symmetry
#' object.
#'
#' @export
#'
#' @examples
#' {
#' }
#'
tidySequenceSymmetry <- function(result, minCellCount) {
  # checks
  checkTidySequenceSymmetry(result, minCellCount)
  #
  # result |>
  #   visOmopResults::spl





}
