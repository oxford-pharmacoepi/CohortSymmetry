#' A tidy visualization of sequence_symmetry objects.
#'
#' @description
#' It provides a tidy dataframe with the contents of the summariseSequenceRatio
#' output.
#'
#' @param result A sequence_symmetry object.
#'
#' @return A tibble with a tidy version of the sequence_symmetry
#' object.
#'
#' @export
#' @examples
#' \donttest{
#' library(CohortSymmetry)
#' cdm <- mockCohortSymmetry()
#' cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
#'                                                  name = "joined_cohorts",
#'                                                  indexTable = "cohort_1",
#'                                                  markerTable = "cohort_2",
#'                                                  combinationWindow = c(0, Inf))
#' res <- CohortSymmetry::summariseSequenceRatio(
#' cdm = cdm,
#' sequenceCohortSet = "joined_cohorts")
#'
#' tidy_result <- CohortSymmetry::tidySequenceSymmetry(res)
#'
#' tidy_result
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }

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
