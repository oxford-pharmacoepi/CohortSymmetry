#' Creates mock cdm object for testing
#' @description
#' Creates a mock cdm with two default synthetic cohorts,
#' one is the index cohort and the other one is the marker cohort.
#' However the users could specify them should they wish.
#'
#' @param indexCohort the tibble of your index cohort.
#' Default is NULL, which means the default indexCohort is being used.
#' @param markerCohort the tibble of your marker cohort.
#' Default is NULL, which means the default indexCohort is being used.
#' @param con connection detail
#' @param schema name of your write schema
#'
#' @return mock cdm object contains your index and marker cohort
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortSymmetry)
#' cdm <- mockCohortSymmetry()
#' cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
#'                                                  name = "joined_cohorts",
#'                                                  indexTable = "cohort_1",
#'                                                  markerTable = "cohort_2",
#'                                                  combinationWindow = c(0, Inf))
#' res <- CohortSymmetry::getSequenceRatios(
#' cdm = cdm,
#' sequenceCohortSet = "joined_cohorts")
#' gtResult <- formatSequenceSymmetry(res)
#' }
mockCohortSymmetry <- function(indexCohort = NULL,
                               markerCohort = NULL,
                               con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
                               schema = "main") {

  if (is.null(indexCohort)){
    indexCohort <- dplyr::tibble(
      cohort_definition_id = c(1, 1, 2, 2, 1, 3, 3, 3, 1, 3),
      subject_id = c(1, 4, 2, 3, 5, 5, 4, 3, 3, 1),
      cohort_start_date = as.Date(
        c(
          "2020-04-01", "2021-06-01", "2022-05-22", "2010-01-01", "2019-08-01", "2019-04-07", "2021-01-01", "2008-02-02", "2009-09-09", "2021-01-01"
        )
      ),
      cohort_end_date = as.Date(
        c(
          "2020-04-01", "2021-08-01", "2022-05-23", "2010-03-01", "2020-04-01", "2020-05-30", "2022-02-02", "2013-12-03", "2009-11-01", "2021-01-01"
        )
      )
    )
  } else {
    indexCohort <- indexCohort
  }

  if (is.null(markerCohort)){
    markerCohort <- dplyr::tibble(
      cohort_definition_id = c(1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 2),
      subject_id = c(1, 3, 4, 2, 5, 1, 2, 3, 4, 5, 1),
      cohort_start_date = as.Date(
        c(
          "2020-12-30", "2010-01-01","2021-05-25","2022-05-31", "2020-05-25", "2019-05-25", "2022-05-25", "2010-09-30", "2022-05-25", "2020-02-29", "2021-01-01"
        )
      ),
      cohort_end_date = .data$cohort_start_date
    )
  } else {
    markerCohort <- markerCohort
  }

  checkmate::assert_tibble(indexCohort)
  checkmate::assert_tibble(markerCohort)

  cdm <-
    omock::mockCdmReference() |> omock::mockCdmFromTable(cohortTable = list(cohort_1 = indexCohort,
                                                                            cohort_2 = markerCohort))

  cdm <- CDMConnector::copy_cdm_to(con = con,
                                   cdm = cdm,
                                   schema = schema)

  return(cdm)

}
