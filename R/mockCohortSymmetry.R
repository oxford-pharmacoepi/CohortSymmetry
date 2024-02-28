


#' Creates mock cdm object for testing
#'
#' @param indexCohort the tibble of your index cohort
#' @param markerCohort the tibble of your marker cohort
#' @param con connection detail
#' @param schema name of your write schema
#'
#' @return mock cdm object contains your index and marker cohort
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortSymmetry)
#' }
mockCohortSymmetry <- function(indexCohort,
                               markerCohort,
                               con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
                               schema = "main") {
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


