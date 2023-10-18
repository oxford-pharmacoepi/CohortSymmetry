connectionDetails<- list(
  con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
  writeSchema = "main",
  mockPrefix = NULL
)


test_that("test cohort tables and ids", {
  library(CohortSymmetry)
  indexCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 2, 1),
    subject_id = c(1, 4, 2, 3, 5),
    cohort_start_date = as.Date(c(
      "2020-01-01", "2020-06-01", "2019-12-01", "2020-01-01", "2020-08-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-04-01", "2020-08-01", "2020-02-02", "2020-03-01", "2022-04-01"
    ))
  )
  markerCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 2, 2),
    subject_id = c(1, 3, 4, 2, 5),
    cohort_start_date = as.Date(
      c(
        "2019-12-30", "2020-01-01","2020-05-25","2020-01-01", "2020-05-25"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2019-12-30","2020-01-01","2020-05-25","2020-01-01","2020-05-25"
      )
    )
  )
  attr(indexCohort, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1, 2),
    cohort_name = c("amiodarone", "antipsychotics")
  )

  attr(markerCohort, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1, 2),
    cohort_name = c("levothyroxine", "anti-parkinson drugs")
  )
  cdm <-
    DrugUtilisation::mockDrugUtilisation(
      connectionDetails,
      cohort1 = indexCohort,
      cohort2 = markerCohort
    )
  # Single id (indexId=markerId=1)
  cdm <- joinCohorts(cdm,
                     indexTable ="cohort1",
                     indexId=1,
                     markerTable = "cohort2",
                     markerId=1)

  # check colnames
  testthat::expect_true(all(
    c("subjectId", "indexId", "markerId", "indexDate", "markerDate",
      "firstDate", "timeGap", "cdm_name") %in%
     colnames(cdm$joined_cohorts)
  ))

  # check number of rows (timeGap=365d)
  testthat::expect_true(nrow(cdm$joined_cohorts) == 1)
  testthat::expect_true(all(abs(cdm$joined_cohorts$markerDate- cdm$joined_cohorts$indexDate) <= 365))

  # check first Date
  testthat::expect_true(cdm$joined_cohorts$firstDate == min(cdm$joined_cohorts$markerDate, cdm$joined_cohorts$markerDate))


  # Single id (indexId=markerId=2)---- not working????
  #  cdm <- joinCohorts(cdm,
  #  indexTable ="cohort1",
  #  indexId=2,
  #  markerTable = "cohort2",
  #     markerId=2)

  # check number of rows (timeGap=365d)
  # testthat::expect_true(all(abs(cdm$joined_cohorts$markerDate- cdm$joined_cohorts$indexDate) <= 365))
  # testthat::expect_true((nrow(cdm$joined_cohorts %>% dplyr::filter(indexId==2 & markerId==2)) == 1))
  # we should have one row for subject id 2

  # Single id (indexId=markerId=2)---- not working????
  # cdm <- joinCohorts(cdm,
  #                    indexTable ="cohort1",
  #                   indexId=1,
  #                    markerTable = "cohort2",
  #                   markerId=2)

   # check number of rows (timeGap=365d)
  # testthat::expect_true(all(abs(cdm$joined_cohorts$markerDate- cdm$joined_cohorts$indexDate) <= 365))
  # testthat::expect_true((nrow(cdm$joined_cohorts %>% dplyr::filter(indexId==1 & markerId==2)) == 1))
   # we should have one row for subject id 4

  # Multiple id
  cdm <- joinCohorts(cdm,
                     indexTable ="cohort1",
                     indexId=NULL,# we need to change this so users don't need to specify NULL
                     markerTable = "cohort2",
                     markerId=NULL, #we need to change this so users don't need to specify NULL
                     timeGap =90)


  # check number of rows (timeGap=90d)
  testthat::expect_true(all(abs(cdm$joined_cohorts$markerDate- cdm$joined_cohorts$indexDate) <= 90))
  testthat::expect_true((nrow(cdm$joined_cohorts %>% dplyr::filter(indexId==1 & markerId==1)) == 1) &
                          (nrow(cdm$joined_cohorts %>% dplyr::filter(indexId==2 & markerId==2)) == 1))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
