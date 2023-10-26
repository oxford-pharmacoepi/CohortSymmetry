connectionDetails<- list(
  con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
  writeSchema = "main",
  mockPrefix = NULL
)


test_that("test cohort tables and ids", {
  indexCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 2, 1, 3, 3, 3, 1, 3),
    subject_id = c(1, 4, 2, 3, 5, 5, 4, 3, 3, 1),
    cohort_start_date = as.Date(c(
      "2020-01-01", "2020-06-01", "2019-12-01", "2020-01-01", "2020-08-01", "2020-04-07", "2020-01-01", "2018-02-02", "2019-09-09", "2019-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-04-01", "2020-08-01", "2020-02-02", "2020-03-01", "2022-04-01", "2023-07-04", "2022-02-02", "2023-12-03", "2019-11-01", "2019-01-01"
    ))
  )
  markerCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 2),
    subject_id = c(1, 3, 4, 2, 5, 1, 2, 3, 4, 5, 1),
    cohort_start_date = as.Date(
      c(
        "2019-12-30", "2020-01-01","2020-05-25","2020-01-01", "2020-05-25", "2019-05-25", "2013-05-25", "2020-09-30", "2022-05-25", "2020-02-29", "2019-01-01"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2019-12-30","2020-01-01","2020-05-25","2020-01-01","2020-05-25", "2019-12-30", "2020-01-01","2021-05-25", "2023-05-25", "2023-05-25", "2019-01-01"
      )
    )
  )
  attr(indexCohort, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 3),
    cohort_name = c("amiodarone", "antipsychotics", "CCB")
  )

  attr(markerCohort, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 3),
    cohort_name = c("levothyroxine", "anti-parkinson drugs", "diuretics")
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
  expect_true(all(
    c("subjectId", "indexId", "markerId", "indexDate", "markerDate",
      "firstDate", "timeGap", "cdm_name") %in%
     colnames(cdm$joined_cohorts)
  ))

  # check number of rows (timeGap=365d)
  expect_true(nrow(cdm$joined_cohorts) == 2)
  expect_true(all(abs(cdm$joined_cohorts$markerDate - cdm$joined_cohorts$indexDate) <= 365))

  # check first Date
  expect_true(all(cdm$joined_cohorts$firstDate == pmin(cdm$joined_cohorts$indexDate, cdm$joined_cohorts$markerDate)))


  # Single id (indexId=markerId=2)
   cdm <- joinCohorts(cdm,
   indexTable ="cohort1",
   indexId=2,
   markerTable = "cohort2",
   markerId=2)

  # check number of rows (timeGap=365d)
  expect_true(all(abs(cdm$joined_cohorts$markerDate - cdm$joined_cohorts$indexDate) <= 365))
  expect_true((nrow(cdm$joined_cohorts %>% dplyr::filter(indexId==2 & markerId==2)) == 1))
  # we should have one row for subject id 2

  # Single id (indexId=1, markerId=2)
  cdm <- joinCohorts(cdm,
                     indexTable ="cohort1",
                    indexId=1,
                     markerTable = "cohort2",
                    markerId=2)

   # check number of rows (timeGap=365d)
  expect_true(all(abs(cdm$joined_cohorts$markerDate - cdm$joined_cohorts$indexDate) <= 365))
  expect_true((nrow(cdm$joined_cohorts %>% dplyr::filter(indexId==1 & markerId==2)) == 3))

  # change of defaulted timeGap
  cdm <- joinCohorts(cdm,
                     indexTable ="cohort1",
                     indexId=1,
                     markerTable = "cohort2",
                     markerId=2,
                     timeGap = 30)

  expect_true(all(abs(cdm$joined_cohorts$markerDate - cdm$joined_cohorts$indexDate) <= 30))
  expect_true((nrow(cdm$joined_cohorts %>% dplyr::filter(indexId==1 & markerId==2)) == 1))

  # Multiple id
  cdm <- joinCohorts(cdm,
                     indexTable ="cohort1",
                     markerTable = "cohort2",
                     timeGap =90)


  # check number of rows (timeGap=90d)
  expect_true(all(abs(cdm$joined_cohorts$markerDate - cdm$joined_cohorts$indexDate) <= 90))
  expect_true((nrow(cdm$joined_cohorts %>% dplyr::filter(indexId==1 & markerId==1)) == 1) &
                          (nrow(cdm$joined_cohorts %>% dplyr::filter(indexId==2 & markerId==2)) == 1))
  # check the total number of combinations, should be 5
  expect_true(cdm$joined_cohorts %>% dplyr::select(indexId, markerId) %>% dplyr::distinct() %>% dplyr::tally() == 5)

  # same start date exclusion
  expect_false(1 %in% (cdm$joined_cohorts %>% dplyr::filter(indexId==3, markerId==2) %>% dplyr::pull(subjectId)))

  # 1 Id vs all Ids
  cdm <- joinCohorts(cdm,
                     indexTable ="cohort1",
                     indexId = 1,
                     markerTable = "cohort2"
                     )

  expect_true((nrow(cdm$joined_cohorts %>% dplyr::filter(indexId==1 & markerId==1)) == 2) &
                (nrow(cdm$joined_cohorts %>% dplyr::filter(indexId==1 & markerId==2)) == 3) &
                (nrow(cdm$joined_cohorts %>% dplyr::filter(indexId==1 & markerId==3)) == 2))

  expect_false(2 %in% (cdm$joined_cohorts %>% dplyr::pull(indexId)))
  expect_false(3 %in% (cdm$joined_cohorts %>% dplyr::pull(indexId)))

  cdm <- joinCohorts(cdm,
                     indexTable ="cohort1",
                     markerTable = "cohort2",
                     markerId = 1
  )

  expect_true((nrow(cdm$joined_cohorts %>% dplyr::filter(indexId==1 & markerId==1)) == 2) &
                (nrow(cdm$joined_cohorts %>% dplyr::filter(indexId==3 & markerId==1)) == 1))

  expect_false(2 %in% (cdm$joined_cohorts %>% dplyr::pull(markerId)))
  expect_false(3 %in% (cdm$joined_cohorts %>% dplyr::pull(markerId)))

  # subset Ids vs subset Ids
  cdm <- joinCohorts(cdm,
                     indexTable ="cohort1",
                     indexId = c(1,2),
                     markerTable = "cohort2",
                     markerId = c(2,3)
  )

  expect_true((nrow(cdm$joined_cohorts %>% dplyr::filter(indexId==2 & markerId==3)) == 1) &
                (nrow(cdm$joined_cohorts %>% dplyr::filter(indexId==2 & markerId==2)) == 1) &
                (nrow(cdm$joined_cohorts %>% dplyr::filter(indexId==1 & markerId==2)) == 3) &
                (nrow(cdm$joined_cohorts %>% dplyr::filter(indexId==1 & markerId==3)) == 2))

  expect_false(3 %in% (cdm$joined_cohorts %>% dplyr::pull(indexId)))
  expect_false(1 %in% (cdm$joined_cohorts %>% dplyr::pull(markerId)))

  # timeGap finiteness
  cdm <- joinCohorts(cdm,
                     indexTable ="cohort1",
                     indexId = 1,
                     markerTable = "cohort2",
                     markerId = 3
  )
  expect_true(nrow(cdm$joined_cohorts) == 2) # default time, 2 entries

  cdm <- joinCohorts(cdm,
                     indexTable ="cohort1",
                     indexId = 1,
                     markerTable = "cohort2",
                     markerId = 3,
                     timeGap = Inf
  )
  expect_true(nrow(cdm$joined_cohorts) == 4)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
