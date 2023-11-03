connectionDetails<- list(
  con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
  writeSchema = "main",
  mockPrefix = NULL
)

indexCohort <- dplyr::tibble(
  cohort_definition_id = c(1, 1, 2, 2, 1, 3, 3, 3, 1, 3),
  subject_id = c(1, 4, 2, 3, 5, 5, 4, 3, 3, 1),
  cohort_start_date = as.Date(
    c(
      "2020-01-01", "2020-06-01", "2019-12-01", "2020-01-01", "2020-08-01", "2020-04-07", "2020-01-01", "2018-02-02", "2019-09-09", "2019-01-01"
    )
  ),
  cohort_end_date = as.Date(
    c(
      "2020-04-01", "2020-08-01", "2020-02-02", "2020-03-01", "2022-04-01", "2023-07-04", "2022-02-02", "2023-12-03", "2019-11-01", "2019-01-01"
    )
  )
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

# check colnames
test_that("mock db: check output format", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                     indexTable ="cohort1",
                     markerTable = "cohort2")
  expect_true(all(
    c("subject_id", "index_id", "marker_id", "index_date", "marker_date",
      "first_date", "time_gap", "cdm_name") %in%
      colnames(cdm$joined_cohorts)
  ))
}
)

#check one ID against one ID
test_that("mock db: one ID against one ID, example 1", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                     indexTable ="cohort1",
                     indexId=1,
                     markerTable = "cohort2",
                     markerId=1)
  # check number of rows (timeGap=365d)
  expect_true((cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n)) == 2)
  expect_true(all(abs(cdm$joined_cohorts$marker_date - cdm$joined_cohorts$index_date) <= 365))

  # check first Date
  expect_true(all(cdm$joined_cohorts$first_date == pmin(cdm$joined_cohorts$index_date, cdm$joined_cohorts$marker_date)))
}
)

test_that("mock db: one ID against one ID, example 2", {
  # Single id (indexId=markerId=2)
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                     indexTable ="cohort1",
                     indexId=2,
                     markerTable = "cohort2",
                     markerId=2)
  # check number of rows (timeGap=365d)
  expect_true(all(abs(cdm$joined_cohorts$marker_date - cdm$joined_cohorts$index_date) <= 365))
  expect_true((cdm$joined_cohorts %>% dplyr::filter(index_id==2 & marker_id==2) %>% dplyr::tally() %>% dplyr::pull(n)) == 1)
  # we should have one row for subject id 2
}
)

test_that("mock db: one ID against one ID, example 3", {
  # Single id (indexId=1, markerId=2)
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                     indexTable ="cohort1",
                     indexId=1,
                     markerTable = "cohort2",
                     markerId=2)

  # check number of rows (timeGap=365d)
  expect_true(all(abs(cdm$joined_cohorts$marker_date - cdm$joined_cohorts$index_date) <= 365))
  expect_true((cdm$joined_cohorts %>% dplyr::filter(index_id==1 & marker_id==2) %>% dplyr::tally() %>% dplyr::pull(n))== 3)
}
)

test_that("mock db: change timeGap", {
  # change of defaulted timeGap
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                     indexTable ="cohort1",
                     indexId=1,
                     markerTable = "cohort2",
                     markerId=2,
                     timeGap = 30)

  expect_true(all(abs(cdm$joined_cohorts$marker_date - cdm$joined_cohorts$index_date) <= 30))
  expect_true((cdm$joined_cohorts %>% dplyr::filter(index_id==1 & marker_id==2) %>% dplyr:: tally() %>% dplyr:: pull(n)) == 1)
}
)

test_that("mock db: all IDs against all IDs", {
  # Multiple id
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                     indexTable ="cohort1",
                     markerTable = "cohort2",
                     timeGap =90)


  # check number of rows (timeGap=90d)
  expect_true(all(abs(cdm$joined_cohorts$marker_date - cdm$joined_cohorts$index_date) <= 90))
  expect_true((cdm$joined_cohorts %>% dplyr::filter(index_id==1 & marker_id==1) %>% dplyr::tally() %>% dplyr::pull(n)) == 1 &
                (cdm$joined_cohorts %>% dplyr::filter(index_id==2 & marker_id==2) %>% dplyr::tally() %>% dplyr::pull(n)) == 1)
  # check the total number of combinations, should be 5
  expect_true(cdm$joined_cohorts %>% dplyr::select(index_id, marker_id) %>% dplyr::distinct() %>% dplyr::tally() %>% dplyr::pull(n) == 5)

  # same start date exclusion
  expect_false(1 %in% (cdm$joined_cohorts %>% dplyr::filter(index_id==3, marker_id==2) %>% dplyr::pull(subject_id)))
}
)

test_that("mock db: one index (rsp. marker) ID against all marker (rsp. index) IDs", {
  # 1 Id vs all Ids
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                     indexTable ="cohort1",
                     indexId = 1,
                     markerTable = "cohort2"
  )

  expect_true((cdm$joined_cohorts %>% dplyr::filter(index_id==1 & marker_id==1) %>% dplyr::tally() %>% dplyr::pull(n) == 2) &
                (cdm$joined_cohorts %>% dplyr::filter(index_id==1 & marker_id==2) %>% dplyr::tally() %>% dplyr::pull(n) == 3) &
                (cdm$joined_cohorts %>% dplyr::filter(index_id==1 & marker_id==3) %>% dplyr::tally() %>% dplyr::pull(n) == 2))

  expect_false(2 %in% (cdm$joined_cohorts %>% dplyr::pull(index_id)))
  expect_false(3 %in% (cdm$joined_cohorts %>% dplyr::pull(index_id)))

  cdm <- CohortSymmetry::getCohortSequence(cdm,
                     indexTable ="cohort1",
                     markerTable = "cohort2",
                     markerId = 1
  )

  expect_true((cdm$joined_cohorts %>% dplyr::filter(index_id==1 & marker_id==1) %>% dplyr::tally() %>% dplyr::pull(n) == 2) &
                (cdm$joined_cohorts %>% dplyr::filter(index_id==3 & marker_id==1) %>% dplyr::tally() %>% dplyr::pull(n) == 1))

  expect_false(2 %in% (cdm$joined_cohorts %>% dplyr::pull(marker_id)))
  expect_false(3 %in% (cdm$joined_cohorts %>% dplyr::pull(marker_id)))

}
)

test_that("mock db: a subset of IDs against a subset of IDs", {
  # subset Ids vs subset Ids
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                     indexTable ="cohort1",
                     indexId = c(1,2),
                     markerTable = "cohort2",
                     markerId = c(2,3)
  )

  expect_true((cdm$joined_cohorts %>% dplyr::filter(index_id==2 & marker_id==3) %>% dplyr::tally() %>% dplyr::pull(n) == 1) &
                (cdm$joined_cohorts %>% dplyr::filter(index_id==2 & marker_id==2) %>% dplyr::tally() %>% dplyr::pull(n) == 1) &
                (cdm$joined_cohorts %>% dplyr::filter(index_id==1 & marker_id==2) %>% dplyr::tally() %>% dplyr::pull(n) == 3) &
                (cdm$joined_cohorts %>% dplyr::filter(index_id==1 & marker_id==3) %>% dplyr::tally() %>% dplyr::pull(n) == 2))

  expect_false(3 %in% (cdm$joined_cohorts %>% dplyr::pull(index_id)))
  expect_false(1 %in% (cdm$joined_cohorts %>% dplyr::pull(marker_id)))
}
)

test_that("mock db: example of timeGap being infinite", {
  # timeGap finiteness
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                     indexTable ="cohort1",
                     indexId = 1,
                     markerTable = "cohort2",
                     markerId = 3
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 2) # default time, 2 entries

  cdm <- CohortSymmetry::getCohortSequence(cdm,
                     indexTable ="cohort1",
                     indexId = 1,
                     markerTable = "cohort2",
                     markerId = 3,
                     timeGap = Inf
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 4)
})

DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
