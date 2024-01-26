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
      "2020-04-01", "2021-06-01", "2022-05-22", "2010-01-01", "2019-08-01", "2019-04-07", "2021-01-01", "1998-02-02", "1999-09-09", "2021-01-01"
    )
  ),
  cohort_end_date = as.Date(
    c(
      "2020-04-01", "2021-08-01", "2022-05-23", "2010-03-01", "2020-04-01", "2020-05-30", "2022-02-02", "2003-12-03", "1999-11-01", "2021-01-01"
    )
  )
)

markerCohort <- dplyr::tibble(
  cohort_definition_id = c(1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 2),
  subject_id = c(1, 3, 4, 2, 5, 1, 2, 3, 4, 5, 1),
  cohort_start_date = as.Date(
    c(
      "2020-12-30", "2010-01-01","2021-05-25","2022-05-31", "2020-05-25", "2019-05-25", "2022-05-25", "2000-09-30", "2022-05-25", "2020-02-29", "2021-01-01"
    )
  ),
  cohort_end_date = as.Date(
    c(
      "2020-12-30","2010-01-01","2021-05-25","2022-06-13","2020-05-25", "2019-12-30", "2022-06-31", "2001-05-25", "2022-05-25", "2020-05-25", "2021-01-01"
    )
  )
)

cdm <-
  DrugUtilisation::mockDrugUtilisation(
    connectionDetails,
    cohort1 = indexCohort,
    cohort2 = markerCohort
  )

# check output table name
test_that("mock db: check output table name", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           name = "output",
                                           indexTable ="cohort1",
                                           markerTable = "cohort2")
  expect_null(cdm$joined_cohorts)
  expect_true(all(
    c("subject_id", "index_id", "marker_id", "index_date", "marker_date",
      "first_date", "cdm_name") %in%
      colnames(cdm$output)
  ))
}
)

# check colnames
test_that("mock db: check output format", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                     indexTable ="cohort1",
                     markerTable = "cohort2")
  expect_true(all(
    c("subject_id", "index_id", "marker_id", "index_date", "marker_date",
      "first_date", "cdm_name") %in%
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
  expect_true((cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n)) == 1)
  loc <- cdm$joined_cohorts %>% dplyr::collect()
  expect_true(all(abs(loc$marker_date - loc$index_date) <= 365))

  # check first Date
  expect_true(all(loc$first_date == pmin(loc$index_date, loc$marker_date)))
  expect_true(all(loc$first_date < loc$second_date))
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
  loc <- cdm$joined_cohorts %>% dplyr::collect()
  expect_true(all(abs(loc$marker_date - loc$index_date) <= 365))
  expect_true((cdm$joined_cohorts %>% dplyr::filter(index_id==2 & marker_id==2) %>% dplyr::tally() %>% dplyr::pull(n)) == 1)
  expect_false((cdm$joined_cohorts %>% dplyr::filter(index_id==1 & marker_id==2) %>% dplyr::tally() %>% dplyr::pull(n)) > 0)
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
  loc <- cdm$joined_cohorts %>% dplyr::collect()
  expect_true(all(abs(loc$marker_date - loc$index_date) <= 365))
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

  loc <- cdm$joined_cohorts %>% dplyr::collect()
  expect_true(all(abs(loc$marker_date - loc$index_date) <= 30))
  expect_true((cdm$joined_cohorts %>% dplyr::filter(index_id==1 & marker_id==2) %>% dplyr:: tally() %>% dplyr:: pull(n)) == 1)
}
)

test_that("mock db: change blackOutPeriod", {
  # change of defaulted timeGap
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           indexId=1,
                                           markerTable = "cohort2",
                                           markerId=2,
                                           blackOutPeriod = 7,
                                           timeGap = 365)

  loc <- cdm$joined_cohorts %>% dplyr::collect()
  expect_true(all(abs(loc$marker_date - loc$index_date) <= 365))
  expect_true((cdm$joined_cohorts %>% dplyr::filter(index_id==1 & marker_id==2) %>% dplyr:: tally() %>% dplyr:: pull(n)) == 2)
  expect_true(all(loc$second_date-loc$first_date>7 & loc$second_date-loc$first_date<365))
}
)

test_that("mock db: all IDs against all IDs", {
  # Multiple id
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                     indexTable = "cohort1",
                     markerTable = "cohort2",
                     timeGap = 90)


  # check number of rows (timeGap=90d)
  loc <- cdm$joined_cohorts %>% dplyr::collect()
  expect_true(all(abs(loc$marker_date - loc$index_date) <= 90))
  expect_true((cdm$joined_cohorts %>% dplyr::filter(index_id==1 & marker_id==2) %>% dplyr::tally() %>% dplyr::pull(n)) == 1 &
                (cdm$joined_cohorts %>% dplyr::filter(index_id==2 & marker_id==2) %>% dplyr::tally() %>% dplyr::pull(n)) == 1)
  # check the total number of combinations, should be 5
  expect_true(cdm$joined_cohorts %>% dplyr::select(index_id, marker_id) %>% dplyr::distinct() %>% dplyr::tally() %>% dplyr::pull(n) == 4)

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

  expect_true((cdm$joined_cohorts %>% dplyr::filter(index_id==1 & marker_id==1) %>% dplyr::tally() %>% dplyr::pull(n) == 1) &
                (cdm$joined_cohorts %>% dplyr::filter(index_id==1 & marker_id==2) %>% dplyr::tally() %>% dplyr::pull(n) == 3) &
                (cdm$joined_cohorts %>% dplyr::filter(index_id==1 & marker_id==3) %>% dplyr::tally() %>% dplyr::pull(n) == 2))

  expect_false(2 %in% (cdm$joined_cohorts %>% dplyr::pull(index_id)))
  expect_false(3 %in% (cdm$joined_cohorts %>% dplyr::pull(index_id)))

  cdm <- CohortSymmetry::getCohortSequence(cdm,
                     indexTable ="cohort1",
                     markerTable = "cohort2",
                     markerId = 1
  )

  expect_true((cdm$joined_cohorts %>% dplyr::filter(index_id==1 & marker_id==1) %>% dplyr::tally() %>% dplyr::pull(n) == 1) &
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
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 3)
})

################## Involving dateRange and priorObservation ###################
# priorObservation
indexCohort <- dplyr::tibble(
  cohort_definition_id = c(1, 1, 1, 1),
  subject_id = c(1, 3, 4, 10),
  cohort_start_date = as.Date(
    c(
      "2020-04-07", "2010-08-27", "2022-01-01", "2000-01-01"
    )
  ),
  cohort_end_date = as.Date(
    c(
      "2020-04-08", "2010-08-27", "2022-01-01", "2000-01-02"
    )
  )
)

markerCohort <- dplyr::tibble(
  cohort_definition_id = c(1, 1, 1, 1),
  subject_id = c(1, 3, 4, 10),
  cohort_start_date = as.Date(
    c(
      "2021-04-25", "2010-08-26","2022-01-02", "2006-03-01"
    )
  ),
  cohort_end_date = as.Date(
    c(
      "2021-04-25","2010-08-27","2022-05-25", "2006-03-14"
    )
  )
)

cdm <-
  DrugUtilisation::mockDrugUtilisation(
    connectionDetails,
    cohort1 = indexCohort,
    cohort2 = markerCohort
  )

test_that("mock db: example of timeGap being infinite with 0 daysPriorObservation", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           daysPriorObservation = 0,
                                           timeGap = Inf
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 4)
})

test_that("mock db: example of fixed timeGap being with 0 daysPriorObservation", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           daysPriorObservation = 0,
                                           timeGap = 365
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 2)
})

test_that("mock db: example of infinite timeGap being with non-zero daysPriorObservation", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           daysPriorObservation = 30,
                                           timeGap = Inf
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 3)
})

test_that("mock db: example of fixed timeGap being with non-zero daysPriorObservation", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           daysPriorObservation = 30,
                                           timeGap = 365
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 2)
})

#dateRange
test_that("mock db: example of given study period start date", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           dateRange = as.Date(c("2020-01-01", NA)),
                                           daysPriorObservation = 0,
                                           timeGap = Inf
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 2)
  expect_true(all(cdm$joined_cohorts %>% dplyr::pull(subject_id) %in% c(1,4)))
})

test_that("mock db: example of given study period start date wih daysPriorObservation and timeGap", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           dateRange = as.Date(c("2020-01-01", NA)),
                                           daysPriorObservation = 365,
                                           timeGap = 365
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 1)
  expect_true(all(cdm$joined_cohorts %>% dplyr::pull(subject_id) %in% c(4)))
})

test_that("mock db: example of given study period start date wih daysPriorObservation", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           dateRange = as.Date(c("2000-01-01", "2020-01-01")),
                                           daysPriorObservation = 365,
                                           timeGap = Inf
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 2)
  expect_true(all(cdm$joined_cohorts %>% dplyr::pull(subject_id) %in% c(3, 10)))
})

test_that("mock db: example of given study period start date wih daysPriorObservation", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           dateRange = as.Date(c("2000-01-01", "2023-01-01")),
                                           daysPriorObservation = 365,
                                           timeGap = Inf
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 3)
  expect_true(all(cdm$joined_cohorts %>% dplyr::pull(subject_id) %in% c(3, 4, 10)))
})

################################# Involving washouts ################################
# indexWashout
indexCohort <- dplyr::tibble(
  cohort_definition_id = c(1, 1, 1, 1),
  subject_id = c(3, 3, 3, 3),
  cohort_start_date = as.Date(
    c(
      "2010-04-07", "2010-08-27", "1984-01-01", "2000-01-01"
    )
  ),
  cohort_end_date = as.Date(
    c(
      "2010-04-08", "2010-08-27", "1984-01-01", "2000-01-02"
    )
  )
)

markerCohort <- dplyr::tibble(
  cohort_definition_id = c(1, 1, 1, 1),
  subject_id = c(3, 3, 3, 3),
  cohort_start_date = as.Date(
    c(
      "2001-04-25", "2010-08-26","2002-01-02", "2006-03-01"
    )
  ),
  cohort_end_date = as.Date(
    c(
      "2001-04-25","2010-08-27","2002-05-25", "2006-03-14"
    )
  )
)

cdm <-
  DrugUtilisation::mockDrugUtilisation(
    connectionDetails,
    cohort1 = indexCohort,
    cohort2 = markerCohort
  )

test_that("mock db: example of multiple entries per person", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           timeGap = Inf
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 1)
  earliest_index_date <- cdm$cohort1 %>%
    dbplyr::window_order(cohort_start_date) %>%
    dplyr::filter(dplyr::row_number()==1) %>%
    dplyr::pull(cohort_start_date)
  earliest_marker_date <- cdm$cohort2 %>%
    dbplyr::window_order(cohort_start_date) %>%
    dplyr::filter(dplyr::row_number()==1) %>%
    dplyr::pull(cohort_start_date)
  expect_true(all(cdm$joined_cohorts %>% dplyr::pull(index_date) == earliest_index_date,
                  cdm$joined_cohorts %>% dplyr::pull(marker_date) == earliest_marker_date))
})

test_that("mock db: example of multiple entries per person - exclusion based on timeGap", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           timeGap = 365
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 0)
})

test_that("mock db: example of multiple entries per person - exclusion based on prior observation", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           daysPriorObservation = 365,
                                           timeGap = Inf
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 0)
})

test_that("mock db: example of multiple entries per person - picking sequence in relation to dateRange", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           dateRange = as.Date(c("2000-01-01", NA)),
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           daysPriorObservation = 365,
                                           timeGap = Inf
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 1)
  expect_true(cdm$joined_cohorts %>% dplyr::pull(first_date) >= as.Date("2000-01-01"))
})

test_that("mock db: example of multiple entries per person - picking sequence in relation to dateRange with fixed timeGap", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           dateRange = as.Date(c("2000-01-01", NA)),
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           daysPriorObservation = 365,
                                           timeGap = 365
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 0)
})

test_that("mock db: example of multiple entries per person - index washout (shouldn't affect the first event)", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           indexWashout = 365,
                                           timeGap = Inf
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 1)
  index_date <- cdm$joined_cohorts %>% dplyr::pull(index_date)
  test_index <- cdm$cohort1 %>% dplyr::filter(cohort_start_date<=as.Date(index_date)) %>% dplyr::collect() %>% dplyr::filter(cohort_start_date+365 >= as.Date(index_date)) %>% dplyr::collect()
  expect_true(test_index %>% dplyr::tally() %>% dplyr::pull(n) == 1)
})

test_that("mock db: example of multiple entries per person - exclusion based on marker washout", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           dateRange = as.Date(c("2002-01-01", NA)),
                                           markerWashout = 365,
                                           timeGap = Inf
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 0)
})

test_that("mock db: example of multiple entries per person - all parameters are altered", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           name = "test_1",
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           dateRange = as.Date(c("2010-01-01", "2020-01-01")),
                                           indexWashout = 365,
                                           markerWashout = 365,
                                           timeGap = 365
  )
  expect_null(cdm$joined_cohorts)
  expect_true(cdm$test_1 %>% dplyr::tally() %>% dplyr::pull(n) == 1)
  expect_true(cdm$test_1 %>% dplyr::pull(first_date) >= as.Date("2010-01-01"))
  expect_true(cdm$test_1 %>% dplyr::pull(first_date) <= as.Date("2020-01-01"))
  loc <- cdm$test_1 %>% dplyr::collect()
  expect_true(abs(loc$marker_date - loc$index_date) <= 365)
  marker_date <- cdm$test_1 %>% dplyr::pull(marker_date)
  index_date <- cdm$test_1 %>% dplyr::pull(index_date)
  test_index <- cdm$cohort1 %>% dplyr::filter(cohort_start_date<=as.Date(index_date)) %>% dplyr::collect() %>% dplyr::filter(cohort_start_date+365 >= as.Date(index_date)) %>% dplyr::collect()
  expect_true(test_index %>% dplyr::tally() %>% dplyr::pull(n) == 1)
  test_marker <- cdm$cohort2 %>% dplyr::filter(cohort_start_date<=as.Date(marker_date)) %>% dplyr::collect() %>% dplyr::filter(cohort_start_date+365 >= as.Date(marker_date)) %>% dplyr::collect()
  expect_true(test_marker %>% dplyr::tally() %>% dplyr::pull(n) == 1)
})

# outside of observation period
indexCohort <- dplyr::tibble(
  cohort_definition_id = c(1, 2),
  subject_id = c(1, 2),
  cohort_start_date = as.Date(
    c(
      "2019-09-01", "2020-06-22"
    )
  ),
  cohort_end_date = as.Date(
    c(
      "2019-09-08", "2020-06-23"
    )
  )
)

markerCohort <- dplyr::tibble(
  cohort_definition_id = c(1, 2),
  subject_id = c(1, 2),
  cohort_start_date = as.Date(
    c(
      "2020-06-22", "2021-06-01"
    )
  ),
  cohort_end_date = as.Date(
    c(
      "2020-07-25","2021-08-27"
    )
  )
)

cdm <-
  DrugUtilisation::mockDrugUtilisation(
    connectionDetails,
    cohort1 = indexCohort,
    cohort2 = markerCohort
  )

test_that("mock db: example of excluding based on records outside of observation period", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           indexId = 1,
                                           markerTable = "cohort2",
                                           markerId = 1
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 0)
})

DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
