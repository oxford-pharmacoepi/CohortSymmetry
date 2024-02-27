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

markerCohort <- dplyr::tibble(
  cohort_definition_id = c(1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 2),
  subject_id = c(1, 3, 4, 2, 5, 1, 2, 3, 4, 5, 1),
  cohort_start_date = as.Date(
    c(
      "2020-12-30", "2010-01-01","2021-05-25","2022-05-31", "2020-05-25", "2019-05-25", "2022-05-25", "2010-09-30", "2022-05-25", "2020-02-29", "2021-01-01"
    )
  ),
  cohort_end_date = cohort_start_date
)

cdm <-
  PatientProfiles::mockPatientProfiles(
    cohort1 = indexCohort,
    cohort2 = markerCohort,
    patient_size = 10
  )

# check output table name
test_that("mock db: check output table name", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           name = "output",
                                           indexTable ="cohort1",
                                           markerTable = "cohort2")
  expect_null(cdm$joined_cohorts)
  expect_true(all(
    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "marker_date",
      "index_date") %in%
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
    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "marker_date",
      "index_date") %in%
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
  expect_true((cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n)) == 2)
  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(CDMConnector::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()
  expect_true(all(abs(loc$marker_date - loc$index_date) <= 365))

  # check first Date
  expect_true(all(loc$cohort_start_date == pmin(loc$index_date, loc$marker_date)))
  expect_true(all(loc$cohort_start_date < loc$cohort_end_date))
}
)

test_that("mock db: one ID against one ID, example 2", {
  # Single id (indexId=markerId=2)
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                     indexTable ="cohort1",
                     indexId=2,
                     markerTable = "cohort2",
                     markerId=2)
  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(CDMConnector::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()

  expect_true(all(abs(loc$marker_date - loc$index_date) <= 365))
  expect_true((loc %>% dplyr::filter(index_name=="cohort_2" & marker_name=="cohort_2") %>% dplyr::tally() %>% dplyr::pull(n)) == 1)
  expect_false((loc %>% dplyr::filter(index_name=="cohort_1" & marker_name=="cohort_2") %>% dplyr::tally() %>% dplyr::pull(n)) > 0)
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

  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(CDMConnector::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()
  expect_true(all(abs(loc$marker_date - loc$index_date) <= 365))
  expect_true((loc %>% dplyr::filter(index_name=="cohort_1" & marker_name=="cohort_2") %>% dplyr::tally() %>% dplyr::pull(n))== 3)
}
)

test_that("mock db: change combinationWindow ", {
  # change of defaulted combinationWindow
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                     indexTable ="cohort1",
                     indexId=1,
                     markerTable = "cohort2",
                     markerId=2,
                     indexMarkerGap = 30,
                     combinationWindow = c(0,30))

  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(CDMConnector::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()
  expect_true(all(abs(loc$marker_date - loc$index_date) <= 30))
  expect_true((loc %>% dplyr::filter(index_name=="cohort_1" & marker_name=="cohort_2") %>% dplyr:: tally() %>% dplyr:: pull(n)) == 1)
}
)

test_that("mock db: change combinationWindow[1]", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           indexId=1,
                                           markerTable = "cohort2",
                                           markerId=2,
                                           combinationWindow = c(7,365))

  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(CDMConnector::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()
  expect_true(all(abs(loc$marker_date - loc$index_date) <= 365))
  expect_true((loc %>% dplyr::filter(index_name=="cohort_1" & marker_name=="cohort_2") %>% dplyr:: tally() %>% dplyr:: pull(n)) == 2)
  expect_true(all(loc$cohort_end_date-loc$cohort_start_date>7 & loc$cohort_end_date-loc$cohort_start_date<365))
}
)

test_that("mock db: all IDs against all IDs", {
  # Multiple id
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                     indexTable = "cohort1",
                     markerTable = "cohort2",
                     combinationWindow = c(0,90))


  # check number of rows (timeGap=90d)
  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(CDMConnector::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()
  expect_true(all(abs(loc$marker_date - loc$index_date) <= 90))
  expect_true((loc %>% dplyr::filter(index_name=="cohort_1" & marker_name=="cohort_2") %>% dplyr::tally() %>% dplyr::pull(n)) == 1 &
                (loc %>% dplyr::filter(index_name=="cohort_2" & marker_name=="cohort_2") %>% dplyr::tally() %>% dplyr::pull(n)) == 1)
  # check the total number of combinations, should be 5
  expect_true(loc %>% dplyr::select(index_name, marker_name) %>% dplyr::distinct() %>% dplyr::tally() %>% dplyr::pull(n) == 4)

  # same start date exclusion
  expect_false(1 %in% (loc %>% dplyr::filter(index_name=="cohort_3", marker_name=="cohort_2") %>% dplyr::pull(subject_id)))
}
)

test_that("mock db: one index (rsp. marker) ID against all marker (rsp. index) IDs", {
  # 1 Id vs all Ids
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                     indexTable ="cohort1",
                     indexId = 1,
                     markerTable = "cohort2"
  )

  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(CDMConnector::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()

  expect_true((loc %>% dplyr::filter(index_name=="cohort_1" & marker_name=="cohort_1") %>% dplyr::tally() %>% dplyr::pull(n) == 2) &
                (loc %>% dplyr::filter(index_name=="cohort_1" & marker_name=="cohort_2") %>% dplyr::tally() %>% dplyr::pull(n) == 3) &
                (loc %>% dplyr::filter(index_name=="cohort_1" & marker_name=="cohort_3") %>% dplyr::tally() %>% dplyr::pull(n) == 3))

  expect_false("cohort_2" %in% (loc %>% dplyr::pull(index_name)))
  expect_false("cohort_3" %in% (loc %>% dplyr::pull(index_name)))

  cdm <- CohortSymmetry::getCohortSequence(cdm,
                     indexTable ="cohort1",
                     markerTable = "cohort2",
                     markerId = 1
  )

  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(CDMConnector::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()

  expect_true((loc %>% dplyr::filter(index_name=="cohort_1" & marker_name=="cohort_1") %>% dplyr::tally() %>% dplyr::pull(n) == 2) &
                (loc %>% dplyr::filter(index_name=="cohort_3" & marker_name=="cohort_1") %>% dplyr::tally() %>% dplyr::pull(n) == 1))

  expect_false("cohort_2" %in% (loc %>% dplyr::pull(marker_name)))
  expect_false("cohort_3" %in% (loc %>% dplyr::pull(marker_name)))

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

  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(CDMConnector::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()

  expect_true((loc %>% dplyr::filter(index_name=="cohort_2" & marker_name=="cohort_3") %>% dplyr::tally() %>% dplyr::pull(n) == 2) &
                (loc %>% dplyr::filter(index_name=="cohort_2" & marker_name=="cohort_2") %>% dplyr::tally() %>% dplyr::pull(n) == 1) &
                (loc %>% dplyr::filter(index_name=="cohort_1" & marker_name=="cohort_2") %>% dplyr::tally() %>% dplyr::pull(n) == 3) &
                (loc %>% dplyr::filter(index_name=="cohort_1" & marker_name=="cohort_3") %>% dplyr::tally() %>% dplyr::pull(n) == 3))

  expect_false("cohort_3" %in% (loc %>% dplyr::pull(index_name)))
  expect_false("cohort_1" %in% (loc %>% dplyr::pull(marker_name)))
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
  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(CDMConnector::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()
  expect_true(loc %>% dplyr::tally() %>% dplyr::pull(n) == 3) # default time, 3 entries

  cdm <- CohortSymmetry::getCohortSequence(cdm,
                     indexTable ="cohort1",
                     indexId = 1,
                     markerTable = "cohort2",
                     markerId = 3,
                     combinationWindow = c(0,Inf)
  )
  loc <- cdm$joined_cohorts %>%
    dplyr::inner_join(CDMConnector::settings(cdm$joined_cohorts), by = "cohort_definition_id", copy = T) %>%
    dplyr::collect()
  expect_true(loc %>% dplyr::tally() %>% dplyr::pull(n) == 4) #inf gives more values
})

CDMConnector::cdmDisconnect(cdm)
################## Involving dateRange and priorObservation ###################
# priorObservation
indexCohort <- dplyr::tibble(
  cohort_definition_id = c(1, 1, 1, 1),
  subject_id = c(1, 4, 2, 3),
  cohort_start_date = as.Date(
    c(
      "2020-04-01", "2021-06-01", "2022-05-22", "2010-01-01"
    )
  ),
  cohort_end_date = cohort_start_date
)

markerCohort <- dplyr::tibble(
  cohort_definition_id = c(1, 1, 1, 1),
  subject_id = c(1, 3, 4, 2),
  cohort_start_date = as.Date(
    c(
      "2020-12-30", "2010-01-01", "2021-05-25", "2022-05-31"
    )
  ),
  cohort_end_date = cohort_start_date
)

cdm <-
  PatientProfiles::mockPatientProfiles(
    cohort1 = indexCohort,
    cohort2 = markerCohort,
    patient_size = 10
  )

test_that("mock db: example of timeGap being infinite with 0 daysPriorObservation", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           daysPriorObservation = 0,
                                           combinationWindow = c(0,Inf)
  )

  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 3)
})

test_that("mock db: example of fixed timeGap being with 0 daysPriorObservation", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           daysPriorObservation = 0,
                                           combinationWindow = c(0,730)
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 3)
})

test_that("mock db: example of infinite timeGap being with non-zero daysPriorObservation", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           daysPriorObservation = 30,
                                           combinationWindow = c(0,Inf)
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 3)
})

test_that("mock db: example of fixed timeGap being with non-zero daysPriorObservation", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           daysPriorObservation = 30,
                                           combinationWindow = c(0,365)
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 3)
})

#dateRange
test_that("mock db: example of given study period start date", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           dateRange = as.Date(c("2020-01-01", NA)),
                                           daysPriorObservation = 0,
                                           combinationWindow = c(0,Inf)
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 3)
  expect_true(all(cdm$joined_cohorts %>% dplyr::pull(subject_id) %in% c(1,2,4)))
})

test_that("mock db: example of given study period start date with daysPriorObservation and timeGap", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           dateRange = as.Date(c("2020-01-01", NA)),
                                           daysPriorObservation = 365
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 3)
  expect_true(all(cdm$joined_cohorts %>% dplyr::pull(subject_id) %in% c(1,2,4)))
})

test_that("mock db: example of given study period start date wih daysPriorObservation", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           dateRange = as.Date(c("2000-01-01", "2022-01-01")),
                                           daysPriorObservation = 30,
                                           combinationWindow = c(0,Inf)
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 2)
  expect_true(all(cdm$joined_cohorts %>% dplyr::pull(subject_id) %in% c(1,4)))
})

test_that("mock db: example of given study period start date wih daysPriorObservation", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           dateRange = as.Date(c("2000-01-01", "2023-01-01")),
                                           daysPriorObservation = 365,
                                           combinationWindow = c(0,Inf)
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 3)
  expect_true(all(cdm$joined_cohorts %>% dplyr::pull(subject_id) %in% c(1, 2, 4)))
})
CDMConnector::cdmDisconnect(cdm)
################################# Involving washouts ################################
# washoutWindow
indexCohort <- dplyr::tibble(
  cohort_definition_id = c(1, 1, 1, 1),
  subject_id = c(3, 3, 3, 3),
  cohort_start_date = as.Date(
    c(
      "2020-04-07", "2020-08-27", "2014-01-01", "2020-01-01"
    )
  ),
  cohort_end_date = as.Date(
    c(
      "2020-04-08", "2020-08-27", "2014-01-01", "2020-01-02"
    )
  )
)

markerCohort <- dplyr::tibble(
  cohort_definition_id = c(1, 1, 1, 1),
  subject_id = c(3, 3, 3, 3),
  cohort_start_date = as.Date(
    c(
      "2021-04-25", "2010-08-26","2022-01-02", "2016-03-01"
    )
  ),
  cohort_end_date = as.Date(
    c(
      "2021-04-25","2010-08-27","2022-05-25", "2016-03-14"
    )
  )
)

cdm <-
 PatientProfiles::mockPatientProfiles(
    cohort1 = indexCohort,
    cohort2 = markerCohort,
    patient_size = 3
  )

test_that("mock db: example of multiple entries per person", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           combinationWindow = c(0,Inf)
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 1)
  earliest_index_date <- cdm$cohort1 %>%
    dplyr::arrange(cohort_start_date) %>%
    dplyr::filter(dplyr::row_number()==1) %>%
    dplyr::pull(cohort_start_date)
  earliest_marker_date <- cdm$cohort2 %>%
    dplyr::arrange(cohort_start_date) %>%
    dplyr::filter(dplyr::row_number()==1) %>%
    dplyr::pull(cohort_start_date)
  expect_true(all(cdm$joined_cohorts %>% dplyr::pull(index_date) == earliest_index_date,
                  cdm$joined_cohorts %>% dplyr::pull(marker_date) == earliest_marker_date))
})

test_that("mock db: example of multiple entries per person - exclusion based on timeGap", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           markerTable = "cohort2"
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 0)
})

test_that("mock db: example of multiple entries per person - exclusion based on prior observation", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           daysPriorObservation = 365,
                                           combinationWindow = c(0, Inf)
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 1)
})

test_that("mock db: example of multiple entries per person - picking sequence in relation to dateRange", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           dateRange = as.Date(c("2000-01-01", NA)),
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           daysPriorObservation = 365,
                                           combinationWindow = c(0,Inf)
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 1)
  expect_true(cdm$joined_cohorts %>% dplyr::pull(cohort_start_date) >= as.Date("2000-01-01"))
})

test_that("mock db: example of multiple entries per person - picking sequence in relation to dateRange with fixed timeGap", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           dateRange = as.Date(c("2000-01-01", NA)),
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           daysPriorObservation = 365
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 0)
})

test_that("mock db: example of multiple entries per person - index washout (shouldn't affect the first event)", {
  cdm <- CohortSymmetry::getCohortSequence(cdm,
                                           indexTable ="cohort1",
                                           markerTable = "cohort2",
                                           washoutWindow = 365,
                                           combinationWindow = c(0,Inf)
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
                                           washoutWindow = 365,
                                           combinationWindow = c(0,Inf)
  )
  expect_true(cdm$joined_cohorts %>% dplyr::tally() %>% dplyr::pull(n) == 1)
})

test_that("getcohortSequence - inputValidation", {
  cdm <- PatientProfiles::mockPatientProfiles()
  expect_error(
    CohortSymmetry::getCohortSequence(
      list(),
      indexTable = "cohort1",
      markerTable = "cohort2",
      dateRange = as.Date(c("2002-01-01", NA)),
      washoutWindow = 365,
      combinationWindow = c(0,Inf)
    ),
    "cdm must be a CDMConnector CDM reference object"
  )
  expect_error(
    CohortSymmetry::getCohortSequence(
      cdm = cdm,
      indexTable = "cohort1",
      indexId = 2,
      markerTable = "cohort2",
      dateRange = as.Date(c("2002-01-01", NA)),
      washoutWindow = 365,
      combinationWindow = c(0,Inf)
    ),
    "Some of the cohort ids given do not exist in cohort1"
  )
  expect_error(
    CohortSymmetry::getCohortSequence(
      cdm = cdm,
      indexTable = "cohort1",
      markerTable = "cohort2",
      dateRange = c(as.Date("2002-01-01"),1),
      washoutWindow = 365,
      combinationWindow = c(0, Inf)
    ),
    "First element in dateRange must be smaller than the second."
  )
  expect_error(
    CohortSymmetry::getCohortSequence(
      cdm = cdm,
      indexTable = "cohort1",
      markerTable = "cohort2",
      dateRange = as.Date(c("2002-01-01", NA)),
      washoutWindow = -1,
      daysPriorObservation = Inf,
      combinationWindow = c(0, Inf)
    )
  )
  expect_error(
    CohortSymmetry::getCohortSequence(
      cdm = cdm,
      indexTable = "cohort1",
      markerTable = "cohort2",
      dateRange = as.Date(c("2002-01-01", NA)),
      indexMarkerGap = 41,
      combinationWindow = c(0, 40)
    )
  )
  expect_error(
    CohortSymmetry::getCohortSequence(
      cdm = cdm,
      indexTable = "cohort1",
      markerTable = "cohort2",
      dateRange = as.Date(c("2002-01-01", NA)),
      combinationWindow = c(80, 40)
    )
  )
})

CDMConnector::cdmDisconnect(cdm)

