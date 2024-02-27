connectionDetails<- list(
  con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
  writeSchema = "main",
  mockPrefix = NULL
)

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

test_that("mock db: unsuccessful examples - Inf prior observation", {
  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                 indexTable ="cohort1",
                                                 markerTable = "cohort2",
                                                 daysPriorObservation = Inf
  ))
})

test_that("mock db: unsuccessful examples - indexTable not strings", {
  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                 indexTable = cohort1,
                                                 markerTable = "cohort2",
                                                 daysPriorObservation = 0
  ))
})

test_that("mock db: unsuccessful examples - markerTable not strings", {
  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                 indexTable = "cohort1",
                                                 markerTable = cohort2,
                                                 daysPriorObservation = 0
  ))
})

test_that("mock db: unsuccessful examples - daysPriorObservation is not numeric", {
  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                 indexTable = "cohort1",
                                                 markerTable = "cohort2",
                                                 daysPriorObservation = "seven"
  ))
  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                 indexTable = "cohort1",
                                                 markerTable = "cohort2",
                                                 daysPriorObservation = 2.5
  ))
})

test_that("mock db: unsuccessful examples - Ids outside of range", {
  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                 indexTable = "cohort1",
                                                 markerTable = "cohort2",
                                                 indexId = 2
  ))
  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                 indexTable = "cohort1",
                                                 markerTable = "cohort2",
                                                 markerId = 2
  ))
})

test_that("mock db: unsuccessful examples - tables not in the CDM", {
  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                 indexTable = "cohort1",
                                                 markerTable = "cohort3",
                                                 indexId = 2
  ))
})

test_that("mock db: unsuccessful examples - tables not in the right format", {
  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                 indexTable = "cohort1",
                                                 markerTable = "drug_exposure"
  ))
})

test_that("mock db: unsuccessful examples - tables not in the right format", {
  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                 indexTable = "cohort1",
                                                 markerTable = "drug_exposure"
  ))
})

test_that("mock db: unsuccessful examples - negative parameters", {
  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                 indexTable = "cohort1",
                                                 markerTable = "drug_exposure",
                                                 daysPriorObservation = -100
  ))
  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                 indexTable = "cohort1",
                                                 markerTable = "drug_exposure",
                                                 washoutWindow = -100
  ))
  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                 indexTable = "cohort1",
                                                 markerTable = "drug_exposure",
                                                 combinationWindow = c(-200,-100)
  ))
})

CDMConnector::cdmDisconnect(cdm)
