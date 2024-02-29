test_that("mock db: unsuccessful examples - Inf prior observation", {
  cdm <- CohortSymmetry::mockCohortSymmetry()

  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                         name = "joined_cohorts",
                                                 indexTable ="cohort1",
                                                 markerTable = "cohort2",
                                                 daysPriorObservation = Inf
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("mock db: unsuccessful examples - indexTable not strings", {
  cdm <- CohortSymmetry::mockCohortSymmetry()
  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                         name = "joined_cohorts",
                                                 indexTable = cohort1,
                                                 markerTable = "cohort2",
                                                 daysPriorObservation = 0
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("mock db: unsuccessful examples - markerTable not strings", {
  cdm <- CohortSymmetry::mockCohortSymmetry()
  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                         name = "joined_cohorts",
                                                 indexTable = "cohort1",
                                                 markerTable = cohort2,
                                                 daysPriorObservation = 0
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("mock db: unsuccessful examples - daysPriorObservation is not numeric", {
  cdm <- CohortSymmetry::mockCohortSymmetry()
  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                         name = "joined_cohorts",
                                                 indexTable = "cohort1",
                                                 markerTable = "cohort2",
                                                 daysPriorObservation = "seven"
  ))
  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                         name = "joined_cohorts",
                                                 indexTable = "cohort1",
                                                 markerTable = "cohort2",
                                                 daysPriorObservation = 2.5
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("mock db: unsuccessful examples - Ids outside of range", {
  cdm <- CohortSymmetry::mockCohortSymmetry()
  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                         name = "joined_cohorts",
                                                 indexTable = "cohort1",
                                                 markerTable = "cohort2",
                                                 indexId = 2
  ))
  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                         name = "joined_cohorts",
                                                 indexTable = "cohort1",
                                                 markerTable = "cohort2",
                                                 markerId = 2
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("mock db: unsuccessful examples - tables not in the CDM", {
  cdm <- CohortSymmetry::mockCohortSymmetry()
  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                         name = "joined_cohorts",
                                                 indexTable = "cohort1",
                                                 markerTable = "cohort3",
                                                 indexId = 2
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("mock db: unsuccessful examples - tables not in the right format", {
  cdm <- CohortSymmetry::mockCohortSymmetry()
  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                         name = "joined_cohorts",
                                                 indexTable = "cohort1",
                                                 markerTable = "drug_exposure"
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("mock db: unsuccessful examples - tables not in the right format", {
  cdm <- CohortSymmetry::mockCohortSymmetry()
  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                         name = "joined_cohorts",
                                                 indexTable = "cohort1",
                                                 markerTable = "drug_exposure"
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("mock db: unsuccessful examples - negative parameters", {
  cdm <- CohortSymmetry::mockCohortSymmetry()
  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                         name = "joined_cohorts",
                                                 indexTable = "cohort1",
                                                 markerTable = "drug_exposure",
                                                 daysPriorObservation = -100
  ))
  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                         name = "joined_cohorts",
                                                 indexTable = "cohort1",
                                                 markerTable = "drug_exposure",
                                                 washoutWindow = -100
  ))
  expect_error(CohortSymmetry::generateSequenceCohortSet(cdm,
                                                         name = "joined_cohorts",
                                                 indexTable = "cohort1",
                                                 markerTable = "drug_exposure",
                                                 combinationWindow = c(-200,-100)
  ))
  CDMConnector::cdmDisconnect(cdm)
})
