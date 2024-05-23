test_that("unsuccessful examples - Inf prior observation", {
  cdm <- mockCohortSymmetry()

  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable ="cohort1",
                                         markerTable = "cohort2",
                                         daysPriorObservation = Inf
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("unsuccessful examples - name not in the right form", {
  cdm <- mockCohortSymmetry()
  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joinCohorts",
                                         indexTable = "cohort_1",
                                         markerTable = "cohort_2",
                                         daysPriorObservation = 0
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("unsuccessful examples - indexTable not strings", {
  cdm <- mockCohortSymmetry()
  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable = cohort1,
                                         markerTable = "cohort2",
                                         daysPriorObservation = 0
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("unsuccessful examples - markerTable not strings", {
  cdm <- mockCohortSymmetry()
  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable = "cohort1",
                                         markerTable = cohort2,
                                         daysPriorObservation = 0
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("unsuccessful examples - daysPriorObservation is not numeric", {
  cdm <- mockCohortSymmetry()
  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable = "cohort1",
                                         markerTable = "cohort2",
                                         daysPriorObservation = "seven"
  ))
  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable = "cohort1",
                                         markerTable = "cohort2",
                                         daysPriorObservation = 2.5
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("unsuccessful examples - Ids outside of range", {
  cdm <- mockCohortSymmetry()
  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable = "cohort1",
                                         markerTable = "cohort2",
                                         indexId = 2
  ))
  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable = "cohort1",
                                         markerTable = "cohort2",
                                         markerId = 2
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("unsuccessful examples - tables not in the CDM", {
  cdm <- mockCohortSymmetry()
  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable = "cohort1",
                                         markerTable = "cohort3",
                                         indexId = 2
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("mock db: unsuccessful examples - tables not in the right format", {
  cdm <- mockCohortSymmetry()
  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable = "cohort1",
                                         markerTable = "drug_exposure"
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("mock db: unsuccessful examples - tables not in the right format", {
  cdm <- mockCohortSymmetry()
  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable = "cohort1",
                                         markerTable = "drug_exposure"
  ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("unsuccessful examples - negative parameters", {
  cdm <- mockCohortSymmetry()
  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable = "cohort1",
                                         markerTable = "drug_exposure",
                                         daysPriorObservation = -100
  ))
  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable = "cohort1",
                                         markerTable = "drug_exposure",
                                         washoutWindow = -100
  ))
  expect_error(generateSequenceCohortSet(cdm,
                                         name = "joined_cohorts",
                                         indexTable = "cohort1",
                                         markerTable = "drug_exposure",
                                         combinationWindow = c(-200,-100)
  ))
  CDMConnector::cdmDisconnect(cdm)
})
