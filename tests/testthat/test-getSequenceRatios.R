test_that("getSequenceRatios", {
  cdm <- PatientProfiles::mockPatientProfiles(patient_size = 100,
                                              drug_exposure_size = 100)

  cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                           indexTable = "cohort1",
                                           markerTable = "cohort2")

  expect_warning(expect_warning(expect_warning(expect_warning(
    expect_warning(expect_warning(expect_warning(expect_warning(
      expect_warning(expect_warning(expect_warning(expect_warning(
        expect_no_error(
          res <- CohortSymmetry::getSequenceRatios(
            cdm = cdm,
            outcomeTable = "joined_cohorts")
        )))))))))))))


  expect_true("sequence_symmetry" %in% class(res))
  expect_true("summarised_result" %in% class(res))
  expect_error(
    CohortSymmetry::getSequenceRatios(
      cdm = cdm,
      outcomeTable = NULL)
  )
  expect_error(
    CohortSymmetry::getSequenceRatios(
      cdm = cdm,
      outcomeTable = character(0))
  )
  expect_error(
    CohortSymmetry::getSequenceRatios(
      cdm = cdm,
      outcomeTable = "joined_cohorts",
      confidenceInterval = 101)
  )
})
