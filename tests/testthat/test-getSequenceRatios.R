test_that("getSequenceRatios", {
  cdm <- PatientProfiles::mockPatientProfiles(patient_size = 100,
                                              drug_exposure_size = 100)

  cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                           indexTable = "cohort1",
                                           markerTable = "cohort2")

  expect_warning(expect_warning(expect_warning(expect_warning(
    expect_warning(expect_warning(expect_warning(expect_warning(
      expect_warning(expect_warning(expect_warning(expect_warning(
        expect_no_error(
          res <- CohortSymmetry::getSequenceRatios(
            cdm = cdm,
            sequenceCohortSet = "joined_cohorts")
        )))))))))))))


  expect_true("sequence_symmetry" %in% class(res))
  expect_true("summarised_result" %in% class(res))
  expect_error(
    CohortSymmetry::getSequenceRatios(
      cdm = cdm,
      sequenceCohortSet = NULL)
  )
  expect_error(
    CohortSymmetry::getSequenceRatios(
      cdm = cdm,
      sequenceCohortSet = character(0))
  )
  expect_error(
    CohortSymmetry::getSequenceRatios(
      cdm = cdm,
      sequenceCohortSet = "joined_cohorts",
      confidenceInterval = 101)
  )

  expect_error(
    CohortSymmetry::getSequenceRatios(
      cdm = cdm,
      sequenceCohortSet = "joined_cohorts",
      confidenceInterval = -101)
  )

    expect_error(
      CohortSymmetry::getSequenceRatios(
      cdm = cdm,
      sequenceCohortSet = "cohort",
      confidenceInterval = 101)
  )
})

test_that("getSequenceRatios - testing ratios and CIs", {
  indexCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
    subject_id = c(1, 4, 2, 3, 5, 5, 4, 3, 6, 1),
    cohort_start_date = as.Date(
      c(
        "2020-04-01", "2021-06-01", "2022-05-22", "2010-01-01", "2019-08-01", "2019-04-07", "2021-01-01", "2008-02-02", "2010-09-09", "2021-01-01"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-04-01", "2021-08-01", "2022-05-23", "2010-03-01", "2020-04-01", "2020-05-30", "2022-02-02", "2013-12-03", "2010-11-01", "2021-01-01"
      )
    )
  )

  markerCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3),
    subject_id = c(1, 3, 4, 2, 5, 1, 2, 3, 4, 5, 6),
    cohort_start_date = as.Date(
      c(
        "2020-12-30", "2010-01-01","2021-05-25","2022-05-31", "2020-05-25", "2019-05-25", "2022-05-25", "2010-09-30", "2022-05-25", "2020-02-29", "2021-01-01"
      )
    ),
    cohort_end_date = cohort_start_date
  )

  cdm <- PatientProfiles::mockPatientProfiles(patient_size = 100,
                                              cohort1 = indexCohort,
                                              cohort2 = markerCohort,
                                              drug_exposure_size = 100)

  cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                           indexTable = "cohort1",
                                           markerTable = "cohort2")

  suppressWarnings(
    res <- CohortSymmetry::getSequenceRatios(
      cdm = cdm,
      sequenceCohortSet = "joined_cohorts")
  )

  res <- CohortSymmetry::tidySequenceSymmetry(res)

  expect_true(all(res$days_prior_observation==0))
  expect_true(all(res$washout_window==0))
  expect_true(all(res$combination_window == "(0,365)"))
  expect_true(all(res$index_marker_gap==365))
  expect_true(all(res$confidence_interval==95))
  expect_true(all(res$restriction==548))
  expect_true(all(as.integer(res$index_first_pharmac_percentage)<=100 & 0 <= as.integer(res$index_first_pharmac_percentage)))

  int <- res %>%
    dplyr::mutate(crude_ci_check = .data$crude_sequence_ratio_lower_CI <= .data$crude_sequence_ratio_upper_CI)

  expect_true(all(as.integer(int$crude_ci_check== T)))
  CDMConnector::cdmDisconnect(cdm)
  })


test_that("getSequenceRatios - testing ratios and CIs", {
  indexCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    subject_id = c(1, 4, 2, 3, 5, 7, 8, 9, 6, 10),
    cohort_start_date = as.Date(
      c(
        "2020-04-01", "2021-06-01", "2022-05-22", "2010-01-01", "2019-08-01", "2019-04-07", "2021-01-01", "2008-02-02", "2010-09-09", "2021-01-01"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-04-01", "2021-08-01", "2022-05-23", "2010-03-01", "2020-04-01", "2020-05-30", "2022-02-02", "2013-12-03", "2010-11-01", "2021-01-01"
      )
    )
  )

  markerCohort <- dplyr::tibble(
    cohort_definition_id = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3),
    subject_id = c(1, 3, 4, 2, 5, 10, 9, 8, 7, 11, 6),
    cohort_start_date = as.Date(
      c(
        "2020-12-30", "2010-01-01","2021-05-25","2022-05-31", "2020-05-25", "2019-05-25", "2022-05-25", "2010-09-30", "2022-05-25", "2020-02-29", "2021-01-01"
      )
    ),
    cohort_end_date = cohort_start_date
  )

  cdm <- PatientProfiles::mockPatientProfiles(patient_size = 100,
                                              cohort1 = indexCohort,
                                              cohort2 = markerCohort,
                                              drug_exposure_size = 100)

  cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                           indexTable = "cohort1",
                                           markerTable = "cohort2")

  res <- CohortSymmetry::getSequenceRatios(
    cdm = cdm,
    sequenceCohortSet = "joined_cohorts")
  res <- CohortSymmetry::tidySequenceSymmetry(res)

  expect_true(all(res$days_prior_observation==0))
  expect_true(all(res$washout_window==0))
  expect_true(all(res$combination_window == "(0,365)"))
  expect_true(all(res$index_marker_gap==365))
  expect_true(all(res$confidence_interval==95))
  expect_true(all(res$restriction==548))
  expect_true((res$index_cohort_name=="cohort_1"))
  expect_true((res$marker_cohort_name=="cohort_3"))
  expect_true(all(as.integer(res$index_first_pharmac_percentage)<=100 & 0 <= as.integer(res$index_first_pharmac_percentage)))

  int <- res %>%
    dplyr::mutate(crude_ci_check = .data$crude_sequence_ratio_lower_CI <= .data$crude_sequence_ratio_upper_CI,
                  adjusted_ci_check = .data$adjusted_sequence_ratio_lower_CI <= .data$adjusted_sequence_ratio_upper_CI)

  expect_true(all(as.integer(int$crude_ci_check== T)))
  expect_true(all(as.integer(int$adjusted_ci_check== T)))
  CDMConnector::cdmDisconnect(cdm)
})

