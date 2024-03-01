test_that("tidySequenceSymmetry", {
  cdm <- CohortSymmetry::mockCohortSymmetry()

  cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                   name = "joined_cohorts",
                                                   indexTable = "cohort_1",
                                                   indexId = c(1,2),
                                                   markerTable = "cohort_2",
                                                   markerId = c(1,2),
                                                   combinationWindow = c(0, Inf))

expect_warning(
      expect_no_error(
        res <- CohortSymmetry::summariseSequenceRatio(
          cdm = cdm,
          sequenceCohortSet = "joined_cohorts")
      ))

  expect_no_error(tidy_result <- tidySequenceSymmetry(res))

  expect_true(unique(tidy_result$combination_window) == "(0,Inf)")
  expect_true(unique(tidy_result$days_prior_observation) == "0")
  expect_true(unique(tidy_result$washout_window) == "0")
  expect_true(unique(tidy_result$confidence_interval) == "95")
  expect_true(unique(tidy_result$index_marker_gap) == "Inf")

  expect_true(all(
    colnames(tidy_result) %in%
      c("result_id",
        "cdm_name", "index_cohort_name", "marker_cohort_name",
        "days_prior_observation", "washout_window", "index_marker_gap",
        "combination_window", "confidence_interval", "restriction",
        "index_first_pharmac_count", "index_first_pharmac_percentage",
        "marker_first_pharmac_count", "marker_first_pharmac_percentage",
        "crude_sequence_ratio_point_estimate",
        "adjusted_sequence_ratio_point_estimate",
        "crude_sequence_ratio_lower_CI", "crude_sequence_ratio_upper_CI",
        "adjusted_sequence_ratio_lower_CI", "adjusted_sequence_ratio_upper_CI")
  ))

  cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                   name = "joined_cohorts",
                                                   indexTable = "cohort_1",
                                                   indexId = c(2,3),
                                                   markerTable = "cohort_2",
                                                   markerId = c(1,2),
                                                   indexMarkerGap = 40,
                                                   combinationWindow = c(0, Inf))

expect_warning(
      expect_no_error(
        res <- CohortSymmetry::summariseSequenceRatio(
          cdm = cdm,
          sequenceCohortSet = "joined_cohorts")
      ))

  expect_no_error(tidy_result <- tidySequenceSymmetry(res))
  expect_true(unique(tidy_result$combination_window) == "(0,Inf)")
  expect_true(unique(tidy_result$index_marker_gap) == "40")

  CDMConnector::cdmDisconnect(cdm)
})
