test_that("tableSequenceRatios - gt output", {
  cdm <- mockCohortSymmetry()
  cdm <- generateSequenceCohortSet(cdm = cdm,
                                                   indexTable = "cohort_1",
                                                   indexId = 1,
                                                   markerTable = "cohort_2",
                                                   markerId = 3,
                                                   name = "joined_cohort")

  res <- summariseSequenceRatios(cohort = cdm$joined_cohort)

  gtResult <- tableSequenceRatios(res)
  expect_true("gt_tbl" %in% (gtResult %>% class()))
  expect_true(all(colnames(gtResult$`_data`) %in%
                    c("Database name", "Index", "Marker", "Study population",
                      "Index first, N (%)", "Marker first, N (%)", "CSR (95% CI)",
                      "ASR (95% CI)")))
  expect_no_error(gtResult <- tableSequenceRatios(res, studyPopulation = FALSE))
  expect_true(all(colnames(gtResult$`_data`) %in%
                    c("Database name", "Index", "Marker",
                      "Index first, N (%)", "Marker first, N (%)", "CSR (95% CI)",
                      "ASR (95% CI)")))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("tableSequenceRatios - tibble output", {
  cdm <- mockCohortSymmetry()
  cdm <- generateSequenceCohortSet(cdm = cdm,
                                                   indexTable = "cohort_1",
                                                   indexId = 1,
                                                   markerTable = "cohort_2",
                                                   markerId = 3,
                                                   name = "joined_cohort")

  res <- summariseSequenceRatios(cohort = cdm$joined_cohort)

  tibble_res <- tableSequenceRatios(res, type = "tibble")

  expect_true("data.frame" %in% (tibble_res %>% class()))
  expect_true(all(colnames(tibble_res) %in%
                    c("Database name", "Index", "Marker", "Study population",
                      "Index first, N (%)", "Marker first, N (%)", "CSR (95% CI)",
                      "ASR (95% CI)")))
  expect_no_error(tibble_res <- tableSequenceRatios(res, type = "tibble",
                                                  studyPopulation = FALSE))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("tableSequenceRatios - flextable output", {
  cdm <- mockCohortSymmetry()
  cdm <- generateSequenceCohortSet(cdm = cdm,
                                                   indexTable = "cohort_1",
                                                   indexId = 1,
                                                   markerTable = "cohort_2",
                                                   markerId = 3,
                                                   name = "joined_cohort")

  res <- summariseSequenceRatios(cohort = cdm$joined_cohort)

  flextable_res <- tableSequenceRatios(res, type = "flextable")

  expect_true("flextable" %in% (flextable_res %>% class()))
  expect_true(all(colnames(flextable_res) %in%
                    c("Database name", "Index", "Marker", "Study population",
                      "Index first, N (%)", "Marker first, N (%)", "CSR (95% CI)",
                      "ASR (95% CI)")))
  expect_no_error(flextable_res <- tableSequenceRatios(res, type = "flextable",
                                                    studyPopulation = FALSE))
  CDMConnector::cdmDisconnect(cdm)
})
