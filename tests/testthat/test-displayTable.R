test_that("tableSequenceRatios - gt output", {
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  cdm <- mockCohortSymmetry()
  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   indexTable = "cohort_1",
                                   indexId = 1,
                                   markerTable = "cohort_2",
                                   markerId = 3,
                                   name = "joined_cohort")

  res <- summariseSequenceRatios(cohort = cdm$joined_cohort, minCellCount = 0)

  gtResult <- tableSequenceRatios(res)
  expect_true("gt_tbl" %in% (gtResult %>% class()))
  expect_true(all(colnames(gtResult$`_data`) %in%
                    c("Database name", "Index", "Marker", "Study population",
                      "Index first, N (%)", "Marker first, N (%)", "CSR (95% CI)",
                      "ASR (95% CI)")))
  expect_no_error(gtResult <- tableSequenceRatios(res, studyPopulation = FALSE))
  expect_no_error(gtResult <- tableSequenceRatios(res, cdmName = FALSE))
  expect_true(all(colnames(gtResult$`_data`) %in%
                    c("Index", "Marker", "Study population",
                      "Index first, N (%)", "Marker first, N (%)", "CSR (95% CI)",
                      "ASR (95% CI)")))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("tableSequenceRatios - tibble output", {
  skip_on_cran()
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  cdm <- mockCohortSymmetry()
  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   indexTable = "cohort_1",
                                   indexId = 1,
                                   markerTable = "cohort_2",
                                   markerId = 3,
                                   name = "joined_cohort")

  res <- summariseSequenceRatios(cohort = cdm$joined_cohort, minCellCount = 0)

  tibble_res <- tableSequenceRatios(res, type = "tibble")

  expect_true("data.frame" %in% (tibble_res %>% class()))
  expect_true(all(colnames(tibble_res) %in%
                    c("Database name", "Index", "Marker", "Study population",
                      "Index first, N (%)", "Marker first, N (%)", "CSR (95% CI)",
                      "ASR (95% CI)")))
  expect_no_error(tibble_res <- tableSequenceRatios(res, type = "tibble",
                                                  studyPopulation = FALSE))
  expect_no_error(gtResult <- tableSequenceRatios(res, type = "tibble",
                                                  cdmName = FALSE))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("tableSequenceRatios - flextable output", {
  skip_on_cran()
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  cdm <- mockCohortSymmetry()
  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   indexTable = "cohort_1",
                                   indexId = 1,
                                   markerTable = "cohort_2",
                                   markerId = 3,
                                   name = "joined_cohort")

  res <- summariseSequenceRatios(cohort = cdm$joined_cohort, minCellCount = 0)

  flextable_res <- tableSequenceRatios(res, type = "flextable")

  expect_true("flextable" %in% (flextable_res %>% class()))
  expect_true(all(colnames(flextable_res) %in%
                    c("Database name", "Index", "Marker", "Study population",
                      "Index first, N (%)", "Marker first, N (%)", "CSR (95% CI)",
                      "ASR (95% CI)")))
  expect_no_error(flextable_res <- tableSequenceRatios(res, type = "flextable",
                                                    studyPopulation = FALSE))
  expect_no_error(flextable_res <- tableSequenceRatios(res, type = "flextable",
                                                       cdmName = FALSE))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("tableSequenceRatio options", {
  skip_on_cran()
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  cdm <- mockCohortSymmetry()
  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   indexTable = "cohort_1",
                                   markerTable = "cohort_2",
                                   name = "joined_cohort")

  expect_warning(
    result <- summariseSequenceRatios(cohort = cdm$joined_cohort, minCellCount = 0)
  )

  expect_no_error(
    tableSequenceRatios(result = result,
                        .options = NULL)
  )

  expect_error(
    tableSequenceRatios(result = result,
                        .options = list(titless = "Title"))
  )

  expect_no_error(
    tableSequenceRatios(result = result,
                        .options = list(
                          groupColumn = c("cdm_name")
                          )))

  expect_no_error(
    tableSequenceRatios(result = result,
                        .options = list(bigMark = " < "))
  )

  expect_no_error(
    tableSequenceRatios(result = result,
                        .options = list(decimalMark = " < "))
  )

  expect_no_error(
    tableSequenceRatios(result = result,
                        .options = list(keepNotFormatted = F))
  )

  expect_no_error(
    tableSequenceRatios(result = result,
                        .options = list(useFormatOrder = F))
  )

  expect_no_error(
    tableSequenceRatios(result = result,
                        .options = list(includeHeaderName = T))
  )

  expect_no_error(
    tableSequenceRatios(result = result,
                        .options = list(includeHeaderKey = F))
  )

  expect_no_error(
    tableSequenceRatios(result = result,
                        .options = list(title = "Title"))
  )

  expect_no_error(
    tableSequenceRatios(result = result,
                        .options = list(
                          title = "Title",
                          subtitle = "Subtitle"))
  )

  expect_no_error(
    tableSequenceRatios(result = result,
                        .options = list(caption = "caption"))
  )

  expect_no_error(
    tableSequenceRatios(result = result,
                        .options = list(na = "NA"))
  )

  expect_no_error(
    tableSequenceRatios(result = result,
                        .options = list(groupAsColumn = T))
  )

  CDMConnector::cdm_disconnect(cdm = cdm)
})
