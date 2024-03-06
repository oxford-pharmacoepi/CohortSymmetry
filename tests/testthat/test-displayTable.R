test_that("tableSequenceRatios", {
  cdm <- PatientProfiles::mockPatientProfiles(patient_size = 100,
                                              drug_exposure_size = 100)

  cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                   name = "joined_cohort",
                                           indexTable = "cohort1",
                                           markerTable = "cohort2",
                                           combinationWindow = c(0, Inf))


  res <- CohortSymmetry::summariseSequenceRatio(
    cdm = cdm,
    sequenceTable = "joined_cohort")

  gtResult <- tableSequenceRatios(res)
  expect_true("gt_tbl" %in% (gtResult %>% class()))
  expect_true(all(colnames(gtResult$`_data`) %in%
                    c("Database name", "Index", "Marker", "Study population",
                      "Index first, N (%)", "Marker first, N (%)", "CSR (95% CI)",
                      "ASR (95% CI)")))
  expect_no_error(gtResult <- tableSequenceRatios(res,
                                                     studyPopulation = FALSE))
  expect_true(all(colnames(gtResult$`_data`) %in%
                    c("Database name", "Index", "Marker",
                      "Index first, N (%)", "Marker first, N (%)", "CSR (95% CI)",
                      "ASR (95% CI)")))

  expect_no_error(
    fxResult <- tableSequenceRatios(res,
                                       type = "fx",
                                       studyPopulation = FALSE,
                                       indexName = FALSE,
                                       .options = list(groupNameCol = "cdm_name"))
  )
  expect_true(all(colnames(fxResult$body$dataset) %in%
                    c("Database name", "Marker","Index first, N (%)",
                      "Marker first, N (%)", "CSR (95% CI)", "ASR (95% CI)")))
  expect_equal(fxResult$body$dataset$`Database name`,
               factor(c("PP_MOCK", NA, NA, NA)))

  expect_no_error(
    tibbleResult <- tableSequenceRatios(res,
                                           type = "tibble",
                                           studyPopulation = FALSE,
                                           cdmName = FALSE)
  )
  expect_true(tibbleResult$Index[1] == "Cohort 1")
  expect_true(all(tibbleResult$Marker %in% c("Cohort 1", "Cohort 2", "Cohort 3")))

  CDMConnector::cdmDisconnect(cdm)
})
