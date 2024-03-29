test_that("eunomia - generateSequenceCohortSet", {

  if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") {
    Sys.setenv("EUNOMIA_DATA_FOLDER" = tempdir())
  }
  if (!dir.exists(Sys.getenv("EUNOMIA_DATA_FOLDER"))) {
    dir.create(Sys.getenv("EUNOMIA_DATA_FOLDER"))
  }
  if (!CDMConnector::eunomia_is_available()) {
    invisible(utils::capture.output(CDMConnector::downloadEunomiaData(pathToData = Sys.getenv("EUNOMIA_DATA_FOLDER"))))
  }

  con <- DBI::dbConnect(duckdb::duckdb(),
                        dbdir = CDMConnector::eunomia_dir())
  cdm <- CDMConnector::cdm_from_con(con, cdm_schema = "main", write_schema = "main")

  index_drug <- CodelistGenerator::getDrugIngredientCodes(cdm = cdm,
                                                          name = "celecoxib")
  marker_drug <- CodelistGenerator::getDrugIngredientCodes(cdm = cdm,
                                                           name = "aspirin")

  cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = "cohort1",
    conceptSet = index_drug
  )
  cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = "cohort2",
    conceptSet = marker_drug
  )

  expect_no_error(
    cdm <- CohortSymmetry::generateSequenceCohortSet(cdm,
                                                     name = "joined_cohorts",
                                                     indexTable ="cohort1",
                                                     markerTable = "cohort2",
                                                     combinationWindow = c(0,Inf)))

 expect_true(nrow(cdm$joined_cohorts %>% dplyr::collect()) > 0)

 expect_no_error(
   res <- CohortSymmetry::summariseSequenceRatio(cdm = cdm,
                                                 sequenceTable = "joined_cohorts")
 )

 expect_true(nrow(res) > 0)

 expect_no_error(
   expect_warning(nice_table <- CohortSymmetry::tableSequenceRatios(res))
 )

 expect_no_error(
   expect_warning(nice_table <- CohortSymmetry::tableSequenceRatios(res, type = "flextable"))
 )

 expect_no_error(
   expect_warning(nice_table <- CohortSymmetry::tableSequenceRatios(res, type = "tibble"))
 )

  CDMConnector::cdm_disconnect(cdm)
})
