test_that("test summariseTemporalSymmetry", {

cdm <- mockCohortSymmetry()
cdm <- generateSequenceCohortSet(cdm = cdm,
                                    name = "joined_cohorts",
                                    indexTable = "cohort_1",
                                    markerTable = "cohort_2")
temporal_symmetry <- summariseTemporalSymmetry(cohort = cdm$joined_cohorts)

expect_true(
  all(names(temporal_symmetry) %in% c(
    "result_id",
    "cdm_name",
    "group_name",
    "group_level",
    "strata_name",
    "strata_level",
    "variable_name",
    "variable_level",
    "estimate_name",
    "estimate_type",
    "estimate_value",
    "additional_name",
    "additional_level"
  )
))



})
