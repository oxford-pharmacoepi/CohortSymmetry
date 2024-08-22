test_that("attrition: output structure", {
  skip_on_cran()
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
  )|>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  markerCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3),
    subject_id = c(1, 3, 4, 2, 5, 1, 2, 3, 4, 5, 6),
    cohort_start_date = as.Date(
      c(
        "2020-12-30", "2010-01-01","2021-05-25","2022-05-31", "2020-05-25", "2019-05-25", "2022-05-25", "2010-09-30", "2022-05-25", "2020-02-29", "2021-01-01"
      )
    ),
    cohort_end_date = cohort_start_date
  ) |>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  cdm <- mockCohortSymmetry(indexCohort = indexCohort,
                            markerCohort = markerCohort)

  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   name = "joined_cohorts",
                                   indexTable = "cohort_1",
                                   markerTable = "cohort_2")

  expect_true(all(c(
    "cohort_definition_id", "number_records", "number_subjects",
    "reason_id", "reason",
    "excluded_records", "excluded_subjects"
  ) %in%
    names(omopgenerics::attrition(cdm$joined_cohorts))))

  expect_true(all(c(omopgenerics::attrition(cdm$joined_cohorts) |>
                      dplyr::select(cohort_definition_id) |>
                      dplyr::distinct()|>
                      dplyr::pull()
  ) %in%
    c(1:4)
  ))

  expect_true(nrow(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(cohort_definition_id=="1"))== "5")

  CDMConnector::cdmDisconnect(cdm)
})

test_that("attrition: cohortDateRange", {
  skip_on_cran()
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
  ) |>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  markerCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3),
    subject_id = c(1, 3, 4, 2, 5, 1, 2, 3, 4, 5, 6),
    cohort_start_date = as.Date(
      c(
        "2020-12-30", "2010-01-01","2021-05-25","2022-05-31", "2020-05-25", "2019-05-25", "2022-05-25", "2010-09-30", "2022-05-25", "2020-02-29", "2021-01-01"
      )
    ),
    cohort_end_date = cohort_start_date
  ) |>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  cdm <- mockCohortSymmetry(indexCohort = indexCohort,
                                            markerCohort = markerCohort)

  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   name = "joined_cohorts",
                                   indexTable = "cohort_1",
                                   indexId = 1,
                                   markerTable = "cohort_2",
                                   markerId = 3,
                                   combinationWindow = c(0, Inf))

  expect_true(all(c(
    "cohort_definition_id", "number_records", "number_subjects",
    "reason_id", "reason",
    "excluded_records", "excluded_subjects"
  ) %in%
    names(omopgenerics::attrition(cdm$joined_cohorts))))

  expect_true(all(omopgenerics::attrition(cdm$joined_cohorts) |>
                    dplyr::select(number_records) == 5))

  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   name = "joined_cohorts",
                                   indexTable = "cohort_1",
                                   indexId = 1,
                                   markerTable = "cohort_2",
                                   markerId = 3,
                                   cohortDateRange=as.Date(c("2019-12-01", "2022-12-31")),
                                   combinationWindow = c(0, Inf))

  expect_true(all(c(
    "cohort_definition_id", "number_records", "number_subjects",
    "reason_id", "reason",
    "excluded_records", "excluded_subjects"
  ) %in%
    names(omopgenerics::attrition(cdm$joined_cohorts))))

  expect_true(all(omopgenerics::attrition(cdm$joined_cohorts) |>
                    dplyr::select(number_records) == 2))

  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   name = "joined_cohorts",
                                   indexTable = "cohort_1",
                                   indexId = 1,
                                   markerTable = "cohort_2",
                                   markerId = 3,
                                   cohortDateRange=as.Date(c("2019-12-01", "2022-12-31")))

  expect_true(all(omopgenerics::attrition(cdm$joined_cohorts) |>
                    dplyr::select(number_records) == 2))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("attrition: combinationWindow", {
  skip_on_cran()
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
  ) |>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  markerCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3),
    subject_id = c(1, 3, 4, 2, 5, 1, 2, 3, 4, 5, 6),
    cohort_start_date = as.Date(
      c(
        "2020-12-30", "2010-01-01","2021-05-25","2022-05-31", "2020-05-25", "2019-05-25", "2022-05-25", "2010-09-30", "2022-05-25", "2020-02-29", "2021-01-01"
      )
    ),
    cohort_end_date = cohort_start_date
  ) |>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  cdm <- mockCohortSymmetry(indexCohort = indexCohort,
                                            markerCohort = markerCohort)

  cdm <- generateSequenceCohortSet(cdm = cdm,
                                                   name = "joined_cohorts",
                                                   indexTable = "cohort_1",
                                                   indexId=1,
                                                   markerTable = "cohort_2",
                                                   markerId=3,
                                                   combinationWindow = c(30,365))

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::select(cohort_definition_id) |>
                     dplyr::distinct() |> dplyr::pull(cohort_definition_id) |>
                     as.numeric(),
                   1)

  expect_true(omopgenerics::attrition(cdm$joined_cohorts) |>
                dplyr::filter(reason == "Events excluded due to the prespecified combination window") |>
                dplyr::pull(excluded_subjects)==1)

  expect_true(omopgenerics::attrition(cdm$joined_cohorts) |>
                dplyr::filter(reason == "Events excluded due to the prespecified index marker gap") |>
                dplyr::pull(excluded_subjects)==0)

  expect_true(omopgenerics::attrition(cdm$joined_cohorts) |>
                dplyr::filter(reason == "Events excluded due to insufficient prior history") |>
                dplyr::pull(excluded_subjects)==0)

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id ==5) |>
                     dplyr::pull(number_subjects) |>
                     as.numeric(),
                   4)

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id ==5) |>
                     dplyr::pull(number_records) |>
                     as.numeric(),
                   4)

  expect_false(2 %in% (cdm$joined_cohorts |>
                         dplyr::collect() |>
                         dplyr::pull(subject_id) |>
                         as.numeric()))

  expect_identical((cdm$joined_cohorts |>
                      dplyr::collect() |>
                      dplyr::pull(subject_id) |>
                      as.numeric()),
                   c(1,3,4,5))

  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   name = "joined_cohorts",
                                   indexTable = "cohort_1",
                                   indexId=1,
                                   markerTable = "cohort_2",
                                   markerId=3,
                                   combinationWindow = c(0,90))

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::select(cohort_definition_id) |>
                     dplyr::distinct() |> dplyr::pull(cohort_definition_id) |>
                     as.numeric(),
                   1)

  expect_true(omopgenerics::attrition(cdm$joined_cohorts) |>
                dplyr::filter(reason == "Events excluded due to the prespecified combination window") |>
                dplyr::pull(excluded_subjects)==4)

  expect_true(omopgenerics::attrition(cdm$joined_cohorts) |>
                dplyr::filter(reason == "Events excluded due to the prespecified index marker gap") |>
                dplyr::pull(excluded_subjects)==0)

  expect_true(omopgenerics::attrition(cdm$joined_cohorts) |>
                dplyr::filter(reason == "Events excluded due to insufficient prior history") |>
                dplyr::pull(excluded_subjects)==0)

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id ==5) |>
                     dplyr::pull(number_subjects) |>
                     as.numeric(),
                   1)

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id ==5) |>
                     dplyr::pull(number_records) |>
                     as.numeric(),
                   1)

  expect_identical(cdm$joined_cohorts |>
                     dplyr::pull(subject_id) |>
                     as.numeric(),
                   2)

  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   name = "joined_cohorts_3",
                                   indexTable = "cohort_1",
                                   indexId=2,
                                   markerTable = "cohort_2",
                                   markerId=3,
                                   combinationWindow = c(0, Inf))

  expect_true(all(omopgenerics::attrition(cdm$joined_cohorts_3) |>
                    dplyr::select(excluded_records) |>
                    dplyr::pull(excluded_records)==0))

  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   name = "joined_cohorts_3",
                                   indexTable = "cohort_1",
                                   indexId=2,
                                   markerTable = "cohort_2",
                                   markerId=3)

  expect_true(omopgenerics::attrition(cdm$joined_cohorts_3) |>
                dplyr::filter(reason == "Events excluded due to the prespecified combination window") |>
                dplyr::pull(excluded_subjects)==4)

  expect_identical((cdm$joined_cohorts_3 |>
                      dplyr::collect() |>
                      dplyr::pull(subject_id)|>
                      as.numeric()),
                   5)

  CDMConnector::cdmDisconnect(cdm)
})

test_that("attrition: indexMarkerGap", {
  skip_on_cran()
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
  ) |>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  markerCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3),
    subject_id = c(1, 3, 4, 2, 5, 1, 2, 3, 4, 5, 6),
    cohort_start_date = as.Date(
      c(
        "2020-12-30", "2010-01-01","2021-05-25","2022-05-31", "2020-05-25", "2019-05-25", "2022-05-25", "2010-09-30", "2022-05-25", "2020-02-29", "2021-01-01"
      )
    ),
    cohort_end_date = cohort_start_date
  ) |>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  cdm <- mockCohortSymmetry(indexCohort = indexCohort,
                            markerCohort = markerCohort)

  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   name = "joined_cohorts_2",
                                   indexTable = "cohort_1",
                                   indexId=1,
                                   markerTable = "cohort_2",
                                   markerId=3,
                                   indexMarkerGap=60)

  expect_true(omopgenerics::attrition(cdm$joined_cohorts_2) |>
                dplyr::filter(reason =="Events excluded due to the prespecified index marker gap") |>
                dplyr::pull(excluded_subjects)==3)

  expect_identical(cdm$joined_cohorts_2 |>
                     dplyr::pull(subject_id) |>
                     as.numeric(),
                   c(2,5)
                   )

  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   name = "joined_cohorts_3",
                                   indexTable = "cohort_1",
                                   indexId=2,
                                   markerTable = "cohort_2",
                                   markerId=3,
                                   combinationWindow = c(0, Inf),
                                   indexMarkerGap = 365)

  expect_true(omopgenerics::attrition(cdm$joined_cohorts_3) |>
                dplyr::filter(reason =="Events excluded due to the prespecified index marker gap") |>
                dplyr::pull(excluded_subjects)==2)

  expect_true(all(omopgenerics::attrition(cdm$joined_cohorts_2) |>
                dplyr::filter(!reason =="Events excluded due to the prespecified index marker gap") |>
                dplyr::pull(excluded_subjects)==0))

  expect_true(all(omopgenerics::attrition(cdm$joined_cohorts_2) |>
                    dplyr::filter(!reason =="Events excluded due to the prespecified index marker gap") |>
                    dplyr::pull(excluded_records)==0))

  expect_identical(cdm$joined_cohorts_3 |>
                     dplyr::pull(subject_id) |>
                     as.numeric(),
                   c(3, 4, 5)
  )

  CDMConnector::cdmDisconnect(cdm = cdm)
})

test_that("attrition: daysPriorObservation", {
  skip_on_cran()
  indexCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1),
    subject_id = c(1, 3, 4, 2, 5, 1, 2, 3, 4, 5, 1),
    cohort_start_date = as.Date(
      c(
        "2020-10-01", "2021-06-01", "2022-05-22", "2010-01-01", "2019-08-01", "2019-04-07", "2021-01-01", "2008-02-02", "2010-09-09", "2021-01-01", "2020-01-01"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-10-01", "2021-08-01", "2022-05-23", "2010-03-01", "2020-04-01", "2020-05-30", "2022-02-02", "2013-12-03", "2010-11-01", "2021-01-01", "2020-03-01"
      )
    )
  ) |>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  markerCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3),
    subject_id = c(1, 3, 4, 2, 5, 1, 2, 3, 4, 5, 1),
    cohort_start_date = as.Date(
      c(
        "2020-12-30", "2010-01-01","2021-05-25","2022-05-31", "2020-05-25", "2019-05-25", "2022-05-25", "2010-09-30", "2022-05-25", "2020-02-29", "2021-01-01"
      )
    ),
    cohort_end_date = cohort_start_date
  ) |>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  cdm <- mockCohortSymmetry(indexCohort = indexCohort,
                                            markerCohort = markerCohort)

  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   name = "joined_cohorts",
                                   indexTable = "cohort_1",
                                   indexId=2,
                                   markerTable = "cohort_2",
                                   markerId=3,
                                   daysPriorObservation = 1460,
                                   combinationWindow = c(0, Inf))

  expect_true(omopgenerics::attrition(cdm$joined_cohorts) |>
                dplyr::filter(reason =="Events excluded due to insufficient prior history") |>
                dplyr::pull(excluded_subjects)==3)

  expect_true(all(omopgenerics::attrition(cdm$joined_cohorts) |>
                dplyr::filter(!reason =="Events excluded due to insufficient prior history") |>
                dplyr::pull(excluded_subjects)==0))

  expect_identical(cdm$joined_cohorts |>
                     dplyr::select(subject_id) |>
                     dplyr::collect() |>
                     dplyr::arrange(subject_id) |>
                     dplyr::pull(subject_id) |>
                     as.numeric(),
                   c(1,2)
  )

  CDMConnector::cdmDisconnect(cdm = cdm)
})

test_that("attrition: washoutWindow", {
  skip_on_cran()
  indexCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
    subject_id = c(1, 1, 1, 2, 2, 1, 1, 1, 2, 2),
    cohort_start_date = as.Date(
      c(
        "2020-10-01", "2021-06-01", "2022-05-22", "2010-01-01", "2019-08-01", "2019-04-07", "2021-01-01", "2008-02-02", "2010-09-09", "2021-01-01"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-10-01", "2021-08-01", "2022-05-23", "2010-03-01", "2020-04-01", "2020-05-30", "2022-02-02", "2013-12-03", "2010-11-01", "2021-01-01"
      )
    )
  ) |>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  markerCohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3),
    subject_id = c(1, 1, 1, 2, 2, 1, 1, 1, 2, 2, 2),
    cohort_start_date = as.Date(
      c(
        "2020-12-30", "2020-01-01","2021-05-25","2021-05-31", "2020-05-25", "2019-05-25", "2022-05-25", "2010-09-30", "2022-05-25", "2020-02-29", "2021-01-01"
      )
    ),
    cohort_end_date = cohort_start_date
  ) |>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  cdm <- mockCohortSymmetry(indexCohort = indexCohort,
                            markerCohort = markerCohort)

  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   name = "joined_cohorts",
                                   indexTable = "cohort_1",
                                   markerTable = "cohort_2",
                                   combinationWindow = c(0, Inf))

  expect_true(all(c(omopgenerics::attrition(cdm$joined_cohorts) |>
                      dplyr::select(cohort_definition_id) |>
                      dplyr::distinct()|>
                      dplyr::pull()
  ) %in%
    c(1:4)
  ))

  expect_identical(nrow(cdm$joined_cohorts |> dplyr::collect()) |>
                     as.numeric(),
                   8)

  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   name = "joined_cohorts",
                                   indexTable = "cohort_1",
                                   indexId = 1,
                                   markerTable = "cohort_2",
                                   markerId = 3,
                                   combinationWindow = c(0, Inf))

  expect_identical(cdm$joined_cohorts |>
                     dplyr::collect() |>
                     nrow() |>
                     as.numeric(),
                   2)

  expect_true(all(omopgenerics::attrition(cdm$joined_cohorts) |>
                    dplyr::pull(excluded_subjects)==0))

  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   name = "joined_cohorts",
                                   indexTable = "cohort_1",
                                   indexId = 1,
                                   markerTable = "cohort_2",
                                   markerId = 3,
                                   cohortDateRange = as.Date(c("2015-01-01", NA)),
                                   combinationWindow = c(0, Inf))

  expect_identical(cdm$joined_cohorts |>
                     dplyr::collect() |>
                     nrow() |>
                     as.numeric(),
                   2)

  expect_true(all(omopgenerics::attrition(cdm$joined_cohorts) |>
                    dplyr::pull(excluded_subjects)==0))

  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   name = "joined_cohorts",
                                   indexTable = "cohort_1",
                                   indexId = 1,
                                   markerTable = "cohort_2",
                                   markerId = 3,
                                   cohortDateRange = as.Date(c("2022-01-01", NA)),
                                   combinationWindow = c(0, Inf))

  expect_identical(cdm$joined_cohorts |>
                     dplyr::collect() |>
                     nrow() |>
                     as.numeric(),
                   1)

  expect_true(all(omopgenerics::attrition(cdm$joined_cohorts) |>
                    dplyr::pull(excluded_subjects)==0))

  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   name = "joined_cohorts",
                                   indexTable = "cohort_1",
                                   indexId = 1,
                                   markerTable = "cohort_2",
                                   markerId = 3,
                                   cohortDateRange = as.Date(c("2022-01-01", NA)),
                                   combinationWindow = c(0, Inf),
                                   washoutWindow = 365)

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason == "Initial qualifying events") |>
                     dplyr::pull(number_records) |>
                     as.numeric(),
                   1)

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason == "Events excluded due to insufficient washout window") |>
                     dplyr::pull(excluded_records) |>
                     as.numeric(),
                   1)

  CDMConnector::cdmDisconnect(cdm = cdm)
})

test_that("attrition: complete example 1", {
  skip_on_cran()
  indexCohort <- dplyr::tibble(
    cohort_definition_id = rep(1, 20),
    subject_id = rep(c(1:4),5),
    cohort_start_date = as.Date(
      c("2010-01-01",
        "2010-01-02",
        "2010-01-03",
        "2010-01-04",
        "2010-01-05",
        "2010-01-06",
        "2010-01-07",
        "2010-01-08",
        "2010-01-09",
        "2010-01-10",
        "2010-01-11",
        "2010-01-12",
        "2010-01-13",
        "2010-01-14",
        "2010-01-15",
        "2010-01-16",
        "2010-01-17",
        "2010-01-18",
        "2010-01-19",
        "2010-01-20")
    ),
    cohort_end_date = cohort_start_date
  ) |>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  markerCohort <- dplyr::tibble(
    cohort_definition_id = rep(1, 20),
    subject_id = rep(c(2,3,1,4),5),
    cohort_start_date = as.Date(
      c("2010-02-01",
        "2010-02-02",
        "2010-02-03",
        "2010-02-04",
        "2010-02-05",
        "2010-02-06",
        "2010-02-07",
        "2010-02-08",
        "2010-02-09",
        "2010-02-10",
        "2010-02-11",
        "2010-02-12",
        "2010-02-13",
        "2010-02-14",
        "2010-02-15",
        "2010-02-16",
        "2010-02-17",
        "2010-02-18",
        "2010-02-19",
        "2010-02-20")
    ),
    cohort_end_date = cohort_start_date
  ) |>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  cdm <- mockCohortSymmetry(indexCohort = indexCohort,
                            markerCohort = markerCohort)

  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   name = "joined_cohorts",
                                   indexTable = "cohort_1",
                                   markerTable = "cohort_2",
                                   daysPriorObservation = 365,
                                   washoutWindow = 30,
                                   indexMarkerGap = 30,
                                   combinationWindow = c(0, 30)
                                   )

  expect_true(all(c(omopgenerics::attrition(cdm$joined_cohorts) |>
                      dplyr::select(reason_id) |>
                      dplyr::pull()
  ) %in%
    c(1:5)
  ))

  expect_true(all(c(omopgenerics::attrition(cdm$joined_cohorts) |>
                      dplyr::select(cohort_definition_id) |>
                      dplyr::distinct() |>
                      dplyr::pull()
  ) %in%
    c(1)
  ))

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id == 1) |>
                     dplyr::pull(number_records) |>
                     as.numeric(),
                   4
                   )

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id == 1) |>
                     dplyr::pull(number_subjects) |>
                     as.numeric(),
                   4
  )

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id == 2) |>
                     dplyr::pull(number_records) |>
                     as.numeric(),
                   2
  )

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id == 2) |>
                     dplyr::pull(number_subjects) |>
                     as.numeric(),
                   2
  )

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id == 2) |>
                     dplyr::pull(excluded_records) |>
                     as.numeric(),
                   2
  )

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id == 2) |>
                     dplyr::pull(excluded_subjects) |>
                     as.numeric(),
                   2
  )

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id == 3) |>
                     dplyr::pull(excluded_records) |>
                     as.numeric(),
                   0
  )

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id == 3) |>
                     dplyr::pull(excluded_subjects) |>
                     as.numeric(),
                   0
  )

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id == 4) |>
                     dplyr::pull(excluded_records) |>
                     as.numeric(),
                   0
  )

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id == 4) |>
                     dplyr::pull(excluded_subjects) |>
                     as.numeric(),
                   0
  )

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id == 5) |>
                     dplyr::pull(excluded_records) |>
                     as.numeric(),
                   0
  )

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id == 5) |>
                     dplyr::pull(excluded_subjects) |>
                     as.numeric(),
                   0
  )

  CDMConnector::cdmDisconnect(cdm = cdm)
})

test_that("attrition: complete example 2", {
  skip_on_cran()
  indexCohort <- dplyr::tibble(
    cohort_definition_id = rep(1, 20),
    subject_id = rep(c(1:4),5),
    cohort_start_date = as.Date(
      c("2010-01-01",
        "2010-01-02",
        "2010-01-03",
        "2010-01-04",
        "2010-01-05",
        "2010-01-06",
        "2010-01-07",
        "2010-01-08",
        "2010-01-09",
        "2010-01-10",
        "2010-01-11",
        "2010-01-12",
        "2010-01-13",
        "2010-01-14",
        "2010-01-15",
        "2010-01-16",
        "2010-01-17",
        "2010-01-18",
        "2010-01-19",
        "2010-01-20")
    ),
    cohort_end_date = cohort_start_date
  ) |>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  markerCohort <- dplyr::tibble(
    cohort_definition_id = rep(1, 20),
    subject_id = rep(c(2,3,1,4),5),
    cohort_start_date = as.Date(
      c("2010-02-01",
        "2010-02-02",
        "2010-02-03",
        "2010-02-04",
        "2010-02-05",
        "2010-02-06",
        "2010-02-07",
        "2010-02-08",
        "2010-02-09",
        "2010-02-10",
        "2010-02-11",
        "2010-02-12",
        "2010-02-13",
        "2010-02-14",
        "2010-02-15",
        "2010-02-16",
        "2010-02-17",
        "2010-02-18",
        "2010-02-19",
        "2010-02-20")
    ),
    cohort_end_date = cohort_start_date
  ) |>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  subject_id = as.integer(.data$subject_id))

  cdm <- mockCohortSymmetry(indexCohort = indexCohort,
                            markerCohort = markerCohort)

  cdm <- generateSequenceCohortSet(cdm = cdm,
                                   name = "joined_cohorts",
                                   indexTable = "cohort_1",
                                   markerTable = "cohort_2",
                                   cohortDateRange = c(as.Date("2010-01-02"), NA),
                                   daysPriorObservation = 365,
                                   washoutWindow = 30,
                                   indexMarkerGap = 30,
                                   combinationWindow = c(0, 90))

  expect_true(all(c(omopgenerics::attrition(cdm$joined_cohorts) |>
                      dplyr::select(reason_id) |>
                      dplyr::pull()
  ) %in%
    c(1:5)
  ))

  expect_true(all(c(omopgenerics::attrition(cdm$joined_cohorts) |>
                      dplyr::select(cohort_definition_id) |>
                      dplyr::distinct() |>
                      dplyr::pull()
  ) %in%
    c(1)
  ))

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id == 1) |>
                     dplyr::pull(number_records) |>
                     as.numeric(),
                   4
  )

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id == 1) |>
                     dplyr::pull(number_subjects) |>
                     as.numeric(),
                   4
  )

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id == 2) |>
                     dplyr::pull(number_records) |>
                     as.numeric(),
                   4
  )

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id == 2) |>
                     dplyr::pull(number_subjects) |>
                     as.numeric(),
                   4
  )

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id == 2) |>
                     dplyr::pull(excluded_records) |>
                     as.numeric(),
                   0
  )

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id == 2) |>
                     dplyr::pull(excluded_subjects) |>
                     as.numeric(),
                   0
  )

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id == 3) |>
                     dplyr::pull(excluded_records) |>
                     as.numeric(),
                   1
  )

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id == 3) |>
                     dplyr::pull(excluded_subjects) |>
                     as.numeric(),
                   1
  )

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id == 4) |>
                     dplyr::pull(excluded_records) |>
                     as.numeric(),
                   1
  )

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id == 4) |>
                     dplyr::pull(excluded_subjects) |>
                     as.numeric(),
                   1
  )

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id == 5) |>
                     dplyr::pull(excluded_records) |>
                     as.numeric(),
                   0
  )

  expect_identical(omopgenerics::attrition(cdm$joined_cohorts) |>
                     dplyr::filter(reason_id == 5) |>
                     dplyr::pull(excluded_subjects) |>
                     as.numeric(),
                   0
  )

  CDMConnector::cdmDisconnect(cdm = cdm)
})
