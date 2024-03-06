test_that("plot working", {
  cdm <- omock::mockCdmReference(cdmName = "mock") |>
    omock::mockPerson(nPerson = 100) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(
      tableName = "marker_cohort",
      numberCohorts = 2,
      cohortName = c("marker_a", "marker_b"),
      seed = 11
    ) |>
    omock::mockCohort(
      tableName = "index_cohort",
      numberCohorts = 2,
      cohortName = c("index_a", "index_b"),
      seed = 33
    )
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cdm <- CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)
  cdm <- generateSequenceCohortSet(cdm, "index_cohort", "marker_cohort", "joined_cohort", combinationWindow = c(0, Inf))

  plotTS <- plotTemporalSymmetry(cdm, "joined_cohort")
  plotTS2 <- plotTemporalSymmetry(cdm, "joined_cohort", xlim = c(-100,100))
  plotTS3 <- plotTemporalSymmetry(cdm, "joined_cohort", colours = c("white", "black"))
  plotTS4 <- plotTemporalSymmetry(cdm, "joined_cohort", censorRange = c(0,6))

  expect_true("ggplot" %in% (plotTS %>% class()))
  expect_true("ggplot" %in% (plotTS2 %>% class()))
  expect_true("ggplot" %in% (plotTS3 %>% class()))
  expect_true("ggplot" %in% (plotTS4 %>% class()))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("expected errors", {
  cdm <- omock::mockCdmReference(cdmName = "mock") |>
    omock::mockPerson(nPerson = 100) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(
      tableName = "marker_cohort",
      numberCohorts = 2,
      cohortName = c("marker_a", "marker_b"),
      seed = 11
    ) |>
    omock::mockCohort(
      tableName = "index_cohort",
      numberCohorts = 2,
      cohortName = c("index_a", "index_b"),
      seed = 33
    )
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cdm <- CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)
  cdm <- generateSequenceCohortSet(cdm, "index_cohort", "marker_cohort", "joined_cohort", combinationWindow = c(0, Inf))

  expect_error(plotTemporalSymmetry("cdm", "joined_cohort"))
  expect_error(plotTemporalSymmetry(cdm, "joinedd_cohort"))
  expect_error(plotTemporalSymmetry(cdm, "joined_cohort", xlim = 2))
  expect_error(plotTemporalSymmetry(cdm, "joined_cohort", xlim = "4"))
  expect_error(plotTemporalSymmetry(cdm, "joined_cohort", xlim = 2))
  expect_error(plotTemporalSymmetry(cdm, "joined_cohort", colours = c("no", "black")))
  expect_error(plotTemporalSymmetry(cdm, "joined_cohort", colours = "red"))
  expect_error(plotTemporalSymmetry(cdm, "joined_cohort", colours = c(3,4)))
  expect_error(plotTemporalSymmetry(cdm, "joined_cohort", censorRange = -3))
  expect_error(plotTemporalSymmetry(cdm, "joined_cohort", censorRange = c(1, "2")))

  CDMConnector::cdmDisconnect(cdm)
})

