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
  plotTS5 <- plotTemporalSymmetry(cdm, "joined_cohort", plotTitle = "Test")
  plotTS6 <- plotTemporalSymmetry(cdm, "joined_cohort", timescale = "day")
  plotTS7 <- plotTemporalSymmetry(cdm, "joined_cohort", labs = c("lab1", "lab2"))
  plotTS8 <- plotTemporalSymmetry(cdm, "joined_cohort", index_ids = c(1))
  plotTS9 <- plotTemporalSymmetry(cdm, "joined_cohort", marker_ids = c(1,2))
  plotTS10 <- plotTemporalSymmetry(cdm, "joined_cohort", scales = "fixed")

  expect_true("ggplot" %in% (plotTS %>% class()))
  expect_true("ggplot" %in% (plotTS2 %>% class()))
  expect_true("ggplot" %in% (plotTS3 %>% class()))
  expect_true("ggplot" %in% (plotTS5 %>% class()))
  expect_true("ggplot" %in% (plotTS6 %>% class()))
  expect_true("ggplot" %in% (plotTS7 %>% class()))
  expect_true("ggplot" %in% (plotTS8 %>% class()))
  expect_true("ggplot" %in% (plotTS9 %>% class()))
  expect_true("ggplot" %in% (plotTS10 %>% class()))

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
  expect_error(plotTemporalSymmetry(cdm, "joined_cohort", plotTitle = 2))
  expect_error(plotTemporalSymmetry(cdm, "joined_cohort", plotTitle = c("1", "2")))
  expect_error(plotTemporalSymmetry(cdm, "joined_cohort", labs = NULL))
  expect_error(plotTemporalSymmetry(cdm, "joined_cohort", labs = c(2,3)))
  expect_error(plotTemporalSymmetry(cdm, "joined_cohort", labs = c("a", "b", "c")))
  expect_error(plotTemporalSymmetry(cdm, "joined_cohort", index_ids = 6))
  expect_error(plotTemporalSymmetry(cdm, "joined_cohort", marker_ids = c("1", "2")))
  expect_error(plotTemporalSymmetry(cdm, "joined_cohort", timescale = 44))
  expect_error(plotTemporalSymmetry(cdm, "joined_cohort", timescale = "days"))
  expect_error(plotTemporalSymmetry(cdm, "joined_cohort", scales = "days"))
  expect_error(plotTemporalSymmetry(cdm, "joined_cohort", scales = c("free", "fixed")))

  CDMConnector::cdmDisconnect(cdm)
})

