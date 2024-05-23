test_that("plot working", {
  cdm <- omock::mockCdmReference(cdmName = "mock") |>
    omock::mockPerson(nPerson = 100) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(
      name = "marker_cohort",
      numberCohorts = 2,
      cohortName = c("marker_a", "marker_b"),
      seed = 11
    ) |>
    omock::mockCohort(
      name = "index_cohort",
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

  result <- summariseTemporalSymmetry(cohort = cdm$joined_cohort)
  plotTS <- plotTemporalSymmetry(result = result)
  plotTS2 <- plotTemporalSymmetry(result = result, xlim = c(-100,100))
  plotTS3 <- plotTemporalSymmetry(result = result, colours = c("white", "black"))
  plotTS4 <- plotTemporalSymmetry(result = result, plotTitle = "Test")
  plotTS5 <- plotTemporalSymmetry(result = result, labs = c("lab1", "lab2"))
  plotTS6 <- plotTemporalSymmetry(result = result, scales = "fixed")

  expect_true("ggplot" %in% (plotTS %>% class()))
  expect_true("ggplot" %in% (plotTS2 %>% class()))
  expect_true("ggplot" %in% (plotTS3 %>% class()))
  expect_true("ggplot" %in% (plotTS5 %>% class()))
  expect_true("ggplot" %in% (plotTS6 %>% class()))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("expected errors", {
  cdm <- omock::mockCdmReference(cdmName = "mock") |>
    omock::mockPerson(nPerson = 100) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(
      name = "marker_cohort",
      numberCohorts = 2,
      cohortName = c("marker_a", "marker_b"),
      seed = 11
    ) |>
    omock::mockCohort(
      name = "index_cohort",
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

  result <- summariseTemporalSymmetry(cohort = cdm$joined_cohort)
  expect_error(plotTemporalSymmetry("cdm", "joined_cohort"))
  expect_error(plotTemporalSymmetry(cdm, "joinedd_cohort"))
  expect_error(plotTemporalSymmetry(result = result, xlim = 2))
  expect_error(plotTemporalSymmetry(result = result, xlim = "4"))
  expect_error(plotTemporalSymmetry(result = result, xlim = 2))
  expect_error(plotTemporalSymmetry(result = result, colours = c("no", "black")))
  expect_error(plotTemporalSymmetry(result = result, colours = "red"))
  expect_error(plotTemporalSymmetry(result = result, colours = c(3,4)))
  expect_error(plotTemporalSymmetry(result = result, plotTitle = 2))
  expect_error(plotTemporalSymmetry(result = result, plotTitle = c("1", "2")))
  expect_error(plotTemporalSymmetry(result = result, labs = NULL))
  expect_error(plotTemporalSymmetry(result = result, labs = c(2,3)))
  expect_error(plotTemporalSymmetry(result = result, labs = c("a", "b", "c")))
  expect_error(plotTemporalSymmetry(result = result, scales = "days"))
  expect_error(plotTemporalSymmetry(result = result, scales = c("free", "fixed")))

  CDMConnector::cdmDisconnect(cdm)
})
