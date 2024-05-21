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
  result <- summariseSequenceRatio(cdm, "joined_cohort")

  plotSR <- plotSequenceRatio(cdm, "joined_cohort", result)
  plotSR2 <- plotSequenceRatio(cdm, "joined_cohort", result, indexId = 1)
  plotSR3 <- plotSequenceRatio(cdm, "joined_cohort", result, markerId = 2)
  plotSR4 <- plotSequenceRatio(cdm, "joined_cohort", result, plotTitle = "Test plot")
  plotSR5 <- plotSequenceRatio(cdm, "joined_cohort", result, labs = c("xlab", "ylab"))
  plotSR6 <- plotSequenceRatio(cdm, "joined_cohort", result, colours = c("blue", "green"))
  plotSR7 <- plotSequenceRatio(cdm, "joined_cohort", result, onlyaSR = TRUE, colours = c("orange"))

  expect_true("ggplot" %in% (plotSR %>% class()))
  expect_true("ggplot" %in% (plotSR2 %>% class()))
  expect_true("ggplot" %in% (plotSR3 %>% class()))
  expect_true("ggplot" %in% (plotSR4 %>% class()))
  expect_true("ggplot" %in% (plotSR5 %>% class()))
  expect_true("ggplot" %in% (plotSR6 %>% class()))

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
  result <- summariseSequenceRatio(cdm, "joined_cohort")
  result2 <- result %>%
    dplyr::select(-c("group_level"))

  expect_error(plotSequenceRatio("cdm", "joined_cohort"))
  expect_error(plotSequenceRatio(cdm, "joinedd_cohort"))
  expect_error(plotSequenceRatio(cdm, "joined_cohort", result2))
  expect_error(plotSequenceRatio(cdm, "joined_cohort", result, colours = c("no", "black")))
  expect_error(plotSequenceRatio(cdm, "joined_cohort", result, colours = "red"))
  expect_error(plotSequenceRatio(cdm, "joined_cohort", result, colours = c(3,4)))
  expect_error(plotSequenceRatio(cdm, "joined_cohort", result, plotTitle = 2))
  expect_error(plotSequenceRatio(cdm, "joined_cohort", result, plotTitle = c("1", "2")))
  expect_error(plotSequenceRatio(cdm, "joined_cohort", result, labs = NULL))
  expect_error(plotSequenceRatio(cdm, "joined_cohort", result, labs = c(2,3)))
  expect_error(plotSequenceRatio(cdm, "joined_cohort", result, labs = c("a", "b", "c")))
  expect_error(plotSequenceRatio(cdm, "joined_cohort", result, indexId = 6))
  expect_error(plotSequenceRatio(cdm, "joined_cohort", result, markerId = c("1", "2")))
  expect_error(plotSequenceRatio(cdm, "joined_cohort", result, onlyaSR = 3))
  expect_error(plotSequenceRatio(cdm, "joined_cohort", result, onlyaSR = TRUE, colours = c("red", "blue")))

  CDMConnector::cdmDisconnect(cdm)
})
