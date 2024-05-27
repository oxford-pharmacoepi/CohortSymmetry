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
  result <- summariseSequenceRatios(cohort = cdm$joined_cohort)

  plotSR1 <- plotSequenceRatios(result)
  plotSR2 <- plotSequenceRatios(result, plotTitle = "Test plot")
  plotSR3 <- plotSequenceRatios(result, labs = c("xlab", "ylab"))
  plotSR4 <- plotSequenceRatios(result, colours = c("blue", "green"))
  plotSR5 <- plotSequenceRatios(result, onlyaSR = TRUE, colours = c("orange"))

  expect_true("ggplot" %in% (plotSR1 %>% class()))
  expect_true("ggplot" %in% (plotSR2 %>% class()))
  expect_true("ggplot" %in% (plotSR3 %>% class()))
  expect_true("ggplot" %in% (plotSR4 %>% class()))
  expect_true("ggplot" %in% (plotSR5 %>% class()))

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
  result <- summariseSequenceRatios(cohort = cdm$joined_cohort)
  result2 <- result %>%
    dplyr::select(-c("group_level"))

  expect_error(plotSequenceRatios("result"))
  expect_error(plotSequenceRatios("result22"))
  expect_error(plotSequenceRatios(result2))
  expect_error(plotSequenceRatios(result, colours = c("no", "black")))
  expect_error(plotSequenceRatios(result, colours = "red"))
  expect_error(plotSequenceRatios(result, colours = c(3,4)))
  expect_error(plotSequenceRatios(result, plotTitle = 2))
  expect_error(plotSequenceRatios(result, plotTitle = c("1", "2")))
  expect_error(plotSequenceRatios(result, labs = NULL))
  expect_error(plotSequenceRatios(result, labs = c(2,3)))
  expect_error(plotSequenceRatios(result, labs = c("a", "b", "c")))
  expect_error(plotSequenceRatios(result, onlyaSR = 3))
  expect_error(plotSequenceRatios(result, onlyaSR = TRUE, colours = c("red", "blue")))

  CDMConnector::cdmDisconnect(cdm)
})
