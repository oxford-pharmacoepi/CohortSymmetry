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
  result <- summariseSequenceRatio(cohort = cdm$joined_cohort)

  plotSR1 <- plotSequenceRatio(result)
  plotSR2 <- plotSequenceRatio(result, plotTitle = "Test plot")
  plotSR3 <- plotSequenceRatio(result, labs = c("xlab", "ylab"))
  plotSR4 <- plotSequenceRatio(result, colours = c("blue", "green"))
  plotSR5 <- plotSequenceRatio(result, onlyaSR = TRUE, colours = c("orange"))

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
  result <- summariseSequenceRatio(cohort = cdm$joined_cohort)
  result2 <- result %>%
    dplyr::select(-c("group_level"))

  expect_error(plotSequenceRatio("result"))
  expect_error(plotSequenceRatio("result22"))
  expect_error(plotSequenceRatio(result2))
  expect_error(plotSequenceRatio(result, colours = c("no", "black")))
  expect_error(plotSequenceRatio(result, colours = "red"))
  expect_error(plotSequenceRatio(result, colours = c(3,4)))
  expect_error(plotSequenceRatio(result, plotTitle = 2))
  expect_error(plotSequenceRatio(result, plotTitle = c("1", "2")))
  expect_error(plotSequenceRatio(result, labs = NULL))
  expect_error(plotSequenceRatio(result, labs = c(2,3)))
  expect_error(plotSequenceRatio(result, labs = c("a", "b", "c")))
  expect_error(plotSequenceRatio(result, onlyaSR = 3))
  expect_error(plotSequenceRatio(result, onlyaSR = TRUE, colours = c("red", "blue")))

  CDMConnector::cdmDisconnect(cdm)
})
