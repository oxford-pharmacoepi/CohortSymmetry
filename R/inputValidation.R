checkInputGetCohortSequence <- function(cdm,
                                        indexTable,
                                        markerTable,
                                        name,
                                        dateRange,
                                        indexId,
                                        markerId,
                                        daysPriorObservation,
                                        washoutWindow,
                                        indexMarkerGap,
                                        combinationWindow
                                        ) {

  # Check cdm objects, writing schema and index/marker tables
  checkCdm(cdm, tables = c(indexTable, markerTable))

  # Check the format of name
  if(stringr::str_detect(name, "^[a-z0-9_]+$", negate = TRUE)){
    cli::cli_abort(c("name must be given in snake case",
                     "i" = "for example 'my_cohort' is allowed but 'MyCohort' is not"))
  }

  # Check the rest of inputs
  errorMessage <- checkmate::makeAssertCollection()

  ## check name format
  checkmate::assertCharacter(name, len = 1, any.missing = FALSE, add = errorMessage)

  ## Check date
  checkDateRange(dateRange, errorMessage)

  ## Checks that Index and Marker ids exist in Index and Marker tables
  checkCohortIds(cdm, indexTable, indexId, errorMessage)
  checkCohortIds(cdm, markerTable, markerId, errorMessage)

  ## Checks columns in Index and Marker tables
  checkColumns(cdm, indexTable, errorMessage)
  checkColumns(cdm, markerTable, errorMessage)

  ## Check daysPriorObservation
  checkDaysPriorObservation(daysPriorObservation, errorMessage)

  ## Check combinationWindow
  checkCombinationWindow(combinationWindow, errorMessage)

  ## Check indexMarkerGap
  checkIndexMarkerGap(indexMarkerGap, combinationWindow, errorMessage)

  ## Check washoutWindow
  checkWashoutWindow(washoutWindow, errorMessage)

  # Report errors
  checkmate::reportAssertions(collection = errorMessage)
}

checkInputGetSequenceRatios <- function(cdm,
                                        sequenceCohortSet,
                                        confidenceInterval,
                                        restriction) {

  # Check cdm objects, writing schema and index/marker tables
  checkCdm(cdm, tables = sequenceCohortSet)

  # Check the rest of inputs
  errorMessage <- checkmate::makeAssertCollection()

  ## Check confidenceInterval
  checkConfidenceInterval(confidenceInterval, errorMessage)

  ## Check restriction
  checkRestriction(restriction, errorMessage)

  # Report errors
  checkmate::reportAssertions(collection = errorMessage)
}

checkSequenceSymmetry <- function(result) {
  omopgenerics::newSummarisedResult(result)
}

checksFormatSequenceSymmetry <- function(type, crude, adjusted, studyPopulation,
                                         indexName, markerName, cdmName, .options) {
  # Checks
  errorMessage <- checkmate::makeAssertCollection()
  ## Booleans
  for (boolean in c(crude, adjusted, studyPopulation, indexName, markerName, cdmName)) {
    checkSingleBoolean(boolean, errorMessage)
  }
  ## Type
  checkType(type, errorMessage)
  ## .options
  checkOptions(.options, errorMessage)
  # Report errors
  checkmate::reportAssertions(collection = errorMessage)
}

####################################################################
# Check cdm object and index/marker tables
checkCdm <- function(cdm, tables = NULL) {
  if (!isTRUE(inherits(cdm, "cdm_reference"))) {
    cli::cli_abort("cdm must be a CDMConnector CDM reference object")
  }
  if (!is.null(tables)) {
    tables <- tables[!(tables %in% names(cdm))]
    if (length(tables) > 0) {
      cli::cli_abort(paste0(
        "tables: ", tables,
        " are not present in the cdm object"
      ))
    }
  }
  invisible(NULL)
}

# # Check writing schema
# assertWriteSchema <- function(cdm, call = rlang::env_parent()) {
#   if (!("write_schema" %in% names(attributes(cdm)))) {
#     cli::cli_abort(
#       message = "write_schema must be provided in the cdm object to use this function",
#       call = call
#     )
#   }
# }

# Checks Index and Marker ids cohorts
checkCohortIds <- function(cdm, CohortTable, CohortId, errorMessage) {
  checkmate::assertNumeric(CohortId, lower = 1, any.missing = FALSE,
                           null.ok = TRUE, add = errorMessage)
  if (!is.null(CohortId)) {
    ids <- cdm[[CohortTable]] %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::pull()
    if(!isTRUE(all(CohortId %in% ids))){
      errorMessage$push(paste0("Some of the cohort ids given do not exist in ", CohortTable))
    }
  }
}

# Checks columns of Index and Marker tables
checkColumns <- function(cdm, CohortTable, errorMessage) {
  col <- colnames(cdm[[CohortTable]])
  exp_col <- c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")
  if(!isTRUE(all(exp_col %in% col))){
    errorMessage$push(paste0("Some of the expected columns in ", CohortTable,
                             " are missing (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)."))
  }
}

# Check indexMarkerGap (Inf or numeric >=1)
checkIndexMarkerGap <- function(indexMarkerGap, combinationWindow, errorMessage) {
  if (!is.null(indexMarkerGap)) {
    if (indexMarkerGap != Inf) {
      checkmate::assertIntegerish(
        indexMarkerGap,
        lower = 0, any.missing = FALSE, max.len = 4, add = errorMessage
      )
    }
    if (indexMarkerGap > combinationWindow[2]) {
      errorMessage$push("indexMarkerGap cannot be bigger than the second element of combinationWindow.")
    }
  }
}

# Check washoutWindow (Inf or numeric)
checkWashoutWindow <- function(washoutWindow, errorMessage) {
  if (washoutWindow != Inf) {
    checkmate::assertIntegerish(
      washoutWindow,
      lower = 0, any.missing = FALSE, max.len = 4, add = errorMessage
    )
  }
}

# Check restriction (Inf or numeric)
checkRestriction <- function(restriction, errorMessage){
  if (restriction != Inf) {
    checkmate::assertIntegerish(
      restriction,
      lower = 0, any.missing = FALSE, max.len = 10, add = errorMessage
    )
  }
}

# Check daysPriorObservation (has to be numeric)
checkDaysPriorObservation <- function(daysPriorObservation, errorMessage){
  if (daysPriorObservation != Inf) {
    checkmate::assertIntegerish(
      daysPriorObservation,
      lower = 0, any.missing = FALSE, max.len = 4, add = errorMessage
    )
  }
  if(!(is.finite(daysPriorObservation))){
    errorMessage$push("daysPriorObservation has to be finite.")
  }
}

# Check combinationWindow (a numeric of length 2)
checkCombinationWindow <- function(combinationWindow, errorMessage){
  checkmate::assert_numeric(combinationWindow, len = 2, any.missing = FALSE, add = errorMessage)
  if (combinationWindow[1] == Inf) {
   errorMessage$push("The first argument of combinationWindow cannot be infinite.")
  }
  if (combinationWindow[2] != Inf){
    checkmate::assertIntegerish(
      combinationWindow[1],
      lower = 0, any.missing = FALSE, max.len = 4, add = errorMessage
    )
    checkmate::assertIntegerish(
      combinationWindow[2],
      lower = 1, any.missing = FALSE, max.len = 4, add = errorMessage
    )
  }
  if (combinationWindow[1] >= combinationWindow[2]) {
    errorMessage$push("The first argument of combinationWindow must be smaller than the second.")
  }
}

checkDateRange <- function(dateRange, errorMessage) {
  checkmate::assertDate(dateRange, len = 2, add = errorMessage)
  if (all(!is.na(dateRange))) {
    if (dateRange[1] >= dateRange[2]) {
      errorMessage$push("First element in dateRange must be smaller than the second.")
    }
  }
}

checkConfidenceInterval <- function(confidenceInterval, errorMessage) {
  checkmate::assertNumeric(
    confidenceInterval, len = 1,
    lower = 0, upper = 100, any.missing = FALSE, add = errorMessage
  )
}

checkType <- function(type, errorMessage) {
  checkmate::assertCharacter(type, min.chars = 2, max.chars = 6, len = 1,
                             add = errorMessage)
}

checkSingleBoolean <- function(splitGroup, errorMessage) {
  checkmate::assertLogical(splitGroup, any.missing = FALSE, len = 1,
                           add = errorMessage)
}

checkOptions <- function(.options, errorMessage) {
  allowedNames <- names(formatSequenceSymmetryOptions())
  optionsNames <- names(.options)
  checkmate::assertList(.options, null.ok = TRUE, any.missing = TRUE,
                        types = c("numeric", "logical", "character", "list"),
                        add = errorMessage)
  names_id <- optionsNames %in% allowedNames
  if(!all(names_id)) {
    errorMessage$push(
      paste0("The following elements are not supported arguments for .options: ",
             paste0(optionsNames[!names_id], collapse = ", "))
    )
  }
}

