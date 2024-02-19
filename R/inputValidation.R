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
                                        ){

  # Check cdm objects, writing schema and index/marker tables
  checkCdm(cdm, tables=c(indexTable, markerTable))
  assertWriteSchema(cdm)

  # Check the format of name
  if(stringr::str_detect(name, "^[a-z0-9_]+$", negate = TRUE)){
    cli::cli_abort(c("name must be given in snake case",
                     "i" = "for example 'my_cohort' is allowed but 'MyCohort' is not"))
  }

  # Check markerId and indexId
  check_marker_id <- is.numeric(markerId)
  if(!is.null(markerId)){
    if(!isTRUE(check_marker_id)){
      cli::cli_abort("markerId must be of type 'numeric'")
    }
  }

  check_index_id <- is.numeric(indexId)
  if(!is.null(indexId)){
    if(!isTRUE(check_index_id)){
      cli::cli_abort("indexId must be of type 'numeric'")
    }
  }

  # Checks that Index and Marker ids exist in Index and Marker tables
  checkCohortIds(cdm,indexTable,indexId)
  checkCohortIds(cdm,markerTable,markerId)

  # Checks columns in Index and Marker tables
  checkColumns(cdm,indexTable)
  checkColumns(cdm,markerTable)

  errorMessage <- checkmate::makeAssertCollection()
  # check relevant formats of the arguments
  checkmate::assertCharacter(name, len = 1, any.missing = FALSE, add = errorMessage)
  checkmate::expect_date(dateRange, len = 2)
  checkmate::assertCharacter(indexTable, len = 1, any.missing = FALSE, add = errorMessage)
  checkmate::assertCharacter(markerTable, len = 1, any.missing = FALSE, add = errorMessage)

  # Check daysPriorObservation
  checkdaysPriorObservation(daysPriorObservation, errorMessage)
  daysCheck <- all(daysPriorObservation >= 0)
  if (!isTRUE(daysCheck)) {
    errorMessage$push(
      "- daysPriorObservation cannot be negative"
    )
  }

  # check combinationWindow
  checkmate::assert_numeric(combinationWindow, len = 2, any.missing = FALSE, add = errorMessage)
  combinationWindowCheck <- all(combinationWindow >= 0)
  if (!isTRUE(combinationWindowCheck)) {
    errorMessage$push(
      "- one or more value of combinationWindow cannot be negative"
    )
  }

  if (!isTRUE(combinationWindow[2]>combinationWindow[1])) {
    errorMessage$push(
      "- the second argument of combinationWindow has to be larger than the first argument in combinationWindow."
    )
  }

  # Check combinationWindow
  checkcombinationWindow(combinationWindow, errorMessage)

  # Check indexMarkerGap
  checkindexMarkerGap(indexMarkerGap, errorMessage)
  indexMarkerGapCheck <- all(indexMarkerGap >= 0)
  if (!isTRUE(indexMarkerGapCheck)) {
    errorMessage$push(
      "- indexMarkerGap cannot be negative"
    )
  }

  # Check washoutWindow
  checkwashoutWindow(washoutWindow, errorMessage)
  washoutCheck <- all(washoutWindow >= 0)
  if (!isTRUE(washoutCheck)) {
    errorMessage$push(
      "- washoutWindow cannot be negative"
    )
  }

  return(checkmate::reportAssertions(collection = errorMessage))
}

checkInputGetSequenceRatios <- function(cdm,
                                        outcomeTable,
                                        confidenceIntervalLevel,
                                        restriction){
  # Check cdm objects, writing schema and index/marker tables
  checkCdm(cdm, tables=c(outcomeTable))
  assertWriteSchema(cdm)

  errorMessage <- checkmate::makeAssertCollection()
  # check relevant formats of the arguments
  checkmate::assertCharacter(outcomeTable, len = 1, any.missing = FALSE, add = errorMessage)

  # Check confidenceIntervalLevel
  checkconfidenceIntervalLevel(confidenceIntervalLevel, errorMessage)
  daysCheck <- all(confidenceIntervalLevel >= 0)
  if (!isTRUE(daysCheck)) {
    errorMessage$push(
      " - confidenceIntervalLevel cannot be negative"
    )
  }

  # Check restriction
  checkrestriction(restriction, errorMessage)
  restrictionCheck <- all(restriction >= 0)
  if (!isTRUE(restrictionCheck)) {
    errorMessage$push(
      "- restriction cannot be negative"
    )
  }

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

# Check writing schema
assertWriteSchema <- function(cdm, call = rlang::env_parent()) {
  if (!("write_schema" %in% names(attributes(cdm)))) {
    cli::cli_abort(
      message = "write_schema must be provided in the cdm object to use this function",
      call = call
    )
  }
}

# Checks Index and Marker ids cohorts
checkCohortIds <- function(cdm,CohortTable, CohortId) {
  if (!is.null(CohortId)) {
    ids <- cdm[[CohortTable]] %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct()%>%
      dplyr::pull()
    if(!isTRUE(all(CohortId %in% ids))){
      cli::cli_abort(paste0("Some of the cohort ids given do not exist in ", CohortTable))
    }
  }
}

# Checks columns of Index and Marker tables
checkColumns <- function(cdm, CohortTable) {
  col <- colnames(cdm[[CohortTable]])
  exp_col <- c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")
  if(!isTRUE(all(exp_col %in% col))){
    cli::cli_abort(paste0("Some of the expected columns in ", CohortTable, " are missing (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)"))
  }
}


# Check combinationWindow (a numeric of length 2)
checkcombinationWindow <- function(combinationWindow, errorMessage){
  if (combinationWindow[1] == Inf) {
  cli::cli_abort("the first argument of combinationWindow cannot be infinite.")
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
}

# Check indexMarkerGap (Inf or numeric >=1)
checkindexMarkerGap <- function(indexMarkerGap, errorMessage){
  if (!is.null(indexMarkerGap)){
    if (indexMarkerGap != Inf) {
      checkmate::assertIntegerish(
        indexMarkerGap,
        lower = 0, any.missing = FALSE, max.len = 4, add = errorMessage
      )
    }
  }
  }

# Check washoutWindow (Inf or numeric)
checkwashoutWindow <- function(washoutWindow, errorMessage){
  if (washoutWindow != Inf) {
    checkmate::assertIntegerish(
      washoutWindow,
      lower = 0, any.missing = FALSE, max.len = 4, add = errorMessage
    )
  }
}

# Check restriction (Inf or numeric)
checkrestriction <- function(restriction, errorMessage){
  if (restriction != Inf) {
    checkmate::assertIntegerish(
      restriction,
      lower = 0, any.missing = FALSE, max.len = 10, add = errorMessage
    )
  }
}

# Check daysPriorObservation (has to be numeric)
checkdaysPriorObservation <- function(daysPriorObservation, errorMessage){
  if (daysPriorObservation != Inf) {
    checkmate::assertIntegerish(
      daysPriorObservation,
      lower = 0, any.missing = FALSE, max.len = 4, add = errorMessage
    )
  }
  if(!(is.finite(daysPriorObservation))){
    cli::cli_abort("daysPriorObservation has to be finite")
  }
}

# Check confidenceInterval (has to be numeric)
checkconfidenceIntervalLevel <- function(checkconfidenceIntervalLevel, errorMessage){
    checkmate::assertNumeric(
      checkconfidenceIntervalLevel,
      lower = 0, upper = 0.5, any.missing = FALSE, add = errorMessage
    )
}

## Check intersect
checkIntersect <- function(list_input){
  intersection <- c()
  for (i in (1:(length(list_input)-1))){
    for (j in ((i+1):length(list_input))){
      intersection <- c(intersection, intersect(list_input[[i]], list_input[[j]]))
    }
  }
  return(intersection)
}
