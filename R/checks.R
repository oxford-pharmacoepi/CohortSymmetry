checkInputGetCohortSequence <- function(cdm,
                                        name,
                                        dateRange,
                                        indexTable,
                                        indexId,
                                        markerTable,
                                        markerId,
                                        daysPriorObservation,
                                        indexWashout,
                                        markerWashout,
                                        timeGap,
                                        firstEver) {

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

  # Check timeGap
  checktimeGap(timeGap, errorMessage)
  gapCheck <- all(timeGap >= 0)
  if (!isTRUE(gapCheck)) {
    errorMessage$push(
      "- timeGap cannot be negative"
    )
  }

  # Check indexWashout
  checkindexWashout(indexWashout, errorMessage)
  indexCheck <- all(indexWashout >= 0)
  if (!isTRUE(indexCheck)) {
    errorMessage$push(
      "- indexWashout cannot be negative"
    )
  }

  # Check markerWashout
  checkmarkerWashout(markerWashout, errorMessage)
  markerCheck <- all(markerWashout >= 0)
  if (!isTRUE(markerCheck)) {
    errorMessage$push(
      "- markerWashout cannot be negative"
    )
  }

  checkmate::assert_logical(firstEver,
                            len = 1,
                            add = errorMessage
  )

  return(checkmate::reportAssertions(collection = errorMessage))
}

#######################################################################################

# Check cdm object and index/marker tables
checkCdm <- function(cdm, tables = NULL) {
  if (!isTRUE(inherits(cdm, "cdm_reference"))) {
    cli::cli_abort("cdm must be a CDMConnector CDM reference object")
  }
  if (!is.null(tables)) {
    tables <- tables[!(tables %in% names(cdm))]
    if (length(tables) > 0) {
      cli::cli_abort(paste0(
        "tables: ",
        paste0(tables, collapse = ", "),
        "are not present in the cdm object"
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
      dplyr::select(.data$cohort_definition_id) %>%
      dplyr::distinct()%>%
      dplyr::pull()
    if(!isTRUE(all(CohortId %in% ids))){
      cli::cli_abort(paste0("Some of the cohort ids given do not exist in ", CohortTable ))
    }
  }
}

# Checks columns of Index and Marker tables
checkColumns <- function(cdm, CohortTable) {
  col <- colnames( cdm[[CohortTable]])
  exp_col <- c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")
  if(!isTRUE(all(exp_col %in% col))){
    cli::cli_abort(paste0("Some of the expected columns in ", CohortTable, " are missing (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)" ))
  }
}


# Check timeGap (Inf, NULL or numeric >=1)
checktimeGap <- function(timeGap, errorMessage){
  if (timeGap != Inf) {
  checkmate::assertIntegerish(
    timeGap,
    lower = 1, any.missing = FALSE, max.len = 4, add = errorMessage,
    null.ok = TRUE
  )
  }
}

# Check indexWashout (Inf or numeric)
checkindexWashout <- function(indexWashout, errorMessage){
  if (indexWashout != Inf) {
    checkmate::assertIntegerish(
      indexWashout,
      lower = 0, any.missing = FALSE, max.len = 4, add = errorMessage,
      null.ok = TRUE
    )
  }
}
#
# Check markerWashout (Inf or numeric)
checkmarkerWashout <- function(markerWashout, errorMessage){
  if (markerWashout != Inf) {
    checkmate::assertIntegerish(
      markerWashout,
      lower = 0, any.missing = FALSE, max.len = 4, add = errorMessage,
      null.ok = TRUE
    )
  }
}

# Check daysPriorObservation (has to be numeric)
checkdaysPriorObservation <- function(daysPriorObservation, errorMessage){
  if (daysPriorObservation != Inf) {
    checkmate::assertIntegerish(
      daysPriorObservation,
      lower = 0, any.missing = FALSE, max.len = 4, add = errorMessage,
      null.ok = TRUE
    )
  }
  if(!(is.finite(daysPriorObservation))){
    cli::cli_abort("daysPriorObservation has to be finite")
  }
}

# Check confidenceInterval (has to be numeric)
checkdaysPriorObservation <- function(daysPriorObservation, errorMessage){
  if (daysPriorObservation != Inf) {
    checkmate::assertIntegerish(
      daysPriorObservation,
      lower = 0, any.missing = FALSE, max.len = 4, add = errorMessage,
      null.ok = TRUE
    )
  }
  if(!(is.finite(daysPriorObservation))){
    cli::cli_abort("daysPriorObservation has to be finite")
  }
}
