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
checktimeGap <- function(timeGap){
  if (timeGap != Inf) {
  checkmate::assertIntegerish(
    timeGap,
    lower = 1, any.missing = FALSE, max.len = 4,
    null.ok = TRUE
  )
  }
}

# Check indexWashout (Inf or numeric)
checkindexWashout <- function(indexWashout){
  if (indexWashout != Inf) {
    checkmate::assertIntegerish(
      indexWashout,
      lower = 0, any.missing = FALSE, max.len = 4,
      null.ok = TRUE
    )
  }
}
#
# Check markerWashout (Inf or numeric)
checkmarkerWashout <- function(markerWashout){
  if (markerWashout != Inf) {
    checkmate::assertIntegerish(
      markerWashout,
      lower = 0, any.missing = FALSE, max.len = 4,
      null.ok = TRUE
    )
  }
}

# Check daysPriorObservation (has to be numeric)
checkdaysPriorObservation <- function(daysPriorObservation){
  if (daysPriorObservation != Inf) {
    checkmate::assertIntegerish(
      daysPriorObservation,
      lower = 0, any.missing = FALSE, max.len = 4,
      null.ok = TRUE
    )
  }
  if(!(is.finite(daysPriorObservation))){
    cli::cli_abort("daysPriorObservation has to be finite")
  }
}
