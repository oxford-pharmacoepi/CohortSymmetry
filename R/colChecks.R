### Credit to Ty - checking if the dataframe has the required columns
colChecks <- function(df, cols) {

  if(!(("data.frame" %in% class(df))|("GeneratedCohortSet" %in% class(df))))
    stop("df input is not a data.frame or a CohortSet object")

  if(!("character" %in% class(cols)))
    stop("col input is not a atomic character vector")

  col_names <- colnames(df)
  cols_found <- cols %in% col_names
  if (!all(cols_found)) {
    stop(paste0(cols[!cols_found], collapse=", "), " not found in input data.frame")
  } else {
    return(TRUE)
  }
}
