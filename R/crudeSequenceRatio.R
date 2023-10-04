### CSR
crudeSequenceRatio <- function(table) {

  colChecks(table, c("days_first", "index_first", "marker_first"))

  n_index_before_marker <- table %>% dplyr::pull("index_first") %>% sum() #how many occasions are there that index was taken before marker
  n_marker_before_index <- table %>% dplyr::pull("marker_first") %>% sum() #how many occasions are there that index was taken after marker

  crudeSequenceRatio <- n_index_before_marker / n_marker_before_index

  return(crudeSequenceRatio)

}
