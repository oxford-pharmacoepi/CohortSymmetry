### CSR
crudeSequenceRatio <- function(table) {

  n_index_before_marker <- table %>% dplyr::pull(.data$index_first) %>% sum() #how many occasions are there that index was taken before marker
  n_marker_before_index <- table %>% dplyr::pull(.data$marker_first) %>% sum() #how many occasions are there that index was taken after marker

  crudeSequenceRatio <- n_index_before_marker / n_marker_before_index

  return(crudeSequenceRatio)

}
