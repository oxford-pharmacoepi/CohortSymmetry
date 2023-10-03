# CI
getConfidenceInterval <- function(table, confidence_interval_level = 0.025){
  colChecks(table, c("marker_first", "index_first"))

  counts <- tibble(
    index_first = table %>% pull(index_first) %>% sum(.),
    marker_first = table %>% pull(marker_first) %>% sum(.)
  )

  if (counts$index_first == 0 & counts$marker_first == 0){
    counts$lowerCI <- counts$upperCI <- NA
  } else if (counts$index_first == 0){
    counts$index_first <-  0.5
    counts$lowerCI <- qbeta(confidence_interval_level, counts$index_first + 0.5, counts$marker_first + 0.5)
    counts$upperCI <- qbeta(1-confidence_interval_level, counts$index_first + 0.5, counts$marker_first + 0.5)

    counts$lowerCI <- counts$lowerCI/(1-counts$lowerCI)
    counts$upperCI <- counts$upperCI/(1-counts$upperCI)
  } else if (counts$marker_first == 0){
    counts$marker_first <-  0.5
    counts$lowerCI <- qbeta(confidence_interval_level, counts$index_first + 0.5, counts$marker_first + 0.5)
    counts$upperCI <- qbeta(1-confidence_interval_level, counts$index_first + 0.5, counts$marker_first + 0.5)

    counts$lowerCI <- counts$lowerCI/(1-counts$lowerCI)
    counts$upperCI <- counts$upperCI/(1-counts$upperCI)
  } else {
    counts$lowerCI <- qbeta(confidence_interval_level, counts$index_first + 0.5, counts$marker_first + 0.5)
    counts$upperCI <- qbeta(1-confidence_interval_level, counts$index_first + 0.5, counts$marker_first + 0.5)

    counts$lowerCI <- counts$lowerCI/(1-counts$lowerCI)
    counts$upperCI <- counts$upperCI/(1-counts$upperCI)
  }
  return(counts)
}
