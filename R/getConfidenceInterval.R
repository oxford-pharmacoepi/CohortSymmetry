# CI
getConfidenceInterval <- function(table, nsr, confidenceInterval = 95){

  confidenceIntervalLevel <- (100-confidenceInterval)/200
  counts <- tibble::tibble(
    index_first = table %>% dplyr::pull("index_first") %>% sum(),
    marker_first = table %>% dplyr::pull("marker_first") %>% sum()
  )

  counts$index_first_by_nsr <- counts$index_first/nsr
  counts$marker_first_by_nsr <- counts$marker_first/nsr

  if (counts$index_first == 0 & counts$marker_first == 0){
    counts$lowerCSR_CI <- counts$upperCSR_CI <- NA
    counts$lowerASR_CI <- counts$upperASR_CI <- NA
  } else if (counts$index_first == 0){
    counts$index_first <-  0.5
    counts$lowerCSR_CI <- stats::qbeta(confidenceIntervalLevel, counts$index_first + 0.5, counts$marker_first + 0.5)
    counts$upperCSR_CI <- stats::qbeta(1-confidenceIntervalLevel, counts$index_first + 0.5, counts$marker_first + 0.5)

    counts$lowerCSR_CI <- counts$lowerCSR_CI/(1-counts$lowerCSR_CI)
    counts$upperCSR_CI <- counts$upperCSR_CI/(1-counts$upperCSR_CI)

    counts$lowerASR_CI <- counts$lowerCSR_CI/nsr
    counts$upperASR_CI <- counts$upperCSR_CI/nsr

  } else if (counts$marker_first == 0){
    counts$marker_first <-  0.5
    counts$lowerCSR_CI <- stats::qbeta(confidenceIntervalLevel, counts$index_first + 0.5, counts$marker_first + 0.5)
    counts$upperCSR_CI <- stats::qbeta(1-confidenceIntervalLevel, counts$index_first + 0.5, counts$marker_first + 0.5)

    counts$lowerCSR_CI <- counts$lowerCSR_CI/(1-counts$lowerCSR_CI)
    counts$upperCSR_CI <- counts$upperCSR_CI/(1-counts$upperCSR_CI)

    counts$lowerASR_CI <- counts$lowerCSR_CI/nsr
    counts$upperASR_CI <- counts$upperCSR_CI/nsr

  } else {
    counts$lowerCSR_CI <- stats::qbeta(confidenceIntervalLevel, counts$index_first + 0.5, counts$marker_first + 0.5)
    counts$upperCSR_CI <- stats::qbeta(1-confidenceIntervalLevel, counts$index_first + 0.5, counts$marker_first + 0.5)

    counts$lowerCSR_CI <- counts$lowerCSR_CI/(1-counts$lowerCSR_CI)
    counts$upperCSR_CI <- counts$upperCSR_CI/(1-counts$upperCSR_CI)

    counts$lowerASR_CI <- counts$lowerCSR_CI/nsr
    counts$upperASR_CI <- counts$upperCSR_CI/nsr
  }
  return(counts)
}
