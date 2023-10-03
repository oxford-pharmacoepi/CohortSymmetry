### ASR
adjustedSequenceRatio <- function(table) {

  return(crudeSequenceRatio(table) / nullSequenceRatio(table))

}
