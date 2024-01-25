### ASR
adjustedSequenceRatio <- function(table, restriction) {

  return(crudeSequenceRatio(table) / nullSequenceRatio(table, restriction))

}
