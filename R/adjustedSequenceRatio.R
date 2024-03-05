### ASR
adjustedSequenceRatio <- function(table, movingAverageRestriction) {

  return(crudeSequenceRatio(table) / nullSequenceRatio(table, movingAverageRestriction))

}
