### For each i in 1 through length(x), it finds the minimum j such that x[i]-x[j]<=delta.
# x needs to be non decreasing.
# i.e., y[i] = min{j: x[i]-x[j]<=delta}
indexDeltaBackward <- function(x, delta) {

  n <- length(x)
  y <- rep(0, n) #create y which is the same size of x

  y[1] <- 1

  j <- 1
  i <- 2

  while (i <= n) {

    if ((x[i] - x[j]) <= delta) {
      y[i] <- j
      i <- i + 1
    } else {
      j <- j + 1
    }

  }

  return(y)

}
