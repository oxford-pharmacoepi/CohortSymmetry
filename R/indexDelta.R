indexDelta <- function(x, delta, direction){
  ### For each i in 1 through length(x), it finds the max j such that x[j]-x[i]<=delta.
  # x needs to be non decreasing.
  # i.e., y[i] = max{j: x[j]-x[i]<=delta}
  if (direction == "Forward"){
    n <- length(x)
    y <- rep(0, n)

    j <- 2
    i <- 1

    while (j <= n) {

      if ((x[j] - x[i]) > delta) {
        y[i] <- j - 1
        i <- i + 1
      } else {
        j <- j + 1
      }

    }

    y[i:n] <- n

    return(y)
  }
  ### For each i in 1 through length(x), it finds the minimum j such that x[i]-x[j]<=delta.
  # x needs to be non decreasing.
  # i.e., y[i] = min{j: x[i]-x[j]<=delta}
  if (direction == "Backward"){
    n <- length(x)
    y <- rep(0, n)

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
}
