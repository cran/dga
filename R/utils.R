CompLogML <- function(D, delta) {
  out <- rowSums(lgamma(D + delta)) - rowSums(lgamma(D * 0 + delta))
  return(out)
}

integer.base.b <- function(x, b = 2) {
  xi <- as.integer(x)
  if (any(is.na(xi) | ((x - xi) != 0))) {
    print(list(ERROR = "x not integer", x = x))
  }
  N <- length(x)
  xMax <- max(x)
  ndigits <- (floor(logb(xMax, base = 2)) + 1)
  Base.b <- array(NA, dim = c(N, ndigits))
  for (i in 1:ndigits) {
    Base.b[, ndigits - i + 1] <- (x %% b)
    x <- (x %/% b)
  }
  if (N == 1) Base.b[1, ] else Base.b
}

MakeCompMatrix <- function(p, delta, Y, Nmissing) {
  compLMLs <- matrix(0, nrow = 2^p - 1, ncol = length(Nmissing))
  bins <- integer.base.b(1:(2^p - 1), 2)
  for (i in 1:(2^p - 1)) {
    inds <- which(bins[i, ] == 1)
    D <- c(apply(Y, inds, sum))
    Dmat <- t(matrix(D, ncol = length(Nmissing), nrow = length(D)))
    Dmat[, 1] <- Dmat[, 1] + Nmissing

    alpha <- rep(delta * 2^(p - sum(bins[i, ])), ncol(Dmat))
    compLMLs[i, ] <- CompLogML(Dmat, alpha)
  }
  return(compLMLs)
}
