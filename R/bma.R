#' @title Multiple Systems Estimation Using Decomposable Graphical Models
#'
#' @description Compute population size posterior distributions for decomposable graphical models. Given observed list inclusion pattern counts
#'
#' @usage bma.cr(Y, Nmissing, delta, graphs, logprior = NULL, log.prior.model.weights = NULL)
#'
#' @param Y a \code{p}-dimensional array (\code{2^p} elements) of list intersection counts.
#'
#'
#' @export
bma.cr <- function(Y, Nmissing, delta, graphs,
                   logprior = NULL,
                   log.prior.model.weights = NULL) {
  if (is.null(logprior)) {
    logprior <- -log(sum(Y) + Nmissing)
  }
  if (length(delta) == 1) {
    delta = rep(delta, length(Y))
    delta = array(delta, dim=dim(Y))
  }

  Y[1] <- 0
  p <- length(dim(Y))
  s_delta = sum(delta)

  # Precomputations
  compMat <- MakeCompMatrix(p, delta, Y, Nmissing)
  D <- lgamma(s_delta) - lgamma(Nmissing + sum(Y) + s_delta)
  multinomialCoefficient <- lgamma(Nmissing + sum(Y) + 1) - sum(lgamma(Y[-1] + 1)) - lgamma(Nmissing + 1)

  # Compute log posterior for all models
  weights <- computeLogPostProbs(compMat, graphs, D, p)
  rowAdd(weights, multinomialCoefficient)
  rowAdd(weights, logprior)
  if (!is.null(log.prior.model.weights)) colAdd(weights, log.prior.model.weights)

  # Normalization
  expNormalize(weights)

  return(weights)
}
