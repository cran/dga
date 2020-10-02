#' Function to compute bayesian model averaging
#'
#' @param Y A dataframe 
#' @param Nmissing A sequence of values for the number of unobserved individuals 
#' @param delta A type of prior
#' @param graphs A decomposable graphical model
#' @param logprior A NULL value for the logprior 
#' @param log.prior.model.weights A NULL value for the log prior of the model weights
#' @return Computes a records MMS
#' @export
bma.cr <- function(Y, Nmissing, delta, graphs,
                   logprior = NULL,
                   log.prior.model.weights = NULL) {
  if (is.null(logprior)) {
    logprior <- -log(sum(Y) + Nmissing)
  }

  Y[1] <- 0
  p <- length(dim(Y))
  alpha <- rep(delta, length(Y))

  # Precomputations
  compMat <- MakeCompMatrix(p, delta, Y, Nmissing)
  D <- lgamma(sum(alpha)) - lgamma(Nmissing + sum(Y) + sum(alpha))
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
