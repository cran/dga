\name{MakeCompMatrix}
\alias{MakeCompMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Component-wise Matrix of Log Marginal Likelihoods}
\description{
Calls CompLogML to create a matrix of number of possible components by length(Nmissing) log marginal likelihoods. Calculates the log marginal likehood of each possible marginal table for every value of Nmissing. }
\usage{
MakeCompMatrix(p, delta, Y, Nmissing)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
Number of lists}
  \item{delta}{
Prior hyperparameter of the Dirichlet distribution.}
  \item{Y}{The \code{2^k} matrix of list intersection counts.}
  \item{Nmissing}{
  The vector of possible values for the missing cell. }
}

\value{
A matrix of log marginal likelihoods.
}

\author{
James Johndrow \email{james.johndrow@gmail.com} and Kristian Lum \email{kl@hrdag.org}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Bayesian model averaging}
\keyword{ marginal likelihood }% __ONLY ONE__ keyword per line
