#' @title dgaFast: Multiple systems estimation using decomposable graphical models.
#'
#' @description
#' Efficient re-implementation of the \code{dga} package of James Johndrow, Kristian Lum and Patrick Ball (2015): "Performs capture-recapture estimation by averaging over decomposable graphical models. This approach builds on Madigan and York (1997)."
#'
#' @note Stratification functions and Venn diagram plotting functions from the \code{dga} package have not been reproduced in \code{dgaFast}. They can be accessed through \code{install.packages("dga"); library(dga)}.
#'
#' @keywords
#' multiple systems estimation
#' capture-recapture
#' graphical models
#' decomposable graphical models
#' bayesian model averaging
#'
#' @references
#'
#' Madigan, David, and Jeremy C. York. "Bayesian methods for estimation of the size of a closed population."  Biometrika 84.1 (1997): 19-31.
#'
#' James Johndrow, Kristian Lum and Patrick Ball (2015). dga: Capture-Recapture Estimation using Bayesian Model Averaging. R package version 1.2. https://CRAN.R-project.org/package=dga
#'
#' @author Olivier Binette and Rebecca C. Steorts
#'
#' @docType package
#' @name dgaFast
#' @aliases dga-package
#' @import Rcpp
#' @useDynLib dgaFast, .registration=TRUE
"_PACKAGE"
