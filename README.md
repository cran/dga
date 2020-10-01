# :dart: dgaFast: Multiple Systems Estimation Using Decomposable Graphical Models

![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)

Efficient re-implementation of the `dga` package of James Johndrow, Kristian Lum and Patrick Ball (2015): "Performs capture-recapture estimation by averaging over decomposable graphical models. This approach builds on Madigan and York (1997)."

## Example usage

Five lists example from Madigan and York (1997) as implemented in the `dga` package:

```r
library(dgaFast) # Re-implements library(dga)

# Number of lists and prior hyperparameter
p = 5
delta = 2^-p
Nmissing <- 1:300 # Reasonable range for the number of unobserved individuals.

# Counts corresponding to list inclusion patterns.
Y = c(0,27,37,19,4,4,1,1,97,22,37,25,2,1,3,5,83,36,34,18,3,5,0,2,30,5,23,8,0,3,0,2)
Y <- array(Y, dim=c(2,2,2,2,2))

# Model-wise posterior probaiblities on the total population size.
# weights[i,j] is the posterior probability for j missing individuals under model graphs5[[j]].
weights <- bma.cr(Y,  Nmissing, delta, graphs5)

# Plot of the posterior distribution.
plotPosteriorN(weights, sum(Y) + Nmissing)
```

## Installation

From GitHub:
```r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("OlivierBinette/dgaFast")
```

## References
- James Johndrow, Kristian Lum and Patrick Ball (2015). dga: Capture-Recapture
  Estimation using Bayesian Model Averaging. R package version 1.2.
  https://CRAN.R-project.org/package=dga
- David Madigan and Jeremy C. York. "Bayesian methods for estimation of the size of a closed population." _Biometrika_. Vol. 84, No. 1 (Mar., 1997), pp.
19-31
