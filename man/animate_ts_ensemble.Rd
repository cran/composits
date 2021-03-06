% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/animate_ts_ensemble.R
\name{animate_ts_ensemble}
\alias{animate_ts_ensemble}
\title{Show tour animation of the data points.}
\usage{
animate_ts_ensemble(
  obj = NULL,
  X = NULL,
  method = NULL,
  edges = NULL,
  max_frames = Inf
)
}
\arguments{
\item{obj}{The output from \code{comp_tsout_ens} or \code{mv_tsout_ens} functions.}

\item{X}{The data matrix used as input to \code{mv_tsout_ens} (not needed if \code{obj} is output from \code{comp_tsout_ens}).}

\item{method}{The dimension reduction method to apply before running the tour (if \code{NULL} tour on the full data space).}

\item{edges}{Set to "all" to connect points by time index, "outlying" to connect tagged outliers to previous and following points.}

\item{max_frames}{The maximum number of bases to generate in the grand tour (default is Inf).}
}
\description{
Show tour animation of the data points.
}
\examples{
\donttest{
set.seed(100)
n <- 600
x <- sample(1:100, n, replace=TRUE)
x[25] <- 200
x[320] <- 300
x2 <- sample(1:100, n, replace=TRUE)
x3 <- sample(1:100, n, replace=TRUE)
x4 <- sample(1:100, n, replace=TRUE)
X <- cbind.data.frame(x, x2, x3, x4)

animate_ts_ensemble(X = X, max_frames = 10)

out1 <- mv_tsout_ens(X, compr=2, fast=FALSE)
animate_ts_ensemble(out1, X, max_frames = 10)

X <- X/rowSums(X)
out2 <- comp_tsout_ens(X, ncomp = 3, compr=2, fast=FALSE)
animate_ts_ensemble(out2, method = "dobin", max_frames = 10)
}

}
