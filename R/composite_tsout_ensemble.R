#' Performs composite time series outlier ensembling.
#'
#' @inheritParams mv_tsout_ens
#' @return A list with the following components:
#' \item{\code{outliers}}{The outliers detected from the multivariate ensemble after comparing with the comparison time series without outliers. }
#' \item{\code{all}}{All the outliers detected from the multivariate ensemble. }
#' \item{\code{outmat}}{A matrix with outlier scores organised by outlier method.}
#' \item{\code{wts}}{The weights of the outlier detection methods.}
#' \item{\code{pca_loadings}}{The basis vectors from PCA.}
#' \item{\code{dobin_loadings}}{The basis vectors from DOBIN. See R package \code{dobin} for more details.}
#' \item{\code{ics_loadings}}{The basis vectors from ICS. See R package \code{ICS} for more details.}
#' \item{\code{ica_loadings}}{The basis vectors from Independent Component Analysis.}
#' \item{\code{decomp_wts}}{Each decomposition method has several components. For example if \code{ncomp}=2, then there are 2 PC components, 2 DOBIn components, etc ... The weight of each component is given different and depends on the decomposition method. These weights are given in \code{decomp_wts}.}
#' \item{\code{outmat4D}}{A 4D array with outlier scores organised by outlier method, decomposition method, components for each decomposition method and time.}
#'  \item{\code{comp_loadings}}{The unconstrained basis vectors on the simplex.}
#'  \item{\code{comp_coords}}{The unconstrained coordinates of the composite time series data.}
#'
#'
#' @examples
#' \dontrun{
#' set.seed(100)
#' n <- 600
#' x <- sample(1:100, n, replace=TRUE)
#' x[25] <- 200
#' x[320] <- 300
#' x2 <- sample(1:100, n, replace=TRUE)
#' x3 <- sample(1:100, n, replace=TRUE)
#' x4 <- sample(1:100, n, replace=TRUE)
#' X <- cbind.data.frame(x, x2, x3, x4)
#' X <- X/rowSums(X)
#' out <- comp_tsout_ens(X, compr=2, fast=FALSE)
#' }
#'
#' @export
comp_tsout_ens <- function(x, m1 =NULL, ncomp=2,  sds=1, rept=1, compr=2, rat=0.05, fast=TRUE){
  coord_obj <- get_coords(x)
  y <- coord_obj$y
  res <- mv_tsout_ens(y, m1, ncomp,  sds, rept, compr, rat, fast)
  res$comp_loadings <- coord_obj$vec
  res$comp_coords <- coord_obj$y
  return(res)
}
