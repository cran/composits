#' Apportions outlier scores to composites.
#'
#' @param obj The output of \code{comp_tsout_ens}
#'
#' @return A list with the following components:
#' \item{\code{scores_out}}{The apportioned scores for outliers for timepoints in \code{mv_tsout_ens$outliers} or \code{comp_tsout_ens$outliers}.}
#' \item{\code{scores_all}}{The apportioned scores for outliers for timepoints in \code{mv_tsout_ens$all} or \code{comp_tsout_ens$all}.}
#'
#' @examples
#' \dontrun{
#' set.seed(1)
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
#' apportioned <- apportion_scores_comp(out)
#' }
#'
#' @export
apportion_scores_comp <- function(obj){
  # obj is a comp_tsout_ens output
  if(is.null(obj$comp_loadings)){
    return('The input needs to be the object returned by the function comp_tsout_ens!')
  }
  # nsp is a matrix with the nullspace basis
  nsp <- obj$comp_loadings
  outmat4D <- obj$outmat4D
  decomp_wts <- obj$decomp_wts
  out_wts <- obj$wts
  outmat3D <- out_wts[1]*outmat4D[ ,1, , ] + out_wts[2]*outmat4D[ ,2, , ] + out_wts[3]*outmat4D[ ,3, , ] + out_wts[4]*outmat4D[ ,4, , ]
  scores_dobin <- outmat3D[ , ,1] %*% diag(decomp_wts[ ,1])
  scores_pca <- outmat3D[ , ,2] %*% diag(decomp_wts[ ,2])
  scores_ics <- outmat3D[ , ,3] %*% diag(decomp_wts[ ,3])
  scores_ica <- outmat3D[ , ,4] %*% diag(decomp_wts[ ,4])

  if(length(obj$dobin_loadings)==1){
    # loadings are NA - probably because of fast = TRUE
    a_dobin <- 0
  }else{
    a_dobin <- nsp%*% obj$dobin_loadings%*% t(scores_dobin)
  }

  if(length(obj$pca_loadings)==1){
    a_pca <- 0
  }else{
    a_pca <- nsp%*% obj$pca_loadings%*% t(scores_pca)
  }

  if(length(obj$ics_loadings)==1){
    a_ics <- 0
  }else{
    a_ics <- nsp%*% obj$ics_loadings%*% t(scores_ics)
  }

  if(length(obj$ica_loadings)==1){
    a_ica <- 0
  }else{
    a_ica <- nsp%*% obj$ica_loadings%*% t(scores_ica)
  }

  a <- abs(a_dobin) + abs(a_pca) + abs(a_ics) + abs(a_ica)

  # Rescaling the value to match the outlier score
  obj$all <- as.data.frame(obj$all)
  inds <- obj$all$Indices
  tscore <- obj$all$Total_Score

  a_ori <- t(a[ ,inds])
  a_ori <- as.data.frame(a_ori)
  rownames(a_ori) <- paste(inds)

  indsout <- obj$outliers$Indices
  if(length(indsout) > 0){
    a_out <- a[ ,indsout]
  }else{
    a_out <- NA
  }
  a_out <- as.data.frame(a_out)
  colnames(a_out) <- paste(indsout)


  out <- list()
  out$scores_out<- a_out
  out$scores_all <- t(a_ori)
  return(out)
}


