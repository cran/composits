#' Apportions outlier scores to composites.
#'
#' @param obj The output of \code{comp_tsout_ens}
#'
#' @return A list with the following components:
#' \item{\code{scores_out}}{The apportioned outlier scores of selected outliers as per code{mv_tsout_ens}.}
#' \item{\code{scores_all}}{The apportioned outlier scores of all identified outliers.}
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
#' out <- mv_tsout_ens(X, compr=2, fast=FALSE)
#' apportioned <- apportion_scores_mv(out)
#' }
#'
#' @export
apportion_scores_mv <- function(obj){
  # obj is a mv_tsout_ens output
  if(is.null(obj$pca_loadings)){
    return('The input needs to be the object returned by the function mv_tsout_ens!')
  }

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
    a_dobin <- obj$dobin_loadings%*% t(scores_dobin)
  }

  if(length(obj$pca_loadings)==1){
    a_pca <- 0
  }else{
    a_pca <- obj$pca_loadings%*% t(scores_pca)
  }

  if(length(obj$ics_loadings)==1){
    a_ics <- 0
  }else{
    a_ics <- obj$ics_loadings%*% t(scores_ics)
  }

  if(length(obj$ica_loadings)==1){
    a_ica <- 0
  }else{
    a_ica <- obj$ica_loadings%*% t(scores_ica)
  }

  a <- abs(a_dobin) + abs(a_pca) + abs(a_ics) + abs(a_ica)

  # Rescaling the value to match the outlier score
  obj$all <- as.data.frame(obj$all)
  indsout <- obj$outliers$Indices
  inds <- obj$all$Indices
  tscore <- obj$all$Total_Score



  a_all <- t(a[ ,inds])
  a_all <- as.data.frame(a_all)
  rownames(a_all) <- paste(inds)

  a_ori <- a[ ,indsout]
  a_ori <- as.data.frame(a_ori)
  colnames(a_ori) <- paste(indsout)

  out <- list()
  out$scores_out<- a_ori
  out$scores_all <- t(a_all)
  return(out)
}


