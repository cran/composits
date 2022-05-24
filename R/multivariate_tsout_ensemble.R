#' Performs multivariate time series outlier ensembling.
#'
#' @param x A  data frame or a matrix object containing a multivariate time series
#' @param m1 Variable indicating dimension reduction methods. Default is set to using all 4 methods: PCA, DOBIN, ICS and ICA.
#' @param ncomp The number of components for each dimension reduction method. Default is set to 2.
#' @param sds The random seed for generating a no-outlier time series.
#' @param rept The number of repetitions for generating a no-outlier time series.
#' @param compr To adjust for multiple testing, the results of the ensemble are compared with the results of a time series without outliers. If \code{compr =1}, a time series is simulated as in \code{simulate_comp_ts} without outliers. If \code{compr = 2}, the top outliers are removed from the outlier series and interpolated values are used for those time points. If \code{compr = 3} both methods of simulation are used for comparison.
#' @param rat A comparison is done with the outliers removed time series. The variable \code{rat} denotes the ratio of outliers to be removed as a proportion of the whole dataset for this comparison.
#' @param fast For faster computation skip ICS decomposition method.
#'
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
#' out <- mv_tsout_ens(X, m1=c(1,2,4), compr=2)
#' }
#'
#' @importFrom stats prcomp rnorm runif
#' @export
mv_tsout_ens <- function(x, m1 =NULL, ncomp=2,  sds=1, rept=1, compr=2, rat=0.05, fast=TRUE){ # wts = NULL,
  d <- dim(x)[2]
  n <- dim(x)[1]
  if(is.null(m1)){
    m1 <- 1:4
  }
  if(fast){
    if(is.null(m1)){
      m1 <- c(1,2,4)
    }else{
      m1 <- intersect(m1, c(1,2,4))
    }
  }
  out1 <- tsout_ensemble(x, m1, ncomp,  sds) # wts,
  res <- out1$all

  if((compr==1)|(compr==3)){
    # thresholds for actual outliers - from no outliers
    noout <- no_outlier_scores_1(d, n, ncomp, sd=123, rept = rept, m1)
    thres1 <- noout$thres1
    thres2 <- noout$thres2
    gap <- thres2 - thres1

    indthres1 <- which(res[ ,2] > thres1)
    outthres1 <- res[indthres1, ]
    outthres1$gapscore1 <- rep(0, length(indthres1))


    stars <-  (outthres1$Total_Score - thres2)/gap
    stars <- ceiling(stars)
    stars[stars < 0] <- 0
    outthres1$gapscore1 <- stars
  }
  if((compr==2)|(compr==3)){
    # Remove outliers over mean + sd from the list and get the outlier scores
    noout2 <- no_outlier_scores_2(x, ncomp, out1, rat=rat, m1)
    thres1 <- noout2$thres1
    thres2 <- noout2$thres2
    if(thres1!=0){
      gap <- thres2 - thres1

      indthres1 <- which(res[ ,2] > thres1)
      outthres2 <- res[indthres1, ]
      outthres2$gapscore2 <- rep(0, length(indthres1))


      stars <-  (outthres2[ ,2] - thres2)/gap
      stars <- ceiling(stars)
      stars[stars < 0] <- 0
      outthres2$gapscore2 <- stars
    }else{
      # thresholds are equal to zero
      # The second comparison method of removing outliers did not produce any outliers to compare with - so return the original data frame
      outthres2 <- res
    }
  }

  if(compr==1){
    outthres <- outthres1
  }else if(compr==2){
    outthres <- outthres2
  }else if(compr==3){
    outthres <- merge(outthres1, outthres2, all = TRUE)
  }

  outthres <- as.data.frame(outthres)
  gp1 <- which(colnames(outthres) %in% c("gapscore1"))
  if(length(gp1)>0){
    colnames(outthres)[gp1] <- "Gap_Score_1"
  }
  gp2 <- which(colnames(outthres) %in% c("gapscore2"))
  if(length(gp2)>0){
    colnames(outthres)[gp2] <- "Gap_Score_2"
  }

  out <- list()
  out$outliers <- as.data.frame(outthres)
  out$all<- res
  out$outmat <- out1$outmat
  out$wts <- out1$wts
  out$pca_loadings <- out1$pca_loadings
  out$dobin_loadings <- out1$dobin_loadings
  out$ics_loadings <- out1$ics_loadings
  out$ica_loadings <- out1$ica_loadings
  out$decomp_wts <- out1$decomp_wts
  out$outmat4D <- out1$outmat4D

  return(out)

}



tsout_ensemble <- function(x, m1 =NULL, ncomp=NULL,  sds=1){  # wts = NULL,
  x <- as.data.frame(x)
  d <- dim(x)[2]
  n <- dim(x)[1]

  if(is.null(m1)){
    m1 <- 1:4
  }
  if(is.null(ncomp)){
    ncomp = 2
  }
  # if(is.null(wts)){
  #   wts <- 1/(1:ncomp)
  # }

  pca_loadings <- dobin_loadings <- ics_loadings <- ica_loadings <- NA
  result <- matrix(0, nrow=n, ncol=length(m1))
  outmat <- matrix(0, nrow=n, ncol=4)
  colnames(outmat) <- c("forecast", "tsoutliers", "otsad", "anomalize" )
  wts_mat <- matrix(0, nrow=ncomp, ncol=4)
  colnames(wts_mat) <- c("dobin", "pca", "ics", "ica")
  outmat4D <- array(0, dim=c(n, 4, ncomp, 4))
  # second index for 4 outlier methods
  # fourth index for 4 coord transformation methods
  for(i in 1:length(m1)){
    j=m1[i]
    obj <- tsout_ensemble_1M(x, pre=m1[i], ncomp )  # , wts
    result[ ,i] <- obj$score
    outmat <- outmat +  obj$outmat
    if(j==1){
      dobin_loadings <- obj$load_vectors
      wts_mat[ ,1] <- obj$wts
      outmat4D[ , , ,1] <- obj$outmat3D
    }else if(j==2){
      pca_loadings <- obj$load_vectors
      wts_mat[ ,2] <- obj$wts
      outmat4D[ , , ,2] <- obj$outmat3D
    }else if(j==3){
      ics_loadings <- obj$load_vectors
      wts_mat[ ,3] <- obj$wts
      outmat4D[ , , ,3] <- obj$outmat3D
    }else if(j==4){
      ica_loadings <- obj$load_vectors
      wts_mat[ ,4] <- obj$wts
      outmat4D[ , , ,4] <- obj$outmat3D
    }
  }
  # Get the weights for the different outlier methods
  wts <- compute_method_weights(outmat)
  # Scale the results by these weights
  # result <- result %*% diag(wts) # I think this needs to be outmat not result

  # Output scaled according to outlier method weights
  outmat2 <- outmat %*% diag(wts)

  result_rowsum <- apply(result, 1, sum)
  outmat_rowsum <- apply(outmat2, 1, sum)

  avg <- apply(outmat2, 1, sum)
  inds <- which(avg > 0)
  # Number of coordinates which found that point as an outlier
  num_comps_all <- apply(result, 1, function(x) sum(x>0))
  num_comps <- num_comps_all[inds]

  result[inds, ] <- result[inds, ]*(outmat_rowsum[inds]/result_rowsum[inds])

  # outmatsum_inds <- which(apply(outmat2, 1, sum) > 0 )
  outmat_positive <- outmat2 > 0
  num_methods_all <- apply( outmat_positive, 1, sum )
  num_methods <- num_methods_all[inds]

  colnames(result) <- c("DOBIN", "PCA", "ICS", "ICA")[m1]

  if(length(inds)==1){
    res <- c(inds, avg[inds], num_comps,  num_methods,  result[inds, ], outmat2[inds, ])
    res <- t(data.frame(res))
  }else{
    res <- cbind.data.frame(inds, avg[inds], num_comps,  num_methods,  result[inds, ], outmat2[inds, ])
  }

  colnames(res) <- c("Indices", "Total_Score", "Num_Coords", "Num_Methods", colnames(result), colnames(outmat) )

  tot_vals <- avg[inds]
  # thres <- mean(tot_vals) + sd(tot_vals)
  # indsgrtr <- which(res[ ,2] > thres)
  #
  # outl <- res[indsgrtr, ]

  out <- list()
  #out$outliers <- outl
  out$all<- res
  out$outmat <- outmat
  out$wts <- wts
  out$pca_loadings <- pca_loadings
  out$dobin_loadings <- dobin_loadings
  out$ics_loadings <- ics_loadings
  out$ica_loadings <- ica_loadings
  out$decomp_wts <- wts_mat
  out$outmat4D <- outmat4D
  return(out)
}


compute_method_weights <- function(outmat){
  wts <- rep(0, 4)
  for(i in 1:4){
    non_zero_inds_i <- which(outmat[ ,i] >0)
    if(length(non_zero_inds_i) >0 ){
      outmat_i <- outmat[ ,-i]
      non_zero_inds_others <- which( apply(outmat_i, 1, sum) > 0 )
      wts[i] <- length(intersect(non_zero_inds_others, non_zero_inds_i))/length(non_zero_inds_i)
    }else{
      wts[i] <- 1
    }
  }
  wts <- wts/sum(wts)
  return(wts)
}
