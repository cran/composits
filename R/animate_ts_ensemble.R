#' Show tour animation of the data points.
#'
#' @param obj The output from \code{comp_tsout_ens} or \code{mv_tsout_ens} functions.
#' @param X The data matrix used as input to \code{mv_tsout_ens} (not needed if \code{obj} is output from \code{comp_tsout_ens}).
#' @param method The dimension reduction method to apply before running the tour (if \code{NULL} tour on the full data space).
#' @param edges Set to "all" to connect points by time index, "outlying" to connect tagged outliers to previous and following points.
#' @param max_frames The maximum number of bases to generate in the grand tour (default is Inf).
#'
#' @examples \donttest{
#' set.seed(100)
#' n <- 600
#' x <- sample(1:100, n, replace=TRUE)
#' x[25] <- 200
#' x[320] <- 300
#' x2 <- sample(1:100, n, replace=TRUE)
#' x3 <- sample(1:100, n, replace=TRUE)
#' x4 <- sample(1:100, n, replace=TRUE)
#' X <- cbind.data.frame(x, x2, x3, x4)
#'
#' animate_ts_ensemble(X = X, max_frames = 10)
#'
#' out1 <- mv_tsout_ens(X, compr=2, fast=FALSE)
#' animate_ts_ensemble(out1, X, max_frames = 10)
#'
#' X <- X/rowSums(X)
#' out2 <- comp_tsout_ens(X, ncomp = 3, compr=2, fast=FALSE)
#' animate_ts_ensemble(out2, method = "dobin", max_frames = 10)
#' }
#'
#' @export
animate_ts_ensemble <- function(obj=NULL, X = NULL, method = NULL, edges = NULL, max_frames = Inf){
  # check that we have the right input
  if (is.null(X)){
    if (is.null(obj$comp_coords)){
      print(paste0("Error: Need either data input X or output from comp_tsout_ens in obj to run."))
      return(NULL)
    }
    X <- obj$comp_coords
  }
  if (!is.null(method)){
    loading_mat <- obj[[paste0(method, "_loadings")]]
    if(! is.matrix(loading_mat)){
      print(paste0("Warning: loadings for method ", method, " not found! Using original data."))
    }
    else{
      X <- as.matrix(X) %*% loading_mat
    }
  }
  if(ncol(X) < 3){
    print(paste0("Error: need at least 3 coordinates for running a tour!"))
    return(NULL)
  }
  if(is.null(colnames(X))){
    coord_name <- ifelse(is.null(method), "X", method)
    colnames(X) <- paste0(coord_name, 1:ncol(X))
  }
  # color for tourr highlights outliers
  col <- rep("black", nrow(X))
  col[obj$outliers[,"Indices"]] <- "red"

  if(!is.null(edges)){
    if(edges == "outlying"){
      idx <- obj$outliers[,"Indices"]
      edges <- matrix(c(idx-1, idx, idx, idx+1), ncol = 2)
    }
    else if(edges == "all"){
      edges <- matrix(c(1:nrow(X)-1, 2:nrow(X)), ncol = 2)
    }
    else edges = NULL
  }
  tourr::animate_xy(X, col=col, edges = edges, axes="bottomleft", max_frames = max_frames)
}
