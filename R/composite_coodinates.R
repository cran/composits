#' Computes unconstrained null space coodinates for compositional data.
#'
#' @param x Compositional data in a dataframe or matrix. The rows need to add up to a constrant value
#'
#' @return A list with the following components:
#' \item{\code{y}}{The unconstrained coordinates of the input}
#' \item{\code{vec}}{The basis vectors for the null space coordinates}
#'
#' @examples
#' set.seed(100)
#' n <- 600
#' x <- sample(1:100, n, replace=TRUE)
#' x2 <- sample(1:100, n, replace=TRUE)
#' x3 <- sample(1:100, n, replace=TRUE)
#' x4 <- sample(1:100, n, replace=TRUE)
#' X <- cbind.data.frame(x, x2, x3, x4)
#' X <- X/apply(X, 1, sum)
#' out <- get_coords(X)
#'
#' @export
get_coords <- function(x){
  # x is a data frame which has compositional data
  # i.e rows of x adds up to a constant value
  row_vals <- apply(x, 1, sum)
  if( abs(min(unique(row_vals)) - max(unique(row_vals))) > 10^(-6) ){
    stop("Data is not compositional!")
  }

  x <- as.matrix(x)
  nu <- rep(1, dim(x)[2])/sqrt(dim(x)[2])
  xperp <- x - x %*% nu %*% t(nu)
  B <- pracma::nullspace(t(as.vector(nu)))
  y <- xperp %*% B
  out <- list()
  out$y <- y
  out$vec <- B
  return(out)
}
