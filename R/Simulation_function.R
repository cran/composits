#' Function to simulate compositional time series data
#'
#' @param N The number of categories in the composition
#' @param TT The time series length
#' @param K  The state vector dimension
#' @param A  The N x K matrix of factor loadings in the observation equation
#' @param B  The K x K autoregressive matrix of the transition equation
#' @param C  The K x K matrix determining the magnitude of the persistent outliers
#' @param mu The K-dimensional intercept vector in the transition equation
#' @param D A K x K matrix determining the variance-covariance matrix of the error term
#' @param outliers_discre An R x 3 matrix of discretionary outliers. R denotes the number of discretionary outliers. The first, second and third columns denote the time position, the composite position and the magnitude of the outliers
#' @param q Probability of persistent outlier eventuating
#'
#' @return A list with the following components:
#' \item{\code{datasim}}{A TT x K  data frame with the generated time series compositional data. }
#' \item{\code{outliers_persist}}{A matrix indicating the time location of the persistant outliers (first column) and the factors (or states) where the outlier eventuates (second column). }
#' \item{\code{outliers_discre}}{A matrix equivalent to the function argument provided by the user.}
#' \item{\code{outliers_timeloc}}{A vector with the time location of all the outliers.}
#'
#'
#' @examples
#' set.seed(2000)
#' N <- 30
#' K <- 2
#' TT <- 500
#' A <- matrix(rnorm(N*K, 0, 0.3), N, K)
#' B <- matrix(c(0.8,0,0,0.5), K, K)
#' C <- matrix(c(5,0,0,4), K, K)
#' mu <- c(0.3, 0.7)
#' D <- matrix(c(0.4,0,0,0.4), K, K)
#' outliers_discre <- matrix(c(117, 2, 10, 40, 8, 200), 2, 3, byrow = TRUE)
#' q <- 0.005
#' y <-  Simulations(N = N,
#'                  TT = TT,
#'                  K = K,
#'                  A = A,
#'                  B = B,
#'                  C = C,
#'                  mu = mu,
#'                  D = D,
#'                  outliers_discre = outliers_discre,
#'                  q = q)
#'
#'
#'@export
Simulations <- function(N, TT, K, A, B, C, mu, D, outliers_discre, q){

X            = matrix(NA, TT + 1, N)
r            = matrix(NA, TT + 1, K)
r[1,]        = solve((diag(1, K)-B)) %*% mu
X[1,]        = rep(0,N)
b            = matrix(as.numeric(runif((TT + 1)*K) > 1-q), TT + 1, K)
timeIndex    = which(b == 1, arr.ind = T)

for (t in 2:(TT+1)) {

  r[t,]        = mu +
                 B %*% as.matrix(r[t-1,]) +
                 D %*% as.matrix(rnorm(K,0,1)) +
                 C %*% as.matrix(b[t,])
  X[t,] = A %*% r[t,]
}
timeIndex[,1] = timeIndex[,1] - 1
Gamma         = exp(X[2:(TT + 1),])


ndiscreout = dim(outliers_discre)[1]
for (out in 1:ndiscreout){
  Gamma[outliers_discre[out,1], outliers_discre[out,2]] = outliers_discre[out,3]
}
cumsumGamma                           = rowSums(Gamma)
datasim                               = as.data.frame(t(apply(as.matrix(1:TT),
                                        1,
                                        function (x) Gamma[x,]/cumsumGamma[x]))
                                        )
outliers_timeloc = sort(union(outliers_discre[,1], timeIndex[,1]))
output = list(outliers_persist = timeIndex,
              outliers_discre = outliers_discre,
              outliers_timeloc = outliers_timeloc,
              datasim = datasim)
return(output)
}

