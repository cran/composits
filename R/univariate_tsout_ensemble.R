#' Performs univariate time series outlier ensemble.
#'
#' @param x A univariate time series as either a \code{ts} object or a vector.
#' @param frequency The frequency associated with the time series
#' @param dates  The dates associated with the time series. This is needed for the package \code{anomalize}. If not explicitly set \code{dates} are set at a frequency 1 ending on the system date.
#'
#' @return A list with the following components:
#' \item{\code{outliers}}{The outliers detected, repeated if detected by multiple outlier methods.}
#' \item{\code{forcastOut}}{The outliers detected R package \code{forecast}.}
#' \item{\code{tsoutliersOut}}{The outliers detected R package \code{tsoutliers}.}
#' \item{\code{otsadOut}}{The outliers detected R package \code{otsad}.}
#' \item{\code{anomalizeOut}}{The outliers detected R package \code{anomalize}.}
#' \item{\code{outmat}}{A matrix containing zeros and ones, with ones representing time points identified as outliers from different methods.}
#'
#'
#' @examples
#' set.seed(100)
#' n <- 500
#' x <- sample(1:100, n, replace=TRUE)
#' x[25] <- 200
#' x[320] <- 270
#' df <- data.frame(timestamp=1:n, value=x)
#' plot(ts(df$value))
#' out <- uv_tsout_ens(x)
#' out
#'
#'@importFrom dplyr %>%
#'@importFrom stats frequency quantile supsmu ts var
#' @importFrom rlang .data
#'
#'@export
uv_tsout_ens <- function(x, frequency=1, dates=NULL){
  if(class(x)=="ts"){
    frequency = frequency(x)
    xts <- x
  }else{
    # x is a vector
    xts <- ts(x, frequency = frequency)
  }

  fcst_o <- tsout_o <- otsad_o <- anomalize_o <- c()
  # OUTLIERS FROM forecast PACKAGE
  tryCatch({
    fcst_out <- tsoutliers2(xts)   # Using modified function with updated outliers definition
    fcst_o <- fcst_out$index
  },error = function(c){
    print("Error in computing forecast-outliers for one component!")
  })

  # OUTLIERS FROM tsoutliers PACKAGE
  tryCatch({
    tsout_out <- tsoutliers::tso(xts, delta = 0.8, maxit = 1, cval.reduce = 0.2, discard.method ="en-masse")  # Modifications. Need to be data dependent
    tsout_o <- tsout_out$outliers$ind    # Adding critical values
  },error = function(c){
    print("Error in computing tsoutliers-tso for one component!")
  })


  # OUTLIERS FROM otsad PACKAGE
  tryCatch({
    otsad_out <- otsad::CpTsSdEwma(data = xts, n.train = 10, m = 20, l=3)         # Change method Reduce training data
    otsad_o <- which(otsad_out$is.anomaly==1)
  },error = function(c){
    print("Error in computing outliers CpTsSdEwma for one component otsad!")
  })

  # OUTLIERS FROM anomalize PACKAGE
  if(is.null(dates)){ # CREATE DATES
    len <- length(x)

    end_dt <- as.Date(Sys.Date())
    start_dt <- end_dt - as.difftime((len-1), units="days")                      # We need to check this: Currently frequency seens to be weekly
    date <- seq(start_dt, end_dt, by="days")
    df <- cbind.data.frame(date, x)
    df[ ,1] <- as.Date(df[ ,1])
  }

  tbl_x <- tibble::as_tibble(df)
  tryCatch({
    anomalize_out <- tbl_x %>%
      anomalize::time_decompose(x) %>%
      anomalize::anomalize(.data$remainder, method = c("gesd"), max_anoms = 0.1) %>%  # Modifying parameters
      anomalize::time_recompose()
    anomalize_o <- which(anomalize_out$anomaly=="Yes")
  },error = function(c){
    print("Error in computing outliers for one component anomalize!")
  })

  outliers <- c(fcst_o, tsout_o, otsad_o, anomalize_o)
  outmat <- matrix(0, nrow=length(x), ncol=4)
  colnames(outmat) <- c("forecast", "tsoutliers", "otsad", "anomalize" )
  if(length(fcst_o)>0){
    outmat[fcst_o, 1] <- 1
  }
  if(length(tsout_o) >0){
    outmat[tsout_o, 2] <- 1
  }
  if(length(otsad_o)>0){
    outmat[otsad_o, 3] <- 1
  }
  if(length(anomalize_o)>0){
    outmat[anomalize_o, 4] <- 1
  }


  result <- list()
  result$outliers <- outliers
  result$forcastOut <- fcst_o
  result$tsoutliersOut <- tsout_o
  result$otsadOut <- otsad_o
  result$anomalizeOut <- anomalize_o
  result$outmat <- outmat
  return(result)
}
