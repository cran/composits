# Modification to tsoutliers from forecast package
# Patricia Menendez
# Date: 15-06-2020
# Change: Definition of extreme outliers

tsoutliers2 <- function (x, iterate = 2, lambda = NULL)
{
  n <- length(x)
  freq <- frequency(x)
  missng <- is.na(x)
  nmiss <- sum(missng)
  if (nmiss > 0L) {
    xx <- forecast::na.interp(x, lambda = lambda)
  }
  else {
    xx <- x
  }
  if (forecast::is.constant(xx)) {
    return(list(index = integer(0), replacements = numeric(0)))
  }
  if (!is.null(lambda)) {
    xx <- forecast::BoxCox(xx, lambda = lambda)
    lambda <- attr(xx, "lambda")
  }
  if (freq > 1 && n > 2 * freq) {
    fit <- forecast::mstl(xx, robust = TRUE)
    rem <- forecast::remainder(fit)
    detrend <- xx - forecast::trendcycle(fit)
    strength <- 1 - var(rem)/var(detrend)
    if (strength >= 0.6) {
      xx <- forecast::seasadj(fit)
    }
  }
  tt <- 1:n
  mod <- supsmu(tt, xx)
  resid <- xx - mod$y
  if (nmiss > 0L) {
    resid[missng] <- NA
  }
  resid.q <- quantile(resid, prob = c(0.25, 0.75), na.rm = TRUE)
  iqr <- diff(resid.q)
  limits <- resid.q + 4 * iqr * c(-1, 1)  # Definition of extreme outliers
  if ((limits[2] - limits[1]) > 1e-14) {
    outliers <- which((resid < limits[1]) | (resid > limits[2]))
  }
  else {
    outliers <- numeric(0)
  }
  x[outliers] <- NA
  x <- forecast::na.interp(x, lambda = lambda)
  if (iterate > 1) {
    tmp <- forecast::tsoutliers(x, iterate = 1, lambda = lambda)
    if (length(tmp$index) > 0) {
      outliers <- sort(unique(c(outliers, tmp$index)))
      x[outliers] <- NA
      x <- forecast::na.interp(x, lambda = lambda)
    }
  }
  return(list(index = outliers, replacements = x[outliers]))
}

