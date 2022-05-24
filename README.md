
<!-- README.md is generated from README.Rmd. Please edit that file -->

# composits <a href='https://sevvandi.github.io/composits/'><img src='man/figures/logo.png' align="right" height="138" /></a>

<!-- badges: start -->
[![R-CMD-check](https://github.com/sevvandi/composits/workflows/R-CMD-check/badge.svg)](https://github.com/sevvandi/composits/actions)
<!-- badges: end -->

The goal of *composits* is to find outliers in compositional,
multivariate and univariate time series. It is an outlier ensemble
method that uses the packages `forecast`, `tsoutliers`, `anomalize` and
`otsad`.

## Installation

You can install the released version of composits from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("composits")
```

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("sevvandi/composits")
```

## Example

``` r
library(composits)
set.seed(100)
n <- 600
x <- sample(1:100, n, replace=TRUE)
x[320] <- 300
x2 <- sample(1:100, n, replace=TRUE)
x3 <- sample(1:100, n, replace=TRUE)
X <- cbind.data.frame(x, x2, x3)
x4 <- sample(1:100, n, replace=TRUE)
X <- cbind.data.frame(x, x2, x3, x4)
out <- mv_tsout_ens(X)
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
#> Converting from tbl_df to tbl_time.
#> Auto-index message: index = date
#> frequency = 7 days
#> trend = 91 days
#> Converting from tbl_df to tbl_time.
#> Auto-index message: index = date
#> frequency = 7 days
#> trend = 91 days
#> Converting from tbl_df to tbl_time.
#> Auto-index message: index = date
#> frequency = 7 days
#> trend = 91 days
#> Converting from tbl_df to tbl_time.
#> Auto-index message: index = date
#> frequency = 7 days
#> trend = 91 days
#> Converting from tbl_df to tbl_time.
#> Auto-index message: index = date
#> frequency = 7 days
#> trend = 91 days
#> Converting from tbl_df to tbl_time.
#> Auto-index message: index = date
#> frequency = 7 days
#> trend = 91 days
#> Converting from tbl_df to tbl_time.
#> Auto-index message: index = date
#> frequency = 7 days
#> trend = 91 days
#> Converting from tbl_df to tbl_time.
#> Auto-index message: index = date
#> frequency = 7 days
#> trend = 91 days
#> Converting from tbl_df to tbl_time.
#> Auto-index message: index = date
#> frequency = 7 days
#> trend = 91 days
#> Converting from tbl_df to tbl_time.
#> Auto-index message: index = date
#> frequency = 7 days
#> trend = 91 days
#> Converting from tbl_df to tbl_time.
#> Auto-index message: index = date
#> frequency = 7 days
#> trend = 91 days
#> Converting from tbl_df to tbl_time.
#> Auto-index message: index = date
#> frequency = 7 days
#> trend = 91 days
out$all
#>     Indices Total_Score Num_Coords Num_Methods     DOBIN      PCA       ICA
#> res     320        1.75          3           3 0.3144603 0.728004 0.7075357
#>     forecast tsoutliers otsad anomalize
#> res      0.5        0.5     0      0.75
out$outliers
#>     Indices Total_Score Num_Coords Num_Methods     DOBIN      PCA       ICA
#> res     320        1.75          3           3 0.3144603 0.728004 0.7075357
#>     forecast tsoutliers otsad anomalize
#> res      0.5        0.5     0      0.75
```

See our [website](https://sevvandi.github.io/composits/index.html) or
our paper (Kandanaarachchi et al. 2020) for more examples.

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-composits" class="csl-entry">

Kandanaarachchi, Sevvandi, Patricia Menendez, Ruben Loaiza-Maya, and
Ursula Laa. 2020. “Outliers in Compositional Time Series Data.” Working
Paper.
<https://www.researchgate.net/publication/343712288_Outliers_in_compositional_time_series_data>.

</div>

</div>
