#' Draws table from comp_tsout_ens or mv_tsout_ens output.
#'
#' @param obj The output from \code{comp_tsout_ens} or \code{mv_tsout_ens} functions.
#' @param uniq_dates An optional parameter to pass in the dates for the dataset.
#' @return Draws a table using R packages \code{grid} and \code{gridExtra}.
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
#' out <- mv_tsout_ens(X, compr=2, fast=FALSE)
#' draw_table(out)
#' }
#'
#' @export
draw_table <- function(obj, uniq_dates=NULL){
  # obj is the output from comp_tsout_ens or mv_tsout_ens
  out <- obj$outliers
  if(!is.null(uniq_dates)){
    out$Indices <- uniq_dates[out$Indices]
  }

  col_names <- colnames(obj$outliers)
  col_coords <- which(col_names %in% c("DOBIN", "PCA", "ICA", "ICS"))
  len_coords <- length(col_coords)
  col_gaps <- which(col_names %in% c("Gap_Score_1", "Gap_Score_2"))
  if(length(col_gaps)>0){
    out1 <- out[ ,c("Indices", col_names[col_coords], "Num_Coords", "forecast", "tsoutliers", "otsad", "anomalize", "Num_Methods", col_names[col_gaps], "Total_Score")]
  }else{
    out1 <- out[ ,c("Indices", col_names[col_coords], "Num_Coords", "forecast", "tsoutliers", "otsad", "anomalize", "Num_Methods", "Total_Score")]
  }

  out2 <- out1[order(out1[ ,"Total_Score"], decreasing=TRUE), ]

  # Round coords and final score to 2 decimals
  for(kk in c(2:dim(out2)[2] )){
    out2[ ,kk] <- round(out2[ ,kk], 2)
  }

  g2 <- gridExtra::tableGrob(out2)


  rows <- cols <- c()

  llcoords <- 2 + len_coords -1
  for(kk in c(2:llcoords)){
    x <- out2[, kk]
    rows <- c(rows, which(x>0) )
    cols <- c(cols, rep(kk, length(which(x>0)) ) )

  }
  rows2 <- rows + 1
  cols2 <- cols + 1

  for(kk in 1:length(rows2)){
    ind1 <- find_cell(g2, rows2[kk], cols2[kk], "core-bg")
    ind2 <- find_cell(g2, rows2[kk], cols2[kk], "core-fg")
    g2$grobs[ind1][[1]][["gp"]] <- grid::gpar(fill="darkolivegreen1", col = "darkolivegreen4", lwd=5)
    g2$grobs[ind2][[1]][["gp"]] <- grid::gpar(fontface="bold")

  }


  rows <- cols <- c()
  llcoords2 <- llcoords + 2
  llcoords3 <- llcoords2 + 3
  for(kk in c(llcoords2:llcoords3)){
    x <- out2[, kk]
    rows <- c(rows, which(x>0) )
    cols <- c(cols, rep(kk, length(which(x>0)) ) )

  }
  rows2 <- rows + 1
  cols2 <- cols + 1

  for(kk in 1:length(rows2)){
    ind1 <- find_cell(g2, rows2[kk], cols2[kk], "core-bg")
    ind2 <- find_cell(g2, rows2[kk], cols2[kk], "core-fg")
    g2$grobs[ind1][[1]][["gp"]] <- grid::gpar(fill="deepskyblue1", col = "dodgerblue", lwd=5)
    g2$grobs[ind2][[1]][["gp"]] <- grid::gpar(fontface="bold")
  }


  ind_max <- 1
  rowsmax <- ind_max + 1
  colsmax <- rep((dim(out)[2]+1), length(rowsmax))


  for(kk in 1:length(ind_max)){
    ind1 <- find_cell(g2, rowsmax[kk], colsmax[kk], "core-bg")
    ind2 <- find_cell(g2, rowsmax[kk], colsmax[kk], "core-fg")
    g2$grobs[ind1][[1]][["gp"]] <- grid::gpar(fill="darksalmon", col = "darkred", lwd=5)
    g2$grobs[ind2][[1]][["gp"]] <- grid::gpar(fontface="bold")

  }
  grid::grid.newpage()
  grid::grid.draw(g2)

}


#' Draws an html table from comp_tsout_ens or mv_tsout_ens output.
#'
#' @inheritParams draw_table
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
#' out <- mv_tsout_ens(X, compr=2, fast=FALSE)
#' draw_table_html(out)
#' }
#'
#' @importFrom rlang .data
#' @export
draw_table_html <- function(obj, uniq_dates=NULL){
  # obj is the output from comp_tsout_ens or mv_tsout_ens
  out <- obj$outliers
  if(!is.null(uniq_dates)){
    out$Indices <- uniq_dates[out$Indices]
  }

  col_names <- colnames(obj$outliers)
  col_coords <- which(col_names %in% c("DOBIN", "PCA", "ICA", "ICS"))
  len_coords <- length(col_coords)
  col_gaps <- which(col_names %in% c("Gap_Score_1", "Gap_Score_2"))
  if(length(col_gaps)>0){
    out1 <- out[ ,c("Indices", col_names[col_coords], "Num_Coords", "forecast", "tsoutliers", "otsad", "anomalize", "Num_Methods", col_names[col_gaps], "Total_Score")]
  }else{
    out1 <- out[ ,c("Indices", col_names[col_coords], "Num_Coords", "forecast", "tsoutliers", "otsad", "anomalize", "Num_Methods", "Total_Score")]
  }

  out2 <- out1[order(out1[ ,"Total_Score"], decreasing=TRUE), ]

  # Round coords and final score to 2 decimals
  for(kk in c(2:dim(out2)[2] )){
    out2[ ,kk] <- round(out2[ ,kk], 2)
  }

  if("ICS" %in% colnames(out2)){
    out2 %>% dplyr::mutate(
     # inds = row.names(.),
      DOBIN = kableExtra::cell_spec(.data$DOBIN, "html", color="black", background = ifelse(.data$DOBIN > 0, "lightgreen", "white")),
      ICA = kableExtra::cell_spec(.data$ICA, "html", color="black", background = ifelse(.data$ICA > 0, "lightgreen", "white")),
      ICS = kableExtra::cell_spec(.data$ICS, "html", color="black", background = ifelse(.data$ICS > 0, "lightgreen", "white")),
      PCA = kableExtra::cell_spec(.data$PCA, "html", color="black", background = ifelse(.data$PCA > 0, "lightgreen", "white")),
      forecast = kableExtra::cell_spec(.data$forecast, "html", color="black", background = ifelse(.data$forecast > 0, "lightblue", "white")),
      tsoutliers = kableExtra::cell_spec(.data$tsoutliers, "html", color="black", background = ifelse(.data$tsoutliers > 0, "lightblue", "white")),
      anomalize = kableExtra::cell_spec(.data$anomalize, "html", color="black", background = ifelse(.data$anomalize > 0, "lightblue", "white")),
      otsad = kableExtra::cell_spec(.data$otsad, "html", color="black", background = ifelse(.data$otsad > 0, "lightblue", "white")),
      Total_Score = kableExtra::cell_spec(.data$Total_Score, "html", color="black", background = ifelse(.data$Total_Score ==max(.data$Total_Score), "pink", "white"))) %>%
      kableExtra::kable(format = "html", escape = F) %>%
      kableExtra::kable_styling("striped", full_width = F) %>%  kableExtra::scroll_box(width = "1000px", height = "400px")
  }else{
    out2 %>% dplyr::mutate(
      # inds = row.names(.data),
      DOBIN = kableExtra::cell_spec(.data$DOBIN, "html", color="black", background = ifelse(.data$DOBIN > 0, "lightgreen", "white")),
      ICA = kableExtra::cell_spec(.data$ICA, "html", color="black", background = ifelse(.data$ICA > 0, "lightgreen", "white")),
      PCA = kableExtra::cell_spec(.data$PCA, "html", color="black", background = ifelse(.data$PCA > 0, "lightgreen", "white")),
      forecast = kableExtra::cell_spec(.data$forecast, "html", color="black", background = ifelse(.data$forecast > 0, "lightblue", "white")),
      tsoutliers = kableExtra::cell_spec(.data$tsoutliers, "html", color="black", background = ifelse(.data$tsoutliers > 0, "lightblue", "white")),
      anomalize = kableExtra::cell_spec(.data$anomalize, "html", color="black", background = ifelse(.data$anomalize > 0, "lightblue", "white")),
      otsad = kableExtra::cell_spec(.data$otsad, "html", color="black", background = ifelse(.data$otsad > 0, "lightblue", "white")),
      Total_Score = kableExtra::cell_spec(.data$Total_Score, "html", color="black", background = ifelse(.data$Total_Score ==max(.data$Total_Score), "pink", "white"))) %>%
      kableExtra::kable(format = "html", escape = F) %>%
      kableExtra::kable_styling("striped", full_width = F) %>%  kableExtra::scroll_box(width = "1150px", height = "200px")
  }
}
