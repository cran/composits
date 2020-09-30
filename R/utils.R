# As used in https://cran.r-project.org/web/packages/gridExtra/vignettes/tableGrob.html
find_cell <- function(table, row, col, name="core-fg"){
  l <- table$layout
  which(l$t==row & l$l==col & l$name==name)
}
