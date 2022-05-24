## ----setup0, include = FALSE--------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=8, fig.height=6
)

## ----load2, echo=FALSE, eval=TRUE, message=FALSE------------------------------
if (!requireNamespace("composits", quietly = TRUE)) {
    stop("Package composits is needed for the vignette. Please install it.",
      call. = FALSE)
}
if (!requireNamespace("forecast", quietly = TRUE)) {
    stop("Package forecast is needed for the vignette. Please install it.",
      call. = FALSE)
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package ggplot2 is needed for the vignette. Please install it.",
      call. = FALSE)
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package dplyr is needed for the vignette. Please install it.",
      call. = FALSE)
}
if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package tidyr is needed for the vignette. Please install it.",
      call. = FALSE)
}
if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package stringr is needed for the vignette. Please install it.",
      call. = FALSE)
}
if (!requireNamespace("broom", quietly = TRUE)) {
    stop("Package broom is needed for the vignette. Please install it.",
      call. = FALSE)
}
if (!requireNamespace("maptools", quietly = TRUE)) {
    stop("Package maptools is needed for the vignette. Please install it.",
      call. = FALSE)
}

## ----setup--------------------------------------------------------------------
library(composits)
library(ggplot2)
library(forecast)
library(dplyr)
library(tidyr)
library(stringr)
library(broom)
library(maptools)

## ----univaraite1--------------------------------------------------------------
gold2 <- forecast::na.interp(gold)
out <- uv_tsout_ens(gold2)
inds <- names(which(table(out$outliers) > 2))

ts_gold <- dplyr::as_tibble(gold2) %>% mutate(t = 1:length(gold2)) %>% rename(value = x)
ggplot(ts_gold, aes(x=t, y=value)) +
  geom_line() +
  geom_vline(xintercept =as.numeric(inds), color="red", alpha=0.8, size=0.5, linetype ='dashed') + ylab("Gold prices") + 
  theme_bw()

## ----univariate2--------------------------------------------------------------
out <- uv_tsout_ens(woolyrnq)
inds <- names(which(table(out$outliers) > 2))

ts_wool <- dplyr::as_tibble(woolyrnq) %>% mutate(t = 1:length(woolyrnq)) %>% rename(value = x)
ggplot(ts_wool, aes(x=t, y=value)) +
  geom_line() +
  geom_vline(xintercept =as.numeric(inds), color="red", alpha=0.8, size=0.5, linetype ='dashed') + ylab("Woollen Yarn Production") + 
  theme_bw()

## ----multivariate1------------------------------------------------------------
stpart <- EuStockMarkets[1:600, ]
stpart <- EuStockMarkets[1:600, ]
as_tibble(stpart) %>% mutate(t = 1:n())   %>%
  pivot_longer(cols=1:4)  %>%
  ggplot2::ggplot( ggplot2::aes(x = t, y = value, color = name)) +  ggplot2::geom_line() +  ggplot2::theme_bw() 

## ----mvout1-------------------------------------------------------------------
out <- mv_tsout_ens(stpart, fast=TRUE)
out$outliers
draw_table_html(out)

## ----mvout2-------------------------------------------------------------------
plot_decomposed_all(obj=out, X = stpart)

## ----mvani1, eval=F, echo=T---------------------------------------------------
#  animate_ts_ensemble(out, X= stpart,  max_frames = 100)

## ----mvani2, eval=F, echo=F---------------------------------------------------
#  library(animation)
#  saveGIF(
#    animate_ts_ensemble(out, X= stpart,  max_frames = 100),
#    movie.name = "mvani.gif", interval=0.2)

## ----mvout3-------------------------------------------------------------------
plot_decomposed(obj=out, X = stpart,  method = "pca")

## ----composite1---------------------------------------------------------------
data('spanish_morte')
df <- spanish_morte[[1]]
uniq_dates <- spanish_morte[[2]]
df2 <- cbind.data.frame(uniq_dates, df)
as_tibble(df2) %>%
  pivot_longer(cols=2:20)  %>%
  ggplot2::ggplot( ggplot2::aes(x = uniq_dates, y = value, color = name)) +  ggplot2::geom_line() +  ggplot2::theme_bw() 

## ----composite2---------------------------------------------------------------
out <- comp_tsout_ens(df, fast=FALSE)
out$outliers
draw_table_html(out, uniq_dates)

## ----composite3---------------------------------------------------------------
plot_decomposed_all(obj=out, X = df)
# animate_ts_ensemble(out, X= df,  max_frames = 1)

## ----compani1, eval=F, echo=T-------------------------------------------------
#  animate_ts_ensemble(out, X= df,  max_frames = 100)

## ----compani2, eval=F, echo=F-------------------------------------------------
#  library(animation)
#  saveGIF(
#    animate_ts_ensemble(out, X= df,  max_frames = 100),
#    movie.name = "compani.gif", interval=0.2)

## ----composite4---------------------------------------------------------------
apportioned <- apportion_scores_comp(out)
apportioned1 <- cbind.data.frame(colnames(df), apportioned$scores_out)
colnames(apportioned1)[1] <- 'reg'
colnames(apportioned1)[2:dim(apportioned1)[2]] <-  paste(uniq_dates[out$outliers$Indices])
apportioned1

## ----composite5---------------------------------------------------------------
shapefile_ccaa <- readShapePoly("ComunidadesAutonomas_ETRS89_30N/Comunidades_Autonomas_ETRS89_30N.shp", proj4string=CRS("+proj=longlat")) 
data_ccaa <- tidy(shapefile_ccaa)
nombres_ccaa <- tibble(shapefile_ccaa$Texto) %>% 
  mutate(id = as.character(seq(0, nrow(.)-1)))

reg <- c("AN", "AR", "AS", "PM", "CN", "CB", "CL", "CM",
         "CT", "VC", "EX", "GA", "MD", "MU", "NA", "PV",
         "LO", "CE", "CE")

nombres_ccaa <- tibble(reg) %>% 
  mutate(id = as.character(seq(0, nrow(.)-1)))

data_ccaa_mapa <- data_ccaa %>% 
  left_join(nombres_ccaa, by = "id")


df3 <- apportioned1 %>%
mutate(reg = str_replace(reg, "RI", "LO")) %>%
  mutate(reg = str_replace(reg, "NC", "NA")) %>%
  mutate(reg = str_replace(reg, "MC", "MU")) %>%
  pivot_longer(-reg, names_to = "date", values_to = "score")

sp_data <- inner_join(data_ccaa_mapa, df3, by="reg") %>%
  filter(reg != "CN")

sp_data %>%
  ggplot() +
  geom_polygon(aes(x= long, y = lat, group=group, fill=score),
               color="black", size=0.2) +
  theme_bw() +
  scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  xlab("") +
  ylab("") +
  coord_fixed() +
  facet_wrap(~date, ncol=5) +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())


