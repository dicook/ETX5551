# Code for class 2
library(tidyverse)
library(tourr)
library(GGally)
library(mulgar)

# Hiding things
set.seed(946)
d <- tibble(x1=runif(200, -1, 1), 
            x2=runif(200, -1, 1), 
            x3=runif(200, -1, 1))
d <- d |>
  mutate(x4 = x3 + runif(200, -0.1, 0.1))
# outlier is visible in d
d <- bind_rows(d, c(x1=0, x2=0, x3=-0.5, x4=0.5))
ggscatmat(d)

# Point is hiding in d_r
d_r <- d |>
  mutate(x1 = cos(pi/6)*x1 + sin(pi/6)*x3,
         x3 = -sin(pi/6)*x1 + cos(pi/6)*x3,
         x2 = cos(pi/6)*x2 + sin(pi/6)*x4,
         x4 = -sin(pi/6)*x2 + cos(pi/6)*x4)
ggscatmat(d_r)
animate_xy(d_r)

# Model-in-the-data-space
data(plane)
plane_pca <- prcomp(plane)
plane_m <- pca_model(plane_pca)
plane_m_d <- rbind(plane_m$points, plane)
animate_xy(plane_m_d, edges=plane_m$edges,
           axes="bottomleft",
           edges.col="#E7950F",
           edges.width=3)

data(aflw)
aflw_std <- aflw |>
  mutate_if(is.numeric, function(x) (x-
                                       mean(x, na.rm=TRUE))/
              sd(x, na.rm=TRUE))
aflw_pca <- prcomp(aflw_std[,7:35], 
                   scale = FALSE, 
                   retx=TRUE)
aflw_model <- pca_model(aflw_pca, d=4, s=1)
aflw_all <- rbind(aflw_model$points, aflw_std[,7:35])
animate_xy(aflw_all, edges=aflw_model$edges,
           edges.col="#E7950F", 
           edges.width=3, 
           half_range=6, 
           axes="off")

# Nonlinear dimension reduction and linking plots
library(Rtsne)
library(uwot)
library(crosstalk)
library(plotly)
library(detourr)

set.seed(44)
cnl_tsne <- Rtsne(clusters_nonlin)
cnl_umap <- umap(clusters_nonlin)
umap_df <- data.frame(umapX = cnl_umap[, 1],
                      umapY = cnl_umap[, 2])
cnl_df <- bind_cols(clusters_nonlin, umap_df)
shared_cnl <- SharedData$new(cnl_df)

detour_plot <- detour(shared_cnl, tour_aes(
  projection = starts_with("x"))) |>
  tour_path(grand_tour(2), 
            max_bases=50, fps = 60) |>
  show_scatter(alpha = 0.7, axes = FALSE,
               width = "100%", height = "450px")

umap_plot <- plot_ly(shared_cnl,
                     x = ~umapX, 
                     y = ~umapY,
                     color = I("black"),
                     height = 450) %>%
  highlight(on = "plotly_selected", 
            off = "plotly_doubleclick") %>%
  add_trace(type = "scatter", 
            mode = "markers")

bscols(
  detour_plot, umap_plot,
  widths = c(5, 6)
)

# p-values
library(nullabor)
library(MASS)
data(wasps)
wasps_lineup <- lineup(null_permute('Group'),
                        wasps[,-1], n=12) |>
   as_tibble()
wasps_lineup_lda <- NULL
for (i in 1:12) {
  x <- wasps_lineup[wasps_lineup$.sample == i,]
  d <- predict(lda(Group~.,
                   data=x[,-43]))$x[,1:2] |>
    as_tibble() |>
    mutate(Group = x$Group)
  wasps_lineup_lda <- rbind(wasps_lineup_lda, d)
}
wasps_lineup_lda <- bind_cols(wasps_lineup_lda, wasps_lineup[,-1])
ggplot(wasps_lineup_lda, aes(x=LD1, y=LD2,
                              colour=Group)) +
   geom_point() +
   facet_wrap(~.sample, ncol=4) +
   scale_colour_brewer(palette="Dark2") +
   theme(legend.position="none")

# Model-based clustering
library(mclust)
load("data/penguins_sub.rda")
animate_xy(penguins_sub[,1:4])
animate_xy(penguins_sub[,1:4], col=penguins_sub$species)

# Four clusters
penguins_mc <- Mclust(penguins_sub[,1:4], 
                      G=4, 
                      modelNames = "VEE")
penguins_mce <- mc_ellipse(penguins_mc)
penguins_cl <- penguins_sub
penguins_cl$cl <- factor(penguins_mc$classification)

penguins_mc_data <- penguins_cl |>
  select(bl:bm, cl) |>
  mutate(type = "data") |>
  bind_rows(bind_cols(penguins_mce$ell,
                      type=rep("ellipse",
                               nrow(penguins_mce$ell)))) |>
  mutate(type = factor(type))

animate_xy(penguins_mc_data[,1:4],
           col=penguins_mc_data$cl,
           pch=c(4, 20 )[as.numeric(penguins_mc_data$type)], 
           axes="off")

# Three clusters
penguins_mc <- Mclust(penguins_sub[,1:4], 
                      G=3, 
                      modelNames = "EEE")
penguins_mce <- mc_ellipse(penguins_mc)
penguins_cl <- penguins_sub
penguins_cl$cl <- factor(penguins_mc$classification)

penguins_mc_data <- penguins_cl |>
  select(bl:bm, cl) |>
  mutate(type = "data") |>
  bind_rows(bind_cols(penguins_mce$ell,
                      type=rep("ellipse",
                               nrow(penguins_mce$ell)))) |>
  mutate(type = factor(type))

animate_xy(penguins_mc_data[,1:4],
           col=penguins_mc_data$cl,
           pch=c(4, 20)[as.numeric(penguins_mc_data$type)], 
           axes="off")
