# Code for class 3
library(tidyverse)
library(tourr)
library(mulgar)
library(mclust)
library(detourr)
library(ggthemes)
library(colorspace)
library(crosstalk)
library(plotly)
library(viridis)

load("data/penguins_sub.rda")
animate_xy(penguins_sub[,1:4], col=penguins_sub$sex)

# Model-based clustering
penguins_BIC <- mclustBIC(penguins_sub[,1:4])
ggmc <- ggmcbic(penguins_BIC, cl=2:9, top=7) + 
  scale_color_discrete_divergingx(palette = "Roma") +
  theme_minimal() 
ggmc

# Examine the four cluster solution
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

# Examine the three cluster solution
penguins_mc <- Mclust(penguins_sub[,1:4], 
                      G=6, 
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
           pch=c(4, 20 )[as.numeric(penguins_mc_data$type)], 
           axes="off")

# Comparing solutions
p_dist <- dist(penguins_sub[,1:4])
p_hcw <- hclust(p_dist, method="ward.D2")
p_cl <- data.frame(cl_w = cutree(p_hcw, 3))

penguins_mc <- Mclust(penguins_sub[,1:4], 
                      G=3, 
                      modelNames = "EEE")
p_cl <- p_cl |> 
  mutate(cl_mc = penguins_mc$classification)

p_cl |> 
  count(cl_w, cl_mc) |> 
  pivot_wider(names_from = cl_mc, 
              values_from = n, 
              values_fill = 0) 

p_cl <- p_cl |> 
  mutate(cl_w_j = jitter(cl_w),
         cl_mc_j = jitter(cl_mc))
penguins_cl <- bind_cols(penguins_sub, p_cl)
p_cl_shared <- SharedData$new(penguins_cl)

set.seed(1046)
detour_plot <- detour(p_cl_shared, tour_aes(
  projection = bl:bm,
  colour = cl_mc)) |>
  tour_path(grand_tour(2), 
            max_bases=100, fps = 60) |>
  show_scatter(alpha = 0.7, axes = FALSE,
               width = "100%", height = "450px")

conf_mat <- plot_ly(p_cl_shared, 
                    x = ~cl_mc_j,
                    y = ~cl_w_j,
                    color = ~cl_mc,
                    colors = viridis_pal(option = "D")(3),
                    height = 450) |>
  highlight(on = "plotly_selected", 
            off = "plotly_doubleclick") |>
  add_trace(type = "scatter", 
            mode = "markers")

bscols(
  detour_plot, conf_mat,
  widths = c(5, 6)
)                 

# Clustering a blob
# See the Fritz Leisch slides
risk <- readRDS("data/risk_MSA.rds")
colnames(risk) <- c("Rec", "Hea", "Car", "Fin", "Saf", "Soc")
risk <- as.data.frame(risk)
animate_xy(risk)

risk_d  <- apply(risk, 2, function(x) (x-mean(x))/sd(x))

# Clustering
nc <- 2
set.seed(1145)
r_km <- kmeans(risk_d, centers=nc,
               iter.max = 500, nstart = 5)

r_km_d <- risk_d |>
  as_tibble() |>
  mutate(cl = factor(r_km$cluster)) |>
  bind_cols(model.matrix(~ as.factor(r_km$cluster) - 1)) 
colnames(r_km_d)[(ncol(r_km_d)-nc+1):ncol(r_km_d)] <- paste0("cluster", 1:nc)
r_km_d <- r_km_d |>
  mutate_at(vars(contains("cluster")), function(x) x+1)

animate_xy(r_km_d[,1:6], col=r_km_d$cl)
