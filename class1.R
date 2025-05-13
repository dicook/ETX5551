library(tourr)
load("data/penguins_sub.rda")
animate_xy(penguins_sub[,1:4])
animate_xy(penguins_sub[,1:4], 
           tour_path = guided_tour(holes()), 
           sphere = TRUE)

animate_xy(penguins_sub[,1:4], 
           col=penguins_sub$species)
set.seed(219)
animate_xy(penguins_sub[,1:4], 
           col=penguins_sub$species, 
           tour_path = guided_tour(
             lda_pp(penguins_sub$species)))
prj <- matrix(c(-0.881,  -0.006, 
              0.254,   0.725,  
              -0.072,  -0.403,  
              0.392,  -0.558), 
                ncol=2, 
                byrow=TRUE)
animate_xy(penguins_sub[,1:4], 
           col=penguins_sub$species, 
           tour_path = radial_tour(prj,
               mvar = 3))
