source(
  "C:/Users/Megan/Desktop/NEwR-2ed_code_data/NEwR-2ed_code_data/NEwR2-Functions/cleanplot.pca.R"
)
library(ape)
library(ggfortify)
library(vegan)
library(ggrepel)

## load matrices
env <- read.csv("./data/envmatrix.csv") 
rownames(env) <- env$X
env <- env[,-1]

# standardize environmental matrix
# given the diversity of measurements in the environmental matrix, we need
# to standardize

## Standardization of all environmental variables
# Center and scale = standardize the variables (z-scores)
env.z <- decostand(env, "standardize")
apply(env.z, 2, mean) # means = 0
apply(env.z, 2, sd) # standard deviations = 1
# Same standardization using the scale() function (which returns
# a matrix)
env.z <- as.data.frame(scale(env))

env.pca <- rda(env.z, scale = TRUE)

cleanplot.pca(env.pca, scaling = 1, mar.percent = 0.06)
cleanplot.pca(env.pca, scaling = 2, mar.percent = 0.06)

## Species plot

# load matrix
spe.h <- read.csv("./data/phytomatrix_transformed.csv")
rownames(spe.h) <- spe.h$X
spe.h <- spe.h[,-1]

spe.h.meta <- read.csv("./data/PhytoplanktonMatrixMetadata.csv")
rownames(spe.h.meta) <- spe.h.meta$X
spe.h.meta <- spe.h.meta[,-1]



spe.h.pca <-
  rda(spe.h, scale = TRUE) ## THIS NEEDS TO BE DOUBLE CHECKED
# A posteriori projection of environmental variables
spe.h.pcoa.env <- envfit(spe.h.pca, env.z, na.rm = T)

## Create clean biplots
env.vectors <- as.data.frame(spe.h.pcoa.env$vectors$arrows)
sig.parms <-
  c(
    "SRP_ug.L_sed",
    "TDP_ug.L_epi",
    "pH_avg_sed",
    "temp_C_avg_epi",
    "PAR",
    "SRP_ug.L_epi",
    "DO_mg.L_avg_epi",
    "TP_ug.L_epi",
    "pH_avg_epi",
    "temp_C_avg_sed"
  )


#sig.parms <- c("SRP_ug.L_epi", "TDP_ug.L_epi", "TP_ug.L_epi", "pH_avg_epi", "temp_C_avg_epi", "PAR", "temp_C_avg_epi")

env.vectors <- env.vectors[rownames(env.vectors) %in% sig.parms, ]
spe.vectors <- spe.h.pca$CA$v[, 1:2]

#spe.keep <- c("P1041", "P2235", "P4398", "P5508", "P5515", "P6554", "P6558", "P7644")
#spe.vectors <- spe.vectors[rownames(spe.vectors) %in% spe.keep,]

p <- autoplot(
  prcomp(spe.h),
  data = spe.h.meta,
  label = F,
  loadings = F,
  loadings.label = F,
  loadings.label.vjust = 1.2,
  loadings.label.hjust = 1.2,
  colour = "station",
  shape = "station"
) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", col = "grey50") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", col = "grey50") +
  geom_segment(
    data = spe.vectors,
    aes(
      x = 0,
      xend = PC1,
      y = 0,
      yend = PC2
    ),
    colour = "red",
    arrow = arrow(length = unit(0.5, "cm"))
  ) +
  geom_text_repel(
    data = spe.vectors,
    aes(
      x = PC1,
      y = PC2,
      label = rownames(spe.vectors)
    ),
    segment.color = NA,
    col = "red",
    size = 3,
    point.padding = 0.35
  ) +
  #geom_segment(data = env.vectors, aes(x = 0, xend = PC1*0.35, y = 0, yend = PC2*0.35),
  #             colour="blue", arrow = arrow(length = unit(0.5, "cm"))) +
  #geom_text_repel(data = env.vectors,
  #                aes(x = PC1*0.35, y = PC2*0.35, label = rownames(env.vectors)),
  #                size = 3, point.padding = 0.45, segment.color = NA, col = "blue") +
  theme(legend.position = "") +
  geom_path(aes(col = station), size = 1.25) +
  scale_color_viridis_d(end = 0.8) +
  geom_point(size = 5, aes(shape = station), col = "white") +
  geom_point(size = 3, aes(col = station, shape = station)) +
  geom_label_repel(aes(label = DOY, col = station),
                   point.padding = 0.35,
                   size = 3) +
  lims(x = c(-0.7, 0.8), y = c(-0.5, 0.5))


p2 <- autoplot(
  prcomp(spe.h),
  data = spe.h.meta,
  label = F,
  loadings = F,
  loadings.label = F,
  loadings.label.vjust = 1.2,
  loadings.label.hjust = 1.2,
  colour = "station",
  shape = "station"
) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", col = "grey50") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", col = "grey50") +
  
  geom_segment(
    data = env.vectors,
    aes(
      x = 0,
      xend = PC1 * 0.45,
      y = 0,
      yend = PC2 * 0.45
    ),
    colour = "blue",
    arrow = arrow(length = unit(0.5, "cm"))
  ) +
  geom_text_repel(
    data = env.vectors,
    aes(
      x = PC1 * 0.45,
      y = PC2 * 0.45,
      label = rownames(env.vectors)
    ),
    size = 3,
    point.padding = 0.45,
    segment.color = NA,
    col = "blue"
  ) +
  theme(legend.position = "left") +
  geom_path(aes(col = station), size = 1.25) +
  scale_color_viridis_d(end = 0.8) +
  geom_point(size = 5, aes(shape = station), col = "white") +
  geom_point(size = 3, aes(col = station, shape = station)) +
  geom_label_repel(aes(label = DOY, col = station),
                   point.padding = 0.35,
                   size = 3) +
  lims(x = c(-0.7, 0.8), y = c(-0.5, 0.5))

library(gridExtra)
grid.arrange(p, p2, ncol = 2)
plot_grid(p, reposition_legend(p2, "top right"))

ggsave(
  grid.arrange(p, p2, ncol = 2),
  filename = "./supporting-files/fig5_PCABiPlots.tiff",
  width = 7.5,
  height = 3,
  scale = 2,
  units = "in"
)

constraints <- env %>% select(station, DOY) %>% filter(DOY >= 186)
con.spe.rda <- rda(spe.h ~ DOY + station, constraints)
anova.cca(con.spe.rda, permutations = how(nperm = 999))

con.spe.rda <- rda(spe.h ~ ., env.z %>% select(sig.parms))
anova.cca(con.spe.rda, permutations = how(nperm = 999))
