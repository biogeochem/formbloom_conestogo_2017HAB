library(tidyverse)
library(akima)
library(lubridate)
library(padr)
library(gridExtra)
library(pander)
library(RCurl)
library(MBA)
library(reshape)
library(viridis)
library(scales)
library(cowplot)
library(nlme)
library(lemon)
library('vegan'); library(ade4)

# load functions
source("./src/themeformatting.R")
source("./src/sondeinterpolationplot.R")
source("./src/cheminterpolationplot.R")

# set theme
theme_set(theme_std(base_family = "", base_size = 11))

# load data
sonde.dat <- read.csv("./data/Conestogo_SondeProfiles_2017_processed.csv")

# Sonde interpolation plots ----

## Temperature ----
temp.clc <- interp.plot(df = sonde.dat, site.id = "clc", 
                        variable = "temp_C_avg", doy.start = 170, doy.end = 230, 
                        scale.start = 12, scale.end = 26) +
  annotate("rect", xmin = 197, xmax = 224, 
           ymin = 372.5, ymax = 373, fill  = "black") +
  annotate("rect", xmin = 173, xmax = 174, 
           ymin = -Inf, ymax = Inf, fill = "white", alpha = 0.6) +
  geom_vline(xintercept = 205, col = "white", alpha = 1) +
  labs(title = "Lacustrine") +
  theme(axis.title = element_blank(), legend.position = "none")


temp.cle <- interp.plot(df = sonde.dat, site.id = "cle", 
                        variable = "temp_C_avg", doy.start = 170, doy.end = 230, 
                        scale.start = 12, scale.end = 26) +
  annotate("rect", xmin = 192, xmax = 222, 
           ymin = 372.5, ymax = 373, fill  = "grey50", col = "black") +
  annotate("rect", xmin = 173, xmax = 174, 
           ymin = -Inf, ymax = Inf, fill = "white", alpha = 0.6) +
  geom_vline(xintercept = 205, col = "white", alpha = 1) +
  labs(title = "Transitional") +
  theme(axis.title.x = element_blank(), legend.background = element_rect(fill = "grey50"), legend.position = "bottom")


# Dissolved Oxygen ----
do.clc <- interp.plot(df = sonde.dat, site.id = "clc", 
                      variable = "DO_mg.L_avg", doy.start = 170, doy.end = 230,
                      scale.start = 0, scale.end = 17) +
  annotate("rect", xmin = 197, xmax = 224, 
           ymin = 372.5, ymax = 373, fill  = "black") +
  annotate("rect", xmin = 173, xmax = 174, 
           ymin = -Inf, ymax = Inf, fill = "white", alpha = 0.6) +
  geom_vline(xintercept = 205, col = "white", alpha = 1) +
  theme(legend.position = "none", axis.title = element_blank())


do.cle <- interp.plot(df = sonde.dat, site.id = "cle", 
                      variable = "DO_mg.L_avg", doy.start = 170, doy.end = 230, 
                      scale.start = 0, scale.end = 17) +
  annotate("rect", xmin = 192, xmax = 222, 
           ymin = 372.5, ymax = 373, fill  = "grey50", col = "black") +
  annotate("rect", xmin = 173, xmax = 174, 
           ymin = -Inf, ymax = Inf, fill = "white", alpha = 0.6) +
  geom_vline(xintercept = 205, col = "white", alpha = 1) +
  theme(axis.title.x = element_blank(), 
        legend.background = element_rect(fill = "grey50"), 
        legend.position = "bottom")

# Chemistry profiles ----
chem.dat <- read.csv("./data/con_chemistry_profiles.csv", header = T) %>%
  mutate(Date = as.Date(Date))

res.dat <- read.csv("./data/reservoirhistorical.csv", header = T)

res.dat <- res.dat %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2017-06-01"), date <= as.Date("2017-08-17"))

merge.chem.dat <- chem.dat %>%
  left_join(res.dat, by = c("Date" = "date")) %>%
  mutate(ele.depth = elevation.m_mean - Depth, DOY = yday(Date),
         station = Site) 

## TDP ----

vari = "TDP"

clc <- interp.plot.chem(df = merge.chem.dat, site.id = "clc", variable = vari, 
                        doy.start = 170, doy.end = 230,
                        scale.start = 5, scale.end = 100)
clc.tdp <- clc + 
  annotate("rect", xmin = 197, xmax = 224, 
           ymin = 372.5, ymax = 373, fill  = "black") +
  annotate("rect", xmin = 173, xmax = 174, 
           ymin = -Inf, ymax = Inf, fill = "white", alpha = 0.6) +
  geom_vline(xintercept = 205, col = "white", alpha = 0.6) +
  labs(y = "Elevation (m.a.s.l") +
  theme(axis.title = element_blank(), plot.title = element_blank(), legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

cle <- interp.plot.chem(df = merge.chem.dat, site.id = "cle",variable = vari, 
                        doy.start = 170, doy.end = 230,
                        scale.start = 5, scale.end = 100)
cle.tdp <- cle + 
  annotate("rect", xmin = 192, xmax = 222, 
           ymin = 372.5, ymax = 373, fill  = "grey50", col = "black") +
  annotate("rect", xmin = 173, xmax = 174, 
           ymin = -Inf, ymax = Inf, fill = "white", alpha = 0.6) +
  #labs(title = "Transitional (East)") +
  geom_vline(xintercept = 205, col = "white", alpha = 1) +
  theme(axis.title.x = element_blank(), plot.title = element_blank(),
        legend.background = element_rect(fill = "grey50"), legend.position = "bottom",
        plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  scale_fill_viridis(limits = c(5, 100), 
                     direction = 1, discrete = F, option = "D", 
                     name = expression(paste("TDP (",mu*g~L^-1,")")))

## TDN ----

vari = "TDN"

clc <- interp.plot.chem(df = merge.chem.dat, site.id = "clc",variable = vari, 
                        doy.start = 170, doy.end = 230, 
                        scale.start = 2.5, scale.end = 6.5)
clc.tdn <- clc + 
  annotate("rect", xmin = 197, xmax = 224, 
           ymin = 372.5, ymax = 373, fill  = "black") +
  annotate("rect", xmin = 173, xmax = 174, 
           ymin = -Inf, ymax = Inf, fill = "white", alpha = 0.6) +
  geom_vline(xintercept = 205, col = "white", alpha = 1) +
  labs(x ="Day of Year") +
  theme(axis.title.y = element_blank(),
        plot.title = element_blank(), legend.position = "none", 
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

cle <- interp.plot.chem(df = merge.chem.dat, site.id = "cle",variable = vari, 
                        doy.start = 170, doy.end = 230, 
                        scale.start = 2.5, scale.end = 6.5)
cle.tdn <- cle + 
  annotate("rect", xmin = 192, xmax = 222, 
           ymin = 372.5, ymax = 373, fill  = "grey50", col = "black") +
  annotate("rect", xmin = 173, xmax = 174, 
           ymin = -Inf, ymax = Inf, fill = "white", alpha = 0.6) +
  labs(x = "Day of Year") +
  geom_vline(xintercept = 205, col = "white", alpha = 1) +
  theme(plot.title = element_blank(), 
        legend.background = element_rect(fill = "grey50"), 
        legend.position = "bottom", 
        plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  scale_fill_viridis(limits = c(2.5, 6.5), 
                   direction = 1, discrete = F, option = "D", 
                   name = expression(paste("TDN (mg N",L^-1,")")))
# Save plot graphic
ggsave(filename = "./supporting-files/Fig2_waterprofiles.tiff",
       plot_grid(reposition_legend(temp.cle, "bottom left", offset = c(0.045, 0.085), plot = F), temp.clc,
                 reposition_legend(do.cle, "bottom left", offset = c(0.045, 0.085), plot = F), do.clc,
                 reposition_legend(cle.tdp, "bottom left", offset = c(0.045, 0.085), plot = F), clc.tdp,
                 reposition_legend(cle.tdn, "bottom left", offset = c(0.045, 0.085), plot = F), clc.tdn,
                 ncol = 2, align ="hv", labels = "auto", axis = "l"),
       dpi = 300, units = "in", device = "tiff", height = 7, width = 5, scale = 1.6)

