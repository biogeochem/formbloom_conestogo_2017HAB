library(tidyverse)
library(lemon)
library(cowplot)

# PAR 
par.data <- read.csv("./data/par_merged.csv",header = T) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         DOY = yday(date)) %>%
  filter(Depth == 2, site != "clw") %>%
  dplyr::select(site, date, DOY, PAR)

site_labels = c(cle = "Transitional", clc = "Lacustrine")
p <- ggplot(data = par.data, aes(x = DOY, y = PAR, col = site, shape = site)) + 
  annotate("rect", xmin = 192, xmax = 222, ymin = 0, ymax = 2, fill  = "grey50", col = "black") +
  annotate("rect", xmin = 197, xmax = 224, ymin = 0, ymax = 2, fill  = "black", col = "black") + # bloom period
  annotate("rect", xmin = 173, xmax = 174, ymin = -Inf, ymax = Inf, fill = "black", alpha = 0.6) +
  geom_vline(xintercept = 203, col = "black", alpha = 0.6) +
  geom_line() +
  geom_point(size = 5, col = "white") +
  geom_point(size = 3) +
  scale_color_viridis_d(begin = 0.3, end = 0.8, direction = -1, "station", labels = site_labels) +
  scale_shape_discrete("station", labels = site_labels) +
  labs(title = paste(expression("Photosynthetically Active Radiation (PAR) at 2 m")), 
       y = expression(PAR~(mu*mol~s^-1~m^-2))) +
  theme_std() + theme(legend.position = "none")

p$data$site <- factor(p$data$site, levels = c("cle","clc"))

# TSS
chem.dat <- read.csv("./data/ConestogoLake_Chemistry_2017-2018.csv") %>%
  mutate(date = as.Date(date), DOY = yday(date)) %>%
  filter(year(date) == 2017, station != "clw", depth_m == 2, parameter == "TSS")

p2 <- ggplot(data = chem.dat, aes(x = yday(date), y = result, col = station, shape = station)) + 
  annotate("rect", xmin = 192, xmax = 222, ymin = 0, ymax = 0.30, fill  = "grey50", col = "black") +
  annotate("rect", xmin = 197, xmax = 224, ymin = 0, ymax = 0.30, fill  = "black", col = "black") + # bloom period
  annotate("rect", xmin = 173, xmax = 174, ymin = -Inf, ymax = Inf, fill = "black", alpha = 0.6) +
  geom_vline(xintercept = 203, col = "black", alpha = 0.6)+
  geom_line() +
  geom_point(size = 5, col = "white") +
  geom_point(size = 3) +
  scale_color_viridis_d(begin = 0.3, end = 0.8,  direction = -1, "station", labels = site_labels) +
  scale_shape_discrete("station", labels = site_labels) +
  #scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
  labs(title = "Total Suspended Solids (TSS) at 2 m", x = "Day of Year", y = "TSS") + 
    theme_std() + theme(axis.title.x = element_blank())
p2$data$station <- factor(p2$data$station, levels = c("cle","clc"))


# merge plots
plot_grid(reposition_legend(p2, "top right", offset = c(0.01,0.01)), p, ncol = 1, align = "hv", labels = "auto")

# save plots
ggsave(plot_grid(reposition_legend(p2, "top right", offset = c(0.01,0.01)), 
                 p, ncol = 1, align = "hv", labels = "auto"), 
       height = 4, width = 3, scale = 2,
       filename = "./supporting-files/Fig4_TSSPAR.tiff", device = "tiff")
