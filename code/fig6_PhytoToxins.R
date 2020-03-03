####
#
#
#
####

# Phytoplankton ----
## Load data
phyto.dat <- read.csv("./data/Conestogo_species_clean.csv") %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"), DOY = yday(date))

phyto.tax <- read.csv("./data/Conestogo_speciestaxonomy_clean.csv") %>%
  select(Access.Code, Phylum, Family, Genus, Species, Description.Code)

# data frames
phyto.merge <- phyto.dat %>%
  left_join(phyto.tax, by = c("species.code"="Access.Code")) %>% # merge taxonomy with counts
  mutate(station = recode_factor(station, "center" = "clc", "east arm" = "cle"),  #fix issues with site factoring
         common.name = paste(Genus, Species, sep = " ")) %>% # create a common name
  filter(date <=as.Date("2017-12-31"), # subset 2017 data
         Description.Code == "PHYT",# remove heterocyst counts
         depth == 2) # only want epi samples

phyto.com <- phyto.merge %>%
  filter(Description.Code == "PHYT") %>%
  group_by(station, date, DOY, Phylum, group) %>%
  dplyr::summarise(total.biomass = sum(biomass.mg.m3), 
                   total.density = sum(density.cells.l))

## Phytoplankton trends
site_labels = c(cle = "Transitional", clc = "Lacustrine")

plot.phyto.com <- phyto.com %>% 
  filter(station != "clw", group != "Euglenophyte") 

plot.phyto.com <- as.data.frame(plot.phyto.com)
plot.phyto.com$group <- droplevels(plot.phyto.com$group)

p <- ggplot(data = plot.phyto.com, 
            aes(x = yday(date), y = total.biomass, fill = group)) +
  annotate("rect", xmin = 173, xmax = 174, ymin = -Inf, ymax = Inf, alpha = 0.6, fill = "grey40")+
  geom_vline(xintercept = yday(as.Date("2017-07-23")), col = "grey40", size = 0.5) +
  geom_area(na.rm = TRUE, position = "stack", col = "white") +
  labs(x = "", y = expression("Biomass (mg m"^-3*")")) + 
  theme(legend.position = "left", legend.background = element_rect(fill = "white"),
        legend.key.size = unit(0.15,"in"), legend.text = element_text(size = 10),
        plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  facet_grid(~ station, labeller = labeller(station = site_labels)) +
  #scale_fill_viridis(discrete = TRUE, option = "D",direction = -1)+
  #scale_color_viridis(discrete = TRUE, option = "D",direction = -1) +
  scale_fill_brewer(palette = "Dark2", guide_legend(title = "")) +
  lims(x = c(170,230)) +
  scale_y_continuous(labels = comma)
p$data$station <- factor(p$data$station, levels = c("cle", "clc"))

p.com <- p
reposition_legend(p, "top left", panel = "panel-1-1", offset = c(0.025, 0.025), plot = T)
print(p.com)

# Toxins ----
tox.raw <- read.csv("./data/Conestogo_toxins-LOQ-MOD.csv") %>%
  mutate(Sampling.date = as.Date(Sampling.date, format = "%Y-%m-%d"), 
         DOY = yday(Sampling.date)) 
#tox.raw %>% group_by(site) %>% filter(MC.LR ==  max(MC.LR))
#tox.raw %>% filter(site == "cle", DOY == 200)
tox.dat <- tox.raw %>%
  select(site, Sampling.date, DOY, Depth..m., MC.LR, MC.RR, MC.YR, AP.A) %>%
  gather(key = toxin, value = concentration, MC.LR:AP.A) %>%
  mutate(concentration = concentration/1000)

tox.dat$concentration[is.na(tox.dat$concentration)] <-0

site_labels <- c(clc = "Lacustrine", cle = "Transitional")

p <- ggplot(data = tox.dat, 
            aes(x = DOY, y = concentration, fill = toxin)) +
  annotate("rect", xmin = 173, xmax = 174, ymin = -Inf, ymax = Inf, alpha = 0.6, fill = "grey40")+
  geom_vline(xintercept = yday(as.Date("2017-07-22")), col = "grey40", size = 0.5) +
  geom_area(na.rm = TRUE, position = "stack", col = "white") + 
  scale_x_continuous(lim = c(170,230), breaks = c(180,200,220), labels = c(180,200,220)) +
  labs(x = "Day of Year (DOY)", 
       y = expression(Toxin~concentration~(mu*g~L^-1))) + 
  theme(legend.position = "bottom", strip.text.x = element_blank(), 
        legend.key.size = unit(0.15,"in"), legend.text = element_text(size = 10),
        plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  facet_wrap(~ site, labeller = labeller(site = site_labels)) +
  #facet_wrap(~ site) +
  scale_fill_viridis(discrete = TRUE, option = "C",direction = 1,
                     guide = guide_legend(title = NULL, ncol = 1))
#scale_color_viridis(discrete = TRUE, option = "C",direction = 1) 
#scale_fill_grey(start = 0.2, end = 0.9, aesthetics = "fill")
#scale_x_date(limits= c(as.Date("2017-07-05"), as.Date("2017-08-17")),
#             date_breaks = "2 weeks", date_labels = "%d %b")

p$data$site<- factor(p$data$site, 
                     levels = c("cle", "clc"))

p.tox <- p

p <- plot_grid(reposition_legend(p.com, 'top left', panel = "panel-1-1", offset = c(0.025,0.01), plot = F),
               reposition_legend(p.tox, 'top left', panel = "panel-1-1", offset = c(0.025,0.025), plot = F), 
               ncol = 1, nrow = 2, align = "hv", axis = c("lr"), labels = "auto")
print(p)

ggsave(plot = p,
       filename = "./supporting-files/Fig6_PhytoToxinTrends.tiff",
       height = 6, width = 7, dpi = 500, device = "tiff", scale = 1.25)

