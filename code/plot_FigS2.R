phyto.dat <- read.csv("../database_Conestogo/clean_data/taxonomy/Conestogo_species_clean.csv") %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"), DOY = yday(date))

phyto.tax <- read.csv("../database_Conestogo/clean_data/taxonomy/Conestogo_speciestaxonomy_clean.csv") %>%
  select(Access.Code, Phylum, Family, Genus, Species, Description.Code)

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
  dplyr::summarize(totalphylum.biomass = sum(biomass.mg.m3), 
                   totalphylum.density = sum(density.cells.l)) %>%
  group_by(station, DOY) %>%
  mutate(total.biomass = sum(totalphylum.biomass), 
            total.density = sum(totalphylum.density),
         percbiomass = totalphylum.biomass/total.biomass)

site_labels = c(cle = "Transitional (East)", clc = "Lacustrine (Center)")
p <- ggplot(data = phyto.com %>% filter(Phylum == "Cyanophyta", station != "clw"), 
       aes(x = DOY, y = percbiomass, 
           col = station, shape = station, fill = station)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_grey(labels = site_labels) +
  scale_fill_grey(labels = site_labels) +
  scale_shape_discrete(labels = site_labels) +
  geom_hline(yintercept= 0.1, col = "red", size = 2) +
  geom_hline(yintercept= 0.5, col = "red", size = 1.5) +
  geom_hline(yintercept= 0.8, col = "red", size = 1) +
  geom_line() +
  geom_point() +
  theme_std() + theme(legend.position ="bottom") +
  labs(x = "day of year", y = "Percent Cyanobacteria Biomass")

ggsave(p, filename = "./supporting-files/SFig2_percentbiomass.tiff",
       height = 2.5, width = 2.5, scale = 1.6, device = "tiff")  
