###
# Phytoplankton site x species matrix
###

phyto.dat <- read.csv("./data/Conestogo_species_clean.csv") %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"), DOY = yday(date))

phyto.tax <- read.csv("./data/Conestogo_speciestaxonomy_clean.csv") %>%
  dplyr::select(Access.Code, Phylum, Family, Genus, Species, Description.Code)

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

# Phytoplankton matrix with Hellinger transformation
## make data into a longform site x species matrix for ordination analyses, generate a site code
phyto.merge.comp.analysis <- phyto.merge %>%
  filter(station != "clw") %>%
  mutate(site.code = paste0(station, DOY)) %>% # create a site code
  select(site.code,
         station,
         date,
         species.code,
         density.cells.l,
         biomass.mg.m3) %>%
  group_by(site.code, station, date, species.code) %>%
  dplyr::summarize_at(vars(density.cells.l, biomass.mg.m3), list(total = sum)) %>%
  droplevels()

phyto.merge.comp.analysis <-
  as.data.frame(phyto.merge.comp.analysis)


# #Which are the taxa that appear only once?
low.counts <- phyto.merge.comp.analysis %>%
  filter(station != "clw") %>%
  roup_by(species.code) %>%
  dplyr::summarize(n = n()) %>% filter(n > 2)
low.count.taxa <- low.counts$species.code

# make a site x species matrix
phyto.merge.comp.analysis.biomass <- phyto.merge.comp.analysis %>%
  filter(species.code %in% low.count.taxa) %>%
  select(site.code, species.code, biomass.mg.m3_total) %>%
  spread(key = species.code, value = biomass.mg.m3_total, fill = 0)

names <- phyto.merge.comp.analysis.biomass$site.code
#phyto.merge.comp.analysis.biomass <- phyto.merge.comp.analysis.biomass[,-(1:3)]
phyto.merge.comp.analysis.biomass <-
  phyto.merge.comp.analysis.biomass[, -(1)]
phyto.merge.comp.analysis.biomass <-
  as.data.frame(phyto.merge.comp.analysis.biomass)
row.names(phyto.merge.comp.analysis.biomass) <- names

#spe <- phyto.merge.comp.analysis.biomass

## remove the low biomass contributors - keep in mind this will affect the outcome if we generate a rel abd matrix or pres/abs matrix
spe <-
  phyto.merge.comp.analysis.biomass[, which(colSums(phyto.merge.comp.analysis.biomass) > 100)]


write.csv(spe, file = "./data/phytomatrix.csv", row.names = FALSE)


# hellinger transformed species data
spe.h <- decostand(spe, "hellinger")

write.csv(spe.h, file = "./data/phytomatrix_transformed.csv", row.names = T)

# create dataframe for sites
site.data <- data.frame(rownames(spe.h)) %>%
  separate(
    rownames.spe.h.,
    into = c("station", "DOY"),
    sep = -3,
    remove = F
  ) %>%
  dplyr::mutate(order = c(seq(1, 7, 1), seq(1, 7, 1))) %>%
  dplyr::rename(site.code = rownames.spe.h.)

spe.h.meta <- spe.h %>%
  mutate(site.code = rownames(spe.h)) %>%
  left_join(site.data, by = c("site.code")) %>%
  mutate(station = as.factor(station))

rownames(spe.h.meta) <- spe.h.meta$site.code

write.csv(spe.h.meta, "./data/PhytoplanktonMatrixMetadata.csv")
