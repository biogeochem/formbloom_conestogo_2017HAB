###
# Build an environmnetal matrix
##

library(tidyverse)

# Environmental ----
con.dat <-
  read.csv(
    "./data/con_met_data.csv",
    header = T
  ) %>%
  mutate(Date = as.Date(Date, format = "%d-%b-%y")) %>%
  select(Site:Secchi)

#head(con.dat)

res.dat <-
  read.csv(
    "./data/reservoirhistorical.csv",
    header = T
  ) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  select(date:wind.speed.km.hr_mean, precip.mm_sum, res.time)

## Create merged data file
res.dat.merged <- con.dat %>%
  left_join(res.dat, by = c("Date" = "date"))

# sonde data for each site and date
sonde.dat <-
  read.csv(
    "./data/Conestogo_SondeProfiles_2017_processed.csv",
    header = T
  )

# create a epi dataset
sonde.epi <- sonde.dat %>%
  filter(depth.adj.new == 2) %>%
  select(station:depth.adj.new, temp_C_avg:pH_avg) %>%
  gather(key = parameter, value = result, temp_C_avg:pH_avg) %>%
  mutate(parameter = paste0(parameter, "_epi"))

sonde.sed <- sonde.dat %>%
  group_by(station, date) %>%
  slice(n()) %>%
  select(station:depth.adj.new, temp_C_avg:pH_avg) %>%
  gather(key = parameter, value = result, temp_C_avg:pH_avg) %>%
  mutate(parameter = paste0(parameter, "_sed"))

# lake physical characteristics at each site
lake.phys <-
  read.csv(
    "./data/Conestogo_WaterColumnMetrics_2017.csv",
    header = T
  ) %>%
  select(station, datetime, thermo.depth, top, bottom, secchi_m) %>%
  mutate(datetime = as.Date(datetime))

sonde.env <- bind_rows(sonde.epi, sonde.sed) %>%
  select(station, date, parameter, result) %>%
  spread(parameter, result) %>%
  mutate(date = as.Date(date))

## Light
par.data <-
  read.csv(
    "./data/par_merged.csv",
    header = T
  ) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         DOY = yday(date)) %>%
  filter(Depth == 2, site != "clw", DOY > 180) %>%
  dplyr::select(site, date, DOY, PAR)


## Chemistry
chem.dat <-
  read.csv(
    "./data/ConestogoLake_Chemistry_2017-2018.csv",
    header = T
  ) %>%
  mutate(date = as.Date(date)) %>%
  filter(date < as.Date("2017-12-31"))

parms.list <- unique(chem.dat$parmUnit)[-c(2, 13, 16, 17, 19, 20, 24, 25)]

chem.epi <-
  chem.dat %>% filter(depth_m == 2, parmUnit %in% parms.list) %>%
  mutate(parameter = paste0(parmUnit, "_epi")) %>%
  select(station, date, parameter, result)

chem.sed <- chem.dat %>%
  filter(parmUnit %in% parms.list) %>%
  group_by(station, date, parmUnit) %>%
  slice(n()) %>%
  mutate(parameter = paste0(parmUnit, "_sed")) %>%
  ungroup() %>%
  select(station, date, parameter, result)

chem.env <- bind_rows(chem.epi, chem.sed) %>%
  select(station, date, parameter, result) %>%
  spread(parameter, result)

env <- chem.env %>%
  left_join(par.data, by = c("station" = "site", "date")) %>%
  left_join(sonde.env, by = c("station", "date")) %>%
  left_join(lake.phys, by = c("station", "date" = "datetime")) %>%
  mutate(DOY = yday(date), site.code = paste0(station, DOY)) %>%
  filter(station != "clw")

#env.mat <- env %>% select(-c(station:date,DOY, site.code))
env.mat <- env[-1, ]

env.mat.select <- env.mat %>% select(NO2.N_mgN.L_epi:thermo.depth)
rownames(env.mat.select) <- env.mat$site.code

write.csv(env.mat.select, "./data/envmatrix.csv", row.names = T)
