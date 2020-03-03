####
# GRCA data scrape
#
####

library(RCurl); library(tidyverse)

# Flow Rates ----

##  Drayton ----
myfile <- getURL('https://waterdata.grandriver.ca/KiWIS/KiWIS?service=kisters&type=queryServices&request=getTimeseriesValues&datasource=0&format=csv&ts_id=8629042&from=2017-05-01&to=2017-08-31&timezone=GMT', 
                 ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
drayton.flow <- read.csv(textConnection(myfile), header=T, sep = ";")

drayton.flow <- drayton.flow[3:nrow(drayton.flow),]
colnames(drayton.flow) <- c("timestamp","value")

drayton.flow.clean <- drayton.flow %>% 
  mutate(timestamp = as.character(timestamp),
         timestamp = gsub("Z", "", timestamp),
         timestamp = gsub("T"," ", timestamp),
         timestamp = as.POSIXct(timestamp, tz = "UTC"),
         timestamp = format(timestamp, tz = "Etc/GMT+4", usetz = FALSE),
         value = as.numeric(as.character(value))) %>% 
  separate(timestamp, into = c("date","time"), sep = " ", remove = FALSE) %>%
  #filter(date>="2018-03-12", date <"2017-11-5") %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

## Moorefield ----
myfile <- getURL('https://waterdata.grandriver.ca/KiWIS/KiWIS?service=kisters&type=queryServices&request=getTimeseriesValues&datasource=0&format=csv&ts_id=8803042&from=2017-03-12&to=2017-08-30&timezone=GMT', 
                 ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
moorefield.flow <- read.csv(textConnection(myfile), header=T, sep = ";")

moorefield.flow <- moorefield.flow[3:nrow(moorefield.flow),]
colnames(moorefield.flow) <- c("timestamp","value")

moorefield.flow.clean <- moorefield.flow %>% 
  mutate(timestamp = as.character(timestamp),
         timestamp = gsub("Z", "", timestamp),
         timestamp = gsub("T"," ", timestamp),
         timestamp = as.POSIXct(timestamp, tz = "UTC"),
         timestamp = format(timestamp, tz = "Etc/GMT+4", usetz = FALSE),
         value = as.numeric(as.character(value))) %>% 
  separate(timestamp, into = c("date","time"), sep = " ", remove = FALSE) %>%
  #filter(date>="2018-03-12", date <"2017-11-5") %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

## merge data frame 
flow.merge <- drayton.flow.clean %>%
  left_join(moorefield.flow.clean, by = c("timestamp","date","time")) %>%
  filter(date >= as.Date("2017-05-01"), date <=as.Date("2017-09-01")) %>%
  mutate(sum.flow = value.x + value.y, timestamp = as_datetime(timestamp)) %>%
  group_by(date) %>%
  mutate(mean.daily.flow = mean(sum.flow, na.rm = T), sd = sd(sum.flow, na.rm = T))

## write datafile
write.csv(flow.merge,
          file = "./data/fig1_flowrates.csv", row.names = F)


# Arthur, ON precip ----

myfile <- getURL('https://waterdata.grandriver.ca/KiWIS/KiWIS?service=kisters&type=queryServices&request=getTimeseriesValues&datasource=0&format=csv&ts_id=12460042&from=2017-06-01&to=2017-08-17&timezone=GMT', 
                 ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
arthur.precip <- read.csv(textConnection(myfile), header=T, sep = ";")

arthur.precip <- arthur.precip[3:nrow(arthur.precip),]
colnames(arthur.precip) <- c("timestamp","value")

arthur.precip.clean <- arthur.precip %>% 
  mutate(timestamp = as.character(timestamp),
         timestamp = gsub("Z", "", timestamp),
         timestamp = gsub("T"," ", timestamp),
         #timestamp = as.POSIXct(timestamp, tz = "UTC"),
         timestamp = format(timestamp, tz = "Etc/GMT+4", usetz = FALSE),
         value = as.numeric(as.character(value))) %>% 
  separate(timestamp, into = c("date","time"), sep = " ", remove = FALSE) %>%
  #filter(date>="2018-03-12", date <"2017-11-5") %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         year = format(date, "%Y"), month = format(date, "%b"), 
         mo.day = format(date, "%m%d"),
         DOY = yday(date),
         period =ifelse(mo.day >="0215" & mo.day < "0501","springfill",
                        ifelse(mo.day >= "0501" & mo.day <"0601","mayfill",
                               ifelse(mo.day>= "0601", "summerdraw","other"))),
         timestamp = as_datetime(timestamp))

arthur.precip.clean.24hr.totals <- arthur.precip.clean %>%
  group_by(year, month, mo.day, DOY, date) %>%
  summarize(daily.total.mm = sum(value, na.rm = TRUE)) %>%
  mutate(period =ifelse(mo.day >="0215" & mo.day < "0501","springfill",
                        ifelse(mo.day >= "0501" & mo.day <"0601","mayfill",
                               ifelse(mo.day>= "0601", "summerdraw","other"))))
arthur.precip.clean.24hr.totals <- as.data.frame(arthur.precip.clean.24hr.totals)

## write data file
write.csv(arthur.precip.clean.24hr.totals,
          file = "./data/fig1_arthurprecip.csv", row.names = F)