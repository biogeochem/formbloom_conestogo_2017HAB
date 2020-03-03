####
#
# Creates reservoir dynamics plots for Figure 1
#
####

library(tidyverse)

# Flow plot
flow.merge <- read.csv("./data/fig1_flowrates.csv") %>%
  mutate(date = as.Date(date))
flow.rates <- ggplot(data = flow.merge %>%
                       filter(date >= as.Date("2017-06-01"),
                              date <= as.Date("2017-08-17")), 
                     aes(x = date, y = mean.daily.flow)) + 
  geom_rect(aes(xmin = as.Date("2017-06-22"), xmax = as.Date("2017-06-24"), 
                ymin = -Inf, ymax = Inf), fill = "grey85") +
  geom_vline(xintercept = as.Date("2017-07-24"), col = "grey85") +
  geom_ribbon(aes(ymin = mean.daily.flow-sd, ymax = mean.daily.flow+sd), fill = "grey55")+
  geom_line(col = "black") +
  geom_hline(yintercept = 0.1, col ="red") +
  labs(x = "", y =  expression(paste("Mean daily\n flow rate (m"^3,")")))+
  theme_std()

# Arthur, ON precipitation plot
arthur.precip.clean.24hr.totals <- read.csv("./data/fig1_arthurprecip.csv") %>%
  mutate(date = as.Date(date))
arthur.precip.p <- ggplot(data = arthur.precip.clean.24hr.totals %>%
                            filter(date >= as.Date("2017-06-01"),
                                   date <= as.Date("2017-08-17")), 
                          aes(x = date, y = daily.total.mm)) + 
  geom_rect(aes(xmin = as.Date("2017-06-22"), xmax = as.Date("2017-06-24"), 
                ymin = -Inf, ymax = Inf), fill = "grey85") +
  geom_vline(xintercept = as.Date("2017-07-24"), col = "grey85") +
  geom_line() +
  labs(x = "", y = "Total daily\n precipitation (mm)") + theme_std()

# Residence time
res.dat <- read.csv("./data/reservoirhistorical.csv", header = T)

res.dat <- res.dat %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2017-06-01"), date <= as.Date("2017-08-17"))

res.time <- ggplot(data = res.dat, 
                   aes(x = date, y = res.time)) + 
  geom_rect(aes(xmin = as.Date("2017-06-22"), xmax = as.Date("2017-06-24"), 
                ymin = -Inf, ymax = Inf), fill = "grey85") +
  geom_vline(xintercept = as.Date("2017-07-24"), col = "grey85") +
  geom_line() +
  labs(y = "Residence time\n (d)", x= "") + theme_std()

# Storage volume
storage <- ggplot(data = res.dat, 
                  aes(x = date, y = storage.10e6.m3_mean)) +
  geom_rect(aes(xmin = as.Date("2017-06-22"), xmax = as.Date("2017-06-24"), 
                ymin = -Inf, ymax = Inf), fill = "grey85") +
  geom_vline(xintercept = as.Date("2017-07-24"), col = "grey85") +
  geom_ribbon(aes(ymin = storage.10e6.m3_mean - storage.10e6.m3_sd, 
                  ymax = storage.10e6.m3_mean + storage.10e6.m3_sd), fill = "grey55") +
  geom_line() + 
  scale_y_continuous(breaks = c(40000,50000,60000), labels = comma) +
  labs(#title = "2017 Mean daily reservoir storage volume",
    caption = "Source: GRCA, KIWIS Online Data Portal",
    x = "", 
    y = expression(paste("Mean daily\n storage volume (Mm"^3,")")))+ theme_std()

# group plot
p <- plot_grid(arthur.precip.p, flow.rates, res.time, storage, ncol=1, align="vh")

print(p)

# save plot
ggsave(filename = "./supporting-files/fig1b-d_reservoirdynamics.pdf",
       plot = p, device = "pdf",
       dpi = 500, units = "in", width = 2.5, height = 6, scale = 1.6)
