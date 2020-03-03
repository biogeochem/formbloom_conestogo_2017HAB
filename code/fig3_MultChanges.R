####
#
#
#
####

library(tidyverse)
library(cowplot)

# Load data ----
chem.dat <- read.csv("./data/con_chemistry_profiles.csv", header = T) %>%
  mutate(Date = as.Date(Date),
         DOY = yday(Date))

colnames(chem.dat) <- c(colnames(chem.dat)[1:18], "DIC.ppm","DIC.13C","DOC",
                        "DOC.13C","pCO2","TDP","SRP","TP", "DOY")
# Data frames ----
flood.dat.diff <- chem.dat %>% 
  filter(Date >= "2017-06-21", Date <="2017-07-10", Site == "clc") %>% 
  dplyr::select(Date:TDN,DOC,TDP:TP) %>%
  gather(Fe:TP, key = parameter, value = value) %>%
  spread(Date, value) 

colnames(flood.dat.diff) <- c("Depth","parameter","pre","post")

flood.dat.diff <- flood.dat.diff %>%
  mutate(Depth = as.factor(Depth),
         parameter = as.factor(parameter),
         post = as.numeric(post),
         pre = as.numeric(pre),
         muldif = post/pre,
         concdiff = post-pre,
         percdiff = ((post - pre)/pre)*100)

# create scale transformation
t_shift <- scales::trans_new("shift",
                             transform = function(x){x-  1},
                             inverse = function(x){x + 1})

# select list of parameters
p.list <- c("SRP","TDP","TP", "Mn")

# plot a
p.plot <- ggplot(data = flood.dat.diff %>%
                   filter(parameter %in%p.list), 
                 aes(x = parameter, y = muldif, fill = Depth, shape = Depth)) +
  geom_bar(stat = "identity", position = "dodge", col = "white", width = 0.5) + 
  geom_hline(yintercept = 1, col = "grey50", size = 0.35)+
  labs(y = "", x = "Measured parameter") + 
  scale_fill_viridis(discrete = T, begin = 0, end = 0.8, guide = guide_legend(reverse = TRUE)) + 
  scale_x_discrete(labels = rev(c("SRP","TDP", "TP", expression('Mn'^'+')))) +
  scale_y_continuous(trans = t_shift)+
  coord_flip()+
  theme_std()

p.plot$data$parameter <- factor(p.plot$data$parameter, levels = rev(c("SRP","TDP","TP","Mn")))
p.plot$data$Depth <- factor(p.plot$data$Depth, levels = c(16,7,2))
p.plot <- reposition_legend(p.plot, "bottom right", offset = c(0.025, 0.025))

# plot b
pars.list <- c("DOC","NH4","NO2","NO3","TDN","Cl","SO4","Mg","Ca","Fe")

p <- ggplot(data = flood.dat.diff %>%
              filter(parameter %in% pars.list), 
            aes(y = muldif, x = parameter, fill = Depth, shape = Depth)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5, col = "white") +
  geom_hline(yintercept = 1, col = "grey50", size = 0.35)+
  labs(y = "Multiplicative change", x = "Measured Parameter") + 
  scale_x_discrete(labels = rev(c("DOC",    
                                  expression('NH'[4]^'+'),
                                  expression('NO'[2]^'-'),
                                  expression('NO'[3]^"-"),
                                  "TDN",
                                  expression('Cl'^'-'),
                                  expression('SO'[4]^'-'),
                                  expression('Mg'^'2+'),
                                  expression('Ca'^'+'),
                                  "TDFe"))) +
  scale_fill_viridis_d(begin = 0, end = 0.8, guide = guide_legend(reverse = TRUE)) + 
  coord_flip() +
  scale_y_continuous(limits = c(0,2), breaks = c(0,0.5,1,1.5,2), 
                     labels = c("0.0", "0.5", "1.0", "1.5", "2.0"), trans = t_shift) +
  theme_std() + theme(legend.position = "none")

p$data$Depth <- factor(p$data$Depth, levels = c(16,7,2))
p$data$parameter <- factor(p$data$parameter, levels = rev(c("DOC","NH4","NO2","NO3","TDN","Cl","SO4","Mg","Ca","Fe")))

p

p <- plot_grid(p.plot, p, align = "v", labels = "auto", ncol = 1, rel_heights = c(0.75,1.5))

# save graphic

ggsave(filename = "./supporting-files/Fig3_MultChanges.tif",
       plot = p, device = "tiff",
       dpi = 500, units = "in", width = 2.5, height = 4.5, scale = 2)

#write.csv(flood.dat.diff, file = "./supporting-files/data_floodchanges.csv", row.names = F)