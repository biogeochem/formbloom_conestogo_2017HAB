interp.plot.chem <- function(df = "", site.id = "", variable = "", doy.start = "", doy.end = ""){
  require(tidyverse); require(lubridate); require(MBA); require(reshape)
  require(viridis)
  
  df <- df %>% 
    mutate(DOY = yday(Date),
           NH4 = as.numeric(as.character(NH4)))
  mba <- mba.surf(df %>% 
                    filter(Site == site.id) %>% 
                    select(DOY,ele.depth,variable) %>%
                    na.omit(), 100, 100)
  dimnames(mba$xyz.est$z) <- list(mba$xyz.est$x, mba$xyz.est$y)
  df3 <- melt(mba$xyz.est$z, varnames = c('DOY', 'ele.depth'), value = 'temperature')
  #print(head(df3))
  
  n.list = c("NO3", "TDN")
  n.list2 = c("NO2", "NH4")
  p.list = c("TDP", "SRP", "TP")
  
  if(variable %in% n.list){
    fig <- ggplot(data = df3, aes(DOY, ele.depth))+
      geom_rect(aes(xmin = doy.start, xmax = doy.end, ymin = 373.38, ymax = 393.27), 
                fill = "grey50") +
      geom_raster(aes(fill = value), interpolate = F, hjust = 0.5, vjust = 0.5, na.rm = T) +
      geom_contour(aes(z = value), color = "white", alpha = 0.4, size = 0.8) + 
      geom_point(data = df %>% filter(Site == site.id), aes(DOY, ele.depth), 
                 col = "grey50", bg = 'grey25', alpha= 0.7, size = 1.25, shape = 21) +
      scale_fill_viridis(limits=c(2.5, 6.5), 
                         direction = 1, discrete = F, option = "D", 
                         name = expression(mg~N~L^-1)) +
      #scale_y_reverse() + 
      #ylim(375, 393) +
      xlim(doy.start, doy.end) +
      labs(x = "Day of Year", y = "Elevation (m)", 
           title = site.id) + 
      #geom_vline(xintercept = 174, col = "grey40") + 
      #geom_vline(xintercept = 204, col = "grey40") +
      theme_std()
    
    
    #print(fig) 
  } else if(variable %in% n.list2){
    fig <- ggplot(data = df3, aes(DOY, ele.depth))+
      geom_rect(aes(xmin = doy.start, xmax = doy.end, ymin = 373.38, ymax = 393.27), 
                fill = "grey50") +
      geom_raster(aes(fill = value), interpolate = F, hjust = 0.5, vjust = 0.5, na.rm = T) +
      geom_contour(aes(z = value), color = "white", alpha = 0.4, size = 0.8) + 
      geom_point(data = df %>% filter(Site == site.id), aes(DOY, ele.depth), 
                 col = "grey50", bg = 'grey25', alpha= 0.7, size = 1.25, shape = 21) +
      scale_fill_viridis(limits=c(0, 0.35), 
                         direction = 1, discrete = F, option = "D", 
                         name = expression(mg~N~L^-1)) +
      #scale_y_reverse() + 
      #ylim(375, 393) +
      xlim(doy.start, doy.end) +
      labs(x = "Day of Year", y = "Elevation (m)", 
           title = site.id) + 
      #geom_vline(xintercept = 174, col = "grey40") + 
      #geom_vline(xintercept = 204, col = "grey40") +
      theme_std()
    
  } else if(variable %in% p.list){
    df3$value[df3$value < 0] <-0
    fig <- ggplot(data = df3, aes(DOY, ele.depth))+
      geom_rect(aes(xmin = doy.start, xmax = doy.end, ymin = 373.38, ymax = 393.27), 
                fill = "grey50") +
      geom_raster(aes(fill = value), interpolate = F, hjust = 0.5, vjust = 0.5) +
      geom_contour(aes(z = value), color = "white", alpha = 0.4, size = 0.8) + 
      geom_point(data = df %>% filter(Site == site.id), aes(DOY, ele.depth), 
                 col = "grey50", bg = 'grey25', alpha= 0.7, size = 1.25, shape = 21)  +
      scale_fill_viridis(limits=c(0, 125), 
                         direction = 1, discrete = F, option = "D",
                         name = expression(paste(mu,"g L"^-1))) +
      #scale_y_reverse() + 
      #ylim(375, 393) +
      xlim(doy.start, doy.end) +
      labs(x = "Day of Year", y = "Elevation (m)", 
           title = paste(site.id)) + 
      #geom_vline(xintercept = 174, col = "grey40") + 
      #geom_vline(xintercept = 204, col = "grey40") +
      theme_std()
    
    #print(fig) 
    
  } else {
    fig <- ggplot(data = df3, aes(DOY, ele.depth))+
      geom_rect(aes(xmin = doy.start, xmax = doy.end, ymin = 373.38, ymax = 393.27), 
                fill = "grey50") +
      geom_raster(aes(fill = value), interpolate = F, hjust = 0.5, vjust = 0.5) +
      geom_contour(aes(z = value), color = "white", alpha = 0.4, size = 0.8) + 
      geom_point(data = df %>% filter(Site == site.id), aes(DOY, ele.depth), 
                 col = "grey50", bg = 'grey25', alpha= 0.7, size = 1.25, shape = 21) +
      scale_fill_viridis(limits=c(0, 0.125), 
                         direction = 1, discrete = F, option = "D",
                         name = expression(mg~L^-1)) +
      #scale_y_reverse() + 
      #ylim(375, 393) +
      xlim(doy.start, doy.end) +
      labs(x = "Day of Year", y = "Elevation (m)") + 
      #geom_vline(xintercept = 174, col = "grey40") + 
      #geom_vline(xintercept = 204, col = "grey40") +
      theme_std() +theme(legend.position = "bottom")
  }
  
}

interp.plot <- function(df = "", site.id = "", variable = "", doy.start = "", doy.end = ""){
  require(MBA); require(tidyverse); require(reshape); require(viridis)
  
  mba <- mba.surf(df %>% 
                    filter(site == site.id) %>% 
                    select(DOY,ele.depth,variable) %>%
                    na.omit(), 100, 100)
  dimnames(mba$xyz.est$z) <- list(mba$xyz.est$x, mba$xyz.est$y)
  df3 <- melt(mba$xyz.est$z, varnames = c('DOY', 'depth'), value = 'temperature')
  # print(head(df3))
  
  if(variable == "temp.C"){
    fig <- ggplot(data = df3, aes(DOY, depth))+
      geom_rect(aes(xmin = doy.start, xmax = doy.end, ymin = 373.38, ymax = 393.27), fill = "grey50") +
      geom_raster(aes(fill = value), interpolate = F, hjust = 0.5, vjust = 0.5) +
      geom_contour(aes(z = value), color = "white", alpha = 0.4, size = 0.8) + 
      geom_point(data = df %>% filter(site == site.id), aes(DOY, ele.depth), 
                 col = "grey50", bg = 'grey25', alpha= 0.7, size = 1.25, shape = 21)  +
      scale_fill_viridis(limits=c(12, 26), 
                         direction = 1, discrete = F, option = "D", 
                         name = expression(degree~C)) +
      #scale_y_reverse() + 
      #ylim(373, 393) +
      xlim(doy.start, doy.end) +
      labs(x = "Day of Year", y = "Elevation (m)") + 
      theme_std() 
    #+ 
    # geom_hline(yintercept = 372.62, col ="red")
    
    return(fig) 
    
  } else if(variable == "DO.mgl"){
    fig <- ggplot(data = df3, aes(DOY, depth)) +
      geom_rect(aes(xmin = doy.start, xmax = doy.end, ymin = 373.38, ymax = 393.27), fill = "grey50") +
      geom_raster(aes(fill = value), interpolate = F, hjust = 0.5, vjust = 0.5) +
      geom_contour(aes(z = value), color = "white", alpha = 0.4, size = 0.8) + 
      geom_point(data = df %>% filter(site == site.id), aes(DOY, ele.depth), 
                 col = "grey50", bg = 'grey25', alpha= 0.7, size = 1.25, shape = 21)  +
      scale_fill_viridis(limits=c(-0.5, 18), alpha = 1, 
                         direction = 1, discrete = F, option = "D",
                         name = expression(paste("mg L"^-1))) +
      #scale_y_reverse() + 
      #ylim(373, 393) +
      xlim(doy.start, doy.end) +
      labs(x = "Day of Year", y = "Elevation (m)") + 
      theme_std() 
    
    return(fig) 
    
  } else if(variable == "Chl"){
    fig <- ggplot(data = df3, aes(DOY, depth))+
      geom_rect(aes(xmin = doy.start, xmax = doy.end, ymin = 373.38, ymax = 393.27), fill = "grey50") +
      #geom_raster(aes(fill = value), interpolate = F, hjust = 0.5, vjust = 0.5) +
      #geom_contour(aes(z = value), color = "white", alpha = 0.4, size = 0.8) + 
      geom_raster(aes(fill = log10(value)), interpolate = F, hjust = 0.5, vjust = 0.5) +
      geom_contour(aes(z = log10(value)), color = "white", alpha = 0.4, size = 0.8) + 
      geom_point(data = df %>% filter(site == site.id), aes(DOY, ele.depth), 
                 col = "grey50", bg = 'grey25', alpha= 0.7, size = 1.25, shape = 21)  +
      scale_fill_viridis(limits=c(-1, 2), alpha = 1, 
                         direction = 1, discrete = F, option = "D",
                         name = expression(paste("log10\nmg L"^-1))) +
      #scale_y_reverse() + 
      #ylim(375, 393) +
      xlim(doy.start, doy.end) +
      labs(x = "Day of Year", y = "Elevation (m)") + 
      theme_std() 
    
    return(fig)   
    
  } else {
    fig <- ggplot(data = df3, aes(DOY, depth))+
      geom_raster(aes(fill = value), interpolate = F, hjust = 0.5, vjust = 0.5) +
      geom_contour(aes(z = value), color = "white", alpha = 0.4, size = 0.8) + 
      geom_point(data = df %>% filter(site == site.id), aes(DOY, ele.depth), 
                 col = "grey50", bg = 'grey25', alpha= 0.7, size = 1.25, shape = 21)  +
      scale_fill_viridis(limits=c(10, 17), alpha = 0.7, 
                         direction = 1, discrete = F, option = "D") +
      scale_y_reverse() + 
      xlim(doy.start, doy.end) +
      labs(x = "Day of Year", y = "Depth (m)") + 
      theme_bw()
  }
  
}
