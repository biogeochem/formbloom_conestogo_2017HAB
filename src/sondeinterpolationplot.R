##
## Constructs B-spline interpolation of 0.5 m incremental sonde data
##


interp.plot <-
  function(df = "",
           site.id = "",
           variable = "",
           doy.start = "",
           doy.end = "",
           scale.start = "",
           scale.end = "") {
    require(MBA)
    require(tidyverse)
    require(reshape)
    require(viridis)
    
    mba <- mba.surf(
      df %>% #ungroup() %>%
        filter(station == site.id) %>%
        dplyr::select(DOY, ele.depth, variable) %>%
        na.omit(),
      100,
      100
    )
    dimnames(mba$xyz.est$z) <- list(mba$xyz.est$x, mba$xyz.est$y)
    df3 <-
      melt(mba$xyz.est$z,
           varnames = c('DOY', 'depth'),
           value = 'temperature')
    # print(head(df3))
    
    if (variable == "temp_C_avg") {
      fig <- ggplot(data = df3, aes(DOY, depth)) +
        geom_rect(aes(
          xmin = doy.start,
          xmax = doy.end,
          ymin = 373.38,
          ymax = 393.27
        ),
        fill = "grey50") +
        geom_raster(
          aes(fill = value),
          interpolate = F,
          hjust = 0.5,
          vjust = 0.5
        ) +
        geom_contour(aes(z = value),
                     color = "white",
                     alpha = 0.4,
                     size = 0.8) +
        geom_point(
          data = df %>% filter(station == site.id),
          aes(DOY, ele.depth),
          col = "black",
          alpha = 0.8,
          size = 1.25,
          shape = 1
        )  +
        geom_point(
          data = df %>% filter(station == site.id),
          aes(DOY, ele.depth),
          col = "grey75",
          alpha = 0.8,
          size = 1.15,
          shape = 20
        )  +
        
        scale_fill_viridis(
          limits = c(scale.start, scale.end),
          direction = 1,
          discrete = F,
          option = "D",
          name = expression(paste("Temp (", degree ~ C, ")"))
        ) +
        xlim(doy.start, doy.end) +
        labs(x = "Day of Year", y = "Elevation (m.a.s.l.)") +
        theme_std() +
        theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
      
      
      return(fig)
      
    } else if (variable == "DO_mg.L_avg") {
      ## Dissolved oxygen
      
      fig <- ggplot(data = df3 %>%
                      mutate(value = ifelse(value < 0, 0, value)),
                    aes(DOY, depth)) +
        geom_rect(aes(
          xmin = doy.start,
          xmax = doy.end,
          ymin = 373.38,
          ymax = 393.27
        ),
        fill = "grey50") +
        geom_raster(
          aes(fill = value),
          interpolate = F,
          hjust = 0.5,
          vjust = 0.5
        ) +
        geom_contour(aes(z = value),
                     color = "white",
                     alpha = 0.4,
                     size = 0.8) +
        
        # plot sampling points
        geom_point(
          data = df %>% filter(station == site.id, DO_mg.L_avg > 2),
          aes(DOY, ele.depth),
          col = "black",
          alpha = 0.8,
          size = 1.25,
          shape = 1
        )  +
        geom_point(
          data = df %>% filter(station == site.id, DO_mg.L_avg > 2),
          aes(DOY, ele.depth),
          col = "grey75",
          alpha = 0.8,
          size = 1.15,
          shape = 20
        )  +
        
        # plot hypoxia
        geom_point(
          data = df %>%
            filter(station == site.id, DO_mg.L_avg <= 2, DO_mg.L_avg > 0),
          aes(DOY, ele.depth),
          col = "red",
          alpha = 0.4,
          size = 1.5,
          shape = 22
        ) +
        
        # plot anoxia
        geom_point(
          data = df %>%
            filter(station == site.id, DO_mg.L_avg == 0),
          aes(DOY, ele.depth),
          col = "red",
          alpha = 0.9,
          size = 1.5,
          shape = 18
        ) +
        
        scale_fill_viridis(
          limits = c(scale.start, scale.end),
          alpha = 1,
          direction = 1,
          discrete = F,
          option = "D",
          name = expression(paste("DO (", "mg L" ^ -1, ")"))
        ) +

        xlim(doy.start, doy.end) +
        labs(x = "Day of Year", y = "Elevation (m.a.s.l.)") +
        theme_std() +
        theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
      
      return(fig)
      
    } else if (variable == "chla_ug.L_avg") {
      fig <- ggplot(data = df3, aes(DOY, depth)) +
        geom_rect(aes(
          xmin = doy.start,
          xmax = doy.end,
          ymin = 373.38,
          ymax = 393.27
        ),
        fill = "grey50") +
        geom_raster(
          aes(fill = log10(value)),
          interpolate = F,
          hjust = 0.5,
          vjust = 0.5
        ) +
        geom_contour(aes(z = log10(value)),
                     color = "white",
                     alpha = 0.4,
                     size = 0.8) +
        geom_point(
          data = df %>% filter(station == site.id),
          aes(DOY, ele.depth),
          col = "black",
          alpha = 0.8,
          size = 1.25,
          shape = 1
        )  +
        geom_point(
          data = df %>% filter(station == site.id),
          aes(DOY, ele.depth),
          col = "grey75",
          alpha = 0.8,
          size = 1.15,
          shape = 20
        )  +
        
        scale_fill_viridis(
          limits = c(-1, 2),
          alpha = 1,
          direction = 1,
          discrete = F,
          option = "D",
          name = expression(log10 ~ mu * g / L)
        ) +

        xlim(doy.start, doy.end) +
        labs(x = "Day of Year", y = "Elevation (m.a.s.l.)") +
        theme_std()
      
      return(fig)
      
      #BGA
      
    } else if (variable == "bga_ug.L_avg") {
      fig <- ggplot(data = df3, aes(DOY, depth)) +
        geom_rect(aes(
          xmin = doy.start,
          xmax = doy.end,
          ymin = 373.38,
          ymax = 393.27
        ),
        fill = "grey50") +
        geom_raster(
          aes(fill = value),
          interpolate = F,
          hjust = 0.5,
          vjust = 0.5
        ) +
        geom_contour(aes(z = value),
                     color = "white",
                     alpha = 0.4,
                     size = 0.8) +
        geom_point(
          data = df %>% filter(station == site.id),
          aes(DOY, ele.depth),
          col = "black",
          alpha = 0.8,
          size = 1.25,
          shape = 1
        )  +
        geom_point(
          data = df %>% filter(station == site.id),
          aes(DOY, ele.depth),
          col = "grey75",
          alpha = 0.8,
          size = 1.15,
          shape = 20
        )  +
        
        scale_fill_viridis(
          limits = c(scale.start, scale.end),
          alpha = 1,
          direction = 1,
          discrete = F,
          option = "D",
          name = expression(mu * g / L)
        ) +
        xlim(doy.start, doy.end) +
        labs(x = "Day of Year", y = "Elevation (m.a.s.l.)") +
        theme_std()
      
      return(fig)
      
    } else {
      fig <- ggplot(data = df3, aes(DOY, depth)) +
        geom_raster(
          aes(fill = value),
          interpolate = F,
          hjust = 0.5,
          vjust = 0.5
        ) +
        geom_contour(aes(z = value),
                     color = "white",
                     alpha = 0.4,
                     size = 0.8) +
        geom_point(
          data = df %>% filter(station == site.id),
          aes(DOY, ele.depth),
          col = "black",
          bg = "grey50",
          alpha = 0.4,
          size = 1.25,
          shape = 20
        )  +
        ## pH scale
        scale_fill_viridis(
          limits = c(scale.start, scale.end),
          alpha = 0.7,
          direction = 1,
          discrete = F,
          option = "D"
        ) +
        scale_y_reverse() +
        xlim(doy.start, doy.end) +
        labs(x = "Day of Year", y = "Depth (m)") +
        theme_bw()
    }
    
  }
