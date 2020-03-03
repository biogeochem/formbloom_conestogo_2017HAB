##
## Constructs B-spline interpolation of chemistry data
##

interp.plot.chem <- function(df = "",
                             site.id = "",
                             variable = "",
                             doy.start = "",
                             doy.end = "",
                             scale.start = "",
                             scale.end = "") {
  require(tidyverse)
  require(lubridate)
  require(MBA)
  require(reshape)
  require(viridis)
  
  df <- df %>%
    mutate(DOY = yday(Date),
           NH4 = as.numeric(as.character(NH4)))
  mba <- mba.surf(
    df %>%
      filter(Site == site.id) %>%
      dplyr::select(DOY, ele.depth, variable) %>%
      na.omit(),
    100,
    100,
    n = 1,
    m = 2
  )
  dimnames(mba$xyz.est$z) <- list(mba$xyz.est$x, mba$xyz.est$y)
  df3 <-
    melt(mba$xyz.est$z,
         varnames = c('DOY', 'ele.depth'),
         value = 'value')
  
  #print(head(df3))
  
  fig <- ggplot(data = df3, aes(DOY, ele.depth)) +
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
      vjust = 0.5,
      na.rm = T
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
      name = ifelse(
        grepl("N", variable),
        expression(paste(variable, "(mg N", L ^
                           -1, ")")),
        expression(paste(variable, "(", mu, "g L" ^
                           -1, ")"))
      )
    ) +
    labs(x = "Day of Year", y = "Elevation (m.a.s.l.)",
         title = site.id) +
    xlim(doy.start, doy.end) +
    theme_std() +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
  
}