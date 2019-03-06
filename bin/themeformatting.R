theme_std <- function (base_size = 10, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.ticks = element_line(colour = "grey80", size = 0.75), 
          legend.key = element_blank(), 
          panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(colour = "grey80", fill=NA, size = 0.75),
          axis.line = element_line(colour = "grey80"),
          panel.grid.major = element_line(NA), 
          panel.grid.minor = element_line(NA), 
          axis.text  = element_text(size=rel(0.9), colour = "grey40"),
          #          axis.title  = element_text(size=rel(1.0)),
          axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),
                                      size=rel(1.1), colour = "grey40"),
          axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"),
                                      size=rel(1.1), angle = 90, colour= "grey40"),
          strip.text = element_text(size = rel(1.05), colour = "black"), #face = "normal"
          strip.background = element_blank(),
          plot.margin=unit(c(10,10,10,10),"pt")
    )
}

theme_pub <- function (base_size = 10, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.ticks = element_line(colour = "grey40", size = 0.75), 
          legend.key = element_rect(colour = "grey80"), 
          panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(colour = "grey80", fill=NA),
          axis.line = element_line(colour = "grey80"),
          panel.grid.major = element_line(NA), 
          panel.grid.minor = element_line(NA), 
          axis.text  = element_text(size=rel(0.9), colour = "grey40"),
          #          axis.title  = element_text(size=rel(1.0)),
          axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),
                                      size=rel(1.1), colour = "grey40"),
          axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"),
                                      size=rel(1.1), angle = 90, colour= "grey40"),
          strip.text = element_text(size = rel(1.05), colour = "black"), #face = "normal"
          strip.background = element_blank(),
          plot.margin=unit(c(10,10,10,10),"pt")
    )
}

theme_pres <- function (base_size = 16, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.ticks = element_line(colour = "grey40", size = 0.75), 
          legend.key = element_rect(colour = "grey80"), 
          panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(colour = "grey80", fill=NA),
          axis.line = element_line(colour = "grey80"),
          panel.grid.major = element_line(NA), 
          panel.grid.minor = element_line(NA), 
          axis.text  = element_text(size=rel(0.9), colour = "grey40"),
          #          axis.title  = element_text(size=rel(1.0)),
          axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),
                                      size=rel(1.1), colour = "grey40"),
          axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"),
                                      size=rel(1.1), angle = 90, colour= "grey40"),
          strip.text = element_text(size = rel(1.05), colour = "black"), #face = "normal"
          strip.background = element_blank(),
          plot.margin=unit(c(10,10,10,10),"pt")
    )
}