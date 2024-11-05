contour_do <- function(data_to_plot, mybreaks, mylabels, min_lim, max_lim) {
  p <- ggplot(data = data_to_plot) + 
    theme_classic() + 
    metR::geom_contour_fill(aes(x = FakeYearDay, 
                                y = BinDepth, 
                                z = DO), 
                            na.fill = TRUE, 
                            breaks = mybreaks) + 
    scale_fill_craftfermenter(
      breaks = mybreaks,
      palette = "PuBu",
      direction = -1,
      limits = c(min_lim, max_lim),
      labels = mylabels,
      guide = guide_colorbar(show.limits = T, ticks = F)) +
    metR::geom_contour2(aes(x = FakeYearDay, y = BinDepth, z = DO), 
                        na.fill = T, breaks = 6) + 
    metR::geom_contour2(aes(x = FakeYearDay, y = BinDepth, z = DO), 
                        na.fill = T, breaks = 2, color = "red") +  
    scale_y_reverse(expand = c(0, 0)) + 
    coord_cartesian(xlim = c(0, 366)) + 
    scale_x_continuous(expand = c(0, 0), 
                       breaks = c(yday(paste(yoi, "-01-01", sep = "")), 
                                  yday(paste(yoi, "-02-01", sep = "")), 
                                  yday(paste(yoi, "-03-01", sep = "")), 
                                  yday(paste(yoi, "-04-01", sep = "")), 
                                  yday(paste(yoi, "-05-01", sep = "")), 
                                  yday(paste(yoi, "-06-01", sep = "")), 
                                  yday(paste(yoi, "-07-01", sep = "")), 
                                  yday(paste(yoi, "-08-01", sep = "")), 
                                  yday(paste(yoi, "-09-01", sep = "")), 
                                  yday(paste(yoi, "-10-01", sep = "")), 
                                  yday(paste(yoi, "-11-01", sep = "")), 
                                  yday(paste(yoi, "-12-01", sep = ""))), 
                       labels = month.abb) + 
    geom_vline(aes(xintercept = FakeYearDay), 
               alpha = 0.2) + 
    guides(fill = guide_colorbar(ticks.colour = NA)) + 
    labs(x = "", 
         y = "Depth (m)", 
         fill = "DO (mg/L)")
  return(p)
}
