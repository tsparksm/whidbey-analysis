add_extra_contour_bits <- function() {
  list(
    theme_classic(),  
    scale_y_reverse(expand = c(0, 0)),  
    coord_cartesian(xlim = c(0, 366)), 
    scale_x_continuous(
      expand = c(0, 0), 
      breaks = c(
        yday(paste(yoi, "-01-01", sep = "")), 
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
        yday(paste(yoi, "-12-01", sep = ""))
      ), 
      labels = month.abb
    ),  
    geom_vline(aes(xintercept = FakeYearDay), alpha = 0.2),  
    guides(fill = guide_colorbar(ticks.colour = NA))
  )
}

add_sigmat_contour <- function() {
  list(
    metR::geom_contour_fill(
      aes(x = FakeYearDay, y = BinDepth, z = SigmaTheta), 
      na.fill = TRUE, 
      breaks = mybreaks, 
      color = alpha("white", sigmat_contour_alpha)), 
    scale_fill_cmocean(
      name = "dense", 
      breaks = mybreaks, 
      limits = lims, 
      labels = mylabels, 
      guide = guide_colorbar(ticks = FALSE, reverse = TRUE)
    ), 
    labs(
      x = "", 
      y = "Depth (m)", 
      fill = bquote(sigma[Theta]~(kg/m^3))
    ), 
    add_extra_contour_bits()
  )
}

add_do_contour <- function() {
  list(
    metR::geom_contour_fill(
      aes(x = FakeYearDay, y = BinDepth, z = DO),
      na.fill = TRUE,
      breaks = mybreaks, 
      size = 0
    ),
    scale_fill_paletteer_c(
      palette = "grDevices::Purple-Blue",
      breaks = mybreaks,
      limits = lims,
      labels = mylabels,
      guide = guide_colorbar(ticks = FALSE, reverse = TRUE)
    ),
    labs(
      x = "", 
      y = "Depth (m)", 
      fill = "DO (mg/L)"
    ), 
    add_extra_contour_bits()
  )
}

add_t_contour <- function() {
  list(
    metR::geom_contour_fill(
      aes(x = FakeYearDay, y = BinDepth, z = Temperature), 
      na.fill = TRUE, 
      breaks = mybreaks, 
      size = 0
    ), 
    scale_fill_cmocean(
      name = "thermal", 
      breaks = mybreaks, 
      limits = lims, 
      labels = mylabels, 
      guide = guide_colorbar(ticks = FALSE)
    ), 
    labs(
      x = "", 
      y = "Depth (m)", 
      fill = expression(T~(degree*C))
    ), 
    add_extra_contour_bits()
  )
}

add_s_contour <- function() {
  list(
    metR::geom_contour_fill(
      aes(x = FakeYearDay, y = BinDepth, z = Salinity), 
      na.fill = TRUE, 
      breaks = mybreaks, 
      size = 0
    ), 
    scale_fill_cmocean(
      name = "haline", 
      breaks = mybreaks, 
      limits = lims, 
      labels = mylabels, 
      guide = guide_colorbar(ticks = FALSE)
    ), 
    labs(
      x = "", 
      y = "Depth (m)", 
      fill = "S (PSU)"
    ), 
    add_extra_contour_bits()
  )
}

add_no23_contour <- function() {
  list(
    metR::geom_contour_fill(
      aes(x = FakeYearDay, y = BinDepth, z = NO23), 
      na.fill = TRUE, 
      breaks = mybreaks
    ), 
    scale_fill_cmocean(
      name = "thermal", 
      breaks = mybreaks, 
      limits = lims, 
      labels = mylabels, 
      guide = guide_colorbar(ticks = FALSE)
    ), 
    labs(
      x = "", 
      y = "Depth (m)", 
      fill = "N (mg/L)"
    ), 
    add_extra_contour_bits()
  )
}

get_labels <- function(mybreaks, 
                       min_lim = NA, 
                       max_lim = NA, 
                       round = TRUE, 
                       even_only = FALSE) {
  mylabels <- mybreaks
  if (even_only) {
    mylabels[!(round(mylabels/2, 2) == round(round(mylabels/2, 2)))] <- ""
  } else if (round) {
    mylabels[!(round(mylabels, 2) == round(round(mylabels, 2)))] <- ""
  } else {
    mylabels[!(round(mylabels, 2) == round(round(mylabels, 1), 2))] <- ""
  }
  if (even_only)
  if (!is.na(min_lim)) mylabels[1] <- paste0("<", min_lim)
  if (!is.na(max_lim)) mylabels[length(mylabels)] <- paste0(">", max_lim)
  return(mylabels)
}

get_filename <- function(type, stations, years_to_plot) {
  save_folder <- here("figs", "contour", type, "/")
  station_text <- paste(stations, collapse = "_")
  if (length(years_to_plot) > 1) {
    year_text <- paste0(years_to_plot[1], "-", last(years_to_plot))
  } else {
    year_text <- years_to_plot
  }
  filename <- paste(station_text, type, year_text, sep = "_")
  filepath <- paste0(save_folder, filename, ".png")
  return(filepath)
}

get_limits <- function(data_vector, acc) {
  min_lim <- round_any(
    min(data_vector, na.rm = T), 
    accuracy = acc, 
    f = floor
  )
  max_lim <- round_any(
    max(data_vector, na.rm = T),
    accuracy = acc, 
    f = ceiling
  )
  return(c(min_lim, max_lim))
}
