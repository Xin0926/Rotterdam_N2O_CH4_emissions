
# the code is for plotting the 2D concentration map for both N2O and CH4 in the SI of chapter_3
# author: Xin Tong
# time: JAN 2, 2025


sapply(list.files(pattern="[.]R$", path="c:/Users/xin/Dropbox/xin/Rscripts/functions", full.names=TRUE), source)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 1. LOAD DATA for four flights~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

flights <- c("0830", "0901", "0905", "0906")
for(i in flights) {
  data <- read.csv(paste0("D:/1 PhD Studies/1 Data/Rotterdam_campaign_2022/AirCore measurements datasets/AirCore_N2O_CO_CO2_CH4_CO_2022", i, ".csv"))
  data <- mutate(data, pot = Tair*(1000/Ps)^0.286, ws=rWind::uv2ds(U,V)[,2], wd=rWind::uv2ds(U,V)[,1]); names(data)
  data <- data.frame(date = ISOdate(data$yyyy, data$mm, data$dd, data$hh, data$min, data$sec, tz='UTC'),data)
  
  if(i=="0830"){

    data <- mutate(data,
                   flag = case_when(time>(11*3600+54*60) & time<(12*3600+7*60+42) ~ "1st.transect",
                                    time>(12*3600+10*60) & time<(12*3600+31*60) ~ "2nd.transect",
                                    time>(12*3600+35*60) & time<(12*3600+50*60) ~ "3rd.transect",
                                    time>(12*3600+52*60) & time<(13*3600+14*60) ~ "4th.transect"))
    
    # n2o.break <- round(quantile(data$N2O.ac.ppb, probs = c(0.1, 0.5, 0.9), na.rm = TRUE), 1)
    # ch4.break <- round(quantile(data$CH4.ac.ppb, probs = c(0.1, 0.5, 0.9), na.rm = TRUE), 1)
    
    assign(paste0("n2o.lim.", i), c(336, 337, 338))
    assign(paste0("ch4.lim.", i), c(2010, 2035, 2060))
    
  } else if(i=="0901") {

    data <- mutate(data, flag = case_when(
      # following the wind direction, number the flight leg
      time>(12*3600+47*60+50) & time<(12*3600+58*60+19) ~ "1st.transect",
      time>(13*3600+2*60+32) & time<(13*3600+15*60+12) ~ "2nd.transect",
      time>(13*3600+19*60) & time<(13*3600+30*60+51) ~ "3rd.transect",
      time>(13*3600+34*60) & time<(13*3600+48*60) ~ "4th.transect",
      time>(13*3600+51*60) & time<(14*3600) ~ "5th.transect",
      time>(14*3600+5*60) & time<(14*3600+14*60+53) ~ "6th.transect",
    ))
    
    # n2o.break <- round(quantile(data$N2O.ac.ppb, probs = c(0.1, 0.5, 0.9), na.rm = TRUE), 1)
    # ch4.break <- round(quantile(data$CH4.ac.ppb, probs = c(0.1, 0.5, 0.9), na.rm = TRUE), 1)
    
    assign(paste0("n2o.lim.", i), c(335.8, 336.4, 337.0))
    assign(paste0("ch4.lim.", i), c(2000, 2020, 2040))
    
  } else if(i=="0905") {

    data <- mutate(data, flag = case_when(

      # following the wind direction, number the flight leg
      time>(12*3600) & time<(12*3600+24*60+31) ~ "1st.transect",
      time>(12*3600+26*60+28) & time<(12*3600+46*60+43) ~ "2nd.transect",
      time>(12*3600+50*60+45) & time<(13*3600+10*60) ~ "3rd.transect",
      time>(13*3600+12*60+44) & time<(13*3600+41*60) ~ "4th.transect",
    ))
    
    n2o.break <- round(quantile(data$N2O.ac.ppb, probs = c(0.1, 0.5, 0.9), na.rm = TRUE), 1)
    ch4.break <- round(quantile(data$CH4.ac.ppb, probs = c(0.1, 0.5, 0.9), na.rm = TRUE), 1)
    
    assign(paste0("n2o.lim.", i), c(337, 338, 339))
    assign(paste0("ch4.lim.", i), c(2000, 2025, 2050))
    
  } else {

    data <- mutate(data, flag = case_when(
      # following the wind direction, number the flight leg
      time>(12*3600+26*60) & time<(12*3600+35*60+12) ~ "1st.transect",
      time>(12*3600+39*60+25) & time<(12*3600+48*60) ~ "2nd.transect",
      time>(12*3600+50*60+32) & time<(12*3600+58*60+50) ~ "3rd.transect",
      time>(13*3600+60+20) & time<(13*3600+11*60+23) ~ "4th.transect",
      time>(13*3600+14*60+6) & time<(13*3600+24*60) ~ "5th.transect",
      time>(13*3600+27*60+30) & time<(13*3600+37*60) ~ "6th.transect",
    ))
    
    n2o.break <- round(quantile(data$N2O.ac.ppb, probs = c(0.1, 0.5, 0.9), na.rm = TRUE), 1)
    ch4.break <- round(quantile(data$CH4.ac.ppb, probs = c(0.1, 0.5, 0.9), na.rm = TRUE), 1)
    
    assign(paste0("n2o.lim.", i), c(336, 337, 338))
    assign(paste0("ch4.lim.", i), c(1980, 2025, 2070))
    
  }
  
  assign(paste0("df.", i), data)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 2. Prepare arguments for plotting ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# the same colourbar
n2o.lim <- range(c(df.0830$N2O.ac.ppb, df.0901$N2O.ac.ppb, df.0905$N2O.ac.ppb, df.0906$N2O.ac.ppb), na.rm = TRUE)
ch4.lim <- range(c(df.0830$CH4.ac.ppb, df.0901$CH4.ac.ppb, df.0905$CH4.ac.ppb, df.0906$CH4.ac.ppb), na.rm = TRUE)

# the same range of coordinates
range.lat <- range(c(df.0830$lat, df.0901$lat, df.0905$lat, df.0906$lat), na.rm = TRUE)
range.lon <- range(c(df.0830$lon, df.0901$lon, df.0905$lon, df.0906$lon), na.rm = TRUE)
sbbox <- make_bbox(lon = range.lon, lat = range.lat, f = .05)

# annoate wind direction and speed
list <- list(df.0830, df.0901, df.0905, df.0906)
for(j in 1:4) {
  data <- list[[j]]
  data <- data[!is.na(data$flag),]
  wind <- data.frame(
    lon = mean(data$lon), lat = mean(data$lat), wd = mean.cir(data$wd), ws = mean(data$ws), U = mean(data$U), V = mean(data$V)
  )
  
  assign(paste0("wind.", flights[j]), wind)
}

# tag each subplot in the figure

# The distance per degree of latitude and longitude
R=6371000
per.lat <- pi*R/180
per.lon <- pi*R/180*cos(mean(range.lat)*pi/180)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 3. Plot a figure ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
######
# N2O
######
map.flight = get_stadiamap(bbox=sbbox, zoom=10) # use ggmap(map) in ggplot environment to plot
# API_key <- # check the "D:\1 PhD Studies\3.1 Results of projects\2022_Rotterdam_Campaign\Rotterdam_Project_Journal.docx"
# register_stadiamaps(API_key, write = FALSE)

#col = c("#00007F", "#0000FF", "#007FFF", "#00FFFF", "#7FFF7F", "#FFFF00", "#FF7F00", "#FF0000", "#7F0000");

# Create helper function for common plot elements
create_n2o_plot <- function(data, map.flight, n2o.lim, wind_data, tag, point_size = 0.7, use_viridis = TRUE) {
  base_plot <- ggmap(map.flight) +
    geom_point(data = data, mapping = aes(x = lon, y = lat, colour = N2O.ac.ppb), size = point_size) +
    scale_color_gradientn(
      colors = c("#440154", "#3E4989", "#31688E", "#1F9E89", "#35B779", "#6DCD59", "#B4DE2C", "#FDE725"),
      values = c(1, 0.9, 0.8, 0.7, 0.6, 0.4, 0.2, 0),  
      labels = n2o.lim,
      breaks = n2o.lim
    )+
    geom_segment(
      wind_data,
      mapping = aes(
        x = 4.8, y = 52,
        xend = 4.8 + 10000*sin(wd*pi/180)/per.lon,
        yend = 52 + 10000*cos(wd*pi/180)/per.lat
      ),
      arrow = arrow(length = unit(0.1, "npc")), linewidth = 2
    ) +
    annotate('text', x = 4.8, y = 51.9, label = 'wind', size = 6) +
    guides(color = guide_colourbar(barwidth = 6, barheight = 0.5)) +
    labs(colour = bquote(''*N[2]*O*' [ppb]'), 
         x = 'Longitude [deg]', 
         y = 'Latitude [deg]', 
         tag = tag) +
    theme_classic() +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      legend.margin = margin(t = -8, r = 0, b = 0, l = 0, unit = "pt"),
      axis.title = element_text(size = 8),
      plot.tag = element_text(size = 8),
      plot.margin = margin(t = -5, r = 5, b = -5, l = 0, unit = "pt")
    )
  
}

# Create individual plots
p.n2o.0830 <- create_n2o_plot(df.0830, map.flight, n2o.lim.0830, wind.0830, "(a)", 0.8, use_viridis = TRUE)
p.n2o.0901 <- create_n2o_plot(df.0901, map.flight, n2o.lim.0901, wind.0901, "(c)", 0.8, use_viridis = TRUE)
p.n2o.0905 <- create_n2o_plot(df.0905, map.flight, n2o.lim.0905, wind.0905, "(e)", 0.8, use_viridis = TRUE)
p.n2o.0906 <- create_n2o_plot(df.0906, map.flight, n2o.lim.0906, wind.0906, "(g)", 0.8, use_viridis = TRUE)


# tiff("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/Figures/SI/'_N2O_map.tiff", units="mm", width=72.5, height=180, res=300)
# grid.arrange(p.n2o.0830, p.n2o.0901, p.n2o.0905, p.n2o.0906, nrow=4)
# #subplot(p.3D, p.alt, nrows = 2) %>% layout(scene = list(domain = list(x = c(0,1), y = c(0.5,1))))
# while (!is.null(dev.list()))  dev.off()



######
# CH4
######
create_ch4_plot <- function(data, map.flight, ch4.lim, wind_data, tag, point_size = 0.7, use_viridis = TRUE) {
  base_plot <- ggmap(map.flight) +
    geom_point(data = data, mapping = aes(x = lon, y = lat, colour = CH4.ac.ppb), size = point_size) +
    scale_color_gradientn(
      colors = c("#440154", "#3E4989", "#31688E", "#1F9E89", "#35B779", "#6DCD59", "#B4DE2C", "#FDE725"),
      values = c(1, 0.9, 0.8, 0.7, 0.6, 0.4, 0.2, 0),  
      labels = ch4.lim,
      breaks = ch4.lim
    )+
    geom_segment(
      wind_data,
      mapping = aes(
        x = 4.8, y = 52,
        xend = 4.8 + 10000*sin(wd*pi/180)/per.lon,
        yend = 52 + 10000*cos(wd*pi/180)/per.lat
      ),
      arrow = arrow(length = unit(0.1, "npc")), linewidth = 2
    ) +
    annotate('text', x = 4.8, y = 51.9, label = 'wind', size = 6) +
    guides(color = guide_colourbar(barwidth = 6, barheight = 0.5)) +
    labs(colour = bquote(''*CH[4]*' [ppb]'), 
         x = 'Longitude [deg]', 
         y = 'Latitude [deg]', 
         tag = tag) +
    theme_classic() +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      legend.margin = margin(t = -8, r = 0, b = 0, l = 0, unit = "pt"),
      axis.title = element_text(size = 8),
      plot.tag = element_text(size = 8),
      plot.margin = margin(t = -5, r = 5, b = -5, l = 0, unit = "pt")
    )
}

# Create individual plots
p.ch4.0830 <- create_ch4_plot(df.0830, map.flight, ch4.lim.0830, wind.0830, "(b)", 0.8, use_viridis = TRUE)
p.ch4.0901 <- create_ch4_plot(df.0901, map.flight, ch4.lim.0901, wind.0901, "(d)", 0.8, use_viridis = TRUE)
p.ch4.0905 <- create_ch4_plot(df.0905, map.flight, ch4.lim.0905, wind.0905, "(f)", 0.8, use_viridis = TRUE)
p.ch4.0906 <- create_ch4_plot(df.0906, map.flight, ch4.lim.0906, wind.0906, "(h)", 0.8, use_viridis = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ SAVE COMBINED PLOT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(cowplot)

# Create lists for N2O and CH4 plots without legends
n2o_plots <- list(
  p.n2o.0830 ,
  p.n2o.0901 ,
  p.n2o.0905 ,
  p.n2o.0906 
)

ch4_plots <- list(
  p.ch4.0830,
  p.ch4.0901,
  p.ch4.0905,
  p.ch4.0906
)


# Combine two plots
n2o_column <- plot_grid(plotlist = n2o_plots, ncol = 1, align = 'v')
ch4_column <- plot_grid(plotlist = ch4_plots, ncol = 1, align = 'v')

final_plot <- plot_grid(
  n2o_column, ch4_column, 
  ncol = 2,
  align = 'hv'
)


# Save the plot
ggsave("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/Figures/SI/N2O&CH4_concentration_map.tiff", final_plot,
       width = 180, height = 240,  # wider format for side-by-side plots
       units = "mm",  # adjust dimensions as needed
       dpi = 600)




  "#440154"
 "#3E4989"
 "#31688E"
"#1F9E89"

"#35B779"
"#6DCD59"
"#B4DE2C"
"#FDE725"
    
