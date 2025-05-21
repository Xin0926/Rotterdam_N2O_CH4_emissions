# the code is for plotting the concentrations of N2O and CH4 along hypothetical transects perpendicular to wind direction 
# in the SI of chapter_3
# author: Xin Tong
# time: May 15, 2025


sapply(list.files(pattern="[.]R$", path="c:/Users/xin/Dropbox/xin/Rscripts/functions", full.names=TRUE), source)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 1. LOAD DATA for four flights~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

flights <- c("0830", "0901", "0905", "0906")
for(i in flights) {
  data <- read.csv(paste0("D:/1 PhD Studies/1 Data/Rotterdam_campaign_2022/AirCore measurements datasets/AirCore_N2O_CO_CO2_CH4_CO_2022", i, ".csv"))
  data <- mutate(data, pot = Tair*(1000/Ps)^0.286, ws=rWind::uv2ds(U,V)[,2], wd=rWind::uv2ds(U,V)[,1]); names(data)
  data <- data.frame(date = ISOdate(data$yyyy, data$mm, data$dd, data$hh, data$min, data$sec, tz='UTC'),data)
  
  # smooth n2o
  gaussian <- gau.sm(data, 15, 'N2O.ac.ppb'); data <- cbind(data, smooth.N2O = gaussian[, 2], smooth.N2O.sd  = gaussian[,3])
  # smooth ch4
  gaussian <- gau.sm(data, 15, 'CH4.ac.ppb'); data <- cbind(data, smooth.CH4 = gaussian[, 2], smooth.CH4.sd  = gaussian[,3])
  
  if(i=="0830"){
    
    data <- mutate(data,
                   flag = case_when(time>(11*3600+54*60) & time<(12*3600+7*60+42) ~ "1st.transect",
                                    time>(12*3600+10*60) & time<(12*3600+31*60) ~ "2nd.transect",
                                    time>(12*3600+35*60) & time<(12*3600+50*60) ~ "3rd.transect",
                                    time>(12*3600+52*60) & time<(13*3600+14*60) ~ "4th.transect"))
    
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
    
  } else if(i=="0905") {
    
    data <- mutate(data, flag = case_when(
      
      # following the wind direction, number the flight leg
      time>(12*3600) & time<(12*3600+24*60+31) ~ "1st.transect",
      time>(12*3600+26*60+28) & time<(12*3600+46*60+43) ~ "2nd.transect",
      time>(12*3600+50*60+45) & time<(13*3600+10*60) ~ "3rd.transect",
      time>(13*3600+12*60+44) & time<(13*3600+41*60) ~ "4th.transect",
    ))
    
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
    
  }
  
  assign(paste0("df.", i), data)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 2. convert the coordinates into distance on the hypothetical transects perpendicular to wd ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The distance per degree of latitude and longitude
R=6371000
per.lat <- pi*R/180
per.lon <- pi*R/180*cos(mean(data$lat)*pi/180)


flight <- "0830"
if(flight=="0830"){
  wd <- df.0830$wd[!is.na(df.0830$flag)]
  
  transect1 <- df.0830[which(df.0830$flag=='1st.transect'),]
  lon.zero <- 4.3435
  lat.zero <- 51.9752
  
  distance <- NULL
  for(i in 1:nrow(transect1)){
    if(is.na(transect1$lat[i])){
      d <- NA
    } else {
      d <- sqrt((lon.zero-transect1$lon[i])^2*per.lon^2+(lat.zero-transect1$lat[i])^2*per.lat^2)* sin((bearing(c(3.9824, 51.9887), c(4.4758, 51.9725))-mean.cir(wd)+180)*pi/180)
      
      if(transect1$lon[i]> lon.zero){
        d <- d
      } else {
        d <- d*(-1)
      }
    }
    distance <- c(distance, d)
  }
  transect1 <- cbind(transect1, distance)
  
  
  transect2 <- df.0830[which(df.0830$flag=='2nd.transect'),]
  lon.zero <- 4.237
  lat.zero <- 51.9259
  
  distance <- NULL
  for(i in 1:nrow(transect2)){
    if(is.na(transect2$lat[i])){
      d <- NA
    } else {
      d <- sqrt((lon.zero-transect2$lon[i])^2*per.lon^2+(lat.zero-transect2$lat[i])^2*per.lat^2)* sin((bearing(c(3.9859, 51.9281), c(4.4241, 51.9265))-mean.cir(wd)+180)*pi/180)
      #d <- distHaversine( c(lon.zero, lat.zero), c(transect2$lon[i], transect2$lat[i]))
      if(transect2$lon[i]> lon.zero){
        d <- d
      } else {
        d <- d*(-1)
      }
    }
    distance <- c(distance, d)
  }
  transect2 <- cbind(transect2, distance)
  
  
  transect3 <- df.0830[which(df.0830$flag=='3rd.transect'),]
  lon.zero <- 4.1219
  lat.zero <- 51.8756
  
  distance <- NULL
  for(i in 1:nrow(transect3)){
    if(is.na(transect3$lat[i])){
      d <- NA
    } else {
      d <- sqrt((lon.zero-transect3$lon[i])^2*per.lon^2+(lat.zero-transect3$lat[i])^2*per.lat^2)* sin((bearing(c(4.0032, 51.8796), c(4.5234, 51.8639))-mean.cir(wd)+180)*pi/180)
      if(transect3$lon[i]> lon.zero){
        d <- d
      } else {
        d <- d*(-1)
      }
    }
    distance <- c(distance, d)
  }
  transect3 <- cbind(transect3, distance)
  
  
  transect4 <- df.0830[which(df.0830$flag=='4th.transect'),]
  lon.zero <- 4.036
  lat.zero <- 51.8368
  
  distance <- NULL
  for(i in 1:nrow(transect4)){
    if(is.na(transect4$lat[i])){
      d <- NA
    } else {
      d <- sqrt((lon.zero-transect4$lon[i])^2*per.lon^2+(lat.zero-transect4$lat[i])^2*per.lat^2)* sin((bearing(c(3.9991, 51.8373), c(4.5892, 51.8277))-mean.cir(wd)+180)*pi/180)
      if(transect4$lon[i]> lon.zero){
        d <- d
      } else {
        d <- d*(-1)
      }
    }
    distance <- c(distance, d)
  }
  transect4 <- cbind(transect4, distance)
  
  transect.0830 <- rbind(transect1, transect2, transect3, transect4)
  
}

flight <- "0901"
if(flight=="0901"){
  wd <- df.0901$wd[!is.na(df.0901$flag)]
  
  transect1 <- df.0901[which(df.0901$flag=='1st.transect'),]
  lon.zero.1 <- 4.6879
  lat.zero.1 <- 51.9512
  
  distance <- NULL
  for(i in 1:nrow(transect1)){
    if(is.na(transect1$lat[i])){
      d <- NA
    } else {
      d <- sqrt((lon.zero.1-transect1$lon[i])^2*per.lon^2+(lat.zero.1-transect1$lat[i])^2*per.lat^2)*sin((mean.cir(wd)-bearing(c(4.6326, 51.9995), c(4.7215, 51.9235)))*pi/180)
      
      if(transect1$lat[i]< lat.zero.1){
        d <- d
      } else {
        d <- d*(-1)
      }
    }
    distance <- c(distance, d)
  }
  transect1 <- cbind(transect1, distance)
  
  
  transect2 <- df.0901[which(df.0901$flag=='2nd.transect'),]
  lon.zero.2 <- 4.5225
  lat.zero.2 <- 51.9429
  
  distance <- NULL
  for(i in 1:nrow(transect2)){
    if(is.na(transect2$lat[i])){
      d <- NA
    } else {
      d <- sqrt((lon.zero.2-transect2$lon[i])^2*per.lon^2+(lat.zero.2-transect2$lat[i])^2*per.lat^2)*sin((mean.cir(wd)-bearing(c(4.4749, 52.0018), c(4.6202, 51.8485)))*pi/180)
      if(transect2$lat[i]< lat.zero.2){
        d <- d
      } else {
        d <- d*(-1)
      }
    }
    distance <- c(distance, d)
  }
  transect2 <- cbind(transect2, distance)
  
  
  transect3 <- df.0901[which(df.0901$flag=='3rd.transect'),]
  lon.zero.3 <- 4.4231
  lat.zero.3 <- 51.9412
  
  distance <- NULL
  for(i in 1:nrow(transect3)){
    if(is.na(transect3$lat[i])){
      d <- NA
    } else {
      d <- sqrt((lon.zero.3-transect3$lon[i])^2*per.lon^2+(lat.zero.3-transect3$lat[i])^2*per.lat^2)*sin((mean.cir(wd)-bearing(c(4.3616, 52.0208), c(4.4905, 51.855)))*pi/180)
      if(transect3$lat[i]< lat.zero.3){
        d <- d
      } else {
        d <- d*(-1)
      }
    }
    distance <- c(distance, d)
  }
  transect3 <- cbind(transect3, distance)
  
  
  transect4 <- df.0901[which(df.0901$flag=='4th.transect'),]
  lon.zero.4 <- 4.3191
  lat.zero.4 <- 51.936
  
  distance <- NULL
  for(i in 1:nrow(transect4)){
    if(is.na(transect4$lat[i])){
      d <- NA
    } else {
      d <- sqrt((lon.zero.4-transect4$lon[i])^2*per.lon^2+(lat.zero.4-transect4$lat[i])^2*per.lat^2)*sin((mean.cir(wd)-bearing(c(4.2523, 52.0115), c(4.3723, 51.8622)))*pi/180)
      if(transect4$lat[i]< lat.zero.4){
        d <- d
      } else {
        d <- d*(-1)
      }
    }
    distance <- c(distance, d)
  }
  transect4 <- cbind(transect4, distance)
  
  
  transect5 <- df.0901[which(df.0901$flag=='5th.transect'),]
  lon.zero.5 <- 4.2006
  lat.zero.5 <- 51.9325
  
  distance <- NULL
  for(i in 1:nrow(transect5)){
    if(is.na(transect5$lat[i])){
      d <- NA
    } else {
      d <- sqrt((lon.zero.5-transect5$lon[i])^2*per.lon^2+(lat.zero.5-transect5$lat[i])^2*per.lat^2)*sin((mean.cir(wd)-bearing(c(4.1318, 52.0017), c(4.2665, 51.8623)))*pi/180)
      if(transect5$lat[i]< lat.zero.5){
        d <- d
      } else {
        d <- d*(-1)
      }
    }
    distance <- c(distance, d)
  }
  transect5 <- cbind(transect5, distance)
  
  
  transect6 <- df.0901[which(df.0901$flag=='6th.transect'),]
  lon.zero.6 <- 4.0128
  lat.zero.6 <- 51.9233
  
  distance <- NULL
  for(i in 1:nrow(transect6)){
    if(is.na(transect6$lat[i])){
      d <- NA
    } else {
      d <- sqrt((lon.zero.6-transect6$lon[i])^2*per.lon^2+(lat.zero.6-transect6$lat[i])^2*per.lat^2)*sin((mean.cir(wd)-bearing(c(3.9761, 51.9643), c(4.0877, 51.8294)))*pi/180)
      if(transect6$lat[i]< lat.zero.6){
        d <- d
      } else {
        d <- d*(-1)
      }
    }
    distance <- c(distance, d)
  }
  transect6 <- cbind(transect6, distance)
  
  transect.0901 <- rbind(transect1, transect2, transect3, transect4, transect5, transect6)
  
}

flight <- "0905"
if(flight=="0905"){
  wd <- df.0905$wd[!is.na(df.0905$flag)]
  
  transect1 <- df.0905[which(df.0905$flag=='1st.transect'),]
  lon.zero <- 4.2483
  lat.zero <- 51.8348
  
  distance <- NULL
  for(i in 1:nrow(transect1)){
    if(is.na(transect1$lat[i])){
      d <- NA
    } else {
      d <- sqrt((lon.zero-transect1$lon[i])^2*per.lon^2+(lat.zero-transect1$lat[i])^2*per.lat^2)  #do not have to convert wd as actual wd already perpendicular to the transects
      
      if(transect1$lon[i]> lon.zero){
        d <- d
      } else {
        d <- d*(-1)
      }
    }
    distance <- c(distance, d)
  }
  transect1 <- cbind(transect1, distance)
  
  
  transect2 <- df.0905[which(df.0905$flag=='2nd.transect'),]
  lon.zero <- 4.2501
  lat.zero <- 51.8719
  
  distance <- NULL
  for(i in 1:nrow(transect2)){
    if(is.na(transect2$lat[i])){
      d <- NA
    } else {
      d <- sqrt((lon.zero-transect2$lon[i])^2*per.lon^2+(lat.zero-transect2$lat[i])^2*per.lat^2)
      #d <- distHaversine( c(lon.zero, lat.zero), c(transect2$lon[i], transect2$lat[i]))
      if(transect2$lon[i]> lon.zero){
        d <- d
      } else {
        d <- d*(-1)
      }
    }
    distance <- c(distance, d)
  }
  transect2 <- cbind(transect2, distance)
  
  
  transect3 <- df.0905[which(df.0905$flag=='3rd.transect'),]
  lon.zero <- 4.2478
  lat.zero <- 51.9276
  
  distance <- NULL
  for(i in 1:nrow(transect3)){
    if(is.na(transect3$lat[i])){
      d <- NA
    } else {
      d <- sqrt((lon.zero-transect3$lon[i])^2*per.lon^2+(lat.zero-transect3$lat[i])^2*per.lat^2)
      if(transect3$lon[i]> lon.zero){
        d <- d
      } else {
        d <- d*(-1)
      }
    }
    distance <- c(distance, d)
  }
  transect3 <- cbind(transect3, distance)
  
  
  transect4 <- df.0905[which(df.0905$flag=='4th.transect'),]
  lon.zero <- 4.2497
  lat.zero <- 51.977
  
  distance <- NULL
  for(i in 1:nrow(transect4)){
    if(is.na(transect4$lat[i])){
      d <- NA
    } else {
      d <- sqrt((lon.zero-transect4$lon[i])^2*per.lon^2+(lat.zero-transect4$lat[i])^2*per.lat^2)
      if(transect4$lon[i]> lon.zero){
        d <- d
      } else {
        d <- d*(-1)
      }
    }
    distance <- c(distance, d)
  }
  transect4 <- cbind(transect4, distance)
  
  
  transect.0905 <- rbind(transect1, transect2, transect3, transect4)
  
}

flight <- "0906"
if(flight=="0906"){
  wd <- df.0906$wd[!is.na(df.0906$flag)]
  
  transect1 <- df.0906[which(df.0906$flag=='1st.transect'),]
  lon.zero.1 <- 4.0495
  lat.zero.1 <- 51.8683
  
  distance <- NULL
  for(i in 1:nrow(transect1)){
    if(is.na(transect1$lat[i])){
      d <- NA
    } else {
      d <- sqrt((lon.zero.1-transect1$lon[i])^2*per.lon^2+(lat.zero.1-transect1$lat[i])^2*per.lat^2)*sin((bearing(c(3.9539, 51.9835), c(4.0802, 51.8293))-mean.cir(wd))*pi/180)
      
      if(transect1$lon[i]> lon.zero.1){
        d <- d
      } else {
        d <- d*(-1)
      }
    }
    distance <- c(distance, d)
  }
  transect1 <- cbind(transect1, distance)
  
  
  transect2 <- df.0906[which(df.0906$flag=='2nd.transect'),]
  lon.zero.2 <- 4.2162
  lat.zero.2 <- 51.9042
  
  distance <- NULL
  for(i in 1:nrow(transect2)){
    if(is.na(transect2$lat[i])){
      d <- NA
    } else {
      d <- sqrt((lon.zero.2-transect2$lon[i])^2*per.lon^2+(lat.zero.2-transect2$lat[i])^2*per.lat^2)*sin((bearing(c(4.126, 51.9979), c(4.2589, 51.8614))-mean.cir(wd))*pi/180)
      if(transect2$lon[i]> lon.zero.2){
        d <- d
      } else {
        d <- d*(-1)
      }
    }
    distance <- c(distance, d)
  }
  transect2 <- cbind(transect2, distance)
  
  
  transect3 <- df.0906[which(df.0906$flag=='3rd.transect'),]
  lon.zero.3 <- 4.3204
  lat.zero.3 <- 51.9293
  
  distance <- NULL
  for(i in 1:nrow(transect3)){
    if(is.na(transect3$lat[i])){
      d <- NA
    } else {
      d <- sqrt((lon.zero.3-transect3$lon[i])^2*per.lon^2+(lat.zero.3-transect3$lat[i])^2*per.lat^2)*sin((bearing(c(4.2626, 51.9985), c(4.3716, 51.8603))-mean.cir(wd))*pi/180)
      if(transect3$lon[i]> lon.zero.3){
        d <- d
      } else {
        d <- d*(-1)
      }
    }
    distance <- c(distance, d)
  }
  transect3 <- cbind(transect3, distance)
  
  
  transect4 <- df.0906[which(df.0906$flag=='4th.transect'),]
  lon.zero.4 <- 4.416
  lat.zero.4 <- 51.9482
  
  distance <- NULL
  for(i in 1:nrow(transect4)){
    if(is.na(transect4$lat[i])){
      d <- NA
    } else {
      d <- sqrt((lon.zero.4-transect4$lon[i])^2*per.lon^2+(lat.zero.4-transect4$lat[i])^2*per.lat^2)*sin((bearing(c(4.3662, 52.014), c(4.5004, 51.8404))-mean.cir(wd))*pi/180)
      if(transect4$lon[i]> lon.zero.4){
        d <- d
      } else {
        d <- d*(-1)
      }
    }
    distance <- c(distance, d)
  }
  transect4 <- cbind(transect4, distance)
  
  
  transect5 <- df.0906[which(df.0906$flag=='5th.transect'),]
  lon.zero.5 <- 4.506
  lat.zero.5 <- 51.9702
  
  distance <- NULL
  for(i in 1:nrow(transect5)){
    if(is.na(transect5$lat[i])){
      d <- NA
    } else {
      d <- sqrt((lon.zero.5-transect5$lon[i])^2*per.lon^2+(lat.zero.5-transect5$lat[i])^2*per.lat^2)*sin((bearing(c(4.458, 52.0163), c(4.6126, 51.8517))-mean.cir(wd))*pi/180)
      if(transect5$lon[i]> lon.zero.5){
        d <- d
      } else {
        d <- d*(-1)
      }
    }
    distance <- c(distance, d)
  }
  transect5 <- cbind(transect5, distance)
  
  
  transect6 <- df.0906[which(df.0906$flag=='6th.transect'),]
  lon.zero.6 <- 4.6352
  lat.zero.6 <- 51.996
  
  distance <- NULL
  for(i in 1:nrow(transect6)){
    if(is.na(transect6$lat[i])){
      d <- NA
    } else {
      d <- sqrt((lon.zero.6-transect6$lon[i])^2*per.lon^2+(lat.zero.6-transect6$lat[i])^2*per.lat^2)*sin((bearing(c(4.6023, 52.0263), c(4.7566, 51.8824))-mean.cir(wd))*pi/180)
      if(transect6$lon[i]> lon.zero.6){
        d <- d
      } else {
        d <- d*(-1)
      }
    }
    distance <- c(distance, d)
  }
  transect6 <- cbind(transect6, distance)
  
  transect.0906 <- rbind(transect1, transect2, transect3, transect4, transect5, transect6)
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 3.Plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
######
# N2O
######
# Define common theme settings
my_theme <- theme_bw() +
  theme(legend.title = element_blank(),
        legend.text = element_text(),
        legend.margin = margin(0,0,0,0),
        legend.spacing = unit(0,'cm'),
        legend.key.size = unit(2,'mm'),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = NA, colour = NA),
        axis.title = element_text(),
        axis.text = element_text(),
        plot.tag = element_text(),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(t = 0, r = 5, b = 2, l = 5, unit = "pt"))

# Create function for common plot elements
create_n2o_plot <- function(data, tag, legend_pos = c(0.8, 0.65)) {
  ggplot(data) +
    #geom_point(mapping = aes(x = distance/1000, y = N2O.ac.ppb, group = flag, colour = flag), size = 0.1) +
    geom_path(mapping = aes(x = distance/1000, y = smooth.N2O, group = flag, colour = flag), linewidth = 0.8) +
    geom_ribbon(show.legend = FALSE,
                mapping = aes(x = distance/1000, 
                              ymin = smooth.N2O-smooth.N2O.sd, 
                              ymax = smooth.N2O+smooth.N2O.sd, 
                              fill = flag, 
                              colour = flag),
                alpha = 0.1,
                linetype = 'dashed',
                linewidth = 0.2) +
    scale_color_viridis(discrete = TRUE) +
    scale_fill_viridis(discrete = TRUE) +
    my_theme +
    theme(legend.position = legend_pos) +
    ylab(bquote(''*N[2]*O*' [ppb]')) +
    xlab('Distance [km]') +
    labs(tag = tag)
}



# Function to add text at relative position
add_relative_text <- function(plot, label, rel_x = 0.9, rel_y = 0.9, ...) {
  # Get the plot's coordinate ranges
  build <- ggplot_build(plot)
  x_range <- build$layout$panel_params[[1]]$x.range
  y_range <- build$layout$panel_params[[1]]$y.range
  
  # Calculate absolute position from relative
  x_pos <- x_range[1] + rel_x * (x_range[2] - x_range[1])
  y_pos <- y_range[1] + rel_y * (y_range[2] - y_range[1])
  
  plot + annotate("text", x = x_pos, y = y_pos, label = label, ...)
}

# Create four plots with different x scales
p.n2o.0830 <- create_n2o_plot(transect.0830, "(a)", "right")+
  ggtitle("0830")
p.n2o.0901 <- create_n2o_plot(transect.0901, "(c)", "right")+
  ggtitle("0901")
p.n2o.0905 <- create_n2o_plot(transect.0905, "(e)", "right")+
  ggtitle("0905")
p.n2o.0906 <- create_n2o_plot(transect.0906, "(g)", "right")+
  ggtitle("0906")

# Add text at same relative position (90% x, 90% y)
p.n2o.0830 <- add_relative_text(p.n2o.0830, "East", rel_x = 0.8, rel_y = 0.9, size = 5)
p.n2o.0830 <- add_relative_text(p.n2o.0830, "West", rel_x = 0.2, rel_y = 0.9, size = 5)
p.n2o.0901 <- add_relative_text(p.n2o.0901, "South", rel_x = 0.8, rel_y = 0.9, size = 5)
p.n2o.0901 <- add_relative_text(p.n2o.0901, "North", rel_x = 0.2, rel_y = 0.9, size = 5)
p.n2o.0905 <- add_relative_text(p.n2o.0905, "East", rel_x = 0.8, rel_y = 0.9, size = 5)
p.n2o.0905 <- add_relative_text(p.n2o.0905, "West", rel_x = 0.2, rel_y = 0.9, size = 5)
p.n2o.0906 <- add_relative_text(p.n2o.0906, "South", rel_x = 0.8, rel_y = 0.9, size = 5)
p.n2o.0906 <- add_relative_text(p.n2o.0906, "North", rel_x = 0.2, rel_y = 0.9, size = 5)

# tiff("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/Figures/SI/N2O_along_transects.tiff", units="mm", width=80, height=180, res=300)
# grid.arrange(p.n2o.0830, p.n2o.0901, p.n2o.0905, p.n2o.0906, nrow=4)
# #subplot(p.3D, p.alt, nrows = 2) %>% layout(scene = list(domain = list(x = c(0,1), y = c(0.5,1))))
# while (!is.null(dev.list()))  dev.off()


######
# CH4
######

# Create function for common plot elements
create_ch4_plot <- function(data, tag, legend_pos = c(0.8, 0.65)) {
  ggplot(data) +
    #geom_point(mapping = aes(x = distance/1000, y = N2O.ac.ppb, group = flag, colour = flag), size = 0.1) +
    geom_path(mapping = aes(x = distance/1000, y = smooth.CH4, group = flag, colour = flag), linewidth = 0.8) +
    geom_ribbon(show.legend = FALSE,
                mapping = aes(x = distance/1000, 
                              ymin = smooth.CH4-smooth.CH4.sd, 
                              ymax = smooth.CH4+smooth.CH4.sd, 
                              fill = flag, 
                              colour = flag),
                alpha = 0.1,
                linetype = 'dashed',
                linewidth = 0.2) +
    scale_color_viridis(discrete = TRUE) +
    scale_fill_viridis(discrete = TRUE) +
    my_theme +
    theme(legend.position = legend_pos) +
    ylab(bquote(''*CH[4]*' [ppb]')) +
    xlab('Distance [km]') +
    labs(tag = tag)
}

# Create four plots with different x scales
p.ch4.0830 <- create_ch4_plot(transect.0830, "(a)", "right")+
  ggtitle("0830")
p.ch4.0901 <- create_ch4_plot(transect.0901, "(c)", "right")+
  ggtitle("0901")
p.ch4.0905 <- create_ch4_plot(transect.0905, "(e)", "right")+
  ggtitle("0905")
p.ch4.0906 <- create_ch4_plot(transect.0906, "(g)", "right")+
  ggtitle("0906")

# Add text at same relative position (90% x, 90% y)
p.ch4.0830 <- add_relative_text(p.ch4.0830, "East", rel_x = 0.8, rel_y = 0.9, size = 5)
p.ch4.0830 <- add_relative_text(p.ch4.0830, "West", rel_x = 0.2, rel_y = 0.9, size = 5)
p.ch4.0901 <- add_relative_text(p.ch4.0901, "South", rel_x = 0.8, rel_y = 0.9, size = 5)
p.ch4.0901 <- add_relative_text(p.ch4.0901, "North", rel_x = 0.2, rel_y = 0.9, size = 5)
p.ch4.0905 <- add_relative_text(p.ch4.0905, "East", rel_x = 0.8, rel_y = 0.9, size = 5)
p.ch4.0905 <- add_relative_text(p.ch4.0905, "West", rel_x = 0.2, rel_y = 0.9, size = 5)
p.ch4.0906 <- add_relative_text(p.ch4.0906, "South", rel_x = 0.8, rel_y = 0.9, size = 5)
p.ch4.0906 <- add_relative_text(p.ch4.0906, "North", rel_x = 0.2, rel_y = 0.9, size = 5)


# Create individual plots
# p.ch4.0830 <- create_ch4_plot(transect.0830, "(b)", "right")+
#   ggtitle("0830")+
#   annotate("text", 
#            x = get_position(transect.0830$distance/1000, 0.03), 
#            y = get_position(transect.0830$CH4.ac.ppb, 0.99),
#            label = "West", size = 6)+
#   annotate("text", 
#            x = get_position(transect.0830$distance/1000, 0.97), 
#            y = get_position(transect.0830$CH4.ac.ppb, 0.99),
#            label = "East", size = 6)
# 
# p.ch4.0901 <- create_ch4_plot(transect.0901, "(d)", "right")+
#   ggtitle("0901")+
#   annotate("text", 
#            x = get_position(transect.0901$distance/1000, 0.03), 
#            y = get_position(transect.0901$CH4.ac.ppb, 0.99),
#            label = "North", size = 6)+
#   annotate("text", 
#            x = get_position(transect.0901$distance/1000, 0.97), 
#            y = get_position(transect.0901$CH4.ac.ppb, 0.99),
#            label = "South", size = 6)
# 
# p.ch4.0905 <- create_ch4_plot(transect.0905, "(f)", "right")+
#   ggtitle("0905")+
#   annotate("text", 
#            x = get_position(transect.0905$distance/1000, 0.03), 
#            y = get_position(transect.0905$CH4.ac.ppb, 0.99),
#            label = "West", size = 6)+
#   annotate("text", 
#            x = get_position(transect.0905$distance/1000, 0.97), 
#            y = get_position(transect.0905$CH4.ac.ppb, 0.99),
#            label = "East", size = 6)
# 
# p.ch4.0906 <- create_ch4_plot(transect.0906, "(h)", "right")+
#   ggtitle("0906")+
#   annotate("text", 
#            x = get_position(transect.0906$distance/1000, 0.03), 
#            y = get_position(transect.0906$CH4.ac.ppb, 0.99),
#            label = "North", size = 6)+
#   annotate("text", 
#            x = get_position(transect.0906$distance/1000, 0.97), 
#            y = get_position(transect.0906$CH4.ac.ppb, 0.99),
#            label = "South", size = 6)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ SAVE COMBINED PLOT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(cowplot)

# Extract legends for each N2O plot
legend_0830 <- get_legend(p.n2o.0830 + 
                            theme(legend.position = "right",
                                  legend.key.size = unit(0.5, "cm"),
                                  legend.text = element_text(size = 8)))

legend_0901 <- get_legend(p.n2o.0901 + 
                            theme(legend.position = "right",
                                  legend.key.size = unit(0.5, "cm"),
                                  legend.text = element_text(size = 8)))

legend_0905 <- get_legend(p.n2o.0905 + 
                            theme(legend.position = "right",
                                  legend.key.size = unit(0.5, "cm"),
                                  legend.text = element_text(size = 8)))

legend_0906 <- get_legend(p.n2o.0906 + 
                            theme(legend.position = "right",
                                  legend.key.size = unit(0.5, "cm"),
                                  legend.text = element_text(size = 8)))


# Create lists for N2O and CH4 plots without legends
n2o_plots <- list(
  p.n2o.0830 + theme(legend.position = "none"),
  p.n2o.0901 + theme(legend.position = "none"),
  p.n2o.0905 + theme(legend.position = "none"),
  p.n2o.0906 + theme(legend.position = "none")
)

ch4_plots <- list(
  p.ch4.0830 + theme(legend.position = "none"),
  p.ch4.0901 + theme(legend.position = "none"),
  p.ch4.0905 + theme(legend.position = "none"),
  p.ch4.0906 + theme(legend.position = "none")
)

legends <- list(legend_0830, legend_0901, legend_0905, legend_0906)


# Create three columns: N2O plots, CH4 plots, and legends
n2o_column <- plot_grid(plotlist = n2o_plots, ncol = 1, align = 'v')
ch4_column <- plot_grid(plotlist = ch4_plots, ncol = 1, align = 'v')
legend_column <- plot_grid(plotlist = legends, ncol = 1, align = 'v')


# Combine all three columns
final_plot <- plot_grid(
  n2o_column, ch4_column, legend_column,
  ncol = 3,
  rel_widths = c(0.425, 0.425, 0.15)  # Adjust these values as needed
)

# Save the plot
ggsave("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/Figures/SI/N2O&CH4_along_transects.tiff", final_plot,
       width = 180, height = 240,  # wider format for side-by-side plots
       units = "mm",  # adjust dimensions as needed
       dpi = 600)
