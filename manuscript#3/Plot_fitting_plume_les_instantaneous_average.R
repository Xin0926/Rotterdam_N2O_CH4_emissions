# the code is for plotting modelled INSTANTANEOUS & AVERAGE plumes with two fitting functions
# in the SI of chapter_3
# author: Xin Tong
# time: September 1, 2024


sapply(list.files(pattern="[.]R$", path="c:/Users/xin/Dropbox/xin/Rscripts/functions", full.names=TRUE), source)

############################################### LOAD OR CREATE DATASETS #############################################################
df.0830 <- read.csv("D:/1 PhD Studies/1 Data/les/0830/extract_les/plume.layers.csv")
df.0901 <- read.csv("D:/1 PhD Studies/1 Data/les/0901/extract_les/plume.layers.csv")
df.0906 <- read.csv("D:/1 PhD Studies/1 Data/les/0906/extract_les/plume.layers.csv")

# The distance [m] per degree of latitude and longitude
per.lat <- 111320
per.lon <- function(latitude) {
  return(111320*cos(latitude*pi/180))
}

############################################################
datasets <- list(df.0830, df.0901, df.0906)
flight <- c("0830", "0901", "0906")
for(i in 1:3){
  n2o <- datasets[[i]]
  names(n2o)[names(n2o)=="x"] <- "lon"
  names(n2o)[names(n2o)=="y"] <- "lat"
  names(n2o)[names(n2o)=="N2O_flat"] <- "n2o"
  names(n2o)[names(n2o)=="CH4_flat"] <- "ch4"
  
  n2o <- n2o[n2o$layer=="layer_25",]
  # Convert string to POSIXlt object
  dt_obj <- strptime(n2o$date, format = "%Y-%m-%d %H:%M:%S")
  # Calculate the total seconds since the start of the day
  n2o$timestamp <- dt_obj$hour * 3600 + dt_obj$min * 60 + dt_obj$sec
  
  if(flight[i]=="0830"){
    lon.zero <- 4.2361  # the zero point should be the same to the zero point for the downwind transect of airborne measurements 
    lat.zero <- 51.9259
    n2o <- n2o[n2o$timestamp>(12*3600+10*60) & n2o$timestamp<(12*3600+31*60),] # select only 2nd transect
  } else if(flight[i]=="0901"){
    lon.zero <- 4.0199
    lat.zero <- 51.9136
    n2o <- n2o[n2o$timestamp>(14*3600+5*60) & n2o$timestamp<(14*3600+14*60+53),] # select only 6th transect
  } else {
    lon.zero <- 4.190460
    lat.zero <- 51.93287
    n2o <- n2o[n2o$timestamp>(12*3600+39*60+25) & n2o$timestamp<(12*3600+48*60),] # select only 2nd transect fro 0906
  }

  # Check if latitudes are NA
  valid_coords <- !is.na(n2o$lat)
  
  # Calculate distances only for valid coordinates
  distance <- ifelse(
    valid_coords,
    sqrt((lon.zero - n2o$lon)^2 * per.lon(n2o$lat)^2 + (lat.zero - n2o$lat)^2 * per.lat^2) *
      ifelse(n2o$lon > lon.zero, 1, -1),
    NA
  )
  # Combine the calculated distance with the original data
  n2o$distance <- distance
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~ INSTANTANEOUS OR AVERAGE ~~~~~~~~~~~~~~~~~~~~~~~~~
  if(average==TRUE){
    # calculate average plume for n2o
    n2o_valid <- n2o %>%
      group_by(distance) %>%
      # Calculate the mean value for each group
      summarize(n2o = mean(n2o, na.rm = TRUE), ch4 = mean(ch4, na.rm = TRUE)) %>%
      # Ungroup the data frame
      ungroup()
  } else {
    n2o_valid <- n2o[n2o$date==unique(n2o$date)[length(unique(n2o$date))/2],]
  }
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  
  # Fit the smooth spline model
  fit <- smooth.spline(n2o_valid$distance, n2o_valid$n2o)
  n2o_valid$spline <- predict(fit, n2o_valid$distance)$y
  
  # Fit the loess model
  fit <- loess(n2o_valid$n2o~n2o_valid$distance, data=n2o_valid)
  n2o_valid$loess <- predict(fit, n2o_valid$distance)
  
  melt <- melt(n2o_valid, id.vars = c("distance", "n2o"), measure.vars = c("spline", "loess"))
  
  assign(paste0("n2o.", flight[i]), n2o_valid)
  assign(paste0("n2o.fit.", flight[i]), melt)
}



############################################### create a function for plotting #############################################################
library(viridis)
# create functions for plotting
create_plot <- function(data1, data2){

  ggplot()+
    geom_point(data1, mapping = aes(x = distance/1000, y = n2o), colour = "black", size = 0.5)+
    geom_line(data2, mapping = aes(x = distance/1000, y = value, colour = variable))+
    scale_color_manual(values = c("#CC79A7", "#0072B2"))+
    labs(y = bquote(''*N[2]*O*' Enhancement [ppb]'), x = "Distance [km]")+
    theme_bw()+
    theme(legend.position = 'right', legend.title = element_blank(),
          legend.text = element_text(size = 8), legend.margin = margin(0,0,0,0),
          legend.spacing = unit(0,'cm'), legend.key.size = unit(2,'mm'),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.key = element_rect(fill = NA, colour = NA),
          #legend.margin = margin(t = -8, r = 0, b = 0, l = 0, unit = "pt"),
          axis.title = element_text(size=8),
          axis.text = element_text(size=8),
          plot.tag = element_text(size = 8),
          plot.margin = margin(t = 5, r = 5, b = 1, l = 5, unit = "pt"))
  
}

############################################### plot #############################################################

# apply the functions 
p.n2o.0830 <- create_plot(n2o.0830, n2o.fit.0830)+
  labs(tag = "(g)")
p.n2o.0901 <- create_plot(n2o.0901, n2o.fit.0901)+
  labs(tag = "(h)")
p.n2o.0906 <- create_plot(n2o.0906, n2o.fit.0906)+
  labs(tag = "(i)")

library(patchwork)
p.average <- p.n2o.0830 + p.n2o.0901 + p.n2o.0906+ 
  plot_layout(guides = 'collect', nrow = 1) &
  theme(legend.position = 'right', legend.direction = "vertical",
        plot.tag.location = "margin", plot.tag.position = "topleft",
        plot.title = element_text(hjust = 0.5))


while (!is.null(dev.list()))  dev.off()
# ggsave("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/Figures/SI/n2o.les.instantaneous.plume.tiff", plot = p,
#        width = 300, height = 75, units = "mm", dpi = 300, compression = "lzw")
# ggsave("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/Figures/SI/n2o.les.average.plume.tiff", plot = p,
#        width = 300, height = 75, units = "mm", dpi = 300, compression = "lzw")
