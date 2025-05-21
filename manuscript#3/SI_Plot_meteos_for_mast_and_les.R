# the code is for plotting meteologicla parameters including wd, ws, pressure, and temperature for both LES and mast measurements
# in the SI of chapter_3
# author: Xin Tong
# time: August 19, 2024


sapply(list.files(pattern="[.]R$", path="c:/Users/xin/Dropbox/xin/Rscripts/functions", full.names=TRUE), source)


mast <- read.csv("D:/1 PhD Studies/1 Data/LES/met_mast_Rotterdam.csv", skip =1, sep = ";"); 
datetime <- lubridate::ymd_hms(mast[-1, 1], tz = "UTC")
mast <- data.frame(apply(mast[-1, -1], 2, as.numeric), datetime)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ load mast measurements ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# for the comparison of ws
melt <- melt(mast, id.vars = "datetime", measure.vars = c("MMX_H135_Ws_Q1_avg", "MMX_H130B160_Ws_Q1_avg", "MMX_H080B160_Ws_Q1_avg", "MMX_H025B160_Ws_Q1_avg", "MMX_H025B340_Ws_Q1_avg"))

alt.character <- apply(melt, 1, function(row){
  strsplit(as.character(row["variable"]), "_")[[1]][2]
})

melt <- cbind(melt, alt.character)

# select the same measurement height with different boom directions
lowest.level <- c("H025B160", "H025B340")
melt <- melt[melt$alt.character!=lowest.level[1], ]

alt.num <- apply(melt, 1, function(row){
  as.numeric(substr(row["alt.character"], start = 2, stop = 4))
})

ws.mast <- cbind(melt, alt.num)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# for the comparison of wd
melt <- melt(mast, id.vars = "datetime", measure.vars = c("MMX_H126B340_Wd_Q1_avg", "MMX_H126B160_Wd_Q1_avg", "MMX_H080B340_Wd_Q1_avg", "MMX_H021B160_Wd_Q1_avg")) # wd indicates where wd come from
alt.character <- apply(melt, 1, function(row){
  strsplit(as.character(row["variable"]), "_")[[1]][2]
})

melt <- cbind(melt, alt.character)

# select the same measurement height with different boom directions
lowest.level <- c("H126B340", "H126B160")
melt <- melt[melt$alt.character!=lowest.level[2], ]

alt.num <- apply(melt, 1, function(row){
  as.numeric(substr(row["alt.character"], start = 2, stop = 4))
})

wd.mast <- cbind(melt, alt.num)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# for the comparison of temperature
# DID NOT compare with pressure that is measured at the mast for single height level
mast <- read.csv("D:/1 PhD Studies/1 Data/LES/met_mast_Rotterdam.csv", skip =1, sep = ";"); 
datetime <- lubridate::ymd_hms(mast[-1, 1])
mast <- data.frame(apply(mast[-1, -1], 2, as.numeric), datetime)
mast <- mutate(mast,
               pot.126 = (MMX_H126_Temp_Q1_avg+273.15)*(1000/MMX_H126_Pair_Q1_avg)^0.286,
               pot.21 = (MMX_H021_Temp_Q1_avg+273.15)*(1000/(MMX_H126_Pair_Q1_avg+13))^0.286) # infer the pressure at 21 m based on the pressure at 126 m
# mast$pot.126 <- mast$pot.126-273.15; # calculate potential temperature in degree
# mast$pot.21 <- mast$pot.21-273.15 # calculate potential temperature in degree


melt <- melt(mast, id.vars = "datetime", measure.vars = c("MMX_H126_Temp_Q1_avg", "MMX_H021_Temp_Q1_avg"))
alt.character <- apply(melt, 1, function(row){
  strsplit(as.character(row["variable"]), "_")[[1]][2]
})
melt <- cbind(melt, alt.character)
alt.num <- apply(melt, 1, function(row){
  as.numeric(substr(row["alt.character"], start = 2, stop = 4))
})
temp.mast <- cbind(melt, alt.num)
#temp.mast$datetime <- as.character(temp.mast$datetime)
temp.mast$value <- temp.mast$value+273.15 # change the unit of temperature to kelvin


melt <- melt(mast, id.vars = "datetime", measure.vars = c("pot.126", "pot.21"))
alt.num <- apply(melt, 1, function(row){
  as.numeric(substr(row["variable"], start = 5, stop = 7))
})
pot.mast <- cbind(melt, alt.num)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ select unique time periods for mast measurements ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tmin.0901 <- as.POSIXct("2022-09-01 13:40:00", tz = "UTC"); tmax.0901 <- as.POSIXct("2022-09-01 14:24:00", tz = "UTC")
tmin.0905 <- as.POSIXct("2022-09-05 12:50:00", tz = "UTC"); tmax.0905 <- as.POSIXct("2022-09-05 13:26:00", tz = "UTC")
tmin.0906 <- as.POSIXct("2022-09-06 12:10:00", tz = "UTC"); tmax.0906 <- as.POSIXct("2022-09-06 12:56:00", tz = "UTC")

wd.mast.0901 <- wd.mast[wd.mast$datetime>=tmin.0901 & wd.mast$datetime<=tmax.0901,]
wd.mast.0905 <- wd.mast[wd.mast$datetime>=tmin.0905 & wd.mast$datetime<=tmax.0905,]
wd.mast.0906 <- wd.mast[wd.mast$datetime>=tmin.0906 & wd.mast$datetime<=tmax.0906,]
ws.mast.0901 <- ws.mast[ws.mast$datetime>=tmin.0901 & ws.mast$datetime<=tmax.0901,]
ws.mast.0905 <- ws.mast[ws.mast$datetime>=tmin.0905 & ws.mast$datetime<=tmax.0905,]
ws.mast.0906 <- ws.mast[ws.mast$datetime>=tmin.0906 & ws.mast$datetime<=tmax.0906,]
temp.mast.0901 <- temp.mast[temp.mast$datetime>=tmin.0901 & temp.mast$datetime<=tmax.0901,]
temp.mast.0905 <- temp.mast[temp.mast$datetime>=tmin.0905 & temp.mast$datetime<=tmax.0905,]
temp.mast.0906 <- temp.mast[temp.mast$datetime>=tmin.0906 & temp.mast$datetime<=tmax.0906,]
pot.mast.0901 <- pot.mast[pot.mast$datetime>=tmin.0901 & pot.mast$datetime<=tmax.0901,]
pot.mast.0905 <- pot.mast[pot.mast$datetime>=tmin.0905 & pot.mast$datetime<=tmax.0905,]
pot.mast.0906 <- pot.mast[pot.mast$datetime>=tmin.0906 & pot.mast$datetime<=tmax.0906,]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ load LES data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
flight <- c("0901", "0905", "0906")
mean <- TRUE # if average data to 10 minutes
for(i in 1:3){
  les <- read.csv(paste0("D:/1 PhD Studies/1 Data/LES/", flight[i], "/extract_les/wind.vertical.profile.csv"))
  les <- mutate(les, ws=rWind::uv2ds(U,V)[,2], wd=rWind::uv2ds(U,V)[,1])#;names(data)
  wd <- mutate(les, wd.1 = case_when(
    wd-180>0 ~ wd-180,
    wd-180<0 ~ wd-180+360)) # convert the degree to the number that indicates where wd come from
  
  if(mean == TRUE){
    wd$datetime <- as.POSIXct(wd$datetime, tz = "UTC")
    wd$datetime <- floor_date(wd$datetime, unit = "10 minute")
    wd$alt <- floor(wd$alt)
    wd <- group_by(wd, datetime, lon, lat, alt) %>%
      summarise(ws = mean(ws), wd.1 = mean.cir(wd.1))
  }
  assign(paste0("wd.", flight[i]), wd)

  les <- read.csv(paste0("D:/1 PhD Studies/1 Data/LES/", flight[i], "/extract_les/meteo.vertical.profile.csv"))
  p.total <- (les$p+les$pb)/100 # hpa
  pot <- les$t+300 # kelvin
  tair <- pot*(p.total/1000)^0.286 # kelvin
  e = les$vapor*p.total/(0.622+les$vapor) # hpa
  es <- 6.11*10^(2.5*10^6/461.52*(1/273.15-1/tair)) # hpa; Clausius-Clapeyron relation.
  es.1 <- 6.1078*10^(17.2693882*(tair-273.16)/(tair-35.86)) # the equation proposed by Murray (1967)
  RH <- e/es*100
  meteo <- cbind(les, p.total, pot, tair, e, es, es.1, RH) # temp and pot.temp are in kelvin
  
  if(mean == TRUE){
    meteo$datetime <- as.POSIXct(meteo$datetime, tz = "UTC")
    meteo$datetime <- floor_date(meteo$datetime, unit = "10 minute")
    meteo$alt <- floor(meteo$alt)
    meteo <- group_by(meteo, datetime, lon, lat, alt) %>%
      summarise(pot = mean(pot), tair = mean(tair))
  }
  
  assign(paste0("meteo.", flight[i]), meteo)
  
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



############################################### plot #############################################################
my_theme <- theme_bw()+
  theme(legend.position = 'right', legend.title = element_text(size=8),
                      legend.text = element_text(size = 8), legend.margin = margin(0,0,0,0),
                      legend.spacing = unit(0,'cm'), legend.key.size = unit(2,'mm'),
                      legend.background = element_rect(fill = "transparent", colour = NA),
                      legend.key = element_rect(fill = NA, colour = NA),
                      legend.box = "vertical",
                      #legend.margin = margin(t = -8, r = 0, b = 0, l = 0, unit = "pt"),
                      axis.title = element_text(size=8, margin = margin(t = -2, r = 5, b = 5, l = 5, unit = "pt")), 
                      axis.text = element_text(size=8),
                      plot.tag = element_text(size = 8),
                      plot.title = element_text(hjust = 0.5),
                      plot.margin = margin(t = 5, r = 5, b = 1, l = 5, unit = "pt"))

# create functions for plotting
create_wd_plot <- function(df1, var_df1, df2, xlab_df2){
  
  df1$datetime <- as.character(df1$datetime)
  df2$datetime <- as.character(df2$datetime)
  ggplot()+
    # first plot
    geom_point(df1, mapping = aes(x = {{var_df1}}, y = alt, group = datetime, colour = datetime))+
    geom_path(df1, mapping = aes(x = {{var_df1}}, y = alt, group = datetime, colour = datetime))+
    scale_color_viridis_d(option = "D") + # First color scale
    scale_x_continuous(breaks = seq(0, 360, by = 15)) +
    labs(colour = "LES")+
    
    # second plot
    ggnewscale::new_scale_color()+ # Indicate a new color scale
    geom_point(df2, mapping = aes(x = value, y = alt.num, group = datetime, colour = datetime), shape = 17)+
    geom_path(df2, mapping = aes(x = value, y = alt.num, group = datetime, colour = datetime))+
    scale_color_viridis_d(option = "magma") + # second color scale
    labs(colour = "Mast", x = paste0(xlab_df2, " [deg]"), y = "Altitude [a.g.l.m]")+
    my_theme
}

create_ws_plot <- function(df1, var_df1, df2, xlab_df2, unit){
  
  df1$datetime <- as.character(df1$datetime)
  df2$datetime <- as.character(df2$datetime)
  ggplot()+
    geom_point(df1, mapping = aes(x = {{var_df1}}, y = alt, group = datetime, colour = datetime))+
    geom_path(df1, mapping = aes(x = {{var_df1}}, y = alt, group = datetime, colour = datetime))+
    scale_color_viridis_d(option = "D") + # First color scale
    scale_x_continuous(breaks = seq(1, 8, by = 1)) +
    labs(colour = "LES")+
    xlim(1.5, 8.5)+
    # Indicate a new color scale
    ggnewscale::new_scale_color()+
    geom_point(df2, mapping = aes(x = value, y = alt.num, group = datetime, colour = datetime), shape = 17)+
    geom_path(df2, mapping = aes(x = value, y = alt.num, group = datetime, colour = datetime))+
    scale_color_viridis_d(option = "magma") + # second color scale
    labs(colour = "Mast", x = expression(paste("Wind speed [m s"^{-1}, "]")), y = "Altitude [a.g.l.m]")+
    my_theme
}

create_temp_plot <- function(df1, var_df1, df2, xlab_df2){
  
  df1$datetime <- as.character(df1$datetime)
  df2$datetime <- as.character(df2$datetime)
  ggplot()+
    geom_point(df1, mapping = aes(x = {{var_df1}}, y = alt, group = datetime, colour = datetime))+
    geom_path(df1, mapping = aes(x = {{var_df1}}, y = alt, group = datetime, colour = datetime))+
    scale_color_viridis_d(option = "D") + # First color scale
    scale_x_continuous(breaks = seq(290, 300, by = 0.5)) +
    labs(colour = "LES")+
    
    # Indicate a new color scale
    ggnewscale::new_scale_color()+
    geom_point(df2, mapping = aes(x = value, y = alt.num, group = datetime, colour = datetime), shape = 17)+
    geom_path(df2, mapping = aes(x = value, y = alt.num, group = datetime, colour = datetime))+
    scale_color_viridis_d(option = "magma") + # second color scale
    labs(colour = "Mast", x = paste0(xlab_df2, " [kelvin]"), y = "Altitude [a.g.l.m]")+
    my_theme
}


# apply the functions 
p.wd.0901 <- create_wd_plot(wd.0901, wd.1, wd.mast.0901, "Wind direction")+labs(tag = "(a)")+ggtitle("0901")
p.ws.0901 <- create_ws_plot(wd.0901, ws, ws.mast.0901, "Wind speed", "m~s^-1")+labs(tag = "(d)")+ggtitle("0901")
p.pot.0901 <- create_temp_plot(meteo.0901, pot, pot.mast.0901, "Potential temperature")+labs(tag = "(g)")+ggtitle("0901")
p.temp.0901 <- create_temp_plot(meteo.0901, tair, temp.mast.0901, "Temperature")+labs(tag = "(j)")+ggtitle("0901")


p.wd.0905 <- create_wd_plot(wd.0905, wd.1, wd.mast.0905, "Wind direction")+labs(tag = "(b)")+ggtitle("0905")
p.ws.0905 <- create_ws_plot(wd.0905, ws, ws.mast.0905, "Wind speed", "m~s^-1")+labs(tag = "(e)")+ggtitle("0905")
p.pot.0905 <- create_temp_plot(meteo.0905, pot, pot.mast.0905, "Potential temperature")+labs(tag = "(h)")+ggtitle("0905")
p.temp.0905 <- create_temp_plot(meteo.0905, tair, temp.mast.0905, "Temperature")+labs(tag = "(k)")+ggtitle("0905")


p.wd.0906 <- create_wd_plot(wd.0906, wd.1, wd.mast.0906, "Wind direction")+labs(tag = "(c)")+ggtitle("0906")
p.ws.0906 <- create_ws_plot(wd.0906, ws, ws.mast.0906, "Wind speed", "m~s^-1")+labs(tag = "(f)")+ggtitle("0906")
p.pot.0906 <- create_temp_plot(meteo.0906, pot, pot.mast.0906, "Potential temperature")+labs(tag = "(i)")+ggtitle("0906")
p.temp.0906 <- create_temp_plot(meteo.0906, tair, temp.mast.0906, "Temperature")+labs(tag = "(l)")+ggtitle("0906")


###############################################################################
# organize plots
###############################################################################

library(patchwork)
# Create the individual plots
d1 <- p.wd.0901 + p.ws.0901 + p.pot.0901 + #p.temp.0901 + 
  plot_layout(guides = 'collect', nrow = 3) +
  plot_annotation(title = "0901") & 
  theme(legend.position = "bottom", legend.direction = "vertical",
        plot.tag.location = "margin", plot.tag.position = "topleft",
        plot.title = element_text(hjust = 0.5))

d2 <- p.wd.0905 + p.ws.0905 + p.pot.0905 + #p.temp.0905 + 
  plot_layout(guides = 'collect', nrow = 3) +
  plot_annotation(title = "0905") & 
  theme(legend.position = "bottom", legend.direction = "vertical",
        plot.tag.location = "margin", plot.tag.position = "topleft",
        plot.title = element_text(hjust = 0.5))

d3 <- p.wd.0906 + p.ws.0906 + p.pot.0906 + #p.temp.0906 + 
  plot_layout(guides = 'collect', nrow = 3) +
  plot_annotation(title = "0906") & 
  theme(legend.position = "bottom", legend.direction = "vertical",
        plot.tag.location = "margin", plot.tag.position = "topleft",
        plot.title = element_text(hjust = 0.5))

# Combine the plots with titles
final_plot <- (d1 | d2 | d3) +
  plot_layout(widths = c(1, 1, 1))

while (!is.null(dev.list()))  dev.off()
ggsave("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/Figures/SI/meteo_comparison.1.tiff", plot = final_plot, 
       width = 250, height = 300, units = "mm", dpi = 300, compression = "lzw")
