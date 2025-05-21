

rm(list = ls())
gc()


# Core spatial and mapping packages
library(sf)         # For spatial features handling
library(ggmap)      # For map visualization using ggplot
library(dplyr)      # For data manipulation (filter, %>%, etc.)
library(geodata)    # For accessing GADM data
library(scico)      # For color palettes (used in p2)

# Additional required packages for specific functions
library(ggplot2)    # For plotting (though often loaded with ggmap)
library(units)      # For unit handling in sf objects
library(cowplot)
library(grid)
library(gridExtra)


nl_satellite <- readRDS("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/Figures/main_body/xin_plot/Netherland.rds")

# Define the bounding box in WGS84 coordinates
nl_box <- c(
  left = 3.35, bottom = 50.5, right = 7.3, top = 52.2
)

# Get Netherlands data from GADM and convert to sf
d <- gadm("NLD", level=2, path="C:/Users/xin/Desktop/", version="latest")

# Convert to sf and ensure WGS84 projection (EPSG:4326)
d_sf <- st_as_sf(d) %>% st_transform(4326)

# Create merged polygon for all Netherlands
d_merged <- d_sf %>% st_union() %>% st_make_valid() %>% st_as_sf()

# Extract Rotterdam
rotterdam <- d_sf %>% filter(NAME_2 == "Rotterdam")

# Apply small shift to fix alignment issues
# d_merged <- d_merged %>% st_geometry() %>% {. + cbind(0, -0.025)} %>% st_sf(geometry = .)
# rotterdam <- rotterdam %>% st_geometry() %>% {. + cbind(0, -0.0255)} %>% st_sf(geometry = .)

# Create the map
p1 <- ggmap(nl_satellite) +
  geom_sf(data = d_merged, fill = NA, color = "#7D8EDC", lwd = 0.8, inherit.aes = FALSE) +  
  geom_sf(data = rotterdam, fill = "#0072B2", color = "#0072B2", alpha = 0.5,
          lwd = 0.8, inherit.aes = FALSE) +  
  labs(title = "Satellite Map with Rotterdam Highlighted", tag = "(a)") +
  coord_sf(xlim = c(3.8, 7.1), ylim = c(50.8, 53.8)) + 
  scale_x_continuous(labels = ~paste0(format(abs(.), nsmall = 1), "°", ifelse(. > 0, "E", "W"))) + 
  scale_y_continuous(labels = ~paste0(format(abs(.), nsmall = 1), "°", ifelse(. > 0, "N", "S"))) + 
  theme_minimal() +
  theme(
    aspect.ratio = 1,
    axis.title = element_blank(),
    axis.ticks.length = unit(0.05, "cm"),
    axis.ticks = element_line(color = "black", size = 0.5),
    axis.text = element_text(size = 8, color = "black")
  )


# load data for panel (b)
rotterdam_satellite <- readRDS("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/Figures/main_body/xin_plot/rotterdam.rds")
flight.0906 <- read.csv("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/Figures/main_body/xin_plot/flight_trajectory.csv")
flight.0906 <- mutate(flight.0906, ws=rWind::uv2ds(U,V)[,2], wd=rWind::uv2ds(U,V)[,1])
flight.0906 <- flight.0906[, c("lon","lat","alt", "N2O.ac.ppb", "ws", "wd")]
wd <- mean.cir(flight.0906$wd[which(flight.0906$alt<1000)])  # calculate average wd below the CBL
ws <- mean(flight.0906$ws[which(flight.0906$alt<1000)], na.rm = TRUE)  # calculate average ws below the CBL

wind_data <- data.frame(
  station_x = 4.0,    # Station locations
  station_y = 52.05,    
  wind_dir = wd,  # Wind direction in degrees (from North)
  wind_speed = ws    # Wind speed (for arrow length)
)

# Calculate end points based on direction and speed
# Scale factor for arrow length (adjust as needed)
scale_factor <- 0.04

wind_data$xend <- wind_data$station_x + 
  sin(wind_data$wind_dir * pi/180) * wind_data$wind_speed * scale_factor
wind_data$yend <- wind_data$station_y + 
  cos(wind_data$wind_dir * pi/180) * wind_data$wind_speed * scale_factor




text_data <- data.frame(
  x = c(4, 4.2, 4.7),
  y = c(51.9, 51.9, 51.9),
  label = c("transect 1", "transect 2", "transect 6")
  )

p2 <- ggmap(rotterdam_satellite) +
  geom_sf(
    data = rotterdam, 
    fill = "#0072B2", 
    color = "#0072B2", 
    alpha = 0.65, 
    inherit.aes = FALSE
  ) +
  geom_path(
    data = flight.0906,
    mapping = aes(x = lon, y = lat, color = alt),
    size = 1.5
  ) +
  geom_segment(wind_data, mapping = aes(x = station_x, y = station_y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_text(
    text_data, 
    mapping = aes(x = x, y = y, label = label),
    angle = -55
  ) +
  annotate("text", x = 4.1, y = 51.95, 
           label = "WIA", fontface = "bold")+
  coord_sf(xlim = c(3.9, 5), ylim = c(51.8, 52.1)) +
  scale_color_scico(
    name = expression(paste("Alt (", "m", ")")),
    palette = 'bam',
    na.value = NA
  ) +
  labs(
    title = "Flight trajectory Information",
    tag = "(b)"
  ) +
  scale_x_continuous(
    labels = ~paste0(format(abs(.), nsmall = 1), "°", ifelse(. > 0, "E", "W"))
  ) + 
  scale_y_continuous(
    labels = ~paste0(format(abs(.), nsmall = 1), "°", ifelse(. > 0, "N", "S"))
  ) + 
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.ticks.length = unit(0.05, "cm"),
    axis.ticks = element_line(color = "black", size = 0.5),
    axis.text = element_text(size = 8, color = "black")
  )



################## plot arrows ##############################
arrowA <- data.frame(x1 = 1.6, x2 = 1.7, y1 = 0.98 - 0.08, y2 = 1 - 0.08)
arrowB <- data.frame(x1 = 1.6, x2 = 1.7, y1 = 0.16 - 0.1, y2 = 0.14 - 0.1)
################################################
p3 <- ggplot() +
  coord_equal(xlim = c(0, 3.3), ylim = c(-0.5, 3), expand = FALSE) +
  geom_segment(aes(x = 0.07, xend = 3, y = 0.73 - 0.07, yend = 1.98 - 0.07),
               data = arrowA, 
               color = "#0072B2",
               arrow = arrow(length = unit(0.1, "inches")), lineend = "round",
               color = "black", size = 1) +
  geom_segment(aes(x = 0.07, xend = 3, y = 0.7 - 0.07, yend = -0.06 - 0.01), 
               data = arrowB, 
               arrow = arrow(length = unit(0.1, "inches")), lineend = "round",
               color = "#0072B2",
               color = "black", size = 1) +
  theme_void()






##################################################### combine three plots ###########################################################

tiff(
  filename = 'c:/users/xin/desktop/p1.tiff',
  units = "mm",
  width = 300,      
  height = 100,      # Slightly reduced height due to better space usage
  res = 300,
  compression = "lzw"
)

# Add before creating the plots
p1 <- p1 + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
p2 <- p2 + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))


vp_1 <- viewport(x = 0.2, y = 0.5, height = 0.8, 
                 just = "center")
pushViewport(vp_1)
print(p1, newpage = FALSE)

## insert colorkey
popViewport()
vp_2 <-  viewport(x = 0.7, y = 0.5, height = 0.8, 
                  just = "center")
pushViewport(vp_2)
print(p2, newpage = FALSE)

## insert colorkey
popViewport()
vp_3 <-  viewport(x = 0.18, y = 0.55, width = 0.25, height = 1,
                  just = "left")
pushViewport(vp_3)
print(p3, newpage = FALSE)

while (!is.null(dev.list()))  dev.off()




















































