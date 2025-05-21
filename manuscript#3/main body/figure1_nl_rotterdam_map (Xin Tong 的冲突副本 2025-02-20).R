# Load libraries
library(ggplot2)
library(ggmap)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
library(geodata)
library(dplyr)

# Define the bounding box in WGS84 coordinates
nl_box <- c(
  left = 2.96, bottom = 50.56, right = 7.5, top = 53.86
)

# Get Rotterdam data from GADM and convert to sf
d <- gadm("Netherlands", level=2, path="c:/users/xin/", version="latest", resolution=1)
# Convert to sf object and ensure it's in WGS84 (EPSG:4326)
d_sf <- st_as_sf(d) %>%
  st_transform(4326)  # Ensure WGS84 projection
# Extract Rotterdam
rotterdam <- d_sf[d_sf$NAME_2 == "Rotterdam", ]

# Get the stadia map
nl_satellite <- get_stadiamap(
  bbox = nl_box,
  zoom = 7,
  maptype = "stamen_terrain"
)

# Create the map
ggmap(nl_satellite, extent = "normal") +  # Add extent = "normal"
  geom_sf(data = rotterdam,
          inherit.aes = FALSE,
          fill = 'red',
          color = 'black',
          alpha = 0.6) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10)
  ) +
  labs(
    title = "Satellite Map of the Netherlands",
    subtitle = "Terrain and satellite imagery with Rotterdam boundary"
  ) +
  coord_sf(crs = 4326, default_crs = sf::st_crs(4326))  # Specify both CRS parameters