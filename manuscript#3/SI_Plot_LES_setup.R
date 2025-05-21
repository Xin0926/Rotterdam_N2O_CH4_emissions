data.new <- read.csv("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/updated_inventory.csv")
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

n2o.inventory <- data.new[which(data.new$Stof=="Distikstofoxide" & data.new$V1<4.75), ]
names(n2o.inventory)[names(n2o.inventory)=="V1"] <- "lon";
names(n2o.inventory)[names(n2o.inventory)=="V2"] <- "lat"

condition <- n2o.inventory$Sector=="Refineries" | n2o.inventory$Sector=="Agriculture" | 
  n2o.inventory$Sector=="Sewerage and wastewater treatment plants" | n2o.inventory$Sector=="Waste Disposal"|
  n2o.inventory$Sector=="Energy" | n2o.inventory$Sector=="Chemical Industry"
data <- n2o.inventory[condition,]
data$Emissie <- data$Emissie/366/24 # convert unit from kg/yr to kg/hr

n2o.lim <- c(floor(min(data$Emissie)), ceiling(max(data$Emissie)) )
breaks <- floor(seq(from = min(data$Emissie), to = max(data$Emissie), length.out = 4))
#labels <- format(breaks, scientific = TRUE, digits=1)


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# LOAD CITY BOUNDARY
library(geodata) 
d <- gadm("Netherlands", level=2, path="c:/users/xin09/desktop/", version="latest", resolution=1);
utrecht <- geom(d[281,], df = TRUE);
groningen <- geom(d[89,], df = TRUE);
rotterdam <- geom(d[341,], df = TRUE) 



#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# LOAD FLIGHT PATH
flight.0906 <- read.csv("D:/1 phd studies/1 data/Rotterdam_campaign_2022/0906/AirCore/Aircore_retrieval/AirCore_N2O_CO_CO2_CH4_CO_20220906.csv")


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# LOAD POINT SOURCES
point.source <- read.csv("D:/1 PhD Studies/1 Data/Inventory/Dutch_inventory/N2O_CH4_CO2_CO_emission_Rotterdam_area_company_2020_2021_2022.csv")

point.source <- mutate(point.source, emission = Emissie/366/24) # convert unit from kg/yr to kg/hr

# select data by conditions for n2o
condition <- point.source$Stof=="Distikstofoxide" & 
  point.source$Jaar=="2020" & 
  (point.source$Sector=="Chemical Industry"|point.source$Sector=="Energy"|point.source$Sector=="Refineries"|
     point.source$Sector=="Sewerage and wastewater treatment plants"|point.source$Sector=="Waste Disposal") &
  point.source$lon > 3.9 & point.source$lon < 4.2 & point.source$lat > 51.84 & point.source$lat < 51.96
point.source <- group_by(point.source[condition, ], lon, lat, Sector) %>% 
  summarise(emission = sum(emission))


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# create a boundary for LES domain
les <- matrix(
  c(3.88925170898438, 4.25576782226562, 4.25576782226562, 3.88925170898438, 51.8415908813477, 51.8415908813477, 52.0670433044434, 52.0670433044434), 
  nrow = 4, ncol=2)
les <- as.data.frame(les)
colnames(les) <- c("lon", "lat")




################################# PLOT #################################

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))


p.n2o <- ggplot()+
  #geom_tile(data, alpha = 0.5, mapping = aes(x=lon, y=lat, fill = Emissie), width = 0.07308, height = 0.0449)+ # set the width and height in geom_tile
  #coord_fixed(xlim=lon.lim, ylim=lat.lim)+
  #scale_fill_gradientn(colours = myPalette(10), limits = n2o.lim, breaks = breaks, labels = breaks)+
  labs( fill= bquote(''*N[2]*O*' total emission ['*kg~h^-1*']'), x = 'Longitude [deg]', y = 'Latitude [deg]' )+
  
  geom_polygon(rotterdam, mapping = aes(x = x, y = y), fill = 'black', colour='black', alpha = 0.1)+
  
  ggnewscale::new_scale_fill() +
  geom_point(point.source, mapping = aes(x = lon, y = lat, size = emission, colour = Sector), alpha = 0.6)+
  scale_colour_viridis(discrete = TRUE)+
  labs(size = bquote(''*N[2]*O*' point source emission ['*kg~h^-1*']'))+
  
  geom_polygon(les, mapping = aes(x = lon, y = lat), fill = 'black', colour='black', alpha = 0.1, linetype = 2)+
  
  #facet_wrap(~Sector, ncol=2)+
  
  theme_bw()+
  theme(legend.position = 'bottom', 
        legend.text = element_text(), legend.margin = margin(0,0,0,0),
        legend.spacing.x = unit(0.5,'cm'), legend.key.size = unit(2,'mm'),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = NA, colour = NA),
        axis.title = element_text(),
        axis.text = element_text(),
        plot.tag = element_text(),
        plot.margin = margin(t = 5, r = 5, b = 1, l = 5, unit = "pt"),
        strip.background = element_rect(fill = NA, colour = NA))

tiff("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/Figures/SI/'les_setup.1.tiff", units="mm", width=180, height=100, res=300)
p.n2o
while (!is.null(dev.list()))  dev.off()

