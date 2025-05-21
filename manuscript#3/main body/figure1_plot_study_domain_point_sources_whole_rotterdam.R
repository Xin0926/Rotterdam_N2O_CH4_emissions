# This script is for plotting the study domain
# author: Xin Tong
# time: 2023-11-29





# LOAD BOUNDARIES OF MUNICIPALITIES
library(geodata) # gadm
d <- gadm("Netherlands", level=2, path="c:/users/xin/", version="latest", resolution=1)
data <- as.data.frame(d)

Rotterdam <- geom(d[which(data$NAME_2=="Rotterdam"),], df = TRUE)
Barendrecht <- geom(d[which(data$NAME_2=="Barendrecht"),], df = TRUE)
Botlek.Rotterdam <- geom(d[which(data$NAME_2=="Botlek Rotterdam"),], df = TRUE)
Brielle <- geom(d[which(data$NAME_2=="Brielle"),], df = TRUE)
Capelle.aan.den.IJssel <- geom(d[which(data$NAME_2=="Capelle aan den IJssel"),], df = TRUE)
Europoort.Rotterdam <- geom(d[which(data$NAME_2=="Europoort Rotterdam"),], df = TRUE)
Hook.of.Holland <- geom(d[which(data$NAME_2=="Hook of Holland"),], df = TRUE)
Hoogvliet.Rotterdam <- geom(d[which(data$NAME_2=="Hoogvliet Rotterdam"),], df = TRUE)
Maasvlakte.Rotterdam <- geom(d[which(data$NAME_2=="Maasvlakte Rotterdam"),], df = TRUE)
Maasdijk <- geom(d[which(data$NAME_2=="Maasdijk"),], df = TRUE)
Maassluis <- geom(d[which(data$NAME_2=="Maassluis"),], df = TRUE)
Pernis <- geom(d[which(data$NAME_2=="Pernis"),], df = TRUE)
Ridderkerk <- geom(d[which(data$NAME_2=="Ridderkerk"),], df = TRUE)
Rozenburg <- geom(d[which(data$NAME_2=="Rozenburg"),], df = TRUE)
Schiedam <- geom(d[which(data$NAME_2=="Schiedam"),], df = TRUE)
Spijkenisse <- geom(d[which(data$NAME_2=="Spijkenisse"),], df = TRUE)
Vlaardingen <- geom(d[which(data$NAME_2=="Vlaardingen"),], df = TRUE)
Vondelingenplaat <- geom(d[which(data$NAME_2=="Vondelingenplaat"),], df = TRUE)
Westland <- geom(d[which(data$NAME_2=="Westland"),], df = TRUE)

# text_data <- data.frame(
#   x = c(
#     mean(Rotterdam$x), mean(Barendrecht$x), mean(Botlek.Rotterdam$x), mean(Brielle$x), mean(Capelle.aan.den.IJssel$x), 
#     mean(Europoort.Rotterdam$x), mean(Hook.of.Holland$x), mean(Hoogvliet.Rotterdam$x), mean(Maasvlakte.Rotterdam$x), 
#     mean(Maasdijk$x), mean(Maassluis$x), mean(Pernis$x), mean(Ridderkerk$x), mean(Rozenburg$x), mean(Schiedam$x), 
#     mean(Spijkenisse$x), mean(Vlaardingen$x), mean(Vondelingenplaat$x), mean(Westland$x)
#   ),
#   y = c(
#     mean(Rotterdam$y), mean(Barendrecht$y), mean(Botlek.Rotterdam$y), mean(Brielle$y), mean(Capelle.aan.den.IJssel$y), 
#     mean(Europoort.Rotterdam$y), mean(Hook.of.Holland$y), mean(Hoogvliet.Rotterdam$y), mean(Maasvlakte.Rotterdam$y), 
#     mean(Maasdijk$y), mean(Maassluis$y), mean(Pernis$y), mean(Ridderkerk$y), mean(Rozenburg$y), mean(Schiedam$y), 
#     mean(Spijkenisse$y), mean(Vlaardingen$y), mean(Vondelingenplaat$y), mean(Westland$y)
#   ),
#   label = c( 
#     "Rotterdam", "Barendrecht", "Botlek Rotterdam", "Brielle", "Capelle aan den IJssel", "Europoort Rotterdam", "Hook of Holland",
#     "Hoogvliet Rotterdam", "Maasvlakte Rotterdam", "Maasdijk", "Maassluis", "Pernis", "Ridderkerk", "Rozenburg", "Schiedam",
#     "Spijkenisse", "Vlaardingen", "Vondelingenplaat", "Westland"
#   )
# )
text_data <- data.frame(
  label = c("The Maasvlakte", "horticulture"),
  x = c(4.0309, 4.2137),
  y = c(51.9617, 52.0098)
)


# LOAD FLIGHT PATH
flight.0906 <- read.csv("D:/1 phd studies/1 data/Rotterdam_campaign_2022/0906/AirCore/Aircore_retrieval/AirCore_N2O_CO_CO2_CH4_CO_20220906.csv")
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))


# LOAD four locations of wind measurements
WIND <- read.csv("D:/1 PhD Studies/1 Data/processed/Aerodyne_Tildas/Wind_Measurement_Location.txt", sep = ",")


# LOAD POINT SOURCES
WWTP <- read.csv("D:/1 PhD Studies/1 Data/Inventory/Dutch_Inventory/N2O_CH4_CO2_CO_emission_Rotterdam_area_company_2020_2021_2022.csv")
condition <- (WWTP$Stof=="Distikstofoxide" | WWTP$Stof== "Methaan") & WWTP$Jaar==2020
WWTP <- subset(WWTP, condition)
WWTP <- group_by(WWTP, lon, lat, Sector, Bedrijf, Stof) %>% summarise(emission = sum(Emissie)); 
WWTP <- as.data.frame(WWTP)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# API_key <- # check the "D:\1 PhD Studies\3.1 Results of projects\2022_Rotterdam_Campaign\Rotterdam_Project_Journal.docx"
# register_stadiamaps(API_key, write = FALSE)

bbox <- c(left = 3.773, bottom = 51.8, right = 4.8, top = 52.1335)

p.n2o <- ggmap(get_stadiamap(bbox, maptype = "stamen_terrain"))+ # base map
  geom_polygon(Rotterdam, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.6)+
  geom_polygon(Barendrecht, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.2)+
  geom_polygon(Botlek.Rotterdam, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Brielle, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Capelle.aan.den.IJssel, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Europoort.Rotterdam, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Hook.of.Holland, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Hoogvliet.Rotterdam, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Maasvlakte.Rotterdam, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Maasdijk, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Maassluis, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.2)+
  geom_polygon(Pernis, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Ridderkerk, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.2)+
  geom_polygon(Rozenburg, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.2)+
  geom_polygon(Schiedam, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Spijkenisse, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Vlaardingen, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Vondelingenplaat, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Westland, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.6)+
  
  # add texts to the areas
  geom_text(text_data, mapping = aes(x = x, y = y, label = label), colour = 'black', size = 3)+
  
  # add flight trajectories
  # geom_point(flight.0906, mapping = aes(x = lon, y = lat, colour = N2O.ac.ppb), size = 0.5)+
  # scale_colour_gradientn(colours = myPalette(10))+
  # labs(colour = bquote(''*N[2]*O*' [ppb]'))+
  
  # add four locations for wind measurments
  geom_point(WIND, mapping = aes(x = lon, y = lat), colour = "black", shape = 'diamond')+
  
  # add point sources
  geom_point(WWTP[WWTP$Stof=="Distikstofoxide", ], mapping = aes(x = lon, y = lat, shape = Sector), colour = "purple")+
  scale_shape_manual(values = c(1:8))+
  labs(size = bquote(''*N[2]*O*' Emission ['*kg~yr^-1*']'), tag = "(b)")+
  theme_classic()+
  theme_bw()+
  theme(legend.position = "right", 
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        legend.margin = margin(t = -8, r = 0, b = 0, l = 0, unit = "pt"),
        legend.spacing = unit(0.1, "cm"), legend.key.size = unit(0.1, "cm"),
        axis.title = element_text(size=8), 
        plot.tag = element_text(size = 8),
        plot.margin = margin(t = -5, r = 5, b = -5, l = 5, unit = "pt"))

  


p.ch4 <- ggmap(get_stadiamap(bbox, maptype = "stamen_terrain"))+ # base map
  geom_polygon(Rotterdam, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.6)+
  geom_polygon(Barendrecht, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.2)+
  geom_polygon(Botlek.Rotterdam, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Brielle, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Capelle.aan.den.IJssel, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Europoort.Rotterdam, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Hook.of.Holland, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Hoogvliet.Rotterdam, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Maasvlakte.Rotterdam, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Maasdijk, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Maassluis, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.2)+
  geom_polygon(Pernis, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Ridderkerk, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.2)+
  geom_polygon(Rozenburg, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.2)+
  geom_polygon(Schiedam, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Spijkenisse, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Vlaardingen, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Vondelingenplaat, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.4)+
  geom_polygon(Westland, mapping = aes(x = x, y = y), fill = 'darkgrey', colour='grey', alpha = 0.6)+
  
  # add texts to the areas
  geom_text(text_data, mapping = aes(x = x, y = y, label = label), colour = 'black', size = 3)+
  
  # add flight trajectories
  # geom_point(flight.0906, mapping = aes(x = lon, y = lat, colour = CH4.ac.ppb), size = 0.5)+
  # scale_colour_gradientn(colours = myPalette(10))+
  # labs(colour = bquote(''*CH[4]*' [ppb]'))+
  
  # add four locations for wind measurments
  geom_point(WIND, mapping = aes(x = lon, y = lat), colour = "black", shape = 'diamond')+
  
  # add point sources
  geom_point(WWTP[WWTP$Stof=="Methaan", ], mapping = aes(x = lon, y = lat, shape = Sector), colour = "purple")+
  scale_shape_manual(values = c(1:8))+
  labs(size = bquote(''*CH[4]*' Emission ['*kg~yr^-1*']'), tag = "(c)")+
  
  theme_classic()+
  theme_bw()+
  theme(legend.position = "right", 
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        legend.margin = margin(t = -8, r = 0, b = 0, l = 0, unit = "pt"),
        legend.spacing = unit(0.1, "cm"), legend.key.size = unit(0.1, "cm"),
        axis.title = element_text(size=8), 
        plot.tag = element_text(size = 8),
        plot.margin = margin(t = -5, r = 5, b = -5, l = 5, unit = "pt"))




tiff("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/Figures/figure1.tiff", units="mm", width=150, height=100, res=300)
grid.arrange(p.n2o, p.ch4, nrow = 2)
while (!is.null(dev.list()))  dev.off()

  
  
