
# the code is for plotting the point sources' emissions of energy, refineries, and industrial 
# over the MIA and Rotterdam area for both N2O and CH4 in the SI of chapter_3
# author: Xin Tong
# time: August 26, 2024


data <- read.csv("D:/1 PhD Studies/1 Data/Inventory/Dutch_inventory/N2O_CH4_CO2_CO_emission_Rotterdam_area_company_2020_2021_2022.csv")

data <- mutate(data, emission = Emissie/366/24) # convert unit from kg/yr to kg/hr

# select data by conditions for n2o
condition <- data$Stof=="Distikstofoxide" & 
  data$Jaar=="2020" & 
  (data$Sector=="Chemical Industry"|data$Sector=="Energy"|data$Sector=="Refineries")

MIA.n2o <- group_by(data[condition
                         & data$lat<52.0088 & data$lat>51.9
                         & data$lon<4.2 & data$lon>3.945, ], lon, lat, Sector) %>% 
  summarise(emission = sum(emission))
MIA.n2o <- as.data.frame(MIA.n2o)
MIA.n2o$area <- "Maasvlakte"

ROT.n2o <- group_by(data[condition,], lon, lat, Sector) %>% 
  summarise(emission = sum(emission))
ROT.n2o <- as.data.frame(ROT.n2o)
ROT.n2o$area <- "Rotterdam"


# select data by conditions for ch4
condition <- data$Stof=="Methaan" & 
  data$Jaar=="2020" &
  (data$Sector=="Chemical Industry"|data$Sector=="Energy"|data$Sector=="Refineries")

MIA.ch4 <- group_by(data[condition
                         & data$lat<52.0088 & data$lat>51.9
                         & data$lon<4.2 & data$lon>3.945, ], lon, lat, Sector) %>% 
  summarise(emission = sum(emission))
MIA.ch4 <- as.data.frame(MIA.ch4)
MIA.ch4$area <- "Maasvlakte"

ROT.ch4 <- group_by(data[condition,], lon, lat, Sector) %>% 
  summarise(emission = sum(emission))
ROT.ch4 <- as.data.frame(ROT.ch4)
ROT.ch4$area <- "Rotterdam"


# plot
p.n2o <- ggplot()+
  geom_bar(MIA.n2o, mapping = aes(x = area, y = emission, fill = Sector), stat = "identity")+
  geom_bar(ROT.n2o, mapping = aes(x = area, y = emission, fill = Sector), stat = "identity")+
  scale_fill_viridis(discrete = TRUE) + 
  labs(, y = bquote(''*N[2]*O*' Emission ['*kg~hr^-1*']'), tag = "(a)")+
  theme_classic()+
  theme_bw()+
  theme(legend.position = 'none',  
        legend.title = element_blank(),
        legend.text = element_text(size=8),
        legend.margin = margin(t = -8, r = 0, b = 0, l = 0, unit = "pt"),
        axis.title.x = element_blank(),
        plot.tag = element_text(size = 8),
        plot.margin = margin(t = 0, r = 5, b = 0, l = 5, unit = "pt"))

p.ch4 <- ggplot()+
  geom_bar(MIA.ch4, mapping = aes(x = area, y = emission, fill = Sector), stat = "identity")+
  geom_bar(ROT.ch4, mapping = aes(x = area, y = emission, fill = Sector), stat = "identity")+
  scale_fill_viridis(discrete = TRUE)+ 
  labs(y = bquote(''*CH[4]*' Emission ['*kg~hr^-1*']'), tag = "(b)")+
  theme_classic()+
  theme_bw()+
  theme(legend.position = c(0.25, 0.75), 
        legend.title = element_blank(),
        legend.text = element_text(size=8),
        legend.margin = margin(t = -8, r = 0, b = 0, l = 0, unit = "pt"),
        axis.title.x = element_blank(),
        plot.tag = element_text(size = 8),
        plot.margin = margin(t = 0, r = 5, b = 0, l = 5, unit = "pt"))

#tiff("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/Figures/SI/point_source_emi_barplot.tiff", units="mm", width=300, height=150, res=300)
grid.arrange(p.n2o, p.ch4, ncol=2)
#while (!is.null(dev.list()))  dev.off()























########################### subsequent analysis for deriving the emissions for Rotterdam ################################

# n2o emi for energy and refinery
# 1. upscaling
les <- c(120, 144, 42, 49, 44, 54)
les <- 75
f <- les/sum(MIA.n2o$emission)
print(sum(ROT.n2o$emission)*f)
# 2. replacing
les+sum(ROT.n2o$emission[5:11])



# CH4 emi for 
# 1. upscaling
les <- 1600
f <- les/sum(MIA.ch4$emission)
print(sum(ROT.ch4$emission)*f)
# 2. replacing
les+sum(ROT.ch4$emission[(nrow(MIA.ch4)+1):nrow(ROT.ch4)])
















