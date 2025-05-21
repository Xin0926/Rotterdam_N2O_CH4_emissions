# the code is for comparing the emissions from Maasvlakte industrial areas and from the two point sources reported in teh dutch inventory
# Author: Xin Tong
# Time: september 24, 2023





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD DUTCH INVENTORY FOR POINT SOURCES
WWTP <- read.csv("D:/1 PhD Studies/1 Data/Inventory/Dutch_Inventory/N2O_CH4_CO2_CO_emission_Rotterdam_area_company_2020_2021_2022.csv")
# change the sector name from Dutch to English 
WWTP$Sector[which(WWTP$Sector=='Bouw')] <- "Construction" 
WWTP$Sector[which(WWTP$Sector=="Energiesector")] <- "Energy"
WWTP$Sector[which(WWTP$Sector=="Afvalverwijdering")] <- "Waste Disposal"
WWTP$Sector[which(WWTP$Sector=="Overige industrie")] <- "Other industry" 
WWTP$Sector[which(WWTP$Sector=="Consumenten")] <- "Consumers"
WWTP$Sector[which(WWTP$Sector=="Natuur")] <- "Nature" 
WWTP$Sector[which(WWTP$Sector=="Chemische Industrie")] <- "Chemical Industry"
WWTP$Sector[which(WWTP$Sector=="Riolering en waterzuiveringsinstallaties")] <- "Sewerage and wastewater treatment plants"
WWTP$Sector[which(WWTP$Sector=="Verkeer en vervoer")] <- "Traffic and Transportation"
WWTP$Sector[which(WWTP$Sector=="Landbouw")] <- "Agriculture"
WWTP$Sector[which(WWTP$Sector=="Handel, Diensten en Overheid (HDO)")] <- "Trade, Services and Government (HDO)."
WWTP$Sector[which(WWTP$Sector=="Drinkwatervoorziening")] <- "Drinking Water Supply"
WWTP$Sector[which(WWTP$Sector=="Raffinaderijen")] <- "Refineries"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~FOR N2O~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PICK UP N2O emissions in the Maasvlakte industrial area from the dutch inventory
condition <- #WWTP$Stof=="Distikstofoxide" & 
  WWTP$Stof=="Methaan" &
  WWTP$lon > 4.0 & WWTP$lon < 4.15 & 
  WWTP$lat > 51.92 & WWTP$lat < 51.98 & 
  WWTP$Jaar==2020

df <- subset(WWTP, condition)
df$Emissie <- df$Emissie/366/24 # convert the unit of kg/yr to kg/hr

# LOAD MASS BALANCE ESTIMATES (mol/s) FOR the Maasvlakte industrial area
flux <- data.frame(
  flight = c("0830", "0901", "0905", "0906"),
  emi = c(0.7, 0.7, 2.3, 0.4),
  uncertainty = c(0.5, 0.4, 0.8, 0.3),
  flag = "Top-down"
)

P.N2O <- ggplot()+
  # plot bar for dutch inventory
  geom_bar(df, mapping = aes(x=Gebied, group=Sector, fill=Sector, y=Emissie), stat = 'identity', width = 0.5)+
  labs(fill = 'Inventory') +
  scale_fill_viridis_d()+
  
  # plot bar for mass balance flux
  ggnewscale::new_scale_fill() +
  geom_bar(flux, mapping = aes(x = flight, y=emi*44/1000*3600, fill=flag), alpha=0.6, stat = 'identity', width = 0.5)+
  geom_errorbar(flux, width = 0.15, show.legend=FALSE, mapping=aes(x=flight, ymax = emi*44/1000*3600+uncertainty*44/1000*3600, ymin = emi*44/1000*3600-uncertainty*44/1000*3600), colour = "skyblue")+
  scale_fill_manual(name = "Mass balance flux", labels = "", values = "skyblue") +
  theme_bw()+
  theme(
    axis.title.x = element_blank(),              
    legend.position = c(0.8, 0.8),
    legend.text = element_text(size =8),
    legend.spacing.y = unit(0,'cm'),legend.key.size = unit(0.2,'cm'),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = NA, colour = NA))+
  ylab( bquote(''*N[2]*O*' emission ['*kg/hr*']') )

# SAVE FIGURE
tiff("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/Figures/top-down&inventory_Maasvlakte.tiff", units="mm", width=200, height=75, res=300)
  P.N2O
while (!is.null(dev.list()))  dev.off()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~modified time: 2023-11-26~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # PREPARATION for gridmap: run the script in the rmarkdown file "D:\1 PhD Studies\1 Data\Inventory\The research about the registered emissions in the Dutch website.Rmd"
  # UNIT OF INVENTORY: kg/yr
  #!!!!!!!!!!!!!!!!!!!!!!!!!!
  #data.old <- data
  #!!!!!!!!!!!!!!!!!!!!!!!!!!
  #!!!!!!!!!!!!!!!!!!!!!!!!!!
  data.new <- data
  #!!!!!!!!!!!!!!!!!!!!!!!!!!
  # old inventory and updated inventory do not have a significant difference for the harbour area
  # condition <- data.old$Stof=="Methaan" & data.old$V1 > 4.0 & data.old$V1 < 4.15 & data.old$V2 > 51.92 & data.old$V2 < 51.98
  condition <- data.new$Stof=="Methaan" & data.new$V1 > 4.0 & data.new$V1 < 4.2 & data.new$V2 > 51.9 & data.new$V2 < 52
  df.ch4 <- subset(data.new, condition)
  sum(df.ch4$Emissie)/365/24
  
  
  condition <- data.new$Stof=="Distikstofoxide" & data.new$V1 > 4.0 & data.new$V1 < 4.2 & data.new$V2 > 51.9 & data.new$V2 < 52
  #condition <- data.old$Stof=="Distikstofoxide" & data.old$V1 > 4.0 & data.old$V1 < 4.15 & data.old$V2 > 51.92 & data.old$V2 < 51.98
  df.n2o <- subset(data.new, condition)
  sum(df.n2o$Emissie)/365/24
  
  
  # LOAD DATA SHOWING BOTH MBA estimates and inventories
  df.n2o <- readxl::read_excel("D:/1 PhD Studies/3.1 Results of projects/2022_Rotterdam_Campaign/Harbour_emission.xlsx", sheet=1);
  df.n2o <- as.data.frame(df.n2o)
  df.ch4 <- readxl::read_excel("D:/1 PhD Studies/3.1 Results of projects/2022_Rotterdam_Campaign/Harbour_emission.xlsx", sheet=3);
  df.ch4 <- as.data.frame(df.ch4)
  
  df.n2o$n2o.uncertainty <- as.numeric(df.n2o$n2o.uncertainty)
  df.ch4$ch4.uncertainty <- as.numeric(df.ch4$ch4.uncertainty)
  
  p.n2o <- ggplot(mapping = aes(x = flag))+
    geom_bar(df.n2o[df.n2o$flag=="top-down estimates",], mapping = aes(y = n2o.emi*44*3.6), fill = "indianred2",  alpha = 0.8, stat = "identity", position = position_dodge2())+
    geom_errorbar(df.n2o[df.n2o$flag=="top-down estimates",], 
                  mapping = aes(ymax = n2o.emi*44*3.6 + n2o.uncertainty*44*3.6, ymin = n2o.emi*44*3.6 - n2o.uncertainty*44*3.6), 
                  colour = "indianred2", stat = "identity", position = position_dodge2())+
    geom_bar(df.n2o[df.n2o$flag=="inventory",], mapping = aes(y = n2o.emi), fill = "gold", stat = "identity", position = position_dodge2())+
    theme_bw()+
    theme(
      axis.title.x = element_blank(),              
      legend.spacing.y = unit(0,'cm'),legend.key.size = unit(0.2,'cm'),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = NA, colour = NA))+
    ylab( bquote(''*N[2]*O*' emission ['*kg~h-1*']') )
  
  p.ch4 <- ggplot(mapping = aes(x = flag))+
    geom_bar(df.ch4[df.ch4$flag=="top-down estimates",], mapping = aes(y = ch4.emi*44*3.6), fill = "indianred2",  alpha = 0.8, stat = "identity", position = position_dodge2())+
    geom_errorbar(df.ch4[df.ch4$flag=="top-down estimates",], 
                  mapping = aes(ymax = ch4.emi*44*3.6 + ch4.uncertainty*44*3.6, ymin = ch4.emi*44*3.6 - ch4.uncertainty*44*3.6), 
                  colour = "indianred2", stat = "identity", position = position_dodge2())+
    geom_bar(df.ch4[df.ch4$flag=="inventory",], mapping = aes(y = ch4.emi), fill = "gold", stat = "identity", position = position_dodge2())+
    theme_bw()+
    theme(
      axis.title.x = element_blank(),              
      legend.spacing.y = unit(0,'cm'),legend.key.size = unit(0.2,'cm'),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = NA, colour = NA))+
    ylab( bquote(''*CH[4]*' emission ['*kg~h-1*']') )
  
  tiff("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/Figures/harbour_inventory_MBA_ch4&n2o.tiff", units="mm", width=150, height=75, res=300)
  grid.arrange(p.n2o, p.ch4, ncol = 2)
  while (!is.null(dev.list()))  dev.off()
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~modified time: 2023-11-26~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  
  P.N2O <- ggplot()+
    geom_tile(df.1, alpha = 0.5, mapping = aes(x=V1, y=V2, fill = Emissie), width = 0.07308, height = 0.0449)+ # set the width and height in geom_tile
    #coord_fixed(xlim=coors$lon.lim[coors$location=="Rotterdam"], ylim=coors$lat.lim[coors$location=="Rotterdam"])+
    scale_fill_gradientn(colours = myPalette(10), limits = ch4.lim, breaks = breaks, labels = labels)+
    geom_point(flight.0906, mapping = aes(x = lon, y = lat), size = 0.5)+
    geom_polygon(rotterdam, mapping = aes(x = x, y = y), fill = 'black', colour='black', alpha = 0.1)+
    geom_point(df, mapping = aes(x = lon, y = lat, size = Emissie, shape = Sector))+
    
    theme_bw()+
    theme( plot.title=element_text(size=12, hjust=0.5, vjust=0.5, face='bold'),
           axis.title.x = element_blank(), axis.text.x = element_text(size=12, angle = 90),
           axis.title.y = element_blank(), axis.text.y = element_text(size=12),
           legend.position = 'right',
           legend.text = element_text(size =12),legend.margin = margin(0,0,0,0),
           legend.background = element_rect(fill = "transparent", colour = NA),
           legend.key = element_rect(fill = NA, colour = NA),
           strip.background = element_rect(fill = NA, colour = NA))+
    facet_wrap(~Sector, ncol=4)+
    ggtitle('Rotterdam')+
    labs( fill= bquote(''*CH[4]*' emission ['*kg/yr*']') )
  
  tiff("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/Figures/harbour_inventory_ch4_map.1.tiff", units="mm", width=400, height=200, res=300)
  print(P.N2O)
  while (!is.null(dev.list()))  dev.off()
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  