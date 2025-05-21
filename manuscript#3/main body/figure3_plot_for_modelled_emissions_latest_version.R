# the code is for plotting EMISSIONS based on LES model for the main body of the ms
# author: Xin Tong
# time: April 8, 2025



data <- read.csv("D:/1 PhD Studies/1 Data/Inventory/Dutch_inventory/N2O_CH4_CO2_CO_emission_Rotterdam_area_company_2020_2021_2022.csv")

data <- mutate(data, emission = Emissie/366/24) # convert unit from kg/yr to kg/hr

###################################
# select data by conditions for n2o
#

df.n2o <- data.frame(
  flight = c('0830', '0901', '0906'),
  emi = c(139, 30, 41),
  lower = c(101, 25, 31),
  upper = c(190, 53, 56)
)

mean.n2o <- mean(df.n2o$emi);
sd <- sd(df.n2o$emi)


condition <- data$Stof=="Distikstofoxide" & 
  data$Jaar=="2020" & 
  (data$Sector=="Chemical Industry"|data$Sector=="Energy"|data$Sector=="Refineries")

MIA.n2o <- group_by(data[condition
                         & data$lat<52.0088 & data$lat>51.9
                         & data$lon<4.2 & data$lon>3.945, ], lon, lat, Sector) %>% 
  summarise(emission = sum(emission))
MIA.n2o <- as.data.frame(MIA.n2o)
MIA.n2o$area <- "Maasvlakte"

#
RA.n2o <- sum(data[condition, "emission"])-sum(MIA.n2o$emission)

#
old.n2o <- data.frame(
  area = c("WIA", "RUA"),
  emi = c(sum(MIA.n2o$emission), RA.n2o),
  flag = "Inventory"
)

new.n2o <- data.frame(
  area = c("WIA", "RUA"),
  emi = c(mean.n2o, RA.n2o),
  flag = "This study"
)

n2o <- rbind(old.n2o, new.n2o)

###################################
# select data by conditions for ch4
#
condition <- data$Stof=="Methaan" & 
  data$Jaar=="2020" &
  (data$Sector=="Chemical Industry"|data$Sector=="Energy"|data$Sector=="Refineries")

MIA.ch4 <- group_by(data[condition
                         & data$lat<52.0088 & data$lat>51.9
                         & data$lon<4.2 & data$lon>3.945, ], lon, lat, Sector) %>% 
  summarise(emission = sum(emission))
MIA.ch4 <- as.data.frame(MIA.ch4)
MIA.ch4$area <- "Maasvlakte"

#
RA.ch4 <- sum(data[condition, "emission"])-sum(MIA.ch4$emission)

#
old.ch4 <- data.frame(
  area = c("WIA", "RUA"),
  emi = c(sum(MIA.ch4$emission), RA.ch4),
  flag = "Inventory"
)

new.ch4 <- data.frame(
  area = c("WIA", "RUA"),
  emi = c(1669, RA.ch4),
  flag = "This study"
)

ch4 <- rbind(old.ch4, new.ch4)


######################
n2o.total.int <- sum(old.n2o$emi);
n2o.total.this.study <- sum(new.n2o$emi);
ch4.total.int <- sum(old.ch4$emi);
ch4.total.this.study <- sum(new.ch4$emi)

n2o.error <- data.frame(
  height = c(n2o.total.int, n2o.total.this.study),
  lower = c(n2o.total.int-n2o.total.int*0.35, n2o.total.this.study - sd),
  upper = c(n2o.total.int+n2o.total.int*0.35, n2o.total.this.study + sd)
)

ch4.error <- data.frame(
  height = c(ch4.total.int, ch4.total.this.study),
  lower = c(ch4.total.int-ch4.total.int*0.63, 1183),
  upper = c(ch4.total.int+ch4.total.int*0.63, 2040)
)


old.n2o$area <- factor(old.n2o$area, levels = c("WIA", "RUA"))
new.n2o$area <- factor(new.n2o$area, levels = c("WIA", "RUA"))

old.ch4$area <- factor(old.ch4$area, levels = c("WIA", "RUA"))
new.ch4$area <- factor(new.ch4$area, levels = c("WIA", "RUA"))








###################### plot ######################

p1 <- ggplot()+
  
  # plot LEFT Y AXIS (N2O)
  geom_bar(old.n2o, mapping = aes(x = "Inventory", y = emi, fill = area), alpha = 0.8,
           stat = 'identity', width = 0.5) + 
  geom_bar(new.n2o, mapping = aes(x = "This study", y = emi, fill = area), alpha = 0.8,
           stat = 'identity', width = 0.5) + 
  geom_errorbar(n2o.error, mapping = aes(x = c("Inventory", "This study"), ymin = lower, ymax = upper), colour = "#CC79A7",
                width = 0.1)+
  
  scale_fill_manual(values = c("#CC79A7", "orchid4"))+
  
  ylab(bquote('Estimated '*N[2]*O*' emissions ['*kg~h^-1*'] '))+
  
  #ggtitle('Estimated point sources’ emissions for the category group')+
  
  theme_bw()+
  theme(legend.position = c(0.2, 0.75), legend.title = element_blank(),
        legend.text = element_text(size = 8), legend.margin = margin(0,0,0,0),
        legend.spacing = unit(0,'cm'), #legend.key.size = unit(2,'mm'),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = NA, colour = NA),
        #legend.margin = margin(t = -8, r = 0, b = 0, l = 0, unit = "pt"),
        plot.tag = element_text(size = 8),
        plot.margin = margin(t = 5, r = 5, b = 1, l = 5, unit = "pt"),
        axis.title.x = element_blank())


p2 <- ggplot()+
  
  # plot LEFT Y AXIS (N2O)
  geom_bar(old.ch4, mapping = aes(x = "Inventory", y = emi, fill = area), alpha = 0.8,
           stat = 'identity', width = 0.5) + 
  geom_bar(new.ch4, mapping = aes(x = "This study", y = emi, fill = area), alpha = 0.8,
           stat = 'identity', width = 0.5) + 
  geom_errorbar(ch4.error, mapping = aes(x = c("Inventory", "This study"), ymin = lower, ymax = upper), colour = "#0072B2",
                width = 0.1)+
  
  scale_fill_manual(values = c("#0072B2", "dodgerblue4"))+
  
  ylab(bquote('Estimated '*CH[4]*' emissions ['*kg~h^-1*'] '))+
  
  #ggtitle('Estimated point sources’ emissions for the category group')+
  
  theme_bw()+
  theme(legend.position = c(0.2, 0.75), legend.title = element_blank(),
        legend.text = element_text(size = 8), legend.margin = margin(0,0,0,0),
        legend.spacing = unit(0,'cm'), #legend.key.size = unit(2,'mm'),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = NA, colour = NA),
        #legend.margin = margin(t = -8, r = 0, b = 0, l = 0, unit = "pt"),
        plot.tag = element_text(size = 8),
        plot.margin = margin(t = 5, r = 5, b = 1, l = 5, unit = "pt"),
        axis.title.x = element_blank())



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ organize and place figures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(cowplot)

tiff("D:\\1 PhD Studies\\3.5 Publications\\Manuscript#3_Rotterdam_emission_estimates\\Figures\\main_body\\fig.3.2.tiff", units="mm", width=180, height=80, res=300)

# Create the plot grid
plot_grid(
  p1 + labs(tag = "(a)"),
  p2 + labs(tag = "(b)"), 
  ncol = 2,
  align = 'hv',  # Horizontal alignment
  axis = 'bt'   # Align plots on both top and bottom
)
while (!is.null(dev.list()))  dev.off()
