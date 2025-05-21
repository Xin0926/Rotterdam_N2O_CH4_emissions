

library(viridis)
###############################################################################################
########################################## N2O ################################################
###############################################################################################
# PREPARATION for gridmap: run the script in the rmarkdown file "D:\1 PhD Studies\1 Data\Inventory\The research about the registered emissions in the Dutch website.Rmd"
# UNIT OF INVENTORY: kg/yr
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
data.new <- read.csv("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/updated_inventory.csv")
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

n2o.inventory <- data.new[which(data.new$Stof=="Distikstofoxide" & data.new$V1<4.75), ]
names(n2o.inventory)[names(n2o.inventory)=="V1"] <- "lon";
names(n2o.inventory)[names(n2o.inventory)=="V2"] <- "lat"

df <- mutate(n2o.inventory, source = case_when(
  Sector %in% c(
    "Nature", "Other industry", "Trade, Services and Government (HDO).", "Agriculture", "Consumers", "Construction", "Waste Disposal", "Drinking Water Supply"
  ) ~ "Others",
  Sector == "Traffic and Transportation" ~ "Traffic and Transportation",
  Sector == "Sewerage and wastewater treatment plants" ~ "Sewerage and wastewater treatment plants",
  Sector %in% c("Refineries", "Energy", "Chemical Industry") ~ "Refinery&Energy&Chemical Industry"
)) %>% 
  group_by(source) %>% summarise(emission = sum(Emissie));
df.new <- as.data.frame(df)
df.new$flag <- "updated_inventory"



df.new$source <- factor(
  df.new$source, 
  levels = c("Refinery&Energy&Chemical Industry", "Sewerage and wastewater treatment plants", "Traffic and Transportation", "Others")
)

# plot pie chart
percentages <- round(df.new$emission / sum(df.new$emission) * 100)

# Create pie chart with custom colors and percentages
p.n2o <- ggplot(df.new, aes(x = "", y = emission, fill = source)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_viridis(discrete = TRUE)+
  labs(fill = "category", title = "N2O inventory for Rotterdam") +
  theme_void()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ch4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ch4.inventory <- data.new[which(data.new$Stof=="Methaan" & data.new$V1<4.75), ]
names(ch4.inventory)[names(ch4.inventory)=="V1"] <- "lon";
names(ch4.inventory)[names(ch4.inventory)=="V2"] <- "lat"

df <- mutate(ch4.inventory, source = case_when(
  Sector %in% c(
    "Nature", "Other industry", "Trade, Services and Government (HDO).", "Agriculture", "Consumers", "Construction", "Waste Disposal", "Drinking Water Supply"
  ) ~ "Others",
  Sector == "Traffic and Transportation" ~ "Traffic and Transportation",
  Sector == "Sewerage and wastewater treatment plants" ~ "Sewerage and wastewater treatment plants",
  Sector %in% c("Refineries", "Energy", "Chemical Industry") ~ "Refinery&Energy&Chemical Industry"
)) %>% 
  group_by(source) %>% summarise(emission = sum(Emissie));
df.new <- as.data.frame(df)
df.new$flag <- "updated_inventory"



df.new$source <- factor(
  df.new$source, 
  levels = c("Refinery&Energy&Chemical Industry", "Sewerage and wastewater treatment plants", "Traffic and Transportation", "Others")
)

# plot pie chart
percentages <- round(df.new$emission / sum(df.new$emission) * 100)

# Create pie chart with custom colors and percentages
p.ch4 <- ggplot(df.new, aes(x = "", y = emission, fill = source)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_viridis(discrete = TRUE)+
  labs(fill = "category", title = "ch4 inventory for Rotterdam")+
  theme_void()

tiff("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/Figures/pie chart.n2o&ch4.tiff", units="mm", width=300, height=100, res=300)
grid.arrange(p.n2o, p.ch4, ncol = 2)
while (!is.null(dev.list()))  dev.off()

