# the code is for plotting the Rotterdam area inventory for individual source categories separated by point and diffuse sources
# author: Xin Tong
# Time: JAN 8, 2025






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plot point and diffuse sources for each sector ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df.old <- readxl::read_excel("D:/1 PhD Studies/1 Data/Inventory/Dutch_Inventory/CH4_emission_Rotterdam_point_diffuse_sector.xlsx", sheet = 2);
df.old <- as.data.frame(df.old)

df.new <- readxl::read_excel("D:/1 PhD Studies/1 Data/Inventory/Dutch_Inventory/CH4_emission_Rotterdam_point_diffuse_sector.xlsx", sheet = 3);
df.new <- as.data.frame(df.new)

library(ggpp)
P.CH4 <- ggplot()+
  # plot bar for old dutch inventory
  geom_bar(df.old, mapping = aes(y=Sector, group=flag, fill=flag, x=emission/366/24), stat = 'identity', width = 0.25, position = position_stacknudge(y = -0.15))+
  scale_fill_brewer(palette = "Set1") + 
  labs(fill = 'Old inventory in Tong et al., 2023') +
  
  # plot bar for updated dutch inventory
  ggnewscale::new_scale_fill() +
  geom_bar(df.new, mapping = aes(y=Sector, group=flag, fill=flag, x=emission/366/24), stat = 'identity', width = 0.25, position = position_stacknudge(y = 0.15))+
  scale_fill_viridis(discrete = TRUE) +  labs(fill = 'Updated inventory') +
  
  theme_bw()+
  theme(
    axis.title.y = element_blank(), 
    axis.text.x = element_text(), axis.title.x = element_text(),               
    legend.position = c(0.5, 0.85),
    legend.text = element_text(),
    legend.spacing.y = unit(0,'cm'),legend.key.size = unit(0.2,'cm'),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = NA, colour = NA))+
  xlab( bquote(''*CH[4]*' emission ['*kg~hr^-1*']') )+
  facet_zoom(xlim = c(0, 180))


tiff("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/Figures/SI/old_new_inventory_point_diffuse.CH4.tiff", units="mm", width=300, height=200, res=300)
print(P.CH4)
while (!is.null(dev.list()))  dev.off()

















df.old <- readxl::read_excel("D:/1 PhD Studies/1 Data/Inventory/Dutch_Inventory/N2O_emission_Rotterdam_point_diffuse_sector.xlsx", sheet = 2);
df.old <- as.data.frame(df.old)

df.new <- readxl::read_excel("D:/1 PhD Studies/1 Data/Inventory/Dutch_Inventory/N2O_emission_Rotterdam_point_diffuse_sector.xlsx", sheet = 3);
df.new <- as.data.frame(df.new)

library(ggpp)
P.N2O <- ggplot()+
  # plot bar for old dutch inventory
  geom_bar(df.old, mapping = aes(y=Sector, group=flag, fill=flag, x=emission/366/24), stat = 'identity', width = 0.25, position = position_stacknudge(y = -0.15))+
  scale_fill_brewer(palette = "Set1") + 
  labs(fill = 'Old inventory in Tong et al., 2023') +
  
  # plot bar for updated dutch inventory
  ggnewscale::new_scale_fill() +
  geom_bar(df.new, mapping = aes(y=Sector, group=flag, fill=flag, x=emission/366/24), stat = 'identity', width = 0.25, position = position_stacknudge(y = 0.15))+
  scale_fill_viridis(discrete = TRUE) +  labs(fill = 'Updated inventory') +
  
  theme_bw()+
  theme(
    axis.title.y = element_blank(), 
    axis.text.x = element_text(), axis.title.x = element_text(),               
    legend.position = 'none',
    legend.text = element_text(),
    legend.spacing.y = unit(0,'cm'),legend.key.size = unit(0.2,'cm'),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = NA, colour = NA))+
  xlab( bquote(''*N[2]*O*' emission ['*kg~hr^-1*']') )+
  facet_zoom(xlim = c(0, 7.5))


tiff("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/Figures/SI/old_new_inventory_point_diffuse.N2O.tiff", units="mm", width=300, height=200, res=300)
print(P.N2O)
while (!is.null(dev.list()))  dev.off()
