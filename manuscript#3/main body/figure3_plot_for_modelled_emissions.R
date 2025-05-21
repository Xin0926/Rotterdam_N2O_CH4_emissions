# the code is for plotting EMISSIONS based on LES model for the main body of the ms
# author: Xin Tong
# time: Jan 6, 2025




#~~~~~~~~~~~~~~~~~~~~~~~1. create dataframe for plotting ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##############
# for 1st plot
##############

df.n2o <- data.frame(
  flight = c('0830', '0901', '0906'),
  emi = c(139, 30, 41),
  lower = c(101, 25, 31),
  upper = c(190, 53, 56)
)

mean.n2o <- mean(df.n2o$emi);
sd <- sd(df.n2o$emi)

df.ch4 <- data.frame(
  flight = c('0906'),
  emi = 1669,
  lower = 1183,
  upper = 2040
)


##############
# for 2nd plot
##############
data <- read.csv("D:/1 PhD Studies/1 Data/Inventory/Dutch_inventory/N2O_CH4_CO2_CO_emission_Rotterdam_area_company_2020_2021_2022.csv")

data <- mutate(data, emission = Emissie/366/24) # convert unit from kg/yr to kg/hr

###################################
# select data by conditions for n2o
#
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~2. plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p1 <- ggplot()+
  
  # plot LEFT Y AXIS (N2O)
  geom_bar(df.n2o, mapping = aes(x = flight, y = emi), fill = "#CC79A7", alpha = 0.8,
           stat = 'identity', width = 0.6, 
           position = position_nudge(x = ifelse(df.n2o$flight == '0906', -0.3, 0)))+
  geom_errorbar(df.n2o, mapping = aes(x = flight, ymin = lower, ymax = upper), colour = "#CC79A7", 
           width = 0.2, 
           position = position_nudge(x = ifelse(df.n2o$flight == '0906', -0.3, 0)))+
  
  # plot horizontal line
  geom_hline(yintercept = mean.n2o, colour = "#CC79A7")+
  geom_hline(yintercept = mean.n2o-sd, linetype = 'dashed', colour = "#CC79A7")+
  geom_hline(yintercept = mean.n2o+sd, linetype = 'dashed', colour = "#CC79A7")+
  
  # plot RIGHT Y AXIS (CH4)
  geom_bar(df.ch4, mapping = aes(x = flight, y = emi/10), fill = "#0072B2", alpha = 0.8,
           stat = 'identity', width = 0.6, position=position_nudge(0.3))+
  geom_errorbar(df.ch4, mapping = aes(x = flight, ymin = lower/10, ymax = upper/10), colour = "#0072B2", 
                width = 0.2, 
                position=position_nudge(0.3))+
  
  scale_y_continuous(
    name = bquote('  '*N[2]*O*' emission rate ['*kg~h^-1*']'), 
    sec.axis = sec_axis(~.*10, name = bquote(' '*CH[4]*' emission rate ['*kg~h^-1*']')))+
  
  scale_x_discrete(limits = c('0830', '0901', '0906'), 
                   expand = expansion(add = c(0.2, 0.5))) + # This adds extra space on both sides
  coord_cartesian(xlim = c(0.5, 3.5)) +
  
  ggtitle('Estimated emissions from industrial plants for individual days')+
  
  theme_bw()+
  theme(legend.position = 'right', legend.title = element_blank(),
        legend.text = element_text(size = 8), legend.margin = margin(0,0,0,0),
        legend.spacing = unit(0,'cm'), legend.key.size = unit(2,'mm'),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = NA, colour = NA),
        #legend.margin = margin(t = -8, r = 0, b = 0, l = 0, unit = "pt"),
        axis.title.y.left = element_text(colour = "#CC79A7"),
        axis.text.y.left = element_text(colour = "#CC79A7"),
        axis.title.y.right = element_text(colour = "#0072B2"),
        axis.text.y.right = element_text(colour = "#0072B2"),
        axis.text = element_text(), axis.title.x = element_blank(),
        plot.tag = element_text(size = 8),
        plot.margin = margin(t = 5, r = 5, b = 1, l = 5, unit = "pt"),
        plot.title = element_text()
        )
p1


n2o$area <- factor(n2o$area, levels = c("WIA", "RUA"))
ch4$area <- factor(ch4$area, levels = c("WIA", "RUA"))




# p2 <- ggplot()+
#   
#   # plot LEFT Y AXIS (N2O)
#   geom_bar(n2o, mapping = aes(x = flag, y = emi, fill = area), alpha = 0.8,
#            stat = 'identity', width = 0.4, position=position_stacknudge(x = -0.2))+
#   scale_fill_manual(values = c("#CC79A7", "orchid4"))+
#   
#   # plot bar for mass balance flux
#   ggnewscale::new_scale_fill() +
#   
#   # plot RIGHT Y AXIS (CH4)
#   geom_bar(ch4, mapping = aes(x = flag, y = emi/20, fill = area), alpha = 0.8,
#            stat = 'identity', width = 0.4, position=position_stacknudge(x = 0.2))+
#   scale_fill_manual(values = c("#0072B2", "dodgerblue4"))+
#   
#   scale_y_continuous(
#     name = bquote('Estimated '*N[2]*O*' emissions ['*kg~h^-1*'] '), 
#     sec.axis = sec_axis(~.*20, name = bquote('Estimated '*CH[4]*' emissions ['*kg~h^-1*'] '))
#   )+
#   ggtitle('Estimated emissions for the Rotterdam area')+
#   theme_bw()+
#   theme(legend.position = c(0.25, 0.75), legend.title = element_blank(),
#         legend.text = element_text(size = 8), legend.margin = margin(0,0,0,0),
#         legend.spacing = unit(0,'cm'), #legend.key.size = unit(2,'mm'),
#         legend.background = element_rect(fill = "transparent", colour = NA),
#         legend.key = element_rect(fill = NA, colour = NA),
#         #legend.margin = margin(t = -8, r = 0, b = 0, l = 0, unit = "pt"),
#         axis.title.y.left = element_text(colour = "#CC79A7"), 
#         axis.text.y.left = element_text(colour = "#CC79A7"),
#         axis.title.y.right = element_text(colour = "#0072B2"),
#         axis.text.y.right = element_text(colour = "#0072B2"),
#         axis.title.x = element_blank(),
#         axis.text = element_text(),
#         plot.tag = element_text(size = 8),
#         plot.margin = margin(t = 5, r = 5, b = 1, l = 5, unit = "pt"))
# p2



############################### version 2 for secodn plot ####################################
old.n2o$area <- factor(old.n2o$area, levels = c("WIA", "RUA"))
new.n2o$area <- factor(new.n2o$area, levels = c("WIA", "RUA"))

old.ch4$area <- factor(old.ch4$area, levels = c("WIA", "RUA"))
new.ch4$area <- factor(new.ch4$area, levels = c("WIA", "RUA"))

p2 <- ggplot()+
  
  # plot LEFT Y AXIS (N2O)
  geom_bar(old.n2o, mapping = aes(x = 1, y = emi, fill = area), alpha = 0.8,
           stat = 'identity', width = 0.3, position = position_stacknudge(x = -0.18)) + 
  geom_bar(new.n2o, mapping = aes(x = 1, y = emi, fill = area), alpha = 0.8,
           stat = 'identity', width = 0.3, position = position_stacknudge(x = 0.18)) + 
  geom_errorbar(n2o.error, mapping = aes(x = 1, ymin = lower, ymax = upper), 
                width = 0.1, position = position_nudge(x = c(-0.18, 0.18)))+
  
  scale_fill_manual(values = c("#CC79A7", "orchid4"))+
  
  # Add labels above bars for N2O
  annotate("text", x = 0.82, y = n2o.total.int * 0.5, 
           label = "Inventory", hjust = 0.5) +
  annotate("text", x = 1.18, y = n2o.total.this.study * 0.5, 
           label = "This study", hjust = 0.5) +
  
  
  # plot bar for mass balance flux
  ggnewscale::new_scale_fill() +
  
  # plot RIGHT Y AXIS (CH4)
  geom_bar(old.ch4, mapping = aes(x = 2, y = emi/20, fill = area), alpha = 0.8,
           stat = 'identity', width = 0.3, position = position_stacknudge(x = -0.18)) + 
  geom_bar(new.ch4, mapping = aes(x = 2, y = emi/20, fill = area), alpha = 0.8,
           stat = 'identity', width = 0.3, position = position_stacknudge(x = 0.18)) + 
  geom_errorbar(ch4.error, mapping = aes(x = 2, ymin = lower/20, ymax = upper/20), 
                width = 0.1, position = position_nudge(x = c(-0.18, 0.18)))+
  
  scale_fill_manual(values = c("#0072B2", "dodgerblue4"))+
  
  # Add labels above bars for CH4
  annotate("text", x = 1.82, y = ch4.total.int * 3 /20, 
           label = "Inventory", hjust = 0.5) +
  annotate("text", x = 2.18, y = ch4.total.this.study * 0.5 /20, 
           label = "This study", hjust = 0.5) +
  
    scale_y_continuous(
      name = bquote('Estimated '*N[2]*O*' emissions ['*kg~h^-1*'] '),
      sec.axis = sec_axis(~.*20, name = bquote('Estimated '*CH[4]*' emissions ['*kg~h^-1*'] '))
    )+
    ggtitle('Estimated emissions for the Rotterdam area')+
    theme_bw()+
    theme(legend.position = c(0.55, 0.75), legend.title = element_blank(),
          legend.text = element_text(size = 8), legend.margin = margin(0,0,0,0),
          legend.spacing = unit(0,'cm'), #legend.key.size = unit(2,'mm'),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.key = element_rect(fill = NA, colour = NA),
          #legend.margin = margin(t = -8, r = 0, b = 0, l = 0, unit = "pt"),
          axis.title.y.left = element_text(colour = "#CC79A7"),
          axis.text.y.left = element_text(colour = "#CC79A7"),
          axis.title.y.right = element_text(colour = "#0072B2"),
          axis.text.y.right = element_text(colour = "#0072B2"),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          plot.tag = element_text(size = 8),
          plot.margin = margin(t = 5, r = 5, b = 1, l = 5, unit = "pt"))
  p2











#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~3. organize and place figures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  library(cowplot)
  
tiff("D:\\1 PhD Studies\\3.5 Publications\\Manuscript#3_Rotterdam_emission_estimates\\Figures\\main_body\\fig.3.1.tiff", units="mm", width=325, height=100, res=600)

# Create the plot grid
plot_grid(
  p1 + labs(tag = "(a)"),
  p2 + labs(tag = "(b)"), 
  ncol = 2,
  align = 'h',  # Horizontal alignment
  axis = 'bt'   # Align plots on both top and bottom
)
while (!is.null(dev.list()))  dev.off()
































