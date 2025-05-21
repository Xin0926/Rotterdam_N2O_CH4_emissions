# This script is to plot tunnels emissions in the main body of the whole ms
# author: Xin Tong
# time: 2024-12-06






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~1. prepare the data for plotting ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df <- read.csv("c:/1 PhD Studies/3.1 Results of projects/2022_Rotterdam_Campaign/Tunnel/tunnel_summary.1.csv")
slope <- read.csv("c:/1 PhD Studies/3.1 Results of projects/2022_Rotterdam_Campaign/Tunnel/slope.csv")

# change the unit to ug/g
df$slope_N2O_CO2 <- df$slope_N2O_CO2*1000000
df$slope_CH4_CO2 <- df$slope_CH4_CO2*16*1000000/44

list.n2o <- vector("list", 7)
list.ch4 <- vector("list", 7)
r2 <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

for(i in 1:7) {
  list.n2o[[i]] <- data.frame(r2=r2[i], df[df$r2_N2O_CO2>=r2[i], c("r2_N2O_CO2", "slope_N2O_CO2")]);
  list.ch4[[i]] <- data.frame(r2=r2[i], df[df$r2_CH4_CO2>=r2[i], c("r2_CH4_CO2", "slope_CH4_CO2")])
}

ratio.n2o <- do.call(rbind, list.n2o)
ratio.ch4 <- do.call(rbind, list.ch4)

# the range for the ratio passing the criterion
slope.n2o <- slope[slope$r2>=0.4 & slope$n.n2o.co2>=2,]
slope.ch4 <- slope[slope$r2>=0.4 & slope$n.ch4.co2>=2,]
range.n2o <- range( slope.n2o[, c("mean.n2o.co2", "median.n2o.co2")])
range.ch4 <- range( slope.ch4[, c("mean.ch4.co2", "median.ch4.co2")])






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~2. plotting ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p1 <- ggplot()+

  # right y axis: plot boxplot for enhancemnet ratios
  geom_boxplot(ratio.n2o, mapping = aes(x = r2, y = slope_N2O_CO2/10, group = r2))+
  stat_summary(data = ratio.n2o, mapping = aes(x = r2, y = slope_N2O_CO2/10), fun = mean, geom = "point", shape = "diamond", size = 5) +  # Points for means
  
  # left y axis: plot line for the number of regression when r2 above a threshold
  geom_line(slope, mapping = aes(x = r2, y = n.n2o.co2), group = 1, colour = "#CC79A7")+
  
  # add the min and max considering both mean and median values passing criterion
  # geom_hline(yintercept = min(range.n2o)/10, colour = "black", linetype = "dashed")+
  # geom_hline(yintercept = max(range.n2o)/10, colour = "black", linetype = "dashed")+
  
  # add the min and max considering both mean and median values passing criterion
  annotate("rect",
           xmin = 0.4, xmax = max(slope.n2o$r2),
           ymin = min(range.n2o)/10, ymax = max(range.n2o)/10,
           fill = "darkgrey",
           alpha = 0.5) +
  
  scale_y_continuous(
    name = bquote("Plume numbers with  "*R^2*" above the threshold"),
    sec.axis = sec_axis(~.*10, name = bquote('The ratio of  '*N[2]*O*' and  '*CO[2]*' ['*ug~g^-1*']'))
    )+
  xlab(expression(paste( R^2, " value")))+
  theme_bw()+
  theme(legend.position.inside = c(0.25, 0.75), aspect.ratio = 1,
        legend.text = element_text(), legend.margin = margin(0,0,0,0),
        legend.spacing = unit(0,'cm'), legend.key.size = unit(2,'mm'),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = NA, colour = NA),
        #legend.margin = margin(t = -8, r = 0, b = 0, l = 0, unit = "pt"),
        axis.title.y.left = element_text(colour = "#CC79A7"), 
        axis.text.y.left = element_text(colour = "#CC79A7"), 
        axis.title = element_text(),
        axis.text = element_text(),
        plot.tag = element_text(),
        plot.margin = margin(t = 5, r = 5, b = 1, l = 5, unit = "pt"))


p2 <- ggplot()+
  
  # right y axis: plot boxplot for enhancemnet ratios
  geom_boxplot(ratio.ch4, mapping = aes(x = r2, y = slope_CH4_CO2/100, group = r2))+
  stat_summary(data = ratio.ch4, mapping = aes(x = r2, y = slope_CH4_CO2/100), fun = mean, geom = "point", shape = "diamond", size = 5) +  # Points for means
  
  # left y axis: plot line for the number of regression when r2 above a threshold
  geom_line(slope, mapping = aes(x = r2, y = n.ch4.co2), group = 1, colour = "#CC79A7")+
  
  # add the min and max considering both mean and median values passing criterion
  annotate("rect",
           xmin = 0.4, xmax = max(slope.ch4$r2),
           ymin = min(range.ch4)/100, ymax = max(range.ch4)/100,
           fill = "darkgrey",
           alpha = 0.5) +
  
  scale_y_continuous(
    name = bquote("Plume numbers with  "*R^2*" above the threshold"),
    sec.axis = sec_axis(~.*100, name = bquote('The ratio of  '*CH[4]*' and  '*CO[2]*' ['*ug~g^-1*']'))
  )+
  xlab(expression(paste( R^2, " value")))+
  theme_bw()+
  theme(legend.position.inside = c(0.25, 0.75), aspect.ratio = 1,
        legend.text = element_text(), legend.margin = margin(0,0,0,0),
        legend.spacing = unit(0,'cm'), legend.key.size = unit(2,'mm'),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = NA, colour = NA),
        #legend.margin = margin(t = -8, r = 0, b = 0, l = 0, unit = "pt"),
        axis.title.y.left = element_text(colour = "#CC79A7"), 
        axis.text.y.left = element_text(colour = "#CC79A7"), 
        axis.title = element_text(),
        axis.text = element_text(),
        plot.tag = element_text(),
        plot.margin = margin(t = 5, r = 5, b = 1, l = 5, unit = "pt"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~3. plotting THE COMPARISON OF enhancement ratio and inventory estimate ratio ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
inventory <- data.frame(mean = c(35, 83), gas = c("N2O/CO2", "CH4/CO2"))  # UNIT: ug/g

df.n2o <- data.frame(
  gas = "N2O/CO2",
  ratio = c(mean(c(slope.n2o$mean.n2o.co2, slope.n2o$median.n2o.co2)), 35, 18),
  sd = c(sd(c(slope.n2o$mean.n2o.co2, slope.n2o$median.n2o.co2)), 17.5, 2),
  flag = c("This study", "Inventory", "Popa et al., 2014")
)

df.ch4 <- data.frame(
  gas = "CH4/CO2",
  ratio = c(mean(c(slope.ch4$mean.ch4.co2, slope.ch4$median.ch4.co2)), 83, 17, 49),
  sd = c(sd(c(slope.ch4$mean.ch4.co2, slope.ch4$median.ch4.co2)), 41.5, 1, 29),
  flag = c("This study", "Inventory", "Popa et al., 2014", "Wei and Wang et al., 2020")
)

df.ch4$flag <- factor(df.ch4$flag, levels = c("Inventory", "Popa et al., 2014", "Wei and Wang et al., 2020", "This study"))

p.n2o <- ggplot()+
  
  # plot RIGHT Y AXIS (N2O)
  geom_bar(df.n2o, mapping = aes(x = gas, fill = flag, y = ratio), alpha = 0.8,
           stat = 'identity', width = 0.75, position=position_dodge(0.9))+
  geom_errorbar(df.n2o, show.legend=FALSE, width = 0.2, position=position_dodge(0.9),
                mapping=aes(x = gas, ymax = ratio + sd, ymin = ratio - sd, colour = flag))+
  scale_fill_viridis(discrete = TRUE)+
  scale_colour_viridis(discrete = TRUE)+
  # scale_fill_manual(values = c("#CC79A7", "#0072B2"))+
  # scale_colour_discrete(values = c("#CC79A7", "#0072B2"))+
  
  # scale_y_continuous(
  #   name = bquote('The ratio of  '*CH[4]*' and  '*CO[2]*' ['*ug~g^-1*']'), 
  #   sec.axis = sec_axis(~., name = bquote('The ratio of  '*N[2]*O*' and  '*CO[2]*' ['*ug~g^-1*']'))
  #   )+
  ylab(bquote('The ratio of '*N[2]*O*' and '*CO[2]*' ['*ug~g^-1*']'))+
  labs(fill = '')+
  theme_bw()+
  theme(legend.position = c(0.25, 0.75), aspect.ratio = 1,
        legend.text = element_text(), legend.margin = margin(0,0,0,0),
        legend.spacing = unit(0,'cm'), legend.key.size = unit(2,'mm'),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = NA, colour = NA),
        #legend.margin = margin(t = -8, r = 0, b = 0, l = 0, unit = "pt"),
        # axis.title.y.left = element_text(colour = "#CC79A7"), 
        # axis.text.y.left = element_text(colour = "#CC79A7"), 
        # axis.title.y.right = element_text(colour = "#0072B2"), 
        # axis.text.y.right = element_text(colour = "#0072B2"), 
        axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.text = element_text(),
        plot.tag = element_text(),
        plot.margin = margin(t = 5, r = 5, b = 1, l = 5, unit = "pt"))

p.n2o


p.ch4 <- ggplot()+
  
  # plot RIGHT Y AXIS (N2O)
  geom_bar(df.ch4, mapping = aes(x = gas, fill = flag, y = ratio), alpha = 0.8,
           stat = 'identity', width = 0.75, position=position_dodge(0.9))+
  geom_errorbar(df.ch4, show.legend=FALSE, width = 0.2, position=position_dodge(0.9),
                mapping=aes(x = gas, ymax = ratio + sd, ymin = ratio - sd, colour = flag))+
  scale_fill_viridis(discrete = TRUE)+
  scale_colour_viridis(discrete = TRUE)+
  ylab(bquote('The ratio of '*CH[4]*' and '*CO[2]*' ['*ug~g^-1*']'))+
  labs(fill = '')+
  theme_bw()+
  theme(legend.position = c(0.25, 0.75), aspect.ratio = 1,
        legend.text = element_text(), legend.margin = margin(0,0,0,0),
        legend.spacing = unit(0,'cm'), legend.key.size = unit(2,'mm'),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = NA, colour = NA),
        #legend.margin = margin(t = -8, r = 0, b = 0, l = 0, unit = "pt"),
        # axis.title.y.left = element_text(colour = "#CC79A7"), 
        # axis.text.y.left = element_text(colour = "#CC79A7"), 
        # axis.title.y.right = element_text(colour = "#0072B2"), 
        # axis.text.y.right = element_text(colour = "#0072B2"), 
        axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.text = element_text(),
        plot.tag = element_text(),
        plot.margin = margin(t = 5, r = 5, b = 1, l = 5, unit = "pt"))





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~4. organize and combine the plots into one figure and save it ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

p1 <- p1+labs(tag = "(a)")
p2 <- p2+labs(tag = "(b)")
p.n2o <- p.n2o+labs(tag = "(c)")
p.ch4 <- p.ch4+labs(tag = "(d)")


# ggsave("p1.png", p1, width = 10, height = 10)
# ggsave("p_n2o.png", p.n2o, width = 10, height = 10)
# 
# library(magick)
# p1 <- ggdraw() + draw_image("p1.png")
# p.n2o <- ggdraw() + draw_image("p_n2o.png")


tiff("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/Figures/main_body/fig.5.tunnel.tiff", units="mm", width=250, height=200, res=600)
library(cowplot)
plot_grid(p1, p2, p.n2o, p.ch4, 
          nrow = 2,
          align = 'vh',  # Align both vertically and horizontally
          axis = 'tblr') # Align on all sides
while (!is.null(dev.list()))  dev.off()







