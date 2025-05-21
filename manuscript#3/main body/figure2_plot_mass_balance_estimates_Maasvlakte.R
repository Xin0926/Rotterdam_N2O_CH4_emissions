# The code is for plotting the top-down estimates of N2O and CH4 for the MIA in the main body of my 3rd ms
# author: Xin Tong
# Time: 2024-12-10






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~1. prepare the data for plotting ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD MASS BALANCE ESTIMATES (mol/s) FOR the Maasvlakte industrial area
flux.n2o <- data.frame(
  flight = c("0830", "0901", "0906"),
  emi = c(110.88, 110.88, 63.36),
  uncertainty = c(79.20, 63.36, 47.52),
  flag = "Top-down"
)
mean.n2o <- mean(flux.n2o$emi)
sd.n2o <- sd(flux.n2o$emi)

flux.ch4 <- data.frame(
  flight = c("0906"),
  emi = c(1541),
  uncertainty = c(807),
  flag = "Top-down"
)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~2. plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p1 <- ggplot()+
  
  # plot LEFT Y AXIS (N2O)
  geom_bar(flux.n2o, mapping = aes(x = flight, y = emi), fill = "#CC79A7", alpha = 0.8,
           stat = 'identity', width = 0.6, 
           position=position_nudge(ifelse(flux.n2o$flight=="0906", -0.3, 0)))+
  geom_errorbar(flux.n2o, mapping = aes(x = flight, ymin = emi - uncertainty, ymax = emi + uncertainty), colour = "#CC79A7", alpha = 0.8,
           stat = 'identity', width = 0.2, 
           position=position_nudge(ifelse(flux.n2o$flight=="0906", -0.3, 0)))+
  
  # plot horizontal line
  geom_hline(yintercept = mean.n2o, colour = "#CC79A7")+
  geom_hline(yintercept = mean.n2o-sd, linetype = 'dashed', colour = "#CC79A7")+
  geom_hline(yintercept = mean.n2o+sd, linetype = 'dashed', colour = "#CC79A7")+
  
  # plot RIGHT Y AXIS (CH4)
  geom_bar(flux.ch4, mapping = aes(x = flight, y = emi/10), fill = "#0072B2", alpha = 0.8,
           stat = 'identity', width = 0.6, position=position_nudge(0.3))+
  geom_errorbar(flux.ch4, mapping = aes(x = flight, ymin = (emi - uncertainty)/10, ymax = (emi + uncertainty)/10), colour = "#0072B2", alpha = 0.8,
                stat = 'identity', width = 0.2, position=position_nudge(x = 0.3))+
  
  scale_y_continuous(
    name = bquote('Top-down estimates of '*N[2]*O*' emissions ['*kg~h^-1*']'), 
    sec.axis = sec_axis(~.*10, name = bquote('Top-down estimates of '*CH[4]*' emissions ['*kg~h^-1*']'))
  )+
  
  # Modify x-axis limits
  coord_cartesian(xlim = c(0.5, 3.8)) +
  
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
        axis.title.x = element_blank(),
        axis.text = element_text(size=8),
        plot.tag = element_text(size = 8),
        plot.margin = margin(t = 5, r = 5, b = 1, l = 5, unit = "pt"))

p1


#tiff("D:\\1 PhD Studies\\3.5 Publications\\Manuscript#3_Rotterdam_emission_estimates\\Figures\\fig.2.MIA.top-down.tiff", units="mm", width=150, height=100, res=300)

tiff("C:\\Users\\Xin09\\Desktop\\fig.2.tiff", units="mm", width=150, height=100, res=600)
print(p1)
while (!is.null(dev.list()))  dev.off()
