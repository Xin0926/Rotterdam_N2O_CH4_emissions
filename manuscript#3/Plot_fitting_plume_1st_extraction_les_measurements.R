# the code is for plotting modelled and observed plumes with two fitting functions
# in the SI of chapter_3
# author: Xin Tong
# time: August 31, 2024


sapply(list.files(pattern="[.]R$", path="c:/Users/xin/Dropbox/xin/Rscripts/functions", full.names=TRUE), source)

############################################### LOAD OR CREATE DATASETS #############################################################
#################### load observations ####################
n2o.0830 <- read.csv("D:/1 PhD Studies/3.1 Results of projects/2022_Rotterdam_Campaign/Airborne_meas/0830/n2o.transect2.csv")
n2o.0901 <- read.csv("D:/1 PhD Studies/3.1 Results of projects/2022_Rotterdam_Campaign/Airborne_meas/0901/n2o.transect6.csv")
n2o.0906 <- read.csv("D:/1 PhD Studies/3.1 Results of projects/2022_Rotterdam_Campaign/Airborne_meas/0906/n2o.transect2.csv")

############################################################
# run code to obtain the les plume for 1st extraction method
names(les)[names(les)=="N2O_flat"] <- "n2o"
n2o.0830 <- les
n2o.0901 <- les
n2o.0906 <- les
############################################################

datasets <- list(n2o.0830, n2o.0901, n2o.0906)
flight <- c("0830", "0901", "0906")
for(i in 1:3){
  n2o <- datasets[[i]]
  # Ensure there are no NA or infinite values in the inputs
  valid_indices <- !is.na(n2o$distance) & !is.na(n2o$n2o) & is.finite(n2o$distance) & is.finite(n2o$n2o)

  # Filter the data to include only valid rows
  n2o_valid <- n2o[valid_indices, ]
  
  # Fit the smooth spline model
  fit <- smooth.spline(n2o_valid$distance, n2o_valid$n2o)
  n2o_valid$spline <- predict(fit, n2o_valid$distance)$y
  
  # Fit the loess model
  fit <- loess(n2o_valid$n2o~n2o_valid$distance, data=n2o_valid)
  n2o_valid$loess <- predict(fit, n2o_valid$distance)
  
  melt <- melt(n2o_valid, id.vars = c("distance", "n2o"), measure.vars = c("spline", "loess"))
  
  assign(paste0("n2o.", flight[i]), n2o)
  assign(paste0("n2o.fit.", flight[i]), melt)
}



############################################### create a function for plotting #############################################################
library(viridis)
# create functions for plotting
create_plot <- function(data1, data2){

  ggplot()+
    geom_point(data1, mapping = aes(x = distance/1000, y = n2o), colour = "black", size = 0.5)+
    geom_line(data2, mapping = aes(x = distance/1000, y = value, colour = variable))+
    scale_color_manual(values = c("#CC79A7", "#0072B2"))+
    labs(y = bquote(''*N[2]*O*' [ppb]'), x = "Distance [km]")+
    theme_bw()+
    theme(legend.position = 'right', legend.title = element_blank(),
          legend.text = element_text(size = 8), legend.margin = margin(0,0,0,0),
          legend.spacing = unit(0,'cm'), legend.key.size = unit(2,'mm'),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.key = element_rect(fill = NA, colour = NA),
          #legend.margin = margin(t = -8, r = 0, b = 0, l = 0, unit = "pt"),
          axis.title = element_text(size=8),
          axis.text = element_text(size=8),
          plot.tag = element_text(size = 8),
          plot.margin = margin(t = 5, r = 5, b = 1, l = 5, unit = "pt"))
  
}

############################################### plot #############################################################

# apply the functions 
p.n2o.0830 <- create_plot(n2o.0830, n2o.fit.0830)+
  labs(tag = "(a)")
p.n2o.0901 <- create_plot(n2o.0901, n2o.fit.0901)+
  labs(tag = "(b)")
p.n2o.0906 <- create_plot(n2o.0906, n2o.fit.0906)+
  labs(tag = "(c)")

library(patchwork)
p <- p.n2o.0830 + p.n2o.0901 + p.n2o.0906+ 
  plot_layout(guides = 'collect', nrow = 1) &
  theme(legend.position = 'right', legend.direction = "vertical",
        plot.tag.location = "margin", plot.tag.position = "topleft",
        plot.title = element_text(hjust = 0.5))


while (!is.null(dev.list()))  dev.off()
# ggsave("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/Figures/SI/n2o.observe.plume.tiff", plot = p,
#        width = 300, height = 75, units = "mm", dpi = 300, compression = "lzw")
ggsave("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/Figures/SI/n2o.les.plume.tiff", plot = p,
       width = 300, height = 75, units = "mm", dpi = 300, compression = "lzw")
