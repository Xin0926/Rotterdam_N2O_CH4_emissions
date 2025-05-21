# the code is for plotting multiple linear regression between N2O (CH4) and CO2 for tunnels
# author: Xin Tong
# date: April 9, 2024



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~1. Organize a dataframe with tags for different plumes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df.list <- list()
for(i in c(2,5,8,10,11,17)) {
  
  unique_date <- seq(from = as.Date("2022-08-22", format = "%Y-%m-%d"), to = as.Date("2022-09-07"), by = '1 day')
  
  data <- read.csv(paste0("D:/1 PhD Studies/1 Data/processed/Aerodyne_Tildas/aerodyne&meteo_", unique_date[i], ".csv"))
  data$date <- as.POSIXct(data$date, tz = "UTC")
  reference_timestamp <- as.POSIXct(paste0(unique_date[i], " 00:00:00"), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  # Calculate the difference in seconds between the timestamp and reference timestamp
  data <- cbind(
    data, time = as.numeric(difftime(data$date, reference_timestamp, units = "secs"))
  )
  
  if(unique_date[i]=="2022-08-23") {
    condition <- data.frame(
      min = c(9*3600+55*60, 10*3600+13*60+55, 11*3600+49*60+15, 11*3600+54*60+50),
      max = c(9*3600+58*60, 10*3600+14*60+20, 11*3600+50*60+45, 11*3600+55*60+50)
    )
  }
  
  if(unique_date[i]=="2022-08-26") {
    
    condition <- data.frame(
      min = c(9*3600+41*60, 10*3600+33*60, 10*3600+40*60, 10*3600+45*60),
      max = c(9*3600+42*60, 10*3600+34*60, 10*3600+41*60, 10*3600+47*60)
    )
  }  
  
  if(unique_date[i]=="2022-08-29") {
    
    condition <- data.frame(
      min = c(7*3600+51*60+50, 12*3600),
      max = c(7*3600+52*60+50, 12*3600+1*60)
    )
  }  
  
  if(unique_date[i]=="2022-08-31") {
    
    condition <- data.frame(
      min = c(8*3600+26*60+30, 12*3600+7*60+30, 12*3600+14*60),
      max = c(8*3600+27*60, 12*3600+8*60+30, 12*3600+16*60)
    )
  }  
  
  if(unique_date[i]=="2022-09-01") {
    
    condition <- data.frame(
      min = c(9*3600, 13*3600+37*60+15, 13*3600+44*60+40),
      max = c(9*3600+45, 13*3600+39*60, 13*3600+45*60+40)
    )
  }  
  
  if(unique_date[i]=="2022-09-07") {
    
    condition <- data.frame(
      min = c(12*3600+35*60+15, 12*3600+41*60, 13*3600+41*60),
      max = c(12*3600+37*60, 12*3600+42*60, 13*3600+42*60+30)
    )
  }  
  
  subset.df <- apply(condition, 1, function(row) {
     subset(data, time>row["min"] & time<row["max"])
   })
  
  df.list <- c(df.list, subset.df)
  
}

for (i in seq_along(df.list)) {
  df.list[[i]]$rank <- i  # Add a new column 'Rank' to each dataframe
}
df <- dplyr::bind_rows(df.list)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~2. plot linear regression ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p.n2o <- ggplot(df, mapping = aes(x = X12_CO2/1000, y = X14_N2O))+
  geom_point(mapping = aes(colour = factor(rank)), size = 0.2)+
  geom_smooth(method = "lm", aes(color = factor(rank)), se = FALSE, linewidth = 0.5) + # Use geom_smooth for regression lines
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"),
                   color = factor(rank)),
               formula = y ~ x,
               label.x = 1,
               parse = TRUE, size = 1.5,
               label.y = seq(0.1, 1, 0.05))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_text(8), axis.text = element_text(8),
        plot.tag = element_text(8),
        plot.margin = margin(t = 0, r = 0, b = 2, l = 2, unit = "pt"))+
  ylab( bquote(''*N[2]*O*' [ppb]'))+xlab(bquote(''*CO[2]*' [ppm]'))+labs(tag = "(c)")


p.ch4 <- ggplot(df, mapping = aes(x = X12_CO2/1000, y = X10_CH4))+
  geom_point(mapping = aes(colour = factor(rank)), size = 0.2)+
  geom_smooth(method = "lm", aes(color = factor(rank)), se = FALSE, linewidth = 0.5) + # Use geom_smooth for regression lines
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"),
                   color = factor(rank)),
               formula = y ~ x,
               label.x = 1,
               parse = TRUE, size = 1.5,
               label.y = seq(0.1, 1, 0.05))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_text(8), axis.text = element_text(8),
        plot.tag = element_text(8),
        plot.margin = margin(t = 0, r = 0, b = 2, l = 2, unit = "pt"))+
  ylab( bquote(''*CH[4]*' [ppb]'))+xlab(bquote(''*CO[2]*' [ppm]'))+labs(tag = "(d)")

tiff("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/Figures/SI/'gas_regression_tunnel.tiff", units="mm", width=150, height=75, res=300)
grid.arrange(p.n2o, p.ch4, ncol = 2)
while (!is.null(dev.list()))  dev.off()
