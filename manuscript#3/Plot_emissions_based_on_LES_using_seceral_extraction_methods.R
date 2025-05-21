# the code is for plotting EMISSIONS based on LES
# in the SI of chapter_3
# author: Xin Tong
# time: August 30, 2024


sapply(list.files(pattern="[.]R$", path="c:/Users/xin/Dropbox/xin/Rscripts/functions", full.names=TRUE), source)

library(viridis)
# create functions for plotting
create_plot <- function(data1, data2, data3, stats3, data4, stats4){
  # Define the desired order of the legend
  legend_order <- c("2nd method: instantaneous plumes", "2.min", "5.min", "10.min", "20.min")
  colors <- viridis(length(legend_order), option = "viridis")
  
  ggplot()+
    
    # Third dataset
    geom_boxplot(data = data3, mapping = aes(x = index, y = value, colour = flag), width = 0.15, position = position_nudge(x = 0.45)) +
    geom_point(data = stats3, mapping = aes(x = index, y = mean, colour = flag), shape = "diamond", position = position_nudge(x = 0.45)) +
    geom_errorbar(data = stats3, mapping = aes(x = index, y = mean, ymin = mean - sd, ymax = mean + sd, colour = flag), 
                  width = 0.08, position = position_nudge(x = 0.45)) +
    
    # Fourth dataset
    #ggnewscale::new_scale_color()+
    
    geom_boxplot(data = data4, mapping = aes(x = index, y = value, colour = interval), position = position_dodge2(width = 0.9, preserve = 'single')) +
    geom_point(data = stats4, mapping = aes(x = index, y = mean, group = interval, colour = interval), shape = "diamond", position = position_dodge(width = 0.75)) +
    geom_errorbar(data = stats4, mapping = aes(x = index, y = mean, ymin = mean - sd, ymax = mean + sd, group = interval, colour = interval), 
                  width = 0.2, position = position_dodge(width = 0.75))+
    scale_color_manual(
      values = colors,
      breaks = legend_order,        # Set the order of legend items
      labels = legend_order         # Optionally set the labels for the legend items
    )+   
    
    # first dataset
    ggnewscale::new_scale_color()+
    
    geom_line(data1, mapping = aes(x = index, y = emi, group = 1, colour = flag))+
    # second dataset
    geom_line(data2, mapping = aes(x = index, y = emi, group = 1, colour = flag))+
    scale_color_manual(values = c("#CC79A7", "#0072B2"))+
    
    labs(y = bquote(''*N[2]*O*' emission rate ['*kg~h^-1*']'))+
    
    theme_bw()+
    theme(legend.position = 'right', legend.title = element_blank(),
          legend.text = element_text(size = 8), legend.margin = margin(0,0,0,0),
          legend.spacing = unit(0,'cm'), legend.key.size = unit(2,'mm'),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.key = element_rect(fill = NA, colour = NA),
          #legend.margin = margin(t = -8, r = 0, b = 0, l = 0, unit = "pt"),
          axis.title = element_blank(),
          axis.text = element_text(size=8),
          plot.tag = element_text(size = 8),
          plot.margin = margin(t = 5, r = 5, b = 1, l = 5, unit = "pt"))
  
}

############################################### LOAD OR CREATE DATASETS #############################################################
#~~~~~~~~~~~~~~~ FLGIHT 0830 ~~~~~~~~~~~~~~~
# 1st extraction method:
data1 <- data.frame(
  index = c("LES_spline_meas_spline", "LES_spline_meas_loess", "LES_loess_meas_spline", "LES_loess_meas_loess"),
  emi = c(182, 154, 190, 161),
  flag = "1st method"
)
# 2nd extraction method: plume averaged from the whole simulation period
data2 <- data.frame(
  index = c("LES_spline_meas_spline", "LES_spline_meas_loess", "LES_loess_meas_spline", "LES_loess_meas_loess"),
  emi = c(139, 118, 152, 129),
  flag = "2nd method: average plume"
)

# 2nd extraction method: instantaneous plumes
df <- read.csv("D:/1 PhD Studies/1 Data/LES/0830/results/n2o.layers.csv")
colnames(df)[1] <- "meas"
melt <- melt(df, id.vars = "meas")
str <- strsplit(as.character(melt$variable), "_")
datetime <- sapply(str, function(x) x[1]);
datetime <- as.POSIXct(sub("^X", "", datetime), format = "%Y.%m.%d.%H.%M.%S", tz = "UTC")
fitting <- sapply(str, function(x) x[2]);
fitting[which(fitting=="smooth")] <- "spline"
df <- cbind(melt, datetime, fitting)

results <- list()
stats3 <- NULL
for(j in unique(df$fitting)){
  
  for(k in unique(df$meas)){
    
    df1 <- df[df$meas==k & df$fitting==j, ]
    
    mean <- mean(df1$value, na.rm = TRUE);
    median <- median(df1$value, na.rm = TRUE);
    min <- min(df1$value, na.rm = TRUE);
    max <- max(df1$value, na.rm = TRUE);
    sd <- sd(df1$value, na.rm = TRUE)
    
    name <- paste('LES', j, 'meas', k, sep = "_")
    
    stats3 <- rbind(
      stats3, data.frame(
        species="n2o", LES=j, meas=k, min=min, max=max, mean=mean, sd=sd, median=median, index=name
      )
    )
    
    df1 <- cbind(df1, index = name)
    results[[name]] <- df1
  }
}

data3 <- do.call(rbind, results)
data3$flag <- "2nd method: instantaneous plumes"
stats3$flag <- "2nd method: instantaneous plumes"

# 2nd extraction method: plumes averaged from several time periods
df <- read.csv("D:/1 PhD Studies/1 Data/LES/0830/results/n2o.layers.diff.interval.csv")
colnames(df)[1] <- "meas"
melt <- melt(df, id.vars = "meas")
str <- strsplit(as.character(melt$variable), "_")
interval <- sapply(str, function(x) x[2])
datetime <- sapply(str, function(x) x[3]);
datetime <- as.POSIXct(sub("^X", "", datetime), format = "%Y.%m.%d.%H.%M.%S", tz = "UTC")
fitting <- sapply(str, function(x) x[4]);
fitting[which(fitting=="smooth")] <- "spline"
df <- cbind(melt, interval, datetime, fitting)


results <- vector("list", 16)
stats4 <- NULL
z <- 1
for(j in unique(df$fitting)){
  
  for(k in unique(df$meas)){
    
    for(m in unique(df$interval)){
      
      df1 <- df[df$meas==k & df$fitting==j & df$interval==m, ]
      
      mean <- mean(df1$value, na.rm = TRUE);
      median <- median(df1$value, na.rm = TRUE);
      min <- min(df1$value, na.rm = TRUE);
      max <- max(df1$value, na.rm = TRUE);
      sd <- sd(df1$value, na.rm = TRUE)
      
      name <- paste('LES', j, 'meas', k, sep = "_")
      
      stats4 <- rbind(
        stats4, data.frame(
          species="n2o", LES=j, interval=m, meas=k, min=min, max=max, mean=mean, sd=sd, median=median, index=name
        )
      )
      
      df1 <- cbind(df1, index = name)
      results[[z]] <- df1
      z <- z+1
    }
  }
}

data4 <- do.call(rbind, results)
data4$flag <- "2nd method: plumes avergaed from periods"
stats4$flag <- "2nd method: plumes avergaed from periods"

p.n2o.0830 <- create_plot(data1, data2, data3, stats3, data4, stats4)+
  facet_zoom(ylim = c(110, 500),zoom.size = 0.7)+labs(tag = "(a)")
  


#~~~~~~~~~~~~~~~ FLGIHT 0901 ~~~~~~~~~~~~~~~
# 1st extraction method:
data1 <- data.frame(
  index = c("LES_spline_meas_spline", "LES_spline_meas_loess", "LES_loess_meas_spline", "LES_loess_meas_loess"),
  emi = c(28,40,25,35),
  flag = "1st method"
)
# 2nd extraction method: plume averaged from the whole simulation period
data2 <- data.frame(
  index = c("LES_spline_meas_spline", "LES_spline_meas_loess", "LES_loess_meas_spline", "LES_loess_meas_loess"),
  emi = c(30,42,31,44),
  flag = "2nd method: average plume"
)

# 2nd extraction method: instantaneous plumes
df <- read.csv("D:/1 PhD Studies/1 Data/LES/0901/results/n2o.layers.csv")
colnames(df)[1] <- "meas"
melt <- melt(df, id.vars = "meas")
str <- strsplit(as.character(melt$variable), "_")
datetime <- sapply(str, function(x) x[1]);
datetime <- as.POSIXct(sub("^X", "", datetime), format = "%Y.%m.%d.%H.%M.%S", tz = "UTC")
fitting <- sapply(str, function(x) x[2]);
fitting[which(fitting=="smooth")] <- "spline"
df <- cbind(melt, datetime, fitting)

results <- list()
stats3 <- NULL
for(j in unique(df$fitting)){
  
  for(k in unique(df$meas)){
    
    df1 <- df[df$meas==k & df$fitting==j, ]
    
    mean <- mean(df1$value, na.rm = TRUE);
    median <- median(df1$value, na.rm = TRUE);
    min <- min(df1$value, na.rm = TRUE);
    max <- max(df1$value, na.rm = TRUE);
    sd <- sd(df1$value, na.rm = TRUE)
    
    name <- paste('LES', j, 'meas', k, sep = "_")
    
    stats3 <- rbind(
      stats3, data.frame(
        species="n2o", LES=j, meas=k, min=min, max=max, mean=mean, sd=sd, median=median, index=name
      )
    )
    
    df1 <- cbind(df1, index = name)
    results[[name]] <- df1
  }
}

data3 <- do.call(rbind, results)
data3$flag <- "2nd method: instantaneous plumes"
stats3$flag <- "2nd method: instantaneous plumes"

# 2nd extraction method: plumes averaged from several time periods
df <- read.csv("D:/1 PhD Studies/1 Data/LES/0901/results/n2o.layers.diff.interval.csv")
colnames(df)[1] <- "meas"
melt <- melt(df, id.vars = "meas")
str <- strsplit(as.character(melt$variable), "_")
interval <- sapply(str, function(x) x[2])
datetime <- sapply(str, function(x) x[3]);
datetime <- as.POSIXct(sub("^X", "", datetime), format = "%Y.%m.%d.%H.%M.%S", tz = "UTC")
fitting <- sapply(str, function(x) x[4]);
fitting[which(fitting=="smooth")] <- "spline"
df <- cbind(melt, interval, datetime, fitting)


results <- vector("list", 16)
stats4 <- NULL
z <- 1
for(j in unique(df$fitting)){
  
  for(k in unique(df$meas)){
    
    for(m in unique(df$interval)){
      
      df1 <- df[df$meas==k & df$fitting==j & df$interval==m, ]
      
      mean <- mean(df1$value, na.rm = TRUE);
      median <- median(df1$value, na.rm = TRUE);
      min <- min(df1$value, na.rm = TRUE);
      max <- max(df1$value, na.rm = TRUE);
      sd <- sd(df1$value, na.rm = TRUE)
      
      name <- paste('LES', j, 'meas', k, sep = "_")
      
      stats4 <- rbind(
        stats4, data.frame(
          species="n2o", LES=j, interval=m, meas=k, min=min, max=max, mean=mean, sd=sd, median=median, index=name
        )
      )
      
      df1 <- cbind(df1, index = name)
      results[[z]] <- df1
      z <- z+1
    }
  }
}

data4 <- do.call(rbind, results)
data4$flag <- "2nd method: plumes avergaed from periods"
stats4$flag <- "2nd method: plumes avergaed from periods"

p.n2o.0901 <- create_plot(data1, data2, data3, stats3, data4, stats4)+
  facet_zoom(ylim = c(20, 50),zoom.size = 0.7)+labs(tag = "(b)")


#~~~~~~~~~~~~~~~ FLGIHT 0906 ~~~~~~~~~~~~~~~
# N2O
# 1st extraction method:
data1 <- data.frame(
  index = c("LES_spline_meas_spline", "LES_spline_meas_loess", "LES_loess_meas_spline", "LES_loess_meas_loess"),
  emi = c(35,33,33,31),
  flag = "1st method"
)
# 2nd extraction method: plume averaged from the whole simulation period
data2 <- data.frame(
  index = c("LES_spline_meas_spline", "LES_spline_meas_loess", "LES_loess_meas_spline", "LES_loess_meas_loess"),
  emi = c(41,43,42,45),
  flag = "2nd method: average plume"
)

# 2nd extraction method: instantaneous plumes
df <- read.csv("D:/1 PhD Studies/1 Data/LES/0906/results/n2o.layers.csv")
  colnames(df)[1] <- "meas"
  melt <- melt(df, id.vars = "meas")
  str <- strsplit(as.character(melt$variable), "_")
  datetime <- sapply(str, function(x) x[1]);
  datetime <- as.POSIXct(sub("^X", "", datetime), format = "%Y.%m.%d.%H.%M.%S", tz = "UTC")
  fitting <- sapply(str, function(x) x[2]);
  fitting[which(fitting=="smooth")] <- "spline"
  df <- cbind(melt, datetime, fitting)
  
  results <- list()
  stats3 <- NULL
  for(j in unique(df$fitting)){
    
    for(k in unique(df$meas)){
      
      df1 <- df[df$meas==k & df$fitting==j, ]
      
      mean <- mean(df1$value, na.rm = TRUE);
      median <- median(df1$value, na.rm = TRUE);
      min <- min(df1$value, na.rm = TRUE);
      max <- max(df1$value, na.rm = TRUE);
      sd <- sd(df1$value, na.rm = TRUE)
      
      name <- paste('LES', j, 'meas', k, sep = "_")
      
      stats3 <- rbind(
        stats3, data.frame(
          species="n2o", LES=j, meas=k, min=min, max=max, mean=mean, sd=sd, median=median, index=name
        )
      )
      
      df1 <- cbind(df1, index = name)
      results[[name]] <- df1
    }
  }

  data3 <- do.call(rbind, results)
  data3$flag <- "2nd method: instantaneous plumes"
  stats3$flag <- "2nd method: instantaneous plumes"
  
# 2nd extraction method: plumes averaged from several time periods
  df <- read.csv("D:/1 PhD Studies/1 Data/LES/0906/results/n2o.layers.diff.interval.csv")
    colnames(df)[1] <- "meas"
    melt <- melt(df, id.vars = "meas")
    str <- strsplit(as.character(melt$variable), "_")
    interval <- sapply(str, function(x) x[2])
    datetime <- sapply(str, function(x) x[3]);
    datetime <- as.POSIXct(sub("^X", "", datetime), format = "%Y.%m.%d.%H.%M.%S", tz = "UTC")
    fitting <- sapply(str, function(x) x[4]);
    fitting[which(fitting=="smooth")] <- "spline"
    df <- cbind(melt, interval, datetime, fitting)

  
  results <- vector("list", 16)
  stats4 <- NULL
  z <- 1
    for(j in unique(df$fitting)){
      
      for(k in unique(df$meas)){
        
        for(m in unique(df$interval)){
          
          df1 <- df[df$meas==k & df$fitting==j & df$interval==m, ]
          
          mean <- mean(df1$value, na.rm = TRUE);
          median <- median(df1$value, na.rm = TRUE);
          min <- min(df1$value, na.rm = TRUE);
          max <- max(df1$value, na.rm = TRUE);
          sd <- sd(df1$value, na.rm = TRUE)
          
          name <- paste('LES', j, 'meas', k, sep = "_")
          
          stats4 <- rbind(
            stats4, data.frame(
              species="n2o", LES=j, interval=m, meas=k, min=min, max=max, mean=mean, sd=sd, median=median, index=name
            )
          )
          
          df1 <- cbind(df1, index = name)
          results[[z]] <- df1
          z <- z+1
        }
      }
    }

  data4 <- do.call(rbind, results)
  data4$flag <- "2nd method: plumes avergaed from periods"
  stats4$flag <- "2nd method: plumes avergaed from periods"
  
  # CH4
  # ch4 <- TRUE
  # if(ch4==TRUE){
  #   
  # }
  
  p.n2o.0906 <- create_plot(data1, data2, data3, stats3, data4, stats4)+
    facet_zoom(ylim = c(20, 150),zoom.size = 0.7)+labs(tag = "(c)")
  
  
############################################### plot #############################################################

# apply the functions 

library(patchwork)
p <- p.n2o.0830 + p.n2o.0901 + p.n2o.0906+ 
  plot_layout(guides = 'collect', nrow = 3) &
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.tag.location = "margin", plot.tag.position = "topleft",
        plot.title = element_text(hjust = 0.5))


while (!is.null(dev.list()))  dev.off()
ggsave("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/Figures/SI/n2o.emi.les.tiff", plot = p,
       width = 300, height = 200, units = "mm", dpi = 300, compression = "lzw")
