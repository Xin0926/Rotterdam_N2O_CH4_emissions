# This script is for plotting MBA estimates & old inventory & new inventory regarding rotterdam areas
# Author: Xin Tong
# Modified time: 2025-05-15


library(viridis)
###############################################################################################
########################################## N2O ################################################
###############################################################################################
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
  df.new$flag <- "updated inventory"
  
  
  # CORRECTED INVENTORY BASED ON OUR STUDIES
  df.correct <- df.new
  delta <- 211198.7 # gaussian estimates derived by the correlation - inventory emissions
  df.correct$emission[df.correct$source=="Sewerage and wastewater treatment plants"] <- df.correct$emission[df.correct$source=="Sewerage and wastewater treatment plants"]+delta
  factor <- 2.89
  df.correct$emission[df.correct$source=="Traffic and Transportation"] <- df.correct$emission[df.correct$source=="Traffic and Transportation"]*factor
  emi <- 720288
  df.correct$emission[df$source=="Refinery&Energy&Chemical Industry"] <- emi
  df.correct$flag <- "revised inventory\nin this study"
  
  
  
  # LOAD OLD VERSION OF INVENTORY in the rmarkdown file "D:\1 PhD Studies\1 Data\Inventory\The research about the registered emissions in the Dutch website.Rmd"
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  data.old <- read.csv("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/old_inventory.csv")
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  n2o.inventory <- data.old[which(data.old$Stof=="Distikstofoxide" & data.old$V1<4.75), ]
  names(n2o.inventory)[names(n2o.inventory)=="V1"] <- "lon";
  names(n2o.inventory)[names(n2o.inventory)=="V2"] <- "lat"
  
  df <- mutate(n2o.inventory, source = case_when(
    Sector %in% c(
      "Nature", "Other industry", "Trade, Services and Government (HDO).", "Agriculture", "Consumers", "Construction", "Waste Disposal", "Drinking Water Supply"
    ) ~ "Others",
    Sector == "Traffic and Transportation" ~ "Traffic and Transportation",
    Sector == "Sewerage and wastewater treatment plants" ~ "Sewerage and wastewater treatment plants",
    Sector %in% c("Refineries", "Energy", "Chemical Industry") ~ "Refinery&Energy&Chemical Industry", 
  )) %>% 
    group_by(source) %>% summarise(emission = sum(Emissie));
  df.old <- as.data.frame(df)
  df.old <- cbind(df.old, flag = "original inventory\nin Tong et al., 2023")
  
  
  
  
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # LOAD mass balance estimates and LES harbour emissions
  flux <- readxl::read_excel("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/top-down estimates.xlsx", sheet=1); 
  flux <- as.data.frame(flux)
  flux$flag.1 <- c("WIA", "Tong et al., 2023 (without WIA)")
  flux$flag.1 <- factor(flux$flag.1, levels = c('WIA', 'Tong et al., 2023 (without WIA)'))
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  

  

  ###################################### PLOT #########################################
  # put the sector "others' in the bottom of a stacked bar
  df <- rbind(df.old, df.new, df.correct)
  df$source <- factor(
    df$source, 
    levels = c("Refinery&Energy&Chemical Industry", "Sewerage and wastewater treatment plants", "Traffic and Transportation", "Others")
  )
  df$flag <- factor(
    df$flag, 
    levels = c("original inventory\nin Tong et al., 2023", "updated inventory", "revised inventory\nin this study")
  )
  # make a dataframe for plotting inventory's uncertainty
  errorbar <- group_by(df, flag) %>% summarise(sum = sum(emission)/366/24) %>% as.data.frame()
  errorbar$uncertainty <- errorbar$sum*0.38
  
  flux$sum <- c(sum(flux$n2o.emi), flux$n2o.emi[2])
  
  p.n2o <- ggplot()+
    # plot bar for old dutch inventory
    geom_bar(df, mapping = aes(x = flag, group=source, fill=source, y=emission/366/24), stat = 'identity', width = 0.75)+
    geom_errorbar(errorbar, show.legend=FALSE, width = 0.2, 
                  mapping=aes(x = flag, ymax = sum + uncertainty, ymin = sum - uncertainty))+
    
    scale_fill_viridis(discrete = TRUE)+
    labs(fill = 'Inventory') +
    
    # plot bar for mass balance flux
    ggnewscale::new_scale_fill() +
    geom_bar(flux, mapping = aes(x = flag, y=n2o.emi, fill=flag.1), alpha = 0.8, stat = 'identity', width = 0.75)+
    geom_errorbar(flux[1,], show.legend=FALSE, width = 0.2, 
                  mapping=aes(x = flag, ymax = sum + 160, ymin = sum - 160))+
    #scale_fill_brewer(palette = "Set1") + 
    scale_fill_manual(values = c("#CC79A7", "#0072B2"))+
    scale_colour_manual(values = c("#CC79A7", "#0072B2"))+
    labs(fill = 'Top-down estimates', y = bquote(''*N[2]*O*' Emission ['*kg~hr^-1*']'),tag = "(a)") +
    
    theme_bw()+
    theme(legend.position = c(0.25, 0.75), 
          legend.text = element_text(size = 8), legend.margin = margin(0,0,0,0),
          legend.spacing = unit(0,'cm'), legend.key.size = unit(2,'mm'),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.key = element_rect(fill = NA, colour = NA),
          #legend.margin = margin(t = -8, r = 0, b = 0, l = 0, unit = "pt"),
          axis.title.x = element_blank(), 
          axis.text.x = element_text(angle = 10, hjust = 0.7, vjust = 0.9),
          axis.text = element_text(size=10),
          plot.tag = element_text(size = 8),
          plot.margin = margin(t = 5, r = 5, b = 1, l = 5, unit = "pt"))
  p.n2o

  ###############################################################################################
  ##########################################CH4##################################################
  ###############################################################################################
  # PREPARATION for gridmap: run the script in the rmarkdown file "D:\1 PhD Studies\1 Data\Inventory\The research about the registered emissions in the Dutch website.Rmd"
  # UNIT OF INVENTORY: kg/yr
  
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  CH4.inventory <- data.new[which(data.new$Stof=="Methaan" & data.new$V1<4.75), ]
  names(CH4.inventory)[names(CH4.inventory)=="V1"] <- "lon";
  names(CH4.inventory)[names(CH4.inventory)=="V2"] <- "lat"
  
  df <- mutate(CH4.inventory, source = case_when(
    Sector %in% c(
      "Nature", "Other industry", "Trade, Services and Government (HDO).", "Agriculture", "Consumers", "Construction", "Waste Disposal", "Drinking Water Supply"
    ) ~ "Others",
    Sector == "Traffic and Transportation" ~ "Traffic and Transportation",
    Sector == "Sewerage and wastewater treatment plants" ~ "Sewerage and wastewater treatment plants",
    Sector %in% c("Refineries", "Energy", "Chemical Industry") ~ "Refinery&Energy&Chemical Industry", 
  )) %>% 
    group_by(source) %>% summarise(emission = sum(Emissie));
  df.new <- as.data.frame(df)
  df.new$flag <- "updated inventory"
  
  
  # CORRECTED INVENTORY BASED ON OUR STUDIES
  df.correct <- df.new
  delta <- 249913.4 # gaussian estimates derived by the correlation MINUS inventory emissions
  df.correct$emission[df.correct$source=="Sewerage and wastewater treatment plants"] <- df.correct$emission[df.correct$source=="Sewerage and wastewater treatment plants"]+delta
  factor <- 5.54
  df.correct$emission[df.correct$source=="Traffic and Transportation"] <- df.correct$emission[df.correct$source=="Traffic and Transportation"]*factor
  emi <- 14713200
  df.correct$emission[df$source=="Refinery&Energy&Chemical Industry"] <- emi
  df.correct$flag <- "revised inventory\nin this study"
  
  
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  CH4.inventory <- data.old[which(data.old$Stof=="Methaan" & data.old$V1<4.75), ]
  names(CH4.inventory)[names(CH4.inventory)=="V1"] <- "lon";
  names(CH4.inventory)[names(CH4.inventory)=="V2"] <- "lat"
  
  df <- mutate(CH4.inventory, source = case_when(
    Sector %in% c(
      "Nature", "Other industry", "Trade, Services and Government (HDO).", "Agriculture", "Consumers", "Construction", "Waste Disposal", "Drinking Water Supply"
    ) ~ "Others",
    Sector == "Traffic and Transportation" ~ "Traffic and Transportation",
    Sector == "Sewerage and wastewater treatment plants" ~ "Sewerage and wastewater treatment plants",
    Sector %in% c("Refineries", "Energy", "Chemical Industry") ~ "Refinery&Energy&Chemical Industry", 
  )) %>% 
    group_by(source) %>% summarise(emission = sum(Emissie));
  df.old <- as.data.frame(df)
  df.old <- cbind(df.old, flag = "original inventory\nin Tong et al., 2023")
  

  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # LOAD mass balance estimates and LES harbour emissions
  flux <- readxl::read_excel("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/top-down estimates.xlsx", sheet=2); 
  flux <- as.data.frame(flux)
  flux$flag.1 <- c("WIA", "Tong et al., 2023 (without WIA)")
  flux$flag.1 <- factor(flux$flag.1, levels = c('WIA', 'Tong et al., 2023 (without WIA)'))
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  
  
  # put the sector "others' in the bottom of a stacked bar
  df <- rbind(df.old, df.new, df.correct)
  df$source <- factor(
    df$source, 
    levels = c("Refinery&Energy&Chemical Industry", "Sewerage and wastewater treatment plants", "Traffic and Transportation", "Others")
  )
  df$flag <- factor(
    df$flag, 
    levels = c("original inventory\nin Tong et al., 2023", "updated inventory", "revised inventory\nin this study")
  )
  
  
  
  ######################################## plot #########################################
  flux$sum <- c(sum(flux$ch4.emi), flux$ch4.emi[2])
  
  # make a dataframe for plotting inventory's uncertainty
  errorbar <- group_by(df, flag) %>% summarise(sum = sum(emission)/366/24) %>% as.data.frame()
  errorbar$uncertainty <- errorbar$sum*0.1
    
  p.ch4 <- ggplot()+
    # plot bar for old dutch inventory
    geom_bar(df, mapping = aes(x = flag, group=source, fill=source, y=emission/366/24), stat = 'identity', width = 0.75)+
    geom_errorbar(errorbar, show.legend=FALSE, width = 0.2, 
                  mapping=aes(x = flag, ymax = sum + uncertainty, ymin = sum - uncertainty))+
    
    scale_fill_viridis(discrete = TRUE)+
    labs(fill = 'Inventory') +
    
    # plot bar for mass balance flux
    ggnewscale::new_scale_fill() +
    geom_bar(flux, mapping = aes(x = flag, y=ch4.emi, fill=flag.1), alpha = 0.8, stat = 'identity', width = 0.75)+
    geom_errorbar(flux[1,], show.legend=FALSE, width = 0.2, 
                  mapping=aes(x = flag, ymax = sum + 1000, ymin = sum - 1000))+
    #scale_fill_brewer(palette = "Set1") + 
    scale_fill_manual(values = c("#CC79A7", "#0072B2"))+
    scale_colour_manual(values = c("#CC79A7", "#0072B2"))+
    labs(fill = 'Top-down estimates', y = bquote(''*CH[4]*' Emission ['*kg~hr^-1*']'),tag = "(b)" ) +
    
    theme_bw()+
    theme(legend.position = 'none', 
          legend.text = element_text(size = 8), legend.margin = margin(0,0,0,0),
          legend.spacing = unit(0,'cm'), legend.key.size = unit(2,'mm'),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.key = element_rect(fill = NA, colour = NA),
          #legend.margin = margin(t = -8, r = 0, b = 0, l = 0, unit = "pt"),
          axis.title.x = element_blank(),
          axis.text = element_text(size=10),
          axis.text.x = element_text(angle = 10, hjust = 0.7, vjust = 0.9),
          plot.tag = element_text(size = 8),
          plot.margin = margin(t = 5, r = 5, b = 1, l = 5, unit = "pt"))
  
  tiff("D:/1 PhD Studies/3.5 Publications/Manuscript#3_Rotterdam_emission_estimates/Figures/main_body/fig.7.tiff", units="mm", width=300, height=100, res=600)
  grid.arrange(p.n2o, p.ch4, ncol = 2)
  while (!is.null(dev.list()))  dev.off()
  

