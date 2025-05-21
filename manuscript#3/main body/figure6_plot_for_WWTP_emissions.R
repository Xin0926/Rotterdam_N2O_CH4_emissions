# the code is for plotting emissions from WWTPs for the main body of the ms
# author: Xin Tong
# time: JAN 5, 2025


########################################################################################################################
# The standard deviation of CH4 emissions from Kravling is from Daleman et al., 2012
# The standard deviation of N2O emissions from Kravling is picked up from figure 1 in Daleman et al., 2013


# Uncertainty OF INVENTORY:  FROM 7.5.3 ON PAGE 279 IN THE DUTCH NATIONAL INVENTORY REPORT 2022
# The uncertainty analysis in Annex 2 provides estimates of uncertainties by
# IPCC source category and gas that are based on error propagation. 
# The uncertainty in annual N2O and CH4 emissions from wastewater handling is
# estimated to be 23% and 40%, respectively.



# uncertainty of single-transect IGM quantified emissions:
# Using this methodology we obtain an uncertainty range for single-transect emissions of 0.05–6.5q, where q is the emission rate;
########################################################################################################################








#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! VERSION 1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#~~~~~~~~~~~~~~~~~~~~~~~1. create dataframe for plotting ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#########################
df <- readxl::read_xlsx("D:/1 PhD Studies/3.1 Results of projects/2022_Rotterdam_Campaign/WWTP_emission.xlsx", sheet = 2)
df <- as.data.frame(df)
df[, 3] <- as.numeric(df[, 3]);
df[, 4] <- as.numeric(df[, 4]);
df[, 5] <- as.numeric(df[, 5]);
df[, 6] <- as.numeric(df[, 6]);
df[, 7] <- as.numeric(df[, 7]);
df[, 8] <- as.numeric(df[, 8]);
df[, 9] <- as.numeric(df[, 9]);
df[, 10] <- as.numeric(df[, 10])

model.df <- data.frame(
  WWTP = df[1:4, "WWTP"],
  
  n2o.emi.igm = df[1:4, "emission_N2O"],
  n2o.emi.ivt = df[5:8, "emission_N2O"],
  n2o.sd.igm = df[1:4, "emission_N2O_uncertainty"],
  n2o.sd.ivt = df[5:8, "emission_N2O_uncertainty"],
  
  ch4.emi.igm = df[1:4, "emission_CH4"],
  ch4.emi.ivt = df[5:8, "emission_CH4"],
  ch4.sd.igm = df[1:4, "emission_CH4_uncertainty"],
  ch4.sd.ivt = df[5:8, "emission_CH4_uncertainty"]
)


#########################
data <- read.csv("D:/1 PhD Studies/1 Data/Inventory/Dutch_inventory/N2O_CH4_CO2_CO_emission_Rotterdam_area_company_2020_2021_2022.csv")
data <- mutate(data, emission = Emissie/366/24) # convert unit from kg/yr to kg/hr

# select data by conditions for n2o
condition <- data$Stof=="Distikstofoxide" & 
  data$Jaar=="2020" & 
  (data$Sector=="Sewerage and wastewater treatment plants")
data.n2o <- group_by(data[condition, ], Bedrijf, lon, lat) %>% 
  summarise(emission = sum(emission))
data.n2o <- as.data.frame(data.n2o)

# select data by conditions for ch4
condition <- data$Stof=="Methaan" & 
  data$Jaar=="2020" & 
  (data$Sector=="Sewerage and wastewater treatment plants")
data.ch4 <- group_by(data[condition, ], Bedrijf, lon, lat) %>% 
  summarise(emission = sum(emission))
data.ch4 <- as.data.frame(data.ch4)
locations_df <- filter(data.ch4, Bedrijf %in% c("AWZI Kralingseveer", "RWZI Barendrecht", "RWZI Ridderkerk", "RWZI Rozenburg"))

#########################
# create a function for plotting linear model 
plot_linear_fit <- function(data, pred_data, x_col, y_col, x_sd, y_sd) {
  # Create formula strings
  linear_formula <- as.formula(paste(y_col, "~", x_col))
  model <- lm(linear_formula, data = data)
  
  # Create prediction dataframe
  pred_data <- data.frame(
    emi = pred_data$emission
  )
  names(pred_data) <- x_col
  
  # Generate predictions
  predictions <- predict(model, newdata = pred_data)
  
  # Get model statistics
  model_summary <- summary(model)
  r2 <- round(model_summary$r.squared, 3)
  p_value <- format(model_summary$coefficients[2,4], scientific = TRUE, digits = 3)
  stats_label <- sprintf("R² = %s\np = %s", r2, p_value)
  
  # Set color based on x_col
  set_color <- if(x_col == "ch4.emi.ivt")  "#0072B2" else if(x_col == "n2o.emi.ivt") "#CC79A7" else "black"
  
  # Create plot
  p <- ggplot() +
  
    geom_line(data = data.frame(x = pred_data[[x_col]], y = predictions),
              aes(x = x, y = y), size = 1) +  # Fitted line
    
    geom_point(data = data.frame(x = pred_data[[x_col]], y = predictions), stroke = 2, alpha = 0.5,
               aes(x = x, y = y), shape = 3, size = 4, colour = set_color)+ # upscaled emissions based on the relationship
    
    geom_point(data, mapping = aes_string(x = x_col, y = y_col), size = 2) +  # Data points
    geom_errorbar(data, mapping = aes_string(x = x_col, ymax = paste0(y_col, "+", y_sd), 
                                             ymin = paste0(y_col, "-", y_sd)), width= 0.1) + 
    geom_errorbarh(data, mapping = aes_string(
      y = y_col, xmax = paste0(x_col, "+", x_sd), xmin = paste0(x_col, "-", x_sd))) + 
    
    # Replace geom_text with geom_text_repel for better label placement
    geom_text_repel(data = data,
                    aes_string(x = x_col, y = y_col, label = "WWTP"),
                    box.padding = 0.5,    # Padding around the text
                    point.padding = 0.3,  # Padding from the point
                    force = 2,            # Force of repulsion between labels
                    segment.color = "grey50",  # Color of connecting lines
                    min.segment.length = 0,    # Allow short segments
                    max.overlaps = Inf,        # Don't hide any labels
                    seed = 123) +              # Set seed for reproducible label positions
    
    annotate("text", x = min(pred_data[[x_col]], na.rm = TRUE), 
             y = max(data[[y_col]], na.rm = TRUE),
             label = stats_label, hjust = 0, vjust = 1)  # Statistics

  return(p)
}






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~2. plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# p.n2o <- plot_linear_fit(model.df, data.n2o, "n2o.emi.ivt", "n2o.emi.igm", "n2o.sd.ivt", "n2o.sd.igm")
# 
# p.n2o <- p.n2o +
#   labs(x = bquote(''*N[2]*O*' inventory emission ['*kg~h^-1*']'),
#        y = bquote('Estimated '*N[2]*O*' emission ['*kg~h^-1*']'))+
#   theme_bw()+
#   theme(legend.position = c(0.25, 0.75), legend.title = element_blank(),
#         legend.text = element_text(size = 8), legend.margin = margin(0,0,0,0),
#         legend.spacing = unit(0,'cm'), #legend.key.size = unit(2,'mm'),
#         legend.background = element_rect(fill = "transparent", colour = NA),
#         legend.key = element_rect(fill = NA, colour = NA),
#         #legend.margin = margin(t = -8, r = 0, b = 0, l = 0, unit = "pt"),
#         axis.text = element_text(size=8),
#         plot.tag = element_text(size = 8),
#         plot.margin = margin(t = 5, r = 5, b = 1, l = 5, unit = "pt"))
# 
# 
# 
# p.ch4 <- plot_linear_fit(model.df, data.ch4, "ch4.emi.ivt", "ch4.emi.igm", "ch4.sd.ivt", "ch4.sd.igm")
# 
# p.ch4 <- p.ch4+
#   labs(x = bquote(''*CH[4]*' inventory emission ['*kg~h^-1*']'),
#        y = bquote('Estimated '*CH[4]*' emission ['*kg~h^-1*']'))+
#   theme_bw()+
#   theme(legend.position = c(0.25, 0.75), legend.title = element_blank(),
#         legend.text = element_text(size = 8), legend.margin = margin(0,0,0,0),
#         legend.spacing = unit(0,'cm'), #legend.key.size = unit(2,'mm'),
#         legend.background = element_rect(fill = "transparent", colour = NA),
#         legend.key = element_rect(fill = NA, colour = NA),
#         #legend.margin = margin(t = -8, r = 0, b = 0, l = 0, unit = "pt"),
#         axis.text = element_text(size=8),
#         plot.tag = element_text(size = 8),
#         plot.margin = margin(t = 5, r = 5, b = 1, l = 5, unit = "pt"))





#########################
################################ N2O #####################################
lm <- lm(n2o.emi.igm ~ n2o.emi.ivt, model.df)
names(data.n2o)[4] <- 'n2o.emi.ivt'
pre <- sum(predict(lm, newdata = data.n2o['n2o.emi.ivt']))

df.new <- readxl::read_excel("D:/1 PhD Studies/1 Data/Inventory/Dutch_Inventory/N2O_emission_Rotterdam_point_diffuse_sector.xlsx", sheet = 3);
df.new.n2o <- as.data.frame(df.new[df.new$Sector=="Sewerage and wastewater treatment plants", ]);
df.new.n2o$emission <- df.new.n2o$emission/366/24
df.this.study.n2o <- df.new.n2o 
df.this.study.n2o[df.this.study.n2o$flag=='Point', 'emission'] <- pre



# create a function for calculating the sum and its uncertainty from a linear model:
calc_sum_uncertainty <- function(model, new_data) {
  # Check if inputs are valid
  if (!inherits(model, "lm")) {
    stop("'model' must be a linear model object (class 'lm')")
  }
  if (!is.data.frame(new_data)) {
    stop("'new_data' must be a data frame")
  }
  
  # Get predictions and their uncertainty
  X_new <- model.matrix(model$terms, new_data)
  sigma2 <- summary(model)$sigma^2
  vcov_matrix <- sigma2 * X_new %*% vcov(model) %*% t(X_new)
  
  # Calculate sum and its standard error
  sum_pred <- sum(predict(model, new_data))
  se_sum <- sqrt(sum(vcov_matrix))
  
  # Calculate 95% confidence interval
  t_value <- qt(0.975, df = model$df.residual)
  ci_lower <- sum_pred - t_value * se_sum
  ci_upper <- sum_pred + t_value * se_sum
  
  # Return results as a list
  results <- list(
    sum = sum_pred,
    se = se_sum,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
  
  return(results)
}
# Create new data frame with the PREDICTOR variable name matching the model
new_data <- data.frame(n2o.emi.ivt = data.n2o$n2o.emi.ivt)
results <- calc_sum_uncertainty(lm, new_data)






############################## CH4 ##############################
lm <- lm(ch4.emi.igm ~ ch4.emi.ivt, model.df)
names(data.ch4)[4] <- 'ch4.emi.ivt'
pre <- sum(predict(lm, newdata = data.ch4['ch4.emi.ivt']))

df.new <- readxl::read_excel("D:/1 PhD Studies/1 Data/Inventory/Dutch_Inventory/CH4_emission_Rotterdam_point_diffuse_sector.xlsx", sheet = 3);
df.new.ch4 <- as.data.frame(df.new[df.new$Sector=="Sewerage and wastewater treatment plants", ]);
df.new.ch4$emission <- df.new.ch4$emission/366/24
df.this.study.ch4 <- df.new.ch4 
df.this.study.ch4[df.this.study.ch4$flag=='Point', 'emission'] <- pre

df.new.n2o$flag <- factor(df.new.n2o$flag, levels = c("Point", "diffuse"));
df.this.study.n2o$flag <- factor(df.this.study.n2o$flag, levels = c("Point", "diffuse"));
df.new.ch4$flag <- factor(df.new.ch4$flag, levels = c("Point", "diffuse"));
df.this.study.ch4$flag <- factor(df.this.study.ch4$flag, levels = c("Point", "diffuse"))



n2o.total.int <- sum(df.new.n2o$emission);
n2o.total.this.study <- sum(df.this.study.n2o$emission);
ch4.total.int <- sum(df.new.ch4$emission);
ch4.total.this.study <- sum(df.this.study.ch4$emission)

n2o.error <- data.frame(
  height = c(n2o.total.int, n2o.total.this.study),
  lower = c(n2o.total.int-n2o.total.int*0.23, n2o.total.this.study - n2o.total.this.study*0.23),
  upper = c(n2o.total.int+n2o.total.int*0.23, n2o.total.this.study + n2o.total.this.study*0.23)
)

ch4.error <- data.frame(
  height = c(ch4.total.int, ch4.total.this.study),
  lower = c(ch4.total.int-ch4.total.int*0.4, ch4.total.this.study - ch4.total.this.study*0.4),
  upper = c(ch4.total.int+ch4.total.int*0.4, ch4.total.this.study + ch4.total.this.study*0.4)
)

#################### plot ######################

p <- ggplot()+
  
  # plot LEFT Y AXIS (N2O)
  geom_bar(df.new.n2o, mapping = aes(x = 1, y = emission, fill = flag), alpha = 0.8,
           stat = 'identity', width = 0.3, position = position_stacknudge(x = -0.18)) + 
  geom_bar(df.this.study.n2o, mapping = aes(x = 1, y = emission, fill = flag), alpha = 0.8,
           stat = 'identity', width = 0.3, position = position_stacknudge(x = 0.18)) + 
  geom_errorbar(n2o.error, mapping = aes(x = 1, ymin = lower, ymax = upper), 
                width = 0.1, position = position_nudge(x = c(-0.18, 0.18)))+
  
  scale_fill_manual(values = c("#CC79A7", "orchid4"), labels = c("WWTPs", "Sewage System"))+
  
  # Add labels above bars for N2O
  annotate("text", x = 1, y = n2o.total.int * 0.5, 
           label = "Inventory", hjust = 1.3) +
  annotate("text", x = 1, y = n2o.total.this.study * 0.5, 
           label = "This study", hjust = -0.3) +
  
  
  # plot bar for mass balance flux
  ggnewscale::new_scale_fill() +
  
  # plot RIGHT Y AXIS (CH4)
  geom_bar(df.new.ch4, mapping = aes(x = 2, y = emission/2, fill = flag), alpha = 0.8,
           stat = 'identity', width = 0.3, position = position_stacknudge(x = -0.18)) + 
  geom_bar(df.this.study.ch4, mapping = aes(x = 2, y = emission/2, fill = flag), alpha = 0.8,
           stat = 'identity', width = 0.3, position = position_stacknudge(x = 0.18)) + 
  geom_errorbar(ch4.error, mapping = aes(x = 2, ymin = lower/2, ymax = upper/2), 
                width = 0.1, position = position_nudge(x = c(-0.18, 0.18)))+
  
  scale_fill_manual(values = c("#0072B2", "dodgerblue4"), labels = c("WWTPs", "Sewage System"))+
  
  # Add labels above bars for CH4
  annotate("text", x = 2, y = ch4.total.int * 0.5 /2, 
           label = "Inventory", hjust = 1.3) +
  annotate("text", x = 2, y = ch4.total.this.study * 0.5 /2, 
           label = "This study", hjust = -0.3) +
  
  scale_y_continuous(
    name = bquote(''*N[2]*O*' emission rate ['*kg~h^-1*']'), 
    sec.axis = sec_axis(~.*2, name = bquote(''*CH[4]*' emission rate ['*kg~h^-1*']'))
  )+
  ggtitle('Emissions for Sewerage and wastewater treatment plants')+
  
  theme_bw()+
  theme(legend.position = c(0.35, 0.75), legend.title = element_blank(),
        legend.text = element_text(), legend.margin = margin(0,0,0,0),
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
        plot.tag = element_text(), plot.title = element_text(hjust = 0.5),
        plot.margin = margin(t = 5, r = 5, b = 1, l = 5, unit = "pt"))

p




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~3. organize and place figures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# tiff("D:\\1 PhD Studies\\3.5 Publications\\Manuscript#3_Rotterdam_emission_estimates\\Figures\\fig.6.WWTP.tiff", units="mm", width=400, height=100, res=300)
# grid.arrange(p.n2o+labs(tag = "(a)"), p.ch4+labs(tag = "(b)"), p+labs(tag = "(c)"), ncol = 3)
# while (!is.null(dev.list()))  dev.off()
















#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! VERSION 2 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# run the above code and keep the plot "p"

df <- readxl::read_xlsx("D:/1 PhD Studies/3.1 Results of projects/2022_Rotterdam_Campaign/WWTP_emission.xlsx", sheet = 2)
df <- as.data.frame(df)
# Convert specific numeric columns one by one
df$emission_N2O <- as.numeric(df$emission_N2O)
df$emission_N2O_uncertainty <- as.numeric(df$emission_N2O_uncertainty)
df$emission_CH4 <- as.numeric(df$emission_CH4)
df$emission_CH4_uncertainty <- as.numeric(df$emission_CH4_uncertainty)
df$N2O_lower <- as.numeric(df$N2O_lower)
df$N2O_upper <- as.numeric(df$N2O_upper)
df$CH4_lower <- as.numeric(df$CH4_lower)
df$CH4_upper <- as.numeric(df$CH4_upper)

p.n2o <- ggplot() +
  # IGM data for specific WWTPs
  geom_bar(data = subset(df, flag == "IGM" & WWTP %in% c("Barendrecht", "Ridderkerk")),
           aes(x = WWTP, y = emission_N2O, fill = flag),
           stat = "identity", width = 0.3, alpha = 0.8
           ) +
  geom_errorbar(data = subset(df, flag == "IGM" & WWTP %in% c("Barendrecht", "Ridderkerk")),
                aes(x = WWTP, ymin = N2O_lower, ymax = N2O_upper, group = flag, colour = flag),
                width = 0.1) +

  # IGM data for specific WWTP Kralingseveer
  geom_bar(data = subset(df, flag == "IGM" & WWTP %in% c("Kralingseveer")),
           aes(x = WWTP, y = emission_N2O, fill = flag),
           stat = "identity", width = 0.3, alpha = 0.8) +
  geom_errorbar(data = subset(df, flag == "IGM" & WWTP %in% c("Kralingseveer")),
                aes(x = WWTP, ymin = emission_N2O - emission_N2O_uncertainty, ymax = emission_N2O + emission_N2O_uncertainty,
                    group = flag, colour = flag),
                width = 0.1) +
  
  # # Inventory data
  geom_bar(data = subset(df, flag == "Inventory" & WWTP != "Rozenburg"),
           aes(x = WWTP, y = emission_N2O, fill = flag),
           stat = "identity", alpha = 0.8,
           position = position_nudge(x = 0.32), width = 0.3) +
  geom_errorbar(data = subset(df, flag == "Inventory" & WWTP != "Rozenburg"),
                aes(x = WWTP,
                    ymin = emission_N2O - emission_N2O_uncertainty,  # Changed to minus for lower bound
                    ymax = emission_N2O + emission_N2O_uncertainty,
                    group = flag, colour = flag),
                position = position_nudge(x = 0.32),
                width = 0.1) +

  # # Literature data
  geom_bar(data = subset(df, flag == "literature"),
           aes(x = WWTP, y = emission_N2O, fill = flag),
           stat = "identity", alpha = 0.8,
           position = position_nudge(x = -0.32), width = 0.3) +
  geom_errorbar(data = subset(df, flag == "literature"),
                aes(x = WWTP,
                    ymin = emission_N2O - emission_N2O_uncertainty,  # Changed to minus for lower bound
                    ymax = emission_N2O + emission_N2O_uncertainty,
                    group = flag, colour = flag),
                position = position_nudge(x = -0.32),
                width = 0.1) +
  
  # Colors and themes
  scale_fill_viridis(discrete = TRUE, 
                     option = "viridis",
                     labels = function(x) ifelse(x == "literature", 
                                                 "Daleman et al., 2013", 
                                                 x)) +   # Changed legend label
  scale_color_viridis(discrete = TRUE,
                      option = "viridis",
                      labels = function(x) ifelse(x == "literature",
                                                  "Daleman et al., 2013",
                                                  x)) +  # Changed legend label
  ylab(expression(N[2]*O~"emission"~"["*kg~h^{-1}*"]"))+
  ggtitle('Quantified emissions from wastewater treatment plants')+
  
  theme_bw()+
  theme(legend.position = c(0.75, 0.75), legend.title = element_blank(),
        legend.text = element_text(), legend.margin = margin(0,0,0,0),
        legend.spacing = unit(0,'cm'), #legend.key.size = unit(2,'mm'),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = NA, colour = NA),
        #legend.margin = margin(t = -8, r = 0, b = 0, l = 0, unit = "pt"),
        axis.title.x = element_blank(),
        axis.text = element_text(),
        plot.tag = element_text(), plot.title = element_text(hjust = 0.5),
        plot.margin = margin(t = 5, r = 5, b = 1, l = 5, unit = "pt"))


p.ch4 <- ggplot() +
  # IGM data for specific WWTPs
  geom_bar(data = subset(df, flag == "IGM" & WWTP %in% c("Barendrecht", "Ridderkerk")),
           aes(x = WWTP, y = emission_CH4, fill = flag),
           stat = "identity", width = 0.3, alpha = 0.8
  ) +
  geom_errorbar(data = subset(df, flag == "IGM" & WWTP %in% c("Barendrecht", "Ridderkerk")),
                aes(x = WWTP, ymin = CH4_lower, ymax = CH4_upper, group = flag, colour = flag),
                width = 0.1) +
  
  # IGM data for specific WWTP Kralingseveer
  geom_bar(data = subset(df, flag == "IGM" & WWTP %in% c("Kralingseveer")),
           aes(x = WWTP, y = emission_CH4, fill = flag),
           stat = "identity", width = 0.3, alpha = 0.8) +
  geom_errorbar(data = subset(df, flag == "IGM" & WWTP %in% c("Kralingseveer")),
                aes(x = WWTP, ymin = emission_CH4 - emission_CH4_uncertainty, ymax = emission_CH4 + emission_CH4_uncertainty,
                    group = flag, colour = flag),
                width = 0.1) +
  
  # # Inventory data
  geom_bar(data = subset(df, flag == "Inventory"),
           aes(x = WWTP, y = emission_CH4, fill = flag),
           stat = "identity", alpha = 0.8,
           position = position_nudge(x = 0.32), width = 0.3) +
  geom_errorbar(data = subset(df, flag == "Inventory"),
                aes(x = WWTP,
                    ymin = emission_CH4 - emission_CH4_uncertainty,  # Changed to minus for lower bound
                    ymax = emission_CH4 + emission_CH4_uncertainty,
                    group = flag, colour = flag),
                position = position_nudge(x = 0.32),
                width = 0.1) +
  
  # # Literature data
  geom_bar(data = subset(df, flag == "literature"),
           aes(x = WWTP, y = emission_CH4, fill = flag),
           stat = "identity", alpha = 0.8,
           position = position_nudge(x = -0.32), width = 0.3) +
  geom_errorbar(data = subset(df, flag == "literature"),
                aes(x = WWTP,
                    ymin = emission_CH4 - emission_CH4_uncertainty,  # Changed to minus for lower bound
                    ymax = emission_CH4 + emission_CH4_uncertainty,
                    group = flag, colour = flag),
                position = position_nudge(x = -0.32),
                width = 0.1) +
  
  # Colors and themes
  scale_fill_viridis(discrete = TRUE, 
                     option = "viridis",
                     labels = function(x) ifelse(x == "literature", 
                                                 "Daleman et al., 2012", 
                                                 x)) +   # Changed legend label
  scale_color_viridis(discrete = TRUE,
                      option = "viridis",
                      labels = function(x) ifelse(x == "literature",
                                                  "Daleman et al., 2012",
                                                  x)) +  # Changed legend label
  ylab(expression(CH[4]~"emission"~"["*kg~h^{-1}*"]"))+
  ggtitle('Quantified emissions from wastewater treatment plants')+
  
  theme_bw()+
  theme(legend.position = c(0.25, 0.75), legend.title = element_blank(),
        legend.text = element_text(), legend.margin = margin(0,0,0,0),
        legend.spacing = unit(0,'cm'), #legend.key.size = unit(2,'mm'),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = NA, colour = NA),
        #legend.margin = margin(t = -8, r = 0, b = 0, l = 0, unit = "pt"),
        axis.title.x = element_blank(),
        axis.text = element_text(),
        plot.tag = element_text(), plot.title = element_text(hjust = 0.5),
        plot.margin = margin(t = 5, r = 5, b = 1, l = 5, unit = "pt"))





tiff("D:\\1 PhD Studies\\3.5 Publications\\Manuscript#3_Rotterdam_emission_estimates\\Figures\\main_body\\fig.6.WWTP.tiff", units="mm", width=400, height=100, res=300)
grid.arrange(p.n2o+labs(tag = "(a)"), p.ch4+labs(tag = "(b)"), p+labs(tag = "(c)"), ncol = 3)
while (!is.null(dev.list()))  dev.off()



