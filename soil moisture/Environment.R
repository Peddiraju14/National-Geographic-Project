library(readxl)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(PerformanceAnalytics)
library(openxlsx)
library(lme4)
library(nlme)
library(sjPlot)
library(nlme)
library(lsmeans)

#setwd("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/ng_final/data")
setwd("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/NG_Temp_Analyse/environment")
sm_data <- read_excel("NG Hydrosense readings long form.xlsx")
sm_data<- sm_data |> filter(!is.na(soil_moisture_percent))

#sm_model<-lme(soil_moisture_percent ~ effect*Treatment + (1|plot_id)+(1|mon), data = sm_data) 

# Fit the model using nlme
sm_model_nlme <- lme(soil_moisture_percent ~ effect * Treatment,
                     random = ~ 1 | plot_id / mon,
                     data = sm_data)

# Summarize the model
summary(sm_model_nlme)
#tuckey test
lsmeans(sm_model_nlme, pairwise ~ Treatment | effect)
lsmeans(sm_model_nlme, pairwise ~ effect|Treatment)
tab_model(sm_model_nlme,show.aic = T,show.re.var = F)





setwd("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/ng_final/data/Model outputs/environment")
write.csv(sm_model_results, file = "sm_model_results.csv", row.names = TRUE)
# Define custom levels for months
month_levels <- c( "March", "April", "May", "June", 
                   "July", "August", "September", "October", "November", "December", "January")

# Convert "mon" to a factor with custom levels
sm_data$mon <- factor(sm_data$mon, levels = month_levels)
# Define colors for treatments
treatment_colors <- c("Control" = "#56B4E9", "Drought" = "#D55E00")


sm_plot<- ggplot(sm_data, aes(x = mon, y = soil_moisture_percent, color = Treatment)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_dodge(width = 0.75)) +
  scale_color_manual(values = treatment_colors) +
  facet_wrap(~ effect, ncol = 1) +
  labs(x="Month", y="Soil Moisture (%)")+
  theme_classic() +
  theme(panel.grid = element_blank(), axis.title = element_text(size = 15)) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 20, face = "bold"),  # Bold axis titles and adjust their size
    axis.text.x = element_text(size = 18, angle = 90, vjust = 0.5),  # Adjust size and rotation of x-axis labels
    axis.text.y = element_text(size = 18),  # Adjust size of y-axis labels
    legend.position = "top",
    legend.justification = "center",
    legend.text = element_text(size = 18),  # Adjust size of legend labels
    strip.text = element_text(size = 18)  # Adjust size of faceted labels
  )

setwd("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/ng_final/plots")

ggsave(sm_plot, filename = "sm_plot.jpeg", height = 8, width = 12, dpi = 600)

###############################################################################################
###############################################################################################
# trial for other plot ytpes
# Define colors for treatments
treatment_colors <- c("control" = "deepskyblue", "drought" = "red")

# Read the data from the Excel file
n_data <- readxl::read_excel("C:/Users/Peddiraju/Desktop/ng_revised/environment/all_evs_variables.xlsx", 
                             sheet = "analyse")

summary(aov(Available_Mn~effect*treatment, data=n_data))
# Extract column names for y variables
y_variable_names <- colnames(n_data)[4:12]

# Loop through each y variable
for (y_var in y_variable_names) {
  # Create the ggplot for the current y variable
  plot <- ggplot(n_data, aes(x = effect, y = .data[[y_var]], color = treatment)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(position = position_dodge(width = 0.75)) +
    scale_color_manual(values = treatment_colors) +
    #facet_wrap(~ effect, ncol = 1) +
    theme_bw() +
    labs(title = y_var)  # Set title to y variable name
  
  # Save the plot as a PNG file
  ggsave(filename = paste0(y_var, ".png"), plot = plot, width = 6, height = 4)
}   
