#all questions

#1.	Does rainfall exclusion decrease soil moisture availability by a greater extent at forest edges than interiors?
#2.	Does drought affect plant performance (survival and biomass accumulation) differentially at the forest edge vs. interior?
#3.	Do plant traits mediate drought response and how does this vary between forest edge and interior?
  

# contains packages from line 16 to 46
# question-1; soil moisture model, output and plots from line 49 to 107
# question-2a; survival model, output and plots from line 110 to 162
# question-2b; biomass model, output and plots from line 165 to 293
# question-3a; trait space, output and plots from line 297 to 339
# question-3b; drought response, output and plots from line 342 to 656


library(tidyverse)
library(broom)
library(patchwork)
library(dplyr)
library(data.table)
#FOR PLOTS#
library(ggplot2)
library(ggeffects)
library(ggplotify)
library(wesanderson)
library(ggpubr)
library(grid)
library(effects)
library(readxl)
library(FactoMineR)
library(factoextra)
#FOR STATS#
library(lme4)
library(nlme)
library(lmerTest)
library(r2glmm)
library(sjPlot)
library(MuMIn)
library(gridExtra)
library(sjstats)
library(car)
library(scales)
library(ggeffects)
library(glmmTMB)
library(emmeans)
library(jtools)


###################################### Question-1
#1.	Does rainfall exclusion decrease soil moisture availability by a greater extent at forest edges than interiors?


#soil moisture model, output and plot

setwd("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/NG_Temp_Analyse/All_codes/raw_data")
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


#directory for output plots
setwd("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/NG_Temp_Analyse/All_codes/output_all_codes")

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


ggsave(sm_plot, filename = "sm_plot.jpeg", height = 8, width = 12, dpi = 600)


################################################# Question-2
#2.	Does drought affect plant performance (survival and biomass accumulation) differentially at the forest edge vs. interior?


#survival model, output and plot

setwd("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/NG_Temp_Analyse/All_codes/raw_data")
survival_dat <- read_excel("survival_dat_raw.xls")

# species as random intercepts
model_survival <- glmmTMB(Survival ~ effect * treatment  + (1|sp), 
                          family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                                     optArgs=list(method="BFGS")), 
                          data = survival_dat)

# species as random slopes
model_survival1 <- glmmTMB(Survival ~ effect * treatment  + (effect + treatment | sp), 
                           family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                                      optArgs=list(method="BFGS")), 
                           data = trait_survival_data)
# tuckey test
lsmeans(model_survival, pairwise ~ effect|treatment) 

#summary of model
summary(model_survival)
tab_model(model_survival,show.aic = T,show.re.var = F)


#visualization
survival_plot<-plot_model(model_survival, type = "pred", terms = c("effect", "treatment"), show.data = TRUE) +
  ggtitle(NULL) + 
  xlab("Forest habitat") + 
  ylab("Predicted Survival") +
  geom_smooth(se = FALSE, linetype = "solid", size = 1.5) +
  scale_colour_manual(values = c("#56B4E9", "#D95F02"), labels = c("Control", "Drought"), name = "Treatment") +
  scale_fill_manual(values = c("#56B4E9", "#D95F02"), labels = c("Control", "Drought"), name = "Treatment") +
  theme_classic() +
  theme(
    #panel.grid.major = element_line(color = "gray85", size = 0.5),
    #panel.grid.minor = element_line(color = "gray90", size = 0.25),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 16),
    legend.position = "top",
    legend.justification = "center",
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 15, face = "bold"),
    strip.text.x = element_text(size = 15, face = "bold")
  )
# directory for output files
setwd("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/NG_Temp_Analyse/All_codes/output_all_codes")
ggsave(survival_plot, filename = "survival_plot.tiff",  height = 5, width = 5, dpi = 600)


# Biomass models, output and plots

setwd("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/NG_Temp_Analyse/All_codes/raw_data")
biomass_raw <- read_excel("biomass_raw.xlsx")
# filling empty data to zero
biomass_raw<- biomass_raw %>% replace(is.na(.), 0)

#total biomass model
total_biomass_model<-lme(total~treatment*effect, random = ~ 1 | sites, 
                         data=biomass_raw)
# Summarize the model
summary(total_biomass_model)

lsmeans(total_biomass_model, pairwise ~ effect|treatment) 
lsmeans(total_biomass_model, pairwise ~ treatment|effect) 

tab_model(total_biomass_model,show.aic = T,show.re.var = F)


#above biomass model
above_biomass_model<-lme(above~treatment*effect, random = ~ 1 | sites, 
                         data=biomass_raw)
# Summarize the model
summary(above_biomass_model)

lsmeans(above_biomass_model, pairwise ~ effect|treatment)
lsmeans(above_biomass_model, pairwise ~ treatment|effect)

tab_model(above_biomass_model,show.aic = T,show.re.var = F)


#stem biomass model
stem_biomass_model<-lme(stemb~treatment*effect, random = ~ 1 | sites, 
                        data=biomass_raw)
# Summarize the model
summary(stem_biomass_model)

lsmeans(stem_biomass_model, pairwise ~ effect|treatment)
lsmeans(stem_biomass_model, pairwise ~ treatment|effect)
 
tab_model(stem_biomass_model,show.aic = T,show.re.var = F)

#leaf biomass model
leaf_biomass_model<-lme(leaf~treatment*effect, random = ~ 1 | sites, 
                        data=biomass_raw)
# Summarize the model
summary(leaf_biomass_model)

lsmeans(leaf_biomass_model, pairwise ~ effect|treatment)
lsmeans(leaf_biomass_model, pairwise ~ treatment|effect)

tab_model(leaf_biomass_model,show.aic = T,show.re.var = F)

#root biomass model
root_biomass_model<-lme(root~treatment*effect, random = ~ 1 | sites, 
                        data=biomass_raw)
# Summarize the model
summary(root_biomass_model)

lsmeans(root_biomass_model, pairwise ~ effect|treatment)
lsmeans(root_biomass_model, pairwise ~ treatment|effect)

tab_model(root_biomass_model,show.aic = T,show.re.var = F)

#all models tab_model
tab_model(total_biomass_model, above_biomass_model, stem_biomass_model, leaf_biomass_model,
          root_biomass_model,show.aic = T,show.re.var = F)

############################## graphs for biomass

# Fit the model
total_biomass_model <- lme(total ~ treatment * effect, random = ~ 1 | sites, data = biomass_raw)
above_biomass_model <- lme(above ~ treatment * effect, random = ~ 1 | sites, data = biomass_raw)
leaf_biomass_model <- lme(leaf ~ treatment * effect, random = ~ 1 | sites, data = biomass_raw)
stem_biomass_model <- lme(stemb ~ treatment * effect, random = ~ 1 | sites, data = biomass_raw)
root_biomass_model <- lme(root ~ treatment * effect, random = ~ 1 | sites, data = biomass_raw)


# Get the LSMeans
emm_total_biomass <- emmeans(total_biomass_model, ~ effect | treatment)
emm_above_biomass <- emmeans(above_biomass_model, ~ effect | treatment)
emm_stem_biomass <- emmeans(stem_biomass_model, ~ effect | treatment)
emm_leaf_biomass <- emmeans(leaf_biomass_model, ~ effect | treatment)
emm_root_biomass <- emmeans(root_biomass_model, ~ effect | treatment)

# Convert LSMeans to data frames and add a biomass type column
emm_total_df <- as.data.frame(emm_total_biomass) %>% mutate(biomass_type = "Total")
emm_above_df <- as.data.frame(emm_above_biomass) %>% mutate(biomass_type = "Above ground")
emm_stem_df <- as.data.frame(emm_stem_biomass) %>% mutate(biomass_type = "Stem")
emm_leaf_df <- as.data.frame(emm_leaf_biomass) %>% mutate(biomass_type = "Leaf")
emm_root_df <- as.data.frame(emm_root_biomass) %>% mutate(biomass_type = "Root")

# Combine all data frames into one
emm_df <- bind_rows(emm_total_df, emm_above_df, emm_stem_df, emm_leaf_df, emm_root_df)

# Set the order of the biomass_type factor levels
emm_df$biomass_type <- factor(emm_df$biomass_type, levels = c("Total", 
                                                              "Above ground", 
                                                              "Stem", 
                                                              "Leaf", 
                                                              "Root"))

# Plot the LSMeans
all_biomass<-ggplot(emm_df, aes(x = effect, y = emmean, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ biomass_type, nrow = 1) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.2, 
                position = position_dodge(0.9)) +
  scale_fill_manual(values = c("Control" = "#56B4E9", "Drought" = "#D55E00")) +
  labs(title = ,
       x = "Forest habitat",
       y = "Biomass (gms)") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
    axis.title.x = element_text(vjust = -0.75),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "center",
    strip.text.x = element_text(size = 15, face = "bold")
  )

#direcotry for output files
setwd("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/NG_Temp_Analyse/All_codes/output_all_codes")
ggsave(all_biomass, filename = "all_biomass.jpeg", height = 6, width = 12, dpi = 600)



############################################################ Question-3
#3.	Do plant traits mediate drought response and how does this vary between forest edge and interior?


#making trait space
setwd("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/NG_Temp_Analyse/All_codes/raw_data")
dat <- read_excel("Traits_Dat.xls")

dat <- na.omit(Traits_Dat[, c(2:7)]) # making table of traits for PCA
sp_data<-Traits_Dat%>% select(sp) # taking species in different object to merge later
#########
pca_result <- prcomp(dat, scale = TRUE)

pca_var<-get_pca_var(pca_result)[["coord"]] #### loadings of varibales

pca_coord<- as.data.frame(pca_result[["x"]]) ### co-ordinates of the rows

# Combine species data with PCA coordinates
combined_data <- bind_cols(sp_data, pca_coord)

# Select PC1 and PC2 columns
pc1_pc2 <- combined_data %>% select(sp, PC1, PC2)

# Merge PC1 and PC2 columns back into the original Traits_Dat table
Traits_Dat_with_PC <- Traits_Dat %>% left_join(pc1_pc2, by = "sp")

#visualization of PCA

PCA_plot<-fviz_pca_var(pca_result, col.var = "cos2",
                       gradient.cols = c("red","lightblue" ,"deepskyblue"),labelsize=5, arrowsize=1.2,
                       repel = TRUE)+ # Avoid text overlapping)+
  labs(title = NULL)+ 
  theme_classic()+theme(text = element_text(size = 15),  # Adjust the text size as needed
                        #axis.text = element_text(size = 14),  # Adjust axis text size as needed
                        axis.title = element_text(size = 15),  # Adjust axis title size as needed
                        #axis.line = element_line(size = 1),  # Adjust line thickness as needed
                        #axis.ticks = element_line(size = 1.5),  # Adjust tick thickness as needed
                        plot.title = NULL)+
  guides(col = guide_colourbar(title = "Loadings"))

#directory for output files
setwd("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/NG_Temp_Analyse/All_codes/output_all_codes")
ggsave(PCA_plot, filename = "PCA_plot.tiff",  height = 5, width = 5, dpi = 600)


#generating drought response data as proportion of survival from drought to control treatment
          # from raw survival data and adding traits values

setwd("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/NG_Temp_Analyse/All_codes/raw_data")
Data <- read_excel("survival_dat_raw.xls")

###generating the drought response data

# Summarize the data to count the number of survivors for each species in each treatment and effect
survival_summary <- Data %>%
  group_by(sp, effect, treatment) %>%
  summarize(survivors = sum(Survival), .groups = 'drop')

# Separate the data into drought and control treatments
drought_data <- survival_summary %>% filter(treatment == "Drought")
control_data <- survival_summary %>% filter(treatment == "Control")

# Merge drought and control data to calculate the ratio
merged_data <- merge(drought_data, control_data, by = c("sp", "effect"), suffixes = c("_drought", "_control"))

# Calculate the ratio of drought survivors to control survivors
drought_response_data <- merged_data %>%
  mutate(ratio = survivors_drought / survivors_control) %>%
  select(sp, effect, ratio)

# Join traits data (with PC1 and PC2) to the drought response data
traits_drought_response_data <- drought_response_data %>% 
  left_join(Traits_Dat_with_PC, by = "sp")


#drought response model, output and plot

# Remove rows with missing data in the columns of interest
traits_drought_response_data <- traits_drought_response_data %>%
  filter(!is.na(PC1))


### with PC1
PC1_model<- lmer(ratio~effect*PC1+(1|sp),data=traits_drought_response_data)
#summary of model
summary(PC1_model) # there is significance b/w edge control to interior drought
tab_model(PC1_model,show.aic = T,show.re.var = F)

### with PC1
PC1_model2<- glmmTMB(ratio~effect*PC1+(1|sp), family = nbinom2(link = "log"), data=traits_drought_response_data)
#summary of model
summary(PC1_model2) # there is significance b/w edge control to interior drought
tab_model(PC1_model2,show.aic = T,show.re.var = F)

### with PC2
PC2_model<- lmer(ratio~effect*PC2+(1|sp),data=traits_drought_response_data)
#summary of model
summary(PC2_model) # there is significance b/w edge control to interior drought
tab_model(PC2_model,show.aic = T,show.re.var = F)

### with LMA
LMA_model<- lmer(ratio~effect*LMA+(1|sp),data=traits_drought_response_data)
#summary of model
summary(LMA_model) # there is significance b/w edge control to interior drought
tab_model(LMA_model,show.aic = T,show.re.var = F)

### with LDMC
LDMC_model<- lmer(ratio~effect*LDMC+(1|sp),data=traits_drought_response_data)
#summary of model
summary(LDMC_model) # there is significance b/w edge control to interior drought
tab_model(LDMC_model,show.aic = T,show.re.var = F)

### with Leaf_area
LA_model<- lmer(ratio~effect*Leaf_Area+(1|sp),data=traits_drought_response_data)
#summary of model
summary(LA_model) # there is significance b/w edge control to interior drought
tab_model(LA_model,show.aic = T,show.re.var = F)

### with PC2
SSD_model<- lmer(ratio~effect*SSD+(1|sp),data=traits_drought_response_data)
#summary of model
summary(SSD_model) # there is significance b/w edge control to interior drought
tab_model(SSD_model,show.aic = T,show.re.var = F)
emtrends(SSD_model, pairwise ~ effect, var = "SSD")


### with MRSD
MRSD_model<- lmer(ratio~effect*MRSD+(1|sp),data=traits_drought_response_data)
#summary of model
summary(MRSD_model) # there is significance b/w edge control to interior drought
tab_model(MRSD_model,show.aic = T,show.re.var = F)

### with MRSD
FRSD_model<- lmer(ratio~effect*FRSD+(1|sp),data=traits_drought_response_data)
#summary of model
summary(FRSD_model) # there is significance b/w edge control to interior drought
tab_model(FRSD_model,show.aic = T,show.re.var = F)


#all models tab_model
tab_model(PC1_model, PC2_model, LMA_model, LDMC_model, LA_model, SSD_model, MRSD_model,FRSD_model,
          show.aic = T,show.re.var = F)

################################## Visualizing the drought response data


# Generate predictions from the model PC1
traits_drought_response_data$predicted <- predict(PC1_model, re.form = NA)

# Create the plot
aa<-ggplot(traits_drought_response_data, aes(x = PC1, y = ratio, color = effect)) +
  geom_point(alpha = 1.3,size = 3) +
  geom_line(aes(y = predicted), size = 1) +
  ggtitle("(a)")+
  scale_colour_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_fill_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  theme_classic() +
  labs(x = "PC1",
       y= NULL) +
  #scale_x_continuous(c(-2,2), breaks = seq(-2, 2, by = 1)) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )


# Generate predictions from the model PC2
traits_drought_response_data$predicted <- predict(PC2_model, re.form = NA)

# Create the plot
bb<-ggplot(traits_drought_response_data, aes(x = PC2, y = ratio, color = effect)) +
  geom_point(alpha = 1.3, size=3) +
  ggtitle("(b)")+
  #geom_line(aes(y = predicted), size = 1) +
  scale_colour_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_fill_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_x_continuous(limits= c(-2,2), breaks = seq(-2, 2, by = 1))+
  theme_classic() +
  labs(title = ,
       x = "PC2",
       y = NULL) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )

# Generate predictions from the model LMA
traits_drought_response_data$predicted <- predict(LMA_model, re.form = NA)

# Create the plot
cc<-ggplot(traits_drought_response_data, aes(x = LMA, y = ratio, color = effect)) +
  geom_point(alpha = 1.3, size=3) +
  ggtitle("(c)")+
  geom_line(aes(y = predicted), size = 1,linetype = "dashed") +
  scale_colour_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_fill_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_x_continuous(limits= c(0.2,1), breaks = seq(0.2,1, by = 0.2))+
  theme_classic() +
  labs(title = ,
       x = "LMA",
       y = NULL) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )

# Generate predictions from the model Ldmc
traits_drought_response_data$predicted <- predict(LDMC_model, re.form = NA)

# Create the plot
dd<-ggplot(traits_drought_response_data, aes(x = LDMC, y = ratio, color = effect)) +
  geom_point(alpha = 1.3, size=3) +
  ggtitle("(d)")+
  #geom_line(aes(y = predicted), size = 1) +
  scale_colour_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_fill_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  theme_classic() +
  labs(title = ,
       x = "LDMC",
       y = NULL) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )

# Generate predictions from the model Leaf area
traits_drought_response_data$predicted <- predict(LA_model, re.form = NA)

# Create the plot
ee<-ggplot(traits_drought_response_data, aes(x = Leaf_Area, y = ratio, color = effect)) +
  geom_point(alpha = 1.3, size=3) +
  ggtitle("(e)")+
  #geom_line(aes(y = predicted), size = 1) +
  scale_colour_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_fill_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  theme_classic() +
  labs(title = ,
       x = "Leaf Area",
       y = NULL) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )

# Generate predictions from the model SSD
traits_drought_response_data$predicted <- predict(SSD_model, re.form = NA)

# Create the plot
ff<-ggplot(traits_drought_response_data, aes(x = SSD, y = ratio, color = effect)) +
  geom_point(alpha = 1.3, size=3) +
  ggtitle("(f)")+
  geom_line(aes(y = predicted), size = 1) +
  scale_colour_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_fill_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  theme_classic() +
  labs(title = ,
       x = "SSD",
       y = NULL) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )


# Generate predictions from the model MRSD
traits_drought_response_data$predicted <- predict(MRSD_model, re.form = NA)

# Create the plot
gg<-ggplot(traits_drought_response_data, aes(x = MRSD, y = ratio, color = effect)) +
  geom_point(alpha = 1.3, size=3) +
  ggtitle("(g)")+
  #geom_line(aes(y = predicted), size = 1) +
  scale_colour_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_fill_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  theme_classic() +
  labs(title = ,
       x = "MRSD",
       y = NULL) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )

# Generate predictions from the model FRSD
traits_drought_response_data$predicted <- predict(FRSD_model, re.form = NA)

# Create the plot
hh<-ggplot(traits_drought_response_data, aes(x = FRSD, y = ratio, color = effect)) +
  geom_point(alpha = 1.3, size=3) +
  ggtitle("(h)")+
  #geom_line(aes(y = predicted), size = 1) +
  scale_colour_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_fill_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  theme_classic() +
  labs(title = ,
       x = "FRSD",
       y = NULL) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )

Drought_response<- ggarrange(aa,bb,cc,dd,ee,ff,gg,hh,
                             common.legend = T,
                             #labels = paste(letters[1:8], ".", sep=""),
                             font.label = list(size=15),
                             vjust = 0.5, ncol = 2, nrow = 4)


Drought_response<-annotate_figure(Drought_response,
                                  #top = text_grob("Visualizing len", color = "red", face = "bold", size = 14),
                                  left = text_grob("Drought Response (no. survived (Drought/Control))", 
                                                   color = "black", rot = 90,size = 15),
                                  #right = "I'm done, thanks :-)!",
                                  #fig.lab = "Figure 1", fig.lab.face = "bold"
)


setwd("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/NG_Temp_Analyse/All_codes/output_all_codes")
ggsave(Drought_response, filename = "Drought_response.jpeg", height = 10, width = 6.5, dpi = 600)


