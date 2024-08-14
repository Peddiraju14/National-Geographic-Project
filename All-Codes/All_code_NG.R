# contains packages until line 33
# raw trait data was used to for PCA and add PC1 and PC2 to traits table for further use from line 35 to 60
# raw survival data was modified to ratio and traits were merge from line 66 to 96 
# raw biomass was loaded and merged with traits and all above main data sets were arranged from lines 101 to 120
# Biomass models were ran and visualization of output from lines 125 to 285
# Traits PCA analysis and visualization done from lines  288 to 614
# Drought response models and visualization of output was done from lines 317 to 614
# Survival models and visualization was done form lines 619 894


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

############################################################ traits data

setwd("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/NG_Temp_Analyse/trait space")

Traits_Dat <- read_excel("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/NG_Temp_Analyse/trait space/Traits_Dat.xls")


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



############################################################ survival data

setwd("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/NG_Temp_Analyse/Survival and traits")

# raw data of survival
Data <- read_excel("survival_dat_raw.xls")

# merging traits to raw survival data
trait_survival_data <- Data %>% 
  left_join(Traits_Dat_with_PC, by = "sp")


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

########################################################### Biomass and traits merging

biomass_raw <- read_excel("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/NG_Temp_Analyse/biomass/biomass_raw.xlsx")

#merging biomass data with traits
traits_biomass_data<- biomass_raw %>% 
  left_join(Traits_Dat_with_PC, by="sp")


# Keeping main data sets together for model fitting

# merging traits to raw survival data
trait_survival_data <- Data %>% 
  left_join(Traits_Dat_with_PC, by = "sp")

# Join traits data (with PC1 and PC2) to the drought response data
traits_drought_response_data <- drought_response_data %>% 
  left_join(Traits_Dat_with_PC, by = "sp")

#merging biomass data with traits
traits_biomass_data<- biomass_raw %>% 
  left_join(Traits_Dat_with_PC, by="sp")


################################################### Biomass models

#merging biomass data with traits
traits_biomass_data<- biomass_raw %>% 
  left_join(Traits_Dat_with_PC, by="sp")

# filling empty data to zero
traits_biomass_data<- traits_biomass_data %>% replace(is.na(.), 0)


###          total biomass model

total_biomass_model<-lme(total~treatment*effect, random = ~ 1 | sites, 
    data=traits_biomass_data)


# Summarize the model
summary(total_biomass_model)

lsmeans(total_biomass_model, pairwise ~ effect|treatment) 
lsmeans(total_biomass_model, pairwise ~ treatment|effect) 
# total biomass was higher in control of interior than control of Edge 
# total biomass was not different in drought of interior to Edge 
#No difference in biomass between treatments of either of forest habitats

tab_model(total_biomass_model,show.aic = T,show.re.var = F)


###  above biomass model

above_biomass_model<-lme(above~treatment*effect, random = ~ 1 | sites, 
                        data=traits_biomass_data)


# Summarize the model
summary(above_biomass_model)

lsmeans(above_biomass_model, pairwise ~ effect|treatment)

tab_model(above_biomass_model,show.aic = T,show.re.var = F)

###  stem biomass model

stem_biomass_model<-lme(stemb~treatment*effect, random = ~ 1 | sites, 
                        data=traits_biomass_data)


# Summarize the model
summary(stem_biomass_model)

lsmeans(stem_biomass_model, pairwise ~ effect|treatment)
# stem biomass was higher in control of interior than control of Edge 
# stem biomass was not different in drought of interior to Edge 

tab_model(stem_biomass_model,show.aic = T,show.re.var = F)

###  leaf biomass model

leaf_biomass_model<-lme(leaf~treatment*effect, random = ~ 1 | sites, 
                        data=traits_biomass_data)


# Summarize the model
summary(leaf_biomass_model)

lsmeans(leaf_biomass_model, pairwise ~ effect|treatment)

tab_model(leaf_biomass_model,show.aic = T,show.re.var = F)

###  root biomass model

root_biomass_model<-lme(root~treatment*effect, random = ~ 1 | sites, 
                        data=traits_biomass_data)


# Summarize the model
summary(root_biomass_model)

lsmeans(root_biomass_model, pairwise ~ effect|treatment)
# root biomass was higher in control of interior than control of Edge 
# root biomass was not different in drought of interior to Edge 

tab_model(root_biomass_model,show.aic = T,show.re.var = F)

#all models tab_model
tab_model(total_biomass_model, above_biomass_model, stem_biomass_model, leaf_biomass_model,
          root_biomass_model,show.aic = T,show.re.var = F)


############################## graphs for biomass

# Load necessary packages
library(nlme)
library(emmeans)
library(ggplot2)
library(sjPlot)

# Fit the model
total_biomass_model <- lme(total ~ treatment * effect, random = ~ 1 | sites, data = traits_biomass_data)

above_biomass_model <- lme(above ~ treatment * effect, random = ~ 1 | sites, data = traits_biomass_data)
leaf_biomass_model <- lme(leaf ~ treatment * effect, random = ~ 1 | sites, data = traits_biomass_data)
stem_biomass_model <- lme(stemb ~ treatment * effect, random = ~ 1 | sites, data = traits_biomass_data)
root_biomass_model <- lme(root ~ treatment * effect, random = ~ 1 | sites, data = traits_biomass_data)


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

ggsave(all_biomass, filename = "all_biomass.jpeg", height = 6, width = 12, dpi = 600)



# Display model with AIC and without random effects variance
tab_model(total_biomass_model, show.aic = TRUE, show.re.var = FALSE)


################################# visualization plot of trait space
dat <- na.omit(Traits_Dat[, c(2:7)]) # making table of traits for PCA
sp_data<-Traits_Dat%>% select(sp) # taking species in different object to merge later
#########
pca_result <- prcomp(dat, scale = TRUE)

pca_var<-get_pca_var(pca_result)[["coord"]] #### loadings of varibales

pca_coord<- as.data.frame(pca_result[["x"]]) ### co-ordinates of the rows

# PCA plot of traits: pca_result object is in the beginning

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


ggsave(PCA_plot, filename = "PCA_plot.tiff",  height = 5, width = 5, dpi = 600)



################################# drought response model fitting

# Join traits data (with PC1 and PC2) to the drought response data
traits_drought_response_data <- drought_response_data %>% 
  left_join(Traits_Dat_with_PC, by = "sp")

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
#delete it
abc_delete<- theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text = element_text(size = 13),
        axis.title.x = element_text(vjust = -0.80),
        legend.position = "top",
        legend.justification = "centre")


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


setwd("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/NG_Temp_Analyse/All_codes")
ggsave(Drought_response, filename = "Drought_response.jpeg", height = 10, width = 6.5, dpi = 600)

############################### Survival models
### Models

#################################################### with PC1

model_PC1 <- glmmTMB(Survival ~ PC1 * effect * treatment  + (1|sp), 
                     family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                                optArgs=list(method="BFGS")), 
                     data = trait_survival_data) # there is significance b/w edge control to interior drought

#summary of model
summary(model_PC1) # there is significance b/w edge control to interior drought
tab_model(model_PC1,show.aic = T,show.re.var = F)

#performing tuckey test for different factorial combinations
emtrends(model_PC1, pairwise ~ effect*treatment, var = "PC1")


#################################################### with PC2

model_PC2 <- glmmTMB(Survival ~ PC2 * effect * treatment  + (1|sp), 
                     family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                                optArgs=list(method="BFGS")), 
                     data = trait_survival_data)

#summary of model
summary(model_PC2) # there is marginal significance b/w edge control to interior drought
tab_model(model_PC2,show.aic = T,show.re.var = F)

#performing tuckey test for different factorial combinations
emtrends(model_PC2, pairwise ~ effect*treatment, var = "PC2")

#################################################### with LMA

model_LMA <- glmmTMB(Survival ~ LMA * effect * treatment  + (1|sp), 
                     family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                                optArgs=list(method="BFGS")), 
                     data = trait_survival_data)

#summary of model
summary(model_LMA) # there is marginal significance b/w edge control to interior drought
tab_model(model_LMA,show.aic = T,show.re.var = F)

#performing tuckey test for different factorial combinations
emtrends(model_LMA, pairwise ~ effect*treatment, var = "LMA")


#################################################### with LDMC

model_LDMC <- glmmTMB(Survival ~ LDMC * effect * treatment  + (1|sp), 
                      family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                                 optArgs=list(method="BFGS")), 
                      data = trait_survival_data)

#summary of model
summary(model_LDMC) # there is marginal significance b/w edge control to interior drought
tab_model(model_LDMC,show.aic = T,show.re.var = F)

#performing tuckey test for different factorial combinations
emtrends(model_LDMC, pairwise ~ effect*treatment, var = "LDMC")

#################################################### with Leaf_area

model_LA <- glmmTMB(Survival ~ Leaf_Area * effect * treatment  + (1|sp), 
                    family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                               optArgs=list(method="BFGS")), 
                    data = trait_survival_data)

#summary of model
summary(model_LA) # there is marginal significance b/w edge control to interior drought
tab_model(model_LMA,show.aic = T,show.re.var = F)

#performing tuckey test for different factorial combinations
emtrends(model_LA, pairwise ~ effect*treatment, var = "Leaf_Area")

#################################################### with SSD

model_SSD <- glmmTMB(Survival ~ SSD * effect * treatment  + (1|sp), 
                     family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                                optArgs=list(method="BFGS")), 
                     data = trait_survival_data)

#summary of model
summary(model_SSD) # there is marginal significance b/w edge control to interior drought
tab_model(model_SSD,show.aic = T,show.re.var = F)


#performing tuckey test for different factorial combinations
emtrends(model_SSD, pairwise ~ effect*treatment, var = "SSD")

#################################################### with MRSD

model_MRSD <- glmmTMB(Survival ~ MRSD * effect * treatment  + (1|sp), 
                      family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                                 optArgs=list(method="BFGS")), 
                      data = trait_survival_data)

#summary of model
summary(model_MRSD) # there is no significance b/w edge control to interior drought
tab_model(model_LMA,show.aic = T,show.re.var = F)


#performing tuckey test for different factorial combinations
emtrends(model_MRSD, pairwise ~ effect*treatment, var = "MRSD")

#################################################### with FRSD

model_FRSD <- glmmTMB(Survival ~ FRSD * effect * treatment  + (1|sp), 
                      family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                                 optArgs=list(method="BFGS")), 
                      data = trait_survival_data)

#summary of model
summary(model_FRSD) # there is significance b/w edge control to interior drought
tab_model(model_FRSD,show.aic = T,show.re.var = F)


#performing tuckey test for different factorial combinations
emtrends(model_FRSD, pairwise ~ effect*treatment, var = "FRSD")



#Visualization
a<-plot_model(model_PC1, type = "pred", terms = c("PC1", "treatment", "effect"), show.data = TRUE) +
  ggtitle("(a)") + 
  xlab("PC1") + 
  ylab(NULL) +
  geom_smooth(se = FALSE, linetype = 1, size = 2) +
  scale_colour_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  scale_fill_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  theme_classic() +  
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )

b<-plot_model(model_PC2, type = "pred", terms = c("PC2", "treatment", "effect"), show.data = TRUE) +
  ggtitle("(b)") + 
  xlab("PC2") + 
  ylab(NULL) +
  geom_smooth(se = FALSE, linetype = 1, size = 2) +
  scale_colour_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  scale_fill_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  theme_classic() +  
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )

c<-plot_model(model_LMA, type = "pred", terms = c("LMA", "treatment", "effect"), show.data = TRUE) +
  ggtitle("(c)") + 
  xlab("LMA") + 
  ylab(NULL) +
  geom_smooth(se = FALSE, linetype = 1, size = 2) +
  scale_colour_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  scale_fill_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  theme_classic() +  
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )
d<-plot_model(model_LDMC, type = "pred", terms = c("LDMC", "treatment", "effect"), show.data = TRUE) +
  ggtitle("(d)") + 
  xlab("LDMC") + 
  ylab(NULL) +
  geom_smooth(se = FALSE, linetype = 1, size = 2) +
  scale_colour_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  scale_fill_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  theme_classic() +  
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )
e<-plot_model(model_LA, type = "pred", terms = c("Leaf_Area", "treatment", "effect"), show.data = TRUE) +
  ggtitle("(e)") + 
  xlab("Leaf_Area") + 
  ylab(NULL) +
  geom_smooth(se = FALSE, linetype = 1, size = 2) +
  scale_colour_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  scale_fill_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  theme_classic() +  
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )
f<-plot_model(model_SSD, type = "pred", terms = c("SSD", "treatment","effect"), show.data = TRUE) +
  ggtitle("(f)") + 
  xlab("SSD") + 
  ylab(NULL) +
  geom_smooth(se = FALSE, linetype = 1, size = 2) +
  scale_colour_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  scale_fill_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  theme_classic() +  
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )
g<-plot_model(model_MRSD, type = "pred", terms = c("MRSD", "treatment", "effect"), show.data = TRUE) +
  ggtitle("(g)") + 
  xlab("MRSD") + 
  ylab(NULL) +
  geom_smooth(se = FALSE, linetype = 1, size = 2) +
  scale_colour_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  scale_fill_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  theme_classic() +  
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )
h<-plot_model(model_FRSD, type = "pred", terms = c("FRSD", "treatment", "effect"), show.data = TRUE) +
  ggtitle("(h)") + 
  xlab("FRSD") + 
  ylab(NULL) +
  geom_smooth(se = FALSE, linetype = 1, size = 2) +
  scale_colour_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  scale_fill_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  theme_classic() +  
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )

traits_survival<- ggarrange(a,b,c,d,e,f,g,h,
                            common.legend = T,
                            #labels = paste(letters[1:8], ".", sep=""),
                            font.label = list(size=15),
                            vjust = 0.5, ncol = 2, nrow = 4)


traits_survival<-annotate_figure(traits_survival,
                                 #top = text_grob("Visualizing len", color = "red", face = "bold", size = 14),
                                 left = text_grob("Survival Probability", color = "black", rot = 90,size = 15),
                                 #right = "I'm done, thanks :-)!",
                                 #fig.lab = "Figure 1", fig.lab.face = "bold"
)



ggsave(traits_survival, filename = "traits_survival.svg", height = 14, width = 8, dpi = 600)
