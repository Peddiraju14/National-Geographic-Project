#all questions

#1.	Does rainfall exclusion decrease soil moisture availability by a greater extent at forest edges than interiors?
#2.	Does drought affect plant biomass accumulation differentially at the forest edge vs. interior?
#3.	Do plant traits mediate survival in drought and how does this vary between forest edge and interior? #if we take survival, no need for drought response


# contains packages from line 16 to 44
# question-1; soil moisture model, output and plots from line 46 to 106
# question-2; biomass model, output and plots from line 109 to 248
# question-3a; trait space, output and plots from line 251 to 293
# question-3b; survival, drought and forest habitat output and plots from line 295 to 583

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

#Q1                  soil moisture model, output and plot

setwd("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/NG_Temp_Analyse/All_codes/raw_data")
sm_data <- read_excel("NG Hydrosense readings long form.xlsx")
sm_data<- sm_data |> filter(!is.na(soil_moisture_percent))

#sm_model<-lme(soil_moisture_percent ~ effect*Treatment + (1|plot_id)+(1|mon), data = sm_data) 

# Rescale soil_moisture_percent if necessary (assuming original scale is 0-100)
sm_data <- sm_data %>% mutate(soil_moisture_rescaled = soil_moisture_percent / 100)

# Fit the beta regression model with random slope for Treatment by month
sm_model_beta <- glmmTMB(
  soil_moisture_rescaled ~ effect * Treatment +
    (1 | plot_id) + (Treatment | mon),  # Random slope for Treatment within mon
  data = sm_data,
  family = beta_family()
)

# Model summary
summary(sm_model_beta)

#tuckey test
lsmeans(sm_model_beta, pairwise ~ Treatment | effect)
lsmeans(sm_model_beta, pairwise ~ effect|Treatment)
tab_model(sm_model_beta,show.aic = T,show.re.var = F)

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


#Q2           biomass response to drought at forest habitats model and outputs


# Biomass models, output and plots

setwd("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/NG_Temp_Analyse/All_codes/raw_data")
biomass_raw <- read_excel("biomass_raw.xlsx")
# filling empty data to zero
biomass_raw<- biomass_raw %>% replace(is.na(.), 0)

#total biomass model
total_biomass_model<-# Fit the model with lmer
  total_biomass_model <- lmer(total ~ effect * treatment + (1 + effect * treatment | sp),
                              data = biomass_raw)

# Fit the model with glmmTMB
total_biomass_model <- glmmTMB(total ~ effect * treatment + (1 + effect * treatment | sp),
                               family = gaussian(),control = glmmTMBControl(optimizer=optim, 
                                                                            optArgs=list(method="BFGS")),
                               data = biomass_raw)

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


#Q3                   traits space and them mediating response to drought at forest habitats

#Q3a

#making trait space
setwd("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/NG_Temp_Analyse/All_codes/raw_data")
dat <- read_excel("Traits_Dat.xls")
sp_data<-dat%>% select(sp) # taking species in different object to merge later
datpc <- na.omit(dat[, c(2:7)]) # making table of traits for PCA

#########
pca_result <- prcomp(datpc, scale = TRUE)

pca_var<-get_pca_var(pca_result)[["coord"]] #### loadings of varibales

pca_coord<- as.data.frame(pca_result[["x"]]) ### co-ordinates of the rows

# Combine species data with PCA coordinates
combined_data <- bind_cols(sp_data, pca_coord)

# Select PC1 and PC2 columns
pc1_pc2 <- combined_data %>% select(sp, PC1, PC2)

# Merge PC1 and PC2 columns back into the original Traits_Dat table
Traits_Dat_with_PC <- dat %>% left_join(pc1_pc2, by = "sp")

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

#Q3b
setwd("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/NG_Temp_Analyse/All_codes/raw_data")
Data <- read_excel("survival_dat_raw.xls")

# Join traits data (with PC1 and PC2) to the drought response data
traits_survival_data <- Data %>% 
  left_join(Traits_Dat_with_PC, by = "sp")

#################################################### with PC1

model_PC1 <- glmmTMB(Survival ~ PC1 * effect * treatment  + (1|sp), 
                     family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                                optArgs=list(method="BFGS")), 
                     data = traits_survival_data) # there is significance b/w edge control to interior drought

model_RE_PC1 <- glmmTMB(Survival ~ PC1 * effect * treatment  + (1+effect * treatment |sp), 
                        family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                                   optArgs=list(method="BFGS")), 
                        data = traits_survival_data) # there is significance b/w edge control to interior drought

#summary of model
summary(model_PC1) # there is significance b/w edge control to interior drought
tab_model(model_PC1,show.aic = T,show.re.var = F)

#performing tuckey test for different factorial combinations
emtrends(model_PC1, pairwise ~ effect*treatment, var = "PC1")


#################################################### with PC2

model_PC2 <- glmmTMB(Survival ~ PC2 * effect * treatment  + (1|sp), 
                     family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                                optArgs=list(method="BFGS")), 
                     data = traits_survival_data)

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


setwd("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/NG_Temp_Analyse/All_codes/output_all_codes")
ggsave(traits_survival, filename = "traits_survival.jpeg", height = 14, width = 8, dpi = 600)


