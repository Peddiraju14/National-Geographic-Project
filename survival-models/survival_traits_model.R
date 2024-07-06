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

setwd("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/NG_Temp_Analyse/Survival and traits")

Data<- read.csv("survival-models/traits_survival_dat.csv")

# base model
surv_sp <- glmmTMB(Survival ~ effect * treatment  + (effect * treatment|sp), 
                     family = binomial(link = "logit"),
                   control = glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")), 
                     data = Data)
summary(surv_sp)
coef(surv_sp)

#Models
model_PC1 <- glmmTMB(Survival ~ PC1 * effect * treatment  + (1|sp), 
                     family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                    optArgs=list(method="BFGS")), 
                     data = Data)
summary(model_PC1)

model_PC2 <- glmmTMB(Survival ~ PC2 * effect * treatment  + (1|sp), 
                     family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                    optArgs=list(method="BFGS")), 
                     data = Data)
model_LMA <- glmmTMB(Survival ~ LMA * effect * treatment  + (1|sp), 
                     family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                                optArgs=list(method="BFGS")), 
                     data = Data)
model_LDMC <- glmmTMB(Survival ~ LDMC * effect * treatment  + (1|sp), 
                     family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                                optArgs=list(method="BFGS")), 
                     data = Data)
summary(model_LDMC)

model_LA <- glmmTMB(Survival ~ Leaf_Area * effect * treatment  + (1|sp), 
                     family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                                optArgs=list(method="BFGS")), 
                     data = Data)
model_SSD <- glmmTMB(Survival ~ SSD * effect * treatment  + (1|sp), 
                     family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                                optArgs=list(method="BFGS")), 
                     data = Data)
model_MRSD <- glmmTMB(Survival ~ MRSD * effect * treatment  + (1|sp), 
                     family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                                optArgs=list(method="BFGS")), 
                     data = Data)
model_FRSD <- glmmTMB(Survival ~ FRSD * effect * treatment  + (1|sp), 
                     family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                                optArgs=list(method="BFGS")), 
                     data = Data)
#Visualization
a<-plot_model(model_PC1, type = "pred", 
              terms = c("PC1", "treatment", "effect"), show.data = TRUE) +
  ggtitle("(a)") + 
  xlab("PC1") + 
  ylab(NULL) +
  geom_smooth(se = FALSE, linetype = 1, size = 1) +
  scale_colour_manual(values = c("#56B4E9", "#D55E00"), 
                      labels = c("Control", "Drought"), name = "Treatment") +
  scale_fill_manual(values = c("#56B4E9", "#D55E00"), 
                    labels = c("Control", "Drought"), name = "Treatment") +
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
  geom_smooth(se = FALSE, linetype = 1, size = 1) +
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



ggsave(traits_survival, filename = "traits_survival.jpeg", height = 14, width = 10, dpi = 600)

#MOdel outputs

