library(glmmTMB)
library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)
library(readxl)
library(patchwork)
library(lme4)
library(lmerTest)
library(emmeans)
library(lmerTest)
library(sjPlot)
library(gridExtra)


setwd("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/NG_Temp_Analyse/Drought response and traits")

Dat <- read_excel("Drought_traits_Dat.xlsx")

PC1_model<- lmer(Ds~effect*PC1+(1|sp),data=Dat)
PC2_model<- lmer(Ds~effect*PC2+(1|sp),data=Dat)
LMA_model<- lmer(Ds~effect*LMA+(1|sp),data=Dat)
LDMC_model<- lmer(Ds~effect*LDMC+(1|sp),data=Dat)
LA_model<- lmer(Ds~effect*Leaf_Area+(1|sp),data=Dat)
SSD_model<- lmer(Ds~effect*SSD+(1|sp),data=Dat)
MRSD_model<- lmer(Ds~effect*MRSD+(1|sp),data=Dat)
FRSD_model<- lmer(Ds~effect*FRSD+(1|sp),data=Dat)

aa<-plot_model(PC1_model, type = "pred",terms=c("PC1","effect"), show.values = TRUE, show.data = TRUE)+ 
  xlab ("PC1")+ylab(NULL)+
  geom_smooth(se=F,linetype=1, size=2)+
  ggtitle("(a)")+
  #annotate("text", x = 1, y = 1.4, label = "*", hjust = 0.5, vjust = 0.5, size = 10)+
  #geom_ribbon(aes(ymin=predicted-std.error, ymax=predicted+std.error, fill=group , alpha=1))+
  #geom_smooth(se=F, size = 0.6)+
  scale_colour_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_fill_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  theme_classic() +  # Adding theme_classic()
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre"
  )

bb<-plot_model(PC2_model, type = "pred",terms=c("PC2","effect"), show.values = TRUE, show.data = TRUE)+ 
  xlab ("PC2")+ylab(NULL)+
  geom_smooth(se=F,linetype=0, size=2)+
  ggtitle("(b)")+
  #annotate("text", x = 1, y = 1.4, label = "*", hjust = 0.5, vjust = 0.5, size = 10)+
  #geom_ribbon(aes(ymin=predicted-std.error, ymax=predicted+std.error, fill=group , alpha=1))+
  #geom_smooth(se=F, size = 0.6)+
  scale_colour_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_fill_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  theme_classic() +  # Adding theme_classic()
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre"
  )
cc<-plot_model(LMA_model, type = "pred",terms=c("LMA","effect"), show.values = TRUE, show.data = TRUE)+ 
  xlab ("LMA")+ylab(NULL)+
  geom_smooth(se=F,linetype=1, size=2)+
  ggtitle("(c)")+
  #annotate("text", x = 1, y = 1.4, label = "*", hjust = 0.5, vjust = 0.5, size = 10)+
  #geom_ribbon(aes(ymin=predicted-std.error, ymax=predicted+std.error, fill=group , alpha=1))+
  #geom_smooth(se=F, size = 0.6)+
  scale_colour_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_fill_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  theme_classic() +  # Adding theme_classic()
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre"
  )
dd<-plot_model(LDMC_model, type = "pred",terms=c("LDMC","effect"), show.values = TRUE, show.data = TRUE)+ 
  xlab ("LDMC")+ylab(NULL)+
  geom_smooth(se=F,linetype=1, size=2)+
  ggtitle("(d)")+
  #annotate("text", x = 1, y = 1.4, label = "*", hjust = 0.5, vjust = 0.5, size = 10)+
  #geom_ribbon(aes(ymin=predicted-std.error, ymax=predicted+std.error, fill=group , alpha=1))+
  #geom_smooth(se=F, size = 0.6)+
  scale_colour_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_fill_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  theme_classic() +  # Adding theme_classic()
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre"
  )
ee<-plot_model(LA_model, type = "pred",terms=c("Leaf_Area","effect"), show.values = TRUE, show.data = TRUE)+ 
  xlab ("Leaf Area")+ylab(NULL)+
  geom_smooth(se=F,linetype=1, size=2)+
  ggtitle("(e)")+
  #annotate("text", x = 1, y = 1.4, label = "*", hjust = 0.5, vjust = 0.5, size = 10)+
  #geom_ribbon(aes(ymin=predicted-std.error, ymax=predicted+std.error, fill=group , alpha=1))+
  #geom_smooth(se=F, size = 0.6)+
  scale_colour_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_fill_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  theme_classic() +  # Adding theme_classic()
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre"
  )
ff<-plot_model(SSD_model, type = "pred",terms=c("SSD","effect"), show.values = TRUE, show.data = TRUE)+ 
  xlab ("SSD")+ylab(NULL)+
  geom_smooth(se=F,linetype=1, size=2)+
  ggtitle("(f)")+
  #annotate("text", x = 1, y = 1.4, label = "*", hjust = 0.5, vjust = 0.5, size = 10)+
  #geom_ribbon(aes(ymin=predicted-std.error, ymax=predicted+std.error, fill=group , alpha=1))+
  #geom_smooth(se=F, size = 0.6)+
  scale_colour_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_fill_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  theme_classic() +  # Adding theme_classic()
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre"
  )
gg<-plot_model(MRSD_model, type = "pred",terms=c("MRSD","effect"), show.values = TRUE, show.data = TRUE)+ 
  xlab ("MRSD")+ylab(NULL)+
  geom_smooth(se=F,linetype=1, size=2)+
  ggtitle("(g)")+
  #annotate("text", x = 1, y = 1.4, label = "*", hjust = 0.5, vjust = 0.5, size = 10)+
  #geom_ribbon(aes(ymin=predicted-std.error, ymax=predicted+std.error, fill=group , alpha=1))+
  #geom_smooth(se=F, size = 0.6)+
  scale_colour_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_fill_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  theme_classic() +  # Adding theme_classic()
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre"
  )
hh<-plot_model(FRSD_model, type = "pred",terms=c("FRSD","effect"), show.values = TRUE, show.data = TRUE)+ 
  xlab ("FRSD")+ylab(NULL)+
  geom_smooth(se=F,linetype=1, size=2)+
  ggtitle("(h)")+
  #annotate("text", x = 1, y = 1.4, label = "*", hjust = 0.5, vjust = 0.5, size = 10)+
  #geom_ribbon(aes(ymin=predicted-std.error, ymax=predicted+std.error, fill=group , alpha=1))+
  #geom_smooth(se=F, size = 0.6)+
  scale_colour_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_fill_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  theme_classic() +  # Adding theme_classic()
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre"
  )

Drought_response<- ggarrange(aa,bb,cc,dd,ee,ff,gg,hh,
                            common.legend = T,
                            #labels = paste(letters[1:8], ".", sep=""),
                            font.label = list(size=15),
                            vjust = 0.5, ncol = 2, nrow = 4)


Drought_response<-annotate_figure(Drought_response,
                                 #top = text_grob("Visualizing len", color = "red", face = "bold", size = 14),
                                 left = text_grob("Drought Response (no. survived (Drought/Control))", color = "black", rot = 90,size = 15),
                                 #right = "I'm done, thanks :-)!",
                                 #fig.lab = "Figure 1", fig.lab.face = "bold"
)



ggsave(Drought_response, filename = "Drought_response.jpeg", height = 14, width = 10, dpi = 600)


