library(FactoMineR)
library(factoextra)

setwd("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/NG_Temp_Analyse/trait space")

Traits_Dat <- read_excel("C:/Users/Peddiraju Bandaru/Desktop/LaCONES/NG_Temp_Analyse/trait space/Traits_Dat.xls")

dat <- na.omit(Traits_Dat[, c(2:7)])
#########
pca_result <- prcomp(dat, scale = TRUE)


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

################################### PCA data extraction
pca_var<-get_pca_var(pca_result)[["coord"]] #### loadings of varibales

write.csv(pca_var, file="PCA_trait loadings.csv", row.names = TRUE)

pca_coord<- as.data.frame(pca_result[["x"]]) ### co-ordinates of the rows

write.csv(pca_coord, file="row coordinates.csv", row.names = TRUE)
