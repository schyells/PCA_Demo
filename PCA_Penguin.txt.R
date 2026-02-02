library(stats)
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("palmerpenguins")
library(palmerpenguins)

data("penguins")
head(penguins)
dat.clean<-na.omit(penguins)
head(dat.clean)

dat.PCA<-dat.clean%>%select(-species, -island, -sex,-year)

dat.scale<-scale(dat.PCA)
head(dat.scale)

PCA<-princomp(dat.scale)
summary(PCA, loadings=T, cutoff=0) #point out to write down the comp variation

biplot(PCA)

PCA_scores<-data.frame(PCA$scores, 
                       Species = dat.clean$species,
                       Island = dat.clean$island,
                       Sex = dat.clean$sex,
                       Year = as.factor(dat.clean$year))
PCA_loadings <- data.frame(PCA$loadings[])

plot<-ggplot(data = PCA_scores, mapping=aes(x=Comp.1, y=Comp.2, col=Species))+
  geom_point(aes(shape=Sex))+
  theme_classic()
plot

plot<-plot+xlab("Principal Component 1 (68.6%)")+ylab("Principal Component 2 (19.5%)")
plot

plot<-plot+geom_segment(
  data = PCA_loadings*3,
  aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
  color="black",
  arrow = arrow(length = unit(4, "mm")))
plot


plot<-plot+geom_text(data = PCA_loadings*3,
                     color="black",
                     vjust=-1,
                      aes(x = Comp.1, y = Comp.2, label=rownames(PCA_loadings)))
plot
