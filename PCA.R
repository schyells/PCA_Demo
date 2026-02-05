#####PCA Analysis

#Download PCA R Script 
url.show("https://tinyurl.com/Ellsworth-Demo")

#Install and Load Necessary Software 
library(stats)
install.packages("tidyverse")
library(tidyverse)
install.packages("ggpot2")
library(ggbiplot)
install.packages("palmerpenguins")
library(palmerpenguins)


data("penguins") #Load data set penguins
head(penguins) #Look at the first few rows of the raw data set

dat.clean <- na.omit(penguins) # Remove missing values from the data set

head(dat.clean) #Look at the first few rows of the clean dataset

#Remove categorical variables from dataset
dat.PCA <- dat.clean %>% select(-species,-island,-sex,-year)

# Scale the dataset using a built-in function
dat.scale <- scale(dat.PCA)

head(dat.scale) #Look at the first few rows of the scaled data

PCA <- princomp(dat.scale) #Run the Principal Analysis Component

#Summary recognizes that this is a PCA and will give us the PC and we can 
#request to see the loadings
summary(PCA,loadings = T) 


biplot(PCA) #Raw generic plot

# Build data frame based on the scores and add the categorical variables
PCA_scores <- data.frame(PCA$scores,
                         Species = dat.clean$species,
                         Sex = dat.clean$sex)

# Build data frame of the loadings for each principal component
PCA_loadings <- data.frame(PCA$loadings[])


#Use ggplot to make a better version of our biplot
plot <- ggplot(data = PCA_scores,
  mapping=aes(x=Comp.1, y=Comp.2))+
  geom_point()+
  theme_classic()

plot #visualize the plot


#Add color and shapes to help visualize our categorical data
plot <- ggplot(data = PCA_scores,
  mapping=aes(x=Comp.1, y=Comp.2, col=Species))+
  geom_point(aes(shape=Sex))+
  theme_classic()

plot #visualize the plot

#Add X and Y Labels and the Eigenvalues
plot <- plot + xlab("Principal Component 1 (68.6%)") + 	ylab("Principal Component 2 (19.5%)")

plot #visualize the plot


# Add the loadings as arrows to the plot
plot <- plot+geom_segment(
  data = PCA_loadings*3,
  aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
  color="black",
  arrow = arrow(length = unit(4, "mm")))

plot #visualize the plot

#Add labels to end of each arrow displaying the variables
plot <- plot + geom_text(data = PCA_loadings*3,
  color="black",
  vjust=-1,
  aes(x = Comp.1, y = Comp.2,
  label=rownames(PCA_loadings)))

plot #visualize the plot

#Perform a permanova on the first principal component to see if species are significantly different 
adonis2(PCA_scores$Comp.1 ~ Species,
        data = PCA_scores,
        method = "euc")


#Create a box plot displaying the first principal component separated by species
ggplot(data = PCA_scores, aes(x = Species, y = Comp.1)) +
  geom_boxplot() +
  geom_jitter(aes(colour = Sex), width = 0.3, height = 0) +
  theme_classic()
