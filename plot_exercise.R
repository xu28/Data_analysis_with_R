library(ggplot2)
library(plyr)
setwd("~/Documents/Rdata")
x_all <- read.csv("data.csv")
# Extract the two columns of data that I need to analyze.
x <- x_all[,2:3]
# Calculate the average value separately. (Shown by black dashed line)
x_2 <- ddply(x,"Blood_Pressure_Abnormality",summarise,Level_of_Hemoglobin.mean=mean(Level_of_Hemoglobin))
# Displays hemoglobin levels in normal and abnormal blood pressure individuals separately.
ggplot(x,aes(x=Level_of_Hemoglobin))+
  geom_histogram(aes(y=..density..),binwidth = 0.5,colour="#DCDCDC",fill="#FFDAB9")+
  facet_grid(Blood_Pressure_Abnormality ~ ., labeller = label_both)+
  geom_density(alpha=0.2, colour="#8B7500")+
  geom_vline(colour="black",data=x_2,aes(xintercept=Level_of_Hemoglobin.mean),linetype="dashed",size=1)

# From this figure, we can see that people with abnormal blood pressure have higher hemoglobin levels on average 
# And their hemoglobin level shows a more spread out distribution.