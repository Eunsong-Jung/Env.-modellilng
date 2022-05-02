install.packages("tidyverse")  # data manipulation
install.packages("cluster")    # clustering algorithms
install.packages("factoextra") # clustering visualization
install.packages("dendextend")
install.packages("doBy")

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend)
library(readxl)
library(dplyr)
library(doBy)


df <- read_excel("month_station.xlsx",sheet = "sea")
df$date<-as.character(as.POSIXct(df$date, format = "%y%m%d %H:%M"), format = "%m-%d-%y %H:%M")
info<-data.frame(df$site,df$date)
df2 <- df[,-c(1,2)]
df2<-scale(df2)

d <- dist(df2, method = "euclidean")
hc1 <- hclust(d, method = "ward.D" )
summary(hc1)
plot(hc1, cex = 0.6, hang = -1)
cluster<-cutree(hc1,k=8)
rect.hclust(hc1,k=8)
write.csv(cluster,"cluster_sea.csv")
