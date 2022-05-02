library(vegan)
library(ggplot2)
library("bestNormalize")
library(Hmisc)
library(metaMDS)
library("car")
c<-bestNormalize(phy$bac) ##repeat on all columns.. should find a way for application on a data frame
phy$bac<-c$x.t

phy<-edit(phy)

num_env <-env[, sapply(env, is.numeric)] 
num_env<-num_env+3
hist.data.frame(num_env)
shapiro.test(num_env)

phyt <- subset(num_phy, select = c(bac,cya,chl,tot,chla))
phye<-subset(num_phy,select=-c(bac,cya,chl,tot,chla))

lmfit <- lm(phye)
vif(lmfit) > 10
phye <- subset(phye, select = -c(St,rad,max))

rda<-rda(phyt~.,phye)
summary(rda)
screeplot(rda)

levels(phy$Depth) <- c("0","3","6")
Depth <- phy$Depth
bg <- c("#ff7f00","#1f78b4","#ffff33","#a6cee3","#33a02c","#e31a1c") # 6 nice colors for our ecotypes

plot(rda, type="n", scaling=3)
points(rda, display="species", pch=20, cex=0.7, col="gray32", scaling=3)           # the SNPs
points(rda, display="Sites", pch=21, cex=1.3, col="gray32", scaling=3, bg=bg[Depth]) # the wolves
text(rda, scaling=3, display="bp", col="#0868ac", cex=1)                           # the predictors
legend("bottomright", legend=levels(Depth), bty="n", col="gray32", pch=21, cex=1, pt.bg=bg)
