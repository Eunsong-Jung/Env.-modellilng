install.packages("writexl")
install.packages("RColorBrewer")
install.packages("inlmisc")

library(vegan)
library(ggplot2)
library("bestNormalize")
library(Hmisc)
library("car")
library(readxl)
library(readr)
library(writexl)
library(inlmisc)
library(RColorBrewer)

fish <- read_excel("fish.xlsx")
fish$period <- as.factor(fish$period)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}



fish2 <- data_summary(fish, varname="GLU", groupnames=c("fish", "period"))
fish3 <- data_summary(fish, varname="GOT", groupnames=c("fish", "period"))
fish4 <- data_summary(fish, varname="GPT", groupnames=c("fish", "period"))
fish5 <- data_summary(fish, varname="BUN", groupnames=c("fish", "period"))
fish6 <- data_summary(fish, varname="CRE", groupnames=c("fish", "period"))
fish7 <- data_summary(fish, varname="T.P", groupnames=c("fish", "period"))
fish8 <- data_summary(fish, varname="ALB", groupnames=c("fish", "period"))
fish9 <- data_summary(fish, varname="CA", groupnames=c("fish", "period"))
fish10 <- data_summary(fish, varname="P", groupnames=c("fish", "period"))
fish11 <- data_summary(fish, varname="BKA", groupnames=c("fish", "period"))



f<- ggplot(fish2, aes(x=period, y=GLU, fill=fish)) +
  theme(text = element_text(size=30))+
  geom_bar(stat="identity", color="black", 
           position=position_dodge(),width=.4) +
  geom_errorbar(aes(ymin=GLU-sd, ymax=GLU+sd), width=.2,
                position=position_dodge(0.4))

f2<- ggplot(fish3, aes(x=period, y=GOT, fill=fish)) +   theme(text = element_text(size=30))+ 
  geom_bar(stat="identity", color="black", 
           position=position_dodge(),width=.4) +
  geom_errorbar(aes(ymin=GOT-sd, ymax=GOT+sd), width=.2,
                position=position_dodge(0.4))

f3<- ggplot(fish4, aes(x=period, y=GPT, fill=fish)) +   theme(text = element_text(size=30))+ 
  geom_bar(stat="identity", color="black", 
           position=position_dodge(),width=.4) +
  geom_errorbar(aes(ymin=GPT-sd, ymax=GPT+sd), width=.2,
                position=position_dodge(0.4))

f4<- ggplot(fish5, aes(x=period, y=BUN, fill=fish)) +   theme(text = element_text(size=30))+ 
  geom_bar(stat="identity", color="black", 
           position=position_dodge(),width=.4) +
  geom_errorbar(aes(ymin=BUN-sd, ymax=BUN+sd), width=.2,
                position=position_dodge(0.4))

f5<- ggplot(fish6, aes(x=period, y=CRE, fill=fish)) +   theme(text = element_text(size=30))+ 
  geom_bar(stat="identity", color="black", 
           position=position_dodge(),width=.4) +
  geom_errorbar(aes(ymin=CRE-sd, ymax=CRE+sd), width=.2,
                position=position_dodge(0.4))

f6<- ggplot(fish7, aes(x=period, y=T.P, fill=fish)) +   theme(text = element_text(size=30))+ 
  geom_bar(stat="identity", color="black", 
           position=position_dodge(),width=.4) +
  geom_errorbar(aes(ymin=T.P-sd, ymax=T.P+sd), width=.2,
                position=position_dodge(0.4))

f7<- ggplot(fish8, aes(x=period, y=ALB, fill=fish)) +   theme(text = element_text(size=30))+ 
  geom_bar(stat="identity", color="black", 
           position=position_dodge(),width=.4) +
  geom_errorbar(aes(ymin=ALB-sd, ymax=ALB+sd), width=.2,
                position=position_dodge(0.4))

f8<- ggplot(fish9, aes(x=period, y=CA, fill=fish)) +   theme(text = element_text(size=30))+ 
  geom_bar(stat="identity", color="black", 
           position=position_dodge(),width=.4) +
  geom_errorbar(aes(ymin=CA-sd, ymax=CA+sd), width=.2,
                position=position_dodge(0.4))

f9<- ggplot(fish10, aes(x=period, y=P, fill=fish)) +   theme(text = element_text(size=30))+ 
  geom_bar(stat="identity", color="black", 
           position=position_dodge(),width=.4) +
  geom_errorbar(aes(ymin=P-sd, ymax=P+sd), width=.2,
                position=position_dodge(0.4))

f10<- ggplot(fish11, aes(x=period, y=BKA, fill=fish)) +   theme(text = element_text(size=30))+ 
  geom_bar(stat="identity", color="black", 
           position=position_dodge(),width=.4) +
  geom_errorbar(aes(ymin=BKA-sd, ymax=BKA+sd), width=.2,
                position=position_dodge(0.4))

par(mfrow=c(2,4))
plot(f, main="GLU")
plot(f2, main="GOT")
plot(f3,main="GPT")
plot(f4,main="BUN")
plot(f5,main="CRE")
plot(f6,main="T.P")
plot(f7,main="ALB")
plot(f8,main="CA")
plot(f9,main="P")
plot(f10,main="BKA")

blue<-subset(fish, fish=="blue")
bass<-subset(fish,fish=="bass")



ggplot(blue)+
  geom_bar(data=blue, aes(x=period, y=mean(blue$GLU), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar(data=blue, aes(x=period, ymin=mean(blue$GLU) - sqrt(length(GLU)), ymax=mean(blue$GLU) + 
                            sqrt(length(GLU))), width=0.4, colour="orange", alpha=0.9, size=1.3)


fish <- read_excel("NMDS_fish.xlsx")
fish1 <- subset(fish, select = -c(Fish,Date))
fish1<-scale(fish1)
fish1<-data.frame(fish1)
fish2<-cbind(fish1,subset(fish,select=c(Fish,Date)))



hist.data.frame(fish1)
#sapply(num_diel,shapiro.test)

#num_diel <- subset(num_diel, select = -Air)
#lmfit <- lm(Chla~.,data=num_diel)
#vif(lmfit) > 10


MDS <- metaMDS(fish1, distance="bray", k=2, maxit=999,trymax=100) ##k is the number of dimensions
MDS
#stressplot(MDSdiel)


plot(MDS,display="sites")
NMDS1 <- MDS$points[,1] ##also found using scores(birdMDS)
NMDS2 <- MDS$points[,2]
title(main="Fish")


fish.plot<-cbind(fish2,NMDS1,NMDS2)
fit<-envfit(MDS,fish1,permu = 999, choices=c(1,2))
plot(fit)


arrow<-data.frame(fit$vectors$arrows,R = fit$vectors$r, P = fit$vectors$pvals)
arrow$Var <- rownames(arrow)
arrow2<- subset(arrow, P < 0.05)
arrow2<-arrow[!(arrow2$var=="temB")]
arrow2<-arrow[!(arrow2$var=="tem3")]
arrow2<-arrow[!(arrow2$var=="Chl3")]
arrow.p<-filter(arrow, arrow$P <= 0.05)

p<-ggplot(data=fish.plot, aes(NMDS1, NMDS2))+
  theme_minimal()+
  geom_point(data=fish.plot,aes(NMDS1, NMDS2, color=factor(Date),shape=Fish),size=3)+#separates overlapping points
  scale_fill_brewer(palette="Set1")+
  geom_segment(data=arrow, aes(x=0, y=0, xend=0.02*NMDS1, yend=0.02*NMDS2,label=Var))+
  geom_text(data=arrow,aes(x=0.02*NMDS1,y=0.02*NMDS2,label=Var),size=3)
  #stat_ellipse(aes(fill=Month, alpha=.2,type='t',size =1, geom="polygon")+ ##changes shading on ellipses
                   
  #stat_ellipse(aes(fill=factor(Month)), alpha=.2,type='t',size =1, geom="polygon")+
  #

p


#mds <- as.data.frame(MDS$points)
#nmds_diel <- as.data.frame(num_diel)

# correlation

cor.test(mds$MDS2, nmds_diel$WT)
cor.test(mds$MDS2, nmds_diel$Chla)
cor.test(mds$MDS2, nmds_diel$NO)
cor.test(mds$MDS2, nmds_diel$PO)
cor.test(mds$MDS2, nmds_diel$SI)
cor.test(mds$MDS2, nmds_diel$RWCS)
cor.test(mds$MDS2, nmds_diel$S)
cor.test(mds$MDS2, nmds_diel$Max)
cor.test(mds$MDS2, nmds_diel$Air)
cor.test(mds$MDS2, nmds_diel$Irr)
cor.test(mds$MDS2, nmds_diel$Wind)
cor.test(mds$MDS2, nmds_diel$Dis)
cor.test(mds$MDS2, nmds_diel$Elv)

cor.test(NMDS4diel, diel$WT)
cor.test(NMDS4diel, diel$Chla)
cor.test(NMDS4diel, diel$NO)
cor.test(NMDS4diel, diel$PO)

cor.test(NMDS4diel, diel$RWCS)

cor.test(NMDS4diel, diel$Max)
cor.test(NMDS4diel, diel$Air)
cor.test(NMDS4diel, diel$Irr)
cor.test(NMDS4diel, diel$Wind)
cor.test(NMDS4diel, diel$Dis)





