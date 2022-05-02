library(vegan)
library(ggplot2)
library("bestNormalize")
library(Hmisc)
library("car")
library(readxl)
library(readr)
install.packages("writexl")
library(writexl)
install.packages("wesanderson")
library("wesanderson")

year<- read_excel("month_station.xlsx",sheet = "Year")
year <-subset(year, select = - )
y1<-subset(year,cluster=="1",select=c(-cluster))
y2<-subset(year,cluster=="2",select=c(-cluster))
y3<-subset(year,cluster=="3",select=c(-cluster))
y4<-subset(year,cluster=="4",select=c(-cluster))
#clst$Elv<-3+bestNormalize(clst$Elv)$x.t##repeat on all columns.. should find a way for application on a data frame
#write.csv(clst,file="best_clst.csv")
#write_xlsx(clst,"best_clst.xlsx")

hist.data.frame(year)
sapply(year,shapiro.test)
ny1<-sapply(y1,bestNormalize) 

y4 <- subset(y4, select = -tn)
lmfit4 <- lm(ch~.,data=y4)
vif(lmfit4) > 10


MDSclst <- metaMDS(y1, distance="bray", k=4, trymax=100) ##k is the number of dimensions
#stressplot(MDSclst)

plot(MDSclst,display="sites")
NMDS1clst <- MDSclst$points[,1] ##also found using scores(birdMDS)
NMDS2clst <- MDSclst$points[,2]
#NMDS3clst <- MDSclst$points[,3]
#NMDS4clst <- MDSclst$points[,4]
#title(main="cluster1")


clst.plot<-cbind(y1,NMDS1clst,NMDS2clst)
fitclst<-envfit(MDSclst,y1,permu = 999, choices=c(1,2))
fitclst
plot(fitclst,p.max=0.05)
clst.plot<-edit(clst.plot)

arrow<-data.frame(fitclst$vectors$arrows,R = fitclst$vectors$r, P = fitclst$vectors$pvals)
arrow$Var <- rownames(arrow)
arrow.p<-filter(arrow, arrow$P <= 0.05)

p<-ggplot(data=clst.plot, aes(NMDS1clst, NMDS2clst))+
geom_point(data=clst.plot,aes(NMDS1clst, NMDS2clst, color=cluster),position=position_jitter(.1))+
  scale_color_manual(values=wes_palette(n=4, name="Moonrise3"))+
##separates overlapping points
  #stat_ellipse(aes(fill=cluster)alpha=.2,type='t',size =1, geom="polygon")+ ##changes shading on ellipses
  theme_minimal()+
  geom_segment(data=arrow, aes(x=0, y=0, xend=0.2*NMDS1, yend=0.2*NMDS2,label=Var,lty=Var), arrow=arrow(length=unit(0.4, "cm")*arrow$R))+ ##add arrows (scaled by R-squared value)
  geom_text(data=arrow,aes(x=0.2*NMDS1-0.05,y=0.2*NMDS2,label=Var),size=4)

p
