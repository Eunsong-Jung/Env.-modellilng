library(vegan)
library(ggplot2)
library(Hmisc)
library("car")
library("ggrepel")

norm <- function(x){(x-min(x))/(max(x)-min(x))}

phy <- read_excel("Straitification/phy.xlsx",sheet=1)
num_phy <-subset(phy, select = -c(Site,Month))
num_phy <- log(num_phy+1)

DCA <- decorana (num_phy)

seasonal <- read_excel("Straitification/seasonal.xlsx")
num_seasonal1 <- subset(seasonal, select = c(SI3,PO3,WT0,S,Max))
num_seasonal1 <- data.frame(sapply(num_seasonal1,norm))

cca<- cca(num_phy~.,num_seasonal1,scale="FALSE")
summary(cca)

x11()
plot(cca)
anova(cca,by="term") 
anova(cca,by="axis")
anova(cca)

num_seasonal <- subset(seasonal, select = -c(site,month))
num_seasonal<-data.frame(cbind(sapply(num_seasonal[, 1:8],norm),norm(num_seasonal[, 9:11]),
                               norm(num_seasonal[, 12:14]),norm(num_seasonal[, 15:17]),
                               norm(num_seasonal[, 18:20]),norm(num_seasonal[, 21:23])))

cca0<- cca(num_phy~1.,num_seasonal,scale="FALSE")
cca1<- cca(num_phy~.,num_seasonal,scale="FALSE")
ccaa<-ordistep(cca0,scope=formula(cca1),direction="forward",pin=0.05, pout=0.1,permutations = how(nperm = 999),
               step=20,trace=TRUE )
anova(ccaa,by="term")
anova(ccaa,by="axis")
anova(ccaa)

x11()
plot(ccaa)

CCAseasonal<-summary(cca)

sp=as.data.frame(4*CCAseasonal$species[,1:2])#Depending on the drawing result, the drawing data can be enlarged or reduced to a certain extent, as follows
st=as.data.frame(CCAseasonal$constraints[,1:2])
yz=as.data.frame(CCAseasonal$biplot[,1:2])

Site<-factor(phy$Site,levels=c("1","2","3"),labels=c("Upstream","Midstream","Downstream"))
Month<-factor(phy$Month,levels=c("1","2","3","4","5"),labels=c("November","March","May","August","September"))

fig7<-ggplot() +
  theme_minimal()+
  theme(axis.text = element_text(size = 20),axis.title= element_text(size = 20))+
  #      legend.title=element_text(size = 20),legend.text=element_text(size = 15,color="#404040"))+
  scale_x_continuous(name="CCA1 (22.6%)", breaks=seq(-4,4,by=1),limits=c(-1,2)) +
  scale_y_continuous(name="CCA2 (5.0%)", breaks=seq(-4,4,by=0.5),limits=c(-1,0.5))+
  #geom_point(data = st,aes(CCA1,CCA2,shape=Site, color=Month),size=6)+
  #scale_fill_brewer(palette="Set1")+
  #geom_segment(data = sp,aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
  #             arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
  #                           type = "closed"),linetype=1, size=0.6,colour = "red")+
  
  geom_segment(data = sp,aes(x=0, y=0, xend=CCA1,yend=CCA2),linetype = "dashed",colour = "#00B9E3", size=1,arrow = arrow(length = unit(0.05, "inches")))+
  geom_segment(data = yz,aes(x=0, y=0, xend=CCA1,yend=CCA2),colour = "orange",size=1,arrow = arrow(length = unit(0.05,"inches")))+
  geom_text_repel(data = yz,aes(1.02*CCA1,1.02*CCA2+0.01,label=row.names(yz)),size=5,fontface=2)+
  geom_text_repel(data = sp,aes(0.98*CCA1,0.98*CCA2,label=row.names(sp)),size=5)
x11()
fig7


fig7<-ggplot() +
  theme_minimal()+
  theme(axis.text = element_text(size = 20),axis.title= element_text(size = 20),
        legend.title=element_text(size = 20),legend.text=element_text(size = 15,color="#404040"))+
  scale_x_continuous(name="CCA1", breaks=seq(-4,4,by=1)) +
  scale_y_continuous(name="CCA2", breaks=seq(-4,4,by=1))+
  geom_point(data = st,aes(CCA1,CCA2,shape=Site, color=Month),size=6)+
  scale_fill_brewer(palette="Set1")+
  #geom_segment(data = sp,aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
  #             arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
  #                           type = "closed"),linetype=1, size=0.6,colour = "red")+
  geom_text_repel(data = sp,aes(CCA1,CCA2,label=row.names(sp)),colour = "blue",size=7)+
  geom_segment(data = sp,aes(x=0, y=0, xend=CCA1,yend=CCA2),colour = "blue", arrow = arrow(length = unit(0.05, "inches")))+
  geom_text_repel(data = yz,aes(CCA1,CCA2,label=row.names(yz)),colour = "red",size=5, max.overlaps = 100)+
  geom_segment(data = yz,aes(x=0, y=0, xend=CCA1,yend=CCA2),colour = "red",arrow = arrow(length = unit(0.05,"inches")))

x11()
fig7




phydiel <- read_excel("Straitification/phydiel.xlsx",sheet=1)
num_phydiel <-subset(phydiel, select = -c(site,time))
num_phydiel <- log(num_phydiel+1)

DCA <- decorana (num_phydiel)

diel <- read_excel("Straitification/diel2.xlsx", sheet=2)
num_diel1 <- subset(diel, select = c(S, AT, Max, WL, SI3, FR))
num_diel1 <- data.frame(sapply(num_diel1,norm))
cca<- cca(num_phydiel~.,num_diel1,scale="FALSE")
CCAdiel<-summary(cca)

x11()
plot(cca)
anova(cca,by="term")
anova(cca,by="axis")
anova(cca)


sp=as.data.frame(4*CCA$species[,1:2])#Depending on the drawing result, the drawing data can be enlarged or reduced to a certain extent, as follows
st=as.data.frame(CCA$constraints[,1:2])
yz=as.data.frame(CCA$biplot[,1:2])

Site<-factor(phydiel$site,levels=c("a","b"),labels=c("Upstream","Midstream"))
Time<-factor(phydiel$time,levels=c("a","b","c","d","e","f","g","h","i"),labels=c("9:00","12:00","15:00","18:00","21:00","0:00","3:00","6:00","9:01"))


fig7<-ggplot() +
  theme_minimal()+
  theme(axis.text = element_text(size = 20),axis.title= element_text(size = 20))+
  #      legend.title=element_text(size = 20),legend.text=element_text(size = 15,color="#404040"))+
  scale_x_continuous(name="CCA1 (56.3%)", breaks=seq(-4,4,by=1),limits=c(-1.5,1)) +
  scale_y_continuous(name="CCA2 (12.7%)", breaks=seq(-4,4,by=0.5),limits=c(-1,0.5))+
  #geom_point(data = st,aes(CCA1,CCA2,shape=Site, color=Time),size=6)+
  #scale_fill_brewer(palette="Set1")+
  #geom_segment(data = sp,aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
  #             arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
  #                           type = "closed"),linetype=1, size=0.6,colour = "red")+
  geom_segment(data = sp,aes(x=0, y=0, xend=CCA1,yend=CCA2),linetype = "dashed",colour = "#00B9E3", size=1,arrow = arrow(length = unit(0.05, "inches")))+
  geom_segment(data = yz,aes(x=0, y=0, xend=CCA1,yend=CCA2),colour = "orange",size=1,arrow = arrow(length = unit(0.05,"inches")))+
  geom_text_repel(data = yz,aes(1.1*CCA1-0.02,CCA2+0.03,label=row.names(yz)),size=5,fontface=2)+
  geom_text_repel(data = sp,aes(0.98*CCA1+0.1,0.98*CCA2,label=row.names(sp)),size=5)

x11()
fig7

fig7<-ggplot() +
  theme_minimal()+
  theme(axis.text = element_text(size = 20),axis.title= element_text(size = 20),
        legend.title=element_text(size = 20),legend.text=element_text(size = 15,color="#404040"))+
  scale_x_continuous(name="CCA1", breaks=seq(-4,4,by=1)) +
  scale_y_continuous(name="CCA2", breaks=seq(-4,4,by=1))+
  geom_point(data = st,aes(CCA1,CCA2,shape=Site, color=Time),size=6)+
  scale_fill_brewer(palette="Set1")+
  #geom_segment(data = sp,aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
  #             arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
  #                           type = "closed"),linetype=1, size=0.6,colour = "red")+
  geom_text_repel(data = sp,aes(CCA1,CCA2,label=row.names(sp)),colour = "blue",size=7)+
  geom_segment(data = sp,aes(x=0, y=0, xend=CCA1,yend=CCA2),colour = "blue", arrow = arrow(length = unit(0.05, "inches")))+
  geom_text_repel(data = yz,aes(CCA1,CCA2,label=row.names(yz)),colour = "red",size=5, max.overlaps = 100)+
  geom_segment(data = yz,aes(x=0, y=0, xend=CCA1,yend=CCA2),colour = "red",arrow = arrow(length = unit(0.05,"inches")))+
  stat_ellipse(aes(st$CCA1, st$CCA2,fill=Site), alpha=0.1,size =3, geom="polygon")

x11()
fig7



num_diel <- subset(diel, select = -c(site,time))
num_diel<-data.frame(cbind(sapply(num_diel[, 1:8],norm),norm(num_diel[, 9:11]),
                           norm(num_diel[, 12:14]),norm(num_diel[, 15:17]),
                           norm(num_diel[, 18:20]),norm(num_diel[, 21:23])))


cca0<- cca(num_phydiel~1.,num_diel,scale="FALSE")
cca1<- cca(num_phydiel~.,num_diel,scale="FALSE")

ccaa<-ordistep(cca0,scope=formula(cca1),direction="forward",pin=0.05, pout=0.1,permutations = how(nperm = 999),
               step=20,trace=TRUE )
x11()
plot(ccaa)
anova(ccaa,by="term")
anova(ccaa,by="axis")






rrda<-ordistep(rda0,scope=formula(rda1),direction="forward",pin=0.05, pout=0.1,permutations = how(nperm = 999),
               step=20,trace=TRUE )
rda<-summary(rrda)

rrda$anova
summary(rrda)
anova(rrda,by="term") 

sp=as.data.frame(2*rda$species[,1:2])#Depending on the drawing result, the drawing data can be enlarged or reduced to a certain extent, as follows
st=as.data.frame(rda$sites[,1:2])
yz=as.data.frame(2*rda$biplot[,1:2])

Site<-factor(phy$Site,levels=c("1","2","3"),labels=c("Upstream","Midstream","Downstream"))
Month<-factor(phy$Month,levels=c("1","2","3","4","5"),labels=c("November","March","May","August","September"))


fig7<-ggplot() +
  theme_minimal()+
  theme(axis.text = element_text(size = 20),axis.title= element_text(size = 20),
        legend.title=element_text(size = 20),legend.text=element_text(size = 15,color="#404040"))+
  scale_x_continuous(name="dbRDA1", breaks=c(-2,0,2), limits=c(-2, 2)) +
  scale_y_continuous(name="dbRDA2", breaks=c(-2,0,2),limits=c(-2, 2))+
  geom_point(data = st,aes(CAP1,CAP2,shape=Site, color=Month),size=6)+
  scale_fill_brewer(palette="Set1")+
  #geom_segment(data = sp,aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
  #             arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
  #                           type = "closed"),linetype=1, size=0.6,colour = "red")+
  geom_text_repel(data = sp,aes(CAP1,CAP2,label=row.names(sp)),colour = "blue",size=7)+
  geom_point(data = sp,aes(CAP1,CAP2),colour = "blue",size=1)+
  geom_text_repel(data = yz,aes(CAP1,CAP2,label=row.names(yz)),colour = "red",size=5, max.overlaps = 100)+
  geom_point(data = yz,aes(CAP1,CAP2),colour = "red",size=1)


fig7