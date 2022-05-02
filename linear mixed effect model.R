library('dplyr')
library('egg')
library('vegan')
library('ggrepel')
cv<-data.frame(read.csv("desc.csv")) #coevar calculated using the original data(non-transformed)

p<-ggplot(data=cv,aes(y=coe_tm, x=coe_sm))+
  #theme_minimal()+
  theme(axis.text = element_text(size = 30),axis.title= element_text(size = 30),
        legend.title=element_text(size = 30),legend.text=element_text(size = 20,color="#404040"))+
  scale_x_continuous(name="coevar_space",limits=c(0, 1.2)) +
  scale_y_continuous(name="coevar_time",limits=c(0, 1.2))+
  geom_point(aes(coe_sm,coe_tm, color=variable),size=6)+##separates overlapping points
  scale_fill_brewer(palette="Set1")+
  geom_smooth(method='lm',se=FALSE)+
  geom_errorbar(aes(ymin=coe_tm-coe_ts, ymax=coe_tm+coe_ts))+
  geom_errorbarh(aes(xmin=coe_sm-coe_ss, xmax=coe_sm+coe_ss))
p<-set_panel_size(p, width= unit(20,"in"), height=unit(20,"in"))
ggsave(plot = p,filename = "var.png")

cor.test(cv$coe_tm,cv$coe_sm)

vp<-varpart(sang_year[,5:17],~Site+Year,sang_year[,1:2],data=sang_year[,1:17])
showvarparts(2, bg = c("hotpink","skyblue"))
plot(vp, bg = c("hotpink","skyblue"))
showvarparts(2, bg=2:4)
plot(vp, bg=2:4)
-------------------

year1<-sapply(sang_year[, 5:17],norm)  
MDS <- metaMDS(year1, distance="bray", k=2, maxit=999,trymax=100) ##k is the number of dimensions
NMDS1 <- MDS$points[,1] ##also found using scores(birdMDS)
NMDS2 <- MDS$points[,2]
mds.plot<-cbind(sang_year[,1:2],NMDS1,NMDS2)
fit<-envfit(MDS,year1,permu = 999, choices=c(1,2))

plot(MDS,type='t',display="species")
plot(fit,p.max=0.05)

arrow<-data.frame(fit$vectors$arrows,R = fit$vectors$r, P = fit$vectors$pvals)
arrow$Var <- rownames(arrow)
arrow2<- subset(arrow, P < 0.05)
arrow.p<-filter(arrow, arrow$P <= 0.05)

mds.plot$Site<-factor(mds.plot$Site)
mds.plot$Year<-factor(mds.plot$Year)


p<-ggplot(data=mds.plot, )+
  theme_minimal()+
  theme(axis.text = element_text(size = 30),axis.title= element_text(size = 30),
        legend.title=element_text(size = 30),legend.text=element_text(size = 20,color="#404040"))+
  scale_x_continuous(name="NMDS1", breaks=c(-0.7,0,0.7), limits=c(-0.7, 0.7)) +
  scale_y_continuous(name="NMDS2", breaks=c(-0.7,0,0.7),limits=c(-0.7, 0.7))+
  geom_point(aes(NMDS1, NMDS2, color=Year),size=3)+##separates overlapping points ,shape=Site
  scale_fill_brewer(palette="Set1")+
  #geom_segment(data=arrow2, aes(x=0, y=0, xend=NMDS1, yend=NMDS2,label=Var))+
  geom_text_repel(data=arrow2,max.overlaps=100, aes(x=0.3*NMDS1,y=0.3*NMDS2,label=Var),size=5)+
  geom_segment(data=arrow2,aes(x=0,y=0,xend=0.3*NMDS1,yend=0.3*NMDS2),size=0.1)

p

