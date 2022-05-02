norm <- function(x){(x-min(x))/(max(x)-min(x))}

num_diel <- subset(diel, select = -c(site,time))
num_diel<-data.frame(sapply(num_diel,norm))
num_diel<-data.frame(cbind(sapply(num_diel[, 1:8],norm),norm(num_diel[, 9:11]),
                           norm(num_diel[, 12:14]),norm(num_diel[, 15:17]),
                           norm(num_diel[, 18:20]),norm(num_diel[, 21:23])))


MDSdiel <- metaMDS(num_diel, distance="bray", k=2, maxit=999,trymax=100) ##k is the number of dimensions
NMDS1diel <- MDSdiel$points[,1] ##also found using scores(birdMDS)
NMDS2diel <- MDSdiel$points[,2]
diel.plot<-cbind(diel,NMDS1diel,NMDS2diel)
fitdiel<-envfit(MDSdiel,num_diel,permu = 999, choices=c(1,2))
plot(MDSdiel,type='t',display="species")
plot(fitdiel,p.max=0.05)

arrow<-data.frame(fitdiel$vectors$arrows,R = fitdiel$vectors$r, P = fitdiel$vectors$pvals)
arrow$Var <- rownames(arrow)
arrow2<- subset(arrow, P < 0.05)
arrow.p<-filter(arrow, arrow$P <= 0.05)

Site<-factor(diel$site,levels=c("a","b"),labels=c("Upstream","Midstream"))
Time<-factor(diel$time,levels=c("a","b","c","d","e","f","g","h","i"),labels=c("9:00","12:00","15:00","18:00","21:00","0:00","3:00","6:00","9:01"))


p<-ggplot(data=diel.plot, )+
  theme_minimal()+
  theme(axis.text = element_text(size = 30),axis.title= element_text(size = 30),
        legend.title=element_text(size = 30),legend.text=element_text(size = 20,color="#404040"))+
  scale_x_continuous(name="NMDS1diel", breaks=c(-1,0,1), limits=c(-1, 1)) +
  scale_y_continuous(name="NMDS2diel", breaks=c(-1,0,1),limits=c(-1, 1))+
  geom_point(aes(NMDS1diel, NMDS2diel, color=Time,shape=Site),size=6)+##separates overlapping points
  scale_fill_brewer(palette="Set1")+
  #geom_segment(data=arrow2, aes(x=0, y=0, xend=NMDS1, yend=NMDS2,label=Var))+
  geom_text_repel(data=arrow2,max.overlaps=100, aes(x=0.6*NMDS1,y=0.6*NMDS2,label=Var),size=10)+
  geom_point(data=arrow2,aes(x=0.6*NMDS1,y=0.6*NMDS2),size=1)+
  stat_ellipse(aes(NMDS1diel, NMDS2diel,fill=Site), alpha=0.1,size =3, geom="polygon")

x11(width = 40,height = 40)
p