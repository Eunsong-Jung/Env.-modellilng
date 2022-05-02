library(vegan)
library(ggplot2)
library(Hmisc)
library("car")
library("ggrepel")


seasonal2 <- read_excel("seasonal2.xlsx")
num_seasonal2<-data.frame(sapply(num_seasonal2,norm))

seasonal <- read_excel("Straitification/seasonal.xlsx",sheet=1)
num_seasonal <- subset(seasonal, select = -c(site,month,Chl))
num_seasonal<-data.frame(cbind(norm(num_seasonal[, 1:3]),
                               norm(num_seasonal[, 4:6]),norm(num_seasonal[, 7:9]),
                               norm(num_seasonal[, 10:12]),sapply(num_seasonal[, 13:20],norm))) #,norm(num_seasonal[, 21:23]
num_seasonal<-data.frame(sapply(num_seasonal,norm))

#num_seasonal <- subset(seasonal, select = -c(Site,Month))
#num_seasonal2<-data.frame(scale(num_seasonal2)+3)

phy <- read_excel("Straitification/phy.xlsx",sheet=2)
num_phy <-subset(phy, select = -c(Site,Month))
num_phy<-data.frame(cbind(norm(num_phy[, 1:3]),
                               norm(num_phy[, 4:6]),norm(num_phy[, 7:9])))
num_phy <- log(num_phy+1)
num_phy<-data.frame(sapply(num_phy,norm))

#phy$irr<-3+bestNormalize(phy$irr)$x.t



rda0<- capscale(num_phy~1.,num_seasonal,distance="bray")
rda1<- capscale(num_phy~.,num_seasonal,distance="bray")

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
+
  geom_segment(data = yz,aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                             type = "closed"),linetype=1, size=0.6,colour = "blue")+
  geom_text_repel(data = yz,aes(RDA1,RDA2,label=row.names(yz)))+
  labs(x=paste("RDA 1 (", format(100 *rda$cont[[1]][2,1], digits=4), "%)", sep=""),
       y=paste("RDA 2 (", format(100 *rda$cont[[1]][2,2], digits=4), "%)", sep=""))+
  geom_hline(yintercept=0,linetype=3,size=1) + 
  geom_vline(xintercept=0,linetype=3,size=1)+
  guides(shape=guide_legend(title=NULL,color="black"),
         fill=guide_legend(title=NULL))+
  theme_bw()+theme(panel.grid=element_blank())



num_phy <- scale(num_phy)
hist.data.frame(num_phy)
sapply(num_phy,shapiro.test)
#write.csv(phy,file="phy.csv") 
comb<-cbind(num_phy,num_seasonal)
#num__phy <- subset(num_phy, select = -c(irr,S,max,air))

#write.csv(num__phy,file="test.csv") 

phyt <- subset(num_phy, select = c(bac,cya,chl,tot,chla))
#phye<-subset(num_phy,select=-c(bac,cya,chl,tot,chla))







adjRsq.rrda <- RsquareAdj (rrda)$adj.r.squared
rrda.0<-rda(num_phy~ 1,num_seasonal2)
sel.rrda<-ordiR2step(rrda.0,scope=formula(rrda),R2scope = adjRsq.rrda, direction = 'forward', permutations = 999)
sel.rrda

lmfit <- lm(Chl0~.,data=num_seasonal2)
lmfit
vif(lmfit) > 10

summary(rrda)
screeplot(rrda)
anova_terms<-anova<-anova(rrda,by="term")
anova_terms

plot(rda,type="text",main="Seasonal") 
plot(rrda)
anova(rrda,by="term")
scoreRDA <- scores(rda, display=c("sp", "wa", "lc", "bp"), scaling=2)
scoreRDA$species
scoreRDA$sites
scoreRDA$biplot

# correlation
#scoreOa <- as.data.frame(scoreRDA$sites)
cor.test(scoreOa$RDA1, phye$WT, method="pearson")
cor.test(scoreOa$RDA1, phye$air, method="pearson")
cor.test(scoreOa$RDA1, phye$no, method="pearson")
cor.test(scoreOa$RDA1, phye$po, method="pearson")
cor.test(scoreOa$RDA1, phye$si, method="pearson")
cor.test(scoreOa$RDA1, phye$RWCS, method="pearson")
cor.test(scoreOa$RDA1, phye$S, method="pearson")
cor.test(scoreOa$RDA1, phye$max, method="pearson")
cor.test(scoreOa$RDA1, phye$wind, method="pearson")
cor.test(scoreOa$RDA1, phye$dis, method="pearson")
cor.test(scoreOa$RDA1, phye$elv, method="pearson")

cor.test(scoreOa$RDA2, phye$WT, method="pearson")
cor.test(scoreOa$RDA2, phye$air, method="pearson")
cor.test(scoreOa$RDA2, phye$no, method="pearson")
cor.test(scoreOa$RDA2, phye$po, method="pearson")
cor.test(scoreOa$RDA2, phye$si, method="pearson")
cor.test(scoreOa$RDA2, phye$RWCS, method="pearson")
cor.test(scoreOa$RDA2, phye$S, method="pearson")
cor.test(scoreOa$RDA2, phye$max, method="pearson")
cor.test(scoreOa$RDA2, phye$wind, method="pearson")
cor.test(scoreOa$RDA2, phye$dis, method="pearson")
cor.test(scoreOa$RDA2, phye$elv, method="pearson")

cor.test(scoreOa$RDA1, phyt$tot, method="pearson")
cor.test(scoreOa$RDA1, phyt$chla, method="pearson")
cor.test(scoreOa$RDA1, phyt$chl, method="pearson")
cor.test(scoreOa$RDA1, phyt$cya, method="pearson")
cor.test(scoreOa$RDA1, phyt$bac, method="pearson")

cor.test(scoreOa$RDA2, phyt$tot, method="pearson")
cor.test(scoreOa$RDA2, phyt$chla, method="pearson")
cor.test(scoreOa$RDA2, phyt$chl, method="pearson")
cor.test(scoreOa$RDA2, phyt$cya, method="pearson")
cor.test(scoreOa$RDA2, phyt$bac, method="pearson")

#cor<-rcorr(as.matrix(num_phy))
#cor1<-data.frame(cor$r,cor$P)
cor1
summary(cor1)
write.csv(cor1,file="cor1.csv") 


install_github('fawda123/ggord')
library(ggord)
ggord(rrda, phy$Month, axes=c("1","2"),ptslab=TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # looking at the raw code, this is plotting the 'wa scores', the blue dots are different species  







install.packages("devtools")
library(devtools)
devtools::install_github("gavinsimpson/ggvegan")
library(ggvegan)
library(ggRDA)
#exp_by_x <- (as.list(rda$CCA$eig)$RDA1)/(rda$tot.chi) * 100
#exp_by_y <- (as.list(rda$CCA$eig)$RDA2)/(rda$tot.chi) * 100


ggRDA(rda,envfit_df = phye, sp_size = 5) +
  # Generally theme_classic is a good choice to paint a figure
  theme_classic() +
  # In general, we don't need to show the legend in RDA figure
  theme(legend.position = "none") +
  xlab(paste('RDA1 (', round(exp_by_x, 2), '%)', sep = '')) +
  ylab(paste('RDA2 (', round(exp_by_y, 2), '%)', sep = '')) +
  # scale_XXXXX_manual series provide the ability
  # to define the style of legend by variable value
  scale_size_manual(values = c('ns' = .6,'sig' = .8)) +
  # Q: What's species here? I don't remember their is a significant level which is called 'species'
  # A: Indeed, their is no significant 'species'. However, 
  # the species name in RDA which is generated from geom_text contains colour attribution.
  scale_colour_manual(values = c(
    'ns' = '#606060',
    'sig' = 'black',
    'species' = 'red'
  )) +
  scale_linetype_manual(values = c('ns' = 8, 'sig' = 1))