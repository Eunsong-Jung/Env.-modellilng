#countour plot **
#time line (period, year, month, day)
#normality
#- (month)
#t test -------------------
#sites line period 

library("readxl")
sang<-data.frame(read_excel("sang.xlsx",sheet="date"))
sang<-subset(sang, Site!="195")
sang<-subset(sang, Site!="90" &  Site!="120" & Site!="170"& Site!="195")

install.packages("lubridate")
library("lubridate")

library('dplyr')
sang_month<-sang %>%
  group_by(Site,Year,Month)%>%
  summarise_all("mean",na.rm=T)


sang_year<-sang_month %>%
  group_by(Site,Year)%>%
  summarise_all("mean",na.rm=T)

sang_yea<-sang %>%
  group_by(Site,Year)%>%
  summarise_all("mean",na.rm=T)

sang$Period<-ifelse(sang$Year>=2012,"after",
                    ifelse(sang$Year>=2010,"during",
                           ifelse(sang$Year>=2000,"before","past")))

sang_year$Period<-ifelse(sang_year$Year>=2012,"after",
                         ifelse(sang_year$Year>=2010,"during",
                                ifelse(sang_year$Year>=2000,"before","past")))


sang_month$Period<-ifelse(sang_month$Year>=2012,"after",
                          ifelse(sang_month$Year>=2010,"during",
                                 ifelse(sang_month$Year>=2000,"before","past")))

sang_peri<-sang %>%
  group_by(Site,Period)%>%
  summarise_all("mean",na.rm=T)

sang_perio<-sang_month %>%
  group_by(Site,Period)%>%
  summarise_all("mean",na.rm=T)

sang_period<-sang_year %>%
  group_by(Site,Period)%>%
  summarise_all("mean",na.rm=T)

sang_year$Period <- as.factor(sang_year$Period)  
sang_month$Period <- as.factor(sang_month$Period)
sang$Period <- as.factor(sang$Period)  

for (i in 6:21) {
  ggplot(sang_period,aes(x=Site,y=DO),group=Period)+
    geom_line(aes(color=factor(Period)))
}


for (i in c(27,40,82,90,107,120,150,170,182)) {
  kruskal.test(TP ~ Period,data=sang_year,Site==i)
  summary(kwAllPairsNemenyiTest(DO~ Period, data = sang,Site==i))
}

kruskal.test(Chl ~ Period,data=sang_month,Site==82)
summary(kwAllPairsNemenyiTest(Chl ~ Period, data = sang_month,Site==82))

table<-sang_year%>%
  group_by(Site,Period)%>%
  summarise(Mean=mean(Chl), Std=sd(Chl),
            Max=max(Chl), Min=min(Chl), 
            Median=median(Chl), n=n(),na.rm=T)
table

for (i in 6:21) {
  tryCatch({
    gg<-ggplot(x=sang_period$Site,y=sang_period[,i],group=sang_period$Period)+
      geom_line(aes(color=factor(sang_period$Period)))
    gg
    png(filename=paste(i,"line",".png",sep=""),width=1000,height=500,unit='px',bg='transparent')
    plot(gg,plot.cb=TRUE, plot.phase=TRUE)
    dev.off
  }, error=function(e){})
}


ggplot(sang_year,aes(x=Year,y=TN),group=Site)+
  geom_line(aes(color=factor(Site)))


library("ggplot2")


sang_year$Chl<-astype(int)(sang_year$Ch)
sang_year['Chl'] = sang_year['Chl'].astype(int)
sang_year<-data.frame(read_excel("sang.xlsx",sheet="date"))
writexl::write_xlsx(sang_year,"sang_year.xlsx")

sang_month$series<-as.Date(paste(sang_month$Year,sang_month$Month,01,sep="-"))

sang_month<-sang %>%
  group_by(Site,Year,Month)%>%
  summarise_all("mean",na.rm=T)
sang_month<-subset(sang_month,Month==9)

sang_year<-sang_month %>%
  group_by(Site,Year)%>%
  summarise_all("mean",na.rm=T)

ggplot(sang_year,aes(x=Year,y=Site,z=Chl))+
  stat_contour_filled( breaks=c(0,5,10,15,20,25,30,40,100,200,1000))+
  scale_x_continuous(breaks = seq(1990, 2020, by = 5),limits=c(1990,2020))+
  scale_y_continuous(breaks=c(27,40,82,90,107,120,150,170,182), limits=c(27,182))+
  theme_minimal()

breaks=c(0,5,10,15,20,25,30,40,100,200,700)

install.packages("PMCMRplus")
library("PMCMRplus")




for (i in c(27,40,82,90,107,120,150,170,182)) {
  tryCatch({
    kruskal.test(Chl ~ Period,data=sang_year,Site==i)
    summary(kwAllPairsNemenyiTest(Chl ~ Period, data = sang_year,Site==i))
  }, error=function(e){})
}
