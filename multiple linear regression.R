
library("readxl")
library("lubridate")
library('dplyr')
library('olsrr')
library('regclass')
library('Hmisc')
library("bestNormalize")
library('pastecs')

#Raw data
sang<-data.frame(read_excel("sang.xlsx",sheet="date"))

#칠곡(rk195)제거
sang<-subset(sang, Site!="195")

#일자료를 먼저 월평균한 뒤 연평균자료로 변환

sang$season<-ifelse(sang$Month<=2,"win",
                    ifelse(sang$Month<=5,"spr",
                           ifelse(sang$Month<=9,"sum",
                                  ifelse(sang$Month<=11,"fal","win"))))

#관심 계절만 남김
sang<-data.frame(sang[sang$season=="sum"&sang$Site==27,])


#(월/연) 평균된 데이터 제작
sang_month<-sang %>%
  group_by(Site,Year,Month)%>%
  summarise_all("mean",na.rm=T)

sang_year<-sang %>%
  group_by(Site,Year)%>%
  summarise_all("mean",na.rm=T)

#데이터별 최적 정규분포화 "bestNormalize"
#사용할 데이터에 따라 스크립트 수정(sang or sang_month)
sang_month$WT<-bestNormalize(sang_month$WT,k=5)$x.t
sang_month$DO<-bestNormalize(sang_month$DO,k=5)$x.t
sang_month$Dosat<-bestNormalize(sang_month$Dosat,k=5)$x.t
sang_month$pH<-bestNormalize(sang_month$pH,k=5)$x.t
sang_month$EC<-bestNormalize(sang_month$EC,k=5)$x.t
sang_month$Alk<-bestNormalize(sang_month$Alk,k=5)$x.t
sang_month$NTU<-bestNormalize(sang_month$NTU,k=5)$x.t
sang_month$Chl<-bestNormalize(sang_month$Chl,k=5)$x.t
sang_month$TN<-bestNormalize(sang_month$TN,k=5)$x.t
sang_month$TP<-bestNormalize(sang_month$TP,k=5)$x.t
sang_month$NO<-bestNormalize(sang_month$NO,k=5)$x.t
sang_month$PO<-bestNormalize(sang_month$PO,k=5)$x.t
sang_month$SIO<-bestNormalize(sang_month$SIO,k=5)$x.t
sang_month$NH<-bestNormalize(sang_month$NH,k=5)$x.t
sang_month$SD<-bestNormalize(sang_month$SD,k=5)$x.t

#히스토그램, 정규성검증
year1 <- sang_month[, -c(1,2,3,4,18,19,20,21)]
x11(width = 50,height = 50)
hist.data.frame(year1)
sapply(year1,shapiro.test)
------------------------------------------------------
#year<-cbind(sang_year[,c(1,2,3,4)],year1)

#skip해주세요
#y<-sang %>%
#  group_by(Year)%>%
#  summarise_all("mean",na.rm=T)

#s<-sang %>%
#  group_by(Site)%>%
#  summarise_all("mean",na.rm=T)

#hist.data.frame(y[,-c(1,2,3,4)])
#desc<-data.frame(stat.desc(site))
#desc
#write.csv(desc,"desc3.csv")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #회귀모델 입력변수간 공선성(상관성으로 대체) 검토
rcorr(as.matrix(year1))

#상관분석결과 토대로 최초의 모델구조 직접 입력
mlr<-lm(Chl~WT+NTU+TN+TP+NO+PO+SIO,data=sang_month)

#모델성능에 따른 모델구조 선택 "ols_step_best_subset"
k<-ols_step_best_subset(mlr)
k
plot(k)

#모델구조 유지하면서 연도/지점 분리된 데이터로 개별적 모델 제작  
for (i in c(27,40,82,90,107,120,150,170,182)){
  assign(paste("mlr",i,sep=""),lm(Chl~pH+NTU+NO+PO+SIO,data=sang_month, Site == i))
  }

for (i in 1994:2020){
  assign(paste("mlr",i,sep=""),lm(Chl~NTU+TP+TN+SIO,data=sang_month, Year == i))
}

#모델결과_p값  
for (i in c(27,40,82,90,107,120,150,170,182)){
  assign(paste("p",i,sep=""),pf(summary(get(paste0("mlr", i)))$f[1],summary(get(paste0("mlr", i)))$f[2],summary(get(paste0("mlr", i)))$f[3],lower.tail=F))
  }

for (i in 1994:2020){
    assign(paste("p",i,sep=""),pf(summary(get(paste0("mlr", i)))$f[1],summary(get(paste0("mlr", i)))$f[2],summary(get(paste0("mlr", i)))$f[3],lower.tail=F))
  }

#p값 등 모델결과 내보내기

pv<-cbind(p27,p40,p82,p90,p107,p120,p150,p170,p182)
su<-cbind(summary(mlr27),summary(mlr40),summary(mlr82),summary(mlr90),summary(mlr107),summary(mlr120),summary(mlr150),summary(mlr170),summary(mlr182))
co<-cbind(mlr27$coefficients,mlr40$coefficients,mlr82$coefficients,mlr90$coefficients,mlr107$coefficients,mlr120$coefficients,mlr150$coefficients,mlr170$coefficients,mlr182$coefficients)

pv<-cbind(p1994,p1995,p1996,p1997,p1998,p1999,p2000,p2001,p2002,p2003,p2004,p2005,p2006,p2007,p2008,p2009,p2010,p2011,p2012,p2013,p2014,p2015,p2016,p2017,p2018,p2019,p2020)
adj<-cbind(summary(mlr1994)$adj.r.squared,summary(mlr1995)$adj.r.squared,summary(mlr1996)$adj.r.squared,summary(mlr1997)$adj.r.squared,summary(mlr1998)$adj.r.squared,summary(mlr1999)$adj.r.squared,summary(mlr2000)$adj.r.squared,summary(mlr2001)$adj.r.squared,summary(mlr2002)$adj.r.squared,summary(mlr2003)$adj.r.squared,summary(mlr2004)$adj.r.squared,summary(mlr2005)$adj.r.squared,summary(mlr2006)$adj.r.squared,summary(mlr2007)$adj.r.squared,summary(mlr2008)$adj.r.squared,summary(mlr2009)$adj.r.squared,summary(mlr2010)$adj.r.squared,summary(mlr2011)$adj.r.squared,summary(mlr2012)$adj.r.squared,summary(mlr2013)$adj.r.squared,summary(mlr2014)$adj.r.squared,summary(mlr2015)$adj.r.squared,summary(mlr2016)$adj.r.squared,summary(mlr2017)$adj.r.squared,summary(mlr2018)$adj.r.squared,summary(mlr2019)$adj.r.squared,summary(mlr2020)$adj.r.squared)
rsq<-cbind(summary(mlr1994)$r.squared,summary(mlr1995)$r.squared,summary(mlr1996)$r.squared,summary(mlr1997)$r.squared,summary(mlr1998)$r.squared,summary(mlr1999)$r.squared,summary(mlr2000)$r.squared,summary(mlr2001)$r.squared,summary(mlr2002)$r.squared,summary(mlr2003)$r.squared,summary(mlr2004)$r.squared,summary(mlr2005)$r.squared,summary(mlr2006)$r.squared,summary(mlr2007)$r.squared,summary(mlr2008)$r.squared,summary(mlr2009)$r.squared,summary(mlr2010)$r.squared,summary(mlr2011)$r.squared,summary(mlr2012)$r.squared,summary(mlr2013)$r.squared,summary(mlr2014)$r.squared,summary(mlr2015)$r.squared,summary(mlr2016)$r.squared,summary(mlr2017)$r.squared,summary(mlr2018)$r.squared,summary(mlr2019)$r.squared,summary(mlr2020)$r.squared)
co<-cbind(mlr1994$coefficients,mlr1995$coefficients,mlr1996$coefficients,
          mlr1997$coefficients,mlr1998$coefficients,mlr1999$coefficients,
          mlr2000$coefficients,mlr2001$coefficients,mlr2002$coefficients,
          mlr2003$coefficients,mlr2004$coefficients,mlr2005$coefficients,
          mlr2006$coefficients,mlr2007$coefficients,mlr2008$coefficients,
          mlr2009$coefficients,mlr2010$coefficients,mlr2011$coefficients,
          mlr2012$coefficients,mlr2013$coefficients,mlr2014$coefficients,
          mlr2015$coefficients,mlr2016$coefficients,mlr2017$coefficients,
          mlr2018$coefficients,mlr2019$coefficients,mlr2020$coefficients)

write.csv(pv,file="pv.csv")
write.csv(adj,file="ajd.csv")
write.csv(rsq,file="rsq.csv")
write.csv(co,file="co.csv")

#자동화 작업중
for (i in 1994:2020){
  pv<-do.call(cbind, mget(paste0("p",i))) }

for (i in 1994:2020){
  su<-do.call(cbind, mget(summary(paste0("mlr",i)))) }

#Ends
-------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------
#메모장


mlr_t<-lm(Chl~WT+pH+NTU+NO+PO+NO+SIO,data=year)
ols_step_best_subset(mlr_t)
mlr_t<-lm(Chl~pH+NTU+NO+PO+NO,data=year)
summary(mlr_t)

mlr_s<-lm(Chl~WT+pH+NTU+NO+PO+NO+SIO,data=site)
k<-ols_step_best_subset(mlr_s)
k
plot(k)
mlr_s<-lm(Chl~WT+pH+PO+SIO,data=site)
summary(mlr_s)

class(year)
class(site)

mlr_s<-lm(Chl~WT+DO+pH+NTU+NO,data=site)
summary(mlr_s)
AIC(mlr_s)

k<-ols_step_all_possible(mlr_s)



ols_step_best_subset(mlr_s)


sang$WT<-bestNormalize(sang$WT,k=5)$x.t
sang$DO<-bestNormalize(sang$DO,k=5)$x.t
sang$Dosat<-bestNormalize(sang$Dosat,k=5)$x.t
sang$pH<-bestNormalize(sang$pH,k=5)$x.t
sang$EC<-bestNormalize(sang$EC,k=5)$x.t
sang$Alk<-bestNormalize(sang$Alk,k=5)$x.t
sang$NTU<-bestNormalize(sang$NTU,k=5)$x.t
sang$Chl<-bestNormalize(sang$Chl,k=5)$x.t
sang$TN<-bestNormalize(sang$TN,k=5)$x.t
sang$TP<-bestNormalize(sang$TP,k=5)$x.t
sang$NO<-bestNormalize(sang$NO,k=5)$x.t
sang$PO<-bestNormalize(sang$PO,k=5)$x.t
sang$SIO<-bestNormalize(sang$SIO,k=5)$x.t
sang$NH<-bestNormalize(sang$NH,k=5)$x.t
sang$SD<-bestNormalize(sang$SD,k=5)$x.t

year1 <- sang[, -c(1,2,3,4,18,19,20,21)]

x11(width = 50,height = 50)
hist.data.frame(year1)

sapply(year1,shapiro.test)
------------------------------------------------------
  #year<-cbind(sang_year[,c(1,2,3,4)],year1)
  
  #skip해주세요
  #y<-sang %>%
  #  group_by(Year)%>%
  #  summarise_all("mean",na.rm=T)
  
  #s<-sang %>%
  #  group_by(Site)%>%
  #  summarise_all("mean",na.rm=T)
  
#hist.data.frame(y[,-c(1,2,3,4)])
#desc<-data.frame(stat.desc(site))
#desc
#write.csv(desc,"desc3.csv")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #회귀모델 입력변수간 공선성(상관성으로 대체) 검토
  
  rcorr(as.matrix(year1))

#공선성 통과한 변수 모두 넣고 모델성능에 따른 모델구조 선택 "ols_step_best_subset"
mlr<-lm(Chl~WT+NTU+TN+TP+NO+PO+SIO,data=sang)
k<-ols_step_best_subset(mlr)
k
plot(k)

#도출된 모델구조에 각 데이터(사이트별 또는 지점별)를 넣음
for (i in c(27,40,82,90,107,120,150,170,182)){
  assign(paste("mlr",i,sep=""),lm(Chl~pH+NTU+NO+PO+SIO,data=sang, Site == i))
}

for (i in 1993:2021){
  assign(paste("mlr",i,sep=""),lm(Chl~WT+NTU+TP+PO+NO+SIO,data=sang, Year == i))
}

#모델결과_p값  
for (i in c(27,40,82,90,107,120,150,170,182)){
  assign(paste("p",i,sep=""),pf(summary(get(paste0("mlr", i)))$f[1],summary(get(paste0("mlr", i)))$f[2],summary(get(paste0("mlr", i)))$f[3],lower.tail=F))
}

for (i in 1994:2020){
  assign(paste("p",i,sep=""),pf(summary(get(paste0("mlr", i)))$f[1],summary(get(paste0("mlr", i)))$f[2],summary(get(paste0("mlr", i)))$f[3],lower.tail=F))
}

#p값 등 모델결과 내보내기

pv<-cbind(p27,p40,p82,p90,p107,p120,p150,p170,p182)
su<-cbind(summary(mlr27),summary(mlr40),summary(mlr82),summary(mlr90),summary(mlr107),summary(mlr120),summary(mlr150),summary(mlr170),summary(mlr182))
co<-cbind(mlr27$coefficients,mlr40$coefficients,mlr82$coefficients,mlr90$coefficients,mlr107$coefficients,mlr120$coefficients,mlr150$coefficients,mlr170$coefficients,mlr182$coefficients)

pv<-cbind(p1994,p1995,p1996,p1997,p1998,p1999,p2000,p2001,p2002,p2003,p2004,p2005,p2006,p2007,p2008,p2009,p2010,p2011,p2012,p2013,p2014,p2015,p2016,p2017,p2018,p2019,p2020)
adj<-cbind(summary(mlr1994)$adj.r.squared,summary(mlr1995)$adj.r.squared,summary(mlr1996)$adj.r.squared,summary(mlr1997)$adj.r.squared,summary(mlr1998)$adj.r.squared,summary(mlr1999)$adj.r.squared,summary(mlr2000)$adj.r.squared,summary(mlr2001)$adj.r.squared,summary(mlr2002)$adj.r.squared,summary(mlr2003)$adj.r.squared,summary(mlr2004)$adj.r.squared,summary(mlr2005)$adj.r.squared,summary(mlr2006)$adj.r.squared,summary(mlr2007)$adj.r.squared,summary(mlr2008)$adj.r.squared,summary(mlr2009)$adj.r.squared,summary(mlr2010)$adj.r.squared,summary(mlr2011)$adj.r.squared,summary(mlr2012)$adj.r.squared,summary(mlr2013)$adj.r.squared,summary(mlr2014)$adj.r.squared,summary(mlr2015)$adj.r.squared,summary(mlr2016)$adj.r.squared,summary(mlr2017)$adj.r.squared,summary(mlr2018)$adj.r.squared,summary(mlr2019)$adj.r.squared,summary(mlr2020)$adj.r.squared)
rsq<-cbind(summary(mlr1994)$r.squared,summary(mlr1995)$r.squared,summary(mlr1996)$r.squared,summary(mlr1997)$r.squared,summary(mlr1998)$r.squared,summary(mlr1999)$r.squared,summary(mlr2000)$r.squared,summary(mlr2001)$r.squared,summary(mlr2002)$r.squared,summary(mlr2003)$r.squared,summary(mlr2004)$r.squared,summary(mlr2005)$r.squared,summary(mlr2006)$r.squared,summary(mlr2007)$r.squared,summary(mlr2008)$r.squared,summary(mlr2009)$r.squared,summary(mlr2010)$r.squared,summary(mlr2011)$r.squared,summary(mlr2012)$r.squared,summary(mlr2013)$r.squared,summary(mlr2014)$r.squared,summary(mlr2015)$r.squared,summary(mlr2016)$r.squared,summary(mlr2017)$r.squared,summary(mlr2018)$r.squared,summary(mlr2019)$r.squared,summary(mlr2020)$r.squared)
co<-cbind(mlr1994$coefficients,mlr1995$coefficients,mlr1996$coefficients,
          mlr1997$coefficients,mlr1998$coefficients,mlr1999$coefficients,
          mlr2000$coefficients,mlr2001$coefficients,mlr2002$coefficients,
          mlr2003$coefficients,mlr2004$coefficients,mlr2005$coefficients,
          mlr2006$coefficients,mlr2007$coefficients,mlr2008$coefficients,
          mlr2009$coefficients,mlr2010$coefficients,mlr2011$coefficients,
          mlr2012$coefficients,mlr2013$coefficients,mlr2014$coefficients,
          mlr2015$coefficients,mlr2016$coefficients,mlr2017$coefficients,
          mlr2018$coefficients,mlr2019$coefficients,mlr2020$coefficients)

write.csv(pv,file="pv.csv")
write.csv(adj,file="ajd.csv")
write.csv(rsq,file="rsq.csv")
write.csv(co,file="co.csv")

l<-data.frame(t(bind_cols(as.list(mlr1993$fitted.values),as.list(mlr1994$fitted.values),as.list(mlr1995$fitted.values),as.list(mlr1996$fitted.values),as.list(mlr1997$fitted.values),as.list(mlr1998$fitted.values),as.list(mlr1999$fitted.values),as.list(mlr2000$fitted.values),as.list(mlr2001$fitted.values),as.list(mlr2002$fitted.values),as.list(mlr2003$fitted.values),as.list(mlr2004$fitted.values),as.list(mlr2005$fitted.values),as.list(mlr2006$fitted.values),as.list(mlr2007$fitted.values),as.list(mlr2008$fitted.values),as.list(mlr2009$fitted.values),as.list(mlr2010$fitted.values),as.list(mlr2011$fitted.values),as.list(mlr2012$fitted.values),as.list(mlr2013$fitted.values),as.list(mlr2014$fitted.values),as.list(mlr2015$fitted.values),as.list(mlr2016$fitted.values),as.list(mlr2017$fitted.values),as.list(mlr2018$fitted.values),as.list(mlr2019$fitted.values),as.list(mlr2020$fitted.values,as.list(mlr2021$fitted.values)))))
n<-data.frame(t(bind_cols(as.list(names(mlr1993$fitted.values)),as.list(names(mlr1994$fitted.values)),as.list(names(mlr1995$fitted.values)),as.list(names(mlr1996$fitted.values)),as.list(names(mlr1997$fitted.values)),as.list(names(mlr1998$fitted.values)),as.list(names(mlr1999$fitted.values)),as.list(names(mlr2000$fitted.values)),as.list(names(mlr2001$fitted.values)),as.list(names(mlr2002$fitted.values)),as.list(names(mlr2003$fitted.values)),as.list(names(mlr2004$fitted.values)),as.list(names(mlr2005$fitted.values)),as.list(names(mlr2006$fitted.values)),as.list(names(mlr2007$fitted.values)),as.list(names(mlr2008$fitted.values)),as.list(names(mlr2009$fitted.values)),as.list(names(mlr2010$fitted.values)),as.list(names(mlr2011$fitted.values)),as.list(names(mlr2012$fitted.values)),as.list(names(mlr2013$fitted.values)),as.list(names(mlr2014$fitted.values)),as.list(names(mlr2015$fitted.values)),as.list(names(mlr2016$fitted.values)),as.list(names(mlr2017$fitted.values)),as.list(names(mlr2018$fitted.values)),as.list(names(mlr2019$fitted.values)),as.list(names(mlr2020$fitted.values)),as.list(names(mlr2021$fitted.values)))))
cbind(l,n)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         


x11(width = 50,height = 10)

plot(x=l,y=sang$Chl)
