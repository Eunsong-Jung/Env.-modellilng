#install.packages('Rcpp')
#library(Rcpp)
library(readxl)
library(biwavelet)
library(bestNormalize)
library(dplyr)
library(stats)
install.packages("ts.extend")
install.packages("tidySEM")
install.packages("lavaan", dependencies = TRUE)
install.packages("R.oo") 
library(R.oo)
library(ts.extend)
library(writexl)
library(lavaan)
library(tidySEM)
library(lavaan)
library(ggplot2)
library(dplyr)


climate<-read_excel("wavedata.xlsx")
climate <- climate[,-1]
climate<-data.frame(climate)


    
model<- '
phyto =~ cya+bac+chl+Dr
clim =~ PDO+ESC
meteo =~ RI+WT
water =~ TN+FLJD
phyto ~ clim + meteo + water

'

fit <- sem(model, data = climate)
summary(fit, standardized = TRUE)

get_layout(fit)
graph_sem(fit)

graph_sem(fit, variance_diameter=1,  text_size = 4,layout = lay)


lay<-data.frame(get_layout(fit))
write_xlsx(lay,path="lay.xlsx")
lay<-read_excel("lay.xlsx")

+tot2+bac2+cya2+chl2+cha2+cry2+och2+eug2+mio2
+d+dMa+Lambda+D+Dr+He+Je+DI+Carteria+Chlamydomonas+Sphaerellopsis+Gonium+Pteromonas+
  Sphaerocystis+Pectodictyon+Basichlamys+Eudorina+Pandorina+Oedogonium+Planctonema+Ankyra+
  Characium+Pediastrum+Tetraedron+Golenkinia+Coenochloris+Coenocystis+Eutetramorus+Gloeocystis+
  Coeiastrum+Coelastrum+Scenedesmus+Tetradesmus+Tetrastrum+Westella+Planktosphaeria+Schroederia+
  Ankistrodesmus+Kirchneriella+Monoraphidium+Selenastrum+Polyedriopsis+Treubaria+Chlorella+
  Closteriopsis+Dictyosphaerium+Geminella+Golenkiniopsis+Micractinium+Chodatella+Chodetella+Oocystis+
  Acanthosphaera+Actinastrum+Koliella+Stichococcus+Crucigenia+Cruncigenia+Dispora+Neidium+Nitzschia+
  Cymbella+Encyonema+Gomphonema+Reimeria+Eunotia+Fragilaria+Synedra+Achnanthes+Frusturia+Hantzschia+
  Diploneis+Caloneis+Gyrosigma+Navicula+Pinnularia+Sellaphora+Stauroneis+Epithemia+Cymatopleura+
  Surirella+Asterionella+Diatoma+Tabellaria+Amphora+Aulacoseira+Bacillaria+Cocconeis+Melosira+
  Craticula+Acanthoceras+Cyclostephanos+Cyclotella+Stephanodiscus+Thalassiosira+Chroococcus+
  Microcystis+Aphanizomenon+Anabaena+Planktothrix+Nostoc+Oscillatoria+Phormidium+Woronichinia+
  Aphanocapsa+Cyanotetras+Merismopedia+Synechocystis+Pseudanabaena+Cryptomonas+Chroomonas+Rhodomonas+
  Closterium+Cosmarium+Spondylosium+Staurastrum+Penium+Cylindrocystis+Spirogyra+Elakatothrix+Dinobryon+
  Mallomonas+Synura+Phacus+Euglena+Trachelomonas+Strombonas+Ceratium+Gymnodinium+Glenodinium+
  Peridinium+Chlamydomonadaceae+Goniaceae+Phacotaceae+Sphaerocystidaceae+Sphaerodictyaceae+
  Tetrabaenaceae+Volvocaceae+Oedogoniaceae+Characiaceae+Hydrodictyaceae+Neochloridaceae+
  Radiococcaceae+Scenedesmaceae+Schizochlamydaceae+Schroederiaceae+Selenastraceae+
  Sphaeropleales incertae sedis+Treubariaceae+Chlorellaceae+Oocystaceae+Koliellaceae+Prasiolaceae+
  Trebouxiophyceae incertae sedis+Coccomyxaceae+Bacillariaceae+Cymbellaceae+Gomphonemataceae+
  Eunotiaceae+Fragilariaceae+Achnanthaceae+Amphipleuraceae+Diploneidaceae+Naviculaceae+Pinnulariaceae+
  Sellaphoraceae+Stauroneidaceae+Rhopalodiaceae+Surirellaceae+Tabellariaceae+Catenulaceae+
  Aulacoseiraceae+Cocconeidaceae+Melosiraceae+Stephanodiscaceae+Thalassiosiraceae+Chroococcaceae+
  Microcystaceae+Aphanizomenonaceae+Nostocaceae+Microcoleaceae+Oscillatoriaceae+Coelosphaeriaceae+
  Merismopediaceae+Pseudanabaenaceae+Cryptomonadaceae+Hemiselmidaceae+Pyrenomonadaceae+Closteriaceae+
  Desmidiaceae+Peniaceae+Mesotaeniaceae+Zygnemataceae+Elakatotrichaceae+Dinobryaceae+Mallomonadaceae+
  Synuraceae+Phacidae+Euglenidae+Ceratiaceae+Gymnodiniaceae+Peridiniales incertae sedis+Peridinium +
  Chlamydomonadales+Oedogoniales+Sphaeropleales+Chlorellales+Chlorellales+Prasiolales+
  Trebouxiophyceae ordo incertae sedis+Bacillariales+Cymbellales+Eunotiales+Fragilariales+
  Mastogloiales+Naviculales+Rhopalodiales+Surirellales+Tabellariales+Thalassiophysales+Aulacoseirales+
  Cocconeidales+Melosirales+Chaetocerotales+Stephanodiscales+Thalassiosirales+chroococcales+
  Nostocales+Oscillatoriales+Synechococcales+Cryptomonadales+Pyrenomonadales+Desmidiales+Zygnematales+
  Klebsormidiales+Chromulinales+Synurales+Euglenales+Euglenida+Gonyaulacales+Gymnodiniales+
  Peridiniales+Chlorophyceae+Trebouxiophyceae+Trebouxiophyceae ordo incertae sedis+Bacillariophyceae+
  Coscinodiscophyceae+Mediophyceae+cyanophyceae+Cryptophyceae+Conjugatophyceae+Klebsormidiophyceae+
  Chrysophyceae+Synurophyceae+Euglenophyceae+miozoa



for (i in 93:96) {
tryCatch({
  test<-spectrum.test(bestNormalize(climate[,i],k=5)$x.t)
  #spec<-data.frame(cbind(test$statistic,test$p.value))
 # wt <- wt(cbind(1:276,bestNormalize(climate[,i],k=5)$x.t))
  
  
  png(filename=paste("test",i,".png",sep=""),width=1000,height=500,unit='px',bg='transparent')
  par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 5) + 0.1)
  plot(test,plot.cb=TRUE, plot.phase=TRUE)
  dev.off()
  
#  png(filename=paste("wt",i,".png",sep=""),width=1000,height=500,unit='px',bg='transparent')
#  par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 5) + 0.1)
#  plot(wt,plot.cb=TRUE, plot.phase=TRUE)
#  dev.off()
  
  
 # write_xlsx(spec,path=paste("spec", i, ".xlsx", sep = ""))
}, error=function(e){})
}  


for (i in 1:5) 
for (x in 6:17) {

  wtc<-wtc(cbind(1:276,climate[,i]),cbind(1:276,climate[,x]),mother='morlet')
  png(filename=paste(i,"-",x,".png",sep=""),width=1000,height=500,unit='px',bg='transparent')
  par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 5) + 0.1)
  plot(wtc,plot.cb=TRUE, plot.phase=TRUE)
  dev.off()
}

for (i in 1:5) 
  for (x in 33:306) {
    
    wtc<-wtc(cbind(1:276,climate[,i]),cbind(1:276,climate[,x]),mother='morlet')
    png(filename=paste(i,"-",x,".png",sep=""),width=1000,height=500,unit='px',bg='transparent')
    par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 5) + 0.1)
    plot(wtc,plot.cb=TRUE, plot.phase=TRUE)
    dev.off()
  }

for (i in 6:17) 
  for (x in 18:32) {
    
    wtc<-wtc(cbind(1:276,climate[,i]),cbind(1:276,climate[,x]),mother='morlet')
    png(filename=paste(i,"-",x,".png",sep=""),width=1000,height=500,unit='px',bg='transparent')
    par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 5) + 0.1)
    plot(wtc,plot.cb=TRUE, plot.phase=TRUE)
    dev.off()
  }

for (i in 6:17) #기후기상상
  for (x in 33:306) {
    
    wtc<-wtc(cbind(1:276,climate[,i]),cbind(1:276,climate[,x]),mother='morlet')
    png(filename=paste(i,"-",x,".png",sep=""),width=1000,height=500,unit='px',bg='transparent')
    par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 5) + 0.1)
    plot(wtc,plot.cb=TRUE, plot.phase=TRUE)
    dev.off()
  }

for (i in 18:32) #수질,유량
  for (x in 33:306) {
    
    wtc<-wtc(cbind(1:276,climate[,i]),cbind(1:276,climate[,x]),mother='morlet')
    png(filename=paste(i,"-",x,".png",sep=""),width=1000,height=500,unit='px',bg='transparent')
    par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 5) + 0.1)
    plot(wtc,plot.cb=TRUE, plot.phase=TRUE)
    dev.off()
  }


for (i in 1:32) 
{
    
    wtc<-wtc(cbind(1:276,climate[,i]),cbind(1:276,bestNormalize(climate[,35],k=5)$x.t),mother='morlet')
    png(filename=paste(i,"-cya",".png",sep=""),width=1000,height=500,unit='px',bg='transparent')
    par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 5) + 0.1)
    plot(wtc,plot.cb=TRUE, plot.phase=TRUE)
    dev.off()
}




