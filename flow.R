library(readxl)
library(biwavelet)
library("lubridate")
library('dplyr')
library('ggplot2')

flow <- read_excel("flow.xlsx")
wavedata <- read_excel("wavedata.xlsx")
phyto <- read_excel("phyto.xlsx")
zoo <- read_excel("zoo.xlsx")

wavedata <- wavedata[,-1]
wavedata<-data.frame(wavedata)

zoo_month<-zoo%>%
  group_by(year,month)%>%
  summarise_all("mean",na.rm=T)

w.arr <- array(dim = c(229, NROW(wt.2$wave), NCOL(wt.2$wave)))

x11(width = 50,height = 10)
ggplot(zoo_month,aes(x=Date,y=zoo_month(,4:100))+
  geom_line()

x11(width = 50,height = 20)
wt<-wt(cbind(as.Date(flow$date),flow$rain),mother='morlet')
plot(wt)


wt<-wt(cbind(c(1:298),(wavedata$MEI)),mother='morlet')

x11(width = 50,height = 20)
wtc<-wtc(cbind(1:276,wavedata$FLJD),cbind(1:276,wavedata$WT),mother='morlet')
plot(wtc,plot.cb=TRUE, plot.phase=TRUE)


for (i in 1:229) {
    
  assign(paste("wt.",i,sep=""),wt(cbind(c(1:276),wavedata[,i]),mother='morlet'))
  }

w.arr <- array(dim = c(229, NROW(wt.2$wave), NCOL(wt.2$wave)))

w.arr.dis <- wclust(w.arr)

x11(width = 50,height = 10)
plot(hclust(w.arr.dis$dist.mat, method = "ward.D"),
     sub = "", main = "", ylab = "Dissimilarity", hang = -1)

for (i in 1:229) {
  w.arr[i, , ] <- wt.[i]$wave
}

wt$scale
wave<-data.frame(t(wt$wave))
sig<-data.frame(wt$signif)

x11(width = 50,height = 10)
ggplot(data=wave,aes(x=c(1:12419),y=as.numeric(X92)))+
  geom_line()

smooth.wavelet(wt$wave,365)

w.arr[1, , ] <- wt.1$wave
w.arr[2, , ] <- wt.2$wave
w.arr[3, , ] <- wt.3$wave
w.arr[4, , ] <- wt.4$wave
w.arr[5, , ] <- wt.5$wave
w.arr[6, , ] <- wt.6$wave
w.arr[7, , ] <- wt.7$wave
w.arr[8, , ] <- wt.8$wave
w.arr[9, , ] <- wt.9$wave
w.arr[10, , ] <- wt.10$wave
w.arr[11, , ] <- wt.11$wave
w.arr[12, , ] <- wt.12$wave
w.arr[13, , ] <- wt.13$wave
w.arr[14, , ] <- wt.14$wave
w.arr[15, , ] <- wt.15$wave
w.arr[16, , ] <- wt.16$wave
w.arr[17, , ] <- wt.17$wave
w.arr[18, , ] <- wt.18$wave
w.arr[19, , ] <- wt.19$wave
w.arr[20, , ] <- wt.20$wave
w.arr[21, , ] <- wt.21$wave
w.arr[22, , ] <- wt.22$wave
w.arr[23, , ] <- wt.23$wave
w.arr[24, , ] <- wt.24$wave
w.arr[25, , ] <- wt.25$wave
w.arr[26, , ] <- wt.26$wave
w.arr[27, , ] <- wt.27$wave
w.arr[28, , ] <- wt.28$wave
w.arr[29, , ] <- wt.29$wave
w.arr[30, , ] <- wt.30$wave
w.arr[31, , ] <- wt.31$wave
w.arr[32, , ] <- wt.32$wave
w.arr[33, , ] <- wt.33$wave
w.arr[34, , ] <- wt.34$wave
w.arr[35, , ] <- wt.35$wave
w.arr[36, , ] <- wt.36$wave
w.arr[37, , ] <- wt.37$wave
w.arr[38, , ] <- wt.38$wave
w.arr[39, , ] <- wt.39$wave
w.arr[40, , ] <- wt.40$wave
w.arr[41, , ] <- wt.41$wave
w.arr[42, , ] <- wt.42$wave
w.arr[43, , ] <- wt.43$wave
w.arr[44, , ] <- wt.44$wave
w.arr[45, , ] <- wt.45$wave
w.arr[46, , ] <- wt.46$wave
w.arr[47, , ] <- wt.47$wave
w.arr[48, , ] <- wt.48$wave
w.arr[49, , ] <- wt.49$wave
w.arr[50, , ] <- wt.50$wave
w.arr[51, , ] <- wt.51$wave
w.arr[52, , ] <- wt.52$wave
w.arr[53, , ] <- wt.53$wave
w.arr[54, , ] <- wt.54$wave
w.arr[55, , ] <- wt.55$wave
w.arr[56, , ] <- wt.56$wave
w.arr[57, , ] <- wt.57$wave
w.arr[58, , ] <- wt.58$wave
w.arr[59, , ] <- wt.59$wave
w.arr[60, , ] <- wt.60$wave
w.arr[61, , ] <- wt.61$wave
w.arr[62, , ] <- wt.62$wave
w.arr[63, , ] <- wt.63$wave
w.arr[64, , ] <- wt.64$wave
w.arr[65, , ] <- wt.65$wave
w.arr[66, , ] <- wt.66$wave
w.arr[67, , ] <- wt.67$wave
w.arr[68, , ] <- wt.68$wave
w.arr[69, , ] <- wt.69$wave
w.arr[70, , ] <- wt.70$wave
w.arr[71, , ] <- wt.71$wave
w.arr[72, , ] <- wt.72$wave
w.arr[73, , ] <- wt.73$wave
w.arr[74, , ] <- wt.74$wave
w.arr[75, , ] <- wt.75$wave
w.arr[76, , ] <- wt.76$wave
w.arr[77, , ] <- wt.77$wave
w.arr[78, , ] <- wt.78$wave
w.arr[79, , ] <- wt.79$wave
w.arr[80, , ] <- wt.80$wave
w.arr[81, , ] <- wt.81$wave
w.arr[82, , ] <- wt.82$wave
w.arr[83, , ] <- wt.83$wave
w.arr[84, , ] <- wt.84$wave
w.arr[85, , ] <- wt.85$wave
w.arr[86, , ] <- wt.86$wave
w.arr[87, , ] <- wt.87$wave
w.arr[88, , ] <- wt.88$wave
w.arr[89, , ] <- wt.89$wave
w.arr[90, , ] <- wt.90$wave
w.arr[91, , ] <- wt.91$wave
w.arr[92, , ] <- wt.92$wave
w.arr[93, , ] <- wt.93$wave
w.arr[94, , ] <- wt.94$wave
w.arr[95, , ] <- wt.95$wave
w.arr[96, , ] <- wt.96$wave
w.arr[97, , ] <- wt.97$wave
w.arr[98, , ] <- wt.98$wave
w.arr[99, , ] <- wt.99$wave
w.arr[100, , ] <- wt.100$wave
w.arr[101, , ] <- wt.101$wave
w.arr[102, , ] <- wt.102$wave
w.arr[103, , ] <- wt.103$wave
w.arr[104, , ] <- wt.104$wave
w.arr[105, , ] <- wt.105$wave
w.arr[106, , ] <- wt.106$wave
w.arr[107, , ] <- wt.107$wave
w.arr[108, , ] <- wt.108$wave
w.arr[109, , ] <- wt.109$wave
w.arr[110, , ] <- wt.110$wave
w.arr[111, , ] <- wt.111$wave
w.arr[112, , ] <- wt.112$wave
w.arr[113, , ] <- wt.113$wave
w.arr[114, , ] <- wt.114$wave
w.arr[115, , ] <- wt.115$wave
w.arr[116, , ] <- wt.116$wave
w.arr[117, , ] <- wt.117$wave
w.arr[118, , ] <- wt.118$wave
w.arr[119, , ] <- wt.119$wave
w.arr[120, , ] <- wt.120$wave
w.arr[121, , ] <- wt.121$wave
w.arr[122, , ] <- wt.122$wave
w.arr[123, , ] <- wt.123$wave
w.arr[124, , ] <- wt.124$wave
w.arr[125, , ] <- wt.125$wave
w.arr[126, , ] <- wt.126$wave
w.arr[127, , ] <- wt.127$wave
w.arr[128, , ] <- wt.128$wave
w.arr[129, , ] <- wt.129$wave
w.arr[130, , ] <- wt.130$wave
w.arr[131, , ] <- wt.131$wave
w.arr[132, , ] <- wt.132$wave
w.arr[133, , ] <- wt.133$wave
w.arr[134, , ] <- wt.134$wave
w.arr[135, , ] <- wt.135$wave
w.arr[136, , ] <- wt.136$wave
w.arr[137, , ] <- wt.137$wave
w.arr[138, , ] <- wt.138$wave
w.arr[139, , ] <- wt.139$wave
w.arr[140, , ] <- wt.140$wave
w.arr[141, , ] <- wt.141$wave
w.arr[142, , ] <- wt.142$wave
w.arr[143, , ] <- wt.143$wave
w.arr[144, , ] <- wt.144$wave
w.arr[145, , ] <- wt.145$wave
w.arr[146, , ] <- wt.146$wave
w.arr[147, , ] <- wt.147$wave
w.arr[148, , ] <- wt.148$wave
w.arr[149, , ] <- wt.149$wave
w.arr[150, , ] <- wt.150$wave
w.arr[151, , ] <- wt.151$wave
w.arr[152, , ] <- wt.152$wave
w.arr[153, , ] <- wt.153$wave
w.arr[154, , ] <- wt.154$wave
w.arr[155, , ] <- wt.155$wave
w.arr[156, , ] <- wt.156$wave
w.arr[157, , ] <- wt.157$wave
w.arr[158, , ] <- wt.158$wave
w.arr[159, , ] <- wt.159$wave
w.arr[160, , ] <- wt.160$wave
w.arr[161, , ] <- wt.161$wave
w.arr[162, , ] <- wt.162$wave
w.arr[163, , ] <- wt.163$wave
w.arr[164, , ] <- wt.164$wave
w.arr[165, , ] <- wt.165$wave
w.arr[166, , ] <- wt.166$wave
w.arr[167, , ] <- wt.167$wave
w.arr[168, , ] <- wt.168$wave
w.arr[169, , ] <- wt.169$wave
w.arr[170, , ] <- wt.170$wave
w.arr[171, , ] <- wt.171$wave
w.arr[172, , ] <- wt.172$wave
w.arr[173, , ] <- wt.173$wave
w.arr[174, , ] <- wt.174$wave
w.arr[175, , ] <- wt.175$wave
w.arr[176, , ] <- wt.176$wave
w.arr[177, , ] <- wt.177$wave
w.arr[178, , ] <- wt.178$wave
w.arr[179, , ] <- wt.179$wave
w.arr[180, , ] <- wt.180$wave
w.arr[181, , ] <- wt.181$wave
w.arr[182, , ] <- wt.182$wave
w.arr[183, , ] <- wt.183$wave
w.arr[184, , ] <- wt.184$wave
w.arr[185, , ] <- wt.185$wave
w.arr[186, , ] <- wt.186$wave
w.arr[187, , ] <- wt.187$wave
w.arr[188, , ] <- wt.188$wave
w.arr[189, , ] <- wt.189$wave
w.arr[190, , ] <- wt.190$wave
w.arr[191, , ] <- wt.191$wave
w.arr[192, , ] <- wt.192$wave
w.arr[193, , ] <- wt.193$wave
w.arr[194, , ] <- wt.194$wave
w.arr[195, , ] <- wt.195$wave
w.arr[196, , ] <- wt.196$wave
w.arr[197, , ] <- wt.197$wave
w.arr[198, , ] <- wt.198$wave
w.arr[199, , ] <- wt.199$wave
w.arr[200, , ] <- wt.200$wave
w.arr[201, , ] <- wt.201$wave
w.arr[202, , ] <- wt.202$wave
w.arr[203, , ] <- wt.203$wave
w.arr[204, , ] <- wt.204$wave
w.arr[205, , ] <- wt.205$wave
w.arr[206, , ] <- wt.206$wave
w.arr[207, , ] <- wt.207$wave
w.arr[208, , ] <- wt.208$wave
w.arr[209, , ] <- wt.209$wave
w.arr[210, , ] <- wt.210$wave
w.arr[211, , ] <- wt.211$wave
w.arr[212, , ] <- wt.212$wave
w.arr[213, , ] <- wt.213$wave
w.arr[214, , ] <- wt.214$wave
w.arr[215, , ] <- wt.215$wave
w.arr[216, , ] <- wt.216$wave
w.arr[217, , ] <- wt.217$wave
w.arr[218, , ] <- wt.218$wave
w.arr[219, , ] <- wt.219$wave
w.arr[220, , ] <- wt.220$wave
w.arr[221, , ] <- wt.221$wave
w.arr[222, , ] <- wt.222$wave
w.arr[223, , ] <- wt.223$wave
w.arr[224, , ] <- wt.224$wave
w.arr[225, , ] <- wt.225$wave
w.arr[226, , ] <- wt.226$wave
w.arr[227, , ] <- wt.227$wave
w.arr[228, , ] <- wt.228$wave
w.arr[229, , ] <- wt.229$wave



