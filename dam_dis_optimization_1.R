install.packages("mco")
library(mco)
library(scatterplot3d)

## TDD optimization problem:
#TDD <- function(HADR,YJ,IH,AD,GB,YC,HC,NG,OM,MY) {
  TDD <- function(x) {
  y <- numeric(2)
  y[1]<- 0.000356179627785029*x[1]+0.015525028242429*x[2]+0.0106588508617361*x[3]+0.0127328655114269*x[4]+
    0.0668986049045417*x[5]+-0.217357485351951*x[6]+-0.0239225691978742*x[7]+
    0.000751844316629026*x[8]+0.0283723046743154*x[9]+-0.263294877632337*x[10]+
    20.27805228 #Chl-a
  
  y[2] <- 0.00110843494088869*x[1]+0.00289276593746834*x[2]+0.00203132431274714*x[3]+0.00420092548686187*x[4]+
    0.49834498363375*x[5]+-0.03526089064941839*x[6]+-0.00169833399152515*x[7]+0.001576336059478738*x[8]+
    0.00625650907602472*x[9]+0.0396851622406775*x[10]+20.1658065643455  #TU
  

 
    return (y)
  }
  
  y[3] <- (5-(0.000907089062596906*x[1]+-0.0052520044239025*x[2]+-0.00433677153763648*x[3]+0.00554301195481515*x[4]+
                0.0301607802352348*x[5]+-0.0263628300835028*x[6]+-0.00494683803234832*x[7]+0.000270369396276587*x[8]+
                -0.0321251692457338*x[9]+0.0603087253060636*x[10]+4.721232707))^2 #WL
  
r1 <- nsga2(TDD, 10,2,
            generations=200, popsize=1000,
            cprob=0.7, cdist=20,
            mprob=0.2, mdist=20,
            lower.bounds=c(0,0.21,3.11,0.74,0.05,3.72,1.99,10.42,0.51,0.81),
            upper.bounds=c(2369.741935,90.53,119.95,185.41,15.48,21.96,252.52,515.54,46.72,17.79))



plot(r1,xlab="Chl-a", ylab="TU", main="Objectives space")

plot(r1$par, xlab="HADR", ylab="YJ", main="Parameters space")
par(opar)
opar <-par(mfrow=c(1,2))
par<-data.frame(r1$par,r1$pareto.optimal)
val<-data.frame(r1$value)
library(writexl)
write_xlsx(par,path="MOO.xlsx")
write_xlsx(val,path="MOO2.xlsx")

install.packages("plotly")
library(plotly)
library(dplyr)
res <- r1$value %>% 
  as.data.frame() %>% 
  bind_cols(as.data.frame(r1$parameters)) %>% 
  filter(r1$pareto.optimal == TRUE) %>% 
  mutate_all(.funs = function(x){round(x,3)}) %>%
  distinct() %>%  
  
  rename("y[1]" = V1, "y[2]" = V2, "y[3]" = V3,
         "x[1]" = V11, "x[2]" = V21, "x[3]" = V31, "x[4]" = V4, "x[5]" = V5,"x[6]" = V6,"x[7]" = V7,
         "x[8]" = V8,"x[9]" = V9,"x[10]" = V10)
plot_ly(r1, x=~y[1], y=~y[2])


