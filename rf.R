install.packages("MASS") 
install.packages("randomForest") 
install.packages("caret")
library(MASS) 
library(randomForest) 
library(caret)
library(readxl)

chl_mod <- read_excel("WHOLE.xlsx")
chl_mod2 <- read_excel("WHOLE.xlsx", sheet = "Sheet2")
chl_mod3 <- read_excel("WHOLE.xlsx", sheet = "Sheet3")
set.seed(10)
rf.fit = randomForest(CH ~  PH+SE+TU+TN+ESC
                      , data=chl_mod, mtry = floor(5/3), ntree = 500, importance = T)

varImpPlot(rf.fit, sort=TRUE, n.var=min(30, nrow(rf.fit$importance)),
           type=NULL, class=NULL, scale=TRUE,
           main=deparse(substitute(rf.fit)))


test_x3 = chl_mod3[c("PH","SE","TU","TN","ESC")]
test_y1 = chl_mod$CH

y_pred3 = predict(rf.fit, test_x3)
qq3<-data.frame(chl_mod3$Date,y_pred3)
write.csv(qq3,"qq3.csv")
plot(test_y,y_pred)
m = lm(y_pred ~ test_y) 
m
abline(m,col='red')
summary(m)

chl_re <- read.csv("qq1.csv")
plot(chl_re$date,chl_re$model, type = 'l',col='red')
par(new = T)
plot(chl_re$date,chl_re$observed, type = 'l', ylim = c(1,500))
lines(y_pred, type = 'l', col = 'blue')

getTree(rf.fit, k=1, labelVar=FALSE)
plot(rf.fit, type="l")
predict(rf.fit, chl_mod2, type="response",
        norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE)
