#檢視資料
d=read.csv(file="C:/Users/user/Desktop/迴歸分析/期末data/Housing.csv",header = TRUE)
names(d)
#刪除缺失值
d=d[complete.cases(d), ]
length(d$price)
#歸檔
options(max.print = 100000)
write.csv(d,file="C:/Users/user/Desktop/迴歸分析/house_prediction.csv",)
names(d)
head(d)
summary(d[,sapply(d,is.numeric)])
median(d$area)
#設定變數和模型
d["mainroad"]=as.integer(d$mainroad=="yes")
d["guestroom"]=as.integer(d$guestroom=="yes")
d["basement"]=as.integer(d$basement=="yes")
d["hotwaterheating"]=as.integer(d$hotwaterheating=="yes")
d["airconditioning"]=as.integer(d$airconditioning=="yes")
d["semi_furnished"]=as.integer(d$furnishingstatus=="semi-furnished")
d["furnished"]=as.integer(d$furnishingstatus=="furnished")
#拆分成訓練集d1 和 測試集 d2 
set.seed((100))
index=sample(seq_len(nrow(d)), size = 0.8 * nrow(d))
d1=d[index,]
d2=d[-index,]
head(d)
names(d)
#配模
y=d1$price
z=d2$price
l=lm(price ~area   +bedrooms  +bathrooms +mainroad +guestroom+stories+basement+hotwaterheating+airconditioning
     +parking+semi_furnished+furnished,d1)
sm=summary(l)
anova(l)
{
res=l$res#殘差
par(mfcol=c(2,2))
plot(l)#殘差圖
par(mfcol=c(1,1))
}

# shapiro常態 durbinwastson獨立性 ncvtest變異數齊一性
library(car)
shapiro.test(res)
durbinWatsonTest(l)
ncvTest(l)

#boxcox轉換
{ library(MASS)
bc=boxcox(l)
b=bc$x[which.max(bc$y)]
y1=(y**0.02-1)/0.02
z2=(z**0.02-1)/0.02
}

#轉換後再測試一次
l1=lm(y1 ~area   +bedrooms  +bathrooms +mainroad +guestroom+stories+basement+hotwaterheating+airconditioning
      +parking+semi_furnished+furnished,d1)
sm1=summary(l1)
anova(l1)
{
  res=l1$res#殘差
  par(mfcol=c(2,2))
  plot(l1)#殘差圖
  par(mfcol=c(1,1))
}

#mean有曲線，但是變異數應該一致 shapiro durbinwastson ncvtest
library(car)
shapiro.test(res)
durbinWatsonTest(l1)
ncvTest(l1)
#做到這邊的話要記得自己已經轉換過y y1和y的意義已經不一樣 解釋也不一樣
#----------------------------------------------------------

#outlier函數
outlierTest(l)
influencePlot(l1)
dfbetas(l)
which(dfbetas(l)>2)
dffits(l)
which(as.vector(dffits(l))>2)
covratio(l)
which(abs(as.vector(covratio(l))-1)>3*7/n)
d=d[-c(29,14,3,144,1),] #刪除離群值

#檢驗共線性 未轉換檢定l 轉換後檢定 l1
library(car)
vif(l1)
#選模
m1=step(l1)#逐步
m2=step(l1,selection="backward")
m3=step(l1,selection="forward")
print("逐步")
m1
cat("backward")
m2
cat("forward")
m3

#預測
library(nnet)
#生成優質的模型當作訓練集
train.result=lm(y1 ~bedrooms+guestroom  +parking +hotwaterheating +mainroad+furnished+semi_furnished+basement
                +bathrooms+stories+airconditioning+area,d1)
sm2=summary(train.result)

vif(train.result) #再次查看貢獻性

pred=predict(train.result,newdata=d1)#訓練與真值的結果

head(pred)
head(z2)
test_pred=predict(train.result, newdata=d2)#測試與真值的結果

#透過圖做預測的說明

plot(z2,test_pred,xlab="真值",ylab="測試結果",main="預測結果圖")
yp=test_pred
yt=((yp*0.02)+1)**50
abline(0,1,col="red")

plot(z,yt,xlab="轉換前真值",ylab="轉換後測試結果",main="預測結果圖(轉換後)")
abline(0,1,col="green")
sum(d$price)/sum(d$area)

coefficients(sm2)[9,1]

