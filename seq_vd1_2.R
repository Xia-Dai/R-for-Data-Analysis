#设置工作目录
setwd("E:\\R语言\\R工作目录")
getwd()


options(stringsAsFactors = FALSE)

library(ROracle)

drv <- dbDriver("Oracle")
con <- dbConnect(drv, username = "ZBA_CZC", password = "w8XnYH", dbname = "dssdw1_132.35.224.1_www.itsmacct-fort1.unicom.local_22051")


#sql1 <- "SELECT * FROM TEMP_DX_TELECOM_DATA_USER"
#dtest1 <- dbGetQuery(con, sql1)
#write.table(dtest1, file = "TEMP_DX_TELECOM_DATA_USER.txt", sep = ",", quote = TRUE, na = "", row.names = TRUE)

user <- read.table("TEMP_DX_TELECOM_DATA_USER.txt",sep = ",",header = TRUE)
dim(user)
names(user)
user <- user[order(user[,2],decreasing = FALSE),]
user$user_code[1,2]

sql <- "SELECT * FROM TEMP_DX_TELECOM_DATA_SEQ11"
dtest <- dbGetQuery(con, sql)
filename <- "TEMP_DX_TELECOM_DATA_SEQ11.txt"
write.table(dtest, file = filename, sep = ",", quote = TRUE, na = "", row.names = TRUE)
seq <- read.table("TEMP_DX_TELECOM_DATA_SEQ11.txt",sep = ",",header = TRUE)
names(seq)
dim(seq)

uc <- as.factor(seq$USER_CODE)
ts <- as.numeric(seq$USE_SEQ)
vd <- as.factor(seq$VIDEO_TYPE)
cnt <- as.numeric(seq$USE_COUNT)
options(digits = 4)

Nall <- data.frame(uc,ts,vd,cnt)
str(Nall)
vd1 <- Nall[which(Nall$uc =="900686" ),] #
y_max <- max(vd1$cnt)
vd1
plot(vd1$ts,vd1$cnt,xlab = "time_seq",ylab = "vd1_count",xlim = c(0,49),ylim = c(0,y_max))
library(KernSmooth)
??KernSmooth
require(graphics)
h=dpill(vd1$ts,vd1$cnt,blockmax = 5, divisor = 25,trim=0.05,proptrun = 0.01)
?ksmooth
fit <- locpoly(vd1$ts,vd1$cnt,kernel = "normal",bandwidth = h )
fit3 <- ksmooth(vd1$ts,vd1$cnt, "normal", bandwidth = 0.5,n.points = length(vd1$ts)) #锋利、尖，bd越大越平滑
par(mfrow =c(1,2))

plot(vd1$ts,vd1$cnt,pch=1, xlab = "time_seq",ylab = "vd1_count",xlim = c(0,49),ylim = c(0,y_max),col=2)

lines(fit,col = 1)
lines(fit3,col=4) #拟合图像

xw <- fit3$x
yc <- fit3$y

plot(xw, yc, pch=10,   ylab="vd1_count(times)", xlab="week_day", bty="l",col=1) 


library(SemiPar)
??Semipar

ct<-vd1$cnt
td <-vd1$ts
knots.sem1 <- seq(1,49,by=5) #分段
knots.sem2 <- seq(1,49,by=7) #分段
semi.fit1 <- spm(yc ~ f(xw,df=50,spar=1,basis="trunc.poly")) # df 自由度 spar 光滑参数 knots 分段间隔
semi.fit2 <- spm(yc ~ f(xw,df=10,spar=3,basis="trunc.poly"))
semi.fit3 <- spm(ct~ f(td))

summary(semi.fit1)
summary(semi.fit2)
summary(semi.fit3)
par(mfrow = c(1,2))

ymax<-max(yc)
par(mfrow=c(2,3))
plot(semi.fit1,xlab="fit_wd1",ylab ="fit_vdcnt1",col=3,ylim =c(0,ymax))
plot(semi.fit2,xlab="fit_wd2",ylab ="fit_vdcnt2",col=2,ylim =c(0,ymax))
plot(semi.fit3,xlab="fit_wd2",ylab ="fit_vdcnt2",col=2,ylim =c(0,ymax))

plot(xw,resid(semi.fit1),xlab="xw",ylab ="res1",col=3)
plot(xw,resid(semi.fit2),xlab="xw",ylab ="res2",col=2)
plot(td,resid(semi.fit3),xlab="ts",ylab ="res2",col=2)

#plot(xw,fitted.spm(semi.fit1)[,1],xlab="fitd_wd1",ylab ="fitd_vdcnt1",col=5)
#plot(xw,fitted.spm(semi.fit2)[,1],xlab="fitd_wd",ylab ="fitd_vdcnt",col=6)


length(xw)
length(fitted.spm(semi.fit1))
str(fitted.spm(semi.fit1))
fitted.spm(semi.fit3)

next_w <- data.frame(xw=seq(50,56,by=1))
next_w1 <- data.frame(td=seq(50,56,by=1))
pred1<-predict(semi.fit1,newdata = next_w,se=TRUE,interval ="prediction",level=0.95)
pred2<-predict(semi.fit2,newdata = next_w,se=TRUE,interval ="prediction",level=0.95)
pred3<-predict(semi.fit3,newdata = next_w1,se=TRUE,interval ="prediction",level=0.95)

par(mfrow=c(2,3))
#plot(xw, yc, pch=10,   ylab="vd1_count(times)", xlab="week_day", bty="l",col=1) 
#plot(vd1$ts,vd1$cnt,pch=1, xlab = "time_seq",ylab = "vd1_count",xlim = c(0,49),ylim = c(0,ymax),col=2)


plot(pred1$fit,xlab = "pred_1")
plot(pred2$fit,xlab = "pred_2")
plot(pred3$fit,xlab = "pred_3")

plot(pred1$se,xlab = "se_1")
plot(pred2$se,xlab = "se_2")
plot(pred3$se,xlab = "se_3")

#
round(pred1$fit+pred1$se/2)
round(pred2$fit+pred2$se/2)
round(pred3$fit++pred3$se/2)

vd1$cnt




# lm
lm1 <- lm(yc ~ xw, data = fit3)
summary(lm1)
lm2 <-step(lm1,direction = "both")
summary(lm2)
lm2
out<-trunc(predict(lm2,data =data.frame(xw=50),interval = "prediction",level=0.95))
apply(out, 2, mean)
apply(out, 2, sd)

# The loess fit 
?loess
model.loess <- loess(yc ~ xw, data=fit3, span = 0.8,degree = 2,interations= 1) 
model.loess
n <- length(yc)
x.loess <- seq(min(xw), max(xw), length=n) 
y.loess <- predict(model.loess, data.frame(xw=x.loess))
summary(model.loess)

# The lowess fit 
#?lowess
#model.lowess <- lowess(xw,yc, f = 0.5)
#model.lowess
#model.lowess
#*** 默认设置 robust linear tricube加权 ***
#n <- length(yc) 
#x.lowess <- seq(min(xw), max(xw), length=n) 
#y.lowess <- predict(model.lowess, data.frame(xw=x.lowess)) #得到预测值便于比较 

#figure
plot(xw, yc, pch=10,   ylab="vd1_count(times)", xlab="week_day", bty="l") 
lines(x.loess, y.loess,col=1) 
#lines(x.lowess, y.lowess, col=2) 
abline(lm2,col=3)  
legend("topright", c("Loess", "OLS") , lty=c(1,2), bty="n", cex=.8,col=c(1,3)) 

y.loess <- predict(model.loess, data.frame(xw=50),internal ="prediction",level=0.95)
y.loess

?legend


library(spline)
fit1<-lm(y~bs(x,df=))

fit.w <- fit$x
fit.cnt <- fit$y

# 评估模型拟合度

predict(fit,data=data.frame(50),interval ="confidence")

