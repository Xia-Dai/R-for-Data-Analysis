#设置工作目录
# office
setwd("E:\\R语言\\R工作目录")
getwd()
#home
setwd("F:/R工作目录")
getwd()

options(stringsAsFactors = FALSE)
options(digits = 4)

library(ROracle)

drv <- dbDriver("Oracle")
con <- dbConnect(drv, username = "ZBA_CZC", password = "w8XnYH", dbname = "dssdw1_132.35.224.1_www.itsmacct-fort1.unicom.local_22051")


sql1 <- "SELECT * FROM TEMP_DX_TELECOM_USER_SEQ81"
dtest1 <- dbGetQuery(con, sql1)
write.table(dtest1, file = "TEMP_DX_TELECOM_USER_SEQ81.txt", sep = ",", quote = TRUE, na = "", row.names = TRUE)

user <- read.table("TEMP_DX_TELECOM_USER_SEQ81.txt",sep = ",",header = TRUE)
dim(user)
names(user)
user <- user[order(user[,1],decreasing = FALSE),]
user$USER_CODE[1]
n <-length(user$USER_CODE)

#全7周
sql_7 <- "SELECT * FROM TEMP_DX_TELECOM_DATA_SEQ81"
dtest_7 <- dbGetQuery(con, sql_7)
filename_7 <- "TEMP_DX_TELECOM_DATA_SEQ81.txt"
write.table(dtest_7, file = filename_7, sep = ",", quote = TRUE, na = "", row.names = TRUE)

seq_7 <- read.table("TEMP_DX_TELECOM_DATA_SEQ81.txt",sep = ",",header = TRUE)
names(seq_7)
dim(seq_7)

uc_7 <- as.factor(seq_7$USER_CODE)
ts_7 <- as.numeric(seq_7$USE_SEQ)
vd_7 <- as.factor(seq_7$VIDEO_TYPE)
cnt_7 <- as.numeric(seq_7$USE_COUNT)
all_7 <- data.frame(uc_7,ts_7,vd_7,cnt_7)
str(all_7)

#训练集前6周
sql <- "SELECT * FROM TEMP_DX_TELECOM_DATA_SEQ82"
dtest <- dbGetQuery(con, sql)
filename <- "TEMP_DX_TELECOM_DATA_SEQ82.txt"
write.table(dtest, file = filename, sep = ",", quote = TRUE, na = "", row.names = TRUE)
seq <- read.table("TEMP_DX_TELECOM_DATA_SEQ82.txt",sep = ",",header = TRUE)
names(seq)
dim(seq)

uc <- as.factor(seq$USER_CODE)
ts <- as.numeric(seq$USE_SEQ)
vd <- as.factor(seq$VIDEO_TYPE)
cnt <- as.numeric(seq$USE_COUNT)
Nall <- data.frame(uc,ts,vd,cnt)
str(Nall)

#extract user data all 7 weeks

vd1_7 <- all_7[which(all_7$uc_7 =="366095" ),] #
vd1_7
y_max_7 <- max(vd1_7$cnt_7)

plot(vd1_7$ts_7,vd1_7$cnt_7,xlab = "time_7seq",ylab = "vd1_count",xlim = c(0,49),ylim = c(0,y_max_7))

# extract user data in previous 6 weeks

vd1 <- Nall[which(Nall$uc =="366095" ),] #
y_max <- max(vd1$cnt)
vd1
plot(vd1$ts,vd1$cnt,xlab = "time_seq",ylab = "vd1_count",xlim = c(0,49),ylim = c(0,y_max))

#拟合图形
library(KernSmooth)
??KernSmooth
require(graphics)
h=dpill(vd1$ts,vd1$cnt,blockmax = 5, divisor = 25,trim=0.05,proptrun = 0.01)

par(mfrow =c(1,2))
x_max <-length(vd1$ts)
plot(vd1$ts,vd1$cnt,pch=1, xlab = "time_seq",ylab = "vd1_count",xlim = c(0,x_max),ylim = c(0,y_max),col=2)

fit3 <- ksmooth(vd1$ts,vd1$cnt, "normal", bandwidth = 0.5,n.points = 100L) #锋利、尖，bd越大越平滑
lines(fit3,col=4) #拟合图像
xw <- fit3$x
yc <- fit3$y
plot(xw, yc, pch=10,   ylab="vd1_count(times)", xlab="week_day", bty="l",col=1) 

#建模惩罚样条估计
library(SemiPar)

ct<-vd1$cnt
td <-vd1$ts
ln <- length(vd1$uc)
knots.sem1 <- seq(1,ln,by=1) #分段
knots.sem2 <- seq(1,ln,by=1) #分段
knots.sem3 <- seq(1,ln,by=1) #分段
semi.fit1 <- spm(yc ~ f(xw,knots=knots.sem1,spar=0.17)) # df 自由度 spar 光滑参数 knots 分段间隔 ,df=30,spar=1,basis="trunc.poly"
semi.fit2 <- spm(yc ~ f(xw,knots=knots.sem2,spar=0.18)) # 上周值多时选择０.１６５，值少时选择小值０.１４８
semi.fit3 <- spm(ct~ f(td))

summary(semi.fit1)
summary(semi.fit2)
summary(semi.fit3)

ymax<-max(fitted.values(semi.fit1))
par(mfrow=c(2,3))
plot(semi.fit1,xlab="fit_wd1",ylab ="fit_vdcnt1",col=3,ylim =c(0,ymax))
plot(semi.fit2,xlab="fit_wd2",ylab ="fit_vdcnt2",col=2,ylim =c(0,ymax))
plot(semi.fit3,xlab="fit_wd2",ylab ="fit_vdcnt2",col=2,ylim =c(0,ymax))

plot(xw,resid(semi.fit1),xlab="xw",ylab ="res1",col=3)
plot(xw,resid(semi.fit2),xlab="xw",ylab ="res2",col=2)
plot(td,resid(semi.fit3),xlab="ts",ylab ="res2",col=2)

par(mfrow=c(1,3))
qqnorm(resid(semi.fit1),xlab = "fit1_res")
qqline(resid(semi.fit1))
qqnorm(resid(semi.fit2),xlab = "fit2_res")
qqline(resid(semi.fit2))
qqnorm(resid(semi.fit3),xlab = "fit3_res")
qqline(resid(semi.fit3))

next_w <- data.frame(xw=seq(43,49,by=1))
next_w1 <- data.frame(td=seq(43,49,by=1))
pred1<-predict(semi.fit1,newdata = next_w,se=TRUE,interval ="prediction",level=0.95)
pred2<-predict(semi.fit2,newdata = next_w,se=TRUE,interval ="prediction",level=0.95)
pred3<-predict(semi.fit3,newdata = next_w1,se=TRUE,interval ="prediction",level=0.95)

par(mfrow=c(2,3))
pred1$fit[pred1$fit<0] <-0
pred2$fit[pred2$fit<0] <-0
pred3$fit[pred3$fit<0] <-0

plot(pred1$fit,xlab = "pred_1")
plot(pred2$fit,xlab = "pred_2")
plot(pred3$fit,xlab = "pred_3")

plot(pred1$se,xlab = "se_1")
plot(pred2$se,xlab = "se_2")
plot(pred3$se,xlab = "se_3")

#结果比较
par(mfrow=c(1,4))
pred1$fit[pred1$fit<0] <-0
pred2$fit[pred2$fit<0] <-0
pred3$fit[pred3$fit<0] <-0

plot(vd1_7$ts_7,vd1_7$cnt_7,xlab = "time_7seq",ylab = "vd1_count",xlim = c(43,49),ylim = c(0,y_max_7))
y_prd1 <- max(round(pred1$fit))
plot(round(pred1$fit),xlab = "pred_1",ylim = c(0,y_prd1),col=1)
y_prd2 <- max(round(pred2$fit))
plot(round(pred2$fit),xlab = "pred_2",ylim = c(0,y_prd2),col=2)
y_prd3 <- max(round(pred3$fit))
plot(pred3$fit,xlab = "pred_3",ylim = c(0,y_prd3),col=3)


vd1_7[which(vd1_7$ts ==seq(43,49,by=1)),c(2,4)]
round(pred1$fit)
round(pred2$fit)
round(pred3$fit)

floor(pred1$fit)
floor(pred2$fit)
floor(pred3$fit)
sum(abs(vd1_7[which(vd1_7$ts ==seq(43,49,by=1)),4] -round(pred1$fit)))
sum(abs(vd1_7[which(vd1_7$ts ==seq(43,49,by=1)),4]- round(pred2$fit)))
#