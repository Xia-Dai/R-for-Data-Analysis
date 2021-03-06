#设置工作目录
# office
setwd("E:\\R语言\\R工作目录")
getwd()
#home
setwd("F:/R工作目录")
getwd()

options(stringsAsFactors = FALSE)

#数据库连接导数
#library(ROracle)

#drv <- dbDriver("Oracle")
#con <- dbConnect(drv, username = "ZBA_CZC", password = "w8XnYH", dbname = "dssdw1_132.35.224.1_www.itsmacct-fort1.unicom.local_22051")

#单用户
#sql1 <- "SELECT * FROM TEMP_DX_TELECOM_USER_SEQ21"
#dtest1 <- dbGetQuery(con, sql1)
#write.table(dtest1, file = "TEMP_DX_TELECOM_USER_SEQ21.txt", sep = ",", quote = TRUE, na = "", row.names = TRUE)

user <- read.table("TEMP_DX_TELECOM_USER_SEQ71.txt",sep = ",",header = TRUE)
dim(user)
names(user)

user_p0 <- user[which(user$USER_DAYS >= 0 & user$USER_DAYS < 7),]
user_p1 <- user[which(user$USER_DAYS >= 7 & user$USER_DAYS < 10),]
user_p2 <- user[which(user$USER_DAYS >= 10),]

dim(user_p0)
dim(user_p1)
dim(user_p2)

user <- user[order(user[,1],decreasing = FALSE),]
as.factor(user$USER_CODE)[1]
n <-length(user$USER_CODE)

#数据预测
sql <- "SELECT * FROM TEMP_DX_TELECOM_DATA_SEQ71"
dtest <- dbGetQuery(con, sql)
filename <- "TEMP_DX_TELECOM_DATA_SEQ71.txt"
write.table(dtest, file = filename, sep = ",", quote = TRUE, na = "", row.names = TRUE)
seq <- read.table("TEMP_DX_TELECOM_DATA_SEQ71.txt",sep = ",",header = TRUE)
names(seq)
dim(seq)

uc <- as.factor(seq$USER_CODE)
ts <- as.numeric(seq$USE_SEQ)
vd <- as.factor(seq$VIDEO_TYPE)
cnt <- as.numeric(seq$USE_COUNT)
options(digits = 4)

Nall <- data.frame(uc,ts,vd,cnt)
str(Nall)

#初始化
rm(preds_vd)
gc()
preds_vd <- data.frame(use_code= 1,xw= 0,use_cnt= 0 )
preds_vd
ord_i <- data.frame(id = seq(1,n,by =1) )

#getData <- function(i) 
for (i in 1:n)  {
  
  vd1 <- Nall[which(Nall$uc == as.factor(user$USER_CODE)[i] ),] # 分别取第i个用户
  y_max <- max(vd1$cnt)
  ln <- length(vd1$uc)
  
  #拟合图形
  library(KernSmooth)
  require(graphics)
  
  fit3 <- ksmooth(vd1$ts,vd1$cnt, "normal", bandwidth = 0.5,n.points = 100L) #锋利、尖，bd越大越平滑
  xw <- fit3$x
  yc <- fit3$y
  
  #建模惩罚样条估计
  library(SemiPar)

  knots.sem2 <- seq(1,ln,by=1) #分段
  semi.fit2 <- spm(yc ~ f(xw,knots=knots.sem2,spar=0.18)) # 上周值多时选择０.１６５，值少时选择小值０.１４８
  
  #预测
  next_w <- data.frame(xw=seq(50,56,by=1))
  pred2<-predict(semi.fit2,newdata = next_w,se=TRUE,interval ="prediction",level=0.95)
  
  #结果整理
  pred2$fit[pred2$fit<0] <-0
  fit <-round(pred2$fit)
  uc_pred <- rep(user$USER_CODE[i],7)
  preds <- data.frame(use_code = uc_pred,use_day = next_w,use_cnt = fit)
  
  preds_vd <- rbind(preds_vd,preds)

  rm(preds)
  gc()
  print(i)
  i<- i+1
}

preds_vd
dim(preds_vd)
names(preds_vd)

#lapply(ord_i$id,getData)
#导出数据
write.table(preds_vd, file = "Predict for v7.txt", sep = ",", quote = TRUE, na = "", row.names = TRUE)


