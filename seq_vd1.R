setwd("F:/R工作目录")
getwd()

options(stringsAsFactors = FALSE)

library(ROracle)

drv <- dbDriver("Oracle")
con <- dbConnect(drv, username = "ZBA_CZC", password = "w8XnYH", dbname = "dssdw1_132.35.224.1_www.itsmacct-fort1.unicom.local_22051")


sql1 <- "SELECT * FROM TEMP_DX_TELECOM_DATA_USER"
dtest1 <- dbGetQuery(con, sql1)
write.table(dtest1, file = "TEMP_DX_TELECOM_DATA_USER.txt", sep = ",", quote = TRUE, na = "", row.names = TRUE)

user <- read.table("TEMP_DX_TELECOM_DATA_USER.txt",sep = ",",header = TRUE)
dim(user)
names(user)
user[,2]
user <- user[order(user[,2],decreasing = FALSE),]
user[,2]

sql <- "SELECT * FROM TEMP_DX_TELECOM_DATA_SEQ1"
dtest <- dbGetQuery(con, sql)
filename <- "TEMP_DX_TELECOM_DATA_SEQ1.txt"

write.table(dtest, file = filename, sep = ",", quote = TRUE, na = "", row.names = TRUE)

seq <- read.table("TEMP_DX_TELECOM_DATA_SEQ1.txt",sep = ",",header = TRUE)
names(seq)
dim(seq)

uc <- as.factor(seq$USER_CODE)
ts <- as.numeric(seq$USE_SEQ)
vd <- as.factor(seq$VIDEO_TYPE)
cnt <- as.numeric(seq$USE_COUNT)

Nall <- data.frame(uc,ts,vd,cnt)
str(Nall)
vd1 <- Nall[which(Nall$uc =="529817" ),] #
ymax <- max(vd1$cnt)
ymax
plot(vd1$ts,vd1$cnt,xlab = "time_seq",ylab = "vd1_count",xlim = c(0,49),ylim = c(0,ymax))
library(KernSmooth)
h=dpill(vd1$ts,vd1$cnt)
fit <- locpoly(vd1$ts,vd1$cnt,kernel = "normal",bandwidth = 2)
lines(fit)
str(fit)

fit.w <- fit$x
fit.cnt <- fit$y

# 评估模型拟合度

predict(fit,data=data.frame(50),interval ="confidence")


