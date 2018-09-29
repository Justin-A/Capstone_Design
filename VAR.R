Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")
setwd("~/Desktop/Justin/2018-1/Capstone/Data/Naver/")

pw <- read.csv("박원순_Naver_2014.csv") ; head(pw)
ss <- read.csv("서울시장_Naver_2014.csv") ; head(ss)
jm <- read.csv("정몽준_Naver_2014.csv") ; head(jm)

# pw1 <- pw[grep("박", pw$contents), ]
# pw2 <- pw[grep("원순", pw$contents), ]
# pw3 <- pw[grep("원숭", pw$contents), ]
# pw4 <- pw[grep("시장", pw$contents), ]
# 
# pw_new <- rbind(pw1, pw2, pw3, pw4) ; dim(pw_new)
# pw_new <- unique(pw_new) ; dim(pw_new)
# 
# #
# jm1 <- jm[grep("정", jm$contents), ]
# jm2 <- jm[grep("몽준", jm$contents), ]
# jm3 <- jm[grep("의원", jm$contents), ]
# jm4 <- jm[grep("막내", jm$contents), ]
# jm5 <- jm[grep("아들", jm$contents), ]
# jm6 <- jm[grep("회장", jm$contents), ]
# 
# jm_new <- rbind(jm1, jm2, jm3, jm4, jm5, jm6) ;  dim(jm_new)
# jm_new <- unique(jm_new) ; dim(jm_new)
# 
# #
# # 박원순
# data_pw <- pw_new[,c(-1, -3)]
data_pw <- pw
data_pw$sympathyCount <- as.numeric(data_pw$sympathyCount)
data_pw$antipathyCount <- as.numeric(data_pw$antipathyCount)
hist(data_pw$sympathyCount) ; table(data_pw$sympathyCount)
hist(data_pw$antipathyCount) ; table(data_pw$antipathyCount)
data_pw$rate <- data_pw$sympathyCount / (data_pw$sympathyCount + data_pw$antipathyCount)

# 정몽준
# data_jm <- jm_new[,c(-1, -3)]
data_jm <- jm
data_jm$sympathyCount <- as.numeric(data_jm$sympathyCount)
data_jm$antipathyCount <- as.numeric(data_jm$antipathyCount)
hist(data_jm$sympathyCount) ; table(data_jm$sympathyCount)
hist(data_jm$antipathyCount) ; table(data_jm$antipathyCount)
data_jm$rate <- data_jm$sympathyCount / (data_jm$sympathyCount + data_jm$antipathyCount)

# 박원순_날짜별 (pw_total)
data_pw$count <- 1
k1 <- aggregate(data_pw$count, by = list(data_pw$regTime), sum)
colnames(k1) <- c("regTime", "PW_Count")
k1$regTime <- as.Date(k1$regTime, format = "%d/%m/%Y")
pw_count <- k1[c(order(k1$regTime)),] ; pw_count <- pw_count[1:61,] ; pw_count

k2 <- aggregate(data_pw$sympathyCount, by = list(data_pw$regTime), sum)
colnames(k2) <- c("regTime", "Agree")
k2$regTime <- as.Date(k2$regTime, format = "%d/%m/%Y")
pw_agree <- k2[c(order(k2$regTime)), ] ; pw_agree <- pw_agree[1:61,] ; pw_agree

k3 <- aggregate(data_pw$sympathyCount, by = list(data_pw$regTime), mean)
colnames(k3) <- c("regTime", "Agree")
k3$regTime <- as.Date(k3$regTime, format = "%d/%m/%Y")
pw_agree_r <- k3[c(order(k3$regTime)), ] ; pw_agree_r <- pw_agree_r[1:61,] ; pw_agree_r

k4 <- aggregate(data_pw$antipathyCount, by = list(data_pw$regTime), sum)
colnames(k4) <- c("regTime", "Disagree")
k4$regTime <- as.Date(k4$regTime, format = "%d/%m/%Y")
pw_disagree <- k4[c(order(k4$regTime)), ] ; pw_disagree <- pw_disagree[1:61,] ; pw_disagree

k5 <- aggregate(data_pw$antipathyCount, by = list(data_pw$regTime), mean)
colnames(k5) <- c("regTime", "Agree")
k5$regTime <- as.Date(k5$regTime, format = "%d/%m/%Y")
pw_disagree_r <- k5[c(order(k5$regTime)), ] ; pw_disagree_r <- pw_disagree_r[1:61,] ; pw_disagree_r

k6 <- aggregate(data_pw$rate, by = list(data_pw$regTime), mean)
colnames(k6) <- c("regTime", "Rate")
k6$regTime <- as.Date(k6$regTime, format = "%d/%m/%Y")
pw_rate <- k6[c(order(k6$regTime)),] ; pw_rate <- pw_rate[1:61,] ; pw_rate

pw_total <- data.frame(pw_count, pw_agree$Agree, pw_agree_r$Agree, 
                       pw_disagree$Disagree, pw_disagree_r$Agree, pw_rate$Rate)
colnames(pw_total) <- c("Date", "Count", "Agree_sum", "Agree_rate", "Disagree_sum", "Disagree_rate", "Rate")

# 정몽준_날짜별 (jm_total)
data_pw <- data_jm
data_pw$count <- 1
k1 <- aggregate(data_pw$count, by = list(data_pw$regTime), sum)
colnames(k1) <- c("regTime", "PW_Count")
k1$regTime <- as.Date(k1$regTime, format = "%d/%m/%Y")
pw_count <- k1[c(order(k1$regTime)),] ; pw_count <- pw_count[1:61,] ; pw_count

k2 <- aggregate(data_pw$sympathyCount, by = list(data_pw$regTime), sum)
colnames(k2) <- c("regTime", "Agree")
k2$regTime <- as.Date(k2$regTime, format = "%d/%m/%Y")
pw_agree <- k2[c(order(k2$regTime)), ] ; pw_agree <- pw_agree[1:61,] ; pw_agree

k3 <- aggregate(data_pw$sympathyCount, by = list(data_pw$regTime), mean)
colnames(k3) <- c("regTime", "Agree")
k3$regTime <- as.Date(k3$regTime, format = "%d/%m/%Y")
pw_agree_r <- k3[c(order(k3$regTime)), ] ; pw_agree_r <- pw_agree_r[1:61,] ; pw_agree_r

k4 <- aggregate(data_pw$antipathyCount, by = list(data_pw$regTime), sum)
colnames(k4) <- c("regTime", "Disagree")
k4$regTime <- as.Date(k4$regTime, format = "%d/%m/%Y")
pw_disagree <- k4[c(order(k4$regTime)), ] ; pw_disagree <- pw_disagree[1:61,] ; pw_disagree

k5 <- aggregate(data_pw$antipathyCount, by = list(data_pw$regTime), mean)
colnames(k5) <- c("regTime", "Agree")
k5$regTime <- as.Date(k5$regTime, format = "%d/%m/%Y")
pw_disagree_r <- k5[c(order(k5$regTime)), ] ; pw_disagree_r <- pw_disagree_r[1:61,] ; pw_disagree_r

k6 <- aggregate(data_pw$rate, by = list(data_pw$regTime), mean)
colnames(k6) <- c("regTime", "Rate")
k6$regTime <- as.Date(k6$regTime, format = "%d/%m/%Y")
pw_rate <- k6[c(order(k6$regTime)),] ; pw_rate <- pw_rate[1:61,] ; pw_rate

jm_total <- data.frame(pw_count, pw_agree$Agree, pw_agree_r$Agree, 
                       pw_disagree$Disagree, pw_disagree_r$Agree, pw_rate$Rate)
colnames(jm_total) <- c("Date", "Count", "Agree_sum", "Agree_rate", "Disagree_sum", "Disagree_rate", "Rate")

#
pw_total$Rate_N <- 1-pw_total$Rate
jm_total$Rate_N <- 1-jm_total$Rate

# ARIMA
data <- pw_total[,8]
data <- jm_total[,8]
data <- ts(data)

m <- lm(coredata(data) ~ index(data))
apts.diff <- diff(log(data))

data.arima <- auto.arima(data)
summary(data.arima)
model1 <- arima(data, order = c(2, 0, 0)) ; model1
model_forecast <- forecast(model1, h = 4) ; model_forecast
plot(model_forecast)

# VAR
# df <- read.csv("2014_Research.csv")
# df <- df[, c(1,2,3)]
# df
# library(Amelia)
# imp.a = amelia(df, m = 5)
# df1 = imp.a$imputations[[2]] ; df1 ; summary(df1)
# df1$무응답 <- 100 - df1$박원순 - df1$정몽준
# summary(df1)
# write.csv(df1, "2014_Research_Predict.csv")

setwd("~/Desktop/Justin/2018-1/Capstone/Data")
df1 <- read.csv("2014_Research_Predict.csv")
df1 <- df1[,-1] ; colnames(df1) <- c("Date","박원순","정몽준","무응답")

head(pw_total)
pw_total$support_p <- df1$박원순
jm_total$support_j <- df1$정몽준

total_data <- merge(pw_total, jm_total, by = c("Date"))
total_data 

#VAR_TOTAL
colnames(total_data)
total_data2 <- total_data[,c(9,17,2,10)] #Count
total_data2 <- total_data[,c(9,17,3,11)] #Like
total_data2 <- total_data[,c(9,17,5,13)] #Dislike

#total_data2 <- total_data[, c(-7, -8, -15, -16)]
library(vars)
res <- VAR(total_data2,
           lag.max = 7,
           ic = "SC",
           type = "none")
prediction <- predict(res, n.ahead = 4, ci = 0.95) ; prediction
plot(prediction)

# install.packages("vars") ; library(vars)
res <- VAR(pw_total[ ,c(2:6, 9)],  # prod, rw 변수 이용
           lag.max = 7,       # 제한될 최대 시차 = 3
           ic = "SC",         # Schwarz의 정보기준을 이용하여 자동으로 시차 결정
           type = "none")     # 상수항, 추세항, 제거 모형 지정
prediction <- predict(res, n.ahead = 4, ci = 0.95) ; prediction 
plot(prediction)

res <- VAR(jm_total[ ,2:6, 9],  # prod, rw 변수 이용
           lag.max = 7,       # 제한될 최대 시차 = 3
           ic = "SC",         # Schwarz의 정보기준을 이용하여 자동으로 시차 결정
           type = "none")     # 상수항, 추세항, 제거 모형 지정
prediction <- predict(res, n.ahead = 4, ci = 0.95) ; prediction
plot(prediction)

#AIC, HQ, SC, FPE