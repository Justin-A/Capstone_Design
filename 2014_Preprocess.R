Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")
setwd("~/Desktop/Justin/2018-1/Capstone/Data/Naver/")

pw <- read.csv("박원순_Naver_2014.csv") ; head(pw)
ss <- read.csv("서울시장_Naver_2014.csv") ; head(ss)
jm <- read.csv("정몽준_Naver_2014.csv") ; head(jm)

View(pw)
View(jm)
View(ss)

# 박원순
data_pw <- pw[,c(-1, -3)]
data_pw$sympathyCount <- as.numeric(data_pw$sympathyCount)
data_pw$antipathyCount <- as.numeric(data_pw$antipathyCount)
hist(data_pw$sympathyCount) ; table(data_pw$sympathyCount)
hist(data_pw$antipathyCount) ; table(data_pw$antipathyCount)
data_pw$rate <- data_pw$sympathyCount / (data_pw$sympathyCount + data_pw$antipathyCount)

# 정몽준
data_jm <- jm[,c(-1, -3)]
data_jm$sympathyCount <- as.numeric(data_jm$sympathyCount)
data_jm$antipathyCount <- as.numeric(data_jm$antipathyCount)
hist(data_jm$sympathyCount) ; table(data_jm$sympathyCount)
hist(data_jm$antipathyCount) ; table(data_jm$antipathyCount)
data_jm$rate <- data_jm$sympathyCount / (data_jm$sympathyCount + data_jm$antipathyCount)

# 서울시장
data_ss <- ss[,c(-1, -3)]
data_ss$sympathyCount <- as.numeric(data_ss$sympathyCount)
data_ss$antipathyCount <- as.numeric(data_ss$antipathyCount)
hist(data_ss$sympathyCount) ; table(data_ss$sympathyCount)
hist(data_ss$antipathyCount) ; table(data_ss$antipathyCount)
data_ss$rate <- data_ss$sympathyCount / (data_ss$sympathyCount + data_ss$antipathyCount)

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

# 서울시장_날짜별 (jm_total)
data_pw <- data_ss
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

ss_total <- data.frame(pw_count, pw_agree$Agree, pw_agree_r$Agree, 
                       pw_disagree$Disagree, pw_disagree_r$Agree, pw_rate$Rate)
colnames(ss_total) <- c("Date", "Count", "Agree_sum", "Agree_rate", "Disagree_sum", "Disagree_rate", "Rate")

#
head(pw_total) ; head(jm_total) ; head(ss_total)

#
View(pw_total)

##################################################
pw_total$Rate_N <- 1-pw_total$Rate
jm_total$Rate_N <- 1-jm_total$Rate
ss_total$Rate_N <- 1-ss_total$Rate

# PW
data <- pw_total[,4]
data <- ts(data)
plot(data)
acf(data)
pacf(data)
spectrum(data)
acf(diff(log(data)))
pacf(diff(log(data)))

#library(zoo)
m <- lm(coredata(data) ~ index(data))
#apts.eltr <- ts(resid(m), index(apts))
plot(apts.eltr)
plot(diff(log(data)))
apts.diff <- diff(log(data))

#install.packages("forecast") ; library(forecast)
data.arima <- auto.arima(data)
summary(data.arima)
model1 <- arima(data, order = c(0, 0, 2)) ; model1
model_forecast <- forecast(model1, h = 4) ; model_forecast
plot(model_forecast)

# JM
data <- contents_count[,3]
data <- ts(data)
plot(data)
acf(data)
pacf(data)
spectrum(data)
acf(diff(log(data)))
pacf(diff(log(data)))

library(zoo)
m <- lm(coredata(data) ~ index(data))
apts.eltr <- ts(resid(m), index(apts))
plot(apts.eltr)
plot(diff(log(data)))
apts.diff <- diff(log(data))

install.packages("forecast") ; library(forecast)
data.arima <- auto.arima(data)
summary(data.arima)
model1 <- arima(data, order = c(0, 1, 1)) ; model1
model_forecast <- forecast(model1, h = 4) ; model_forecast
plot(model_forecast)

dim(pw[pw$sympathyCount > 100, ])
dim(jm[jm$sympathyCount > 100, ])

#
res <- VAR(pw_total[ ,c(2:6)],  # prod, rw 변수 이용
           lag.max = 7,       # 제한될 최대 시차 = 3
           ic = "AIC",         # Schwarz의 정보기준을 이용하여 자동으로 시차 결정
           type = "none")     # 상수항, 추세항, 제거 모형 지정
summary(res) # prod, rw 변수, 둘다 p-value < 2.2e-16 유의, covariance, correlation
plot(res) # prod, rw 순서

prediction <- predict(res, n.ahead = 4, ci = 0.95)
plot(prediction)


res <- VAR(pw_total[ ,c(2:6)],  # prod, rw 변수 이용
           lag.max = 7,       # 제한될 최대 시차 = 3
           ic = "AIC",         # Schwarz의 정보기준을 이용하여 자동으로 시차 결정
           type = "none")     # 상수항, 추세항, 제거 모형 지정
prediction <- predict(res, n.ahead = 4, ci = 0.95) ; prediction 
plot(prediction)

res <- VAR(jm_total[ ,c(2:6)],  # prod, rw 변수 이용
           lag.max = 7,       # 제한될 최대 시차 = 3
           ic = "AIC",         # Schwarz의 정보기준을 이용하여 자동으로 시차 결정
           type = "none")     # 상수항, 추세항, 제거 모형 지정
prediction <- predict(res, n.ahead = 4, ci = 0.95) ; prediction
plot(prediction)

#AIC, HQ, SC, FPE





#

data <- pw_total[,6]
data <- jm_total[,6]
data <- ts(data)

m <- lm(coredata(data) ~ index(data))
apts.diff <- diff(log(data))

data.arima <- auto.arima(data)
summary(data.arima)
model1 <- arima(data, order = c(0, 1, 2)) ; model1
model_forecast <- forecast(model1, h = 4) ; model_forecast
plot(model_forecast)
