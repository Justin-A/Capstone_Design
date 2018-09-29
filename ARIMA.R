Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")
setwd("~/Desktop/Justin/2018-1/Capstone/Data/Naver/")

pw <- read.csv("박원순_Naver_2014.csv") ; head(pw)
#ss <- read.csv("서울시장_Naver_2014.csv") ; head(ss)
jm <- read.csv("정몽준_Naver_2014.csv") ; head(jm)

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
# data_ss <- ss[,c(-1, -3)]
# data_ss$sympathyCount <- as.numeric(data_ss$sympathyCount)
# data_ss$antipathyCount <- as.numeric(data_ss$antipathyCount)
# hist(data_ss$sympathyCount) ; table(data_ss$sympathyCount)
# hist(data_ss$antipathyCount) ; table(data_ss$antipathyCount)
# data_ss$rate <- data_ss$sympathyCount / (data_ss$sympathyCount + data_ss$antipathyCount)

# 날짜별 데이터 개수
data_pw$count <- 1
k1 <- aggregate(data_pw$count, by = list(data_pw$regTime), sum)
colnames(k1) <- c("regTime", "PW_Count")
k1$regTime <- as.Date(k1$regTime, format = "%d/%m/%Y")
pw_count <- k1[c(order(k1$regTime)),] ; pw_count <- pw_count[1:61,] ; pw_count

k2 <- aggregate(data_pw$sympathyCount, by = list(data_pw$regTime), sum)
colnames(k2) <- c("regTime", "Agree")
k2$regTime <- as.Date(k2$regTime, format = "%d/%m/%Y")
pw_agree <- k2[c(order(k2$regTime)), ] ; pw_agree <- pw_agree[1:61,] ; pw_agree

k3 <- aggregate(data_pw$antipathyCount, by = list(data_pw$regTime), sum)
colnames(k3) <- c("regTime", "Disagree")
k3$regTime <- as.Date(k3$regTime, format = "%d/%m/%Y")
pw_disagree <- k3[c(order(k3$regTime)), ] ; pw_disagree <- pw_disagree[1:61,] ; pw_disagree

k4 <- aggregate(data_pw$rate, by = list(data_pw$regTime), mean)
colnames(k4) <- c("regTime", "Rate")
k4$regTime <- as.Date(k4$regTime, format = "%d/%m/%Y")
pw_rate <- k4[c(order(k4$regTime)),] ; pw_rate <- pw_rate[1:61,] ; pw_rate

pw_total <- data.frame(pw_count, pw_agree$Agree, pw_disagree$Disagree, pw_rate$Rate)
colnames(pw_total) <- c("Date", "Count", "Agree_sum", "Disagree_sum", "Rate")

##########################################################################################

data_jm$count <- 1
k1 <- aggregate(data_jm$count, by = list(data_jm$regTime), sum)
colnames(k1) <- c("regTime", "JM_Count")
k1$regTime <- as.Date(k1$regTime, format = "%d/%m/%Y")
jm_count <- k1[c(order(k1$regTime)),] ; jm_count <- jm_count[1:61,] ; jm_count

k2 <- aggregate(data_jm$sympathyCount, by = list(data_jm$regTime), sum)
colnames(k2) <- c("regTime", "Agree")
k2$regTime <- as.Date(k2$regTime, format = "%d/%m/%Y")
jm_agree <- k2[c(order(k2$regTime)), ] ; jm_agree <- jm_agree[1:61,] ; jm_agree

k3 <- aggregate(data_jm$antipathyCount, by = list(data_jm$regTime), sum)
colnames(k3) <- c("regTime", "Disagree")
k3$regTime <- as.Date(k3$regTime, format = "%d/%m/%Y")
jm_disagree <- k3[c(order(k3$regTime)), ] ; jm_disagree <- jm_disagree[1:61,] ; jm_disagree

k4 <- aggregate(data_jm$rate, by = list(data_jm$regTime), mean)
colnames(k4) <- c("regTime", "Rate")
k4$regTime <- as.Date(k4$regTime, format = "%d/%m/%Y")
jm_rate <- k4[c(order(k4$regTime)),] ; jm_rate <- jm_rate[1:61,] ; jm_rate

jm_total <- data.frame(jm_count, jm_agree$Agree, jm_disagree$Disagree, jm_rate$Rate)
colnames(jm_total) <- c("Date", "Count", "Agree_sum", "Disagree_sum", "Rate")

#######################################################################################
setwd("~/Desktop/Justin/2018-1/Capstone/Data/")
research <- read.csv("2014_Research.csv")
research <- research[,-4]
# Amelia
library(Amelia)

imp.a <- amelia(research, m = 5)
sa1 <- imp.a$imputations[[1]] ; sa1$무응답 <- 100 - sa1$박원순 - sa1$정몽준 ; summary(sa1$무응답) 
sa2 <- imp.a$imputations[[2]] ; sa2$무응답 <- 100 - sa2$박원순 - sa2$정몽준 ; summary(sa2$무응답) 
sa3 <- imp.a$imputations[[3]] ; sa3$무응답 <- 100 - sa3$박원순 - sa3$정몽준 ; summary(sa3$무응답) 
sa4 <- imp.a$imputations[[4]] ; sa4$무응답 <- 100 - sa4$박원순 - sa4$정몽준 ; summary(sa4$무응답) 
sa5 <- imp.a$imputations[[5]] ; sa5$무응답 <- 100 - sa5$박원순 - sa5$정몽준 ; summary(sa5$무응답) 
#######################################################################################
pw_total$support <- sa2$박원순
jm_total$support <- sa2$정몽준
#######################################################################################
# ARIMA
# https://github.com/haven-jeon/rdatamining/blob/master/time_series.Rmd
# https://woosa7.github.io/R-%EC%8B%9C%EA%B3%84%EC%97%B4%EB%B6%84%EC%84%9D-Time-Series-ARIMA/

ts_sa <- ts(sa4$박원순)
data <- ts_sa
auto.arima(data)
model <- arima(data, order = c(0, 1, 1)) ; model
model_forecast <- forecast(model, h = 4) ; model_forecast

ts_sa <- ts(sa4$정몽준)
data <- ts_sa
auto.arima(data)
model <- arima(data, order = c(0, 0, 0)) ; model
model_forecast <- forecast(model, h = 4) ; model_forecast

################################################################################################
res <- VAR(pw_total[, c(2,3,4,6)],  # prod, rw 변수 이용
           lag.max = 4,       # 제한될 최대 시차 = 3
           ic = "SC",         # Schwarz의 정보기준을 이용하여 자동으로 시차 결정
           type = "none")     # 상수항, 추세항, 제거 모형 지정
prediction <- predict(res, n.ahead = 4, ci = 0.95) ; prediction

res <- VAR(jm_total[, c(2,3,4,6)],  # prod, rw 변수 이용
           lag.max = 4,       # 제한될 최대 시차 = 3
           ic = "SC",         # Schwarz의 정보기준을 이용하여 자동으로 시차 결정
           type = "none")     # 상수항, 추세항, 제거 모형 지정
prediction <- predict(res, n.ahead = 4, ci = 0.95) ; prediction

