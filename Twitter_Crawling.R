Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")
setwd("~/Desktop/Justin")

if(!require(base64enc)) install.packages("base64enc") ; library(base64enc)
if(!require(RCurl)) install.packages("RCurl") ; library(RCurl)
if(!require(twitteR)) install.packages("twitteR") ; library(twitteR)
if(!require(ROAuth)) install.packages("ROAuth") ; library(ROAuth)
if(!require(KoNLP)) install.packages("KoNLP") ; library(KoNLP)
if(!require(wordcloud)) install.packages("wordcloud") ; library(wordcloud)
if(!require(plyr)) install.packages("plyr") ; library(plyr)
if(!require(tm)) install.packages("tm") ; library(tm)
if(!require(openssl)) install.packages("openssl") ; library(openssl)
if(!require(httpuv)) install.packages("httpuv") ; library(httpuv)

reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

consumerKey <- "USd0smg7zuJJrM2W1Qubf8T64"
consumerSecret <- "WllhJL3eaz62oifmyfqfVcN95qLRk8m6UTmMERyW7sdpd5RggH"
accessToken <- "975289527290937351-k6gGsSUimduSduxNrD4Xd9bwCI7ws1F"
accessTokenSecret <- "iAY6i7LIbfQBzRYD4lYQuSAC2DyeKvNQ3DURRI3go6R5f"

cred <- OAuthFactory$new(consumerKey = consumerKey,
                         consumerSecret = consumerSecret,
                         requestURL = reqURL,
                         accessURL = accessURL,
                         authURL = authURL)
cred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)

stDate <- '2018-06-12'
tilDate <- '2018-06-13'

# 박원순
keyword <- '박원순'

twitter <- searchTwitter(keyword, since = stDate, until = tilDate, lang = "ko", n = 100000) ; length(twitter)
twitter.df <- twListToDF(twitter)

twitter.df <- twitter.df[twitter.df$retweetCount!=0,]
twitter.text <- twitter.df$text

twitter.text <- gsub("\n", "", twitter.text)
twitter.text <- gsub("\r", "", twitter.text)
twitter.text <- gsub("RT", "", twitter.text)
twitter.text <- gsub("http", "", twitter.text)
twitter.text <- gsub("ㅠ", "", twitter.text)
twitter.text <- gsub("ㅋ", "", twitter.text)

pw <- gsub("[^가-힣]", " ", twitter.text)
pw <- as.data.frame(pw)
write.csv(pw, "박원순.csv")

# 김문수
keyword <- '김문수'

twitter <- searchTwitter(keyword, since = stDate, until = tilDate, lang = "ko", n = 100000) ; length(twitter)
twitter.df <- twListToDF(twitter)

twitter.df <- twitter.df[twitter.df$retweetCount!=0,]
twitter.text <- twitter.df$text

twitter.text <- gsub("\n", "", twitter.text)
twitter.text <- gsub("\r", "", twitter.text)
twitter.text <- gsub("RT", "", twitter.text)
twitter.text <- gsub("http", "", twitter.text)
twitter.text <- gsub("ㅠ", "", twitter.text)
twitter.text <- gsub("ㅋ", "", twitter.text)

pw <- gsub("[^가-힣]", " ", twitter.text)
pw <- as.data.frame(pw)
write.csv(pw, "김문수.csv")

# 안철수
keyword <- '안철수'

twitter <- searchTwitter(keyword, since = stDate, until = tilDate, lang = "ko", n = 100000) ; length(twitter)
twitter.df <- twListToDF(twitter)

twitter.df <- twitter.df[twitter.df$retweetCount!=0,]
twitter.text <- twitter.df$text

twitter.text <- gsub("\n", "", twitter.text)
twitter.text <- gsub("\r", "", twitter.text)
twitter.text <- gsub("RT", "", twitter.text)
twitter.text <- gsub("http", "", twitter.text)
twitter.text <- gsub("ㅠ", "", twitter.text)
twitter.text <- gsub("ㅋ", "", twitter.text)

pw <- gsub("[^가-힣]", " ", twitter.text)
pw <- as.data.frame(pw)
write.csv(pw, "안철수.csv")


# pw <- twitter.text
# write.csv(twitter.text, "content.csv")
# 
# library(rJava)
# library(KoNLP)
# library(tm)
# data <- read.csv("content.csv")
# content <- gsub("[^가-힣]", " ", data$x)
# content_noun <- extractNoun(content)
# content_noun <- unlist(content_noun)
# dd <- data.frame(table(content_noun))
# dd[c(order(dd$Freq), decreasing = F),]