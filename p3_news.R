library(mosaic)
library(tidyverse)
library(class)
library(foreach)
library(knitr)
library(kableExtra)



# data
news = read.csv("../data/online_news.csv")
head(news, 5)
ncol(news)

# data cleaning

## delete url
news = subset(news, select = -c(url))

## delete the data with "zero word in the content"
summary(news)
news_0_content = news[which(news$n_tokens_content==0),]
summary(news_0_content$average_token_length)
summary(news_0_content) # zero length of the words in the content, no polarity data
news = news[-which(news$n_tokens_content==0),]
summary(news)

## logshare ?  
hist(news$shares, xlim=c(1,10000), breaks=5000) ; abline(v=1400, col='red')
hist(log(news$shares), breaks=50) ; abline(v=log(1400), col='red')
summary(news$shares)
news = mutate(news, logshares = log(shares))

## num of image & video
hist(news$num_imgs, xlim = c(0,50), breaks = 128)
hist(news$num_videos, xlim = c(0,30), breaks = 91)
news = mutate(news, img = ifelse(num_imgs==0,0,1))
news = mutate(news, video = ifelse(num_videos==0,0,1))


## how it looks

###
ggplot(data=news) + geom_point(aes(x=num_keywords, y=logshares), size=0.1) + geom_hline(yintercept = log(1400), col="red")
ggplot(data=news) + geom_point(aes(x=title_subjectivity, y=logshares), size=0.1) + geom_hline(yintercept = log(1400), col="red")
ggplot(data=news) + geom_point(aes(x=global_rate_positive_words, y=logshares), size=0.1) + geom_hline(yintercept = log(1400), col="red")
ggplot(data=news) + geom_boxplot(aes(x=factor(img), y=logshares)) + geom_hline(yintercept = log(1400), col="red")
ggplot(data=news) + geom_boxplot(aes(x=factor(video), y=logshares)) + geom_hline(yintercept = log(1400), col="red")

## data channel
data_channel_chk = news$data_channel_is_bus + news$data_channel_is_entertainment + 
  news$data_channel_is_lifestyle + news$data_channel_is_socmed + 
  news$data_channel_is_tech + news$data_channel_is_world
summary(data_channel_chk)

news_data_ch = select(news, data_channel_is_bus, data_channel_is_entertainment, 
                      data_channel_is_lifestyle, data_channel_is_socmed, 
                      data_channel_is_tech, data_channel_is_world)
data_channel = factor(data.matrix(news_data_ch) %*% 1:ncol(news_data_ch), 
                      labels = c("other", "bus", "entertn", "lifesty", "socmd", "tech", "world"))
news_data_ch = data.frame(news_data_ch, data_channel, news$logshares, news$img, news$video)
ggplot(data = news_data_ch) + geom_boxplot((aes(x=data_channel, y=news.logshares))) + 
  geom_hline(yintercept = log(1400), col="red")

ggplot(data = news_data_ch) + geom_boxplot((aes(x=factor(news.img), y=news.logshares))) +
  facet_wrap( ~ data_channel, nrow = 1) +
  geom_hline(yintercept = log(1400), col="red")

ggplot(data = news_data_ch) + geom_boxplot((aes(x=factor(news.video), y=news.logshares))) +
  facet_wrap( ~ data_channel, nrow = 1) +
  geom_hline(yintercept = log(1400), col="red")

## the day of published 
weekend_chk = news$weekday_is_saturday + news$weekday_is_sunday - news$is_weekend

news_days = select(news, weekday_is_monday, weekday_is_tuesday, weekday_is_wednesday,
                   weekday_is_thursday, weekday_is_friday, weekday_is_saturday, weekday_is_sunday)
days = factor(data.matrix(news_days) %*% 1:ncol(news_days), 
              labels = c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun"))
news_days = data.frame(news_days, days, news$logshares)
ggplot(data = news_days) + geom_boxplot((aes(x=days, y=news.logshares))) + geom_hline(yintercept = log(1400), col="red")


# define function

## confidence table


conf_table = function(y, yhat) {
  y_test = ifelse(y>log(1400), 1, 0)
  yh_t = ifelse(yhat>log(1400), 1, 0)
  table(y_test, yh_t)
}


## conf_rate

conf_rate = function(y, yhat) {
  y_test = ifelse(y>log(1400), 1, 0)
  yh_t = ifelse(yhat>log(1400), 1, 0)
  sum(yh_t != y_test)/length(y)
}


# linear or knn model

## Split into training and testing sets
n = nrow(news)
n_train = round(0.8*n)  # round to nearest integer
n_test = n - n_train

## linear model

err_vals = do(100)*{
  
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  news_train = news[train_cases,]
  news_test = news[test_cases,]
  
  lm1 = lm(logshares ~ n_tokens_content + num_hrefs + num_self_hrefs + average_token_length + 
             num_keywords + data_channel_is_lifestyle + 
             num_imgs * (data_channel_is_bus + data_channel_is_socmed + data_channel_is_world) +
             video * (data_channel_is_entertainment + data_channel_is_bus + 
                        data_channel_is_socmed + data_channel_is_tech + data_channel_is_world) +
             self_reference_avg_sharess * self_reference_max_shares + 
             is_weekend + global_rate_positive_words * avg_positive_polarity + 
             avg_negative_polarity + title_subjectivity + title_sentiment_polarity, 
           data = news_train)
  lm2 =  lm(logshares ~ n_tokens_title + n_tokens_content + num_hrefs + num_self_hrefs + 
              average_token_length + num_keywords + (video + num_imgs) * 
              (data_channel_is_lifestyle + data_channel_is_entertainment +
                 data_channel_is_bus + data_channel_is_socmed +
                 data_channel_is_tech + data_channel_is_world) +
              self_reference_avg_sharess * (self_reference_min_shares + 
                                              self_reference_max_shares) + is_weekend + 
              global_rate_positive_words * avg_positive_polarity + 
              global_rate_negative_words * avg_negative_polarity + title_subjectivity +
              title_sentiment_polarity, data = news_train)
  lm3 = lm(logshares ~ .-shares- weekday_is_sunday - is_weekend - img -video, data = news_train)

  yhat_test1 = predict(lm1, news_test)
  yhat_test2 = predict(lm2, news_test)
  yhat_test3 = predict(lm3, news_test)

  # confusion rate
  c(conf_rate(news_test$logshares, yhat_test1), conf_rate(news_test$logshares, yhat_test2),
    conf_rate(news_test$logshares, yhat_test3)) %>% round(3)
  
}
colMeans(err_vals)

# chencking model by in-sample performance in order to show confusion table 
lmF = lm(logshares ~ n_tokens_content + num_hrefs + num_self_hrefs + average_token_length + 
           num_keywords + data_channel_is_lifestyle + num_imgs * 
           (data_channel_is_bus + data_channel_is_socmed + data_channel_is_world) + video * 
           (data_channel_is_entertainment + data_channel_is_bus + data_channel_is_socmed +
              data_channel_is_tech + data_channel_is_world) +  self_reference_avg_sharess * 
           self_reference_max_shares + is_weekend + global_rate_positive_words * 
           avg_positive_polarity + avg_negative_polarity +
           title_subjectivity + title_sentiment_polarity, data = news)
summary(lmF)
yhatF = predict(lmF, news)
conf_table(news$logshares, yhatF)
ct = conf_table(news$logshares, yhatF)
err = data.frame(OverallErrorRate = (ct[1,2]+ct[2,1])/sum(ct[]),
                 TruePositiveRate = ct[2,2]/sum(ct[2,]),
                 FalsePositiveRate = ct[1,2]/sum(ct[1,])) %>% round(3)
kable(err) %>% kable_styling("striped") 

# classification model : by binomial model 

## define viral variable
news = mutate(news, viral = ifelse(shares > 1400,1,0))

err_vals = do(100) * {

  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  news_train = news[train_cases,]
  news_test = news[test_cases,]
  
  logit_m = glm(viral ~  n_tokens_content + num_hrefs + num_self_hrefs + average_token_length + 
                  num_keywords + data_channel_is_lifestyle + num_imgs * 
                  (data_channel_is_bus + data_channel_is_socmed + data_channel_is_world) + video * 
                  (data_channel_is_entertainment + data_channel_is_bus + data_channel_is_socmed +
                     data_channel_is_tech + data_channel_is_world) +  self_reference_avg_sharess * 
                  self_reference_max_shares + is_weekend + global_rate_positive_words * 
                  avg_positive_polarity + avg_negative_polarity +
                  title_subjectivity + title_sentiment_polarity, data = news_train, family = 'binomial')
  phat_logit = predict(logit_m, news_test, type = 'response')
  yhat_logit = ifelse(phat_logit>0.5, 1, 0)
  ct_lg = table(news_test$viral, yhat_logit)
  
  # linear
  lmF = lm(logshares ~ n_tokens_content + num_hrefs + num_self_hrefs + average_token_length + 
             num_keywords + data_channel_is_lifestyle + num_imgs * 
             (data_channel_is_bus + data_channel_is_socmed + data_channel_is_world) + video * 
             (data_channel_is_entertainment + data_channel_is_bus + data_channel_is_socmed +
                data_channel_is_tech + data_channel_is_world) +  self_reference_avg_sharess * 
             self_reference_max_shares + is_weekend + global_rate_positive_words * 
             avg_positive_polarity + avg_negative_polarity +
             title_subjectivity + title_sentiment_polarity, data = news_train)
  yhatF = predict(lmF, news_test)
  ct_lm = conf_table(news_test$logshares, yhatF)
  
  # result
  c((1-sum(diag(ct_lg))/sum(ct_lg)), (1-sum(diag(ct_lm))/sum(ct_lm)),
    ct_lg[2,2]/sum(ct_lg[2,]), ct_lm[2,2]/sum(ct_lm[2,]),
    ct_lg[1,2]/sum(ct_lg[1,]), ct_lm[1,2]/sum(ct_lm[1,]))
} 

errMean = colMeans(err_vals) %>% round(3)
err = matrix(errMean, nrow = 2, dimnames = 
               list(c("Classification Model", "Numerical Model"), 
               c("OverallErrorRate", "TruePositiveRate", "FalsePositiveRate")))
kable(err) %>% kable_styling("striped") 




