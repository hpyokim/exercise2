library(tidyverse)
library(mosaic)
library(knitr)
library(kableExtra)
library(fastDummies)
library(kableExtra)

# data
brca = read.csv("../data/brca.csv")
head(brca)

## cancer factors
xtabs(~cancer + recall, data = brca) %>% prop.table(margin=2) %>% 
  round(3) %>% kable() %>% kable_styling("striped")
xtabs(~cancer + age, data = brca) %>% prop.table(margin=2) %>%
  round(3) %>% kable() %>% kable_styling("striped")
xtabs(~cancer + history, data = brca) %>% prop.table(margin=2) %>% 
  round(3) %>% kable() %>% kable_styling("striped")
xtabs(~cancer + symptoms, data = brca) %>% prop.table(margin=2) %>% 
  round(3) %>% kable() %>% kable_styling("striped")
xtabs(~cancer + menopause, data = brca) %>% prop.table(margin=2) %>% 
  round(3) %>% kable() %>% kable_styling("striped")
xtabs(~cancer + density, data = brca) %>% prop.table(margin=2) %>% 
  round(3) %>% kable() %>% kable_styling("striped")

# conservative rate

## without patient risk factors
xtabs(~recall + radiologist, data = brca) %>% 
  prop.table(margin = 2) %>% round(3) %>% kable() %>% kable_styling("striped")

## modelling patient risk factors : regress cancer on features

### split 80% train and 20% test 
n = nrow(brca) 
n_train = round(0.8*n)
n_test = n - n_train

### function for evaluation by deviance
dev_out = function(y, probhat) {
  rc_pairs = cbind(seq_along(y), y)
  -2*sum(log(probhat[rc_pairs]))
}

### new dummies of high probabilty of cancer
brca = mutate(brca, old = ifelse(brca$age=='age70plus', 1, 0), 
              menop = ifelse(brca$menopause=='postmenounknown', 1, 0),
              dense = ifelse(brca$density=='density4', 1, 0))
  
### averaging test result
err_grid = do(100) * {
  #### split train and test set
  train_cases = sample.int(n, n_train, replace = FALSE)
  test_cases = setdiff(1:n, train_cases)
  brca_train = brca[train_cases,]
  brca_test = brca[test_cases,]

  #### bench mark; using a global cancer probability
  p_bm = length(which(brca_test$cancer=="1"))/n_test
  bm = rep(p_bm, times = n_test)
  
  #### logit
  logit1 = glm(cancer ~ . - recall - radiologist - old - menop - dense, data = brca_train, family = 'binomial')
  logit2 = glm(cancer ~ history + symptoms + old + menop + dense, data = brca_train, family = 'binomial')
  logit3 = glm(cancer ~ history, data = brca_train, family = 'binomial')
  logit4 = glm(cancer ~ history + symptoms, data = brca_train, family = 'binomial')
  logit5 = glm(cancer ~ history + symptoms + old, data = brca_train, family = 'binomial')
  
  ##### likelyhood 
  phat1 = predict(logit1, brca_test, type = "response")
  phat2 = predict(logit2, brca_test, type = "response")
  phat3 = predict(logit3, brca_test, type = "response")
  phat4 = predict(logit4, brca_test, type = "response")
  phat5 = predict(logit5, brca_test, type = "response")
  
  #### deviance
  dev_bm = dev_out(brca_test$cancer, bm)
  dev_logit1 = dev_out(brca_test$cancer, phat1)
  dev_logit2 = dev_out(brca_test$cancer, phat2)
  dev_logit3 = dev_out(brca_test$cancer, phat3)
  dev_logit4 = dev_out(brca_test$cancer, phat4)
  dev_logit5 = dev_out(brca_test$cancer, phat5)
  
  c(dev_bm, dev_logit1, dev_logit2, dev_logit3, dev_logit4, dev_logit5)
}
dev_table = data.frame(
  Models = c("Cancer Prob(BM)", "Model1", "Model2", "Model3", "Model4", "Model5"),
  Deviances = colMeans(err_grid)
) 
kable(t(dev_table)) %>% 
  kable_styling("striped", full_width = TRUE) # select 'logit3' model


## conservative test : high conservative for recalling those who have less probabilty of cancer 

### split data for each radiologist
brca_13 = brca[brca$radiologist=="radiologist13",]
brca_34 = brca[brca$radiologist=="radiologist34",]
brca_66 = brca[brca$radiologist=="radiologist66",]
brca_89 = brca[brca$radiologist=="radiologist89",]
brca_95 = brca[brca$radiologist=="radiologist95",]

### test model : 'logit3' model
logit_test = glm(cancer ~ history, data = brca, family = 'binomial')

### function for calculating conserative concerning cancer probabilities
conserv = function(y) {
  p_canc = predict(logit_test, y, type = "response")
  recall_p_canc = y$recall/p_canc
  sum(recall_p_canc)/nrow(y)
} # more conservative for less cancer probabilities

### conservative
conserv_table = data.frame(
  radiologist=c("radiologist13","radiologist34","radiologist66","radiologist89","radiologist95"),
  conservative_rate=c(conserv(brca_13), conserv(brca_34), conserv(brca_66), conserv(brca_89), conserv(brca_95)))

kable(t(conserv_table)) %>% 
  kable_styling("striped") # radiologist89 is the most conservative

# adding patient risk factors to determine recalling 

## by TPR(minimizing false negative), FPR(minimizing false positive)

### function for TPR
TPR = function(y, yhat) {
  length(which(y==1 & yhat==1))/length(which(y==1))     
  }

### function for FPR
FPR = function(y, yhat) {
  length(which(y==0 & yhat==1))/length(which(y==0))
}

## compare recall-only model and risk-factor models
### function for yhat
yhat = function(model, test_set){
  phat = predict(model, test_set, type = "response")
  ifelse(phat > 0.5, 1, 0)
}


### averaging test result
test_grid = do(100) * {
  
  #### split train and test set
  train_cases = sample.int(n, n_train, replace = FALSE)
  test_cases = setdiff(1:n, train_cases)
  brca_train = brca[train_cases,]
  brca_test = brca[test_cases,]
  
  #### bench mark for confusion rate : cancer probabilty
  #p_bm = length(which(brca_test$cancer=="1"))/n_test
  #bm = rep(p_bm, times = n_test)
  
  #### logit
  M_rf1 = glm(cancer ~ .- radiologist - old - menop - dense, data = brca_train, family = 'binomial')
  M_rf2 = glm(cancer ~ recall + history, data = brca_train, family = 'binomial')
  M_rf3 = glm(cancer ~ history, data = brca_train, family = 'binomial')
  
  ##### likelyhood and 
  yh_rf1 = yhat(M_rf1, brca_test)
  yh_rf2 = yhat(M_rf2, brca_test)
  yh_rf3 = yhat(M_rf3, brca_test)
  
  #### TPR, FPR
  c(TPR(brca_test$cancer,brca_test$recall), FPR(brca_test$cancer,brca_test$recall), 
    TPR(brca_test$cancer,yh_rf1), FPR(brca_test$cancer,yh_rf1),
    TPR(brca_test$cancer,yh_rf2), FPR(brca_test$cancer,yh_rf2),
    TPR(brca_test$cancer,yh_rf3), FPR(brca_test$cancer,yh_rf3))
}
errMeans = colMeans(test_grid) %>% round(3)
err = matrix(errMeans, nrow=2, dimnames = 
                list(c("TPR", "FPR"), 
                     c("Recall", "Model1", "Model2", "Model3")))
kable(err) %>% kable_styling("striped", full_width = TRUE) 


