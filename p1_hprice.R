library(tidyverse)
library(mosaic)
library(foreach)
library(FNN)
library(knitr)
library(kableExtra)

# data
data(SaratogaHouses)
summary(SaratogaHouses)
head(SaratogaHouses)

## how it looks
hist(SaratogaHouses$price, breaks = 50)
hist(log(SaratogaHouses$price), breaks = 50) # log(price) is worse than price

lm(price ~ ., data = SaratogaHouses)
ggplot(SaratogaHouses) + geom_point(aes(lotSize, price)) + 
  geom_smooth(aes(lotSize, price), se = FALSE) #
ggplot(SaratogaHouses) + geom_point(aes(age, price)) + 
  geom_smooth(aes(age, price), se=FALSE) #-
ggplot(SaratogaHouses) + geom_point(aes(landValue, price)) + 
  geom_smooth(aes(landValue, price), se=FALSE) ###
ggplot(SaratogaHouses) + geom_point(aes(livingArea, price)) + 
  geom_smooth(aes(livingArea, price), se=FALSE) ###
ggplot(SaratogaHouses) + geom_point(aes(pctCollege, price)) + 
  geom_smooth(aes(pctCollege, price), se=FALSE) #
ggplot(SaratogaHouses) + geom_boxplot(aes(factor(bedrooms), price)) ##
ggplot(SaratogaHouses) + geom_boxplot(aes(factor(fireplaces), price)) ###
ggplot(SaratogaHouses) + geom_boxplot(aes(factor(bathrooms), price)) ###
ggplot(SaratogaHouses) + geom_boxplot(aes(factor(rooms), price)) ###
ggplot(SaratogaHouses) + geom_boxplot(aes(heating, price)) ##
ggplot(SaratogaHouses) + geom_boxplot(aes(fuel, price)) ##
ggplot(SaratogaHouses) + geom_boxplot(aes(sewer, price)) ##
ggplot(SaratogaHouses) + geom_boxplot(aes(waterfront, price)) ##
ggplot(SaratogaHouses) + geom_boxplot(aes(newConstruction, price)) ##
ggplot(SaratogaHouses) + geom_boxplot(aes(centralAir, price)) ##

# modeling
## functio for testing : root-mean-square error
rmse = function(y, yhat) {
  sqrt( mean( (y - yhat)^2 ) )
}

## Split into training and testing sets
n = nrow(SaratogaHouses)
n_train = round(0.8*n)  # round to nearest integer
n_test = n - n_train

## LPM

rmse_vals = do(100) * {
  ### split train set and test set
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  saratoga_train = SaratogaHouses[train_cases,]
  saratoga_test = SaratogaHouses[test_cases,] 
  
  ### fit
  lm_M = lm(price ~ . - sewer - waterfront - landValue - newConstruction, data=saratoga_train)
  lm_B = lm(price ~ lotSize + landValue + waterfront + newConstruction + 
              bedrooms*bathrooms + heating + fuel + pctCollege + rooms*bedrooms + 
              rooms*bathrooms + rooms*heating + livingArea, data=saratoga_train)
  lm_1 = lm(price ~ lotSize + livingArea * landValue + bedrooms * rooms + bathrooms * rooms +
              heating + waterfront + newConstruction + centralAir,
            data = saratoga_train)
  #lm_2 = lm(price ~ lotSize + livingArea * landValue + bathrooms * rooms +
  #heating + waterfront + newConstruction + centralAir,
   #         data = saratoga_train)
  
  ### predict
  yhat_M = predict(lm_M, saratoga_test)
  yhat_B = predict(lm_B, saratoga_test)
  yhat_1 = predict(lm_1, saratoga_test)
  # yhat_2 = predict(lm_2, saratoga_test)
  
  ### evaluation
  c(rmse(saratoga_test$price, yhat_M),
    rmse(saratoga_test$price, yhat_B),
    rmse(saratoga_test$price, yhat_1))
    #, rmse(saratoga_test$price, yhat_2))
  
}
rmse_lpm = data.frame(mediim_Model_BM1 = colMeans(rmse_vals[1]),
                      biggerboom_Model_BM2 = colMeans(rmse_vals[2]),
                      New_Model = colMeans(rmse_vals[3]))
kable(rmse_lpm) %>% kable_styling("striped", full_width = F) 


summary(lm_1)

## KNN
### new variables for modelling 
KNN_X = model.matrix(~lotSize + livingArea * landValue + bedrooms * rooms + bathrooms * rooms +
                       heating + waterfront + newConstruction + centralAir-1,
                     data = SaratogaHouses)
KNN_y = SaratogaHouses$price

### averaging
#k_grid = exp(seq(log(2), log(300), length=30)) %>% round %>% unique
#k_grid = seq(1, 50, by=2)
k_grid = seq(1, 30, by=2)
                 
err_grid = foreach(k = k_grid, .combine='c') %do% {
  out = do(100) * {
    
    #### split
    train_cases = sample.int(n, n_train, replace=FALSE)
    test_cases = setdiff(1:n, train_cases)
    X_train = KNN_X[train_cases,]
    X_test = KNN_X[test_cases,]
    y_train = KNN_y[train_cases]
    y_test = KNN_y[test_cases]
    
    #### scale the training set features
    scale_factors = apply(X_train, 2, sd)
    X_train_sc = scale(X_train, scale=scale_factors)
    X_test_sc = scale(X_test, scale=scale_factors)
    
    # fitting
    knn_try = knn.reg(X_train_sc, X_test_sc, y_train, k=k)
    
    # errors
    rmse(y_test, knn_try$pred)
  }
  mean(out$result)
}
rmse_KNN = data.frame(K_value = c(which(err_grid == min(err_grid))), 
                      KNN_RMSE = c(min(err_grid)),
                      LPM_RMSE = colMeans(rmse_vals[3]))
kable(rmse_KNN) %>% kable_styling("striped", full_width = F) 

plot(k_grid, err_grid, ylim = c(55000, 70000))
abline(h=colMeans(rmse_vals[3]), col='red')




