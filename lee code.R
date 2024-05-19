
############################################################################################################
# load libraries + setup
############################################################################################################

set.seed(42)

# load libraries
library(ggplot2)
library(corrplot)
library(caret)
library(MASS)
library(rsample)
library(AmesHousing)
library(Metrics)
library(car)
library(ggcorrplot)
library(ISLR2)
library(pls)
library(caret) 
library(gam)
library(randomForest)
library(tidyverse)
library(pls)
library(snow)
library(Metrics)

# some of these overwrite caret
#library(nnet)
#library(keras)
#library(tensorflow)

# parallel setup
library(doParallel)
registerDoParallel(makePSOCKcluster(detectCores() - 15))

# check directory
getwd()


############################################################################################################
# Load data
############################################################################################################
test <- read.csv("C:\\Users\\lawor\\OneDrive\\Desktop\\School\\ST 563\\Final project\\YearPredictTest.txt", header=TRUE)
train <- read.csv("C:\\Users\\lawor\\OneDrive\\Desktop\\School\\ST 563\\Final project\\YearPredictTrain.txt", header=TRUE)

# exclude class
train.set1 <- train[-92]
test.set1 <- test[-92]

# exclude year
train.set2 <- train[-91]
test.set2 <- test[-91]

train.set2$Class <- as.factor(train.set2$Class) 
test.set2$Class <- as.factor(test.set2$Class) 

############################################################################################################
# data review
############################################################################################################

# check for Nas
sum(is.na(train))

# look at summary data
summary(train[-92])
# variable relationships
corrplot(cor(train[c(1:10)]), method = 'number')
corrplot(cor(train[c(25:35)]), method = 'number')
corrplot(cor(train[c(57:67)]), method = 'number')
corrplot(cor(train[c(81:91)]), method = 'number')
corrplot(cor(train[c(34:44)]), method = 'number')
corrplot(cor(train[c(70:80)]), method = 'number')

# Further check
correlation <- cor(train[-92])
which(correlation >= 0.70 | correlation >= 0.80)

# get highest correlations
library(data.table)
setDT(melt(cor(train[1:91])))[Var1 != Var2, .SD[which.max(abs(value))], keyby=Var1]

# get highest correlations 2
correlation <- as.data.frame(as.table(cor(train[1:91])))
subset(correlation, abs(Freq) > 0.5 & abs(Freq) < 1) 

##Histogram of the response variable
hist(train$Year ,breaks = 90 ,labels = TRUE)

aggregate(train$Class ,by=list(train$Class) ,FUN = length)


# plot average distributions by class
par(mfrow=c(3 ,4))
boxplot(train$timbreAvg1 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreAvg2 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreAvg3 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreAvg4 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreAvg5 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreAvg6 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreAvg7 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreAvg8 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreAvg9 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreAvg10 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreAvg11 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreAvg12 ~ train$Class ,horizontal = TRUE)
# not much difference in averages amoung classes

# plot average distributions by year (group 1)
par(mfrow=c(3 ,4))
boxplot(train$timbreCov1 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov2 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov3 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov4 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov5 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov6 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov7 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov8 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov9 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov10 ~ train$Class ,horizontal = TRUE)
# not much difference in averages among classes IQR

# plot average distributions by year (group 2)
par(mfrow=c(3 ,4))
boxplot(train$timbreCov11 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov12 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov13 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov14 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov15 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov16 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov17 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov18 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov19 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov20 ~ train$Class ,horizontal = TRUE)
# not much difference in averages among classes IQR

# plot average distributions by year (group 3)
par(mfrow=c(3 ,4))
boxplot(train$timbreCov21 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov22 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov23 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov24 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov25 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov26 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov27 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov28 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov29 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov30 ~ train$Class ,horizontal = TRUE)
# not much difference in averages among classes IQR

# plot average distributions by year (group 4)
par(mfrow=c(3 ,4))
boxplot(train$timbreCov31 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov32 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov33 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov34 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov35 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov36 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov37 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov38 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov39 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov40 ~ train$Class ,horizontal = TRUE)
# not much difference in averages among classes IQR

# plot average distributions by year (group 4)
par(mfrow=c(3 ,4))
boxplot(train$timbreCov41 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov42 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov43 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov44 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov45 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov46 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov47 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov48 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov49 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov50 ~ train$Class ,horizontal = TRUE)
# not much difference in averages among classes IQR

# plot average distributions by year (group 5)
par(mfrow=c(3 ,4))
boxplot(train$timbreCov51 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov52 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov53 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov54 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov55 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov56 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov57 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov58 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov59 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov60 ~ train$Class ,horizontal = TRUE)
# not much difference in averages among classes IQR

# plot average distributions by year (group 6)
par(mfrow=c(3 ,4))
boxplot(train$timbreCov61 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov62 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov63 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov64 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov65 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov66 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov67 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov68 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov69 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov70 ~ train$Class ,horizontal = TRUE)
# not much difference in averages among classes IQR

# plot average distributions by year (group 7)
par(mfrow=c(3 ,4))
boxplot(train$timbreCov71 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov72 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov73 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov74 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov75 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov76 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov77 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov78 ~ train$Class ,horizontal = TRUE)
# not much difference in averages among classes IQR


# plot  a handful
par(mfrow=c(2 ,2))
boxplot(train$timbreAvg1  ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreAvg2  ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov51 ~ train$Class ,horizontal = TRUE)
boxplot(train$timbreCov52 ~ train$Class ,horizontal = TRUE)


# take a look at afew by year instead
par(mfrow=c(1 ,2))
boxplot(train$timbreAvg1 ~ train$Year ,horizontal = TRUE)
boxplot(train$timbreAvg2 ~ train$Year ,horizontal = TRUE)

par(mfrow=c(1 ,2))
boxplot(train$timbreCov51~ train$Year ,horizontal = TRUE)
boxplot(train$timbreCov52 ~ train$Year ,horizontal = TRUE)



############################################################################################################
# Year - linear reg
############################################################################################################

# train model
system.time(
  year_model_linear <- train(Year ~ .
                              ,data = train.set1
                              
                              ,trControl = trainControl(method = 'cv' ,number = 10 ,verboseIter = TRUE)
                              ,preProcess = c('center' ,'scale')
                              
                              ,method = 'lm'
                              ,metric = 'RMSE')
)

# save model
saveRDS(year_model_linear ,'year_model_linear.rds')

# plot results
plot(year_model_linear)


############################################################################################################
# Year - elastic (done on RMSE)
############################################################################################################

# train model
system.time(
year_model_elastic <- train(Year ~ .
                            ,data = train.set1
                            
                            ,trControl = trainControl(method = 'cv' ,number = 10 ,verboseIter = TRUE)
                            ,preProcess = c('center' ,'scale')
                            
                            ,method = 'glmnet'
                            ,tuneGrid =  expand.grid(alpha =  seq(0 ,1 ,len = 10), lambda = seq(0 ,5 ,len = 10)) # alpha = 0 = ridge, = 1 = lasso. small lambda = least squares
                            ,metric = 'RMSE')
)

# save model
saveRDS(year_model_elastic ,'year_model_elastic.rds')

# plot results
plot(year_model_elastic)

############################################################################################################
# Year - gam
############################################################################################################

# train model
system.time(
year_model_gam <- train(Year ~ .
                        ,data = train.set1
                        
                        ,trControl = trainControl(method = 'cv' ,number = 10 ,verboseIter = TRUE)
                        ,preProcess = c('center' ,'scale')
                        
                        ,method = 'gamSpline'
                        ,tuneGrid =  expand.grid(df = seq(1 ,100 ,by = 10)) 
                        ,metric = 'RMSE')
) #  12/6/2022 took 4912 seconds. 4912/60/60

# save model
saveRDS(year_model_gam ,'year_model_gam.rds')

# plot results
plot(year_model_gam)

############################################################################################################
# Year - boosting
############################################################################################################

# train model
system.time(
year_model_gbm <- train(Year ~ .
                        ,data = train.set1
                        
                        ,trControl = trainControl(method = 'cv' ,number = 10 ,verboseIter = TRUE)
                        ,preProcess = c('center' ,'scale')
                        
                        ,method = 'gbm'
                        ,tuneGrid = expand.grid(interaction.depth = seq(from = 1 ,to = 3 , by = 1) # high depth nukes run time, depth 1 was pretty good
                                                ,n.trees = (1:100)*50
                                                ,shrinkage = seq(from = .01 ,to = .1 ,by = .03) # learning rate, higher value = more prone to overfit
                                                ,n.minobsinnode = 10)

                        ,metric = 'RMSE')
) # took 9026 seconds w/ parallel cpu, 9026/60/60 = 2.5 hours

# save model
saveRDS(year_model_gbm ,'year_model_gbm.rds')

# plot results
plot(year_model_gbm)
summary(year_model_gbm) # good info on what variables contribute more

############################################################################################################
# Year - poly
############################################################################################################

# creat df to hold results
model_results <- data.frame(matrix(ncol = 3, nrow = 17))
colnames(model_results) <- c('degree' ,'RMSE' ,'MAE')

# train model
system.time(
  for(i in 1:10){
    
    form <- formula(paste(names(train.set1)[91] ," ~ " ,paste0("poly(" ,names(train.set1)[1:90] ,',' ,i ,')' ,collapse=" + ")))
    
    year_model_poly2 <- train(form
                             ,data = train.set1
                             
                             ,trControl = trainControl(method = 'cv' ,number = 10 ,verboseIter = TRUE)
                             ,preProcess = c('center' ,'scale')
                             
                             ,method = 'glm'
                             ,metric = 'RMSE')
    
    model_results$degree[i] <- i
    model_results$RMSE[i]<- year_model_poly$results$RMSE
    model_results$MAE[i] <- year_model_poly$results$MAE
    
  }
)

# check results
model_results # best fit is of degree 3, after 3 errors shoot up

# build optimal model
system.time(
  for(i in 3:3){
    
    form <- formula(paste(names(train)[91] ," ~ " ,paste0("poly(" ,names(train)[1:90] ,',' ,i ,')' ,collapse=" + ")))
    
    year_model_poly2 <- train(form
                             ,data = train.set1
                             
                             ,trControl = trainControl(method = 'cv' ,number = 10 ,verboseIter = TRUE)
                             ,preProcess = c('center' ,'scale')
                             
                             ,method = 'glm'
                             ,metric = 'RMSE')
    
    model_results$degree[i] <- i
    model_results$RMSE[i]<- year_model_poly$results$RMSE
    model_results$MAE[i] <- year_model_poly$results$MAE
    
  }
)

# save model
saveRDS(year_model_poly ,'year_model_poly.rds')
  
# plot results
plot(year_model_poly)

############################################################################################################
# Year - knn
############################################################################################################

# train model
system.time(
  year_model_knn <- train(Year ~ .
                          ,data = train.set1
                          
                          ,trControl = trainControl(method = 'cv' ,number = 10 ,verboseIter = TRUE)
                          ,preProcess = c('center','scale')
                          
                          ,method = 'knn'
                          ,tuneGrid = expand.grid(k = c(1:30))
                          ,metric = 'RMSE')
) # 12/6/2022 took 3953 seconds, about 1 hour

# save model
saveRDS(year_model_knn ,'year_model_knn.rds')

# plot results
plot(year_model_elastic)

############################################################################################################
# Year - regression tree
############################################################################################################

system.time(
  year_model_reg_tree <- train(Year ~ .
                               ,data = train.set1
                               
                               ,trControl = trainControl(method = 'repeatedcv' ,number = 10 ,repeats = 3 ,verboseIter = TRUE)
                               ,preProcess = c('center','scale')
                               
                               ,method = 'rpart'
                               ,tuneGrid = expand.grid(cp = seq(from = .0001 ,to = .01 ,by = .0005)) # complexity param used for tree pruning
                               ,metric = 'RMSE')
)

# save model
saveRDS(year_model_reg_tree ,'year_model_reg_tree.rds')

# plot results
plot(year_model_reg_tree)

############################################################################################################
# Year - random forest
############################################################################################################
system.time(
  year_model_random_forest <- train(Year ~ .
                                    ,data = train.set1
                                    
                                    ,trControl = trainControl(method = 'cv' ,number = 5 ,verboseIter = TRUE ,allowParallel = TRUE)
                                    ,preProcess = c('center','scale')
                                    
                                    
                                    ,method = 'rf'
                                    ,tuneGrid = expand.grid(mtry = c(1:20))
                                    ,metric = 'RMSE')
) # took 651 seconds on TEST set for 15 predictors, 10 folds, resulting lowest RMSE of 9.6ish on 15 predictors, after that RMSE trends up
# 12/8/2022 ran for 14 hours and didn't finish

# save model
saveRDS(year_model_random_forest ,'year_model_random_forest.rds')

############################################################################################################
# Year - NN
  # run this after everything else, due to the aditional transformations
  # tuning code left out, optimal parameters inserted into network
############################################################################################################

# training data
train.set1 <- na.omit(train.set1)
x_train <- model.matrix(Year ~ .- 1, data = train.set1) %>% 
  scale() 
y_train <- train.set1$Year

# testing data
test.set1 <- na.omit(test.set1)
x_test <- model.matrix(Year ~ .- 1, data = test.set1) %>% 
  scale() 
y_test <- test.set1$Year

# create network structure 
network <- keras_model_sequential() %>% 
  
  # layer 1
  layer_dense(units = 100
              ,activation = "relu" 
              ,input_shape = ncol(x_train)) %>%  
  layer_dropout(rate = 0) %>% # % of data to drop each layer
  
  
  # final layer
  layer_dense(units = 1)

# compile network
network %>% compile( 
  optimizer = optimizer_rmsprop()
  ,loss = 'mean_squared_error'
  ,metrics = c('mean_squared_error') 
)

history <- network %>% 
  fit(x_train, y_train, 
      epochs = 200, # number of batches, higher value here generally leads to better performance in this case, but it bottoms out
      batch_size = 90000/200, # number of training observations for calculating each gradient
      validation_split = 0.2) 

# save model
network %>% save_model_tf('C:\Users\lawor\OneDrive\Documents\year_model_nn.hdf5')


############################################################################################################
# generate test errors
############################################################################################################

# load models
year_model_elastic <- readRDS('C:/Users/lawor/OneDrive/Desktop/School/ST 563/Final project/year_model_elastic.rds')
year_model_poly <- readRDS('C:/Users/lawor/OneDrive/Desktop/School/ST 563/Final project/year_model_poly.rds')
year_model_gbm <- readRDS('C:/Users/lawor/OneDrive/Desktop/School/ST 563/Final project/year_model_gbm.rds')
year_model_gam <- readRDS('C:/Users/lawor/OneDrive/Desktop/School/ST 563/Final project/year_model_gam.rds')
year_model_knn <- readRDS('C:/Users/lawor/OneDrive/Desktop/School/ST 563/Final project/year_model_knn.rds')
year_model_reg_tree <- readRDS('C:/Users/lawor/OneDrive/Desktop/School/ST 563/Final project/year_model_reg_tree.rds')
year_model_linear <- readRDS('C:/Users/lawor/OneDrive/Desktop/School/ST 563/Final project/year_model_linear.rds')
network <- load_model_tf('C:/Users/lawor/OneDrive/Desktop/School/ST 563/Final project/Neural Net/year-model-v3.hdf5')

# NN values
test.set1 <- cbind(test.set1 ,Year_nn = predict(network, x_test))
nn_test_MAE <- mean(abs(test.set1$Year - test.set1$Year_nn))
nn_test_MSE <- mean((test.set1$Year - test.set1$Year_nn)^2)
nn_test_RMSE <- sqrt(nn_test_MSE)

train.set1 <- cbind(train.set1 ,Year_nn = predict(network, x_train))
nn_train_MAE <- mean(abs(train.set1$Year - train.set1$Year_nn))
nn_train_MSE <- mean((train.set1$Year - train.set1$Year_nn)^2)
nn_train_RMSE <- sqrt(nn_train_MSE)

# elastic values
test.set1$Year_elastic <- predict(year_model_elastic ,newdata = test.set1)
elastic_test_MAE <- mean(abs(test.set1$Year - test.set1$Year_elastic))
elastic_test_MSE <- mean((test.set1$Year - test.set1$Year_elastic)^2)
elastic_test_RMSE <- sqrt(elastic_test_MSE)

train.set1$Year_elastic <- predict(year_model_elastic ,newdata = train.set1)
elastic_train_MAE <- mean(abs(train.set1$Year - train.set1$Year_elastic))
elastic_train_MSE <- mean((train.set1$Year - train.set1$Year_elastic)^2)
elastic_train_RMSE <- sqrt(elastic_train_MSE)

# gam values
test.set1$Year_gam <- predict(year_model_gam ,newdata = test.set1) # make sure you load gam library or this fails
gam_test_MAE <- mean(abs(test.set1$Year - test.set1$Year_gam))
gam_test_MSE <- mean((test.set1$Year - test.set1$Year_gam)^2)
gam_test_RMSE <- sqrt(gam_test_MSE)

train.set1$Year_gam <- predict(year_model_gam ,newdata = train.set1)
gam_train_MAE <- mean(abs(train.set1$Year - train.set1$Year_gam))
gam_train_MSE <- mean((train.set1$Year - train.set1$Year_gam)^2)
gam_train_RMSE <- sqrt(gam_train_MSE)

# gbm values
test.set1$Year_gbm <- predict(year_model_gbm ,newdata = test.set1)
gbm_test_MAE <- mean(abs(test.set1$Year - test.set1$Year_gbm))
gbm_test_MSE <- mean((test.set1$Year - test.set1$Year_gbm)^2)
gbm_test_RMSE <- sqrt(gbm_test_MSE)

train.set1$Year_gbm <- predict(year_model_gbm ,newdata = train.set1)
gbm_train_MAE <- mean(abs(train.set1$Year - train.set1$Year_gbm))
gbm_train_MSE <- mean((train.set1$Year - train.set1$Year_gbm)^2)
gbm_train_RMSE <- sqrt(gbm_train_MSE)

# poly values
test.set1$Year_poly <- predict(year_model_poly ,newdata = test.set1)
poly_test_MAE <- mean(abs(test.set1$Year - test.set1$Year_poly))
poly_test_MSE <- mean((test.set1$Year - test.set1$Year_poly)^2)
poly_test_RMSE <- sqrt(poly_test_MSE)

train.set1$Year_poly <- predict(year_model_poly ,newdata = train.set1)
poly_train_MAE <- mean(abs(train.set1$Year - train.set1$Year_poly))
poly_train_MSE <- mean((train.set1$Year - train.set1$Year_poly)^2)
poly_train_RMSE <- sqrt(poly_train_MSE)

# knn values
test.set1$Year_knn <- predict(year_model_knn ,newdata = test.set1)
knn_test_MAE <- mean(abs(test.set1$Year - test.set1$Year_knn)) 
knn_test_MSE <- mean((test.set1$Year - test.set1$Year_knn)^2)
knn_test_RMSE <- sqrt(knn_test_MSE)

#train.set1$Year_knn <- predict(year_model_knn ,newdata = train.set1) # slow
#knn_train_MAE <- mean(abs(train.set1$Year - train.set1$Year_knn))
#knn_train_MSE <- mean((train.set1$Year - train.set1$Year_knn)^2) 

# reg tree values
test.set1$Year_reg_tree <- predict(year_model_reg_tree ,newdata = test.set1)
reg_tree_test_MAE <- mean(abs(test.set1$Year - test.set1$Year_reg_tree)) 
reg_tree_test_MSE <- mean((test.set1$Year - test.set1$Year_reg_tree)^2)
reg_tree_test_RMSE <- sqrt(reg_tree_test_MSE)

train.set1$Year_reg_tree <- predict(year_model_reg_tree ,newdata = train.set1) 
reg_tree_train_MAE <- mean(abs(train.set1$Year - train.set1$Year_reg_tree)) 
reg_tree_train_MSE <- mean((train.set1$Year - train.set1$Year_reg_tree)^2)
reg_tree_train_RMSE <- sqrt(reg_tree_train_MSE)

# linear values
test.set1$Year_linear <- predict(year_model_linear ,newdata = test.set1)
linear_test_MAE <- mean(abs(test.set1$Year - test.set1$Year_linear)) 
linear_test_MSE <- mean((test.set1$Year - test.set1$Year_linear)^2)
linear_test_RMSE <- sqrt(linear_train_MSE)

train.set1$Year_linear <- predict(year_model_linear ,newdata = train.set1) 
linear_train_MAE <- mean(abs(train.set1$Year - train.set1$Year_linear)) 
linear_train_MSE <- mean((train.set1$Year - train.set1$Year_linear)^2) 
linear_train_RMSE <- sqrt(linear_train_MSE)

############################################################################################################
# combine results
############################################################################################################
year_model_comparison <- data.frame(
  model = c('year_model_elastic')
  ,target = c('Year')
  ,test_MAE = c(elastic_test_MAE)
  ,test_MSE = c(elastic_test_MSE)
  ,test_RMSE = c(elastic_test_RMSE)
  
  ,train_MAE = c(elastic_train_MAE)
  ,train_MSE = c(elastic_train_MSE)
  ,train_RMSE = c(elastic_train_RMSE)
)

year_model_comparison[nrow(year_model_comparison)+1,] <- data.frame('year_model_gbm' ,'Year' ,gbm_test_MAE ,gbm_test_MSE ,gbm_test_RMSE ,gbm_train_MAE ,gbm_train_MSE ,gbm_train_RMSE)
year_model_comparison[nrow(year_model_comparison)+1,] <- data.frame('year_model_gam' ,'Year' ,gam_test_MAE ,gam_test_MSE ,gam_test_RMSE ,gam_train_MAE ,gam_train_MSE ,gam_train_RMSE)
year_model_comparison[nrow(year_model_comparison)+1,] <- data.frame('year_model_poly' ,'Year' ,poly_test_MAE ,poly_test_MSE ,poly_test_RMSE ,poly_train_MAE ,poly_train_MSE ,poly_train_RMSE)

year_model_comparison[nrow(year_model_comparison)+1,] <- data.frame('year_model_knn' ,'Year' ,knn_test_MAE ,knn_test_MSE ,knn_test_RMSE ,0 ,0 ,0) # excluding trianing values here due to performance
year_model_comparison[nrow(year_model_comparison)+1,] <- data.frame('year_model_reg_tree' ,'Year' ,reg_tree_test_MAE ,reg_tree_test_MSE ,reg_tree_test_RMSE ,reg_tree_train_MAE ,reg_tree_train_MSE ,reg_tree_train_RMSE)
year_model_comparison[nrow(year_model_comparison)+1,] <- data.frame('year_model_linear' ,'Year' ,linear_test_MAE ,linear_test_MSE ,linear_test_RMSE ,linear_train_MAE ,linear_train_MSE ,linear_train_RMSE)
year_model_comparison[nrow(year_model_comparison)+1,] <- data.frame('year_model_nn' ,'Year' ,nn_test_MAE ,nn_test_MSE ,nn_test_RMSE ,nn_train_MAE ,nn_train_MSE ,nn_train_RMSE)
year_model_comparison

############################################################################################################
# plots of test results
############################################################################################################

hist_test_original <- hist(test.set1$Year ,breaks = 90 ,labels = TRUE)
hist_test_elastic <- hist(test.set1$Year_elastic ,breaks = 90 ,labels = TRUE)
hist_test_gbm <- hist(test.set1$Year_gbm ,breaks = 90 ,labels = TRUE)
hist_test_gam <- hist(test.set1$Year_gam ,breaks = 90 ,labels = TRUE)
hist_test_poly <- hist(test.set1$Year_poly ,breaks = 90 ,labels = TRUE)
hist_test_knn <- hist(test.set1$Year_knn ,breaks = 90 ,labels = TRUE)
hist_test_reg_tree <- hist(test.set1$Year_reg_tree ,breaks = 90 ,labels = TRUE)
hist_test_linear <- hist(test.set1$Year_linear ,breaks = 90 ,labels = TRUE)
hist_test_nn <- hist(test.set1$Year_nn ,breaks = 90 ,labels = TRUE)

# test histograms 1
par(mfrow = c(3 ,3))
hist(test.set1$Year ,breaks = 90 ,labels = TRUE)
hist(test.set1$Year_elastic ,breaks = 90 ,labels = TRUE)
hist(test.set1$Year_gbm ,breaks = 90 ,labels = TRUE)
hist(test.set1$Year_gam ,breaks = 90 ,labels = TRUE)
hist(test.set1$Year_poly ,breaks = 90 ,labels = TRUE)
hist(test.set1$Year_knn ,breaks = 90 ,labels = TRUE)
hist(test.set1$Year_reg_tree ,breaks = 90 ,labels = TRUE)
hist(test.set1$Year_linear ,breaks = 90 ,labels = TRUE)
hist(test.set1$Year_nn ,breaks = 90 ,labels = TRUE)

# test histogram 2
par(mfrow = c(1 ,1))
plot(hist_test_original ,col = 'grey' ,labels = TRUE ,xlim = c(1920 ,2020))
plot(hist_test_elastic ,col = rgb(1 ,1 ,0 ,0.5) ,add = TRUE ,xlim = c(1920 ,2020))
plot(hist_test_gbm ,col = rgb(1 ,0 ,0 ,0.5) ,add = TRUE ,xlim = c(1920 ,2020))
plot(hist_test_gam ,col = rgb(0 ,1 ,0 ,0.5) ,add = TRUE ,xlim = c(1920 ,2020))
plot(hist_test_poly ,col = rgb(0 ,0 ,1 ,0.5) ,add = TRUE ,xlim = c(1920 ,2020))
plot(hist_test_knn ,col = rgb(1 ,0 ,1 ,0.5) ,add = TRUE ,xlim = c(1920 ,2020))
plot(hist_test_reg_tree ,col = rgb(0 ,0.7 ,0 ,0.5) ,add = TRUE ,xlim = c(1920 ,2020))
plot(hist_test_linear ,col = rgb(1 ,1 ,0.5 ,0.7) ,add = TRUE ,xlim = c(1920 ,2020))
#plot(hist_test_nn ,col = rgb(1 ,1 ,0.7 ,0.7) ,add = TRUE ,xlim = c(1920 ,2020))

legend("topleft"
       ,c('original year' ,'predicted year elastic' ,'predicted year GBM' ,'predicted year GAM' 
          ,'predicted year polynomial' ,'predicted year knn' ,'predicted year regression tree' ,'predicted year linear regression')
       ,col=c('grey' ,rgb(1 ,1 ,0 ,0.5) ,rgb(1 ,0 ,0 ,0.5) ,rgb(0 ,1 ,0 ,0.5) , rgb(0 ,0 ,1 ,0.5) ,rgb(1 ,0 ,1 ,0.5) ,rgb(1 ,1 ,1 ,0.5) ,rgb(1 ,1 ,0.5 ,0.5))
       ,lwd=10)


#######################################################################################################################################################
# Classifcation models
#######################################################################################################################################################

# load data
test <- read.csv("C:\\Users\\lawor\\OneDrive\\Desktop\\School\\ST 563\\Final project\\YearPredictTest.txt", header=TRUE)
train <- read.csv("C:\\Users\\lawor\\OneDrive\\Desktop\\School\\ST 563\\Final project\\YearPredictTrain.txt", header=TRUE)


# exclude year
train.set2 <- train[-91]
test.set2 <- test[-91]

# set class
train.set2$Class <- as.factor(train.set2$Class) 
test.set2$Class <- as.factor(test.set2$Class) 

# look at classifcation
#aggregate(train.set2$Class ,by=list(train.set2$Class) ,FUN = length)
  # only 7009 samples form prior to 1980

# down sample to address class imbalance
train.set3 <- downSample(train.set2[-91], train.set2$Class, list = FALSE, yname = 'Class')
#aggregate(train.set3$Class ,by=list(train.set3$Class) ,FUN = length)

############################################################################################################
# classification tree
  # runs fine on full training set
############################################################################################################\\

system.time(
  class_model_tree <- train(Class  ~ .
                            ,data = train.set2
                            
                            ,trControl = trainControl(method = 'cv' ,number = 10 ,verboseIter = TRUE ,allowParallel = TRUE)
                            ,preProcess = c('center','scale')
                            
                            ,method = 'rpart'
                            ,tuneGrid = expand.grid(cp = seq(.0001 ,.01 ,len = 10)) # cp is the complexity param
                            ,metric = 'Accuracy')
) 

# save model
saveRDS(class_model_tree ,'class_model_tree.rds')

# plot results
plot(class_model_tree)

# generate predictions
test.set2$Class_tree <- predict(class_model_tree ,newdata = test.set2)

# get accuracy
classtree.acc <- confusionMatrix(test.set2$Class_tree, test.set2$Class)
classtree.acc

# variable importance
varImp(class_model_tree, scale = FALSE)

############################################################################################################
# random forest
############################################################################################################
system.time(
  class_model_random_forest <- train(Class  ~ .
                                     ,data = train.set3 # down sampled set for performance
                                     
                                     ,trControl = trainControl(method = 'cv' ,number = 10 ,verboseIter = TRUE ,allowParallel = TRUE)
                                     ,preProcess = c('center','scale')
                                     
                                     ,method = 'rf'
                                     ,tuneGrid = expand.grid(mtry = seq(1 ,79 ,len = 10)) # mtry = # of random predictors
                                     ,metric = 'Accuracy')
) 

# save model
saveRDS(class_model_random_forest ,'class_model_random_forest.rds')
  #class_model_random_forest <- readRDS('C:/Users/lawor/OneDrive/Desktop/School/ST 563/Final project/class_model_random_forest.rds')

# plot results
plot(class_model_random_forest)

# generate predictions
test.set2$Class_random_forest <- predict(class_model_random_forest ,newdata = test.set2)

# get accuracy
classforest.acc <- confusionMatrix(test.set2$Class_random_forest, test.set2$Class)
classforest.acc

# variable importance
varImp(class_model_random_forest, scale = FALSE)

############################################################################################################
# bagged tree
############################################################################################################
system.time(
  class_model_bag_tree <- train(Class  ~ .
                                     ,data = train.set3 # very slow on full data, use down sampled set instead
                                     
                                     ,trControl = trainControl(method = 'cv' ,number = 10 ,verboseIter = TRUE ,allowParallel = TRUE)
                                     ,preProcess = c('center','scale')
                                     
                                     ,method = 'treebag'
                                     ,metric = 'Accuracy') # there are no hyper params to tune here
) 

# save model
saveRDS(class_model_bag_tree ,'class_model_bag_tree.rds')


# generate predictions
test.set2$Class_bag_tree <- predict(class_model_bag_tree ,newdata = test.set2)

# get accuracy
classbagging.acc <- confusionMatrix(test.set2$Class_bag_tree, test.set2$Class)
classbagging.acc

# variable importance
varImp(class_model_bag_tree, scale = FALSE)

############################################################################################################
# boosted tree
############################################################################################################
system.time(
  class_model_boosted_tree <- train(Class  ~ .
                                    ,data = train.set3 # very slow on full data, use down sampled set instead
                                    
                                    ,trControl = trainControl(method = 'cv' ,number = 10 ,verboseIter = TRUE ,allowParallel = TRUE)
                                    ,preProcess = c('center','scale')
                                    
                                    ,method = 'gbm'
                                    ,tuneGrid = expand.grid(n.trees = c(25,50,100,150,200) 
                                                            ,interaction.depth = seq(from = 1 ,to = 4 , by = 1)
                                                            ,shrinkage = c(.01 ,.1) #seq(from = .01 ,to = 1 ,by = .4)
                                                            ,n.minobsinnode = 10)
                                    ,metric = 'Accuracy')
) 

# save model
saveRDS(class_model_boosted_tree ,'class_model_boosted_tree.rds')

# generate predictions
test.set2$Class_boosted_tree <- predict(class_model_boosted_tree ,newdata = test.set2)

# get accuracy
classboosted.acc <- confusionMatrix(test.set2$Class_boosted_tree, test.set2$Class)
classboosted.acc

############################################################################################################
# QDA
############################################################################################################
system.time(
  class_model_qda<- train(Class  ~ .
                          ,data = train.set2
                                    
                           ,trControl = trainControl(method = 'cv' ,number = 10 ,verboseIter = TRUE ,allowParallel = TRUE)
                           #,preProcess = c('center','scale')
                                    
                          ,method = 'qda'
                          ,metric = 'Accuracy')
) 

# save model
saveRDS(class_model_qda,'class_model_qda.rds')

# generate predictions
test.set2$Class_qda <- predict(class_model_qda ,newdata = test.set2)

# get accuracy
classqda.acc <- confusionMatrix(test.set2$Class_qda, test.set2$Class)
classqda.acc

############################################################################################################
# lda
############################################################################################################
system.time(
  class_model_lda<- train(Class  ~ .
                          ,data = train.set2
                          
                          ,trControl = trainControl(method = 'cv' ,number = 10 ,verboseIter = TRUE ,allowParallel = TRUE)
                          #,preProcess = c('center','scale')
                          
                          ,method = 'lda'
                          ,metric = 'Accuracy')
) 

# save model
saveRDS(class_model_lda,'class_model_lda.rds')

# generate predictions
test.set2$Class_lda <- predict(class_model_lda ,newdata = test.set2)

# get accuracy
classlda.acc <- confusionMatrix(test.set2$Class_lda, test.set2$Class)
classlda.acc # 0.6567

############################################################################################################
# nb
############################################################################################################
system.time(
  class_model_nb <- train(Class  ~ .
                          ,data = train.set2
                          
                          ,trControl = trainControl(method = 'cv' ,number = 10 ,verboseIter = TRUE ,allowParallel = TRUE)
                          #,preProcess = c('center','scale')
                          
                          ,method = 'nb'
                          ,metric = 'Accuracy')
) 

# save model
saveRDS(class_model_nb,'class_model_nb.rds')

# generate predictions
test.set2$Class_nb <- predict(class_model_nb ,newdata = test.set2) # running @@@@@@@@@@@@@@

# get accuracy
classnb.acc <- confusionMatrix(test.set2$Class_nb, test.set2$Class)
classnb.acc

############################################################################################################
# logistic regression
############################################################################################################
system.time(
  class_model_logistic <- train(Class  ~ .
                                ,data = train.set2
                                
                                ,trControl = trainControl(method = 'cv' ,number = 10 ,verboseIter = TRUE ,allowParallel = TRUE)
                                ,preProcess = c('center','scale')
                                
                                ,method = 'multinom'
                                ,family = 'binomial'
                                ,metric = 'Accuracy')
)

# save model
saveRDS(class_model_logistic ,'class_model_logistic.rds')

# generate predictions
test.set2$Class_logistic <- predict(class_model_logistic ,newdata = test.set2) 

# get accuracy
classlogistic.acc <- confusionMatrix(test.set2$Class_logistic, test.set2$Class)
classlogistic.acc # 0.6639

############################################################################################################
# Combine model predictions (majority vote)
############################################################################################################

# get majority vote form the classifiers
test.set2 <- cbind(test.set2, do.call(rbind, apply(test.set2[-1], 1, function(x) {
  x1 <- table(x)
  data.frame(Count = max(x1), Class_stacked=names(x1)[which.max(x1)])})))

# set data type
test.set2$Class_stacked <- as.factor(test.set2$Class_stacked) 

# get accuracy
classstacked.acc <- confusionMatrix(test.set2$Class_stacked ,test.set2$Class)
classstacked.acc


############################################################################################################
# combine results
############################################################################################################

# load models
  # only need ot run this if you don't rebuild the models above
class_model_tree <- readRDS('C:/Users/lawor/OneDrive/Desktop/School/ST 563/Final project/class_model_tree.rds')
class_model_random_forest <- readRDS('C:/Users/lawor/OneDrive/Desktop/School/ST 563/Final project/class_model_random_forest.rds')
class_model_bag_tree <- readRDS('C:/Users/lawor/OneDrive/Desktop/School/ST 563/Final project/class_model_bag_tree.rds')
class_model_boosted_tree <- readRDS('C:/Users/lawor/OneDrive/Desktop/School/ST 563/Final project/class_model_boosted_tree.rds')
class_model_qda <- readRDS('C:/Users/lawor/OneDrive/Desktop/School/ST 563/Final project/class_model_qda.rds')
class_model_lda <- readRDS('C:/Users/lawor/OneDrive/Desktop/School/ST 563/Final project/class_model_lda.rds')
class_model_nb <- readRDS('C:/Users/lawor/OneDrive/Desktop/School/ST 563/Final project/class_model_nb.rds')
class_model_logistic <- readRDS('C:/Users/lawor/OneDrive/Desktop/School/ST 563/Final project/class_model_logistic.rds')


# combine accuracy measures
class_model_comparison <- data.frame(
  model = c('Classification tree')
  ,target = c('Class')
  ,test_accuracy = c(classtree.acc$overall['Accuracy'])
)

class_model_comparison[nrow(class_model_comparison)+1,] <- data.frame('class_model_random_forest' ,'Class' ,classforest.acc$overall['Accuracy'])
class_model_comparison[nrow(class_model_comparison)+1,] <- data.frame('class_model_bagged_tree' ,'Class' ,classbagging.acc$overall['Accuracy'])
class_model_comparison[nrow(class_model_comparison)+1,] <- data.frame('class_model_boosted_tree' ,'Class' ,classboosted.acc$overall['Accuracy'])
class_model_comparison[nrow(class_model_comparison)+1,] <- data.frame('class_model_qda' ,'Class' ,classqda.acc$overall['Accuracy'])
class_model_comparison[nrow(class_model_comparison)+1,] <- data.frame('class_model_lda' ,'Class' ,classlda.acc$overall['Accuracy'])
class_model_comparison[nrow(class_model_comparison)+1,] <- data.frame('class_model_nb' ,'Class' ,classnb.acc$overall['Accuracy'])
class_model_comparison[nrow(class_model_comparison)+1,] <- data.frame('class_model_logistic' ,'Class' ,classlogistic.acc$overall['Accuracy'])
class_model_comparison[nrow(class_model_comparison)+1,] <- data.frame('Majority Vote' ,'Class' ,classstacked.acc$overall['Accuracy'])

class_model_comparison


