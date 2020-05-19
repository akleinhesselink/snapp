# load libraries (first libraries need to be installed using install.packages('library_name'))
library(tidyverse)
library(caret)
library(nnet)
library(xgboost)
library(Ckmeans.1d.dp)
library(RANN)
library(e1071)
library(randomForest)

# read the data
snapp <- read.csv('C:\\Users\\Dursoa\\Documents\\Snapp\\CSchallenge\\multinomial-model\\snapp_responses_out-corr-only100spp-toErol.csv')

# keep useful columns
col <- c('score', 'mivs', 'family', 'global_region', 'difficulty')
data <- snapp[, (colnames(snapp) %in% col), drop=FALSE]

str(data)

# # centering and scaling numerical columns (if any)
# preprocvalues <- preProcess(data, method=c('center', 'scale'))
# train_processed <- predict(preprocvalues, data)

# converting every categorical variable to numerical using dummy variables
# full rank i.e. the first alphabetical category from each nominal variables are removed (i.e. the base regression)
dmy <- dummyVars(' ~ .', data=data, fullRank=T)
data_transformed <- data.frame(predict(dmy, newdata=data))

# converting the dependent variable to categorical
data_transformed$score <- as.factor(data_transformed$score)

# split the data into training and test set
set.seed(123)
training.samples <- createDataPartition(data_transformed$score, p=0.8, list=FALSE)
train_data  <- data_transformed[training.samples, ]
test_data <- data_transformed[-training.samples, ]
outcomeName <- 'score'

# # recursive feature selection (if need be)
# control <- rfeControl(functions=rfFuncs, method='repeatedcv', repeats=3, verbose=FALSE)
# predictors <- names(train_data)[!names(train_data) %in% outcomeName]
# score_pred_profile <- rfe(train_data[, predictors], train_data[, outcomeName], rfeControl=control)
# score_pred_profile
# predictors <- c()

# setting Cross-Validation
fitControl <- trainControl(method='repeatedcv', number=5, repeats=5)

# training the models with tune length (no need to give specific values for each parameter)
model_multinom <- train(train_data[, !(colnames(train_data)==outcomeName)], train_data[, outcomeName], method='multinom', trControl=fitControl, tuneLength=10, verbose=FALSE)
model_xgbTree <- train(train_data[, !(colnames(train_data)==outcomeName)], train_data[, outcomeName], method='xgbTree', trControl=fitControl, tuneLength=10, verbose=FALSE)

# summarizing the model
print(model_multinom)
print(model_xgbTree)

# if we want to fine tune specific parameters
# parameters that can be tuned for each model
modelLookup(model='multinom')
modelLookup(model='xgbTree')

# grid search (with specific values for the targeted parameters)
grid_xgbTree <- expand.grid(n.trees=c(10, 20, 50, 100, 500, 1000), shrinkage=c(0.01, 0.05, 0.1, 0.5), n.minobsinnode=c(3, 5, 10), interaction.depth=c(1, 5, 10))

# training the models
model_grid_xgbTree <- train(train_data[, predictors], train_data[, outcomeName], method='xgbTree', trControl=fitControl, tuneGrid=grid_xgbTree, verbose=FALSE)

# summarizing the model
print(model_multinom)
print(model_xgbTree)
print(model_grid_xgbTree)

# checking variable importance for each model
varImp(object=model_multinom, scale = FALSE)
varImp(object=model_xgbTree, scale = FALSE)
varImp(object=model_grid_xgbTree, scale = FALSE)

# plotting Variable importance for each model
plot(varImp(object=model_multinom, scale = FALSE), main='Multinomial Regression - Variable Importance')
plot(varImp(object=model_xgbTree, scale = FALSE), main='XGBoost - Variable Importance')
plot(varImp(object=model_grid_xgbTree, scale = FALSE), main='XGBoost with Grid Search - Variable Importance')

# predictions for each model
predictions_multinom <- predict.train(object=model_multinom, test_data[, predictors], type='raw')
table(predictions_multinom)
confusionMatrix(predictions_multinom, test_data[, outcomeName])
predictions_xgbTree <- predict.train(object=model_xgbTree, test_data[, predictors], type='raw')
table(predictions_xgbTree)
confusionMatrix(predictions_xgbTree, test_data[, outcomeName])
model_grid_xgbTree <- predict.train(object=model_xgbTree, test_data[, predictors], type='raw')
table(model_grid_xgbTree)
confusionMatrix(model_grid_xgbTree, test_data[, outcomeName])