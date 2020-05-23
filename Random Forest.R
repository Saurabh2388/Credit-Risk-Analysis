set.seed(1234)
training_index <- sample(nrow(fullset_final)*0.66)
test_index     <- setdiff(seq(2:nrow(fullset_final)), training_index )
training_data  <- fullset_final[training_index, ]
test_data      <- fullset_final[test_index, ]
---------------------------
        Model - 1
model2 <- randomForest(TARGET ~ ., data = training_data, ntree = 500, mtry = 10, importance = TRUE)
model2
predTrain <- predict(model2, training_data, type = "class")
# Checking classification accuracy
table(predTrain, training_data$TARGET) 
predValid <- predict(model2, test_data, type = "class")
# Checking classification accuracy
mean(predValid == test_data$TARGET)                    
table(predValid,test_data$TARGET)
library(randomForest)
importance(model2) 
varImpPlot(model2) 
varImpPlot(model2) 
summary(model2)

require(pROC)
rf.roc<-roc(training_data$TARGET,model2$votes[,2])
plot(rf.roc,print.acu=TRUE)
auc(rf.roc)
---------------------------
        Model 2.
model3 <- randomForest(TARGET ~ CODE_GENDER+Age+AMT_CREDIT.x+AMT_CREDIT_SUM+REGION_RATING_CLIENT+EXT_SOURCE_1+EXT_SOURCE_2+EXT_SOURCE_3+NAME_EDUCATION_TYPE+OCCUPATION_TYPE+ORGANIZATION_TYPE+OWN_CAR_AGE, data = training_data, ntree = 500, mtry = 3, importance = TRUE)
require(pROC)
rf.roc1<-roc(training_data$TARGET,model3$votes[,2])
plot(rf.roc1,print.acu=TRUE)
plot(rf.roc1, print.auc = TRUE, main ="AUC for ROC curve - Random Forest mtry = 3 and 12 features")
auc(rf.roc1)
predTrain <- predict(model3, training_data, type = "class")
# Checking classification accuracy
table(predTrain, training_data$TARGET) 
predValid <- predict(model3, test_data, type = "class")
# Checking classification accuracy
mean(predValid == test_data$TARGET)                    
table(predValid,test_data$TARGET)
library(randomForest)
importance(model3) 
varImpPlot(model3) 
summary(model3)
plot(rf.roc, print.auc = TRUE, main ="AUC for ROC curve - Random Forest mtry = 10")
