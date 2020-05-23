library(caTools)
library(tidyverse)
library(caret)
library(knitr)
library(xgboost)
library(LightGBM)
set.seed(1122) 
sample = sample.split(fullset_final$TARGET, SplitRatio = .70)
trn_gbm = subset(fullset_final, sample == TRUE)
test_gbm  = subset(fullset_final, sample == FALSE)
----------------
        ##Transform to Numeric
        trn_gbm2 <- trn_gbm %>%
        select(-TARGET)
feat <- colnames(trn_gbm2)
for (a in feat) {
        if ((class(trn_gbm2[[a]])=="factor") || (class(trn_gbm2[[a]])=="character")) {
                levels <- unique(trn_gbm2[[a]])
                trn_gbm2[[a]] <- as.numeric(factor(trn_gbm2[[a]], levels=levels))
        }
}
trn_gbm2$TARGET = NULL
trn_gbm2$TARGET = as.factor(trn_gbm$TARGET)
levels(trn_gbm2$TARGET) = make.names(unique(trn_gbm2$TARGET))
test_gbm2 = test_gbm 
feat <- colnames(test_gbm2)
for (b in feat) {
        if ((class(test_gbm2[[b]])=="factor") || (class(test_gbm2[[b]])=="character")) {
                levels <- unique(test_gbm2[[b]])
                test_gbm2[[b]] <- as.numeric(factor(test_gbm2[[b]], levels=levels))
        }
}
----------------
        ##Model
        form = TARGET ~ .
fitControl <- trainControl(method="none",number = 5,  classProbs = TRUE, summaryFunction = twoClassSummary)
xgb.Grid <- expand.grid(nrounds = 100,
                        max_depth = 7,
                        eta = .05,
                        gamma = 0,
                        colsample_bytree = .8,
                        min_child_weight = 1,
                        subsample = 1)

set.seed(132)
gbm_1 = train(form, data = trn_gbm2,
              method = "xgbTree",trControl = fitControl,
              tuneGrid = xgb.Grid,na.action = na.pass,metric="ROC"
)
gbm_1
----------------
        ##Variable Imp
        imp = varImp(gbm_1)
var_imp <- data.frame(Variables = row.names(imp[[1]]), 
                      Importance = round(imp[[1]]$Overall,2))
----------------
        # Create ranks
        rank_imp <- var_imp %>%
        mutate(Rank = paste0('#',dense_rank(desc(Importance)))) %>%
        head(25)
rank_impfull = rank_imp
ggplot(rank_imp, aes(x = reorder(Variables, Importance), 
                     y = Importance)) +
        geom_bar(stat='identity',colour="white", fill = “dodgerblue3”) +
        geom_text(aes(x = Variables, y = 1, label = Rank),
                  hjust=0, vjust=.5, size = 4, colour = 'black',
                  fontface = 'bold') +
        labs(x = 'Variables', title = 'Relative Variable Importance') +
        coord_flip() + 
        theme_bw()
----------------
        ##prediction
        pred = predict(gbm_1,test_gbm2,na.action=na.pass,type="prob")
sol <- data.frame('SK_ID_CURR' = as.integer(test_gbm$SK_ID_CURR), 'TARGET' = pred[,2])
##Preprocessing
full <- bind_rows(trn_gbm,test_gbm)
Target <- trn_gbm$TARGET
Id <- test_gbm$SK_ID_CURR
full[,c('SK_ID_CURR','TARGET')] <- NULL
chr <- full[,sapply(full, is.character)]
num <- full[,sapply(full, is.numeric)]
chr[is.na(chr)] <- "Not Available"
fac <- chr %>% 
        lapply(as.factor) %>% 
        as_data_frame()
full <- bind_cols(fac, num)
rm(chr, fac, num)
full[is.na(full)] <- 0
num <- train[, sapply(train,is.numeric)]
rm(train, test)
train <- full[1:length(Target),]
test <- full[(length(Target)+1):nrow(full),]
----------------
        ##Create the Data Partition
        set.seed(123)
intrn <- createDataPartition(Target, p=.9, list = F)
tr1 <- train[intrn,]
va1 <- train[-intrn,]
tr1_ta <- Target[intrn]
va1_ta <- Target[-intrn]
----------------
        ##Create the Model
        ```{r  message=FALSE, warning=FALSE}
lgb.trn = lgb.Dataset(data.matrix(tr1), label = tr1_ta)
lgb.val= lgb.Dataset(data.matrix(va1), label = va1_ta)
params = list(
        objective = "binary"
        , metric = "auc"
        , min_data_in_leaf = 1
        , min_sum_hessian_in_leaf = 100
        , feature_fraction = 1
        , bagging_fraction = 1
        , bagging_freq = 0
)
model1_gb <- lgb.trn(
        params = params
        , data = lgb.train
        , valids = list(val = lgb.val)
        , learning_rate = 0.05
        , num_leaves = 7
        , num_threads = 2
        , nrounds = 3000
        , early_stopping_rounds = 200
        , eval_freq = 50
)
----------------
        ##Importance 
        gbm1_impr = lgb.importance(model1_gb, percentage = TRUE) %>% head(6)
gbm1_impr %>% kable()
var_imp <- data.frame(Variables = gbm1_impr$Feature, 
                      Importance = gbm1_impr$Gain)
# Create a rank variable based on importance
rank_imp <- var_imp %>%
        mutate(Rank = paste0('#',dense_rank(desc(Importance)))) %>%
        head(6)
rank_impfull = rank_imp
ggplot(rank_imp, aes(x = reorder(Variables, Importance), 
                     y = Importance)) +
        geom_bar(stat='identity',colour="white", fill = fillColor2) +
        geom_text(aes(x = Variables, y = 0.1, label = Rank),
                  hjust=0, vjust=.5, size = 4, colour = 'black',
                  fontface = 'bold') +
        labs(x = 'Variables', title = 'Relative Variable Importance') +
        coord_flip() + 
        theme_bw()
----------------
        ##pred
        gb_pred <- predict(model1_gb, data = data.matrix(test), n = model1_gb$best_iter)
result <- data.frame(SK_ID_CURR = Id, TARGET = lgb_pred)
