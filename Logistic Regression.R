## Model 1
model_log = glm(data = fullset_final,TARGET ~ CODE_GENDER + Age + AMT_CREDIT.x + OCCUPATION_TYPE + NAME_EDUCATION_TYPE + NAME_INCOME_TYPE + FLAG_OWN_CAR + ORGANIZATION_TYPE,family=binomial(link=logit))
CODE_GENDER + Age + AMT_CREDIT.x + OCCUPATION_TYPE + NAME_EDUCATION_TYPE + NAME_INCOME_TYPE + FLAG_OWN_CAR + ORGANIZATION_TYPE
summary(model_log)
--------------------------------
        ## Model 2
        model_log2 = glm(data = fullset_final,TARGET ~ CODE_GENDER + Age + AMT_CREDIT.x + OCCUPATION_TYPE + NAME_EDUCATION_TYPE + AMT_CREDIT_SUM + REGION_RATING_CLIENT + EXT_SOURCE_1 + EXT_SOURCE_2 + EXT_SOURCE_3 + FLAG_OWN_CAR + ORGANIZATION_TYPE + FLAG_OWN_CAR + OWN_CAR_AGE,family=binomial(link=logit))
summary(model_log2)
model_log2_red <- glm(data = fullset_final, TARGET ~ 1, family=binomial(link=logit))
1-(logLik(model_log2))/(logLik(model_log2_red))
## PLOTTING ROC CURVES – Model 1
fit_glm <- glm(TARGET ~ CODE_GENDER + Age + AMT_CREDIT.x + OCCUPATION_TYPE + NAME_EDUCATION_TYPE + NAME_INCOME_TYPE + FLAG_OWN_CAR + ORGANIZATION_TYPE, training_data, family=binomial(link="logit"))
glm_link_scores <- predict(fit_glm, test_data, type="link")
glm_prob_scores <- predict(fit_glm, test_data, type="terms")
glm_response_scores <- predict(fit_glm, test_data, type="response")
roc_full_resolution <- roc(test_data$TARGET, glm_response_scores)
rounded_scores <- round(glm_response_scores, digits=2)
roc_rounded <- roc(test_data$TARGET, rounded_scores)
plot(roc_full_resolution, print.auc=TRUE, main = "AUC Logistic Regression Model - 8 features")
--------------------------------
        ## PLOTTING ROC CURVES – Model 2
        fit_glm3 <- glm(TARGET ~ CODE_GENDER + Age + AMT_CREDIT.x + OCCUPATION_TYPE + NAME_EDUCATION_TYPE + AMT_CREDIT_SUM + REGION_RATING_CLIENT + EXT_SOURCE_1 + EXT_SOURCE_2 + EXT_SOURCE_3  + ORGANIZATION_TYPE + OWN_CAR_AGE , training_data, family=binomial(link="logit"))
glm_link_scores3 <- predict(fit_glm3, test_data, type="link")
glm_prob_scores3 <- predict(fit_glm3, test_data, type="terms")
glm_response_scores3 <- predict(fit_glm3, test_data, type="response")
roc_full_resolution3 <- roc(test_data$TARGET, glm_response_scores3)
rounded_scores3 <- round(glm_response_scores3, digits=2)
roc_rounded3 <- roc(test_data$TARGET, rounded_scores3)
plot(roc_full_resolution3, print.auc=TRUE, main = "AUC for ROC curve - Logistic Regression Model - 12 features")
