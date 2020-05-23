## Loading relevant packages.
library(tidyverse)
library(xgboost)
library(magrittr)
library(Hmisc)
library(plotly)
library(GGally)
library(skimr)
library(data.table)
library(caret)
library(DT)
library(viridis)
library(mlr)
library(outliers)
library(lubridate)
library(stringi)
library(pROC)
library(randomForest)
library(xgboost)
----------------------------
        ## Loading Cap function for outlier treatment
        cap <- function(x){
                quantiles <- quantile( x, c(.05, 0.25, 0.75, .95 ) )
                x[ x < quantiles[2] - 1.5*IQR(x) ] <- quantiles[1]
                x[ x > quantiles[3] + 1.5*IQR(x) ] <- quantiles[4]
                x
        }
        ----------------------------
                ## Loading Datasets
                burbal <- read_csv("bureau_balance.csv") 
                bur <- read_csv("bureau.csv")
                ccbal <- read_csv("credit_card_balance.csv")
                payments <- read_csv("installments_payments.csv") 
                pcbal <- read_csv("POS_CASH_balance.csv")
                prev <- read_csv("previous_application.csv")
                train <- read_csv("application_train.csv") 
                test <- read_csv("application_test.csv")
                ----------------------------
                        ## Joining Tables
                        total_burbal <- burbal %>%
                        mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
                        group_by(SK_ID_BUREAU) %>% 
                        summarise_all(funs(mean, .args = list(na.rm = TRUE))) 
                rm(burbal); gc()
                
                total_bur <- bur %>% 
                        left_join(total_burbal, by = "SK_ID_BUREAU") %>% 
                        select(-SK_ID_BUREAU) %>% 
                        mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
                        group_by(SK_ID_CURR) %>% 
                        summarise_all(funs(mean, .args = list(na.rm = TRUE)))
                rm(bur, total_burbal); gc()
                total_ccbal <- ccbal %>% 
                        select(-SK_ID_PREV) %>% 
                        mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
                        group_by(SK_ID_CURR) %>% 
                        summarise_all(funs(mean, .args = list(na.rm = TRUE)))
                rm(ccbal); gc()
                total_payments <- payments %>% 
                        select(-SK_ID_PREV) %>% 
                        mutate(PAYMENT_DIFF = AMT_INSTALMENT - AMT_PAYMENT,
                               DPD = DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT,
                               DBD = DAYS_INSTALMENT - DAYS_ENTRY_PAYMENT,
                               DPD = ifelse(DPD > 0, DPD, 0),
                               DBD = ifelse(DBD > 0, DBD, 0)) %>% 
                        group_by(SK_ID_CURR) %>% 
                        summarise_all(funs(mean, .args = list(na.rm = TRUE))) 
                rm(payments); gc()
                total_pcbal <- pcbal %>% 
                        select(-SK_ID_PREV) %>% 
                        mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
                        group_by(SK_ID_CURR) %>% 
                        summarise_all(funs(mean, .args = list(na.rm = TRUE)))
                rm(pcbal); gc()
                total_prev <- prev %>%
                        select(-SK_ID_PREV) %>% 
                        mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
                        mutate(DAYS_FIRST_DRAWING = ifelse(DAYS_FIRST_DRAWING == 365243, NA, DAYS_FIRST_DRAWING),
                               DAYS_FIRST_DUE = ifelse(DAYS_FIRST_DUE == 365243, NA, DAYS_FIRST_DUE),
                               DAYS_LAST_DUE_1ST_VERSION = ifelse(DAYS_LAST_DUE_1ST_VERSION == 365243, NA, DAYS_LAST_DUE_1ST_VERSION),
                               DAYS_LAST_DUE = ifelse(DAYS_LAST_DUE == 365243, NA, DAYS_LAST_DUE),
                               DAYS_TERMINATION = ifelse(DAYS_TERMINATION == 365243, NA, DAYS_TERMINATION)) %>% 
                        group_by(SK_ID_CURR) %>% 
                        summarise_all(funs(mean, .args = list(na.rm = TRUE))) 
                rm(prev); gc()
                fullset <- train %>% 
                        left_join(total_bur, by = "SK_ID_CURR") %>% 
                        left_join(total_ccbal, by = "SK_ID_CURR") %>% 
                        left_join(total_payments, by = "SK_ID_CURR") %>% 
                        left_join(total_pcbal, by = "SK_ID_CURR") %>% 
                        left_join(total_prev, by = "SK_ID_CURR") %>% 
                        mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
                        mutate(na = apply(., 1, function(x) sum(is.na(x)))) 
                rm(func, total_bur, total_ccbal, total_payments, total_pcbal, total_prev); gc()
                ----------------------------
                        ## Dealing with missing values
                        colSums(is.na(fullset))
                ----------------------------
                        ## Imputing mean values in the full set + check for special
                        fullset_impute <- data.frame(
                                sapply(
                                        fullset,
                                        function(x) ifelse(is.na(x),
                                                           mean(x, na.rm = TRUE),
                                                           x)))
                colSums(is.na(fullset_impute))
                is.special <- function(x){
                        if (is.numeric(x)) !is.finite(x) else is.na(x)
                }
                sum(is.special(fullset_impute))
                sum(is.na(fullset))
                ----------------------------
                        ## Feature selection
                        options(java.parameters = "-Xmx4096m")
                library(rJava)
                library(Fselector)
                fin <- fullset_final %>% select(-SK_ID_CURR)
                weights<- chi.squared(TARGET~., fin)
                ----------------------------
                        ## Subsetting Final set based on top 100 by attribute importance weight in chi-squared feature test.
                        fullset_final <- fullset_impute %>%    select(SK_ID_CURR,TARGET, NAME_CONTRACT_TYPE.x, CODE_GENDER, FLAG_OWN_CAR, CNT_CHILDREN, AMT_CREDIT.x, AMT_GOODS_PRICE.x, NAME_INCOME_TYPE,  NAME_EDUCATION_TYPE,  NAME_HOUSING_TYPE, REGION_POPULATION_RELATIVE,  DAYS_BIRTH,DAYS_EMPLOYED, DAYS_REGISTRATION, DAYS_ID_PUBLISH, OWN_CAR_AGE,FLAG_EMP_PHONE,  FLAG_WORK_PHONE, FLAG_PHONE, OCCUPATION_TYPE,               REGION_RATING_CLIENT,REGION_RATING_CLIENT_W_CITY,HOUR_APPR_PROCESS_START.x,REG_CITY_NOT_LIVE_CITY,    REG_CITY_NOT_WORK_CITY, LIVE_CITY_NOT_WORK_CITY, ORGANIZATION_TYPE, EXT_SOURCE_1,EXT_SOURCE_2,EXT_SOURCE_3, APARTMENTS_AVG, ELEVATORS_AVG, FLOORSMAX_AVG, FLOORSMIN_AVG, LIVINGAREA_AVG,APARTMENTS_MODE, ELEVATORS_MODE, FLOORSMAX_MODE, LIVINGAREA_MODE,APARTMENTS_MEDI,ELEVATORS_MEDI,FLOORSMAX_MEDI,FLOORSMIN_MEDI,LIVINGAREA_MEDI, TOTALAREA_MODE, DEF_30_CNT_SOCIAL_CIRCLE, DEF_60_CNT_SOCIAL_CIRCLE, DAYS_LAST_PHONE_CHANGE, FLAG_DOCUMENT_3, FLAG_DOCUMENT_6, AMT_REQ_CREDIT_BUREAU_YEAR, CREDIT_ACTIVE, DAYS_CREDIT, DAYS_CREDIT_ENDDATE, DAYS_ENDDATE_FACT, AMT_CREDIT_SUM,  DAYS_CREDIT_UPDATE, MONTHS_BALANCE.x, STATUS, MONTHS_BALANCE.y, AMT_BALANCE,AMT_DRAWINGS_ATM_CURRENT,AMT_DRAWINGS_CURRENT, AMT_INST_MIN_REGULARITY, AMT_RECEIVABLE_PRINCIPAL,AMT_RECIVABLE,AMT_TOTAL_RECEIVABLE,              CNT_DRAWINGS_ATM_CURRENT,CNT_DRAWINGS_CURRENT,CNT_DRAWINGS_POS_CURRENT,NUM_INSTALMENT_VERSION,DAYS_INSTALMENT,      DAYS_ENTRY_PAYMENT, AMT_INSTALMENT, AMT_PAYMENT, PAYMENT_DIFF, DBD,MONTHS_BALANCE,CNT_INSTALMENT, CNT_INSTALMENT_FUTURE, AMT_ANNUITY, AMT_APPLICATION,AMT_DOWN_PAYMENT,HOUR_APPR_PROCESS_START.y,RATE_DOWN_PAYMENT, NAME_CASH_LOAN_PURPOSE, NAME_CONTRACT_STATUS, DAYS_DECISION,NAME_PAYMENT_TYPE, CODE_REJECT_REASON, NAME_TYPE_SUITE.y, NAME_GOODS_CATEGORY, NAME_PRODUCT_TYPE, CHANNEL_TYPE,CNT_PAYMENT,PRODUCT_COMBINATION,DAYS_FIRST_DRAWING,DAYS_FIRST_DUE,
                                                                      DAYS_LAST_DUE_1ST_VERSION,DAYS_LAST_DUE,DAYS_TERMINATION)
                ----------------------
                        ## Factoring and binning – Tidying data.
                        fullset_final$NAME_CONTRACT_TYPE.x <- factor(fullset_final$NAME_CONTRACT_TYPE.x, levels = c("1", "2"), labels = c("Cash", "Revolving"))
                fullset_final$CODE_GENDER <- factor(fullset_final$CODE_GENDER, levels = c("1", "2","3"), labels = c("M", "F","Not Specified"))
                fullset_final$FLAG_OWN_CAR <- factor(fullset_final$FLAG_OWN_CAR, levels = c("1", "2"), labels = c("Y", "N"))
                fullset_final$CNT_CHILDREN <- as.factor(ifelse(fullset_final$CNT_CHILDREN == 0,"0",
                                                               ifelse(fullset_final$CNT_CHILDREN == 1,"1",">=2")))
                fullset_final$AMT_CREDIT.x <- fullset_final$AMT_CREDIT.x %>% cap()
                fullset_final$AMT_CREDIT.x <- cut(fullset_final$AMT_CREDIT.x, breaks = c(0, 100000, 250000, 400000,550000, 700000, 850000, Inf), labels = c("0-100k","101k - 250k","251k - 400k","401k - 550k","551k-700k","700k-850K", "850k+"))
                fullset_final$AMT_GOODS_PRICE.x <- fullset_final$AMT_GOODS_PRICE.x %>% cap()
                fullset_final$AMT_GOODS_PRICE.x <- cut(fullset_final$AMT_GOODS_PRICE.x, breaks = c(0, 100000, 250000, 400000,550000, 700000, 850000, Inf), labels = c("0-100k","101k - 250k","251k - 400k","401k - 550k","551k-700k","700k-850K", "850k+"))
                fullset_final$NAME_INCOME_TYPE <- factor(fullset_final$NAME_INCOME_TYPE, levels = c("1", "2","3","4","5","6","7","8"), labels = c("Businessman","Commercial-associate","Maternity-leave","Pensioner","State-servant","Student","Unemployed","Working"))
                fullset_final$NAME_EDUCATION_TYPE <- factor(fullset_final$NAME_EDUCATION_TYPE, levels = c("1", "2","3","4","5"), labels = c("Academic degree","Higher education","Incomplete higher","Lower secondary","Secondary/secondary special"))
                fullset_final$NAME_HOUSING_TYPE <- factor(fullset_final$NAME_HOUSING_TYPE, levels = c("1", "2","3","4","5","6"), labels = c("Co-op apartment","House / apartment","Municipal apartment","Office apartment","Rented apartment","With parents" ))
                fullset_final$REGION_POPULATION_RELATIVE <- fullset_final$REGION_POPULATION_RELATIVE %>% cap()
                fullset_final$REGION_POPULATION_RELATIVE <- cut(fullset_final$REGION_POPULATION_RELATIVE, breaks = c(0, .01, .02, .03,.04,Inf), labels = c("Lowest","Lower","Moderate","Higher","Highest"))
                colnames(fullset_final)[13] <- "Age"
                fullset_final$Age <- (fullset_final$Age/365) * (-1)
                fullset_final$Age <- round(fullset_final$Age)
                fullset_final$Age <- cut(fullset_final$Age, breaks = c(0, 25, 35, 45,55,Inf), labels = c("<25","26 - 35","36-45","46-55","55+"))
                colnames(fullset_final)[14] <- "Years_Employed"
                fullset_final$Years_Employed <- round((fullset_final$Years_Employed/365) * (-1))
                fullset_final$Years_Employed <- cut(fullset_final$Years_Employed, breaks = c(-Inf, 1, 3, 5, 8,10,Inf), labels = c("<1","1 - 3","3 - 5","5 - 8","8 - 10", "10 +"))
                colnames(fullset_final)[15] <- "Years_Registered"
                fullset_final$Years_Registered <- round((fullset_final$Years_Registered/365) * (-1))
                fullset_final$Years_Registered <- cut(fullset_final$Years_Registered, breaks = c(-Inf, 1, 3, 5, 8,10,Inf), labels = c("<1","1 - 3","3 - 5","5 - 8","8 - 10", "10 +"))
                colnames(fullset_final)[16] <- "Years_IDPUB"
                fullset_final$Years_IDPUB <- round((fullset_final$Years_IDPUB / 365) * (-1))
                fullset_final$Years_IDPUB <- cut(fullset_final$Years_IDPUB, breaks = c(-Inf, 1, 3, 5, 8,10,13,Inf), labels = c("<1","1 - 3","3 - 5","5 - 8","8 - 10", "10 - 13","13 +"))
                fullset_final$OWN_CAR_AGE <- fullset_final$OWN_CAR_AGE %>% cap()
                fullset_final$OWN_CAR_AGE <- cut(fullset_final$OWN_CAR_AGE, breaks = c(0, 3, 6, 9, 12,Inf), labels = c("<3","3 - 6","6 - 9","9 - 12","12 +"))
                fullset_final$FLAG_EMP_PHONE <- factor(fullset_final$FLAG_EMP_PHONE, levels = c("0", "1"), labels = c("Y", "N"))
                fullset_final$FLAG_WORK_PHONE <- factor(fullset_final$FLAG_WORK_PHONE, levels = c("0", "1"), labels = c("Y", "N"))
                fullset_final$FLAG_PHONE <- factor(fullset_final$FLAG_PHONE, levels = c("0", "1"), labels = c("Y", "N"))
                fullset_final$OCCUPATION_TYPE <- factor(
                        ifelse(fullset_final$OCCUPATION_TYPE %in% c("1","6","7","8","11","12"), "High-Skilled",
                               ifelse(fullset_final$OCCUPATION_TYPE %in% c("2","3","4","13","14","15","16","17"), "Med-Skilled", "Low-Skilled")
                        ))
                fullset_final$REGION_RATING_CLIENT <- factor(fullset_final$REGION_RATING_CLIENT , levels = c("1", "2", "3"), labels = c("L1", "L2", "L3"))
                fullset_final$REGION_RATING_CLIENT_W_CITY <- factor(fullset_final$REGION_RATING_CLIENT_W_CITY , levels = c("1", "2", "3"), labels = c("L1", "L2", "L3"))
                fullset_final$HOUR_APPR_PROCESS_START.x <- 
                        ifelse(fullset_final$HOUR_APPR_PROCESS_START.x %in% c("7","8","9","10","11","12"),"Morning",
                               ifelse(fullset_final$HOUR_APPR_PROCESS_START.x %in% c("13","14","15","16"),"Afternoon",
                                      ifelse(fullset_final$HOUR_APPR_PROCESS_START.x %in% c("17","18","19","20"),"Evening","Night")))
                fullset_final$HOUR_APPR_PROCESS_START.x <- factor(fullset_final$HOUR_APPR_PROCESS_START.x)
                fullset_final$REG_CITY_NOT_LIVE_CITY <- factor(fullset_final$REG_CITY_NOT_LIVE_CITY, levels = c("0", "1"), labels = c("Y", "N"))
                fullset_final$REG_CITY_NOT_WORK_CITY <- factor(fullset_final$REG_CITY_NOT_WORK_CITY, levels = c("0", "1"), labels = c("Y", "N"))
                fullset_final$LIVE_CITY_NOT_WORK_CITY <- factor(fullset_final$LIVE_CITY_NOT_WORK_CITY, levels = c("0", "1"), labels = c("Y", "N"))
                round(fullset_final$ORGANIZATION_TYPE)
                fullset_final$ORGANIZATION_TYPE <- 
                        ifelse( fullset_final$ORGANIZATION_TYPE %in% c("4", "5", "6", "43"), "Business", 
                                ifelse( fullset_final$ORGANIZATION_TYPE %in% c("15","16","17","18","19","20","21","22","23","24","25","26","27"), "Industry",
                                        ifelse( fullset_final$ORGANIZATION_TYPE %in% c("46","47","48","49","50","51","52"), "Trade",
                                                ifelse( fullset_final$ORGANIZATION_TYPE %in% c("53","54","55","56"), "Transport",
                                                        ifelse( fullset_final$ORGANIZATION_TYPE %in% c("11","12","35","32","36","42"),"Government",
                                                                ifelse( fullset_final$ORGANIZATION_TYPE %in% c("39","40","41","44","45","1","33","13","31","38","57"), "Service",
                                                                        ifelse( fullset_final$ORGANIZATION_TYPE %in% c("2","10","7","29","9"), "Blue collar",
                                                                                ifelse( fullset_final$ORGANIZATION_TYPEE %in% c("8","37","14"), "Real Estate",
                                                                                        ifelse( fullset_final$ORGANIZATION_TYPE %in% c("3","28","30"), "Banking", "Others")))))))))
                fullset_final$ORGANIZATION_TYPE <- stri_replace_na(fullset_final$ORGANIZATION_TYPE, replacement = "Others")
                fullset_final$ORGANIZATION_TYPE <- factor(fullset_final$ORGANIZATION_TYPE)
                
                round(fullset_final$NAME_CASH_LOAN_PURPOSE)
                fullset_final$NAME_CASH_LOAN_PURPOSE <- 
                        ifelse( fullset_final$NAME_CASH_LOAN_PURPOSE %in% c("1", "3", "4", "5"), "Real Estate",
                                ifelse( fullset_final$NAME_CASH_LOAN_PURPOSE %in% c("6", "7", "8"), "Vehicle",
                                        ifelse( fullset_final$NAME_CASH_LOAN_PURPOSE %in% c("9"), "Education",
                                                ifelse( fullset_final$NAME_CASH_LOAN_PURPOSE %in% c("10","11","12","13","15","21"), "Household",
                                                        ifelse( fullset_final$NAME_CASH_LOAN_PURPOSE %in% c("16"), "Travel",
                                                                ifelse( fullset_final$NAME_CASH_LOAN_PURPOSE %in% c("23","19","22"), "Personal", "Others"
                                                                ))))))
                fullset_final$NAME_CASH_LOAN_PURPOSE <- fullset_final$NAME_CASH_LOAN_PURPOSE %>% factor()
                fullset_final$EXT_SOURCE_1 <- fullset_final$EXT_SOURCE_1 %>% cap()
                fullset_final$EXT_SOURCE_1 <- cut(fullset_final$EXT_SOURCE_1, breaks = c(0, 0.25, 0.50, 0.75,1), labels = c("0.00 - 0.25","0.26 - 0.50","0.51 - 0.75","0.76 - 1"))
                fullset_final$EXT_SOURCE_2 <- fullset_final$EXT_SOURCE_2 %>% cap()
                fullset_final$EXT_SOURCE_2 <- cut(fullset_final$EXT_SOURCE_2, breaks = c(-Inf, 0.25, 0.50, 0.75,1), labels = c("0.00 - 0.25","0.26 - 0.50","0.51 - 0.75","0.76 - 1"))
                fullset_final$EXT_SOURCE_3 <- fullset_final$EXT_SOURCE_3 %>% cap()
                fullset_final$EXT_SOURCE_3 <- cut(fullset_final$EXT_SOURCE_3, breaks = c(-Inf, 0.25, 0.50, 0.75,1), labels = c("0.00 - 0.25","0.26 - 0.50","0.51 - 0.75","0.76 - 1"))
                fullset_final$APARTMENTS_AVG <- cut(fullset_final$APARTMENTS_AVG, breaks = c(-Inf, 0.25, 0.50, 0.75,1), labels = c("0.00 - 0.25","0.26 - 0.50","0.51 - 0.75","0.76 - 1"))
                fullset_final$ELEVATORS_AVG <- cut(fullset_final$ELEVATORS_AVG, breaks = c(-Inf, 0.25, 0.50, 0.75,1), labels = c("<0.25","0.26 - 0.50","0.51 - 0.75","0.76+"))
                fullset_final$FLOORSMAX_AVG <- cut(fullset_final$FLOORSMAX_AVG, breaks = c(-Inf, 0.25, 0.50, 0.75,1), labels = c("0.00 - 0.25","0.26 - 0.50","0.51 - 0.75","0.76 - 1"))
                fullset_final$FLOORSMIN_AVG <- cut(fullset_final$FLOORSMIN_AVG, breaks = c(-Inf, 0.25, 0.50, 0.75,1), labels = c("0.00 - 0.25","0.26 - 0.50","0.51 - 0.75","0.76 - 1"))
                fullset_final$LIVINGAREA_AVG <- cut(fullset_final$LIVINGAREA_AVG, breaks = c(-Inf, 0.25, 0.50, 0.75,1), labels = c("0.00 - 0.25","0.26 - 0.50","0.51 - 0.75","0.76 - 1"))
                fullset_final$APARTMENTS_MODE <- cut(fullset_final$APARTMENTS_MODE, breaks = c(-Inf, 0.25, 0.50, 0.75,1), labels = c("0.00 - 0.25","0.26 - 0.50","0.51 - 0.75","0.76 - 1"))
                fullset_final$ELEVATORS_MODE <- cut(fullset_final$ELEVATORS_MODE, breaks = c(-Inf, 0.25, 0.50, 0.75,1), labels = c("<0.25","0.26 - 0.50","0.51 - 0.75","0.76+"))
                fullset_final$FLOORSMAX_MODE <- cut(fullset_final$FLOORSMAX_MODE, breaks = c(-Inf, 0.25, 0.50, 0.75,1), labels = c("0.00 - 0.25","0.26 - 0.50","0.51 - 0.75","0.76 - 1"))
                fullset_final$LIVINGAREA_MODE <- cut(fullset_final$LIVINGAREA_MODE, breaks = c(-Inf, 0.25, 0.50, 0.75,1), labels = c("0.00 - 0.25","0.26 - 0.50","0.51 - 0.75","0.76 - 1"))
                fullset_final$APARTMENTS_MEDI <- cut(fullset_final$APARTMENTS_MEDI, breaks = c(-Inf, 0.25, 0.50, 0.75,1), labels = c("0.00 - 0.25","0.26 - 0.50","0.51 - 0.75","0.76 - 1"))
                fullset_final$ELEVATORS_MEDI <- cut(fullset_final$ELEVATORS_MEDI, breaks = c(-Inf, 0.25, 0.50, 0.75,1), labels = c("0.00 - 0.25","0.26 - 0.50","0.51 - 0.75","0.76 - 1"))
                
                fullset_final$FLOORSMAX_MEDI <- cut(fullset_final$FLOORSMAX_MEDI, breaks = c(-Inf, 0.25, 0.50, 0.75,1), labels = c("0.00 - 0.25","0.26 - 0.50","0.51 - 0.75","0.76 - 1"))
                fullset_final$FLOORSMIN_MEDI <- cut(fullset_final$FLOORSMIN_MEDI, breaks = c(-Inf, 0.25, 0.50, 0.75,1), labels = c("0.00 - 0.25","0.26 - 0.50","0.51 - 0.75","0.76 - 1"))
                fullset_final$LIVINGAREA_MEDI <- cut(fullset_final$LIVINGAREA_MEDI, breaks = c(-Inf, 0.25, 0.50, 0.75,1), labels = c("0.00 - 0.25","0.26 - 0.50","0.51 - 0.75","0.76 - 1"))
                fullset_final$TOTALAREA_MODE <- cut(fullset_final$TOTALAREA_MODE, breaks = c(-Inf, 0.25, 0.50, 0.75,1), labels = c("0.00 - 0.25","0.26 - 0.50","0.51 - 0.75","0.76 - 1"))
                fullset_final$DAYS_LAST_PHONE_CHANGE <- 
                        fullset_final$DAYS_LAST_PHONE_CHANGE <- round((fullset_final$DAYS_LAST_PHONE_CHANGE /365)* (-1))
                fullset_final$DAYS_LAST_PHONE_CHANGE <- cut(fullset_final$DAYS_LAST_PHONE_CHANGE, breaks = c( -Inf, 1, 2, 3,4,Inf), labels = c("<1","1 - 2","2 - 3","3 - 4","4 +")) 
                fullset_final$DEF_30_CNT_SOCIAL_CIRCLE <- fullset_final$DEF_30_CNT_SOCIAL_CIRCLE %>% cap()
                fullset_final$DEF_30_CNT_SOCIAL_CIRCLE <- cut(fullset_final$DEF_30_CNT_SOCIAL_CIRCLE, breaks = c(-Inf, 0.25, 0.50, 0.75,1), labels = c("0.00 - 0.25","0.26 - 0.50","0.51 - 0.75","0.76 - 1"))
                round(fullset_final$DEF_60_CNT_SOCIAL_CIRCLE)
                fullset_final$DEF_60_CNT_SOCIAL_CIRCLE <- cut(fullset_final$DEF_60_CNT_SOCIAL_CIRCLE, breaks = c(-Inf, 2, 4, 6,Inf), labels = c("<2","2 - 4","4 - 6","6+"))
                fullset_final$FLAG_DOCUMENT_3 <- factor(fullset_final$FLAG_DOCUMENT_3, levels = c("0", "1"), labels = c("Y", "N"))
                fullset_final$FLAG_DOCUMENT_6 <- factor(fullset_final$FLAG_DOCUMENT_6, levels = c("0", "1"), labels = c("Y", "N"))
                fullset_final$AMT_REQ_CREDIT_BUREAU_YEAR <- fullset_final$AMT_REQ_CREDIT_BUREAU_YEAR %>% cap()
                fullset_final$AMT_REQ_CREDIT_BUREAU_YEAR <- cut(fullset_final$AMT_REQ_CREDIT_BUREAU_YEAR, breaks = c(-Inf,0, 2, 4, 6,Inf), labels = c("<0","0 - 2","2 - 4","4 - 6", "6+"))
                fullset_final$CREDIT_ACTIVE <- factor(round(fullset_final$CREDIT_ACTIVE))
                if (levels(fullset_final$CREDIT_ACTIVE) == 2) {
                        fullset_final$CREDIT_ACTIVE <- "closed"
                } else {
                        fullset_final$CREDIT_ACTIVE <- "active"
                }
                fullset_final$CREDIT_ACTIVE <- factor(fullset_final$CREDIT_ACTIVE)
                #Change days to year
                D2Y_scale <- function(x){
                        x <- (-x/365)
                }
                fullset_final$DAYS_CREDIT <- D2Y_scale(fullset_final$DAYS_CREDIT) 
                fullset_final$DAYS_CREDIT_ENDDATE <- D2Y_scale(fullset_final$DAYS_CREDIT_ENDDATE) 
                fullset_final$DAYS_CREDIT_UPDATE <- D2Y_scale(fullset_final$DAYS_CREDIT_UPDATE)
                fullset_final$DAYS_DECISION <- D2Y_scale(fullset_final$DAYS_DECISION)
                fullset_final$DAYS_ENDDATE_FACT <- D2Y_scale(fullset_final$DAYS_ENDDATE_FACT)
                fullset_final$DAYS_INSTALMENT <- D2Y_scale(fullset_final$DAYS_INSTALMENT)
                fullset_final$DAYS_ENTRY_PAYMENT <- D2Y_scale(fullset_final$DAYS_ENTRY_PAYMENT)
                fullset_final$DAYS_CREDIT <- cut(fullset_final$DAYS_CREDIT, breaks = c(-Inf,2,4,6,Inf), labels = c("<2","2-4","4-6","6+")) 
                fullset_final$DAYS_CREDIT_ENDDATE <- cut(fullset_final$DAYS_CREDIT_ENDDATE, breaks = c(-Inf,1, 2, 3, Inf), labels = c("<1", "1-2", "2-3", "3+"))
                fullset_final$DAYS_CREDIT_UPDATE <- cut(fullset_final$DAYS_CREDIT_UPDATE, breaks = c(-Inf,0.5, 1, 1.5, 2, Inf), labels = c("<0.5", "0.5-1", "1-1.5", "1.5-2", "2+"))
                fullset_final$DAYS_DECISION <- cut(fullset_final$DAYS_DECISION, breaks = c(0, 2, 4, 6, 8, Inf), labels = c("<2", "2-4", "4-6", "6-8", "8+"))
                fullset_final$DAYS_ENDDATE_FACT <- cut(fullset_final$DAYS_ENDDATE_FACT, breaks = c(-Inf,1, 2, 3, 4, 5, Inf), labels = c("<1", "1-2", "2-3", "3-4", "4-5", "5+"))
                fullset_final$DAYS_INSTALMENT <- cut(fullset_final$DAYS_INSTALMENT, breaks = c(-Inf, 1, 2, 3, 4, 5, Inf), labels = c("<1", "1-2", "2-3", "3-4", "4-5", "5+"))
                fullset_final$DAYS_ENTRY_PAYMENT <- cut(fullset_final$DAYS_ENTRY_PAYMENT, breaks = c(-Inf, 1, 2, 3, 4, 5, Inf), labels = c("<1", "1-2", "2-3", "3-4", "4-5", "5+"))
                fullset_final$AMT_CREDIT_SUM <- cut(fullset_final$AMT_CREDIT_SUM, breaks = c(-Inf, 100000, 200000, 300000, 400000, Inf), labels = c("<100k", "100-200k", "200-300k", "300-400k", ">400k"))
                fullset_final$AMT_BALANCE <- cut(fullset_final$AMT_BALANCE, breaks = c(-Inf, 20000, 40000, 60000, 80000, Inf), labels = c("<20k", "20~ 40k", "40~60k", "60~80k", ">80k"))
                fullset_final$AMT_DRAWINGS_ATM_CURRENT <- cut(fullset_final$AMT_DRAWINGS_ATM_CURRENT, breaks = c(-Inf, 10000, 20000, Inf), labels = c("<10k", "10~20k", ">20k"))
                fullset_final$AMT_DRAWINGS_CURRENT <- cut(fullset_final$AMT_DRAWINGS_CURRENT, breaks = c(-Inf, 10000, 20000, Inf), labels = c("<10k", "10~20k", ">20k"))
                fullset_final$AMT_INST_MIN_REGULARITY <- cut(fullset_final$AMT_INST_MIN_REGULARITY, breaks = c(-Inf,  3000, 6000, 9000, Inf), labels = c("<3k", "3~6k", "6~9k", ">9k"))
                fullset_final$AMT_RECEIVABLE_PRINCIPAL <- cut(fullset_final$AMT_RECEIVABLE_PRINCIPAL, breaks = c(-Inf, 40000, 80000, 120000, Inf), labels = c("<40k", "40~80k", "80~120k", ">120k"))
                fullset_final$AMT_RECIVABLE <- cut(fullset_final$AMT_RECIVABLE, breaks = c(-Inf, 40000, 80000, Inf), labels = c("<40k", "40~80k", ">120k"))
                fullset_final$AMT_TOTAL_RECEIVABLE <- cut(fullset_final$AMT_TOTAL_RECEIVABLE, breaks = c(-Inf, 40000, 80000, Inf), labels = c("<40k", "40~80k", ">120k"))
                fullset_final$AMT_INSTALMENT <- cut(fullset_final$AMT_INSTALMENT, breaks = c(-Inf, 6000, 12000, 18000, 24000, Inf), labels = c("<6k", "6~12k", "12~18k", "18~24k", ">24k")
                                                    fullset_final$AMT_PAYMENT <- cut(fullset_final$AMT_PAYMENT, breaks = c(-Inf, 5000, 10000, 15000, 20000, Inf), labels = c("<5k", "5~10k", "10~15k", "15~20k", ">20k")
                                                                                     fullset_final$CNT_DRAWINGS_ATM_CURRENT <- cut(fullset_final$CNT_DRAWINGS_ATM_CURRENT, breaks = c(-Inf, 0.3, 0.6, 1, Inf), labels = c("<0.3", "0.3~0.6", "0.6~1", ">1"))
                                                                                     fullset_final$CNT_DRAWINGS_CURRENT <- cut(fullset_final$CNT_DRAWINGS_CURRENT, breaks = c(-Inf, 1, 2, Inf), labels = c("<1", "1~2", ">2"))
                                                                                     fullset_final$CNT_DRAWINGS_POS_CURRENT <- cut(fullset_final$CNT_DRAWINGS_POS_CURRENT, breaks = c(-Inf, 1, 2, Inf), labels = c("<1", "1~2", ">2"))
                                                                                     fullset_final$NUM_INSTALMENT_VERSION <- factor(round(fullset_final$NUM_INSTALMENT_VERSION))
                                                                                     fullset_final$MONTHS_BALANCE.x <- cut(fullset_final$MONTHS_BALANCE.x, breaks = c(-Inf, -24, -12, Inf), labels = c("2+", "1 - 2", "<1"))
                                                                                     fullset_final$MONTHS_BALANCE.y <- cut(fullset_final$MONTHS_BALANCE.y, breaks = c(-Inf, -24, -12, Inf), labels = c("2+", "1 - 2", "<1"))
                                                                                     fullset_final$NUM_INSTALMENT_VERSION <- factor(fullset_final$NUM_INSTALMENT_VERSION
                                                                                                                                    fullset_final$PAYMENT_DIFF<- 
                                                                                                                                            cut(fullset_final$PAYMENT_DIFF, breaks = c(-Inf, 50, 150,350,Inf),
                                                                                                                                                labels = c("<50", "50-150", "150-350", "350+"),
                                                                                                                                                include.lowest = TRUE)
                                                                                                                                    fullset_final$MONTHS_BALANCE<- 
                                                                                                                                            cut(fullset_final$MONTHS_BALANCE, breaks = c(-Inf, -36, -24,-12,Inf),
                                                                                                                                                labels = c("> 3 years", "2-3 years", "1-2 years","< 1 year"),
                                                                                                                                                include.lowest = TRUE)
                                                                                                                                    fullset_final$CNT_INSTALMENT <- 
                                                                                                                                            cut(fullset_final$CNT_INSTALMENT, breaks = c(-Inf,9, 12,18,Inf),
                                                                                                                                                labels = c("<9", "9-12", "12-18",">18"),
                                                                                                                                                include.lowest = TRUE)
                                                                                                                                    fullset_final$CNT_INSTALMENT_FUTURE<- 
                                                                                                                                            cut(fullset_final$CNT_INSTALMENT_FUTURE, breaks = c(0,5, 8,11,Inf),
                                                                                                                                                labels = c("<5", "5-8", "8-11",">11"),
                                                                                                                                                include.lowest = TRUE)
                                                                                                                                    fullset_final$AMT_ANNUITY<- 
                                                                                                                                            cut(fullset_final$AMT_ANNUITY, breaks = c(0,8000, 13000,17000,20000,Inf),
                                                                                                                                                labels = c("0-8k", "8K-13k", "13K-17k","17k-20k","20k+"),
                                                                                                                                                include.lowest = TRUE
                                                                                                                                                fullset_final$AMT_APPLICATION<- 
                                                                                                                                                        cut(fullset_final$AMT_APPLICATION, breaks = c(0, 64082 , 112500 ,145336,183219,Inf),
                                                                                                                                                            labels = c("0-64k", "64K-113k", "113K-146k","146k-184k","184k+"),
                                                                                                                                                            include.lowest = TRUE)
                                                                                                                                                fullset_final$AMT_DOWN_PAYMENT<- 
                                                                                                                                                        cut(fullset_final$AMT_DOWN_PAYMENT, breaks = c(-Inf, 4500 ,6000,7500,Inf),
                                                                                                                                                            labels = c("<4.5k", "4.5k - 6k","6k - 7k",">7.5K"),
                                                                                                                                                            include.lowest = TRUE)
                                                                                                                                                fullset_final$HOUR_APPR_PROCESS_START.y <- 
                                                                                                                                                        ifelse(fullset_final$HOUR_APPR_PROCESS_START.y %in% c("7","8","9","10","11","12"),"Morning",
                                                                                                                                                               ifelse(fullset_final$HOUR_APPR_PROCESS_START.y %in% c("13","14","15","16"),"Afternoon",
                                                                                                                                                                      ifelse(fullset_final$HOUR_APPR_PROCESS_START.y %in% c("17","18","19","20"),"Evening","Night")))
                                                                                                                                                fullset_final$HOUR_APPR_PROCESS_START.y <- factor(fullset_final$HOUR_APPR_PROCESS_START.y)
                                                                                                                                                fullset_final$STATUS <- fullset_final$STATUS %>% round() %>% factor(levels = c("1","2","3","4","5","6","7","8"), labels = c("0", "1", "2" ,"3" ,"4", "5", "C", "X"))
                                                                                                                                                fullset_final$RATE_DOWN_PAYMENT<- 
                                                                                                                                                        cut(fullset_final$RATE_DOWN_PAYMENT, breaks = c(-Inf,0, 0.0819243 ,0.1077803,Inf),
                                                                                                                                                            labels = c("No Down Payment","0-8%", "8-10%", ">10%") ,
                                                                                                                                                            include.lowest = TRUE)
                                                                                                                                                fullset_final$DBD<- 
                                                                                                                                                        cut(fullset_final$DBD, breaks = c(-Inf,7,14,21,28,Inf),
                                                                                                                                                            labels = c("< 1 Week","1-2 Weeks","2-3 Weeks","3-4 Weeks",">4 Weeks") ,
                                                                                                                                                            include.lowest = TRUE)
                                                                                                                                                fullset_final$NAME_CONTRACT_STATUS <- fullset_final$NAME_CONTRACT_STATUS %>% round() %>% factor(levels = c("1","2","3","4"), labels = c("Approved"  ,   "Canceled"  ,   "Refused"    ,  "Unused offer"))
                                                                                                                                                fullset_final$NAME_PAYMENT_TYPE <- fullset_final$NAME_PAYMENT_TYPE %>% round() %>% factor(levels = c("1","2","3","4"), labels = c("Cash through bank","Cashless from employer","Non-cash from own account","Others"))
                                                                                                                                                fullset_final$CODE_REJECT_REASON <- fullset_final$CODE_REJECT_REASON %>% round() %>% factor(levels = c("1","2","3","4","5","6","7","8","9"), labels = c("CLIENT","HC","LIMIT","SCO","SCOFR","SYSTEM","VERIF","XAP", "XNA"   ))
                                                                                                                                                fullset_final$NAME_TYPE_SUITE.y <- round(fullset_final$NAME_TYPE_SUITE.y)
                                                                                                                                                fullset_final$NAME_TYPE_SUITE.y <- 
                                                                                                                                                        ifelse(fullset_final$NAME_TYPE_SUITE.y == 1 , "Kids",
                                                                                                                                                               ifelse(fullset_final$NAME_TYPE_SUITE.y == 2, "Family",       
                                                                                                                                                                      ifelse(fullset_final$NAME_TYPE_SUITE.y == 6, "Partner",
                                                                                                                                                                             ifelse(fullset_final$NAME_TYPE_SUITE.y == 7, "Unaccompanied","Others"
                                                                                                                                                                             ))))
                                                                                                                                                fullset_final$NAME_TYPE_SUITE.y <- factor(fullset_final$NAME_TYPE_SUITE.y)
                                                                                                                                                fullset_final$NAME_PRODUCT_TYPE <- fullset_final$NAME_PRODUCT_TYPE %>% round() %>% factor(levels = c("1","2","3"), labels = c("walk-in", "x-sell" , "Others"  ))
                                                                                                                                                
                                                                                                                                                fullset_final$NAME_GOODS_CATEGORY <- round(fullset_final$NAME_GOODS_CATEGORY)
                                                                                                                                                fullset_final$NAME_GOODS_CATEGORY <- 
                                                                                                                                                        ifelse(fullset_final$NAME_GOODS_CATEGORY %in% c("2"), "Animals",       
                                                                                                                                                               ifelse(fullset_final$NAME_GOODS_CATEGORY %in% c("3","6","8","20","23"), "Technology",
                                                                                                                                                                      ifelse(fullset_final$NAME_GOODS_CATEGORY %in% c("4","26"), "Auto",
                                                                                                                                                                             ifelse(fullset_final$NAME_GOODS_CATEGORY %in% c("7","12","13","14","15"), "Real Estate/ Home",
                                                                                                                                                                                    ifelse(fullset_final$NAME_GOODS_CATEGORY %in% c("5","17","24","25","10"), "Personal",      
                                                                                                                                                                                           ifelse(fullset_final$NAME_GOODS_CATEGORY %in% c("11","19","16","18"), "Animals", "Others"  
                                                                                                                                                                                           ))))))
                                                                                                                                                fullset_final$NAME_GOODS_CATEGORY <- factor(fullset_final$NAME_GOODS_CATEGORY)
                                                                                                                                                fullset_final$CHANNEL_TYPE <- fullset_final$CHANNEL_TYPE %>% round() %>% factor(levels = c("1","2","3","4","5","6","7","8"), labels = c("AP+ (Cash loan)","Car dealer","corporate sales","Contact center","Country-wide","Credit and cash offices","Regional / Local","Stone"  ))
                                                                                                                                                fullset_final$CNT_PAYMENT <- 
                                                                                                                                                        cut(fullset_final$CNT_PAYMENT, breaks = c(-Inf,10,15,20,Inf),
                                                                                                                                                            labels = c("< 10","10 ~ 15","15 ~ 20","20+") ,
                                                                                                                                                            include.lowest = TRUE)
                                                                                                                                                fullset_final$PRODUCT_COMBINATION <- round(fullset_final$PRODUCT_COMBINATION)
                                                                                                                                                fullset_final$PRODUCT_COMBINATION <- factor(fullset_final$PRODUCT_COMBINATION,levels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17"), labels = c("Card Street" ,"Card X-Sell","Cash", "Cash Street: high", "Cash Street: low","Cash Street: middle","Cash X-Sell: high","Cash X-Sell: low","Cash X-Sell: middle","POS household with interest","POS household without interest","POS industry with interest","POS industry without interest","POS mobile with interest","POS mobile without interest","POS other with interest","POS others without interest"))
                                                                                                                                                fullset_final$DAYS_FIRST_DRAWING <- cut(fullset_final$DAYS_FIRST_DRAWING , breaks = c(-Inf,-1095,-730,-365,Inf),
                                                                                                                                                                                        labels = c(">3 years","2-3 years","1-2 years","<1 year") ,
                                                                                                                                                                                        include.lowest = TRUE)
                                                                                                                                                fullset_final$DAYS_FIRST_DUE <- cut(fullset_final$DAYS_FIRST_DUE , breaks = c(-Inf,-1095,-730,-365,Inf),
                                                                                                                                                                                    labels = c(">3 years","2-3 years","1-2 years","<1 year") ,
                                                                                                                                                                                    include.lowest = TRUE)
                                                                                                                                                fullset_final$DAYS_LAST_DUE_1ST_VERSION <- fullset_final$DAYS_LAST_DUE_1ST_VERSION %>% cap()
                                                                                                                                                fullset_final$DAYS_LAST_DUE_1ST_VERSION <- cut(fullset_final$DAYS_LAST_DUE_1ST_VERSION , breaks = c(-Inf,-1095,-730,-365,Inf),
                                                                                                                                                                                               labels = c(">3 years","2-3 years","1-2 years","<1 year") ,
                                                                                                                                                                                               include.lowest = TRUE)
                                                                                                                                                fullset_final$DAYS_LAST_DUE <- cut(fullset_final$DAYS_LAST_DUE , breaks = c(-Inf,-1095,-730,-365,Inf),
                                                                                                                                                                                   labels = c(">3 years","2-3 years","1-2 years","<1 year") ,
                                                                                                                                                                                   include.lowest = TRUE)
                                                                                                                                                fullset_final$DAYS_TERMINATION <- cut(fullset_final$DAYS_TERMINATION , breaks = c(-Inf,-1095,-730,-365,Inf),
                                                                                                                                                                                      labels = c(">3 years","2-3 years","1-2 years","<1 year") ,
                                                                                                                                                                                      include.lowest = TRUE)
                                                                                                                                                --------------------------------
                                                                                                                                                        ## Checking for inconsistencies and factor distribution
                                                                                                                                                        sum(is.na(fullset_final))
                                                                                                                                                sapply( fullset_final[ sapply(fullset_final, is.factor)], table)
                                                                                                                                                --------------------------------
                                                                                                                                                        