## Univariate 
library(ggthemes)
library(ggplot2)
library(ggpubr)
library(rcartocolor)
library(ggmosaic)
library(cowplot)
library(ggcorrplot)
library(vcd)
library(tidyverse)
cor(fullset_final)
bar_theme <- theme(text  = element_text(size = 12),
                   axis.title.y = element_blank(),
                   axis.text.y = element_blank(),
                   axis.text.x = element_blank(),
                   axis.title.x = element_blank(),
                   legend.position="none")
my_colors <- c('#e4f1e1', '#b4d9cc', '#89c0b6', '#63a6a0', '#448c8a', '#287274', '#0d585f')
p1 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = NAME_CONTRACT_TYPE.x, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x ="", title ="Contract type")
p2 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = CODE_GENDER, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x ="", title ="Gender")
p3 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = FLAG_OWN_CAR, fill =  TARGET), position = position_dodge2()) +
        bar_theme  + labs(fill = "", x ="",  title ="Owned car")
p4 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = CNT_CHILDREN, fill =  TARGET), position = position_dodge2()) +
        bar_theme  + labs(fill = "", x ="",  title ="Child number")
p5 <- ggplot(data = fullset_final) +
        geom_bar(aes(x =  AMT_CREDIT.x , fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x ="",  title ="Amount of credit")
p6 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = AMT_GOODS_PRICE.x, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x ="",  title ="Good price")
p7 <- ggplot(data = fullset_final) +
        geom_bar(aes(x =  NAME_INCOME_TYPE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x ="",  title ="Income type")
p8 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = NAME_EDUCATION_TYPE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x ="",  title ="Education type")
p9 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = NAME_HOUSING_TYPE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x ="",  title ="Housing type")
p10 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = REGION_POPULATION_RELATIVE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x ="",  title ="Relative")
p11 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = Age, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x ="", y = "", title ="Age")
p12 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = Years_Employed, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x ="", y = "", title ="Employed years")
p13 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = Years_Registered, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x ="", y = "", title ="Registered years")
p14 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = Years_IDPUB, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x ="", y = "", title ="Years ID")
p15 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = OWN_CAR_AGE, fill =  TARGET), position = position_dodge2()) +
        bar_theme  + labs(fill = "", x ="", y = "", title ="Car age")
p16 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = FLAG_EMP_PHONE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x ="", y = "", title ="FLAG EMP PHONE")
p17 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = FLAG_WORK_PHONE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x ="", title = "FLAG WORK PHONE")
p18 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = FLAG_PHONE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x ="", title = "FLAG PHONE")
p19 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = OCCUPATION_TYPE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x ="", title = "Occupation type")
p20 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = REGION_RATING_CLIENT, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x ="", title = "RATING CLIENT")
p21 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = REGION_RATING_CLIENT_W_CITY, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x ="", title = "RATING CLIENT with CITY")
p22 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = HOUR_APPR_PROCESS_START.x, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x ="", title = "HOUR APPR PROCES]")
p23 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = REG_CITY_NOT_LIVE_CITY, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x ="", title = "LIVE in CITY or Not")
p24 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = REG_CITY_NOT_WORK_CITY, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x ="", title = "WORK in CITY or Not")
p25 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = LIVE_CITY_NOT_WORK_CITY, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x ="", title = "LIVE CITY NOT WORK CITY")
sum1 <- plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
                  p11, p12, p13, p14, p15, p16, p18, p19, p20, 
                  p21, p22, p23, p24, p25, nrow = 5)
sum1 <- add_sub(sum1, "The first 25 variables across on Target (Good and Bad)")
ggdraw(sum1)
legend("bottomright", legend=c("Line 1", "Line 2"),
       col=c("red", "blue"))
p26 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = ORGANIZATION_TYPE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Organization Type")
p27 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = EXT_SOURCE_1, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Extra source 1")
p28 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = EXT_SOURCE_2, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Extra source 2")
p29 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = EXT_SOURCE_3, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Extra source 3")
p30 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = APARTMENTS_AVG, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Average Appartment")

p31 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = ELEVATORS_AVG, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Average elevator")
p32 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = FLOORSMAX_AVG, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Maimun of floor")
p33 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = FLOORSMIN_AVG, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Minimun of floors" )
p34 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = LIVINGAREA_AVG, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Living area")
p35 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = APARTMENTS_MODE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Appartment mode")
p36 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = ELEVATORS_MODE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Elevator mode")
p37 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = FLOORSMAX_MODE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Floor max mode")
p38 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = APARTMENTS_MODE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Floor mode")
p39 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = LIVINGAREA_MODE , fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title= "Living area mode" )
p40 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = APARTMENTS_MEDI, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Appartmnent medium")
p41 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = ELEVATORS_MEDI, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title =  "Elevator medium")
p42 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = FLOORSMAX_MEDI, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Floor maxium medium")
p43 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = FLOORSMIN_MEDI, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Floor minimun medium" )
p44 <- ggplot(data = fullset_final) +
        geom_bar(aes(x =  LIVINGAREA_MEDI, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Living area medium")
p45 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = TOTALAREA_MODE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Total area mode")
p46 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = DEF_30_CNT_SOCIAL_CIRCLE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "30 days social cirlce")
p47 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = DEF_60_CNT_SOCIAL_CIRCLE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "",  title = "60 days social circle")
p48 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = DAYS_LAST_PHONE_CHANGE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Days from last phone number change")
p49 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = FLAG_DOCUMENT_3, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Flag document 3")
p50 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = FLAG_DOCUMENT_6, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Flag document 6")
sum2 <- plot_grid(p26, p27, p28, p29, p30, p31, p32, p33, p34, p35,
                  p36, p37, p38, p39, p40, p41, p42, p43, p44, p45,
                  p46, p47, p48, p49, p50, nrow = 5)
sum2 <- add_sub(sum2, "From 26th to 50th variables based on Target (Good and bad)")
ggdraw(sum2)
p51 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = AMT_REQ_CREDIT_BUREAU_YEAR, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Year amount")
p52 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = CREDIT_ACTIVE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Active credit")
p53 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = DAYS_CREDIT, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Days of credit")
p54 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = DAYS_CREDIT_ENDDATE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Days of end credit")
p55 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = DAYS_ENDDATE_FACT, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Actual end date")
p56 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = AMT_CREDIT_SUM, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Credit sum")
p57 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = DAYS_CREDIT_UPDATE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Updated days of credit")
p58 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = MONTHS_BALANCE.x, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Monthly balance x")
p59 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = STATUS, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Status")
p60 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = MONTHS_BALANCE.y, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Monthly blance y")
p61 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = AMT_BALANCE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Amount of balance")
p62 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = AMT_DRAWINGS_ATM_CURRENT, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Drawing from current account")
p63 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = AMT_DRAWINGS_CURRENT, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Current Drawing")
p64 <- ggplot(data = fullset_final) +
        geom_bar(aes(x =  AMT_INST_MIN_REGULARITY, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Monthly minimum amount ")
p65 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = AMT_RECEIVABLE_PRINCIPAL, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Amount of principle receive")

p66 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = AMT_RECIVABLE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Receivable amount")
p67 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = AMT_TOTAL_RECEIVABLE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Total receivable amount")
p68 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = CNT_DRAWINGS_ATM_CURRENT, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Count drawing from atm")
p69 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = CNT_DRAWINGS_CURRENT, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Count of current drawings")
p70 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = CNT_DRAWINGS_POS_CURRENT, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Drawings from POS ")
p71 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = NUM_INSTALMENT_VERSION, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Number of instalment")
p72 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = DAYS_INSTALMENT, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Days of instalment")

p73 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = DAYS_ENTRY_PAYMENT, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Days of entry payment")
p74 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = AMT_INSTALMENT, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Amount of instalment")
p75 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = AMT_PAYMENT, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Amount of payment")
sum3 <- plot_grid(p51, p52, p53, p54, p55, p56, p57, p58, p59, p60,
                  p61, p62, p63, p64, p65, p66, p67, p68, p69, p70,
                  p71, p72, p73, p74, p75, nrow = 5)
sum3 <- add_sub(sum3, "The 51st ~ 75th variables based on Target (Good and Bad)")
ggdraw(sum3)
p76 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = PAYMENT_DIFF, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Payment difference")
p77 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = DBD, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "DBD")
p78 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = MONTHS_BALANCE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Months Balance")
p79 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = CNT_INSTALMENT, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Count of instalment")
p80 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = CNT_INSTALMENT_FUTURE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Count of future instalment")
p81 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = AMT_ANNUITY, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Annual amount")
p82 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = AMT_APPLICATION, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Application amount")
p83 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = AMT_DOWN_PAYMENT, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Amount of down payment")
p84 <- ggplot(data = fullset_final) +
        geom_bar(aes(x =  HOUR_APPR_PROCESS_START.y, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Hour of starting application")
p85 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = RATE_DOWN_PAYMENT, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Rate of down payment")
p86 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = NAME_CASH_LOAN_PURPOSE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Loan purpose")
p87 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = NAME_CONTRACT_STATUS, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Contract status")
p88 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = DAYS_DECISION, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Decision days")
p89 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = NAME_PAYMENT_TYPE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Payment type")
p90 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = CODE_REJECT_REASON, fill =  TARGET), position = position_dodge2())+ 
        bar_theme + labs(fill = "", x = "", title = "Rejection reason")
p91 <- ggplot(data = fullset_final) +
        geom_bar(aes(x =  NAME_TYPE_SUITE.y , fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Suite name")
p92 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = NAME_GOODS_CATEGORY , fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Cood category")
p93 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = NAME_PRODUCT_TYPE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Product type")
p94 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = CHANNEL_TYPE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Channel type")
p95 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = CNT_PAYMENT, fill =  TARGET), position = position_dodge2()) + 
        bar_theme + labs(fill = "", x = "", title = "Current payment")
p96 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = PRODUCT_COMBINATION, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Combinaition product")
p97 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = DAYS_FIRST_DRAWING, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Days of first drawing")
p98 <- ggplot(data = fullset_final) +
        geom_bar(aes(x = DAYS_FIRST_DUE, fill =  TARGET), position = position_dodge2()) +
        bar_theme + labs(fill = "", x = "", title = "Days of first due")
sum4 <- plot_grid(p76, p77, p78, p79, p80, p81, p82, p83, p84, p85,
                  p86, p87, p88, p89, p90, p91, p92, p93, p94, p95,
                  p96, p97, p98)
sum4 <- add_sub(sum4, "Last 25 variables based on Target (Good and bad)")
ggdraw(sum4)



## Bivariate
b1 <- ggplot(data = fullset_final)+
        geom_bar(aes(x = FLAG_OWN_CAR, fill = TARGET), position = position_dodge2()) +
        labs(fill = "Target", x = "Owned Car", title = "Owned car across the target") + theme_economist() +
        facet_grid( ~ CODE_GENDER)
b2 <- ggplot(data = fullset_final)+
        geom_bar(aes(x = NAME_TYPE_SUITE.y, fill = TARGET), position = position_dodge2()) +
        labs(fill = "Target", x = "Suite type", title = "Suite type across the target") + theme_economist() +
        facet_grid( NAME_CONTRACT_TYPE.x~.)
b3 <- ggplot(data = fullset_final)+
        geom_bar(aes(x = NAME_INCOME_TYPE, fill = TARGET), position = position_dodge2()) +
        labs(fill = "Target", x = "Income type", title = "Income type based on target") + theme_economist() +
        facet_grid( CNT_CHILDREN~.)
b4 <- ggplot(data = fullset_final)+
        geom_bar(aes(x =  Age, fill = TARGET), position = position_dodge2()) +
        labs(fill = "Target", x = "Age", title = "Age ditribution based on type") + theme_economist() +
        facet_grid(OCCUPATION_TYPE ~ .)
b5 <- ggplot(data = fullset_final)+
        geom_bar(aes(x = ORGANIZATION_TYPE, fill = TARGET), position = position_dodge2()) +
        labs(fill = "Target", x = "Orgnization Type", title = "Orgnization type based on Occupation type") + theme_economist() + facet_grid(OCCUPATION_TYPE ~ .)

b6 <- ggplot(data = fullset_final)+
        geom_bar(aes(x = APARTMENTS_AVG, fill = TARGET), position = position_dodge2()) +
        labs(fill = "Target", x = "Appartment average ", title = "Average appartment across occupation type") + theme_economist() + facet_grid(  OCCUPATION_TYPE ~ .)
b7 <- ggplot(data = fullset_final)+
        geom_bar(aes(x = LIVINGAREA_MEDI, fill = TARGET), position = position_dodge2()) +
        labs(fill = "Target", x = "Living area", title = "Living area across number of child") + theme_economist() +
        facet_grid(CNT_CHILDREN ~ .)
b8 <- ggplot(data = fullset_final)+
        geom_bar(aes(x = DAYS_LAST_PHONE_CHANGE, fill = TARGET), position = position_dodge2()) +
        labs(fill = "Target", x = "Days of changing phone", 
             title = "Days of changing phone across credit status") + theme_economist() +
        facet_grid( ~ CREDIT_ACTIVE)
b9 <- ggplot(data = fullset_final)+
        geom_bar(aes(x = DAYS_CREDIT, fill = TARGET), position = position_dodge2()) +
        labs(fill = "Target", x = "Owned Car", title = "Credit days across amount of credit") + theme_economist() +
        facet_grid(~AMT_CREDIT_SUM )
b10 <- ggplot(data = fullset_final)+
        geom_bar(aes(x = STATUS, fill = TARGET), position = position_dodge2()) +
        labs(fill = "Target", x = "Status", title = "Status across Monthly balance") + theme_economist() +
        facet_grid( MONTHS_BALANCE.x~ .)
b11 <- ggplot(data = fullset_final)+
        geom_bar(aes(x = AMT_DRAWINGS_CURRENT, fill = TARGET), position = position_dodge2()) +
        labs(fill = "Target", x = "Drawing amount", title = "Drawing amount across principal reveive") + theme_economist() +
        facet_grid( ~ AMT_RECEIVABLE_PRINCIPAL)
b12 <- ggplot(data = fullset_final)+
        geom_bar(aes(x = AMT_TOTAL_RECEIVABLE, fill = TARGET), position = position_dodge2()) +
        labs(fill = "Target", x = "Annual receivable amount", 
             title = "Annual receivable amount across occupationtype") + theme_economist() +
        facet_grid(OCCUPATION_TYPE ~ .)
b13 <- ggplot(data = fullset_final)+
        geom_bar(aes(x = AMT_PAYMENT, fill = TARGET), position = position_dodge2()) +
        labs(fill = "Target", x = "Amount of payment", title = "Amount of payment across monthly balance") + theme_economist() +   facet_grid( MONTHS_BALANCE.x ~.)
b14 <- ggplot(data = fullset_final)+
        geom_bar(aes(x = AMT_ANNUITY, fill = TARGET), position = position_dodge2()) +
        labs(fill = "Target", x = "Anual Amount", title = "Anual amount across Age") + theme_economist() +   facet_grid(Age ~.)
b15 <- ggplot(data = fullset_final)+
        geom_bar(aes(x = NAME_CONTRACT_STATUS, fill = TARGET), position = position_dodge2()) +
        labs(fill = "Target", x = "Contract Status", title = "Contract status across Occupation type") + theme_economist() +   facet_grid(~OCCUPATION_TYPE)

#Multivariate
ggplot(data = fullset_final) +
        geom_mosaic(aes(x = product(CODE_GENDER,  NAME_CONTRACT_TYPE.x), fill = TARGET)) +
        theme_economist() + scale_fill_carto_d(palette = 2) +
        labs(title = "Target distribution across gender and contract type")
ggplot(data = fullset_final) +
        geom_mosaic(aes(x = product(NAME_EDUCATION_TYPE, OCCUPATION_TYPE) , fill = TARGET))+ theme_economist() + scale_fill_carto_d(palette = 2) +
        labs(title = "Target distribution across education type and occupation type")
ggplot(data = fullset_final) +
        geom_mosaic(aes(x = product(NAME_PAYMENT_TYPE, CREDIT_ACTIVE), fill = TARGET)) +
        theme_economist() + scale_fill_carto_d(palette = 2) +
        labs(title = "Target distribution across payment type and credit status")
ggplot(data = fullset_final) +
        geom_mosaic(aes(x = product(NAME_EDUCATION_TYPE, Age), fill =TARGET))+
        theme_economist() + scale_fill_carto_d(palette = 2) +
        labs(title = "Target distribution across age and education level")
ggplot(data = fullset_final )+
        geom_mosaic(aes(x = product(NAME_CONTRACT_STATUS, DAYS_DECISION), fill = TARGET)) +
        theme_economist() + scale_fill_carto_d(palette = 2) +
        labs(title = "Target distribution across contract status and days for decision")
