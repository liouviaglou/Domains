# verifying that tld & registrar do not make an impact on splits

###############Housekeeping#####################

library(party)
library(partykit)
library(rpart)
library(caret) 
library(e1071)

library(stringr)
library(stringi)
library(dplyr)
library(modeest)
library(stringdist)
library(pbapply)
library(RMySQL)
library(gnm)
library(ggplot2)
library(rPython)
library(tidyverse)
library(R.utils)
library(data.table)
library(reshape)
library(lubridate)
library(clipr)
library(reshape2)
library(plotly)
library(tidyr)
library(cluster)
library(dbplyr)
library(magrittr)

getwd()
setwd("/Users/lubagloukhov/Documents/Consulting/Radix/Domains_202003/scripts/dtree")

###############Source Functions#####################

source('../orig/functions.R')
source('../orig/functions_models.R')

python.load("../orig/gibb_detect/gib_detect_train.py",TRUE)

###############Read files#####################

renewal_training_data<-readRDS("../../data/input/npv/renewal_training_data")
# renewal_training_data_first<-readRDS("../../data/input/npv/renewal_training_data_first")
# renewal_price_map<-readRDS("../../data/input/npv/renewal_price_map")
# standard_renewal_prices<-readRDS("../../data/input/npv/standard_renewal_prices")
# 
# 
# test_data<-readRDS("../../data/input/npv/test_data")
# test_data_prepped<-readRDS("../../data/input/npv/test_data_prepped")
# test_data_op<-readRDS("../../data/input/npv/test_data_op")
train_data <- readRDS("../../data/output/npv/first_renewal_preds")



table(train_data$reseller, train_data$registrar)

predictors <- c("tld", 
                "reseller", # same as registrar for this dataset
                "pattern_domain_count", 
                "log_reg_arpt",
                "sld_length", 
                "gibb_score",
                "sld_type", 
                "day_domains",
                "reg_period")
response <- "renewal_status"
setdiff(predictors, names(train_data))


##############################################
#############Test/Train Prep##################
##############################################


train_index <- sample(1:nrow(train_data), 0.8 * nrow(train_data))
test_index <- setdiff(1:nrow(train_data), train_index)

# Build X_train, y_train, X_test, y_test

train <- train_data[train_index,]
test <- train_data[test_index,]

summary(subset(train_data,,c(predictors, response)))

##############################################
###############Orig model#####################
##############################################

first_renewal_model <- readRDS("../../data/output/npv/first_renewal_model")

####Predict & Calc Accuracy on Full Data Set######

prediction <- ifelse(train_data$first_renewal_prediction > 0.5, TRUE, FALSE)
actuality <- ifelse(train_data$renewal_status == "Renewed", TRUE, FALSE)
confusionMatrix(table(prediction, actuality), positive = "TRUE")

#              actuality
# prediction   FALSE    TRUE
# FALSE        1718317  205139
# TRUE         11590   14036

# Sensitivity : 0.064040 (vs  0.038969 dtree_md5)     
# Specificity : 0.993300 (vs  0.996421 dtree_md5)     

############Train/test split##################

############Prep Data###################
# train<-mass_prep_data(train)
# test<-mass_prep_data(test)

###########Train Model##################

# first_renewal_model_train<-mass_build_model_first_renewal(train)
# saveRDS(first_renewal_model_train, "../../data/output/dtree/first_renewal_model_train")
# saveRDS(train, "../../data/output/dtree/train")
# saveRDS(test, "../../data/output/dtree/test")

first_renewal_model_train <- readRDS("../../data/output/dtree/first_renewal_model_train")
train <- readRDS("../../data/output/dtree/train")
test <- readRDS("../../data/output/dtree/test")

train_df <- rbindlist(train)
test_df <- rbindlist(test)

####Predict & Calc Accuracy on Test Set######

# first_renewal_model_test_predict <- mass_predict_first_renewal(
#                             test, first_renewal_model_train)
# saveRDS(first_renewal_model_test_predict, 
#         "../../data/output/dtree/first_renewal_model_test_predict")

first_renewal_model_test_predict <- readRDS(
  "../../data/output/dtree/first_renewal_model_test_predict")

first_renewal_model_test_predBin <- ifelse(
  first_renewal_model_test_predict$first_renewal_prediction >.5, 
  "Renewed", "Not Renewd")
confusionMatrix(table(first_renewal_model_test_predBin, 
                      first_renewal_model_test_predict$renewal_status), 
                positive = "Renewed")

#                Not Renewd Renewed
# Not Renewd       342410   40584
# Renewed            3307    3516

# Sensitivity : 0.07973 (vs  0.044921 on  dtree_md5_tt)  
# Specificity : 0.99043 (vs 0.995603 on dtree_md5_tt)


##############################################
#############Decision Trees###################
##############################################

###########All variables & Shallow#################

# Limited depth of 5
# train on train, test on test

names(train_df)[sapply(train_df, is.character)]

train_df_f <- train_df %>%
  mutate_if(sapply(train_df, is.character), as.factor)

names(train_df_f)[sapply(train_df_f, is.character)]

train_df_f$renew_Y <- format(as.Date(train_df_f$renew_date, 
                                     format="%Y-%m-%d"),"%Y")
train_df_f$creation_Y <- format(as.Date(train_df_f$creation_date, 
                                        format="%Y-%m-%d"),"%Y")
train_df_f$expiry_Y <- format(as.Date(train_df_f$expiry_date, 
                                      format="%Y-%m-%d"),"%Y")
train_df_f$renew_M <- format(as.Date(train_df_f$renew_date, 
                                     format="%Y-%m-%d"),"%m")
train_df_f$creation_M <- format(as.Date(train_df_f$creation_date, 
                                        format="%Y-%m-%d"),"%m")
train_df_f$expiry_M <- format(as.Date(train_df_f$expiry_date, 
                                      format="%Y-%m-%d"),"%m")

train_df_f <- subset(train_df_f, select = -c(renew_date,
                                             creation_date,
                                             expiry_date))
train_df_f <- subset(train_df_f, select = -c(sld,
                                             pattern,
                                             domain))
train_df_f <- subset(train_df_f, select = -c(sld_type2,
                                             tld_registrar_index,
                                             renew_reseller,
                                             renew_registrar))
train_df_f <- subset(train_df_f, select = -c(renewal_type,
                                           renewed_count,
                                           renew_type,
                                           autorenew_type,
                                           renew_period,
                                           renew_arpt))
train_df_f <- subset(train_df_f, select = -c(renew_domain_revenue,
                                           status))
train_df_f <- subset(train_df_f, select = -c(first_renewal_prediction,
                                             domain_id))
test_df_f <- test_df %>%
  mutate_if(sapply(test_df, is.character), as.factor)

names(test_df_f)[sapply(test_df_f, is.character)]

test_df_f$renew_Y <- format(as.Date(test_df_f$renew_date, 
                                     format="%Y-%m-%d"),"%Y")
test_df_f$creation_Y <- format(as.Date(test_df_f$creation_date, 
                                        format="%Y-%m-%d"),"%Y")
test_df_f$expiry_Y <- format(as.Date(test_df_f$expiry_date, 
                                      format="%Y-%m-%d"),"%Y")
test_df_f$renew_M <- format(as.Date(test_df_f$renew_date, 
                                     format="%Y-%m-%d"),"%m")
test_df_f$creation_M <- format(as.Date(test_df_f$creation_date, 
                                        format="%Y-%m-%d"),"%m")
test_df_f$expiry_M <- format(as.Date(test_df_f$expiry_date, 
                                      format="%Y-%m-%d"),"%m")

test_df_f <- subset(test_df_f, select = -c(renew_date,
                                             creation_date,
                                             expiry_date))
test_df_f <- subset(test_df_f, select = -c(sld,
                                             pattern,
                                             domain))
test_df_f <- subset(test_df_f, select = -c(sld_type2,
                                           tld_registrar_index,
                                           renew_reseller,
                                           renew_registrar))
test_df_f <- subset(test_df_f, select = -c(renewal_type,
                                           renewed_count,
                                           renew_type,
                                           autorenew_type,
                                           renew_period,
                                           renew_arpt))
test_df_f <- subset(test_df_f, select = -c(autorenew_type,
                                           renew_type,
                                           renew_domain_revenue,
                                           status))
test_df_f <- subset(test_df_f, select = -c(first_renewal_prediction,
                                           domain_id))

test_data<-readRDS("../../data/input/npv/test_data")


a <- c("a", "b", "c")
b <- c("b", "c", "d")
setdiff(names(train_df), names(test_data))

setdiff(b, a)

outputtree_01 <- ctree(renewal_status ~ .,
                         data = train_df_f, maxdepth = 5)

png(file = "../../data/output/dtree3/output_tree_01.png",
    width = 2000, height = 750)
plot(outputtree_01)
dev.off()
save(outputtree_01, file="../../data/output/dtree3/output_tree_01")

load("../../data/output/dtree3/output_tree_01")

# prediction & confuson matrix -- full datatse
t_predict_01 <- predict(outputtree_01, test_df_f)
confusionMatrix(table(t_predict_01, test_df_f$renewal_status), 
                positive = "Renewed")

# t_predict_01 Not Renewd Renewed
# Not Renewd     344698   42638
# Renewed          1019    1462

# Sensitivity : 0.033152      
# Specificity : 0.997053  

# (compare to dtree2/outputtree_04)


###########Super Simple & Shallow#################

# Same variables as segmented glms 
# (no tls & reseller/registrar)
# Limited depth of 5
# train on train, test on test

# outputtree_02 <- ctree(renewal_status ~ pattern_domain_count +
#                            log_reg_arpt + sld_length + gibb_score +
#                            sld_type + day_domains + reg_period,
#                          data = train_df, maxdepth = 5)
# 
# png(file = "../../data/output/dtree2/outputtree_02.png",
#     width = 2000, height = 750)
# plot(outputtree_02)
# dev.off()
# save(outputtree_02, file="../../data/output/dtree2/output_tree_02")

load("../../data/output/dtree2/output_tree_02")

# prediction & confuson matrix -- test dataset
t_predict_02 <- predict(outputtree_02, test_df)
confusionMatrix(table(t_predict_02, test_df$renewal_status), 
                positive = "Renewed")

# t_predict_02 Not Renewd Renewed
# Not Renewd     344197   42119
# Renewed          1520    1981
# 
# Sensitivity : 0.044921       
# Specificity : 0.995603 

#############Simple & Shallow#################

# Same variables as segmented glms 
# (incl. tld & reseller/registrar)
# Limited depth of 5
# train & test on full dataset (train_data)

# outputtree_03 <- ctree(renewal_status ~ factor(tld) + factor(reseller) + 
#                                  pattern_domain_count + log_reg_arpt + 
#                                  sld_length + gibb_score + sld_type + 
#                                  day_domains + reg_period, 
#                                data = train_data, maxdepth = 5)
# 
# png(file = "../../data/output/dtree2/outputtree_03.png",
#     width = 2000, height = 750)
# plot(outputtree_03)
# dev.off()
# save(outputtree_02, file="../../data/output/dtree2/outputtree_03")

load("../../data/output/dtree2/outputtree_03")

# prediction & confuson matrix -- full dataset
t_predict_03 <- predict(outputtree_03, train_data)
confusionMatrix(table(t_predict_03, train_data$renewal_status), 
                positive = "Renewed")

# t_predict_03 Not Renewd Renewed
# Not Renewd    1724863  212046
# Renewed          5044    7129

# Sensitivity : 0.032527        
# Specificity : 0.997084 

#############Simple & Shallow#################

# Same variables as segmented glms 
# (incl. tld & reseller/registrar)
# Limited depth of 5
# train & test on full dataset (train_data)

# outputtree_04 <- ctree(renewal_status ~ factor(tld) + factor(reseller) +
#                                  pattern_domain_count + log_reg_arpt +
#                                  sld_length + gibb_score + sld_type +
#                                  day_domains + reg_period,
#                                data = train_df, maxdepth = 5)
# 
# png(file = "../../data/output/dtree2/outputtree_04.png",
#     width = 2000, height = 750)
# plot(outputtree_03)
# dev.off()
# save(outputtree_02, file="../../data/output/dtree2/outputtree_04")

load("../../data/output/dtree2/outputtree_04")

# prediction & confuson matrix -- test dataset
t_predict_04 <- predict(outputtree_04, test_df)
confusionMatrix(table(t_predict_04, test_df$renewal_status), 
                positive = "Renewed")

# t_predict_04 Not Renewd Renewed
# Not Renewd     344813   42781
# Renewed           904    1319

# Sensitivity : 0.029909        
# Specificity : 0.997385 

##############Weighted Dtree test/train#################

# Same variables as segmented glms 
# (no tls & reseller/registrar)
# Limited depth of 5
# train on train, test on test
# reeighted for imbalanced data (W=8x)

W = ifelse(train_df$renewal_status=="Renewed", 8, 1)

# outputtree_05 <- ctree(renewal_status ~ pattern_domain_count +
#                            log_reg_arpt + sld_length + gibb_score +
#                            sld_type + day_domains + reg_period,
#                          data = train_df, maxdepth = 5, weights=W)
# 
# png(file = "../../data/output/dtree2/outputtree_05.png",
#     width = 2000, height = 750)
# plot(outputtree_05)
# dev.off()
# save(outputtree_05, file="../../data/output/dtree2/outputtree_05")

load("../../data/output/dtree2/outputtree_05")

# prediction & confuson matrix -- test dataset
t_predict_05 <- predict(outputtree_05, test_df)
confusionMatrix(table(t_predict_05, test_df$renewal_status), 
                positive = "Renewed")

# t_predict_05 Not Renewd Renewed
# Not Renewd      97771    3448
# Renewed        247946   40652
# 
# Sensitivity : 0.9218          
# Specificity : 0.2828  

##############Weighted Dtree test/train#################
###########Recreate Seg-Glm w/o tld_reg###############

# Same variables as segmented glms 
# (no tls & reseller/registrar)
# Limited depth of 5
# train on train, test on test
# reeighted for imbalanced data (W=recreate)

# W = ifelse(train_df$renewal_status=="Renewed", 3, 2) 
# t_predict_06 Not Renewd Renewed
# Not Renewd     341611   40310
# Renewed          4106    3790
# Sensitivity : 0.085941        
# Specificity : 0.988123 
# W = ifelse(train_df$renewal_status=="Renewed", 4, 3) 24 18
# t_predict_06 Not Renewd Renewed
# Not Renewd     341611   40310
# Renewed          4106    3790
# Sensitivity : 0.085941        
# Specificity : 0.988123  
# W = ifelse(train_df$renewal_status=="Renewed", 2, 1) 
# t_predict_06 Not Renewd Renewed
# Not Renewd     337639   38672
# Renewed          8078    5428
# Sensitivity : 0.12308        
# Specificity : 0.97663    
# W = ifelse(train_df$renewal_status=="Renewed", 6, 5) 24 20
# t_predict_06 Not Renewd Renewed
# Not Renewd     344201   42121
# Renewed          1516    1979
# Sensitivity : 0.044875       
# Specificity : 0.995615  
# W = ifelse(train_df$renewal_status=="Renewed", 24, 19)
# t_predict_06 Not Renewd Renewed
# Not Renewd     344170   42111
# Renewed          1547    1989
# Sensitivity : 0.045102      
# Specificity : 0.995525 

W = ifelse(train_df$renewal_status=="Renewed", 4, 3) 

outputtree_06 <- ctree(renewal_status ~ pattern_domain_count +
                         log_reg_arpt + sld_length + gibb_score +
                         sld_type + day_domains + reg_period,
                       data = train_df, maxdepth = 5, weights=W)

png(file = "../../data/output/dtree2/outputtree_06.png",
    width = 2000, height = 750)
plot(outputtree_06)
dev.off()
save(outputtree_06, file="../../data/output/dtree2/outputtree_06")

# load("../../data/output/dtree2/outputtree_06")

# prediction & confuson matrix -- test dataset
t_predict_06 <- predict(outputtree_06, test_df)
confusionMatrix(table(t_predict_06, test_df$renewal_status), 
                positive = "Renewed")


##############Weighted Dtree test/train#################
###########Recreate Seg-Glm w/ tld_reg###############

# Same variables as segmented glms 
# (no tls & reseller/registrar)
# Limited depth of 5
# train on train, test on test
# reweighted for imbalanced data (W=recreate)

W = ifelse(train_df$renewal_status=="Renewed", 4, 3)

outputtree_07 <- ctree(renewal_status ~ factor(tld) + factor(reseller) +
                         pattern_domain_count +
                         log_reg_arpt + sld_length + gibb_score +
                         sld_type + day_domains + reg_period,
                       data = train_df, maxdepth = 5, weights=W)

# png(file = "../../data/output/dtree2/outputtree_07.png",
#     width = 2000, height = 750)
# plot(outputtree_07)
# dev.off()
# save(outputtree_07, file="../../data/output/dtree2/outputtree_07")

# load("../../data/output/dtree2/outputtree_07")

# prediction & confuson matrix -- test dataset
t_predict_07 <- predict(outputtree_07, test_df)
confusionMatrix(table(t_predict_07, test_df$renewal_status), 
                positive = "Renewed")

##############Weighted Dtree test/train#################
###########Even less depth & w/o tld_reg###############

# Same variables as segmented glms 
# (no tls & reseller/registrar)
# Limited depth of 3
# train on train, test on test
# reeighted for imbalanced data (W=recreate)

W = ifelse(train_df$renewal_status=="Renewed", 4, 3) 

outputtree_08 <- ctree(renewal_status ~ pattern_domain_count +
                         log_reg_arpt + sld_length + gibb_score +
                         sld_type + day_domains + reg_period,
                       data = train_df, maxdepth = 3, weights=W)

png(file = "../../data/output/dtree2/outputtree_08.png",
    width = 2000, height = 750)
plot(outputtree_08)
dev.off()

save(outputtree_08, file="../../data/output/dtree2/outputtree_08.png")

# load("../../data/output/dtree2/outputtree_08")

# prediction & confuson matrix -- test dataset
t_predict_08 <- predict(outputtree_08, test_df)
confusionMatrix(table(t_predict_08, test_df$renewal_status), 
                positive = "Renewed")

