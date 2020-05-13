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

train_index <- sample(1:nrow(train_data), 0.8 * nrow(train_data))
test_index <- setdiff(1:nrow(train_data), train_index)

# Build X_train, y_train, X_test, y_test

train <- train_data[train_index,]
test <- train_data[test_index,]

summary(subset(train_data,,c(predictors, response)))

# "tld" : url extension (such as .site, .online, etc)
# "reseller", : client ( "gmo", "go daddy", "namecheap")
# "pattern_domain_count", 
# "log_reg_arpt",
# "sld_length", 
# "gibb_score",
# "sld_type", 
# "day_domains",
# "reg_period"

predictors <- c("tld", "reseller",
                "pattern_domain_count", 
                "log_reg_arpt",
                "sld_length", 
                "gibb_score",
                "sld_type", 
                "day_domains",
                "reg_period")
response <- "renewal_status"
setdiff(predictors, names(train_data))



###############Orig model#####################

first_renewal_model <- readRDS("../../data/output/npv/first_renewal_model")

prediction <- ifelse(train_data$first_renewal_prediction > 0.5, TRUE, FALSE)
actuality <- ifelse(train_data$renewal_status == "Renewed", TRUE, FALSE)
confusionMatrix(table(prediction, actuality), positive = "TRUE")

##############################################
###############Orig model#####################
############Train/test split##################
##############################################

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

######Predict & Calc Accuracy###########
# first_renewal_model_test_predict <- mass_predict_first_renewal(
#                             test, first_renewal_model_train)
# saveRDS(first_renewal_model_test_predict, 
#         "../../data/output/dtree/first_renewal_model_test_predict")

first_renewal_model_test_predict <- readRDS(
  "../../data/output/dtree/first_renewal_model_test_predict")

first_renewal_model_test_predBin <- ifelse(
  first_renewal_model_test_predict$first_renewal_prediction >.5, "Renewed", "Not Renewd")
confusionMatrix(table(first_renewal_model_test_predBin, 
                      first_renewal_model_test_predict$renewal_status), 
                positive = "Renewed")

##############################################
###############First DTree model##############
##############Max Depth 5 - EZ Viz############
############Train/test split##################
##############################################


output.tree_train_md5 <- ctree(renewal_status ~ pattern_domain_count + 
                           log_reg_arpt + sld_length + gibb_score + 
                           sld_type + day_domains + reg_period, 
                         data = train_df, maxdepth = 5)

png(file = "../../data/output/dtree/first_renewal_dtree_train_md5_032420_01.png",
    width = 3000, height = 750,)
plot(output.tree_train_md5)
dev.off()

first_dtree_md5_test_predict <- predict(output.tree_train_md5, test_df)
saveRDS(first_dtree_md5_test_predict,
        "../../data/output/dtree/first_dtree_md5_test_predict")

confusionMatrix(table(first_dtree_md5_test_predict, 
                      first_renewal_model_test_predict$renewal_status), 
                positive = "Renewed")

# try with weights for imbalanced data

W = ifelse(train_df$renewal_status=="Renewed", 8, 1)
output.tree_train_md5_W8 = ctree(renewal_status ~ pattern_domain_count + 
                                   log_reg_arpt + sld_length + gibb_score + 
                                   sld_type + day_domains + reg_period, 
                                 data = train_df, maxdepth = 5, weights=W)


png(file = "../../data/output/dtree/first_renewal_dtree_train_md5_032420_02_W8.png",
    width = 3000, height = 750,)
plot(output.tree_train_md5_W8)
dev.off()

first_dtree_md5_W8_test_predict <- predict(output.tree_train_md5_W8, test_df)
saveRDS(first_dtree_md5_W8_test_predict,
        "../../data/output/dtree/first_dtree_md5_W8_test_predict")

confusionMatrix(table(first_dtree_md5_W8_test_predict, 
                      first_renewal_model_test_predict$renewal_status), 
                positive = "Renewed")

W = ifelse(train_df$renewal_status=="Renewed", 2, 1)
output.tree_train_md5_W2 = ctree(renewal_status ~ pattern_domain_count + 
                                   log_reg_arpt + sld_length + gibb_score + 
                                   sld_type + day_domains + reg_period, 
                                 data = train_df, maxdepth = 5, weights=W)


png(file = "../../data/output/dtree/first_renewal_dtree_train_md5_032420_03_W2.png",
    width = 3000, height = 750,)
plot(output.tree_train_md5_W2)
dev.off()

first_dtree_md5_W2_test_predict <- predict(output.tree_train_md5_W2, test_df)
saveRDS(first_dtree_md5_W2_test_predict,
        "../../data/output/dtree/first_dtree_md5_W2_test_predict")

confusionMatrix(table(first_dtree_md5_W2_test_predict, 
                      first_renewal_model_test_predict$renewal_status), 
                positive = "Renewed")



#############First DTree model#####################

# output.tree <- load("../../data/output/npv/first_renewal_tree")

# t_predict <- predict(output.tree, train_data)
# saveRDS(t_predict, "../../data/output/npv/t_predict")
# 
# actuality <- ifelse(train_data$renewal_status == "Renewed", TRUE, FALSE)
# confusionMatrix(table(t_prediction, actuality), positive = "TRUE")


###############First DTree model#####################
##############Max Depth 5 - EZ Viz####################
##########Train & Test on Full Datast##################


# paste(response, paste(predictors, collapse=" + "), sep=" ~ ")
# 
# 
# output.tree_md5 <- ctree(renewal_status ~ pattern_domain_count + 
#                            log_reg_arpt + sld_length + gibb_score + 
#                            sld_type + day_domains + reg_period, 
#                          data = train_data, maxdepth = 5)
# 
# png(file = "../../data/output/npv/first_renewal_dtree_md5_032420_01.png",
#     width = 2000, height = 750,)
# # Plot the tree.
# plot(output.tree_md5)
# # Save the file.
# dev.off()
# Save RObj
# save(output.tree_md5, file="../../data/output/npv/first_renewal_tree_md5")

load("../../data/output/npv/first_renewal_tree_md5")

#simpler viz's
# png(file = "../../data/output/npv/first_renewal_dtree_md5_032420_02.png",
#     width = 2000, height = 750,)
# st <- as.simpleparty(output.tree_md5)
# plot(st)
# dev.off()

# png(file = "../../data/output/npv/first_renewal_dtree_md5_032420_03.png",
#     width = 2000, height = 750,)
# myfun <- function(i) c(
#   as.character(i$prediction),
#   paste("n =", i$n),
#   format(round(i$distribution/i$n, digits = 3), nsmall = 3)
# )
# plot(st, tp_args = list(FUN = myfun), ep_args = list(justmin = 20))
# dev.off()

# prediction & confuson matrix -- full datatse
t_predict_md5 <- predict(output.tree_md5, train_data)
confusionMatrix(table(t_predict_md5, train_data$renewal_status), positive = "Renewed")

# t_predict_md5 Not Renewd Renewed
# Not Renewd       1723716  210634
# Renewed             6191    8541

# Sensitivity : 0.038969        
# Specificity : 0.996421 

###############First DTree model#####################
##############Max Depth 5 - EZ Viz####################
##########Train on train, Test on test##################



# paste(response, paste(predictors, collapse=" + "), sep=" ~ ")
# 
# 
# output.tree_md5_tt <- ctree(renewal_status ~ pattern_domain_count +
#                            log_reg_arpt + sld_length + gibb_score +
#                            sld_type + day_domains + reg_period,
#                          data = train_df, maxdepth = 5)
# 
# png(file = "../../data/output/npv/first_renewal_dtree_md5_032420_tt.png",
#     width = 2000, height = 750,)
# # Plot the tree.
# plot(output.tree_md5_tt)
# # Save the file.
# dev.off()
# # Save RObj
# save(output.tree_md5_tt, file="../../data/output/npv/first_renewal_tree_md5_tt")

load("../../data/output/npv/first_renewal_tree_md5_tt")

#simpler viz's
# png(file = "../../data/output/npv/first_renewal_dtree_md5_032420_02.png",
#     width = 2000, height = 750,)
# st <- as.simpleparty(output.tree_md5)
# plot(st)
# dev.off()

# png(file = "../../data/output/npv/first_renewal_dtree_md5_032420_03.png",
#     width = 2000, height = 750,)
# myfun <- function(i) c(
#   as.character(i$prediction),
#   paste("n =", i$n),
#   format(round(i$distribution/i$n, digits = 3), nsmall = 3)
# )
# plot(st, tp_args = list(FUN = myfun), ep_args = list(justmin = 20))
# dev.off()

# prediction & confuson matrix -- test dataset
t_predict_md5_tt <- predict(output.tree_md5_tt, test_df)
confusionMatrix(table(t_predict_md5_tt, test_df$renewal_status), positive = "Renewed")

# t_predict_md5_tt Not Renewd Renewed
# Not Renewd           344197   42119
# Renewed                1520    1981

# Sensitivity : 0.044921       
# Specificity : 0.995603  



###############First DTree model#####################
############## including reseller####################

train_data$reseller <- as.factor(train_data$reseller)
output.tree_md5_r <- ctree(renewal_status ~ pattern_domain_count + 
                           log_reg_arpt + sld_length + gibb_score + 
                           sld_type + day_domains + reg_period + reseller, 
                         data = train_data, maxdepth = 5)

png(file = "../../data/output/npv/first_renewal_dtree_md5_r_032420_01.png",
    width = 2000, height = 750,)
# Plot the tree.
plot(output.tree_md5)
# Save the file.
dev.off()
# Save RObj
# save(output.tree_md5, file="../../data/output/npv/first_renewal_tree_md5")

#simpler viz's
png(file = "../../data/output/npv/first_renewal_dtree_md5_032420_02.png",
    width = 2000, height = 750,)
st <- as.simpleparty(output.tree_md5)
plot(st)
dev.off()

png(file = "../../data/output/npv/first_renewal_dtree_md5_032420_03.png",
    width = 2000, height = 750,)
myfun <- function(i) c(
  as.character(i$prediction),
  paste("n =", i$n),
  format(round(i$distribution/i$n, digits = 3), nsmall = 3)
)
plot(st, tp_args = list(FUN = myfun), ep_args = list(justmin = 20))
dev.off()

# prediction & confuson matrix
t_predict_md5 <- predict(output.tree_md5, train_data)
confusionMatrix(table(t_predict_md5, train_data$renewal_status), positive = "Renewed")
