##### Evaluating Decision Tree performance vs. LookupTables


##############################################
#############              ###################
############# HOUSEKEEPING ###################
#############              ###################
##############################################


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
library(pbapply)

library(partykit)
library(caret)


setwd("/Users/lubagloukhov/Documents/Consulting/Radix/Domains_202003/scripts/newdata_202005")
inputdir <- "../../data/input/newdata_202005/datashare"
outputdir <- "../../data/output/newdata_202005"


# load functions
source('./../orig/functions.R')
source('./../orig/functions_models.R')


# load data
renewal_training_data <- readRDS(file.path(inputdir,
                                           "renewal_training_data_2020_02_25"))
# dim(renewal_training_data)
# [1] 5553487      37

# Subset 1% of the data for plotting
renewal_training_data_sub <- renewal_training_data[sample(.N,nrow(renewal_training_data)*.01)]

# table(renewal_training_data$reseller_country)
# 60 countries or so

# prep data 
#  (1m37.896s for .0005 subset "renewal_training_data_sub_prepped")
#  (10m11.809s for .01 subset "renewal_training_data_sub_prepped_01")
renewal_training_data_sub_prepped<-mass_prep_data(renewal_training_data_sub)
saveRDS(renewal_training_data_sub_prepped,
        file.path(outputdir, "renewal_training_data_sub_prepped_01"))

# renewal_training_data_sub_prepped <- readRDS("../../data/output/newdata_202005/renewal_training_data_sub_prepped")
# renewal_training_data_sub_prepped_df <- rbindlist(renewal_training_data_sub_prepped)
# dim(renewal_training_data_sub_prepped_df)
# # [1] 2776   37
# # str(renewal_training_data_prepped_df)
# 
# # refactor vars to correspond to previously trained model
# 
# # examine training data of existing model
# train <- readRDS("../../data/output/dtree/train")
# train_df = rbindlist(train)
# test <- readRDS("../../data/output/dtree/test")
# test_df = rbindlist(test)
# 
# 
# renewal_training_data_sub_prepped_df$reseller <- factor(
#   renewal_training_data_sub_prepped_df$reseller, levels=levels(train_df$reseller)
# )
# table(renewal_training_data_sub_prepped_df$reseller)
# levels(renewal_training_data_sub_prepped_df$reseller)
# sum(is.na(renewal_training_data_sub_prepped_df$reseller))
# length(is.na(renewal_training_data_sub_prepped_df$reseller))
# 
# # missing data?
# na_vect <- lapply(renewal_training_data_sub_prepped_df, function(x) sum(is.na(x)))
# na_vect[na_vect>0]
# 
# # load pretrained decision tree
# load("../../data/output/dtree2/outputtree_07")
# # predict
# dtre_prob <- predict(outputtree_07, 
#                      renewal_training_data_sub_prepped_df, 
#                      type = "prob")
# renewal_training_data_sub_prepped_df$dtre_prob <- predict(outputtree_07, 
#          renewal_training_data_sub_prepped_df, 
#          type = "prob")[,1]
# 
# # ctree skisps over obs with missing vars, resulting in a predictvector shorter than newdata -- try rpart instead
# library(tidyverse)
# library(caret)
# library(rpart)
# library(rpart.plot)
# 
# # W = ifelse(train_df$renewal_status=="Renewed", 4, 3)
# # 
# # outputtree_07_rpart <- rpart(renewal_status ~ factor(tld) + factor(reseller) +
# #                          pattern_domain_count +
# #                          log_reg_arpt + sld_length + gibb_score +
# #                          sld_type + day_domains + reg_period,
# #                        data = train_df, method = "class", weights=W,
# #                        control = rpart.control(minsplit=1, minbucket=1, 
# #                                                cp=0.001, maxdepth = 5)
# #                        )
# # 
# # save(outputtree_07_rpart, 
# #      file=file.path(outputdir, "outputtree_07rpart"))
# # png(file = file.path(outputdir, "outputtree_07rpart.png"),
# #     width = 2000, height = 750)
# # rpart.plot(outputtree_07_rpart)
# # dev.off()
# 
# # load("../../data/output/dtree2/outputtree_07")
# 
# # prediction & confuson matrix -- test dataset
# t_predict_07_rpart <- predict(outputtree_07_rpart, test_df, type="class")
# confusionMatrix(table(t_predict_07_rpart, test_df$renewal_status),
#                 positive = "Renewed")
# 
# # t_predict_07_rpart Not Renewd Renewed
# # Not Renewd     343522   40530
# # Renewed          2590    3175
# # 
# # Sensitivity : 0.072646 (vs ctree 0.082676 )       
# # Specificity :  0.992517 (vs ctree 0.989436 )
# 
# 
# # calculate lift????????? prop in model table less than prop w/o model?
# dim(subset(renewal_training_data_sub_prepped_df,
#            renewal_training_data_sub_prepped_df$renewal_status=="Renewed")) 
# #329 obs
# var <- "dtre_prob"
# perc <-  .05
# renewal_training_data_sub_prepped_df_01 <- data.frame(renewal_training_data_sub_prepped_df)
# renewal_training_data_sub_prepped_df_01 <- renewal_training_data_sub_prepped_df_01[
#   order(renewal_training_data_sub_prepped_df_01[var],decreasing = TRUE),][
#     1:round(dim(renewal_training_data_sub_prepped_df_01)[1]*perc),]
# dim(subset(renewal_training_data_sub_prepped_df_01,
#            renewal_training_data_sub_prepped_df_01$renewal_status=="Renewed"))[1]/(dim(subset(renewal_training_data_sub_prepped_df,renewal_training_data_sub_prepped_df$renewal_status=="Renewed"))[1])/perc
# 
# 
# # predict
# renewal_training_data_sub_prepped_df$dtre_prob <- predict(outputtree_07_rpart, 
#            renewal_training_data_sub_prepped_df,  type = "prob")[,1]
# 
# 
# # compare to lookup table
# head(renewal_training_data_sub_prepped_df)
# prop.table(table(renewal_training_data_sub_prepped_df$renewal_status))
# 
