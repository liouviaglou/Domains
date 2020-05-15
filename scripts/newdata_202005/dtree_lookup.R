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
# renewal_training_data <- readRDS(file.path(inputdir, 
                                           "renewal_training_data_2020_02_25"))
# dim(renewal_training_data)
# [1] 5553487      37

# Subset.05% of the data for plotting
# renewal_training_data_sub <- renewal_training_data[sample(.N,nrow(renewal_training_data)*.0005)]

# table(renewal_training_data$reseller_country)
# 60 countries or so

# prep data
# renewal_training_data_sub_prepped<-mass_prep_data(renewal_training_data_sub)
# saveRDS(renewal_training_data_sub_prepped, 
#         file.path(outputdir, "renewal_training_data_sub_prepped"))

renewal_training_data_sub_prepped <- readRDS("../../data/output/newdata_202005/renewal_training_data_sub_prepped")
renewal_training_data_sub_prepped_df <- rbindlist(renewal_training_data_sub_prepped)
dim(renewal_training_data_sub_prepped_df)
# [1] 2776   37
# str(renewal_training_data_prepped_df)


# retrain decision tree on training data but with new factor levels
# train <- readRDS("../../data/output/dtree/train")
# train_df <- rbindlist(train)
# test <- readRDS("../../data/output/dtree/test")
# test_df <- rbindlist(train)
# levels(train_df$reseller ) <- c(levels(train_df$reseller ), 
#                                 levels(factor(
#                                   renewal_training_data_prepped_df$reseller))) 

# W = ifelse(train_df$renewal_status=="Renewed", 4, 3)
# outputtree_07 <- ctree(renewal_status ~ factor(tld) + factor(reseller) +
#                          pattern_domain_count +
#                          log_reg_arpt + sld_length + gibb_score +
#                          sld_type + day_domains + reg_period,
#                        data = train_df, maxdepth = 5, weights=W)
# 
# save(outputtree_07, file=file.path(outputdir, "outputtree_07"))
# png(file = file.path(outputdir, "outputtree_07.png"),
#     width = 2000, height = 750)
# plot(outputtree_07)
# dev.off()

load("../../data/output/dtree2/outputtree_07")

# prediction & confuson matrix -- test dataset
t_predict_07 <- predict(outputtree_07, test_df)
confusionMatrix(table(t_predict_07, test_df$renewal_status), 
                positive = "Renewed")

# t_predict_07 Not Renewd Renewed
# Not Renewd    1369415  160269
# Renewed         14775   14806
# Sensitivity : 0.084569        
# Specificity : 0.989326   


# formula(outputtree_07)
# # renewal_status ~ factor(tld) + factor(reseller) + pattern_domain_count + 
# #   log_reg_arpt + sld_length + gibb_score + sld_type + day_domains + 
# #   reg_period

# # predict
renewal_training_data_sub_prepped_df$dtre_prob <- predict(outputtree_07, 
         newdata = renewal_training_data_sub_prepped_df, 
         type = "prob")

# Error in model.frame.default(delete.response(object$terms), newdata, xlev = xlev) : factor factor(reseller) has new levels 1&1 internet, 10d
