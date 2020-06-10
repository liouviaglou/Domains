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
library(mlbench)
library(party)
library(caret)
library(vcd)


setwd("/Users/lubagloukhov/Documents/Consulting/Radix/Domains_202003/scripts/newdata_202005")
inputdir <- "../../data/input/newdata_202005/datashare"
outputdir <- "../../data/output/newdata_202005"


# load functions
source('./../orig/functions.R')
source('./../orig/functions_models.R')

# load data (lookup table)
# renewal_training_data_sub_prepped <- readRDS("../../data/output/newdata_202005/renewal_training_data_sub_prepped")
# renewal_training_data_sub_prepped_df <- rbindlist(renewal_training_data_sub_prepped)
# dim(renewal_training_data_sub_prepped_df)
# [1] 2776   37

# # load data (dtree)
# train <- readRDS("../../data/output/dtree/train")
# test <- readRDS("../../data/output/dtree/test")
# train_df <- rbindlist(train)
# test_df <- rbindlist(test)

# #fit mob on PIMA data
# data("PimaIndiansDiabetes", package = "mlbench")
# pid_formula <- diabetes ~ glucose | pregnant + pressure + triceps + insulin + mass + pedigree + age
# logit <- function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
#   glm(y ~ 0 + x, family = binomial, start = start)
# }
# pid_tree <- mob(pid_formula, data = PimaIndiansDiabetes, fit = logit)
# pid_tree2 <- glmtree(diabetes ~ glucose | pregnant + pressure + triceps + insulin + mass + pedigree + age, data = PimaIndiansDiabetes, family = binomial)
# plot(pid_tree) 
# plot(pid_tree2) 
# 
# # fit mob on dtree data -- first, just partitioning on tld & reseller
# predictors <- c("tld",
#                 "reseller", # same as registrar for this dataset
#                 "pattern_domain_count",
#                 "log_reg_arpt",
#                 "sld_length",
#                 "gibb_score",
#                 "sld_type",
#                 "day_domains",
#                 "reg_period")
# response <- "renewal_status"
# paste(response, paste(predictors, collapse=" + "), sep=" ~ ")
# formula01 <- renewal_status ~  pattern_domain_count + log_reg_arpt + sld_length + gibb_score + sld_type + day_domains + reg_period |  tld + reseller 
# logit <- function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
#   glm(y ~ 0 + x, family = binomial, start = start)
#   }
# mobtree01 <- mob(formula01, data = train_df, fit = logit)

# save(mobtree01, 
#      file=file.path(outputdir, "mobtree01"))
# png(file = file=file.path(outputdir, "mobtree01.png")),
#     width = 2000, height = 750)
# plot(mobtree01)
# dev.off()

load(file.path(outputdir, "mobtree01"))

# prediction & confuson matrix
mtpredict01 <- predict(mobtree01, test_df)
confusionMatrix(table(mtpredict01, test_df$renewal_status),
                positive = "Renewed")

# t_predict_03 Not Renewd Renewed
# Not Renewd    1724863  212046
# Renewed          5044    7129

# Sensitivity : 0.032527
# Specificity : 0.997084

