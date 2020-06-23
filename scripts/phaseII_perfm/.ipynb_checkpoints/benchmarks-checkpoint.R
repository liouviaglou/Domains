# in 06/08 meeting, we succesfully zeroed in on lift, gains curve & its AUC
# as appropriate performance metrics. this script establishes benchmakrs
# based on original segmented glm model & initial simple decision tree


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
library(caret)

library(partykit)
library(mlbench)
library(party)
library(caret)
library(vcd)

library(RColorBrewer)



setwd("/Users/lubagloukhov/Documents/Consulting/Radix/Domains_202003/scripts/paseII_perfm")
inputdir <- "../../data/input/"
outputdir <- "../../data/output/paseII_perfm"


# load functions
source('./../orig/functions.R')
source('./../orig/functions_models.R')


# functions

lift_funct <- function(depvar, predcol, groups=10) {
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)}
  if(is.factor(depvar)) depvar <- as.integer(depvar)
  if(is.factor(predcol)) predcol <- as.integer((predcol)
  helper = data.frame(cbind(depvar, predcol))
  helper[,"bucket"] = ntile(-helper[,"predcol"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(depvar), funs(total = n(),
                                    totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

depvar <- first_renewal_model_test_predict$renewal_status
predcol <- first_renewal_model_test_predict$first_renewal_prediction


##############################################
##                                          ##
## LIFT, GAINS CURVE, AUC for SEGMENTED GLM ##
##                                          ##
##############################################

# load data

# the output of running mass_predict_first_renewal() 
# on test 
#   20% leftover subset 
#      of train_data "../../data/output/npv/first_renewal_preds"
# w/ first_renewal_model_train
#   the output of running mass_build_model_first_renewal()
#   on train 
#     80% random subset 
#        of train_data "../../data/output/npv/first_renewal_preds"

first_renewal_model_test_predict <- readRDS(
  "../../data/output/dtree/first_renewal_model_test_predict")

View(first_renewal_model_test_predict)

# Defining a function
chart_lift <- function (pred_df=first_renewal_model_test_predict,
                        dep_var = "renewal_status",
                        pred_var = "first_renewal_prediction") {
  N <- 10  # total number of rows to preallocate--possibly an overestimate
  lift_df <- data.frame(P =rep(NA, N), 
                        actu_renwd2=rep(NA, N), 
                        gain=rep(NA, N), 
                        lift=rep(NA, N), 
                        stringsAsFactors=FALSE)          # you don't know levels yet
  actu_renwd <- sum(first_renewal_model_test_predict[[dep_var]]=='Renewed')
  
  i = 1
  for(P in seq(.1,1,length=10)){
    temp_df <- data.frame(pred_df)[c(dep_var,pred_var)]
    ttmp_df <- temp_df[order(temp_df[pred_var],decreasing = TRUE),][1:round(dim(temp_df)[1]*P),]
    actu_renwd2 <-  sum(ttmp_df[[dep_var]] == 'Renewed')
    gain = actu_renwd2/actu_renwd
    lift = gain/(P)
    
    lift_df[i, ] <- list(P, actu_renwd2, gain, lift)
    i = i+1
  }
  return(lift_df)
}

lift_df <- chart_lift()

plot_gains <- function (lift_df=lift_df) {
  lift_df2 <- lift_df %>%
    add_row(P = 0, gain =0) %>%
    arrange(P)
  
  gains_plot <- ggplot(lift_df2, aes(P,  gain)) +
    geom_point() +
    geom_line() +
    # This makes another geom_line that only sees the first and last row of the data
    geom_line(data = lift_df2 %>% slice(1, n())) +
    
    scale_y_continuous(breaks = seq(0, 1, by = .1), limits = c(0,1)) +
    scale_x_continuous(breaks = seq(0, 1, by = .1)) +
    labs(title = "Cumulative Gains Plot - Seg Glm",
         y = "Cumulative Gain %")
  
  return(gains_plot)
}

gains_plot <- plot_gains (lift_df) 

calc_auc <- function (lift_df=lift_df) {
  lift_df2 <- as.data.table(lift_df)
  lift_df2 <- subset(lift_df2, select = c("P","gain"))
  names(lift_df2) <- c("x0", "y0")
  lift_df2 <- rbind(list(0, 0), lift_df2)
  lift_df2[, c("x1","y1") := list(x0, y0)]
  lift_df2[, c("x1","y1") := shift(.SD, 1, 0, "lead"), .SDcols=c("x0","y0")]
  lift_df2[, area:=(x1-x0)*y0+.5*(y1-y0)*(x1-x0)]
  lift_df2 <- lift_df2[-nrow(lift_df2),]
  auc <- sum(lift_df2$area)
  return(auc)
}

auc <- calc_auc(lift_df)


##############################################
##                                          ##
## LIFT, GAINS CURVE, AUC for BENCH DECTREE ##
##                                          ##
##############################################

# from spreadhseet Radix Dtre Compare
# DTree (w tld_reg) weighted_4_3	outputtree_07

load("../../data/output/dtree2/outputtree_07")
test <- readRDS("../../data/output/dtree/test")
test_df <- rbindlist(test)

# prediction 
t_predict_07 <- data.frame(predict(outputtree_07, test_df, type="prob"))
test_df[, "dtree_renewal_prediction" := t_predict_07$Renewed]

lift_df_2 <- chart_lift(pred_df=test_df,
                      dep_var = "renewal_status",
                      pred_var = "dtree_renewal_prediction")
gains_plot_2 <- plot_gains (lift_df_2) 

auc_2 <- calc_auc(lift_df_2)



##############################################
##                                          ##
##             multi-GAINS plot             ##
##                                          ##
##############################################

plot_multigains <- function (lift_df_list=list(seg_glm = lift_df, 
                                               ben_dtr = lift_df_2),
                             auc_list = list(seg_glm = auc, 
                                             ben_dtr=auc_2)) {
  
  lift_df_list <- lapply(lift_df_list, function(df) {
    df <- df %>%
      add_row(P = 0, gain =0) %>%
      arrange(P)
  })
  
  # colors <- brewer.pal(n = length(lift_df_list), name = "Set2")
  # colors <- colors[1:length(lift_df_list)]
  # name_map = paste(names(lift_df_list),colors, sep="=")[1:length(lift_df_list)]
  
  auc_list = lapply(auc_list, round,4)
  auc_map = paste(names(lift_df_list),auc_list, sep=" = ")[1:length(lift_df_list)]
  
  
  gains_plot <- ggplot(NULL, aes(P,  gain)) +
    geom_line(data = lift_df_list[[1]] %>% slice(1, n())) +
    
    scale_y_continuous(breaks = seq(0, 1, by = .1), limits = c(0,1)) +
    scale_x_continuous(breaks = seq(0, 1, by = .1)) +
    labs(title = "Cumulative Gains Plot",
         y = "Cumulative Gain",
         x = "Percentile")
  
  for(i in seq(length(lift_df_list))){
    name = names(lift_df_list)[[i]]
    df = lift_df_list[[i]]
    color=colors[[i]]
    auc = auc_list[[i]]
    gains_plot <- gains_plot + list(geom_line(data=df), 
                                    geom_point(data=df))+ 
      annotate("text", x = .8, y = .6-i*.1, label = auc_map[[i]])
  }
  
  
  return(gains_plot)
}

plot_multigains()
