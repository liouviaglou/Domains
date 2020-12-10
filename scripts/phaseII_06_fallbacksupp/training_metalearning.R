# Rscript training_metalearning.R > /home/jupyter/Domains_202003/data/output/training_metalearning.log 2>&1

# Takes as input preds_df output from predictions_metalearning.R
# Supplements with fallback
# Computes overall model performance
# Engineers features at tld-reseller level
# Trains metalearning model(s) to assign model based on features
# Assigns model based on previous

suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(ranger))
suppressMessages(library(pbapply))
suppressMessages(library(stringr))
suppressMessages(library(e1071))

source('functions_fallback.R')
source('functions_metalearning.R')
source('../phaseII_03_forest/functions_eval.R')


dataDir='/home/jupyter/Domains_202003/data/output/datapull_20201116'
modelDir='/home/jupyter/Domains_202003/data/output/models_20201104'
outputDir='/home/jupyter/Domains_202003/data/output/datapull_20201127'

########################################################################################################
#
# LOAD DATA
#
########################################################################################################
# Load preds output from predictions_metalearning.R
#   Notes: 1. Script writes a preds.csv file to fullDir/preds 
#             (/home/jupyter/Domains_202003/data/output/models_2020****/preds)
#          2. Script reads in RData & preprocesses (creating train & test subsets at the end) 
#             via load_prep_data_expiry_2.R
#          ... but in this case, we had some data pull issues (variables tld & geo excluded)
#              that required me to hack together train & test and subsequent predictions
#              instead of rerunning the entire pipeline
expiry_df_test_preds <- read.csv(file.path(dataDir,"expiry_df_test_preds.csv"))

# Load training data used for predictions_metalearning.R to assign fallback values
expiry_df_train <- read.csv(file.path(dataDir,"expiry_df_train.csv"))

# Load geo_suppl for train and test-pred data
geoLookupDF <- read.csv("/home/jupyter/Domains_202003/data/input/PredictiveModelAnalysis_ResellerGeoMap.csv")

# Supplement both train and test_preds with geo information
expiry_df_train_g <- geo_suppl(expiry_df_train, geoLookupDF = geoLookupDF)
expiry_df_test_preds_g <- geo_suppl(expiry_df_test_preds, geoLookupDF = geoLookupDF)


########################################################################################################
#
# SUPPLEMENT predictions w/ FALLBACK
#
########################################################################################################

# generate list of fallback tables
# npv_fallback_list = fallback_gen( npv_historic_renewal_data = expiry_df_train_g, 
#                                  reseller_am_geo_map = geoLookupDF)

# # return list members to in-memory objects of the same name
# names(npv_fallback_list)
# for(i in 1:length(npv_fallback_list)) assign(names(npv_fallback_list)[i], npv_fallback_list[[i]])

# # generate list of low-volume tld-re's from training data
# tld_registrar_excl_list = tld_registrar_excl_df(expiry_df_train_g)

# # generate placeholder (*_fb) columns in preds df where predictions for low-volume tld-registrars get set to NA
# expiry_df_test_preds_g <- expiry_df_test_preds_g %>%
#      mutate( across(contains('pred_'), 
#                     .fns = list(fb = ~ifelse(tld_registrar_index %in% tld_registrar_excl_list, NA, . )) ))

# # apply fallback tables (creating cols *_fb2)
# expiry_df_test_preds_g <- fallback_app_1(test_data_op=expiry_df_test_preds_g,
#                in_col='pred_seg2_rf_ALL_fb',
#                out_col='pred_seg2_rf_ALL_fb2')
# expiry_df_test_preds_g <- fallback_app_1(test_data_op=expiry_df_test_preds_g,
#                in_col='pred_seg2_glm_ALL_fb',
#                out_col='pred_seg2_glm_ALL_fb2')
# expiry_df_test_preds_g <- fallback_app_1(test_data_op=expiry_df_test_preds_g,
#                in_col='pred_seg_rf_ALL_fb',
#                out_col='pred_seg_rf_ALL_fb2')
# expiry_df_test_preds_g <- fallback_app_1(test_data_op=expiry_df_test_preds_g,
#                in_col='pred_seg_glm_ALL_fb',
#                out_col='pred_seg_glm_ALL_fb2')
# expiry_df_test_preds_g <- fallback_app_1(test_data_op=expiry_df_test_preds_g,
#                in_col='pred_agg_rf_fb',
#                out_col='pred_agg_rf_fb2')
# expiry_df_test_preds_g <- fallback_app_1(test_data_op=expiry_df_test_preds_g,
#                in_col='pred_agg_glm_fb',
#                out_col='pred_agg_glm_fb2')
# expiry_df_test_preds_g <- fallback_app_1(test_data_op=expiry_df_test_preds_g,
#                in_col='pred_agg_rf_ALL_fb',
#                out_col='pred_agg_rf_ALL_fb2')
# expiry_df_test_preds_g <- fallback_app_1(test_data_op=expiry_df_test_preds_g,
#                in_col='pred_agg_glm_ALL_fb',
#                out_col='pred_agg_glm_ALL_fb2')


# ########################################################################################################
# #
# # GENERATE Meta-(performance)Metrics at tld-reseller level
# #
# ########################################################################################################


# metrics_df <- expiry_df_test_preds_g %>%
#   group_by(tld_registrar_index) %>%
#   do( l10_seg2_glm = l10_dplyr(., pred_var = "pred_seg2_glm_ALL"),
#       l10_seg_glm = l10_dplyr(., pred_var = "pred_seg_glm_ALL"),
#        l10_agg_glm_ALL = l10_dplyr(., pred_var = "pred_agg_glm_ALL"),
#        l10_agg_glm = l10_dplyr(., pred_var = "pred_agg_glm"),
#        l10_seg2_rf = l10_dplyr(., pred_var = "pred_seg2_rf_ALL"),
#        l10_seg_rf = l10_dplyr(., pred_var = "pred_seg_rf_ALL"),
#        l10_agg_rf = l10_dplyr(., pred_var = "pred_agg_rf"),
#        l10_agg_rf_ALL = l10_dplyr(., pred_var = "pred_agg_rf_ALL"),
     
#       auc_seg2_glm = auc_dplyr(., pred_var = "pred_seg2_glm_ALL"),
#       auc_seg_glm = auc_dplyr(., pred_var = "pred_seg_glm_ALL"),
#        auc_agg_glm_ALL = auc_dplyr(., pred_var = "pred_agg_glm_ALL"),
#        auc_agg_glm = auc_dplyr(., pred_var = "pred_agg_glm"),
#        auc_seg2_rf = auc_dplyr(., pred_var = "pred_seg2_rf_ALL"),
#        auc_seg_rf = auc_dplyr(., pred_var = "pred_seg_rf_ALL"),
#        auc_agg_rf_ALL = auc_dplyr(., pred_var = "pred_agg_rf_ALL"),
#        auc_agg_rf = auc_dplyr(., pred_var = "pred_agg_rf"),
      
#      l10_seg2_glm_fb = l10_dplyr(., pred_var = "pred_seg2_glm_ALL_fb2"),
#       l10_seg_glm_fb = l10_dplyr(., pred_var = "pred_seg_glm_ALL_fb2"),
#        l10_agg_glm_fb = l10_dplyr(., pred_var = "pred_agg_glm_fb2"),
#        l10_agg_glm_ALL_fb = l10_dplyr(., pred_var = "pred_agg_glm_ALL_fb2"),
#        l10_seg2_rf_fb = l10_dplyr(., pred_var = "pred_seg2_rf_ALL_fb2"),
#        l10_seg_rf_fb = l10_dplyr(., pred_var = "pred_seg_rf_ALL_fb2"),
#        l10_agg_rf_fb = l10_dplyr(., pred_var = "pred_agg_rf_fb2"),
#        l10_agg_rf_ALL_fb = l10_dplyr(., pred_var = "pred_agg_rf_ALL_fb2"),
     
#       auc_seg2_glm_fb = auc_dplyr(., pred_var = "pred_seg2_glm_ALL_fb2"),
#       auc_seg_glm_fb = auc_dplyr(., pred_var = "pred_seg_glm_ALL_fb2"),
#        auc_agg_glm_fb = auc_dplyr(., pred_var = "pred_agg_glm_fb2"),
#        auc_agg_glm_ALL_fb = auc_dplyr(., pred_var = "pred_agg_glm_ALL_fb2"),
#        auc_seg2_rf_fb = auc_dplyr(., pred_var = "pred_seg2_rf_ALL_fb2"),
#        auc_seg_rf_fb = auc_dplyr(., pred_var = "pred_seg_rf_ALL_fb2"),
#        auc_agg_rf_fb = auc_dplyr(., pred_var = "pred_agg_rf_fb2"),
#        auc_agg_rf_ALL_fb = auc_dplyr(., pred_var = "pred_agg_rf_ALL_fb2"),
#       )


# ########################################################################################################
# #
# # GENERATE Meta-fetaures at tld-reseller level 
# #
# ########################################################################################################

# # new geo level feature eng
# meta_df = expiry_df_test_preds_g %>%
#   add_count(tld_registrar_index, reseller_geo) %>%
#   group_by(tld_registrar_index) %>%
#   summarise(
#             geo_maj = reseller_geo[n == max(n)][1],
#             geo_cnt = n_distinct(reseller_geo),
#             n = n(),
#             ren_prp = sum(renewal_status=='Renewed')/sum(n),
#             tld_cnt = n_distinct(tld), tld_rat = tld_cnt/n,
              
#             daydom_min = min(day_domains), 
#             daydom_max = max(day_domains), 
#             daydom_mean = mean(day_domains, na.rm = TRUE), 
#             daydom_rng = daydom_max - daydom_min, 
#             daydom_std = sd(day_domains, na.rm = TRUE), 
#             daydom_skew = skewness(day_domains, na.rm = TRUE), 
#             daydom_kurt = kurtosis(day_domains, na.rm = TRUE),

#             sldlen_min = min(sld_length), 
#             sldlen_max = max(sld_length), 
#             sldlen_mean = mean(sld_length, na.rm = TRUE), 
#             sldlen_rng = sldlen_max - sldlen_min, 
#             sldlen_std = sd(sld_length, na.rm = TRUE), 
#             sldlen_skew = skewness(sld_length, na.rm = TRUE), 
#             sldlen_kurt = kurtosis(sld_length, na.rm = TRUE),
  
#             gibbs_min = min(gibb_score), 
#             gibbs_max = max(gibb_score), 
#             gibbs_mean = mean(gibb_score, na.rm = TRUE), 
#             gibbs_rng = gibbs_max - gibbs_min, 
#             gibbs_std = sd(gibb_score, na.rm = TRUE), 
#             gibbs_skew = skewness(gibb_score, na.rm = TRUE), 
#             gibbs_kurt = kurtosis(gibb_score, na.rm = TRUE),
  
#             pdcnt_min = min(pattern_domain_count), 
#             pdcnt_max = max(pattern_domain_count), 
#             pdcnt_mean = mean(pattern_domain_count, na.rm = TRUE), 
#             pdcnt_rng = pdcnt_max - pdcnt_min, 
#             pdcnt_std = sd(pattern_domain_count, na.rm = TRUE), 
#             pdcnt_skew = skewness(pattern_domain_count, na.rm = TRUE), 
#             pdcnt_kurt = kurtosis(pattern_domain_count, na.rm = TRUE),
  
#             regarpt_min = min(reg_arpt), 
#             regarpt_max = max(reg_arpt), 
#             regarpt_mean = mean(reg_arpt, na.rm = TRUE), 
#             regarpt_rng = regarpt_max - regarpt_min, 
#             regarpt_std = sd(reg_arpt, na.rm = TRUE), 
#             regarpt_skew = skewness(reg_arpt, na.rm = TRUE), 
#             regarpt_kurt = kurtosis(reg_arpt, na.rm = TRUE))

# # add a handful more vars 
# country_maj = expiry_df_test_preds_g %>%
#   add_count(tld_registrar_index, reseller_country) %>%
#   group_by(tld_registrar_index) %>%
#   mutate(reseller_country_maj = reseller_country[n == max(n)][1]) %>%
#   select(-n) %>% 
#   group_by(tld_registrar_index,reseller_country_maj) %>%
#     summarise(n = n()) %>% 
#     arrange(desc(n)) %>%
#     pull(reseller_country_maj)

# country_cnt = expiry_df_test_preds_g %>%
#   add_count(tld_registrar_index, reseller_country) %>%
#   group_by(tld_registrar_index) %>%
#   summarise(reseller_country_cnt = n_distinct(reseller_country)) %>%
#   pull(reseller_country_cnt)  

# region_maj = expiry_df_test_preds_g %>%
#   add_count(tld_registrar_index, region) %>%
#   group_by(tld_registrar_index) %>%
#   mutate(region_maj = region[n == max(n)][1]) %>%
#   select(-n) %>% 
#   group_by(tld_registrar_index,region_maj) %>%
#     summarise(n = n()) %>% 
#     arrange(desc(n)) %>%
#     pull(region_maj)

# region_cnt = expiry_df_test_preds_g %>%
#   add_count(tld_registrar_index, region) %>%
#   group_by(tld_registrar_index) %>%
#   summarise(reseller_region_cnt = n_distinct(reseller_country)) %>%
#   pull(reseller_region_cnt)  

# meta_df = meta_df %>% 
#     mutate(country_maj = country_maj,
#             region_maj = region_maj,
#             country_cnt = country_cnt,
#             region_cnt = region_cnt,
#             )

# ########################################################################################################
# #
# # JOIN metrics and meta for pred_df
# #
# ########################################################################################################

# metametrics_df <- merge(meta_df, metrics_df, on = 'tld_registrar_index', all = TRUE)


# ########################################################################################################
# #
# # ADD win flags for seg2_glm vs. seg2_gm_fb vs. agg_rf_ALL
# #
# ########################################################################################################

# (auc_vars = c('auc_seg2_glm','auc_agg_rf_ALL','auc_seg2_glm_fb'))
# (l10_vars = c('l10_seg2_glm','l10_agg_rf_ALL','l10_seg2_glm_fb'))

# metametrics_df <- metametrics_df %>%
#     mutate (auc_win_04=sapply(apply(.[,c(auc_vars)], 
#          1, function(x) names(x)[which.max(x)]) , function(s) if (length(s) == 0) NA else paste(s, collapse = " ")) ,
#             l10_win_04=sapply(apply(.[,c(l10_vars)], 
#          1, function(x) names(x)[which.max(x)]), function(s) if (length(s) == 0) NA else paste(s, collapse = " ")) 
#             ) 
             
# metametrics_df <- metametrics_df %>% mutate_if(is.list,as.numeric) 
             
# ########################################################################################################
# #
# # IMPUTE missing values
# #
# ########################################################################################################

# # but first, remove observations with missing wins -- we don't want to impute these dependent variables 
# metametrics_df <- metametrics_df %>% filter(!is.na(auc_win_04))
             
# library(missRanger)
# metametrics_imp_df <- missRanger(metametrics_df, num.trees = 100)
 
# ########################################################################################################
# #
# # TRAIN/TEST split
# # No need: Metalearning model trained on entire 20% test subset of expiry & tested on subsequent data pull
# #
# ########################################################################################################

# # set.seed(123) 
# # smp_siz = floor(0.8*nrow(metametrics_imp_df))
# # train_ind = sample(seq_len(nrow(metametrics_imp_df)),size = smp_siz) 
# # train = metametrics_imp_df[train_ind,] 
# # test = metametrics_imp_df[-train_ind,]  


# ########################################################################################################
# #
# # TRAIN models
# #
# ########################################################################################################

# # l10

# #Compute weights to balance the RF
# Y = metametrics_imp_df$l10_win_04
# w <- 1/table(Y)
# w <- w/sum(w)

# weights <- rep(0, length(Y))

# for (model in unique(Y)){
#     weights[Y==model] <- w[model]
# }


# model_l10 <- ranger(formula         = l10_win_04 ~ ., 
#                 data            = metametrics_imp_df %>% 
#                                     select('l10_win_04') %>% 
#                                     bind_cols(
#                                         metametrics_imp_df %>% 
#                                         select(-contains('auc'),-contains('l10'),-'tld_registrar_index', -'tld_rat')), 
#                 importance = 'impurity', 
#                 num.trees       = 500,
#                 probability = TRUE,
#                 replace = FALSE,
#                 sample.fraction = .8,
#                 seed            = 123,
#                 respect.unordered.factors=TRUE,
#                case.weights=weights)


                                        
# # auc
                                        
# #Compute weights to balance the RF
# Y = metametrics_imp_df$auc_win_04
# w <- 1/table(Y)
# w <- w/sum(w)

# weights <- rep(0, length(Y))

# for (model in unique(Y)){
#     weights[Y==model] <- w[model]
# }

                                               
# model_auc <- ranger(formula         = auc_win_04 ~ ., 
#                 data            = metametrics_imp_df %>% 
#                                     select('auc_win_04') %>% 
#                                     bind_cols(
#                                         metametrics_imp_df %>% 
#                                         select(-contains('auc'),-contains('l10'),-'tld_registrar_index', -'tld_rat')), 
#                 importance = 'impurity', 
#                 num.trees       = 500,
#                 probability = TRUE,
#                 replace = FALSE,
#                 sample.fraction = .8,
#                 seed            = 123,
#                 respect.unordered.factors=TRUE,
#                case.weights=weights)

                                    
# ########################################################################################################
# #
# # ASSIGN model to data based on predictions results
# #  need to modify this so it takes in new data pull
# #
# ########################################################################################################                  

# # MODIFICATION TO TEST ON NEW DATA PULL
# # 1. create new big query table (do this from within R?) using data coming after training data
# #    models trained on expiry 20190601-20200901 data pulled on 11/16 
# #    so new data pull should be 20200902-20201102 (assuming 3-4 week lag in assigning renewal_status flag)
# #    ... use saved query get_expiry_data_20201127 but change the 2 separate date windows accordingly
# #    ... then choose "SAVE RESULTS" & save to bigquery table named like 
# #        radix2020.expiry.expiry_20200902_20201102_20201127 (last date is date of pull)
# # 2. use notebook 03_* to pull the data into an RDS/csv (.94 million rows)
                                               
# expiry_new_df <- readRDS(file.path(outputDir,"expiry_20200902_20201102_20201127")) %>% 
#                                                filter(expiry_date < "2020-10-08") %>% 
#                                                filter(!is.na(gibb_score)) %>% 
#                                                mutate (reg_arpt = ifelse(reg_arpt <= 0, 0.0001,reg_arpt),
#                                                        log_reg_arpt = log(reg_arpt),
#                                                        tld_registrar_index = tolower(paste(tld, reseller,sep="")))
                                               
# expiry_new_df <- geo_suppl(expiry_new_df, geoLookupDF = geoLookupDF)

# # engineer metadata
# new_meta_df = expiry_new_df %>%
#   add_count(tld_registrar_index, reseller_geo) %>%
#   group_by(tld_registrar_index) %>%
#   summarise(
#             geo_maj = reseller_geo[n == max(n)][1],
#             geo_cnt = n_distinct(reseller_geo),
#             n = n(),
#             ren_prp = sum(renewal_status=='Renewed')/sum(n),
#             tld_cnt = n_distinct(tld), tld_rat = tld_cnt/n,
              
#             daydom_min = min(day_domains), 
#             daydom_max = max(day_domains), 
#             daydom_mean = mean(day_domains, na.rm = TRUE), 
#             daydom_rng = daydom_max - daydom_min, 
#             daydom_std = sd(day_domains, na.rm = TRUE), 
#             daydom_skew = skewness(day_domains, na.rm = TRUE), 
#             daydom_kurt = kurtosis(day_domains, na.rm = TRUE),

#             sldlen_min = min(sld_length), 
#             sldlen_max = max(sld_length), 
#             sldlen_mean = mean(sld_length, na.rm = TRUE), 
#             sldlen_rng = sldlen_max - sldlen_min, 
#             sldlen_std = sd(sld_length, na.rm = TRUE), 
#             sldlen_skew = skewness(sld_length, na.rm = TRUE), 
#             sldlen_kurt = kurtosis(sld_length, na.rm = TRUE),
  
#             gibbs_min = min(gibb_score), 
#             gibbs_max = max(gibb_score), 
#             gibbs_mean = mean(gibb_score, na.rm = TRUE), 
#             gibbs_rng = gibbs_max - gibbs_min, 
#             gibbs_std = sd(gibb_score, na.rm = TRUE), 
#             gibbs_skew = skewness(gibb_score, na.rm = TRUE), 
#             gibbs_kurt = kurtosis(gibb_score, na.rm = TRUE),
  
#             pdcnt_min = min(pattern_domain_count), 
#             pdcnt_max = max(pattern_domain_count), 
#             pdcnt_mean = mean(pattern_domain_count, na.rm = TRUE), 
#             pdcnt_rng = pdcnt_max - pdcnt_min, 
#             pdcnt_std = sd(pattern_domain_count, na.rm = TRUE), 
#             pdcnt_skew = skewness(pattern_domain_count, na.rm = TRUE), 
#             pdcnt_kurt = kurtosis(pattern_domain_count, na.rm = TRUE),
  
#             regarpt_min = min(reg_arpt), 
#             regarpt_max = max(reg_arpt), 
#             regarpt_mean = mean(reg_arpt, na.rm = TRUE), 
#             regarpt_rng = regarpt_max - regarpt_min, 
#             regarpt_std = sd(reg_arpt, na.rm = TRUE), 
#             regarpt_skew = skewness(reg_arpt, na.rm = TRUE), 
#             regarpt_kurt = kurtosis(reg_arpt, na.rm = TRUE))

# # add a handful more vars 
# country_maj = expiry_new_df %>%
#   add_count(tld_registrar_index, reseller_country) %>%
#   group_by(tld_registrar_index) %>%
#   mutate(reseller_country_maj = reseller_country[n == max(n)][1]) %>%
#   select(-n) %>% 
#   group_by(tld_registrar_index,reseller_country_maj) %>%
#     summarise(n = n()) %>% 
#     arrange(desc(n)) %>%
#     pull(reseller_country_maj)

# country_cnt = expiry_new_df %>%
#   add_count(tld_registrar_index, reseller_country) %>%
#   group_by(tld_registrar_index) %>%
#   summarise(reseller_country_cnt = n_distinct(reseller_country)) %>%
#   pull(reseller_country_cnt)  

# region_maj = expiry_new_df %>%
#   add_count(tld_registrar_index, region) %>%
#   group_by(tld_registrar_index) %>%
#   mutate(region_maj = region[n == max(n)][1]) %>%
#   select(-n) %>% 
#   group_by(tld_registrar_index,region_maj) %>%
#     summarise(n = n()) %>% 
#     arrange(desc(n)) %>%
#     pull(region_maj)

# region_cnt = expiry_new_df %>%
#   add_count(tld_registrar_index, region) %>%
#   group_by(tld_registrar_index) %>%
#   summarise(reseller_region_cnt = n_distinct(reseller_country)) %>%
#   pull(reseller_region_cnt)  

# new_meta_df = new_meta_df %>% 
#     mutate(country_maj = country_maj,
#             region_maj = region_maj,
#             country_cnt = country_cnt,
#             region_cnt = region_cnt,
#             )                                    
                                    
# # impute missing values
# new_meta_imp_df <- missRanger(new_meta_df, num.trees = 100)

                              
# # predict model based on meta features

# # l10

# new_pred_l10 <- as.data.frame(predict(model_l10, 
#                 data = new_meta_imp_df,
#                 type="response")$predictions) %>%
#     mutate (l10_win_04_pred_model=sapply(apply(., 
#                           1, function(x) names(x)[which.max(x)]) , 
#                                     function(s) if (length(s) == 0) NA else paste(s, collapse = " ")) 
#             ) 
                                        
# # auc


# new_pred_auc <- as.data.frame(predict(model_auc, 
#                 data = new_meta_imp_df,
#                 type="response")$predictions) %>%
                                               
#     mutate (auc_win_04_pred_model=sapply(apply(., 
#                           1, function(x) names(x)[which.max(x)]) , 
#                                     function(s) if (length(s) == 0) NA else paste(s, collapse = " ")) 
#             ) 

                                                                 
# new_metametrics_imp_pred_df <- cbind(new_meta_df,new_pred_l10$l10_win_04_pred_model,new_pred_auc$auc_win_04_pred_model)
# new_metametrics_imp_pred_df <- new_metametrics_imp_pred_df %>% rename(l10_win_04_pred_model = length(new_metametrics_imp_pred_df)-1,
#                                                                      auc_win_04_pred_model = length(new_metametrics_imp_pred_df))

# new_metametrics_imp_pred_df <- new_metametrics_imp_pred_df %>% 
#    mutate_at(vars(l10_win_04_pred_model), list(~(gsub("l10_", "", .)))) %>% 
#    mutate_at(vars(auc_win_04_pred_model), list(~(gsub("auc_", "", .))))
                              
                              
# save(expiry_new_df, file=file.path(outputDir, 'meta_preds','expiry_new_df.RData'))
# save(new_metametrics_imp_pred_df, file=file.path(outputDir, 'meta_preds','new_metametrics_imp_pred_df.RData'))

# expiry_df_test_preds_assign <- merge(expiry_new_df, 
#                                      metametrics_imp_pred_df %>% 
#                                         select(tld_registrar_index, length(test_pred)-1, length(test_pred)), 
#                                      by="tld_registrar_index", all.y=TRUE) 
# expiry_df_test_preds_assign <- expiry_df_test_preds_assign %>% 
#                                 rename(l10_win_04_pred_model = length(expiry_df_test_preds_assign)-1,
#                                       auc_win_04_pred_model = length(expiry_df_test_preds_assign))
                      
                                               
                                               
########################################################################################################
#
# ASSIGN probability to data based on model assignment, predicting for given model using model objects
#
########################################################################################################

# load(file.path(outputDir, 'meta_preds','expiry_new_df.RData'))
# load(file.path(outputDir, 'meta_preds','new_metametrics_imp_pred_df.RData'))



# pred_select(expiry_new_df,
#                          new_metametrics_imp_pred_df,
#                          dataDir=dataDir,
#                          modelDir=modelDir,
#                          outputDir=outputDir
#                       )

########################################################################################################
#
# Aside: CALC probability for all models for all observations, using model objects
#
########################################################################################################

load(file.path(outputDir, 'meta_preds','expiry_new_df.RData')) # new data
expiry_test_list_new = split(expiry_new_df, expiry_new_df$tld_registrar_index) # new data
expiry_test_list = split(expiry_df_train, expiry_df_train$tld_registrar_index) # old data (loaded above)

# define tld-re's for testing
tld_reseller_list = expiry_new_df %>%  distinct(tld_registrar_index) %>% pull(tld_registrar_index) # tested on new data
tld_registrar_excl_list = tld_registrar_excl_df(train_df = expiry_df_train) # trained on old data, exclusion base don old data

# predict based on saved models
preds_df <- pred_all(tld_reseller_list, tld_registrar_excl_list,
                     test_list = expiry_test_list_new,
                     modelDir=modelDir,
                     fullDir=outputDir)

dir.create(file.path(outputDir,'preds'))
write.csv(preds_df, file=file.path(outputDir,'preds','preds.csv'),row.names = FALSE)