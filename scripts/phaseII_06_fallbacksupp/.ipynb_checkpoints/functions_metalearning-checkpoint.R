# Rscript tld_reseller_compare_lis_fb.R >> /home/jupyter/local/Domains_202003/data/output/tld_reseller_compare_list_3.log 2>&1

suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(ranger))
suppressMessages(library(pbapply))

# load & prep input data
source('../orig/functions_models.R')
# source('../phaseII_03_forest/functions_eval.R')
# source('../phaseII_03_forest/load_prep_data_expiry.R')

#########################################################################################  
#
# I. TRAIN MODELS, SAVE MODEL OBJECTS
#
#########################################################################################   

# I. A) AGGREGATE MODELS ################################################################   

train_agg_glm <- function(train_list) {
    # agg glm (aggregarted glm (including tld and reseller as predictors))

    train_df =  rbindlist(train_list)
    
    model = build_model_first_renewal_agg(train_df)
    
    return(model)
}

train_agg_rf <- function(train_list) {
    # agg rf (aggregarted rf (including tld and reseller as predictors))
    
    train_df =  rbindlist(train_list)
    
    if(dim(train_df)[1]==1){
    # if train data only has one observation, 
    # ... sample_fraction must be 1 (cant sample fraction of 1 observation)
        sample_fraction=1
    }else{
        sample_fraction=.8
    }
    
    model <- ranger(
        formula         = renewal_status ~
                            pattern_domain_count+log_reg_arpt+sld_length+gibb_score+
                            sld_type+day_domains+reg_period+tld+reseller, 
        data            = train_df, 
        importance      = 'impurity', 
        num.trees       = 1000,
        probability     = TRUE,
        replace         = FALSE,
        sample.fraction = sample_fraction,
        seed            = 123,
        respect.unordered.factors=TRUE)  
    
    return(model)
}
   

# I. B) RESELLER MODELS #################################################################

train_seg_glm <- function(train_list, reseller_str) {
    # seg glm (reseller-segmented glm (including tld as predictor))

    # subset data for seg models
    tld_registrars = names(train_list)[endsWith(names(train_list),reseller_str)]
    train_list_reseller = train_list[tld_registrars]
    train_df_reseller =  rbindlist(train_list_reseller,use.names=TRUE)

    if((nlevels(train_df_reseller$tld) < 2)){
        # if there are not enough tlds to segment-on, 
        # ... do not include tld as predictor
        # ... i.e. build standard Radix model
        model = build_model_first_renewal(train_df_reseller)
    }else{
        model = build_model_first_renewal_reg(train_df_reseller)
    }

    return(model)
}

train_seg_rf <- function(train_list, reseller_str) {
    # seg rf (reseller-segmented rf)
    
    # subset data for seg models
    tld_registrars = names(train_list)[endsWith(names(train_list),reseller_str)]
    train_list_reseller = train_list[tld_registrars]
    train_df_reseller =  rbindlist(train_list_reseller,use.names=TRUE)
    
    if(dim(train_df_reseller)[1]==1){
        # if train data only has one observation, 
        # ... sample_fraction must be 1 (cant sample fraction of 1 observation)
        sample_fraction=1
    }else{
        sample_fraction=.8
    }
    
    model <- ranger(
        formula         = renewal_status ~
                            pattern_domain_count+log_reg_arpt+sld_length+gibb_score+
                            sld_type+day_domains+reg_period+tld, 
        data            = train_df_reseller, 
        importance      = 'impurity', 
        num.trees       = 1000,
        probability     = TRUE,
        replace         = FALSE,
        sample.fraction = sample_fraction,
        seed            = 123,
        respect.unordered.factors=TRUE)  
    
    return(model)
}


# I. C) TLD-RESELLER MODELS #############################################################

train_seg2_glm <- function(train_list, tld_reseller_str) {
    # seg2 glm (tld-reseller-segmented glm)
    
    # subset data for seg2 models
    train_list_tld_reseller = train_list[tld_reseller_str]
    train_df_tld_reseller =  rbindlist(train_list_tld_reseller,use.names=TRUE)   
    
    model = mass_build_model_first_renewal(train_list_tld_reseller)
    return(model)
    
}

train_seg2_rf <- function(train_list, tld_reseller_str) {
    # seg2 rf (tld-reseller-segmented rf)
    
    # subset data for seg2 models
    train_list_tld_reseller = train_list[tld_reseller_str]
    train_df_tld_reseller =  rbindlist(train_list_tld_reseller,use.names=TRUE)   
    
    if(dim(train_df_tld_reseller)[1]==1){
        # if train data only has one observation, 
        # ... sample_fraction must be 1 (cant sample fraction of 1 observation)
        sample_fraction=1
    }else{
        sample_fraction=.8
    }    
    
    model <- ranger(
        formula         = renewal_status ~
                            pattern_domain_count+log_reg_arpt+sld_length+gibb_score+
                            sld_type+day_domains+reg_period, 
                        data            = train_df_tld_reseller, 
                        importance = 'impurity', 
                        num.trees       = 1000,
                        probability = TRUE,
                        replace = FALSE,
                        sample.fraction = sample_fraction,
                        seed            = 123,
                        respect.unordered.factors=TRUE)
    return(model)
    
}

#########################################################################################  
#
# II. USE MODEL OBJECTS to GEN PREDICTIONS
#
#########################################################################################   

# I. A) AGGREGATE MODELS ################################################################   

pred_agg_glm <- function(model, test_list, tld_reseller_str) {
    
    # agg glm (aggregarted glm (including tld and reseller as predictors))
    
    test_list_tld_reseller = test_list[tld_reseller_str]
    test_df_tld_reseller =  rbindlist(test_list_tld_reseller,use.names=TRUE)
  
    # if test data contains no observations, skip!
    if (dim(test_df_tld_reseller)[1]==0){
        pred_df_agg_glm = NA
    } else {
        pred = predict_first_renewal_agg(test_df_tld_reseller, model)
    
        pred_df_agg_glm = data.frame("actual" = pred$renewal_status,
                                      "predicted" = pred$first_renewal_prediction)
    } 
 
    return(pred_df_agg_glm)
}

pred_agg_rf <- function(model, test_list, tld_reseller_str) {
    # agg rf (aggregarted rf (including tld and reseller as predictors))
    
    test_list_tld_reseller = test_list[tld_reseller_str]
    test_df_tld_reseller =  rbindlist(test_list_tld_reseller,use.names=TRUE)
    
    # if test data contains no observations, skip!
     if (dim(test_df_tld_reseller)[1]==0){
        pred_df_agg_rf = NA
    }  else {
        model = model_agg_rf 
        pred <- predict(model, 
                        data = test_df_tld_reseller,
                        type="response")$predictions

        # if all Renewed col doesn't exist in predictions, create it with value 0
        if(is.null(as.data.frame(pred)$Renewed)){
            pred <- as.data.frame(pred)
            pred$Renewed <- 0
        }

        pred_df_agg_rf = data.frame("actual" = test_df_tld_reseller$renewal_status,
                          "predicted" = as.data.frame(pred)$Renewed)
    }
    
    return(pred_df_agg_rf)
}
   

# I. B) RESELLER MODELS #################################################################

pred_seg_glm <- function(test_list, tld_reseller_str) {
    # seg glm (reseller-segmented glm (including tld as predictor))
    
    print(tld_reseller_str)
    test_list_tld_reseller = test_list[tld_reseller_str]
    test_df_tld_reseller =  rbindlist(test_list_tld_reseller,use.names=TRUE)    

    # if test data contains no observations, skip!
    if (dim(test_df_tld_reseller)[1]==0){
        pred_df_seg_glm = NA
    } else {
        
        reseller_str = test_df_tld_reseller %>% filter(tld_registrar_index==tld_reseller_str) %>% 
        distinct(reseller) %>% pull(reseller)

        model_name <- paste0('model_seg_glm_',str_replace_all(reseller_str, "[^[:alnum:]]", ""))
        model <- get(model_name)


        pred = predict_first_renewal_reg(test_df_tld_reseller, model)

        pred_df_seg_glm = data.frame("actual" = pred$renewal_status,
                                      "predicted" = pred$first_renewal_prediction)
    } 
    return(pred_df_seg_glm)
}

pred_seg_rf <- function(test_list, tld_reseller_str) {
    # seg rf (reseller-segmented rf)
    
    test_list_tld_reseller = test_list[tld_reseller_str]
    test_df_tld_reseller =  rbindlist(test_list_tld_reseller,use.names=TRUE)
    
     # if test data contains no observations, skip!
    if (dim(test_df_tld_reseller)[1]==0){
        pred_df_seg_rf = NA
    } else {
            
        reseller_str = test_df_tld_reseller %>% filter(tld_registrar_index==tld_reseller_str) %>% 
        distinct(reseller) %>% pull(reseller)

        model_name <- paste0('model_seg_rf_',str_replace_all(reseller_str, "[^[:alnum:]]", ""))
        model <- get(model_name)

        pred <- predict(model, 
                    data = test_df_tld_reseller,
                    type="response")$predictions

        # if all Renewed col doesn't exist in predictions, create it with value 0
        if(is.null(as.data.frame(pred)$Renewed)){
            pred <- as.data.frame(pred)
            pred$Renewed <- 0
        }

        pred_df_seg_rf = data.frame("actual" = test_df_tld_reseller$renewal_status,
                          "predicted" = as.data.frame(pred)$Renewed)

         } 
    return(pred_df_seg_rf)
}


# I. C) TLD-RESELLER MODELS #############################################################

pred_seg2_glm <- function(test_list, tld_reseller_str) {
    # seg2 glm (tld-reseller-segmented glm)

    test_list_tld_reseller = test_list[tld_reseller_str]
    test_df_tld_reseller =  rbindlist(test_list_tld_reseller,use.names=TRUE)
    
    # if test data contains no observations, skip!
    if (dim(test_df_tld_reseller)[1]==0){
        pred_df_seg2_glm = NA
    } else {
        
        model_name <- paste0('model_seg2_glm_',str_replace_all(tld_reseller_str, "[^[:alnum:]]", ""))
        model <- get(model_name)

        pred = mass_predict_first_renewal(test_list_tld_reseller, model)
    
        pred_df_seg2_glm = data.frame("actual" = pred$renewal_status,
                                      "predicted" = pred$first_renewal_prediction)
    }
    
    return(pred_df_seg2_glm)
    
}

pred_seg2_rf <- function(test_list, tld_reseller_str) {
    # seg2 rf (tld-reseller-segmented rf)
    
    test_list_tld_reseller = test_list[tld_reseller_str]
    test_df_tld_reseller =  rbindlist(test_list_tld_reseller,use.names=TRUE)
    
    if (dim(test_df_tld_reseller)[1]==0){
        pred_df_seg2_rf = NA
    } else {
      
        model_name <- paste0('model_seg2_rf_',str_replace_all(tld_reseller_str, "[^[:alnum:]]", ""))
        model <- get(model_name)

        pred <- predict(model, 
                        data = test_df_tld_reseller,
                        type="response")$predictions

        # if all Renewed col doesn't exist in predictions, create it with value 0
        if(is.null(as.data.frame(pred)$Renewed)){
            pred <- as.data.frame(pred)
            pred$Renewed <- 0
        }

        pred_df_seg2_rf = data.frame("actual" = test_df_tld_reseller$renewal_status,
                          "predicted" = as.data.frame(pred)$Renewed)
    }
    
    # need to combine all predictions into one dataframe, rbind with test data
    return()

    
}

# # III. SUPPLEMENT w/ FALLBACK

# train_all <- function (tld_reseller_str,
#                       reseller_str,
#                       model_agg_glm = model_agg_glm,
#                       model_agg_rf = model_agg_rf,
#                       train_list = expiry_train_prepped_2_1,
#                       test_list = expiry_test_prepped_2_1){
# }
    


# pred_all <- function (tld_reseller_str,
#                       reseller_str,
#                       model_agg_glm = model_agg_glm,
#                       model_agg_rf = model_agg_rf,
#                       train_list = expiry_train_prepped_2_1,
#                       test_list = expiry_test_prepped_2_1){
# }
    





# tld_reseller_compare <- function (  tld_reseller_str,
#                                     reseller_lookup,
#                                     model_agg_glm = model_agg_glm,
#                                     model_agg_rf = model_agg_rf,
#                                     train_list = expiry_train_prepped_2_1,
#                                     test_list = expiry_test_prepped_2_1){
    
#     cat("\n\n")
#     print(tld_reseller_str[[1]])
    
#     reseller_str = reseller_lookup %>% filter(tld_registrar_index==tld_reseller_str) %>% pull(reseller)
#     tld_registrars = names(expiry_train_prepped_2_1)[endsWith(names(expiry_train_prepped_2_1),reseller_str)]
    
#     # subset data for seg2 models
#     train_list_tld_reseller = train_list[tld_reseller_str]
#     test_list_tld_reseller = test_list[tld_reseller_str]
#     train_df_tld_reseller =  rbindlist(train_list_tld_reseller,use.names=TRUE)
#     test_df_tld_reseller =  rbindlist(test_list_tld_reseller,use.names=TRUE)
    
#     # subset data for seg models
#     train_list_reseller = train_list[tld_registrars]
#     test_list_reseller = test_list[tld_registrars]
#     train_df_reseller =  rbindlist(train_list_reseller,use.names=TRUE)
#     test_df_reseller =  rbindlist(test_list_reseller,use.names=TRUE)
    

#     #####################################################################################
#     # seg2 glm (tld-reseller-segmented glm)
#     #####################################################################################
    
#     cat("\n seg2 glm")
    
#     # if test data contains no observations, skip!
#     if (dim(test_df_tld_reseller)[1]==0){
#         pred_df_seg2_glm = NA
#     } else {
#         model = mass_build_model_first_renewal(train_list_tld_reseller)
#         pred = mass_predict_first_renewal(test_list_tld_reseller, model)
    
#         pred_df_seg2_glm = data.frame("actual" = pred$renewal_status,
#                                       "predicted" = pred$first_renewal_prediction)
#     }
    
#     #####################################################################################    
#     # seg glm (reseller-segmented glm (including tld as predictor))
#     #####################################################################################

#     cat("\n seg glm")
    
#     # if test data contains no observations, skip!
#     if (dim(test_df_tld_reseller)[1]==0){
#         pred_df_seg_glm = NA
#     } else {

#         if((nlevels(train_df_reseller$tld) < 2)){
#             model = build_model_first_renewal(train_df_reseller)
#         }else{
#             model = build_model_first_renewal_reg(train_df_reseller)
#         }



#         pred = predict_first_renewal_reg(test_df_tld_reseller, model)

#         pred_df_seg_glm = data.frame("actual" = pred$renewal_status,
#                                       "predicted" = pred$first_renewal_prediction)
#     }

    
#     #####################################################################################    
#     # agg glm (aggregarted glm (including tld and reseller as predictors))
#     #####################################################################################
    
#     cat("\n agg glm")
    
#     # if test data contains no observations, skip!
#     if (dim(test_df_tld_reseller)[1]==0){
#         pred_df_agg_glm = NA
#     } else {
#         model = model_agg_glm
#         pred = predict_first_renewal_agg(test_df_tld_reseller, model)
    
#         pred_df_agg_glm = data.frame("actual" = pred$renewal_status,
#                                       "predicted" = pred$first_renewal_prediction)
#     }
    
    
#     #####################################################################################
#     # seg2 rf (tld-reseller-segmented rf)
#     #####################################################################################

#     cat("\n seg2 rf")
    
#     # if test data contains no observations, skip!
#     if (dim(test_df_tld_reseller)[1]==0){
#         pred_df_seg2_rf = NA
#     } else {
#     #     if train data only has one observation, sample_fraction must be 1 (cant sample fraction of 1 observation)
#         if(dim(train_df_tld_reseller)[1]==1){
#             sample_fraction=1
#         }else{
#             sample_fraction=.8
#         }

#         model <- ranger(formula         = renewal_status ~ pattern_domain_count+log_reg_arpt+sld_length+gibb_score+sld_type+day_domains+reg_period, 
#                         data            = train_df_tld_reseller, 
#                         importance = 'impurity', 
#                         num.trees       = 1000,
#                         probability = TRUE,
#                         replace = FALSE,
#                         sample.fraction = sample_fraction,
#                         seed            = 123,
#                         respect.unordered.factors=TRUE)

#         pred <- predict(model, 
#                         data = test_df_tld_reseller,
#                         type="response")$predictions

#         # if all Renewed col doesn't exist in predictions, create it with value 0
#         if(is.null(as.data.frame(pred)$Renewed)){
#             pred <- as.data.frame(pred)
#             pred$Renewed <- 0
#         }

#         pred_df_seg2_rf = data.frame("actual" = test_df_tld_reseller$renewal_status,
#                           "predicted" = as.data.frame(pred)$Renewed)
#     }
    
    
#     #####################################################################################
#     # seg rf (reseller-segmented rf)
#     #####################################################################################

#     cat("\n seg rf")
    
#     # if test data contains no observations, skip!
#     if (dim(test_df_tld_reseller)[1]==0){
#         pred_df_seg_rf = NA
#     } else {
#     #     if train data only has one observation, sample_fraction must be 1 (cant sample fraction of 1 observation)
#         if(dim(train_df_reseller)[1]==1){
#             sample_fraction=1
#         }else{
#             sample_fraction=.8
#         }

#         model <- ranger(formula         = renewal_status ~ pattern_domain_count+log_reg_arpt+sld_length+gibb_score+sld_type+day_domains+reg_period+tld, 
#                         data            = train_df_reseller, 
#                         importance = 'impurity', 
#                         num.trees       = 1000,
#                         probability = TRUE,
#                         replace = FALSE,
#                         sample.fraction = sample_fraction,
#                         seed            = 123,
#                         respect.unordered.factors=TRUE)

#         pred <- predict(model, 
#                         data = test_df_tld_reseller,
#                         type="response")$predictions

#         # if all Renewed col doesn't exist in predictions, create it with value 0
#         if(is.null(as.data.frame(pred)$Renewed)){
#             pred <- as.data.frame(pred)
#             pred$Renewed <- 0
#         }

#         pred_df_seg_rf = data.frame("actual" = test_df_tld_reseller$renewal_status,
#                           "predicted" = as.data.frame(pred)$Renewed)
#     }
    
#     #####################################################################################    
#     # agg rf (aggregarted rf (including tld and reseller as predictors))
#     #####################################################################################

#     cat("\n agg rf")
    
#     # if test data contains no observations, skip!
#      if (dim(test_df_tld_reseller)[1]==0){
#         pred_df_agg_rf = NA
#     }  else {
#         model = model_agg_rf 
#         pred <- predict(model, 
#                         data = test_df_tld_reseller,
#                         type="response")$predictions

#         # if all Renewed col doesn't exist in predictions, create it with value 0
#         if(is.null(as.data.frame(pred)$Renewed)){
#             pred <- as.data.frame(pred)
#             pred$Renewed <- 0
#         }

#         pred_df_agg_rf = data.frame("actual" = test_df_tld_reseller$renewal_status,
#                           "predicted" = as.data.frame(pred)$Renewed)
#     }

     
    
    
#     return(list(pred_df_seg2_glm = pred_df_seg2_glm, 
#                 pred_df_seg_glm = pred_df_seg_glm, 
#                 pred_df_agg_glm = pred_df_agg_glm, 
#                 pred_df_seg2_rf = pred_df_seg2_rf, 
#                 pred_df_seg_rf = pred_df_seg_rf, 
#                 pred_df_agg_rf = pred_df_agg_rf
#     ))
    
    

# }



# tld_reseller_compare_list <- function ( tld_reseller_list=tld_reseller_list,
#                                         reseller_lookup=reseller_lookup,
#                                         model_agg_glm= agg_glm_basic_model,
#                                         model_agg_rf= ranger_03_expiry2_f,
#                                         train_list = expiry_train_prepped_2_1,
#                                         test_list = expiry_test_prepped_2_1){
    
    
#                         x <- lapply(tld_reseller_list, 
#                                     function(tld_reseller_str) tld_reseller_compare(tld_reseller_str,
#                                                                                 reseller_lookup,
#                                                                                 model_agg_glm = agg_glm_basic_model,
#                                                                                 model_agg_rf = ranger_03_expiry2_f,
#                                                                                 train_list,
#                                                                                 test_list))

#                         return(x)
#                                     }

                                 
# # load load saved agg models
# load("../../data/agg_glm_basic_model")
# load("../../data/ranger_03_expiry2_f")

# # placeholder if agg models aren't needed
# # agg_glm_basic_model=1
# # ranger_03_expiry2_f=1
                                    
# # tld_reseller_list = expiry_train_df_1 %>% group_by(tld_registrar_index, reseller) %>% tally() %>% arrange(desc(n)) %>% head(817) %>% pull(tld_registrar_index)
# # reseller_lookup = expiry_train_df_1 %>% group_by(tld_registrar_index, reseller) %>% tally() %>% arrange(desc(n)) 
# # return_list = tld_reseller_compare_list(tld_reseller_list=tld_reseller_list,
# #                                         reseller_lookup=reseller_lookup,
# #                                         model_agg_glm = agg_glm_basic_model,
# #                                         model_agg_rf = ranger_03_expiry2_f,
# #                                         train_list = expiry_train_prepped_2_1,
# #                                         test_list = expiry_test_prepped_2_1)
                             
# # save(return_list,  file="../../data/output/tld_reseller_compare_list_0001_0817_seg_rf")
                                    
                                   
# tld_reseller_list = expiry_train_df_1 %>% group_by(tld_registrar_index, reseller) %>% tally() %>% arrange(desc(n)) %>% tail(906) %>% pull(tld_registrar_index)
# reseller_lookup = expiry_train_df_1 %>% group_by(tld_registrar_index, reseller) %>% tally() %>% arrange(desc(n))
                                    
# return_list = tld_reseller_compare_list(tld_reseller_list=tld_reseller_list,
#                                         reseller_lookup=reseller_lookup,
#                                         model_agg_glm = agg_glm_basic_model,
#                                         model_agg_rf = ranger_03_expiry2_f,
#                                         train_list = expiry_train_prepped_2_1,
#                                         test_list = expiry_test_prepped_2_1)
                                    
# saveRDS(return_list, file="../../data/output/tld_reseller_compare_list_0818_1723.RDS") 
                             
# save(return_list,  file="../../data/output/tld_reseller_compare_list_0818_1723.RData")