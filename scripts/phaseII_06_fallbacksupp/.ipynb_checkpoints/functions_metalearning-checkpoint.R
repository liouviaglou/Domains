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
# II. GEN PREDICTIONS w/ MODEL OBJECTS 
#
#########################################################################################   

# I. A) AGGREGATE MODELS ################################################################   

pred_agg_glm <- function(model, test_list, tld_reseller_str) {
    
    print(tld_reseller_str)
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
    
    print(tld_reseller_str)
    # agg rf (aggregarted rf (including tld and reseller as predictors))
    
    test_list_tld_reseller = test_list[tld_reseller_str]
    test_df_tld_reseller =  rbindlist(test_list_tld_reseller,use.names=TRUE)
    
    # if test data contains no observations, skip!
     if (dim(test_df_tld_reseller)[1]==0){
        pred_df_agg_rf = NA
    }  else {
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
    
    print(tld_reseller_str)
    # seg glm (reseller-segmented glm (including tld as predictor))
    
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
    
    print(tld_reseller_str)
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
    
    print(tld_reseller_str)
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
    
    print(tld_reseller_str)
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
    return(pred_df_seg2_rf)

    
}


#########################################################################################  
#
# III. WEED OUT & SUPPLEMENT w/ FALLBACK 
#
#########################################################################################   

#########################################################################################  
#
# IV. AGG Functions
#
#########################################################################################   

train_all <- function (tld_reseller_list,
                       train_list = expiry_train_prepped_2_1,
                       test_list = expiry_test_prepped_2_1,
                       model_agg_glm = NULL,
                       model_agg_rf = NULL,
                      fullDir='../../data/output/models_20201015'){
    
    reseller_list = rbindlist(test_list, ,use.names=TRUE) %>% 
      filter(tld_registrar_index %in% tld_reseller_list) %>% 
      distinct(reseller)  %>%  pull(reseller)

    dir.create(fullDir, showWarnings = FALSE)
    
    if(is.null(model_agg_glm)) {
        model_agg_glm = train_agg_glm(train_list)
        save(model_agg_glm, 
             file=file.path(fullDir, 'model_agg_glm.Rdata')
            )
    }    
    
    if(is.null(model_agg_rf)) {
        model_agg_rf = train_agg_rf(train_list)   
        save(model_agg_rf, 
             file=file.path(fullDir, 'model_agg_rf.Rdata')
            )
        
    } 
    
    for (reseller_str in reseller_list) {
        
        model_name <- paste0('model_seg_glm_',str_replace_all(reseller_str, "[^[:alnum:]]", ""))
        print(model_name)
        
        assign(model_name,train_seg_glm(train_list, reseller_str) )
        save(list=model_name, 
             file=file.path(fullDir, paste0(model_name,'.Rdata'))
            )
        
        model_name <- paste0('model_seg_rf_',str_replace_all(reseller_str, "[^[:alnum:]]", ""))
        print(model_name)
        
        assign(model_name,train_seg_rf(train_list, reseller_str)  )
        save(list=model_name, 
             file=file.path(fullDir, paste0(model_name,'.Rdata'))
            )
        
    } 
    
    
    for (tld_reseller_str in tld_reseller_list) {

        model_name <- paste0('model_seg2_glm_',str_replace_all(tld_reseller_str, "[^[:alnum:]]", ""))
        print(model_name)

        assign(model_name,train_seg2_glm(train_list, tld_reseller_str) )
        save(list=model_name, 
             file=file.path(fullDir, paste0(model_name,'.Rdata'))
            )

        model_name <- paste0('model_seg2_rf_',str_replace_all(tld_reseller_str, "[^[:alnum:]]", ""))
        print(model_name)

        assign(model_name,train_seg2_rf(train_list, tld_reseller_str)  )
        save(list=model_name, 
             file=file.path(fullDir, paste0(model_name,'.Rdata'))
            )
    }
    


}
    

pred_all <- function (tld_reseller_list,
                      test_list = expiry_test_prepped_2_1,
                      fullDir='../../data/output/models_20201015' # dir of models
                      ){   
    
    print(cat("\n\nPredicting model_agg_glm\n"))
    load(file.path(fullDir, 'model_agg_glm.Rdata'))    
    preds_agg_glm = lapply(tld_reseller_list, 
           function(tld_reseller_str) pred_agg_glm(model_agg_glm, test_list, tld_reseller_str)
           )
    rm(model_agg_glm)
    gc()
    
    print(cat("\n\nPredicting model_agg_rf\n"))
    load(file.path(fullDir, 'model_agg_rf.Rdata'))
    preds_agg_rf = lapply(tld_reseller_list, 
           function(tld_reseller_str) pred_agg_rf(model_agg_rf, test_list, tld_reseller_str)
           )
    rm(model_agg_rf)
    gc()

    print(cat("\n\nPredicting model_seg_glm\n")  )  
    lapply(Sys.glob(file.path(fullDir,'model_seg_glm_*')),load,.GlobalEnv)
    preds_seg_glm = lapply(tld_reseller_list, 
           function(tld_reseller_str) pred_seg_glm(
               test_list, 
               tld_reseller_str)
           )
    rm(list=ls(pattern='^model_seg_glm_'))
    
    print(cat("\n\nPredicting model_seg_rf\n")  )  
    lapply(Sys.glob(file.path(fullDir,'model_seg_rf_*')),load,.GlobalEnv)
    preds_seg_rf = lapply(tld_reseller_list, 
           function(tld_reseller_str) pred_seg_rf(
               test_list, 
               tld_reseller_str)
           )
    rm(list=ls(pattern='^model_seg_rf_'))

    print(cat("\n\nPredicting model_seg2_glm\n"))
    lapply(Sys.glob(file.path(fullDir,'model_seg2_glm_*')),load,.GlobalEnv)
    preds_seg2_glm = lapply(tld_reseller_list, 
           function(tld_reseller_str) pred_seg2_glm(
               test_list, 
               tld_reseller_str)
           )
    rm(list=ls(pattern='^model_seg2_glm_'))

    print(cat("\n\nPredicting model_seg2_rf\n")    )    
    lapply(Sys.glob(file.path(fullDir,'model_seg2_rf_*')),load,.GlobalEnv)
    preds_seg2_rf = lapply(tld_reseller_list, 
           function(tld_reseller_str) pred_seg2_rf(
               test_list, 
               tld_reseller_str)
           )
    rm(list=ls(pattern='^model_rf2_rf_'))

    
    # combine all preds into one list
    preds_list = list()
    i=1
    for (tld_reseller_str in tld_reseller_list) {
        if (is.na(preds_seg_glm[[i]])) {
            preds_list[[tld_reseller_str]]= NA
        } else{
            preds_list[[tld_reseller_str]] = cbind(
                  test_list[[tld_reseller_str]],
                  'pred_agg_glm'=preds_agg_glm[[i]]$predicted,
                  'pred_agg_rf'=preds_agg_rf[[i]]$predicted,
                  'pred_seg_glm'=preds_seg_glm[[i]]$predicted,
                  'pred_seg_rf' = preds_seg_rf[[i]]$predicted,
                  'pred_seg2_glm'=preds_seg2_glm[[i]]$predicted,
                  'pred_seg2_rf'=preds_seg2_rf[[i]]$predicted)
        }

        i=i+1
    }

    na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }
    preds_list <- na.omit.list(preds_list)
    preds_df <- rbindlist(preds_list,use.names=TRUE)
                                                   
    return(list(preds_seg_glm, preds_seg_rf, preds_seg2_glm, preds_seg2_rf, preds_list, preds_df))

}
    


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
    



