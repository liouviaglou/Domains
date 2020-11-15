suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(ranger))
suppressMessages(library(pbapply))
suppressMessages(library(zoo))

# load & prep input data
source('../orig/functions_models.R')
source('functions_fallback.R')
# source('../phaseII_03_forest/functions_eval.R')
# source('../phaseII_03_forest/load_prep_data_expiry.R')

#########################################################################################  
#
# I. TRAIN MODELS, SAVE MODEL OBJECTS
#
#########################################################################################   

# I. A) AGGREGATE MODELS ################################################################   

train_agg_glm <- function(train_list,tld_reseller_list) {
    # agg glm (aggregarted glm (including tld and reseller as predictors))
    
    train_list = train_list[tld_reseller_list]
    train_df =  rbindlist(train_list,use.names=TRUE)
    model = build_model_first_renewal_agg(train_df)

    return(model)
}


train_agg_glm_ALL <- function(train_list,tld_reseller_list) {
    # agg glm (aggregarted glm (including tld and reseller as predictors))
    
    train_list = train_list[tld_reseller_list]
    train_df =  rbindlist(train_list,use.names=TRUE)
    model = build_model_first_renewal_agg(train_df)

    return(model)
}

train_agg_rf <- function(train_list,tld_reseller_list) {
    # agg rf (aggregarted rf (including tld and reseller as predictors))
    
    train_list = train_list[tld_reseller_list]
    train_df =  rbindlist(train_list, use.names=TRUE)
    
    if(dim(train_df)[1]==1){
    # if train data only has one observation, 
    # ... sample_fraction must be 1 (cant sample fraction of 1 observation)
        sample_fraction=1
    }else{
        sample_fraction=.8
    }
    
    suppressMessages(model <- ranger(
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
        respect.unordered.factors=TRUE)  )
    
    return(model)
}
   

# I. B) RESELLER MODELS #################################################################

train_seg_glm <- function(train_list, reseller_str) {
    # seg glm (reseller-segmented glm (including tld as predictor))

    # subset data for seg models
    tld_registrars = names(train_list)[endsWith(names(train_list),tolower(reseller_str))]
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
    tld_registrars = names(train_list)[endsWith(names(train_list),tolower(reseller_str))]
    train_list_reseller = train_list[tld_registrars]
    train_df_reseller =  rbindlist(train_list_reseller,use.names=TRUE)
    
    if(dim(train_df_reseller)[1]==1){
        # if train data only has one observation, 
        # ... sample_fraction must be 1 (cant sample fraction of 1 observation)
        sample_fraction=1
    }else{
        sample_fraction=.8
    }
    
    suppressMessages(model <- ranger(
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
        respect.unordered.factors=TRUE)  )
    
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
    
    suppressMessages(model <- ranger(
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
                        respect.unordered.factors=TRUE))
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
    if ( (dim(test_df_tld_reseller)[1]==0)|(!exists("model")) ){
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
     if ((dim(test_df_tld_reseller)[1]==0) |(!exists("model")) ){
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
    if ((dim(test_df_tld_reseller)[1]==0) | (!exists("model"))){
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
    if ((dim(test_df_tld_reseller)[1]==0) | (!exists("model"))) {
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
    if ((dim(test_df_tld_reseller)[1]==0) | (!exists("model"))){
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
    
    if ((dim(test_df_tld_reseller)[1]==0) | (!exists("model"))){
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
# III. EXCLUDE & SUPPLEMENT w/ FALLBACK 
#
#########################################################################################   

tld_registrar_excl <- function(train_list = expiry_train_prepped_2_1,
                              N=NULL) {
    
    train_df = rbindlist(train_list, use.names=TRUE)
    
    if(is.null(N)){ # threshold for low-volume tld-re
        # by default, N=100*(num_quarters)
        
        num_q = (as.yearqtr(max(train_df$expiry_date))-as.yearqtr(min(train_df$expiry_date)))*4
        N = as.integer(100 * num_q)
    }
    
    tld_registrar_excl_list = train_df %>% group_by(tld_registrar_index) %>% tally() %>% 
    filter(n<N) %>% pull(tld_registrar_index)
    
    N_perc = as.double(round(100*length(tld_registrar_excl_list)/(train_df %>% 
                        summarise(n_distinct(tld_registrar_index)) %>% pull(1) ),2))
    
    cat(paste0("Excluding ",length(tld_registrar_excl_list)," tld-re's (",
                
                N_perc,"%), due to volume < ",N," \n"))
    
    
    # remove where tld is .pw and .in.net (added 20201103)
    
    tld_registrar_excl_list_2 = train_df %>% filter(tld=="pw" | tld=="in.net") %>% distinct(tld_registrar_index)  %>% pull(tld_registrar_index) 
        
    N_perc_2 = as.double(round(100*length(tld_registrar_excl_list_2)/(train_df %>% 
                        summarise(n_distinct(tld_registrar_index)) %>% pull(1) ),2))
    
    cat(paste0("Excluding ",length(tld_registrar_excl_list_2)," tld-re's (",
                
                N_perc_2,"%) associated with tld's 'pw' and 'in.net' \n"))
    
    return(tld_registrar_excl_list)
    
}

geo_lookup <- function(geoLookupDF,
                         preds_df) {
        
    # 1. use geo lookup with duplicate rows removed
    #    remove duplicate rows in geo_lookup due to registrar level segmentation
    #    results in dims matching for expiry_test

    geoLookupDF <- geoLookupDF %>% distinct(reseller,reseller_country, reseller_geo)

    preds_df$reseller <- factor(preds_df$reseller)
    preds_df$reseller_country <- factor(preds_df$reseller_country)
    preds_df <- as.data.frame(preds_df)

    preds_df <- merge(preds_df,
                      geoLookupDF,
                      on=c('reseller','reseller_country'), 
                      all.x = TRUE)
    
    
    
    # 2. create a new lookup where we drop everything except for 
    #    reseller_country and _geo and have NA map to Others
    
    geoLookupDF <- geoLookupDF %>% distinct(reseller_country, reseller_geo) %>% 
                    mutate(reseller_geo = as.character(reseller_geo)) %>%
                    mutate(eseller_geo = if_else(is.na(reseller_country), 'Others', reseller_geo) ) %>% 
                    distinct(reseller_country, reseller_geo) %>% 
                    mutate(reseller_geo = as.factor(reseller_geo))

    # use new lookup to fill reseller_geo based just on reseller_country
    preds_df[['reseller_geo']][is.na(preds_df[['reseller_geo']])]<-
                    geoLookupDF$reseller_geo[match(
                        preds_df$reseller_country[is.na(preds_df[['reseller_geo']])],
                        geoLookupDF$reseller_country)]

    # manual fix for reseller geo 
    preds_df[['reseller_geo']][preds_df[['reseller_country']]=='Southafrica']<-'South Africa'

    # Print remaining NA reseller_geos
    rem_res <- preds_df %>% filter(is.na(reseller_geo)) %>% 
          distinct(reseller,reseller_country, reseller_geo) %>% pull(reseller)
    cat("Resellers with unmatched reseller_geo's: ",paste0(rem_res, sep=", "))
    
    return(preds_df)
}


#########################################################################################  
#
# IV. AGG Functions
#
#########################################################################################   

train_all <- function (tld_reseller_list,
                       tld_registrar_excl_list,
                       train_list = expiry_train_prepped_2_1,
                       test_list = expiry_test_prepped_2_1,
                       model_agg_glm = NULL,
                       model_agg_rf = NULL,
                      fullDir='../../data/output/models_20201015'){
    
    # keep list of all tld-re's
    tld_reseller_list_ALL = tld_reseller_list
    reseller_list_ALL = rbindlist(test_list, ,use.names=TRUE) %>% 
      filter(tld_registrar_index %in% tld_reseller_list_ALL) %>% 
      distinct(reseller)  %>%  pull(reseller)
    
    # exclude low-volume tld-re's 
    tld_reseller_list = tld_reseller_list[!(tld_reseller_list %in% tld_registrar_excl_list)]
    reseller_list = rbindlist(test_list, ,use.names=TRUE) %>% 
      filter(tld_registrar_index %in% tld_reseller_list) %>% 
      distinct(reseller)  %>%  pull(reseller)

    
    if(is.null(model_agg_glm)) {
        
        cat("\n\nTraining model_agg_glm_ALL\n")
        model_agg_glm_ALL = train_agg_glm(train_list,tld_reseller_list_ALL)
        save(model_agg_glm_ALL, 
             file=file.path(fullDir, 'model_agg_glm_ALL.Rdata'))
        
        }    
    
    if(is.null(model_agg_glm)) {
        
        cat("\n\nTraining model_agg_glm\n")
        model_agg_glm = train_agg_glm(train_list,tld_reseller_list)
        save(model_agg_glm, 
             file=file.path(fullDir, 'model_agg_glm.Rdata'))
        
        }    
    
    if(is.null(model_agg_rf)) {
        cat("\n\nTraining model_agg_rf_ALL\n")
        model_agg_rf_ALL = train_agg_rf(train_list,tld_reseller_list_ALL)   
        save(model_agg_rf_ALL, 
             file=file.path(fullDir, 'model_agg_rf_ALL.Rdata')
            )
        
    } 
    
    if(is.null(model_agg_rf)) {
        cat("\n\nTraining model_agg_rf\n")
        model_agg_rf = train_agg_rf(train_list,tld_reseller_list)   
        save(model_agg_rf, 
             file=file.path(fullDir, 'model_agg_rf.Rdata')
            )
        
    } 
    
    
    cat("\n\nTraining model_seg_glm & model_seg_rf\n")
    for (reseller_str in reseller_list_ALL) {
        
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
    
    
    cat("\n\nTraining model_seg2_glm & model_seg2_rf\n")
    for (tld_reseller_str in tld_reseller_list_ALL) {

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
    
    return(tld_reseller_list_ALL) # return all, predict for all and then exclude

}
    

pred_all <- function (tld_reseller_list, 
                      tld_registrar_excl_list,
                      test_list = expiry_test_prepped_2_1,
                      fullDir='../../data/output/models_20201015' # dir of models
                      ){   
    
    predDir = file.path(fullDir, 'preds')
    
    tld_reseller_list_ALL = tld_reseller_list
    
    # exclude low-volume tld-re's      
    tld_reseller_list = tld_reseller_list[!(tld_reseller_list %in% tld_registrar_excl_list)]
   
    cat("\n\nPredicting model_agg_glm_ALL\n")
    load(file.path(fullDir, 'model_agg_glm_ALL.Rdata'))    
    preds_agg_glm_ALL = lapply(tld_reseller_list_ALL, 
           function(tld_reseller_str) pred_agg_glm(model_agg_glm_ALL, test_list, tld_reseller_str)
           )
    rm(model_agg_glm)
    gc()
    
    save(preds_agg_glm_ALL, file=file.path(predDir, 'preds_agg_glm_ALL.Rdata'))
    
    cat("\n\nPredicting model_agg_glm\n")
    load(file.path(fullDir, 'model_agg_glm.Rdata'))    
    preds_agg_glm = lapply(tld_reseller_list_ALL, 
           function(tld_reseller_str) pred_agg_glm(model_agg_glm, test_list, tld_reseller_str)
           )
    rm(model_agg_glm)
    gc()
    
    save(preds_agg_glm, file=file.path(predDir, 'preds_agg_glm.RData'))
    
    cat("\n\nPredicting model_agg_rf_ALL\n")
    load(file.path(fullDir, 'model_agg_rf_ALL.Rdata'))
    preds_agg_rf_ALL = lapply(tld_reseller_list_ALL, 
           function(tld_reseller_str) pred_agg_rf(model_agg_rf_ALL, test_list, tld_reseller_str)
           )
    rm(model_agg_rf)
    gc() 
    
    save(preds_agg_rf_ALL, file=file.path(predDir, 'preds_agg_rf_ALL.RData'))
    
    cat("\n\nPredicting model_agg_rf\n")
    load(file.path(fullDir, 'model_agg_rf.Rdata'))
    preds_agg_rf = lapply(tld_reseller_list_ALL, 
           function(tld_reseller_str) pred_agg_rf(model_agg_rf, test_list, tld_reseller_str)
           )
    rm(model_agg_rf)
    gc()
    
    save(preds_agg_rf, file=file.path(predDir, 'preds_agg_rf.RData'))

    cat("\n\nPredicting model_seg_glm_ALL\n")   
    lapply(Sys.glob(file.path(fullDir,'model_seg_glm_*')),load,.GlobalEnv)
    preds_seg_glm_ALL = lapply(tld_reseller_list_ALL, 
           function(tld_reseller_str) pred_seg_glm(
               test_list, 
               tld_reseller_str)
           )
    rm(list=ls(pattern='^model_seg_glm_'))
    
    save(preds_seg_glm_ALL, file=file.path(predDir, 'preds_seg_glm_ALL.RData'))
    
    cat("\n\nPredicting model_seg_rf_ALL\n")  
    lapply(Sys.glob(file.path(fullDir,'model_seg_rf_*')),load,.GlobalEnv)
    preds_seg_rf_ALL = lapply(tld_reseller_list_ALL, 
           function(tld_reseller_str) pred_seg_rf(
               test_list, 
               tld_reseller_str)
           )
    rm(list=ls(pattern='^model_seg_rf_'))

    save(preds_seg_rf_ALL, file=file.path(predDir, 'preds_seg_rf_ALL.RData'))    
    
#     load(file.path(fullDir, 'preds_seg_rf_ALL.RData'))
#     load(file.path(fullDir, 'preds_seg_glm_ALL.RData'))
#     load(file.path(fullDir, 'preds_agg_rf.RData'))
#     load(file.path(fullDir, 'preds_agg_rf_ALL.RData'))
#     load(file.path(fullDir, 'preds_agg_glm.RData'))
#     load(file.path(fullDir, 'preds_agg_glm_ALL.Rdata'))
    
    cat("\n\nPredicting model_seg2_glm_ALL\n")
    lapply(Sys.glob(file.path(fullDir,'model_seg2_glm_*')),load,.GlobalEnv)
    preds_seg2_glm_ALL = lapply(tld_reseller_list_ALL, 
           function(tld_reseller_str) pred_seg2_glm(
               test_list, 
               tld_reseller_str)
           )
    rm(list=ls(pattern='^model_seg2_glm_'))
    
    save(preds_seg2_glm_ALL, file=file.path(fullDir, 'preds_seg2_glm_ALL.RData'))    

    cat("\n\nPredicting model_seg2_rf_ALL\n")     
    lapply(Sys.glob(file.path(fullDir,'model_seg2_rf_*')),load,.GlobalEnv)
    preds_seg2_rf_ALL = lapply(tld_reseller_list_ALL, 
           function(tld_reseller_str) pred_seg2_rf(
               test_list, 
               tld_reseller_str)
           )
    rm(list=ls(pattern='^model_seg2_rf_'))
    
    save(preds_seg2_rf_ALL, file=file.path(predDir, 'preds_seg2_rf_ALL.RData'))    

    
    # combine all preds into one list
    preds_list = list()
    i=1
    for (tld_reseller_str in tld_reseller_list_ALL) {
        if (is.na(preds_seg_glm_ALL[[i]])) { # replace preds_seg_glm with preds_seg_glm_ALL. dont know why 20201028
            preds_list[[tld_reseller_str]]= NA
        } else{
            preds_list[[tld_reseller_str]] = cbind(
                  test_list[[tld_reseller_str]],
                  'pred_agg_glm_ALL'=preds_agg_glm_ALL[[i]]$predicted,
                  'pred_agg_rf_ALL'=preds_agg_rf_ALL[[i]]$predicted,
                  'pred_agg_glm'=preds_agg_glm[[i]]$predicted,
                  'pred_agg_rf'=preds_agg_rf[[i]]$predicted,
                  'pred_seg_glm_ALL'=preds_seg_glm_ALL[[i]]$predicted,
                  'pred_seg_rf_ALL' = preds_seg_rf_ALL[[i]]$predicted,
                  'pred_seg2_glm_ALL'=preds_seg2_glm_ALL[[i]]$predicted,
                  'pred_seg2_rf_ALL'=preds_seg2_rf_ALL[[i]]$predicted)
        }

        i=i+1
    }

    na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }
    preds_list <- na.omit.list(preds_list)
    preds_df <- rbindlist(preds_list,use.names=TRUE)
                                                       
    # add in excluded tld-res -- not needed since we're predicting on ALL
#     excl_df = rbindlist(test_list[tld_registrar_excl_list], use.names=TRUE)
#     preds_df = rbind(preds_df, excl_df, use.names=TRUE, fill=TRUE)                               
                                                   
    return(preds_df)

}
    

