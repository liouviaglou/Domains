# Rscript predictions_metalearning.R >> /home/jupyter/local/Domains_202003/data/output/predictions_metalearning.log 2>&1

suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(ranger))
suppressMessages(library(pbapply))

# load & prep input data
source('../orig/functions_models.R')
source('../phaseII_03_forest/functions_eval.R')
source('../phaseII_03_forest/load_prep_data_expiry.R')
source('/functions_metalearning.R')


tld_reseller_list = expiry_train_df_1 %>%  pull(tld_registrar_index)

reseller_list = expiry_train_df_1 %>% pull(reseller)

train_all <- function (tld_reseller_list,
                       reseller_list,
                       train_list = expiry_train_prepped_2_1,
                       test_list = expiry_test_prepped_2_1
                       model_agg_glm = NULL,
                       model_agg_rf = NULL){
    
    subDir = paste("models", format(Sys.Date(), format="%Y%m%d") , sep = "_")
    fullDir = file.path('../../data/output', subDir)
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
        
        model_seg_glm = train_seg_glm(train_list, reseller_str) 
        save(model_seg_glm, 
             file=file.path(fullDir, paste0('model_seg_glm',sub(" ", "_", reseller_str),'.Rdata'))
            )
        
        model_seg_rf = train_seg_rf(train_list, reseller_str) 
        save(model_seg_rf, 
             file=file.path(fullDir, paste0('model_seg_rf',sub(" ", "_", reseller_str),'.Rdata'))
            )
        
    } 
    
    
    for (tld_reseller_str in tld_reseller_list) {
        
        model_seg2_glm = train_seg2_glm(train_list, tld_reseller_str) 
        save(model_seg2_glm, 
             file=file.path(fullDir, paste0('model_seg2_glm_',sub(" ", "_", tld_reseller_str),'.Rdata'))
            )
        
        model_seg2_rf = train_seg2_rf(train_list, tld_reseller_str)
        save(model_seg2_rf, 
             file=file.path(fullDir, paste0('model_seg2_rf',sub(" ", "_", tld_reseller_str),'.Rdata'))
            )
        
    }
    


}
    

pred_all <- function (tld_reseller_list,
                      test_list = expiry_test_prepped_2_1){
    
    
    
    
    load(file.path(fullDir, 'model_agg_glm.Rdata'))    
    preds_agg_glm = lapply(tld_reseller_list, 
           function(tld_reseller_str) pred_agg_glm(model_agg_glm, test_list, tld_reseller_str)
           )
    rm(model_agg_glm)
    gc()
    
    load(file.path(fullDir, 'model_agg_rf.Rdata'))
    preds_agg_rf = lapply(tld_reseller_list, 
           function(tld_reseller_str) pred_agg_rf(model_agg_rf, test_list, tld_reseller_str)
           )
    rm(model_agg_rf)
    gc()  
    
    
#         for (tld_reseller_str in tld_reseller_list) {
#             preds_agg_glm = pred_agg_glm(model_agg_glm, test_list, tld_reseller_str)
#             }
 


    preds_seg_glm = pred_seg_glm(model, test_list, tld_reseller_str)
    preds_seg_rf = pred_seg_rf(model, test_list, tld_reseller_str)

    preds_seg2_glm = pred_seg2_glm(model, test_list, tld_reseller_str)
    preds_seg2_rf = pred_seg2_rf(model, test_list, tld_reseller_str)
        
    }
    
}




tld_reseller_compare <- function (  tld_reseller_str,
                                    reseller_lookup,
                                    model_agg_glm = model_agg_glm,
                                    model_agg_rf = model_agg_rf,
                                    train_list = expiry_train_prepped_2_1,
                                    test_list = expiry_test_prepped_2_1){
    
    cat("\n\n")
    print(tld_reseller_str[[1]])
    
    reseller_str = reseller_lookup %>% filter(tld_registrar_index==tld_reseller_str) %>% pull(reseller)
    tld_registrars = names(expiry_train_prepped_2_1)[endsWith(names(expiry_train_prepped_2_1),reseller_str)]
    
    # subset data for seg2 models
    train_list_tld_reseller = train_list[tld_reseller_str]
    test_list_tld_reseller = test_list[tld_reseller_str]
    train_df_tld_reseller =  rbindlist(train_list_tld_reseller,use.names=TRUE)
    test_df_tld_reseller =  rbindlist(test_list_tld_reseller,use.names=TRUE)
    
    # subset data for seg models
    train_list_reseller = train_list[tld_registrars]
    test_list_reseller = test_list[tld_registrars]
    train_df_reseller =  rbindlist(train_list_reseller,use.names=TRUE)
    test_df_reseller =  rbindlist(test_list_reseller,use.names=TRUE)
    

    #####################################################################################
    # seg2 glm (tld-reseller-segmented glm)
    #####################################################################################
    
    cat("\n seg2 glm")
    
    # if test data contains no observations, skip!
    if (dim(test_df_tld_reseller)[1]==0){
        pred_df_seg2_glm = NA
    } else {
        model = mass_build_model_first_renewal(train_list_tld_reseller)
        pred = mass_predict_first_renewal(test_list_tld_reseller, model)
    
        pred_df_seg2_glm = data.frame("actual" = pred$renewal_status,
                                      "predicted" = pred$first_renewal_prediction)
    }
    
    #####################################################################################    
    # seg glm (reseller-segmented glm (including tld as predictor))
    #####################################################################################

    cat("\n seg glm")
    
    # if test data contains no observations, skip!
    if (dim(test_df_tld_reseller)[1]==0){
        pred_df_seg_glm = NA
    } else {

        if((nlevels(train_df_reseller$tld) < 2)){
            model = build_model_first_renewal(train_df_reseller)
        }else{
            model = build_model_first_renewal_reg(train_df_reseller)
        }



        pred = predict_first_renewal_reg(test_df_tld_reseller, model)

        pred_df_seg_glm = data.frame("actual" = pred$renewal_status,
                                      "predicted" = pred$first_renewal_prediction)
    }

    
    #####################################################################################    
    # agg glm (aggregarted glm (including tld and reseller as predictors))
    #####################################################################################
    
    cat("\n agg glm")
    
    # if test data contains no observations, skip!
    if (dim(test_df_tld_reseller)[1]==0){
        pred_df_agg_glm = NA
    } else {
        model = model_agg_glm
        pred = predict_first_renewal_agg(test_df_tld_reseller, model)
    
        pred_df_agg_glm = data.frame("actual" = pred$renewal_status,
                                      "predicted" = pred$first_renewal_prediction)
    }
    
    
    #####################################################################################
    # seg2 rf (tld-reseller-segmented rf)
    #####################################################################################

    cat("\n seg2 rf")
    
    # if test data contains no observations, skip!
    if (dim(test_df_tld_reseller)[1]==0){
        pred_df_seg2_rf = NA
    } else {
    #     if train data only has one observation, sample_fraction must be 1 (cant sample fraction of 1 observation)
        if(dim(train_df_tld_reseller)[1]==1){
            sample_fraction=1
        }else{
            sample_fraction=.8
        }

        model <- ranger(formula         = renewal_status ~ pattern_domain_count+log_reg_arpt+sld_length+gibb_score+sld_type+day_domains+reg_period, 
                        data            = train_df_tld_reseller, 
                        importance = 'impurity', 
                        num.trees       = 1000,
                        probability = TRUE,
                        replace = FALSE,
                        sample.fraction = sample_fraction,
                        seed            = 123,
                        respect.unordered.factors=TRUE)

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
    
    
    #####################################################################################
    # seg rf (reseller-segmented rf)
    #####################################################################################

    cat("\n seg rf")
    
    # if test data contains no observations, skip!
    if (dim(test_df_tld_reseller)[1]==0){
        pred_df_seg_rf = NA
    } else {
    #     if train data only has one observation, sample_fraction must be 1 (cant sample fraction of 1 observation)
        if(dim(train_df_reseller)[1]==1){
            sample_fraction=1
        }else{
            sample_fraction=.8
        }

        model <- ranger(formula         = renewal_status ~ pattern_domain_count+log_reg_arpt+sld_length+gibb_score+sld_type+day_domains+reg_period+tld, 
                        data            = train_df_reseller, 
                        importance = 'impurity', 
                        num.trees       = 1000,
                        probability = TRUE,
                        replace = FALSE,
                        sample.fraction = sample_fraction,
                        seed            = 123,
                        respect.unordered.factors=TRUE)

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
    
    #####################################################################################    
    # agg rf (aggregarted rf (including tld and reseller as predictors))
    #####################################################################################

    cat("\n agg rf")
    
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

     
    
    
    return(list(pred_df_seg2_glm = pred_df_seg2_glm, 
                pred_df_seg_glm = pred_df_seg_glm, 
                pred_df_agg_glm = pred_df_agg_glm, 
                pred_df_seg2_rf = pred_df_seg2_rf, 
                pred_df_seg_rf = pred_df_seg_rf, 
                pred_df_agg_rf = pred_df_agg_rf
    ))
    
    

}



tld_reseller_compare_list <- function ( tld_reseller_list=tld_reseller_list,
                                        reseller_lookup=reseller_lookup,
                                        model_agg_glm= agg_glm_basic_model,
                                        model_agg_rf= ranger_03_expiry2_f,
                                        train_list = expiry_train_prepped_2_1,
                                        test_list = expiry_test_prepped_2_1){
    
    
                        x <- lapply(tld_reseller_list, 
                                    function(tld_reseller_str) tld_reseller_compare(tld_reseller_str,
                                                                                reseller_lookup,
                                                                                model_agg_glm = agg_glm_basic_model,
                                                                                model_agg_rf = ranger_03_expiry2_f,
                                                                                train_list,
                                                                                test_list))

                        return(x)
                                    }

                                 
# load load saved agg models
load("../../data/agg_glm_basic_model")
load("../../data/ranger_03_expiry2_f")

# placeholder if agg models aren't needed
# agg_glm_basic_model=1
# ranger_03_expiry2_f=1
                                    
# tld_reseller_list = expiry_train_df_1 %>% group_by(tld_registrar_index, reseller) %>% tally() %>% arrange(desc(n)) %>% head(817) %>% pull(tld_registrar_index)
# reseller_lookup = expiry_train_df_1 %>% group_by(tld_registrar_index, reseller) %>% tally() %>% arrange(desc(n)) 
# return_list = tld_reseller_compare_list(tld_reseller_list=tld_reseller_list,
#                                         reseller_lookup=reseller_lookup,
#                                         model_agg_glm = agg_glm_basic_model,
#                                         model_agg_rf = ranger_03_expiry2_f,
#                                         train_list = expiry_train_prepped_2_1,
#                                         test_list = expiry_test_prepped_2_1)
                             
# save(return_list,  file="../../data/output/tld_reseller_compare_list_0001_0817_seg_rf")
                                    
                                   
tld_reseller_list = expiry_train_df_1 %>% group_by(tld_registrar_index, reseller) %>% tally() %>% arrange(desc(n)) %>% tail(906) %>% pull(tld_registrar_index)
reseller_lookup = expiry_train_df_1 %>% group_by(tld_registrar_index, reseller) %>% tally() %>% arrange(desc(n))
                                    
return_list = tld_reseller_compare_list(tld_reseller_list=tld_reseller_list,
                                        reseller_lookup=reseller_lookup,
                                        model_agg_glm = agg_glm_basic_model,
                                        model_agg_rf = ranger_03_expiry2_f,
                                        train_list = expiry_train_prepped_2_1,
                                        test_list = expiry_test_prepped_2_1)
                                    
saveRDS(return_list, file="../../data/output/tld_reseller_compare_list_0818_1723.RDS") 
                             
save(return_list,  file="../../data/output/tld_reseller_compare_list_0818_1723.RData")