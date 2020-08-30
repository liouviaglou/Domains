# Rscript reseller_compare_list.R >> /home/jupyter/local/Domains_202003/data/output/reseller_compare_list_seg2_rd.log 2>&1

suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(ranger))
suppressMessages(library(pbapply))

# load & prep input data
source('../orig/functions_models.R')
source('../phaseII_03_forest/functions_eval.R')
source('../phaseII_03_forest/load_prep_data_expiry.R')

reseller_compare <- function (reseller_str,
                             train_list = expiry_train_prepped_2_1,
                             test_list = expiry_test_prepped_2_1){
    
    cat("\n\n")
    print(reseller_str)
    
    # subset data
    
    # modified 08/06 & run (for 51-n)
    tld_registrars = names(expiry_train_prepped_2_1)
    tld_registrars = tld_registrars[endsWith(tld_registrars,reseller_str)]
    
    train_list = train_list[tld_registrars]
    test_list = test_list[tld_registrars]
    train_df =  rbindlist(train_list)
    test_df =  rbindlist(test_list)
    
    # if test data contains no observations, skip reseller!
    if (dim(test_df)[1]==0){
        return(list( lift_df_seg_glm = NA, 
                     lift_df_agg_glm = NA, 
                     lift_df_ranger_03_fact = NA,
                     lift_df_ranger_03_fact_2 = NA))
    }
    
    # tld-segmented glm
    seg_glm_model = mass_build_model_first_renewal(train_list)
    seg_glm_pred = mass_predict_first_renewal(test_list, seg_glm_model)
    
    seg_glm_pred_df = data.frame("actual" = seg_glm_pred$renewal_status,
                                 "predicted" = seg_glm_pred$first_renewal_prediction)
    lift_df_seg_glm <- chart_lift(pred_df = seg_glm_pred_df,
                                  dep_var = "actual",
                                  pred_var = "predicted")
    
    # agg glm
    
    # if there is only one tld, build_model_first_renewal_reg will fail 
    # ... but performance will be same as tld-segmented
    
    if (length(train_list)==1){
        lift_df_agg_glm <- lift_df_seg_glm
    } else {
        agg_glm_model = build_model_first_renewal_reg(train_df)
        agg_glm_pred = predict_first_renewal_reg(test_df, agg_glm_model)
        
        agg_glm_pred_df = data.frame("actual" = agg_glm_pred$renewal_status,
                                 "predicted" = agg_glm_pred$first_renewal_prediction)
        lift_df_agg_glm <- chart_lift(pred_df = agg_glm_pred_df,
                                  dep_var = "actual",
                                  pred_var = "predicted")
    }
    
    
    # agg rf_f
    
    # if train only has one observation, sample fraction must be == 1
    
    if(dim(train_df)[1]==1){
        sample_fraction=1
    }else{
        sample_fraction=.8
    }
    
    ranger_03_fact <- ranger(
    formula         = renewal_status ~ pattern_domain_count+log_reg_arpt+sld_length+gibb_score+sld_type+day_domains+reg_period+tld, 
    data            = train_df, 
    importance = 'impurity', 
    num.trees       = 5,
    probability = TRUE,
    mtry            = 3,
    min.node.size   = 1000,
    replace = FALSE,
    sample.fraction = sample_fraction,
    seed            = 123,
    respect.unordered.factors=TRUE)
    
    # if test data has no rows, lift_df is NA
    if (dim(test_df)[1]==0){
        lift_df_ranger_03_fact <- NA
    }else{
         ranger_predict_03_fact <- predict(ranger_03_fact, 
                              data = test_df,
                              type="response")$predictions
    
        # if all Renewed col doesn't exist in predictions, create it with value 0
        if(is.null(as.data.frame(ranger_predict_03_fact)$Renewed)){
            ranger_predict_03_fact$Renewed <- 0
        }

        ranger_predict_03_fact <- as.data.frame(ranger_predict_03_fact)$Renewed
        ranger_predict_03_fact = data.frame("actual" = test_df$renewal_status,
                  "predicted" = ranger_predict_03_fact)
        lift_df_ranger_03_fact <- chart_lift(pred_df = ranger_predict_03_fact,
                                dep_var = "actual",
                                pred_var = "predicted")
    }
    
    # seg2 rf_f
    
    ranger_predict_03_fact_2 = vector()
    ranger_labels = vector()
    
    for (i in seq(length(tld_registrars))){ # for each tld-reseller in reseller subset...
        tld_registrar = tld_registrars[i]
        
        # ...create a separate train_df and test_df
        train_df = train_list[[tld_registrar]]
        test_df = test_list[[tld_registrar]]        
        
        if(dim(train_df)[1]==1){
            sample_fraction=1
        }else{
            sample_fraction=.8
        }

        ranger_03_fact <- ranger(
        formula         = renewal_status ~ pattern_domain_count+log_reg_arpt+sld_length+gibb_score+sld_type+day_domains+reg_period, 
        data            = train_df, 
        importance = 'impurity', 
        num.trees       = 5,
        probability = TRUE,
        mtry            = 3,
        min.node.size   = 1000,
        replace = FALSE,
        sample.fraction = sample_fraction,
        seed            = 123,
        respect.unordered.factors=TRUE)

        # if test data has no rows, lift_df is NA
        if ( is.null(test_df)){
            predictions <- NA
        }else if ((dim(test_df)[1]==0)) {
            predictions <- NA
        }else{
             predictions <- predict(ranger_03_fact, 
                                  data = test_df,
                                  type="response")$predictions

            # if all Renewed col doesn't exist in predictions, create it with value 0
            if(is.null(as.data.frame(predictions)$Renewed)){
                predictions <- as.data.frame(predictions)
                predictions$Renewed <- 0
            }}

            ranger_predict_03_fact_2 <- append(ranger_predict_03_fact_2,
                                             as.data.frame(predictions)$Renewed)
        
            ranger_labels <- append(as.character(ranger_labels),
                                    as.character(test_df$renewal_status))
            
        }
        
        if ( is.null(test_df)){
            lift_df_ranger_03_fact_2 <- NA
        }else if ((dim(test_df)[1]==0)) {
            lift_df_ranger_03_fact_2 <- NA
        }else{
        ranger_predict_03_fact_2 = data.frame("actual" = ranger_labels,
                      "predicted" = ranger_predict_03_fact_2)
        lift_df_ranger_03_fact_2 <- chart_lift(pred_df = ranger_predict_03_fact_2,
                                    dep_var = "actual",
                                    pred_var = "predicted")


        
        
        }
    
    
    
    return(list(lift_df_seg_glm = lift_df_seg_glm, 
                lift_df_agg_glm = lift_df_agg_glm, 
                lift_df_ranger_03_fact = lift_df_ranger_03_fact,
                lift_df_ranger_03_fact_2 = lift_df_ranger_03_fact_2))
    
    

}



reseller_compare_list <- function (reseller_list=reseller_list,
                             train_list = expiry_train_prepped_2_1,
                             test_list = expiry_test_prepped_2_1){
    
    
                        x <- lapply(reseller_list, 
                                    function(reseller_str) reseller_compare(reseller_str,
                                                                           train_list,
                                                                           test_list))

                        return(x)
                                    }

reseller_list = expiry_train_df_sub %>% group_by(reseller) %>% tally() %>% arrange(desc(n)) %>% pull(reseller)
                
reseller_compare_list_51_n = reseller_compare_list(reseller_list=reseller_list,
                             train_list = expiry_train_prepped_2_1,
                             test_list = expiry_test_prepped_2_1)
                             
save(reseller_compare_list_51_n,  file="../../data/output/tld_reseller_compare_list")