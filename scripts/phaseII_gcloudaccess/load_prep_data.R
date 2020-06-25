library(data.table)

# Full training data was generated as follows
# renewal_training_data_first<-readRDS("../../data/input/npv/renewal_training_data_first")
# renewal_training_data$renewal_status<-as.factor(renewal_training_data$renewal_status)
# renewal_training_data_first<-renewal_training_data %>%
#  dplyr::filter(renewal_type == "FirstTime")
# renewal_training_data_first<-mass_prep_data(renewal_training_data_first)
# tld_registrar_list<-names(renewal_training_data_first)
# prediction_list<-pblapply(tld_registrar_list, function(i) list_predict_first_renewal(i, # renewal_training_data_first, first_renewal_model))
# prediction_list<-prediction_list[!is.na(prediction_list)]
# prediction_op<-rbindlist(prediction_list)
# train_data<-rbindlist(renewal_training_data_first)
# train_data$first_renewal_prediction<-prediction_op$probabilities[match(train_data$domain_id,prediction_op$domain_id)]
# saveRDS(train_data, "../../data/output/npv/first_renewal_preds")

train_data <- readRDS("../../data/first_renewal_preds")

# Train & Test Split were generated as follows, do not rerun for consistency
# train_index <- sample(1:nrow(train_data), 0.8 * nrow(train_data))
# test_index <- setdiff(1:nrow(train_data), train_index)
# train <- train_data[train_index,]
# test <- train_data[test_index,]

train <- readRDS("../../data/train")
test <- readRDS("../../data/test")

train_df <- rbindlist(train)
test_df <- rbindlist(test)

df_prep <- function (df){
    df_f <- subset(df, select = -c(sld,pattern,domain,sld_type2,tld_registrar_index,
                                           renew_reseller,renew_registrar,renewal_type,renewed_count,
                                           renew_type,autorenew_type,renew_period,renew_arpt,
                                           renew_domain_revenue,status,first_renewal_prediction,
                                           domain_id))
    
    df_f$renew_Y <- format(as.Date(df_f$renew_date, 
                                     format="%Y-%m-%d"),"%Y")
    df_f$creation_Y <- format(as.Date(df_f$creation_date, 
                                            format="%Y-%m-%d"),"%Y")
    df_f$expiry_Y <- format(as.Date(df_f$expiry_date, 
                                          format="%Y-%m-%d"),"%Y")
    df_f$renew_M <- format(as.Date(df_f$renew_date, 
                                         format="%Y-%m-%d"),"%m")
    df_f$creation_M <- format(as.Date(df_f$creation_date, 
                                            format="%Y-%m-%d"),"%m")
    df_f$expiry_M <- format(as.Date(df_f$expiry_date, 
                                          format="%Y-%m-%d"),"%m")

    df_f <- subset(df_f, select = -c(renew_date,
                                                 creation_date,
                                                 expiry_date))
    
    df_f <- df_f %>% mutate_if(sapply(df_f, is.character), as.factor)
    
    return(df_f)
}

# tic("total")
# tic("train prep")
train_df_f <- df_prep(train_df)
# toc()
# tic("text prep")
test_df_f <- df_prep(test_df)
# toc()
# toc()