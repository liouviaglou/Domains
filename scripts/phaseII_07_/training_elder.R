# Rscript training_elder.R > /home/jupyter/Domains_202003/data/output/training_elder.log 2>&1

# This is the script that does it all: training, predicting, metalearning, ensembling, retraining, repredicting, etc.
# All components are contained within if statements to allow components to be run individually for testing.

# Here is how the general framework is set up:
# 1. Define common tld-resellers by choosing resellers w/>=1000 domains. Based on that list, also define two other groups: uncommon tld-resellers, and all tld-resellers
# 2. Train segmented models using common tld-resellers
# 3. Train aggregated models using all tld-resellers
# 4. Predict common tld-resellers using both segmented and aggregated models
# 5. Predict uncommon tld-resellers using only aggregated models
# 6. Have two separate ensemble models--one for the common and one for uncommon, since they have different numbers of models available (6 vs 2)

suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(ranger))
suppressMessages(library(pbapply))
suppressMessages(library(stringr))
suppressMessages(library(bigrquery))

# load & prep input data
source('/home/jupyter/Domains_202003/scripts/orig/functions_models.R')
source('/home/jupyter/Domains_202003/scripts/phaseII_06_fallbacksupp/functions_metalearning.R')

# Set date of most recent pull
today <- as.Date("2021-02-23")
maxdate <- today - 50
mindate <- maxdate - 456
dp <- TRUE
pred_2021 <- T  # To run test predictions using 2021
recent_pred_date <- as.Date("2021-06-30")

# Hardcoding these for now..
date_split1 <- as.Date("2020-04-13")
date_split2 <- as.Date("2020-10-24")

# Which components of script should be run
run_train1 <- F
pred_train2 <- F
create_lookup <- F
train_meta_model <- F
train_ensemble <- F
retrain_all <- F
pred_test <- T
pred_meta_model <- F
pred_meta <- T
pred_ensemble <- T
save_bq <- F

use_meta_model <- F
use_retrained <- T

# Which metrics and models
mets <- c('l10', 'auc', 'mape')
skipModels <- c("model_agg_glm", "model_agg_rf")

# Local directory
directory <- '/home/jupyter/Domains_202003/data/output/dp_datapull_20210223/'
directory_2021 <- paste0('/home/jupyter/Domains_202003/data/output/dp_datapull_', strftime(recent_pred_date, format="%Y%m%d"), '/')
# define output folder
cat("creating folder\n")
fullDir='/home/jupyter/Domains_202003/data/output/dp_models_20201104_all'
dir.create(file.path(fullDir,'preds'), recursive = TRUE)


train1_file <- paste0('expiry_20191006_', strftime(date_split1, format="%Y%m%d"), '_train1.RDS')
train2_file <- paste0('expiry_', strftime(date_split1, format="%Y%m%d"), '_', strftime(date_split2, format="%Y%m%d"), '_train2.RDS')
test_file <- paste0('expiry_', strftime(date_split2, format="%Y%m%d"), '_20210104_test.RDS')
test_dir <- paste0(directory, 'expiry_', strftime(date_split2, format="%Y%m%d"), '_20210104_test.RDS')

# Defining paths for saving predictions and models
# Key is predictions for the retrain version are saved separately
predDir <- file.path(fullDir, 'preds')

if (pred_2021) {
    retrainDir='/home/jupyter/Domains_202003/data/output/dp_models_retrained_2021'
    if (use_retrained) {
        predDir <- file.path(retrainDir, 'preds')
    }
    
    test_dir <- paste0(directory_2021, 'expiry_20210104_', strftime(recent_pred_date, format="%Y%m%d"), '_test.RDS')  # Test data
    test_preds_path <- file.path(predDir,'preds_test_2021')  # Path for saving test predictions from 6 base models
    test_meta_preds_path <- file.path(predDir,'preds_test_with_meta_2021.csv')  # Path for saving test predictions including meta model predictions
    ensemble_preds_path <- file.path(predDir,'preds_ensemble_2021.csv')  # Path for saving test predictions including ensemble predictions
    meta_lookup_path <- file.path(fullDir, 'preds', 'meta_lookup_table_2021.csv')  # Path for meta lookup table
    bq_table_name <- paste0('myriad-303821.expiry.preds_20210104_',format(recent_pred_date, format="%Y%m%d"))
} else {
    retrainDir='/home/jupyter/Domains_202003/data/output/dp_models_retrained'
    if (use_retrained) {
        predDir <- file.path(retrainDir, 'preds')
    }
    test_dir <- paste0(directory, 'expiry_', strftime(date_split2, format="%Y%m%d"), '_20210104_test.RDS')  # Test input data
    test_preds_path <- file.path(predDir,'preds_test.csv')  # Path for saving test predictions from 6 base models
    test_meta_preds_path <- file.path(predDir,'preds_test_with_meta.csv')  # Path for saving test predictions including meta model predictions
    ensemble_preds_path <- file.path(predDir,'preds_ensemble.csv')  # Path for saving test predictions including ensemble predictions
    meta_lookup_path <- file.path(fullDir, 'preds', 'meta_lookup_table.csv')  # Path for meta lookup table
    bq_table_name <- paste0('myriad-303821.expiry.preds_',format(date_split2, format="%Y%m%d"),'_',format(maxdate, format="%Y%m%d"))
}

dir.create(file.path(predDir,'preds'), recursive = TRUE)

if (use_meta_model) {
    # Change naming of files to reflect using meta_model vs lookup table
    test_meta_preds_path <- gsub("meta", "meta_model_missRanger", test_meta_preds_path)
    meta_lookup_path <- gsub("meta", "meta_model_missRanger", meta_lookup_path)
    bq_table_name <- gsub("preds", "meta_model_preds", bq_table_name)
}

# Read in train datasets
if (run_train1 | pred_train2 | pred_test | retrain_all) {
    cat("reading in first training data\n")
    train1_str <- paste0('expiry_',format(mindate, format="%Y%m%d"),'_',format(date_split1, format="%Y%m%d") ,'_train1.RDS')
    expiry_train1_df <- readRDS(paste0(directory, train1_str))
    expiry_train1_df$old_renewal_status <- expiry_train1_df$renewal_status
    expiry_train1_df$renewal_status <- as.integer(expiry_train1_df$renewal_status == "Renewed")
    expiry_train1_list <- split(expiry_train1_df, expiry_train1_df$tld_registrar_index)
    
    # Define tld-re's for training
    cat("defining tld-re's\n")
    exclude_tlds <- c('pw', 'in.net', 'uno')
    include_reg <- expiry_train1_df %>% 
        group_by(reseller) %>% 
        summarize(n=n()) %>% 
        filter(n >= 1000) %>% 
        pull(reseller)  # Only get resellers with at least 1000 domains in train1
    print(include_reg)
    lim_mask <- (!expiry_train1_df$tld %in% exclude_tlds) & (expiry_train1_df$reseller %in% include_reg)
    unlim_mask <- (!expiry_train1_df$tld %in% exclude_tlds)
    print(mean(lim_mask))
    print(mean(unlim_mask))
    tld_reseller_list <- expiry_train1_df[lim_mask, ] %>% distinct(tld_registrar_index) %>% pull(tld_registrar_index)
    tld_reseller_list_all <- expiry_train1_df[unlim_mask, ] %>% distinct(tld_registrar_index) %>% pull(tld_registrar_index)
    tld_registrar_excl_list = c()
}

if (pred_train2 | retrain_all) {
    cat("reading in second training data\n")
    train2_str <- paste0('expiry_',format(date_split1, format="%Y%m%d"),'_',format(date_split2, format="%Y%m%d") ,'_train2.RDS')
    expiry_train2_df <- readRDS(paste0(directory, train2_str))
    expiry_train2_df$renewal_status <- as.integer(expiry_train2_df$renewal_status == "Renewed")
    expiry_train2_df$old_renewal_status <- expiry_train2_df$renewal_status
    expiry_train2_list <- split(expiry_train2_df, expiry_train2_df$tld_registrar_index)    
}

if (pred_test) {
    # Read in test dataset
    cat("reading in testing data\n")
    expiry_test_df <- readRDS(test_dir)
    expiry_test_df$old_renewal_status <- expiry_test_df$renewal_status
    expiry_test_df$renewal_status <- as.integer(expiry_test_df$renewal_status == "Renewed")
    expiry_test_list <- split(expiry_test_df, expiry_test_df$tld_registrar_index)    
}

# train & save models
if (run_train1) {
    
    cat("starting training\n")
   
    cat("Training segmented\n")
    # Use common tld-resellers to train segmented models
    n = train_all(  tld_reseller_list,
                                    tld_registrar_excl_list,
                                    train_list = expiry_train1_list,
                                    test_list = expiry_train2_list,
                                    skipModels=c(skipModels, "model_agg_glm_ALL", "model_agg_rf_ALL"),
                                    fullDir=fullDir,
                                    dp=dp)

    cat("Training aggregated\n")
    # Use all tld-resellers to train aggregated models
    n = train_all(  tld_reseller_list_all,
                                    tld_registrar_excl_list,
                                    train_list = expiry_train1_list,
                                    test_list = expiry_train2_list,
                                    skipModels=c(skipModels, "model_seg2_glm_ALL", "model_seg_glm_ALL",
                                                "model_seg_rf_ALL", "model_seg2_rf_ALL"),
                                    fullDir=fullDir,
                                    dp=dp)    
}

if (pred_train2) {
    # define tld-re's for testing on train2
    big_mask <- (!expiry_train2_df$tld %in% exclude_tlds) & (expiry_train2_df$reseller %in% include_reg)
    small_mask <- (!expiry_train2_df$tld %in% exclude_tlds) & (!expiry_train2_df$reseller %in% include_reg)
    pred_tld_reseller_list <- expiry_train2_df[big_mask, ] %>% distinct(tld_registrar_index) %>% pull(tld_registrar_index)
    pred_tld_reseller_list_small <- expiry_train2_df[small_mask, ] %>% distinct(tld_registrar_index) %>% pull(tld_registrar_index)

    # predict train2 based on saved models
    cat("Predicting train2 for large resellers\n")
    train2_preds_big <- pred_all(pred_tld_reseller_list, tld_registrar_excl_list,
                         test_list = expiry_train2_list,
                         modelDir=fullDir,
                         fullDir=fullDir,
                         skipPred=c(skipModels),
                         skipReturn=skipModels)

    cat("Predicting train2 for small resellers\n")
    train2_preds_small <- pred_all(pred_tld_reseller_list_small, tld_registrar_excl_list,
                         test_list = expiry_train2_list,
                         modelDir=fullDir,
                         fullDir=fullDir,
                         skipPred=c(skipModels, "model_seg_rf_ALL", "model_seg_glm_ALL",
                                   "model_seg2_rf_ALL", "model_seg2_glm_ALL"),
                         skipReturn=c(skipModels, "model_seg_rf_ALL", "model_seg_glm_ALL",
                                   "model_seg2_rf_ALL", "model_seg2_glm_ALL"))    
    
    # Concatenate and fillna(0)
    train2_preds <- bind_rows(train2_preds_big, train2_preds_small)
    train2_preds <- data.frame(train2_preds)
    pred_cols <- colnames(train2_preds)[grepl("^pred_", colnames(train2_preds))]
    train2_preds[, pred_cols][is.na(train2_preds[, pred_cols])] <- 0

    
    write.csv(train2_preds, file=file.path(fullDir,'preds','preds_train2.csv'),row.names = FALSE)
    write.csv(train2_preds_big, file=file.path(fullDir,'preds','preds_train2_big.csv'),row.names = FALSE)
    write.csv(train2_preds_small, file=file.path(fullDir,'preds','preds_train2_small.csv'),row.names = FALSE)
} else if (create_lookup | train_meta_model | train_ensemble) {
    train2_preds <- read.csv(file.path(fullDir,'preds','preds_train2.csv'))
    train2_preds_big <- read.csv(file.path(fullDir,'preds','preds_train2_big.csv'))
    train2_preds_small <- read.csv(file.path(fullDir,'preds','preds_train2_small.csv'))
}

if (create_lookup) {
    # Score predictions on train2
    print('Scoring predictions')
    metrics_df <- train2_preds %>%
      group_by(tld_registrar_index) %>%
      do( l10_pred_seg2_glm_ALL = l10_dplyr(., pred_var = "pred_seg2_glm_ALL"),
          l10_pred_seg_glm_ALL = l10_dplyr(., pred_var = "pred_seg_glm_ALL"),
           l10_pred_agg_glm_ALL = l10_dplyr(., pred_var = "pred_agg_glm_ALL"),
           l10_pred_seg2_rf_ALL = l10_dplyr(., pred_var = "pred_seg2_rf_ALL"),
           l10_pred_seg_rf_ALL = l10_dplyr(., pred_var = "pred_seg_rf_ALL"),
           l10_pred_agg_rf_ALL = l10_dplyr(., pred_var = "pred_agg_rf_ALL"),

          auc_pred_seg2_glm_ALL = auc_dplyr(., pred_var = "pred_seg2_glm_ALL"),
          auc_pred_seg_glm_ALL = auc_dplyr(., pred_var = "pred_seg_glm_ALL"),
           auc_pred_agg_glm_ALL = auc_dplyr(., pred_var = "pred_agg_glm_ALL"),
           auc_pred_seg2_rf_ALL = auc_dplyr(., pred_var = "pred_seg2_rf_ALL"),
           auc_pred_seg_rf_ALL = auc_dplyr(., pred_var = "pred_seg_rf_ALL"),
           auc_pred_agg_rf_ALL = auc_dplyr(., pred_var = "pred_agg_rf_ALL"),

          mape_pred_seg2_glm_ALL = mape_dplyr(., pred_var = "pred_seg2_glm_ALL"),
          mape_pred_seg_glm_ALL = mape_dplyr(., pred_var = "pred_seg_glm_ALL"),
           mape_pred_agg_glm_ALL = mape_dplyr(., pred_var = "pred_agg_glm_ALL"),
           mape_pred_seg2_rf_ALL = mape_dplyr(., pred_var = "pred_seg2_rf_ALL"),
           mape_pred_seg_rf_ALL = mape_dplyr(., pred_var = "pred_seg_rf_ALL"),
           mape_pred_agg_rf_ALL = mape_dplyr(., pred_var = "pred_agg_rf_ALL"),
        )

    df <- apply(metrics_df,2,as.character)
    write.csv(df, file=file.path(fullDir,'preds','metrics_df.csv'),row.names = FALSE)
    # Create table with tld_registrar_index, metric, and best performing model
    print('Creating lookup table')
    df_list <- list()
    for (met in mets) {
        cat(paste0(met, '\n'))
        met_cols <- names(metrics_df)[grepl(paste0("^", met), names(metrics_df))]
        tmp <- metrics_df[, met_cols]
        s <- nchar(met) + 2
        rep_cols <- substring(met_cols, s)
        if (met == "mape") {
            best_models <- apply(tmp, 1, function(x) rep_cols[which.min(x)])
        } else {
            best_models <- apply(tmp, 1, function(x) rep_cols[which.max(x)])
        }
        new_df <- data.frame(tld_registrar_index = metrics_df$tld_registrar_index,
                             best_model = best_models,
                             metric = rep(met, nrow(metrics_df)))
        df_list[[met]] <- new_df
    }
    lookup_table <- bind_rows(df_list)
#     write.csv(lookup_table, file=file.path(fullDir,'preds','lookup_table_train2.csv'),row.names = FALSE)
    write.csv(lookup_table, file=meta_lookup_path, row.names = FALSE)

} else if (pred_meta & (!pred_meta_model)) {
    lookup_table = read.csv(meta_lookup_path)
}
                                 
if (train_meta_model) {
    meta_df <- gen_meta_df(train2_preds)
    metametrics_df <- merge(meta_df, lookup_table, by='tld_registrar_index', all=TRUE)
    saveRDS(metametrics_df, file.path(fullDir, "meta_data.RDS"))
    
    # Train Model
    for (met in unique(lookup_table$metric)) {
        mask <- metametrics_df$metric == met
        drop_cols <- c('tld_registrar_index', 'metric')
        curr_data <- metametrics_df[mask, (!names(metametrics_df) %in% drop_cols)]
        
        
        model <- ranger(best_model ~ .,
                        data=curr_data,
                        importance='impurity',
                        num.trees=500,
                        probability = TRUE,
                        replace = FALSE,
                        sample.fraction = .8,
                        seed            = 123,
                        respect.unordered.factors=TRUE)
        save(model, file=file.path(fullDir, paste0('model_meta_', met,'.Rdata')))
    }
}

if (train_ensemble) {
    pred_cols_big <- colnames(train2_preds_big)[grepl("^pred_", colnames(train2_preds_big))]
    pred_cols_small <- c('pred_agg_glm_ALL', 'pred_agg_rf_ALL')

    right_side_big <- paste(c("1", pred_cols_big), collapse="+")
    f_big <- as.formula(paste0("renewal_status ~ ", right_side_big))
    print(f_big)

    right_side_small <- paste(c("1", pred_cols_small), collapse="+")
    f_small <- as.formula(paste0("renewal_status ~ ", right_side_small))
    print(f_small)
    
    model_big<-glm(f_big,
               family=binomial(link='logit'),
               data=train2_preds_big, 
               y = FALSE, model = FALSE)
    save(model_big, 
             file=file.path(fullDir, 'model_glm_ensemble_big.Rdata'))

    model_small<-glm(f_small,
               family=binomial(link='logit'),
               data=train2_preds_small, 
               y = FALSE, model = FALSE)
    save(model_small, 
             file=file.path(fullDir, 'model_glm_ensemble_small.Rdata'))
}
                                 
if (retrain_all) {
    cat("Contatenating training sets")
    if (pred_2021) {
        old_test_dir <- paste0(directory, 'expiry_', strftime(date_split2, format="%Y%m%d"), '_20210104_test.RDS')
        expiry_tmp_df <- readRDS(old_test_dir)
        expiry_tmp_df$old_renewal_status <- expiry_tmp_df$renewal_status
        expiry_tmp_df$renewal_status <- as.integer(expiry_tmp_df$renewal_status == "Renewed")
        expiry_train_df <- rbind(expiry_train1_df, expiry_train2_df, expiry_tmp_df)
        expiry_train_list <- split(expiry_train_df, expiry_train_df$tld_registrar_index)
    } else {
        expiry_train_df <- rbind(expiry_train1_df, expiry_train2_df)
        expiry_train_list <- split(expiry_train_df, expiry_train_df$tld_registrar_index)
    }

    allModels <-c("model_agg_rf_ALL", "model_agg_glm_ALL", "model_seg2_glm_ALL", "model_agg_glm",
                  "model_agg_rf", "model_seg_glm_ALL", "model_seg_rf_ALL", "model_seg2_rf_ALL")
    cat("Retraining segmented\n")
    # Use common tld-resellers to train segmented models
    n = train_all(  tld_reseller_list,
                    tld_registrar_excl_list,
                    train_list = expiry_train_list,
                    test_list = expiry_test_list,
                    skipModels=c(skipModels, "model_agg_glm_ALL", "model_agg_rf_ALL"),
                    fullDir=retrainDir,
                    dp=dp)

    cat("Retraining aggregated\n")
    # Use all tld-resellers to train aggregated models
    n = train_all(  tld_reseller_list_all,
                    tld_registrar_excl_list,
                    train_list = expiry_train_list,
                    test_list = expiry_test_list,
                    skipModels=c(skipModels, "model_seg2_glm_ALL", "model_seg_glm_ALL",
                                 "model_seg_rf_ALL", "model_seg2_rf_ALL", "model_agg_rf_ALL", "model_agg_glm_ALL"),
                    fullDir=retrainDir,
                    dp=dp)    
}
                                 
                   
if (pred_test) {

    if (use_retrained) {
        modelDir = retrainDir
    } else {
        modelDir = fullDir
    }
    # define tld-re's for testing
    big_mask <- (!expiry_test_df$tld %in% exclude_tlds) & (expiry_test_df$reseller %in% include_reg)
    small_mask <- (!expiry_test_df$tld %in% exclude_tlds) & (!expiry_test_df$reseller %in% include_reg)
    pred_tld_reseller_list <- expiry_test_df[big_mask, ] %>% distinct(tld_registrar_index) %>% pull(tld_registrar_index)
    pred_tld_reseller_list_small <- expiry_test_df[small_mask, ] %>% distinct(tld_registrar_index) %>% pull(tld_registrar_index)

    # predict test based on saved models
    cat("Predicting test for large resellers\n")
    test_preds_big <- pred_all(pred_tld_reseller_list, tld_registrar_excl_list,
                         test_list = expiry_test_list,
                         modelDir=modelDir,
                         fullDir=modelDir,
                         skipPred=c(skipModels),
                         skipReturn=skipModels)

    cat("Predicting test for small resellers\n")
    test_preds_small <- pred_all(pred_tld_reseller_list_small, tld_registrar_excl_list,
                         test_list = expiry_test_list,
                         modelDir=modelDir,
                         fullDir=modelDir,
                         skipPred=c(skipModels, "model_seg_rf_ALL", "model_seg_glm_ALL",
                                   "model_seg2_rf_ALL", "model_seg2_glm_ALL"),
                         skipReturn=c(skipModels, "model_seg_rf_ALL", "model_seg_glm_ALL",
                                   "model_seg2_rf_ALL", "model_seg2_glm_ALL"))    
    
    # To Dataframe
    test_preds_big <- data.frame(test_preds_big)
    test_preds_small <- data.frame(test_preds_small)

    # Concatenate and fillna(0)
    test_preds <- bind_rows(test_preds_big, test_preds_small)
    pred_cols <- colnames(test_preds)[grepl("^pred_", colnames(test_preds))]
    test_preds[, pred_cols][is.na(test_preds[, pred_cols])] <- 0

    
    write.csv(test_preds, file=paste0(test_preds_path, '.csv'),row.names = FALSE)
    write.csv(test_preds_big, file=paste0(test_preds_path, '_big.csv'),row.names = FALSE)
    write.csv(test_preds_small, file=paste0(test_preds_path, '_small.csv'),row.names = FALSE)
} else if (pred_meta | pred_meta_model | pred_ensemble) {
    test_preds <- read.csv(paste0(test_preds_path, '.csv'))
    test_preds_big <- read.csv(paste0(test_preds_path, '_big.csv'))
    test_preds_small <- read.csv(paste0(test_preds_path, '_small.csv'))
}

                               
if (pred_meta_model) {
    meta_df <- gen_meta_df(test_preds)
    
    # Generate predictions
    df_list <- list()
    for (met in mets) {
        curr_meta_df <- data.frame(meta_df)
        model_name <- paste0('model_meta_', met)
        load(file.path(fullDir, paste0(model_name,'.Rdata')))
        
        # Restrict to same columns as training dataset
        train_cols <- names(model$variable.importance)
        missing_cols <- train_cols[(!train_cols %in% colnames(meta_df))]
        curr_meta_df[, missing_cols] <- -1
        curr_meta_df <- curr_meta_df[, colnames(curr_meta_df) %in% train_cols]
        
        # Make and save predictions
        pred <- data.frame(predict(model, data=curr_meta_df)$predictions)
        if (met == "mape") {
            pred = -1 * pred
        }
        print(head(pred))
        tmp <- data.frame(
            tld_registrar_index=meta_df$tld_registrar_index,
            metric=rep(met, nrow(meta_df)),
            best_model=names(pred)[max.col(pred)]
        )
        df_list[[met]] <- tmp
    }
    lookup_table <- bind_rows(df_list)
    write.csv(lookup_table, file=meta_lookup_path, row.names = FALSE)
} else if (pred_meta & use_meta_model) {
    lookup_table = read.csv(meta_lookup_path)
}

if (pred_meta) {
    # Use lookup table to decide which model to use and assign meta preds
    for (met in mets) {
        model_col <- paste0(met, '_best_model')
        pred_col <- paste0('pred_meta_', met)
        tmp <- lookup_table %>% filter(metric == met) %>%
            select(tld_registrar_index, best_model)
        colnames(tmp)[colnames(tmp) == 'best_model'] <- model_col
        print(head(tmp))
        test_preds <- test_preds %>% 
            left_join(tmp, by='tld_registrar_index')
        test_preds[,pred_col] <- rep(NA, nrow(test_preds))
        for (model in unique(test_preds[, model_col])) {
            mask <- test_preds[, model_col] == model
            mask[is.na(mask)] = FALSE
            print(mean(mask))
            test_preds[mask, pred_col] <- test_preds[mask, model]
        }
    }

    # Save results
    write.csv(test_preds, file=test_meta_preds_path, row.names = FALSE)
} else if (save_bq) {
    print('Reading in test preds')
    test_preds <- read.csv(test_meta_preds_path)
}
                                 
if (pred_ensemble) {
    load(file.path(fullDir, 'model_glm_ensemble_big.Rdata'))
    load(file.path(fullDir, 'model_glm_ensemble_small.Rdata'))
    pred_cols_big <- colnames(test_preds)[grepl("^pred_[as]", colnames(test_preds))]
    pred_cols_small <- c('pred_agg_glm_ALL', 'pred_agg_rf_ALL')

    test_preds_big[, "pred_ensemble"] <- predict(model_big, newdata=test_preds_big[, pred_cols_big], type="response")
    test_preds_small[, "pred_ensemble"] <- predict(model_small, newdata=test_preds_small[, pred_cols_small], type="response")

    test_preds <- bind_rows(test_preds_big, test_preds_small)
    write.csv(test_preds, file=ensemble_preds_path, row.names = FALSE)
} else if (save_bq) {
    print('Reading in test preds')
    test_preds <- read.csv(ensemble_preds_path)
}
                                 
if (save_bq) {
    # Convert wide to long
    print('Converting wide to long')
    pred_vars <- names(test_preds)[grepl("^pred_", names(test_preds))]
    preds_df_melt <- melt(setDT(test_preds), measure.vars = pred_vars, variable.name = "model", value.name = "predicted")

    # Save to bq table in chunks
    cat(paste0("Saving meta_preds with ", nrow(preds_df_melt), " rows\n"))
    nrows <- nrow(preds_df_melt)
    max_rows <- 1000000
    curr_start <- 1
    curr_end <- 0
    while (curr_end < nrows) {
        curr_end <- min(curr_start + max_rows - 1, nrows)
        cat(paste0("Saving rows ", curr_start, "-", curr_end, "\n"))
        bq_table_upload(bq_table_name, preds_df_melt[curr_start:curr_end, ], fields=preds_df_melt, 
                        create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')
        if (curr_end == nrows) break
        curr_start <- curr_start + max_rows
    }
}
