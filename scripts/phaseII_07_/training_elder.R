# Rscript training_elder.R /home/jupyter/Domains_202003/data/output/dp_datapull_20220822/expiry_20170101_20220822_test.RDS /home/jupyter/Domains_202003/data/output/dp_models_20210907 548 91 90 2021-08-31 2022-08-22 > /home/jupyter/Domains_202003/data/output/training_elder.log 2>&1
# Rscript training_elder.R /home/jupyter/Domains_202003/data/output/dp_datapull_20220822/expiry_20170101_20220822_test.RDS /home/jupyter/Domains_202003/data/output/dp_models_20210915 548 91 90 2020-12-31 2021-08-31 > /home/jupyter/Domains_202003/data/output/training_elder.log 2>&1


# This is the script that does it all: training, predicting, metalearning, ensembling, retraining, repredicting, etc.
# All components are contained within if statements to allow components to be run individually for testing.

# Here is how the general framework is set up:
# 1. Define common tld-resellers by choosing resellers w/>=1000 domains and tld-resellers with >= 50 domains. Based on that list, also define two other groups: uncommon tld-resellers, and all tld-resellers
# 2. Train segmented models using common tld-resellers
# 3. Train aggregated models using all tld-resellers
# 4. Predict common tld-resellers using both segmented and aggregated models
# 5. Predict uncommon tld-resellers using only aggregated models
# 6. Have two separate ensemble models--one for the common and one for uncommon, since they have different numbers of models available (6 vs 2)

suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(ranger))
suppressMessages(library(stringr))
suppressMessages(library(bigrquery))
suppressMessages(library(futile.logger))

# load & prep input data
source('/home/jupyter/Domains_202003/scripts/phaseII_07_/functions_metalearning.R')

args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)<7) {
  stop("Seven arguments must be supplied for this script: Input filepath, output directory, max training days, max train1 days, lookup eval days, end of train, and end of test", call.=FALSE)
}

inputFilepath <- args[1]
outputDir <- args[2]
train_days <- as.integer(args[3])
train1_days <- as.integer(args[4])
lookup_days <- as.integer(args[5])
end_train <- as.Date(args[6])
end_test <- as.Date(args[7])


# Get relevant dates
min_train1_date <- end_train - train1_days - lookup_days
train12_split_date <- end_train - lookup_days
min_retrain_date <- end_train - train_days

# Create output folder and subfolders
predDir <- file.path(outputDir, 'preds')
dir.create(predDir, recursive = TRUE)

# Which components of script should be run
run_train1 <- T
pred_train2 <- T
create_lookup <- T
retrain_all <- T
pred_test <- T
pred_lookup <- T
pred_ensemble <- T
save_bq <- T

use_retrained <- T
use_ensemble <- T
use_lookup <- T
dp <- T

# Which metrics and models
mets <- c('l10', 'auc', 'mape')
skipModels <- c("model_agg_glm", "model_agg_rf")

# Set logging threshold
flog.threshold(INFO)

# Read in data
flog.info("reading in all data\n")
df <- readRDS(inputFilepath)
df[, "old_renewal_status"] <- df[, "renewal_status"]
df$renewal_status <- as.integer(df$renewal_status %in% c("Renewed", "Transfered"))
exclude_tlds <- c('pw', 'in.net', 'uno')
df <- df[!df$tld %in% exclude_tlds, ]
df$expiry_date <- as.Date(df$expiry_date)
df[, "tld_registrar_index"] = tolower(paste0(df$tld, df$reseller))
df[, "reseller"] = tolower(df$reseller)

# Split train and test datasets
# Dataset coverage:
# Train1: [------------->      :                ]  For training initial models
# Train2: [              ----->:                ]  For creating lookup tables
# Train : [      ------------->:                ]  For creating final models
# Test  : [                    :--------------->]  For generating predictions

flog.info("Splitting data into train and test\n")
expiry_train_df <- df[(df$expiry_date > min_retrain_date) & (df$expiry_date <= end_train), ]
expiry_train1_df <- df[(df$expiry_date > min_train1_date) & (df$expiry_date <= train12_split_date), ]
expiry_train2_df <- df[(df$expiry_date > train12_split_date) & (df$expiry_date <= end_train), ]
expiry_test_df <- df[(df$expiry_date > end_train) & (df$expiry_date <= end_test), ]
rm(df)
gc()

# Define tld-re's for training
get_tld_reg <- function(df, n_tld=1000, n_tld_re=50) {
    keep_reg <- df %>% group_by(reseller) %>%
    summarize(n=n()) %>% 
    filter(n >= n_tld) %>% 
    pull(reseller) # Only get resellers with at least 1000 domains
    
    keep_tld_reg <- df %>% 
    group_by(tld_registrar_index) %>% 
    summarize(n=n()) %>% 
    filter(n >= n_tld_re) %>% 
    pull(tld_registrar_index) # Only get tld-reseller combos with at least 50 domains
    
    mask <- (df$reseller %in% keep_reg) & (df$tld_registrar_index %in% keep_tld_reg)
    df[mask, ] %>% distinct(tld_registrar_index) %>% pull(tld_registrar_index)
}

flog.info("defining tld-re's\n")
include_reg_train1 <- get_tld_reg(expiry_train1_df)
include_reg_train <- get_tld_reg(expiry_train_df)

tld_reseller_list <- intersect(include_reg_train1, include_reg_train)
flog.debug(tld_reseller_list)
tld_reseller_list_train1_all <- expiry_train1_df %>% distinct(tld_registrar_index) %>% pull(tld_registrar_index)
tld_reseller_list_train_all <- expiry_train_df %>% distinct(tld_registrar_index) %>% pull(tld_registrar_index)
tld_registrar_excl_list = c()

# train & save models
if (run_train1) {
    
    flog.info("Starting Train1")
    
    expiry_train1_list <- split(expiry_train1_df, expiry_train1_df$tld_registrar_index)
    expiry_train2_list <- split(expiry_train2_df, expiry_train2_df$tld_registrar_index)
    flog.info("Training segmented\n")
    # Use common tld-resellers to train segmented models
    n = train_all(  tld_reseller_list,
                                    tld_registrar_excl_list,
                                    train_list = expiry_train1_list,
                                    test_list = expiry_train2_list,
                                    skipModels=c(skipModels, "model_agg_glm_ALL", "model_agg_rf_ALL"),
                                    fullDir=outputDir,
                                    dp=dp)

    flog.info("Training aggregated\n")
    # Use all tld-resellers to train aggregated models
    n = train_all(  tld_reseller_list_train1_all,
                                    tld_registrar_excl_list,
                                    train_list = expiry_train1_list,
                                    test_list = expiry_train2_list,
                                    skipModels=c(skipModels, "model_seg2_glm_ALL", "model_seg_glm_ALL",
                                                "model_seg_rf_ALL", "model_seg2_rf_ALL"),
                                    fullDir=outputDir,
                                    dp=dp)    

    rm(expiry_train1_list, expiry_train2_list)
}
flog.info("Removing train1 files")
rm(expiry_train1_df)
gc()

if (pred_train2) {
    # define tld-re's for testing on train2
    expiry_train2_list <- split(expiry_train2_df, expiry_train2_df$tld_registrar_index)
    big_mask <- expiry_train2_df$tld_registrar_index %in% tld_reseller_list
    pred_tld_reseller_list <- expiry_train2_df[big_mask, ] %>% distinct(tld_registrar_index) %>% pull(tld_registrar_index)
    pred_tld_reseller_list_small <- expiry_train2_df[!big_mask, ] %>% distinct(tld_registrar_index) %>% pull(tld_registrar_index)

    # predict train2 based on saved models
    flog.info("Predicting train2 for large resellers\n")
    train2_preds_big <- pred_all(pred_tld_reseller_list, tld_registrar_excl_list,
                         test_list = expiry_train2_list,
                         modelDir=outputDir,
                         fullDir=outputDir,
                         skipPred=c(skipModels),
                         skipReturn=skipModels)

    flog.info("Predicting train2 for small resellers\n")
    train2_preds_small <- pred_all(pred_tld_reseller_list_small, tld_registrar_excl_list,
                         test_list = expiry_train2_list,
                         modelDir=outputDir,
                         fullDir=outputDir,
                         skipPred=c(skipModels, "model_seg_rf_ALL", "model_seg_glm_ALL",
                                   "model_seg2_rf_ALL", "model_seg2_glm_ALL"),
                         skipReturn=c(skipModels, "model_seg_rf_ALL", "model_seg_glm_ALL",
                                   "model_seg2_rf_ALL", "model_seg2_glm_ALL"))    
    
    # Concatenate and fillna(0)
    train2_preds <- bind_rows(train2_preds_big, train2_preds_small)
    train2_preds <- data.frame(train2_preds)
    pred_cols <- colnames(train2_preds)[grepl("^pred_", colnames(train2_preds))]
    train2_preds[, pred_cols][is.na(train2_preds[, pred_cols])] <- 0

    # Save files
    write.csv(train2_preds, file=file.path(predDir,'preds_train2.csv'),row.names = FALSE)
    write.csv(train2_preds_big, file=file.path(predDir,'preds_train2_big.csv'),row.names = FALSE)
    write.csv(train2_preds_small, file=file.path(predDir,'preds_train2_small.csv'),row.names = FALSE)

    rm(expiry_train2_list)

} else if (create_lookup | train_ensemble) {
    train2_preds <- read.csv(file.path(predDir,'preds_train2.csv'))
}
flog.info("Removing train2 files")
rm(expiry_train2_df)
gc()


if (create_lookup) {
    # Score predictions on train2
    flog.info('Scoring predictions')
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
    write.csv(df, file=file.path(predDir,'metrics_df.csv'),row.names = FALSE)
    
    # Create table with tld_registrar_index, metric, and best performing model
    flog.info('Creating lookup table')
    df_list <- list()
    for (met in mets) {
        flog.info(paste0(met, '\n'))
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
    write.csv(lookup_table, file=file.path(predDir,'lookup_table.csv'), row.names = FALSE)

} else if (pred_lookup) {
    lookup_table = read.csv(file.path(predDir,'lookup_table.csv'))
}
                            
if (retrain_all) {
    retrainDir <- file.path(outputDir, "retrained_models")
    dir.create(retrainDir, recursive = TRUE)

    expiry_train_list <- split(expiry_train_df, expiry_train_df$tld_registrar_index)
    expiry_test_list <- split(expiry_test_df, expiry_test_df$tld_registrar_index)
    allModels <-c("model_agg_rf_ALL", "model_agg_glm_ALL", "model_seg2_glm_ALL", "model_agg_glm",
                  "model_agg_rf", "model_seg_glm_ALL", "model_seg_rf_ALL", "model_seg2_rf_ALL")
    flog.info("Retraining segmented\n")
    # Use common tld-resellers to train segmented models
    n = train_all(  tld_reseller_list,
                    tld_registrar_excl_list,
                    train_list = expiry_train_list,
                    test_list = expiry_test_list,
                    skipModels=c(skipModels, "model_agg_glm_ALL", "model_agg_rf_ALL"),
                    fullDir=retrainDir,
                    dp=dp)

    flog.info("Retraining aggregated\n")
    # Use all tld-resellers to train aggregated models
    n = train_all(  tld_reseller_list_train_all,
                    tld_registrar_excl_list,
                    train_list = expiry_train_list,
                    test_list = expiry_test_list,
                    skipModels=c(skipModels, "model_seg2_glm_ALL", "model_seg_glm_ALL",
                                 "model_seg_rf_ALL", "model_seg2_rf_ALL"),
                    fullDir=retrainDir,
                    dp=dp)  

    rm(expiry_train_list, expiry_test_list)  
}
flog.info("Removing train files")
rm(expiry_train_df)
gc()
                                 
                   
if (pred_test) {

    if (use_retrained) {
        modelDir = file.path(outputDir, "retrained_models")
    } else {
        modelDir = outputDir
    }

    expiry_test_list <- split(expiry_test_df, expiry_test_df$tld_registrar_index)

    # define tld-re's for testing
    big_mask <- expiry_test_df$tld_registrar_index %in% tld_reseller_list
    pred_tld_reseller_list <- expiry_test_df[big_mask, ] %>% distinct(tld_registrar_index) %>% pull(tld_registrar_index)
    pred_tld_reseller_list_small <- expiry_test_df[!big_mask, ] %>% distinct(tld_registrar_index) %>% pull(tld_registrar_index)

    # predict test based on saved models
    flog.info("Predicting test using all models for large resellers\n")
    test_preds_big <- pred_all(pred_tld_reseller_list, tld_registrar_excl_list,
                         test_list = expiry_test_list,
                         modelDir=modelDir,
                         fullDir=modelDir,
                         skipPred=c(skipModels, "model_seg_rf_ALL", "model_seg_glm_ALL",
                                   "model_seg2_rf_ALL", "model_seg2_glm_ALL"),
                         skipReturn=skipModels)

    flog.info("Predicting test using aggregated models for small resellers\n")
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

    rm(expiry_test_list)
    
    # Save files
    write.csv(test_preds, file=file.path(predDir, 'preds_test.csv'),row.names = FALSE)
    write.csv(test_preds_big, file=file.path(predDir, 'preds_test_big.csv'),row.names = FALSE)
    write.csv(test_preds_small, file=file.path(predDir, 'preds_test_small.csv'),row.names = FALSE)
} else if (pred_lookup | pred_ensemble) {
    test_preds <- read.csv(file.path(predDir, 'preds_test.csv'))
}
flog.info("Removing test files")
rm(expiry_test_df)
gc()
                                 
if (pred_lookup) {
    # Use lookup table to decide which model to use and assign meta preds
    for (met in mets) {
        model_col <- paste0(met, '_best_model')
        pred_col <- paste0('pred_lookup_', met)
        tmp <- lookup_table %>% filter(metric == met) %>%
            select(tld_registrar_index, best_model)
        colnames(tmp)[colnames(tmp) == 'best_model'] <- model_col
        flog.info(head(tmp))
        test_preds <- test_preds %>% 
            left_join(tmp, by='tld_registrar_index')
        test_preds[,pred_col] <- rep(NA, nrow(test_preds))
        for (model in unique(test_preds[, model_col])) {
            mask <- test_preds[, model_col] == model
            mask[is.na(mask)] = FALSE
            flog.info(mean(mask))
            test_preds[mask, pred_col] <- test_preds[mask, model]
        }
    }

    # Save results
    write.csv(test_preds, file=file.path(predDir, "test_preds_lookup.csv"), row.names = FALSE)
} else if (save_bq & use_lookup) {
    flog.info('Reading in lookup test preds')
    test_preds <- read.csv(file.path(predDir, "test_preds_lookup.csv"))
}
                                 
if (pred_ensemble) {

    pred_cols_big <- colnames(test_preds)[grepl("^pred_[as]", colnames(test_preds))]
    pred_cols_small <- c('pred_agg_glm_ALL', 'pred_agg_rf_ALL')

    # New Ensemble approach--just average!
    test_preds_big[, "pred_ensemble"] <- rowMeans(test_preds_big[, pred_cols_big])
    test_preds_small[, "pred_ensemble"] <- rowMeans(test_preds_small[, pred_cols_small])
    
    test_preds <- bind_rows(test_preds_big, test_preds_small)
    write.csv(test_preds, file=file.path(predDir, "test_preds_ensemble.csv"), row.names = FALSE)
} else if (save_bq & use_ensemble) {
    flog.info('Reading in ensemble test preds')
    ensemble_test_preds <- read.csv(file.path(predDir, "test_preds_ensemble.csv"))
    if (use_lookup) {
        meta_pred_cols <- colnames(test_preds)[grepl("^pred_lookup", colnames(test_preds))]
        test_preds <- merge(ensemble_test_preds, test_preds[, c("domain", meta_pred_cols)], by="domain")
    } else {
        test_preds <- ensemble_test_preds
    }
}
                                 
if (save_bq) {
    min_date <- min(expiry_test_df$expiry_date)
    max_date <- max(expiry_test_df$expiry_date)
    bq_table_name <- paste0("myriad-303821.expiry.dp_expiry_preds_", min_date, "_", max_date)
    flog.info(paste("BQ Table Name:", bq_table_name, "\n"))
    
    # Convert wide to long
    flog.info('Converting wide to long')
    pred_vars <- names(test_preds)[grepl("^pred_", names(test_preds))]
    preds_df_melt <- melt(setDT(test_preds), measure.vars = pred_vars, variable.name = "model", value.name = "predicted")

    # Save to bq table in chunks
    flog.info(paste0("Saving meta_preds with ", nrow(preds_df_melt), " rows\n"))
    nrows <- nrow(preds_df_melt)
    max_rows <- 500000
    curr_start <- 1
    curr_end <- 0
    while (curr_end < nrows) {
        curr_end <- min(curr_start + max_rows - 1, nrows)
        flog.info(paste0("Saving rows ", curr_start, "-", curr_end, "\n"))
        bq_table_upload(bq_table_name, preds_df_melt[curr_start:curr_end, ], fields=preds_df_melt, 
                        create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')
        if (curr_end == nrows) break
        curr_start <- curr_start + max_rows
    }
}
