# Rscript training_elder.R > /home/jupyter/Domains_202003/data/output/training_elder.log 2>&1

suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(ranger))
suppressMessages(library(pbapply))
suppressMessages(library(stringr))
suppressMessages(library(bigrquery))

# load & prep input data
source('/home/jupyter/Domains_202003/scripts/orig/functions_models.R')
source('/home/jupyter/Domains_202003/scripts/phaseII_03_forest/functions_eval.R')
source('/home/jupyter/Domains_202003/scripts/phaseII_06_fallbacksupp/functions_metalearning.R')
# source('/home/jupyter/Domains_202003/scripts/phaseII_06_fallbacksupp/load_prep_data_expiry_2.R')
# defines expiry_df & list of expiry_20180101_20190331
# as well as expiry_train_df, expiry_test_df,  expiry_train_list, expiry_test_list

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
pred_test <- T
pred_meta <- T
save_bq <- T

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
if (pred_2021) {
    test_dir <- paste0(directory_2021, 'expiry_20210104_', strftime(recent_pred_date, format="%Y%m%d"), '_test.RDS')
    print(test_dir)
    test_preds_path <- file.path(fullDir,'preds','preds_test_2021.csv')
    test_meta_preds_path <- file.path(fullDir,'preds','preds_test_with_meta_2021.csv')
    bq_table_name <- paste0('myriad-303821.expiry.preds_20210104_',format(recent_pred_date, format="%Y%m%d"))
    
} else {
    test_dir <- paste0(directory, 'expiry_', strftime(date_split2, format="%Y%m%d"), '_20210104_test.RDS')
    test_preds_path <- file.path(fullDir,'preds','preds_test.csv')
    test_meta_preds_path <- file.path(fullDir,'preds','preds_test_with_meta.csv')
    bq_table_name <- paste0('myriad-303821.expiry.preds_',format(date_split2, format="%Y%m%d"),'_',format(maxdate, format="%Y%m%d"))
}

# Read in train datasets
if (run_train1 | pred_train2 | pred_test) {
    cat("reading in first training data\n")
    train1_str <- paste0('expiry_',format(mindate, format="%Y%m%d"),'_',format(date_split1, format="%Y%m%d") ,'_train1.RDS')
    expiry_train1_df <- readRDS(paste0(directory, train1_str))
    expiry_train1_df$old_renewal_status <- expiry_train1_df$renewal_status
    expiry_train1_df$renewal_status <- as.integer(expiry_train1_df$renewal_status == "Renewed")
    expiry_train1_list <- split(expiry_train1_df, expiry_train1_df$tld_registrar_index)
    
    # define tld-re's for training
    cat("defining tld-re's\n")
    exclude_tlds <- c('pw', 'in.net', 'uno')
    # include_reg <- c('Go Daddy', 'Namecheap')#, 'GMO', 'Google')
    # include_reg <- expiry_train1_df %>% 
    #     distinct(reseller) %>% 
    #     pull(reseller) # All resellers
    include_reg <- expiry_train1_df %>% 
        group_by(reseller) %>% 
        summarize(n=n()) %>% 
        filter(n >= 1000) %>% 
        pull(reseller)  # Only get resellers with at least 1000 domains in train1
    print(include_reg)
    mask <- (!expiry_train1_df$tld %in% exclude_tlds) & (expiry_train1_df$reseller %in% include_reg)
    print(mean(mask))
    tld_reseller_list <- expiry_train1_df[mask, ] %>% distinct(tld_registrar_index) %>% pull(tld_registrar_index)
    tld_registrar_excl_list = c()
}

if (pred_train2) {
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
    tld_reseller_list = train_all(  tld_reseller_list,
                                    tld_registrar_excl_list,
                                    train_list = expiry_train1_list,
                                    test_list = expiry_train2_list,
                                    skipModels=c(skipModels, "model_seg_rf_ALL", "model_seg2_rf_ALL", "model_agg_rf_ALL", "model_agg_glm_ALL"),
                                    fullDir=fullDir,
                                    dp=dp)    
}

if (pred_train2) {
    # define tld-re's for testing on train2
    mask <- (!expiry_train2_df$tld %in% exclude_tlds) & (expiry_train2_df$reseller %in% include_reg)
    tld_reseller_list <- expiry_train2_df[mask, ] %>% distinct(tld_registrar_index) %>% pull(tld_registrar_index)

    # predict train2 based on saved models
    train2_preds <- pred_all(tld_reseller_list, tld_registrar_excl_list,
                         test_list = expiry_train2_list,
                         modelDir=fullDir,
                         fullDir=fullDir,
                         skipPred=c("model_agg_rf_ALL", "model_agg_glm", "model_agg_rf", 
                                    "model_seg_rf_ALL", "model_seg2_rf_ALL"),
                         skipReturn=skipModels)
    write.csv(train2_preds, file=file.path(fullDir,'preds','preds_train2.csv'),row.names = FALSE)
} else if (create_lookup) {
    train2_preds <- read.csv(file.path(fullDir,'preds','preds_train2.csv'))
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
    write.csv(lookup_table, file=file.path(fullDir,'preds','lookup_table_train2.csv'),row.names = FALSE)
} else if (pred_meta) {
    lookup_table = read.csv(file.path(fullDir,'preds','lookup_table_train2.csv'))
}  
                             
if (pred_test) {
    # define tld-re's for testing on test
    mask <- (!expiry_test_df$tld %in% exclude_tlds) & (expiry_test_df$reseller %in% include_reg)
    tld_reseller_list <- expiry_test_df[mask, ] %>% distinct(tld_registrar_index) %>% pull(tld_registrar_index)

    # predict test based on saved models
    test_preds <- pred_all(tld_reseller_list, tld_registrar_excl_list,
                         test_list = expiry_test_list,
                         modelDir=fullDir,
                         fullDir=fullDir,
                         pred_folder='preds_test',
                         skipPred=c("model_seg2_glm", "model_agg_glm", "model_agg_rf"),
                         skipReturn=skipModels)
    test_preds <- data.frame(test_preds)
    
    write.csv(test_preds, file=test_preds_path,row.names = FALSE)
} else if (pred_meta) {
    test_preds <- read.csv(test_preds_path)
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




