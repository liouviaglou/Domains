# Rscript predictions_metalearning.R >> /home/jupyter/local/Domains_202003/data/output/predictions_metalearning.log 2>&1

suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(ranger))
suppressMessages(library(pbapply))
suppressMessages(library(stringr))

# load & prep input data
source('../orig/functions_models.R')
source('../phaseII_03_forest/functions_eval.R')
source('../phaseII_03_forest/load_prep_data_expiry.R')
source('functions_metalearning.R')

# define oputput folder
fullDir='../../data/output/models_20201020_3'

# define tld-re's for processing
tld_reseller_list = expiry_train_df_1 %>%  distinct(tld_registrar_index) %>% pull(tld_registrar_index)
tld_registrar_excl_list = tld_registrar_excl(train_list = expiry_train_prepped_2_1)
tld_reseller_list = tld_reseller_list[!(tld_reseller_list %in% tld_registrar_excl_list)]
tld_reseller_list = c('websitenamecheap','storeuol','storemat bao','onlinedonweb','onlinecrazy domains')

# train & save models
tld_reseller_list = train_all(  tld_reseller_list,
                                tld_registrar_excl_list,
                                train_list = expiry_train_prepped_2_1,
                                test_list = expiry_test_prepped_2_1,
                                model_agg_glm = NULL, 
                                model_agg_rf = NULL,
                                fullDir)   

# predict based on saved models
preds_df <- pred_all(tld_reseller_list, tld_registrar_excl_list,
                     test_list = expiry_test_prepped_2_1,
                     fullDir)

write.csv(preds_df, file=file.path(fullDir,'preds.csv'),row.names = FALSE)
