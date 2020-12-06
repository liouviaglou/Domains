# Rscript predictions_metalearning.R > /home/jupyter/Domains_202003/data/output/predictions_metalearning.log 2>&1

suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(ranger))
suppressMessages(library(pbapply))
suppressMessages(library(stringr))

# load & prep input data
source('/home/jupyter/Domains_202003/scripts/orig/functions_models.R')
source('/home/jupyter/Domains_202003/scripts/phaseII_03_forest/functions_eval.R')
source('/home/jupyter/Domains_202003/scripts/phaseII_06_fallbacksupp/functions_metalearning.R')
source('/home/jupyter/Domains_202003/scripts/phaseII_06_fallbacksupp/load_prep_data_expiry_2.R')
# defines expiry_df & list of expiry_20180101_20190331
# as well as expiry_train_df, expiry_test_df,  expiry_train_list, expiry_test_list


# define oputput folder
fullDir='/home/jupyter/Domains_202003/data/output/models_20201104'
dir.create(fullDir)
dir.create(file.path(fullDir,'preds'))

# define tld-re's for training
tld_reseller_list = expiry_train_df %>%  distinct(tld_registrar_index) %>% pull(tld_registrar_index)
tld_registrar_excl_list = tld_registrar_excl(train_list = expiry_train_list)

# train & save models
tld_reseller_list = train_all(  tld_reseller_list,
                                tld_registrar_excl_list,
                                train_list = expiry_train_list,
                                test_list = expiry_test_list,
                                model_agg_glm = NULL, 
                                model_agg_rf = NULL,
                                fullDir)   

# define tld-re's for testing
tld_reseller_list = expiry_test_df %>%  distinct(tld_registrar_index) %>% pull(tld_registrar_index)
tld_registrar_excl_list = tld_registrar_excl(train_list = expiry_train_list)

# predict based on saved models
preds_df <- pred_all(tld_reseller_list, tld_registrar_excl_list,
                     test_list = expiry_test_list,
                     modelDir=fullDir,
                     fullDir=fullDir)

write.csv(preds_df, file=file.path(fullDir,'preds','preds.csv'),row.names = FALSE)
