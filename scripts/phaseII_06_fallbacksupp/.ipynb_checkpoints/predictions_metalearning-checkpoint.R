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


tld_reseller_list = expiry_train_df_1 %>%  distinct(tld_registrar_index) %>% pull(tld_registrar_index)

reseller_list = expiry_train_df_1 %>% distinct(reseller) %>% pull(reseller)




                                 
# load load saved agg models
# load("../../data/agg_glm_basic_model")
# save(agg_glm_basic_model, file="../../data/outut/models_20201015/model_agg_glm.Rdata")
# rm(agg_glm_basic_model)
# gc()
# load("../../data/ranger_03_expiry2_f")
# save(ranger_03_expiry2_f, file="../../data/outut/models_20201015/model_agg_rf.Rdata")
# rm(ranger_03_expiry2_f)
# gc()
