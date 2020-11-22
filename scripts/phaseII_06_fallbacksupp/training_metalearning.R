# Rscript predictions_metalearning.R > /home/jupyter/Domains_202003/data/output/predictions_metalearning.log 2>&1

# Takes as input preds_df output from predictions_metalearning.R
# Supplements with fallback
# Computes overall model performance
# Engineers features at tld-reseller level
# Trains metalearning model(s) to assign model based on features
# Assigns model based on previous

suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(ranger))
suppressMessages(library(pbapply))
suppressMessages(library(stringr))

source('functions_fallback.R')


########################################################################################################
#
# LOAD DATA
#
########################################################################################################
# Load preds output from predictions_metalearning.R
expiry_df_test_preds <- read.csv("../../data/output/datapull_20201116/expiry_df_test_preds.csv")

# Load training data used for predictions_metalearning.R to assign fallback values
expiry_df_train <- read.csv("../../data/output/datapull_20201116/expiry_df_train.csv")

# Load geo_suppl for train and test-pred data
geoLookupDF <- read.csv("/home/jupyter/Domains_202003/data/input/PredictiveModelAnalysis_ResellerGeoMap.csv")

# Supplement both train and test_preds with geo information
expiry_df_train_g <- geo_suppl(expiry_df_train, geoLookupDF = geoLookupDF)
expiry_df_test_preds_g <- geo_suppl(expiry_df_test_preds, geoLookupDF = geoLookupDF)


########################################################################################################
#
# SUPPLEMENT w/ FALLBACK
#
########################################################################################################

# generate list of fallback tables
npv_fallback_list = fallback_gen( npv_historic_renewal_data = expiry_df_train_g, 
                                 reseller_am_geo_map = geoLookupDF)

# return list members to in-memory objects of the same name
names(npv_fallback_list)
for(i in 1:length(npv_fallback_list)) assign(names(npv_fallback_list)[i], npv_fallback_list[[i]])

# generate list of low-volume tld-re's from training data
tld_registrar_excl_list = tld_registrar_excl(train_list = expiry_list_train_g)

# generate placeholder (*_fb) columns in preds df where predictions for low-volume tld-registrars get set to NA
expiry_df_test_preds_g2 <- expiry_df_test_preds_g %>%
     mutate( across(contains('pred_'), 
                    .fns = list(fb = ~ifelse(tld_registrar_index %in% tld_registrar_excl_list, NA, . )) ))