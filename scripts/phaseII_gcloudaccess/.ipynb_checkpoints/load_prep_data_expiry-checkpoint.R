library(data.table)

##################################
#                                #
#    FIRST STAGE RENEWALS ONLY   #
#                                #
##################################

# LOAD list of dfs
system("gsutil cp gs://data_outputt/output/expiry_train_prepped_1list /home/jupyter/local/Domains_202003/data/expiry_train_prepped_1list")
expiry_train_prepped_2 <- readRDS("/home/jupyter/local/Domains_202003/data/expiry_train_prepped_1list")

system("gsutil cp gs://data_outputt/output/expiry_test_prepped_1list /home/jupyter/local/Domains_202003/data/expiry_test_prepped_1list")
expiry_test_prepped_2 <- readRDS("/home/jupyter/local/Domains_202003/data/expiry_test_prepped_1list")

# REMOVE empty list items from each list (does not change the dim of rbound list)
expiry_train_prepped_2_1 <- expiry_train_prepped_2[sapply(expiry_train_prepped_2, function(x) dim(x)[1]) > 0]
expiry_test_prepped_2_1 <- expiry_test_prepped_2[sapply(expiry_test_prepped_2, function(x) dim(x)[1]) > 0]

# RBIND list of dfs
expiry_train_df_1 <- rbindlist(expiry_train_prepped_2_1, fill=TRUE)
expiry_test_df_1 <- rbindlist(expiry_test_prepped_2_1, fill=TRUE)

# SUBSET variables
expiry_train_df_sub <- subset(expiry_train_df_1, 
       select = c(renewal_status, tld, registrar, reseller_country, region, reg_period, 
                  sld_type, sld_length, day_domains, gibb_score, pattern_domain_count,reg_arpt) )                                                 
expiry_test_df_sub <- subset(expiry_test_df_1, 
       select = c(renewal_status, tld, registrar, reseller_country, region, reg_period, 
                  sld_type, sld_length, day_domains, gibb_score, pattern_domain_count,reg_arpt) )

                                                        