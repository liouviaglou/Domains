# Rscript --vanilla load_prep_data_expiry_3_elder.R radix2020 expiry 2021-02-23

# This is a modified version of load_prep_data_expiry_3.R for the Elder team. 
# It retrieves the BQ tables in the radix2020 project that Luba creates by 
# running the load_prep_data_expiry_3.R script, merges in the region information,
# and saves them locally to the Elder GCP project.

# Rscript --vanilla load_prep_data_expiry_3.R radix2020 expiry expiry_prepped_data.sql

library(data.table)
library(bigrquery)
library(dplyr)
options(scipen = 20)

# Script automatically pulls most recently available, accurate 5Q worth of data 
# & splits it into a 10/45/45 test/train/train split
# outputs are 
#    1. 3 BQ tables, in the current project (may not be needed?)
#    2. 3 RDS files, in local directory
#    3. 3 RDS files, in google cloud storage
# intermediary big query table containing entire pull from .sql file
# .... is set to auto-delete within 1 hour of being created (enough time for script to run
# Notes:  - Naming convention for expiry data pulls: expiry_mindate_maxdate_pulldate
#         - Generatiion of 2 is most time & resource intensive, can be moved to subsequent scripts
#           ... 3 depends on 2 (writing directly to GCP without loading into memory is not implemented here)
#         - Dataframes/Lists generated for inputs to predictions_metalearning.R, which doesn't use the 10% test subset (expiry_valid_df)
#           ... we'll need to modify training_metalearning.R to train on expiry_test_* and test on expiry_valid*

# TODO before running:
# Change the directory var & bucket var definitions to suit your folder/bucket structure
# Make the  database (using bq mk expiry)


args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)<3) {
  stop("Three arguments must be supplied for BQ table creation: project name, database name, query file loc", call.=FALSE)
}

projname_str <- args[1]
dbname_str <- args[2]
today <- args[3]
# query_file <- args[3] 

# projname_str <- 'radix2020'
# dbname_str <- 'expiry'
# query_file <- 'expiry_prepped_data.sql'

# today <- Sys.Date()
today <- as.Date(today)

# CREATE local dir for data (to be pushed in its entirety to GCP storage)
directory <- paste0('../../data/output/datapull_', format(today, format="%Y%m%d"))
dir.create(directory, showWarnings = FALSE)

# DEFINE GCP Storage bucket for wiritng tables
bucket <- "gs://data_outputt/output/"

# Read in regional lookup
reg_lookup <- bq_table_download(bq_project_query("radix2020", 
                                paste0("SELECT * FROM  expiry.regionlookup_20180101_20211231_20210311")))
reg_lookup <- unique(reg_lookup)

########################################################################################################################
#                                                                                                                      #
# LOAD data to BQ table in my project (if table doesn't exist already)
# This will create a new a new table expiry.expiry_20180101_20211231_20210215
# Naming convention for expiry data pulls: expiry_mindate_maxdate_pulldate
#                                                                                                                      #
########################################################################################################################


# tblname_str_1 <- paste0('expiry_20180101_20211231_',format(today, format="%Y%m%d"))
# tblloc_str_1 <- paste0(projname_str,':',dbname_str,'.',tblname_str_1)

# # CREATE intermediary big query table containing entire pull from .sql file
# command_str <-  paste0("bq query --max_rows=0 --use_legacy_sql=false --destination_table='",
#                        tblloc_str_1,"' --flagfile='",query_file,"' ")
# cat("Executing command:\n\t", command_str,"\n")
# system(command_str)
# cat("Created BQ table", tblloc_str_1 ,"\n")

# # AUTODELETE intermediary big query table containing entire pull from .sql file
# command_str <- paste0("bq update --expiration 3600 '", tblloc_str_1,"'")
# cat("Executing command:\n\t", command_str,"\n")
# system(command_str)
# cat("Updated BQ table", tblloc_str_1 ," to be automatically deleted in 1 hour (",format(Sys.time()+ 1*60*60, tz="America/Los_Angeles",usetz=TRUE),")\n")

########################################################################################################################
#                                                                                                                      #
# PULL 5Q (456 day) subset, generate 10/45/45% test/train/train split
# https://www.oreilly.com/content/repeatable-sampling-of-data-sets-in-bigquery-for-machine-learning/
#                                                                                                                      #
########################################################################################################################

# DEFINE date ranges given contraints 
maxdate <- today - 50
mindate <- maxdate - 456

# ########################################################################################################################
# # CREATE test table                                                                                                    #
# ########################################################################################################################

tblname_str_2 <- paste0('expiry_',format(mindate, format="%Y%m%d"),'_',format(maxdate, format="%Y%m%d") ,'_test')
tblloc_str_2 <- paste0(projname_str,':',dbname_str,'.',tblname_str_2)

# query_str <- gsub("[\r\n]", " ", paste0("SELECT * FROM  ",dbname_str,'.',tblname_str_1," t WHERE
#   DATE(expiry_date) BETWEEN \"",mindate,"\" AND \"",maxdate,"\" AND renewed_count=1 AND  
#   ABS(HASH(expiry_date)) % 100 < 10"))

# command_str <- paste0("bq query --max_rows=0 --destination_table='", tblloc_str_2,"' '", query_str ,"'")
# cat("Executing command:\n\t", command_str,"\n")
# system(command_str)

# cat("Created BQ table", tblloc_str_2 ,"\n")

eval(call("<-", as.name(tblname_str_2), 
          bq_table_download(bq_project_query(projname_str, 
                                             paste0("SELECT * FROM  ",dbname_str,'.',tblname_str_2)
                                            ))))
saveRDS(get(tblname_str_2),file = paste0(directory,'/',tblname_str_2,'.RDS'), 
        compress=TRUE)


cat("Created RDS table", paste0(directory,'/',tblname_str_2,'.RDS') ,"\n")

expiry_valid_df <- get(tblname_str_2)
expiry_valid_df <- expiry_valid_df %>% 
                filter(!is.na(gibb_score)) %>% # remove where gibb_score, etc. are NA
                mutate (reg_arpt = ifelse(reg_arpt <= 0, 0.0001,reg_arpt), # add necessary columns
                                   log_reg_arpt = log(reg_arpt),
                                   tld_registrar_index = tolower(paste(tld, reseller,sep="")))
# expiry_valid_list <- split(expiry_valid_df, expiry_valid_df$tld_registrar_index)
expiry_valid_df <- merge(expiry_valid_df, reg_lookup, on="domain_id", all.x=TRUE)
write.csv(expiry_valid_df, file = paste0(directory,'/',tblname_str_2,'.csv'), row.names = FALSE)


# ########################################################################################################################
# # CREATE train1 table                                                                                                  #
# ########################################################################################################################

tblname_str_2 <- paste0('expiry_',format(mindate, format="%Y%m%d"),'_',format(maxdate, format="%Y%m%d") ,'_train1')
tblloc_str_2 <- paste0(projname_str,':',dbname_str,'.',tblname_str_2)

# query_str <- gsub("[\r\n]", " ", paste0("SELECT * FROM  ",dbname_str,'.',tblname_str_1," t WHERE
#   DATE(expiry_date) BETWEEN \"",mindate,"\" AND \"",maxdate,"\" AND renewed_count=1 AND  
#   ABS(HASH(expiry_date)) % 100 >= 10 AND  
#   ABS(HASH(expiry_date)) % 100 < 55"))

# command_str <- paste0("bq query --max_rows=0 --destination_table='", tblloc_str_2,"' '", query_str ,"'")
# cat("Executing command:\n\t", command_str,"\n")
# system(command_str)

# cat("Created BQ table", tblloc_str_2 ,"\n")

eval(call("<-", as.name(tblname_str_2), 
          bq_table_download(bq_project_query(projname_str, 
                                             paste0("SELECT * FROM  ",dbname_str,'.',tblname_str_2)
                                            ))))
saveRDS(get(tblname_str_2),file = paste0(directory,'/',tblname_str_2,'.RDS'), 
        compress=TRUE)

cat("Created RDS table", paste0(directory,'/',tblname_str_2,'.RDS') ,"\n")

expiry_train_df <- get(tblname_str_2)
expiry_train_df <- expiry_train_df %>% 
                filter(!is.na(gibb_score)) %>% # remove where gibb_score, etc. are NA
                mutate (reg_arpt = ifelse(reg_arpt <= 0, 0.0001,reg_arpt), # add necessary columns
                                   log_reg_arpt = log(reg_arpt),
                                   tld_registrar_index = tolower(paste(tld, reseller,sep="")))
# expiry_train_list <- split(expiry_train_df, expiry_train_df$tld_registrar_index)
expiry_train_df <- merge(expiry_train_df, reg_lookup, on="domain_id", all.x=TRUE)
write.csv(expiry_train_df, file = paste0(directory,'/',tblname_str_2,'.csv'), row.names = FALSE)


########################################################################################################################
# CREATE train2 table                                                                                                  #
########################################################################################################################

tblname_str_2 <- paste0('expiry_',format(mindate, format="%Y%m%d"),'_',format(maxdate, format="%Y%m%d") ,'_train2')
tblloc_str_2 <- paste0(projname_str,':',dbname_str,'.',tblname_str_2)

# query_str <- gsub("[\r\n]", " ", paste0("SELECT* FROM ",dbname_str,'.',tblname_str_1," t WHERE
#   DATE(expiry_date) BETWEEN \"",mindate,"\" AND \"",maxdate,"\" AND renewed_count=1 AND  
#   ABS(HASH(expiry_date)) % 100 >= 55 "))

# command_str <- paste0("bq query --max_rows=0 --destination_table='", tblloc_str_2,"' '", query_str ,"'")
# cat("Executing command:\n\t", command_str,"\n")
# system(command_str)

# cat("Created BQ table", tblloc_str_2 ,"\n")

eval(call("<-", as.name(tblname_str_2), 
          bq_table_download(bq_project_query(projname_str, 
                                             paste0("SELECT * FROM  ",dbname_str,'.',tblname_str_2)
                                            ))))
saveRDS(get(tblname_str_2),file = paste0(directory,'/',tblname_str_2,'.RDS'), 
        compress=TRUE)

cat("Created RDS table", paste0(directory,'/',tblname_str_2,'.RDS') ,"\n")

expiry_test_df <- get(tblname_str_2)
expiry_test_df <- expiry_test_df %>% 
                filter(!is.na(gibb_score)) %>% # remove where gibb_score, etc. are NA
                mutate (reg_arpt = ifelse(reg_arpt <= 0, 0.0001,reg_arpt), # add necessary columns
                                   log_reg_arpt = log(reg_arpt),
                                   tld_registrar_index = tolower(paste(tld, reseller,sep="")))
# expiry_test_list <- split(expiry_test_df, expiry_test_df$tld_registrar_index)
expiry_test_df <- merge(expiry_test_df, reg_lookup, on="domain_id", all.x=TRUE)
write.csv(expiry_test_df, file = paste0(directory,'/',tblname_str_2,'.csv'), row.names = FALSE)


########################################################################################################################
# WRITE to GCP cloud                                                                                                   #
########################################################################################################################

# system(paste0("gsutil cp -r ",directory," ", bucket))

# cat("Transfered RDS tables to GCP ", paste0(bucket,'datapull_', format(today, format="%Y%m%d")) ,"\n")

