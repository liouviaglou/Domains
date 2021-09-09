# Rscript --vanilla radix_bq_to_local.R myriad-303821 expiry 2021-01-04 2021-06-30
# Rscript --vanilla radix_bq_to_local.R myriad-303821 expiry 2021-07-01 2021-08-18
# Rscript --vanilla radix_bq_to_local.R myriad-303821 expiry 2017-01-01 2022-08-22

# This is a modified version of load_prep_data_expiry_3.R for the Elder team. 
# It retrieves the dp data pull from radix, and uses that to create the train1, train2,
# and test files



library(data.table)
library(bigrquery)
library(dplyr)
options(scipen = 20)

args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)<4) {
  stop("Four arguments must be supplied for BQ table creation: project name, database name, min date, and max date", call.=FALSE)
}

projname_str <- args[1]
dbname_str <- args[2]
min_date <- args[3]
max_date <- args[4]

# projname_str <- 'radix2020'
# dbname_str <- 'expiry'
# query_file <- 'expiry_prepped_data.sql'

# Make date arguments dates
mindate <- as.Date(min_date)
maxdate <- as.Date(max_date)

# CREATE local dir for data (to be pushed in its entirety to GCP storage)
directory <- paste0('../../data/output/dp_datapull_', format(maxdate, format="%Y%m%d"))
dir.create(directory, showWarnings = FALSE, recursive = TRUE)

# DEFINE GCP Storage bucket for wiritng tables
bucket <- "gs://myriad_bucket/output/"

# # Read in regional lookup
# reg_lookup <- bq_table_download(bq_project_query("radix2020", 
#                                 paste0("SELECT * FROM  expiry.regionlookup_20180101_20211231_20210311")))
# reg_lookup <- unique(reg_lookup)
# tblloc_str_2 = bq table full name
# tblname_str_2 = variable name and name of csv file

download_from_radix_bq <- function(tbl_suffix, mindate, maxdate, bucket,
                                   directory, projname_str, dbname_str,
                                   bypass_bq_download=TRUE) {
    tbl_name <- paste0('expiry_',format(mindate, format="%Y%m%d"),'_',format(maxdate, format="%Y%m%d"), "_", tbl_suffix)
    tblloc <- paste0(projname_str,':',dbname_str,'.',tbl_name)
    query_str <- gsub("[\r\n]", " ", paste0("SELECT * FROM prediction_vendors.base_expiry_dp_data_v2 t WHERE
      DATE(expiry_date) BETWEEN \"",mindate,"\" AND \"",maxdate,"\" AND renewed_count=1 "))
    
    command_str <- paste0("bq query --max_rows=0 --project_id='radixbi-249015' --destination_table='", tblloc,"' '", query_str ,"'")
    cat("Executing command:\n\t", command_str,"\n")
    system(command_str)

    cat("Created BQ table", tblloc ,"\n")

    if (bypass_bq_download) {  # bq_table_download has trouble with rate limits for me
        # BQ to Cloud
        dest <- paste0(bucket, tbl_name, "-*.csv")
#         cat("Transferring from BQ to ", dest,"on Google Cloud\n")
#         bq_table_save(bq_project_query(projname_str, paste0("SELECT * FROM  ",dbname_str,'.',tbl_name)
#                                                     ), destination_uris = dest, destination_format = "CSV")
#         cat("Successfully transferred from BQ to Cloud\n\n")

#         # Cloud to Local
#         command_str <- paste0("gsutil cp -r ",dest," ", directory)
#         cat("Executing command:\n\t", command_str,"\n")
#         system(command_str)
#         cat("Completed downloading from Cloud to Local\n")

        # Read in from Local
        file_names <- list.files(path=directory, pattern=paste0(tbl_name, "-.*.csv"))
        file_paths <- paste0(directory, "/", file_names)
        cat("Reading in File Names: ", file_names, "\n")
#         eval(call("<-", as.name(tbl_name), 
#                   read.csv(paste0(directory, "/", tbl_name, ".csv"))))
        expiry_test_df <- do.call(rbind,lapply(file_paths,read.csv))
    } else {

#         eval(call("<-", as.name(tbl_name), 
#                   bq_table_download(bq_project_query(projname_str, 
#                                                      paste0("SELECT * FROM  ",dbname_str,'.',tbl_name), 
#                                                      priority="BATCH"), page_size=10000)))
        expiry_test_df <- bq_table_download(bq_project_query(projname_str, 
                                                     paste0("SELECT * FROM  ",dbname_str,'.',tbl_name), 
                                                     priority="BATCH"), page_size=10000)
    }

#     expiry_test_df <- get(tbl_name)
    expiry_test_df <- expiry_test_df %>% 
                    filter(!is.na(gibb_score)) %>% # remove where gibb_score, etc. are NA
                    mutate (reg_arpt = ifelse(reg_arpt <= 0, 0.0001,reg_arpt), # add necessary columns
                                       log_reg_arpt = log(reg_arpt),
                                       tld_registrar_index = paste(tld, reseller,sep=""))
    # expiry_test_df <- merge(expiry_test_df, reg_lookup, on="domain_id", all.x=TRUE)
    saveRDS(expiry_test_df, ,file = paste0(directory,'/',tbl_name,'.RDS'), compress=TRUE)
    cat("Created RDS table", paste0(directory,'/',tbl_name,'.RDS') ,"\n")
    write.csv(expiry_test_df, file = paste0(directory,'/',tbl_name,'.csv'), row.names = FALSE)
}

download_from_radix_bq("test", mindate=mindate, maxdate=maxdate,
                       bucket, directory, projname_str, dbname_str)
