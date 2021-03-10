library(data.table)

# copy from gcp
# system("gsutil cp gs://data_outputt/output/expiry_train_prepped_1list/home/jupyter/local/Domains_202003/data/expiry_train_prepped_1list")

# Set date of most recent pull
today <- as.Date("2021-02-23")
maxdate <- today - 50
mindate <- maxdate - 456

# Local directory
directory <- paste0('/home/jupyter/Domains_202003/data/output/datapull_', format(today, format="%Y%m%d"), '/')

# Read in train data
cat("Loading train data...")
tblname_str_1 <- paste0('expiry_',format(mindate, format="%Y%m%d"),'_',format(maxdate, format="%Y%m%d") ,'_train1.RDS')
expiry_train_df <- readRDS(paste0(directory, tblname_str_1))
cat("Loaded", expiry_train_df %>% nrow(),"rows\n")

# Read in test data
cat("Loading test data...")
tblname_str_2 <- paste0('expiry_',format(mindate, format="%Y%m%d"),'_',format(maxdate, format="%Y%m%d") ,'_train2.RDS')
expiry_test_df <- readRDS(paste0(directory, tblname_str_2))
cat("Loaded", expiry_test_df %>% nrow(),"rows\n")

# Clean train data
expiry_train_df <- expiry_train_df %>% 
                filter(!is.na(gibb_score)) %>% # remove where gibb_score, etc. are NA
                mutate (reg_arpt = ifelse(reg_arpt <= 0, 0.0001,reg_arpt), # add necessary columns
                                   log_reg_arpt = log(reg_arpt),
                                   tld_registrar_index = tolower(paste(tld, reseller,sep="")))
expiry_train_list <- split(expiry_train_df, expiry_train_df$tld_registrar_index)

# Clean test data
expiry_test_df <- expiry_test_df %>% 
                filter(!is.na(gibb_score)) %>% # remove where gibb_score, etc. are NA
                mutate (reg_arpt = ifelse(reg_arpt <= 0, 0.0001,reg_arpt), # add necessary columns
                                   log_reg_arpt = log(reg_arpt),
                                   tld_registrar_index = tolower(paste(tld, reseller,sep="")))
expiry_test_list <- split(expiry_test_df, expiry_test_df$tld_registrar_index)
