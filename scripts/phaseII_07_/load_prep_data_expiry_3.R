library(data.table)

# TODO:
# 1. make pulldate an arguments to pass to script 
#    (pulldate should be current date, so could just get this programmatically)



########################################################################################################################
#                                                                                                                      #
# LOAD data to BQ table in my project (if table doesn't exist already)
# This will create a new a new table expiry.expiry_20180101_20211231_20210215
# Naming convention for expiry data pulls: expiry_mindate_maxdate_pulldate
#                                                                                                                      #
########################################################################################################################

command_str <-  "bq query --use_legacy_sql=false --destination_table='radix2020:expiry.expiry_20180101_20211231_20210215' --flagfile='expiry_prepped_data.sql' "

system(command_str)

cat("Created BQ table in my project")

########################################################################################################################
#                                                                                                                      #
# PULL 5Q subset, generate 10/45/45% test/train/train split
# https://www.oreilly.com/content/repeatable-sampling-of-data-sets-in-bigquery-for-machine-learning/
#                                                                                                                      #
########################################################################################################################



# copy from gcp
# system("gsutil cp gs://data_outputt/output/expiry_train_prepped_1list/home/jupyter/local/Domains_202003/data/expiry_train_prepped_1list")

# load data
cat("Loading data...")
expiry_df <- readRDS("/home/jupyter/Domains_202003/data/output/expiry_20190601_20200901_20201116_excl")
cat("Loaded", expiry_df %>% nrow(),"rows\n")

# select most recent 5Q [1 quarter = 90 days, 5 quarters = 450 days ]
# 450 days before 20200901 is 20190609 ... round off to 20190601
cat("Removing", expiry_df %>%filter(expiry_date < as.Date("2019-06-01") | expiry_date > as.Date("2020-09-01")) %>% tally() %>% pull(n) ,"rows due to expiry_date constraints\n")
expiry_df <- expiry_df %>% filter(expiry_date >= as.Date("2019-06-01") & expiry_date <= as.Date("2020-09-01"))

# remove renewed_count>1
cat("Removing", expiry_df %>% filter(renewed_count>1) %>% tally() %>% pull(n) ,"rows due to renewed_count constraints\n")
expiry_df <- expiry_df %>% filter(renewed_count==1)

# remove where gibb_score, etc. are NA
cat("Removing", expiry_df %>% filter(is.na(gibb_score)) %>% tally() %>% pull(n) ,"rows due to missing gibb_score\n")
expiry_df <- expiry_df %>% filter(!is.na(gibb_score))
cat("... now dataset min(creation_date) is ", expiry_df %>% summarise(min(creation_date)) %>% pull(1) %>% as.character(),".\n")

# add necessary columns
expiry_df <- expiry_df %>% mutate (reg_arpt = ifelse(reg_arpt <= 0, 0.0001,reg_arpt),
                                   log_reg_arpt = log(reg_arpt),
                                   tld_registrar_index = tolower(paste(tld, reseller,sep="")))

# test/train split 
set.seed(123) 
smp_siz = floor(0.8*nrow(expiry_df))
train_ind = sample(seq_len(nrow(expiry_df)),size = smp_siz) 
expiry_train_df = expiry_df[train_ind,] 
expiry_test_df = expiry_df[-train_ind,]

# split into lists
expiry_list <- split(expiry_df, expiry_df$tld_registrar_index)
expiry_train_list <- split(expiry_train_df, expiry_train_df$tld_registrar_index)
expiry_test_list <- split(expiry_test_df, expiry_test_df$tld_registrar_index)
