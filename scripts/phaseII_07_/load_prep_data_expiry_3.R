# Rscript --vanilla load_prep_data_expiry_3.R radix2020 expiry expiry_prepped_data.sql

library(data.table)

# Script automatically pulls most recently available, accurate 5Q worth of data 
# & splits it into a 10/45/45 test/train/train split
# output are 3 BQ tables pertaining to the split, in the current project
# intermediate 

# TODO:


args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)<3) {
  stop("Two arguments must be supplied for BQ table creation: project name, database name, query file loc", call.=FALSE)
}

projname_str <- args[1]
dbname_str <- args[2]
query_file <- args[3] 


########################################################################################################################
#                                                                                                                      #
# LOAD data to BQ table in my project (if table doesn't exist already)
# This will create a new a new table expiry.expiry_20180101_20211231_20210215
# Naming convention for expiry data pulls: expiry_mindate_maxdate_pulldate
#                                                                                                                      #
########################################################################################################################

today <- Sys.Date()

tblname_str <- paste0('expiry_20180101_20211231_',format(today, format="%Y%m%d"))
tablename_str <- paste0(projname_str,':',dbname_str,'.',tblname_str)

command_str <-  paste0("bq query --use_legacy_sql=false --destination_table='",
                       tablename_str,"' --flagfile='",query_file,"' ")

system(command_str)

cat("Created BQ table", tablename_str ,"\n")

########################################################################################################################
#                                                                                                                      #
# PULL 5Q (456 day) subset, generate 10/45/45% test/train/train split
# https://www.oreilly.com/content/repeatable-sampling-of-data-sets-in-bigquery-for-machine-learning/
#                                                                                                                      #
########################################################################################################################

maxdate <- today - 50
mindate <- maxdate - 456
mindate
maxdate



########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################

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
