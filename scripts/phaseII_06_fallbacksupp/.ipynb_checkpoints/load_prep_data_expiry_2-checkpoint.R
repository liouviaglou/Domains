library(data.table)

# copy from gcp
# system("gsutil cp gs://data_outputt/output/expiry_train_prepped_1list/home/jupyter/local/Domains_202003/data/expiry_train_prepped_1list")

# load data
cat("Loading data...")
expiry_df <- readRDS("/home/jupyter/local/Domains_202003/data/output/expiry_20190601_20200901")
cat("Loaded", expiry_df %>% nrow(),"rows\n")

# select most recent 5Q [1 quarter = 90 days, 5 quarters = 450 days ]
# 450 days before 20200901 is 20190609 ... round off to 20190601
cat("Removing", expiry_df %>%filter(expiry_date < as.Date("2019-06-01") | expiry_date > as.Date("2020-09-01")) %>% tally() %>% pull(n) ,"rows due to expiry_date constraints\n")
expiry_df <- expiry_df %>% filter(expiry_date >= as.Date("2019-06-01") & expiry_date <= as.Date("2020-09-01"))

# remove renewed_count>1
cat("Removing", expiry_df %>% filter(renewed_count==1) %>% tally() %>% pull(n) ,"rows due to renewed_count constraints\n")
expiry_df <- expiry_df %>% filter(renewed_count==1)

# remove where gibb_score, etc. are NA
cat("Removing", expiry_df %>% filter(is.na(gibb_score)) %>% tally() %>% pull(n) ,"rows due to missing gibb_score\n")
expiry_df <- expiry_df %>% filter(!is.na(gibb_score))
cat("... now dataset min(creation_date) is ", expiry_df %>% summarise(min(creation_date)) %>% pull(1) %>% as.character(),".\n")

# remove where tld is .pw and .in.net (added 20201103)
cat("Removing", expiry_df %>% filter(tld=="pw" | tld=="in.net") %>% tally() %>% pull(n) ,"rows due to tld pw and in.net\n")
expiry_df <- expiry_df %>% filter(tld!="pw" & tld!="in.net") 
cat("Remaining", expiry_df %>% nrow() ,"rows\n")

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
