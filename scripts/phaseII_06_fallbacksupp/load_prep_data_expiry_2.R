library(data.table)

# copy from gcp
# system("gsutil cp gs://data_outputt/output/expiry_train_prepped_1list/home/jupyter/local/Domains_202003/data/expiry_train_prepped_1list")

# load data
expiry_df <- readRDS("/home/jupyter/local/Domains_202003/data/output/expiry_20180101_20190331")

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
