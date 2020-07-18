# time Rscript prep_expiry.R >> /home/jupyter/local/Domains_202003/data/output/prep_expiry.log 2>&1


suppressWarnings(library(dplyr))
suppressWarnings(library(data.table))

# for prep data
suppressWarnings(library(rPython))
suppressWarnings(library(stringr))
suppressWarnings(library(pbapply))
suppressWarnings(library(stringdist))

# system("gsutil cp gs://data_outputt/output/expiry_data /home/jupyter/local/Domains_202003/data/expiry_data")
# expiry_data <- readRDS("/home/jupyter/local/Domains_202003/data/expiry_data")
expiry_train <- readRDS("/home/jupyter/local/Domains_202003/data/expiry_train")
expiry_test <- readRDS("/home/jupyter/local/Domains_202003/data/expiry_test")

# exclude subsequent renewals
expiry_train <- expiry_train %>% filter(renewal_type == 'FirstTime')
expiry_test <- expiry_test %>% filter(renewal_type == 'FirstTime')

# system("gsutil cp gs://data_outputt/output/ranger_03 /home/jupyter/local/Domains_202003/data/ranger_03")
# load('../../data/ranger_03')

source('../orig/functions.R')
python.load("../orig/gibb_detect/gib_detect.py",TRUE)

suppressWarnings(expiry_train_prepped_1list <- mass_prep_data(expiry_train))
suppressWarnings(expiry_test_prepped_1list <- mass_prep_data(expiry_test))


saveRDS(expiry_train_prepped_1list,"../../data/output/expiry_train_prepped_1list")
saveRDS(expiry_test_prepped_1list,"../../data/output/expiry_test_prepped_1list")
system("gsutil cp /home/jupyter/local/Domains_202003/data/output/* gs://data_outputt/output/")