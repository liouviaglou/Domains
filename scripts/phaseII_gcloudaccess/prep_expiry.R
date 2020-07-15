# time Rscript prep_expiry.R >> /home/jupyter/local/Domains_202003/data/output/prep_expiry.log 2>&1
# real    142m47.879s
# user    190m34.112s
# sys     4m36.136s      Killed

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

# system("gsutil cp gs://data_outputt/output/ranger_03 /home/jupyter/local/Domains_202003/data/ranger_03")
# load('../../data/ranger_03')

source('../orig/functions.R')
python.load("../orig/gibb_detect/gib_detect.py",TRUE)

# suppressWarnings(expiry_train_prepped <- prep_domain_data(expiry_train))
# suppressWarnings(expiry_test_prepped <- prep_domain_data(expiry_test))

# dim(expiry_train)
# dim(expiry_train_prepped)
# dim(expiry_test)
# dim(expiry_test_prepped)

# [1] 348672     27
# [1] 348591     39


# saveRDS(expiry_train_prepped,"../../data/output/expiry_train_prepped")
# saveRDS(expiry_test_prepped,"../../data/output/expiry_test_prepped")
# system("gsutil cp /home/jupyter/local/Domains_202003/data/output/* gs://data_outputt/output/")



# mass_prep_data
# suppressWarnings(expiry_train_prepped <- prep_domain_data(expiry_train))
suppressWarnings(expiry_test_prepped_2 <- mass_prep_data(expiry_test))

# dim(expiry_train)
# dim(expiry_train_prepped)
dim(expiry_test)
dim(expiry_test_prepped_2)

# [1] 348672     27
# NULL


# saveRDS(expiry_train_prepped,"../../data/output/expiry_train_prepped")
saveRDS(expiry_test_prepped_2,"../../data/output/expiry_test_prepped_2")
system("gsutil cp /home/jupyter/local/Domains_202003/data/output/* gs://data_outputt/output/")
