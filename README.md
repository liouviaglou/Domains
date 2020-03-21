# README

## 20200320

Working on getting orig script npv_script_share to work. Fixing paths, package namespaces (overlapping function names, needing to specify package explicitly, ie dplyr::), 

not clear on mass_predict_first_renewal()'s prediction_list generation... currently applying prediction fucntion on each column of test data. doesn't make sense. needs to aply it for each unique registrar. well, the problem lies in this: test_data_prepped only contains one registrar: namecheap due to mass_prep_data() which  subset's and ends up messing up the rest of the prediction functions because the resulting object is no longer a list of dataframes but just a dataframe. mass_data_prep() takes a long time when its implemented for all registrars, not just namecheap... so.. 

ended up just reading in test_data_prepped RDS.

what is the accuracy of the existing model?