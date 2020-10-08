library(stringr)
library(stringi)
library(dplyr)
library(modeest)
library(stringdist)
library(pbapply)
library(RMySQL)
library(gnm)
library(ggplot2)
library(rPython)
library(tidyverse)
library(R.utils)
library(data.table)
library(reshape)
library(lubridate)
library(clipr)
library(reshape2)
library(plotly)
library(tidyr)
library(cluster)
library(dbplyr)



############Build Fallback Tables#######################
npv_historic_renewal_data<-readRDS("./npv_historic_renewal_data")

reseller_am_geo_map<-readRDS("./reseller_am_geo_map.rds")


npv_historic_renewal_data$reseller_am<-
  reseller_am_geo_map$reseller_am[match(npv_historic_renewal_data$reseller,
                                        reseller_am_geo_map$reseller)]
npv_historic_renewal_data$reseller_geo<-
  reseller_am_geo_map$reseller_geo[match(npv_historic_renewal_data$reseller,
                                         reseller_am_geo_map$reseller)]
npv_historic_renewal_data$reseller_geo[is.na(npv_historic_renewal_data$reseller_geo)]<-"Others"
npv_historic_renewal_data$reg_arpt_slab<-cut(npv_historic_renewal_data$reg_arpt_org, 
                                             breaks = c(-Inf,0,0.3,1,3,5,10,15,25,35,Inf),
                                             right = TRUE)



npv_historic_renewal_data$renewed_count[npv_historic_renewal_data$renewed_count >= 3]<-"3+"


my_summary<-npv_historic_renewal_data %>% 
  filter(reg_period > 1) %>% 
  group_by(tld, renewed_count, registrar, reseller, reseller_geo, region) %>% 
  summarise(expiring_domains = n_distinct(domain),
            renewed_domains = n_distinct(domain[renewal_status %in% c("Renewed", "Transfered")])) %>%
  mutate(renewal_rate = round(renewed_domains/expiring_domains,3))


my_summary<-my_summary %>% 
  group_by(tld, renewed_count, reseller_geo, region) %>% 
  summarise(expiring_domains = sum(expiring_domains),
            renewed_domains = sum(renewed_domains)) %>%
  mutate(renewal_rate = round(renewed_domains/expiring_domains,3))

my_summary<-my_summary %>% filter(expiring_domains >= 10) %>%
  mutate(my_index = paste(tld, reseller_geo, sep = ""))


my_fallback_1<-my_summary %>% 
  group_by(renewed_count, tld, region) %>% 
  summarise(expiring_domains = sum(expiring_domains),
            renewed_domains = sum(renewed_domains)) %>%
  mutate(renewal_rate = round(renewed_domains/expiring_domains,3),
         my_fallback_index = paste(tld, region, sep = ""))


my_fallback_2<-my_summary %>%
  group_by(renewed_count, region) %>%
  summarise(expiring_domains = sum(expiring_domains),
            renewed_domains = sum(renewed_domains)) %>%
  mutate(renewal_rate = round(renewed_domains/expiring_domains,3),
         my_fallback_index = paste(region, sep = ""))


save(list = ls()[grepl("my_", ls())], file = "./my_model_tables.rdata")




############################multi year renewal defaults######################



npv_fallback_first_geo_arpt_tld<-npv_historic_renewal_data %>%
  filter(renewed_count == 1) %>%
  group_by(tld, reseller_geo, reg_arpt_slab) %>%
  summarise(expiring_domains = length(domain),
            renewed_domains = length(domain[renewal_status %in% c("Renewed", "Transfered")])) %>%
  mutate(renewal_rate = round(renewed_domains/expiring_domains,3),
         index = paste(tld, reseller_geo, reg_arpt_slab, sep = "")) %>%
  filter(expiring_domains >= 500)




npv_fallback_first_geo_arpt<-npv_historic_renewal_data %>%
  filter(renewed_count == 1) %>%
  group_by(reseller_geo, reg_arpt_slab) %>%
  summarise(expiring_domains = length(domain),
            renewed_domains = length(domain[renewal_status %in% c("Renewed", "Transfered")])) %>%
  mutate(renewal_rate = round(renewed_domains/expiring_domains,3),
         index = paste(reseller_geo, reg_arpt_slab, sep = "")) %>%
  filter(expiring_domains >= 500)



npv_fallback_first_tld_arpt<-npv_historic_renewal_data %>%
  filter(renewed_count == 1) %>%
  group_by(tld, reg_arpt_slab) %>%
  summarise(expiring_domains = length(domain),
            renewed_domains = length(domain[renewal_status %in% c("Renewed", "Transfered")])) %>%
  mutate(renewal_rate = round(renewed_domains/expiring_domains,3),
         index = paste(tld, reg_arpt_slab, sep = "")) %>%
  filter(expiring_domains >= 0)


npv_fallback_second_tld_geo<-npv_historic_renewal_data %>%
  filter(renewed_count == 2) %>%
  group_by(tld, reseller_geo) %>%
  summarise(expiring_domains = length(domain),
            renewed_domains = length(domain[renewal_status %in% c("Renewed", "Transfered")])) %>%
  mutate(renewal_rate = round(renewed_domains/expiring_domains,3),
         index = paste(tld, reseller_geo, sep = "")) %>%
  filter(expiring_domains >= 50)

npv_fallback_second_geo<-npv_historic_renewal_data %>%
  filter(renewed_count == 2) %>%
  group_by(reseller_geo) %>%
  summarise(expiring_domains = length(domain),
            renewed_domains = length(domain[renewal_status %in% c("Renewed", "Transfered")])) %>%
  mutate(renewal_rate = round(renewed_domains/expiring_domains,3)) %>%
  filter(expiring_domains >= 200)


npv_fallback_third_tld_geo<-npv_historic_renewal_data %>%
  filter(renewed_count == "3+" & 
           !(tld %in% c("online", "tech") & 
               creation_date >= "2015-06-01" & 
               creation_date <= "2015-09-30") &
           !(tld == "site" &              
               creation_date >= "2015-06-01" &
               creation_date <= "2015-10-31")) %>%
  group_by(tld, reseller_geo) %>%
  summarise(expiring_domains = length(domain),
            renewed_domains = length(domain[renewal_status %in% c("Renewed", "Transfered")])) %>%
  mutate(renewal_rate = round(renewed_domains/expiring_domains,3),
         index = paste(tld, reseller_geo, sep = "")) %>%
  filter(expiring_domains >= 50)


npv_fallback_third_geo<-npv_historic_renewal_data %>%
  filter(renewed_count == "3+" & 
           !(tld %in% c("online", "tech") & 
               creation_date >= "2015-06-01" & 
               creation_date <= "2015-09-30") &
           !(tld == "site" &              
               creation_date >= "2015-06-01" &
               creation_date <= "2015-10-31")) %>%
  group_by(reseller_geo) %>%
  summarise(expiring_domains = length(domain),
            renewed_domains = length(domain[renewal_status %in% c("Renewed", "Transfered")])) %>%
  mutate(renewal_rate = round(renewed_domains/expiring_domains,3)) %>%
  filter(expiring_domains >= 50)


npv_fallback_first_final<-npv_historic_renewal_data %>%
  filter(renewed_count == 1) %>%
  group_by(region, reg_arpt_slab) %>%
  summarise(expiring_domains = length(domain),
            renewed_domains = length(domain[renewal_status %in% c("Renewed", "Transfered")])) %>%
  mutate(renewal_rate = round(renewed_domains/expiring_domains,3),
         index = paste(region, reg_arpt_slab, sep = ""))


npv_fallback_second_final<-npv_historic_renewal_data %>%
  filter(renewed_count == 2) %>%
  group_by(region) %>%
  summarise(expiring_domains = length(domain),
            renewed_domains = length(domain[renewal_status %in% c("Renewed", "Transfered")])) %>%
  mutate(renewal_rate = round(renewed_domains/expiring_domains,3))


npv_fallback_third_final<-npv_historic_renewal_data %>%
  filter(renewed_count == "3+") %>%
  group_by(region) %>%
  summarise(expiring_domains = length(domain),
            renewed_domains = length(domain[renewal_status %in% c("Renewed", "Transfered")])) %>%
  mutate(renewal_rate = round(renewed_domains/expiring_domains,3))


save(list = ls()[grepl("npv_fallback_", ls())], file = "./npv_fallback_tables.rdata")






#################Applying fallback tables#####################







load(file = "./npv_fallback_tables.rdata")




test_data_op<-test_data_op %>%
  mutate(temp_index = paste(tld, reseller_geo, reg_arpt_slab, sep = ""))

test_data_op$first_renewal_prediction[is.na(test_data_op$first_renewal_prediction)]<-
  npv_fallback_first_geo_arpt_tld$renewal_rate[match(test_data_op$temp_index[is.na(test_data_op$first_renewal_prediction)],
                                                     npv_fallback_first_geo_arpt_tld$index)]



test_data_op<-test_data_op %>%
  mutate(temp_index = paste(reseller_geo, reg_arpt_slab, sep = ""))

test_data_op$first_renewal_prediction[is.na(test_data_op$first_renewal_prediction)]<-
  npv_fallback_first_geo_arpt$renewal_rate[match(test_data_op$temp_index[is.na(test_data_op$first_renewal_prediction)],
                                                 npv_fallback_first_geo_arpt$index)]


test_data_op<-test_data_op %>%
  mutate(temp_index = paste(tld, reg_arpt_slab, sep = ""))

test_data_op$first_renewal_prediction[is.na(test_data_op$first_renewal_prediction)]<-
  npv_fallback_first_tld_arpt$renewal_rate[match(test_data_op$temp_index[is.na(test_data_op$first_renewal_prediction)],
                                                 npv_fallback_first_tld_arpt$index)]



test_data_op<-test_data_op %>%
  mutate(temp_index = paste(tld, reseller_geo, sep = ""))

test_data_op$second_renewal_prediction[is.na(test_data_op$second_renewal_prediction)]<-
  npv_fallback_second_tld_geo$renewal_rate[match(test_data_op$temp_index[is.na(test_data_op$second_renewal_prediction)],
                                                 npv_fallback_second_tld_geo$index)]




test_data_op$second_renewal_prediction[is.na(test_data_op$second_renewal_prediction)]<-
  npv_fallback_second_geo$renewal_rate[match(test_data_op$reseller_geo[is.na(test_data_op$second_renewal_prediction)],
                                             npv_fallback_second_geo$reseller_geo)]



test_data_op<-test_data_op %>%
  mutate(temp_index = paste(tld, reseller_geo, sep = ""))

test_data_op$third_renewal_prediction[is.na(test_data_op$third_renewal_prediction)]<-
  npv_fallback_third_tld_geo$renewal_rate[match(test_data_op$temp_index[is.na(test_data_op$third_renewal_prediction)],
                                                npv_fallback_third_tld_geo$index)]


test_data_op$third_renewal_prediction[is.na(test_data_op$third_renewal_prediction)]<-
  npv_fallback_third_geo$renewal_rate[match(test_data_op$reseller_geo[is.na(test_data_op$third_renewal_prediction)],
                                            npv_fallback_third_geo$reseller_geo)]




test_data_op<-test_data_op %>%
  mutate(temp_index = paste(region, reg_arpt_slab, sep = ""))

test_data_op$first_renewal_prediction[is.na(test_data_op$first_renewal_prediction)]<-
  npv_fallback_first_final$renewal_rate[match(test_data_op$temp_index[is.na(test_data_op$first_renewal_prediction)],
                                              npv_fallback_first_final$index)]



test_data_op$second_renewal_prediction[is.na(test_data_op$second_renewal_prediction)]<-
  npv_fallback_second_final$renewal_rate[match(test_data_op$region[is.na(test_data_op$second_renewal_prediction)],
                                               npv_fallback_second_final$region)]



test_data_op$third_renewal_prediction[is.na(test_data_op$third_renewal_prediction)]<-
  npv_fallback_third_final$renewal_rate[match(test_data_op$region[is.na(test_data_op$third_renewal_prediction)],
                                              npv_fallback_third_final$region)]

test_data_op<-test_data_op %>%
  select(-temp_index)




#############MY Calculations######


load(file = "./my_model_tables.rdata")




test_data_op<-test_data_op %>%
  mutate(my_index = paste(tld, reseller_geo, sep = ""))

my_summary_1<-my_summary %>%
  filter(renewed_count == 1)

test_data_op$first_renewal_prediction[test_data_op$reg_period > 1]<-
  my_summary_1$renewal_rate[match(test_data_op$my_index[test_data_op$reg_period > 1], 
                                  my_summary_1$my_index)] 
rm(my_summary_1)

my_summary_2<-my_summary %>%
  filter(renewed_count == 2)

test_data_op$second_renewal_prediction[test_data_op$reg_period > 1]<-
  my_summary_2$renewal_rate[match(test_data_op$my_index[test_data_op$reg_period > 1], 
                                  my_summary_2$my_index)] 
rm(my_summary_2)


my_summary_3<-my_summary %>%
  filter(renewed_count == "3+")

test_data_op$third_renewal_prediction[test_data_op$reg_period > 1]<-
  my_summary_3$renewal_rate[match(test_data_op$my_index[test_data_op$reg_period > 1], 
                                  my_summary_3$my_index)] 
rm(my_summary_3)

test_data_op<-test_data_op %>%
  select(-my_index)







#######my fallback##########


########## test_data_op is the output from the prediction script##############


test_data_op<-test_data_op %>%
  mutate(my_fallback_index = paste(tld, region, sep = ""))



my_fallback_1_1<-my_fallback_1 %>%
  filter(renewed_count == 1)

test_data_op$first_renewal_prediction[is.na(test_data_op$first_renewal_prediction) & 
                                        test_data_op$reg_period > 1]<-
  my_fallback_1_1$renewal_rate[match(test_data_op$my_fallback_index[is.na(test_data_op$first_renewal_prediction) & 
                                                                      test_data_op$reg_period > 1],
                                     my_fallback_1_1$my_fallback_index)]

rm(my_fallback_1_1)


my_fallback_1_2<-my_fallback_1 %>%
  filter(renewed_count == 2)


test_data_op$second_renewal_prediction[is.na(test_data_op$second_renewal_prediction) & 
                                         test_data_op$reg_period > 1]<-
  my_fallback_1_2$renewal_rate[match(test_data_op$my_fallback_index[is.na(test_data_op$second_renewal_prediction) & 
                                                                      test_data_op$reg_period > 1],
                                     my_fallback_1_2$my_fallback_index)]

rm(my_fallback_1_2)





my_fallback_1_3<-my_fallback_1 %>%
  filter(renewed_count == "3+")

test_data_op$third_renewal_prediction[is.na(test_data_op$third_renewal_prediction) & 
                                        test_data_op$reg_period > 1]<-
  my_fallback_1_3$renewal_rate[match(test_data_op$my_fallback_index[is.na(test_data_op$third_renewal_prediction) & 
                                                                      test_data_op$reg_period > 1],
                                     my_fallback_1_3$my_fallback_index)]

rm(my_fallback_1_3)


test_data_op<-test_data_op %>%
  select(-my_fallback_index)


############final fallback for multi-year###################

my_fallback_2_1<-my_fallback_2 %>%
  filter(renewed_count == 1)

test_data_op$first_renewal_prediction[is.na(test_data_op$first_renewal_prediction) & 
                                        test_data_op$reg_period > 1]<-
  my_fallback_2_1$renewal_rate[match(test_data_op$region[is.na(test_data_op$first_renewal_prediction) & 
                                                           test_data_op$reg_period > 1],
                                     my_fallback_2_1$region)]

rm(my_fallback_2_1)


my_fallback_2_2<-my_fallback_2 %>%
  filter(renewed_count == 2)


test_data_op$second_renewal_prediction[is.na(test_data_op$second_renewal_prediction) & 
                                         test_data_op$reg_period > 1]<-
  my_fallback_2_2$renewal_rate[match(test_data_op$region[is.na(test_data_op$second_renewal_prediction) & 
                                                           test_data_op$reg_period > 1],
                                     my_fallback_2_2$region)]

rm(my_fallback_2_2)



my_fallback_2_3<-my_fallback_2 %>%
  filter(renewed_count == "3+")

test_data_op$third_renewal_prediction[is.na(test_data_op$third_renewal_prediction) & 
                                        test_data_op$reg_period > 1]<-
  my_fallback_2_3$renewal_rate[match(test_data_op$region[is.na(test_data_op$third_renewal_prediction) & 
                                                           test_data_op$reg_period > 1],
                                     my_fallback_2_3$region)]

rm(my_fallback_2_3)






