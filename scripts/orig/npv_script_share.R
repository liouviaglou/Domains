############Library Includes##########
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
library(magrittr)

#############Needs Python###############
getwd()


python.load("./gibb_detect/gib_detect_train.py",TRUE)


###############Source Functions#####################

source('./functions.R')
source('./functions_models.R')

###############Read files#####################

renewal_training_data<-readRDS("../../data/input/npv/renewal_training_data")
renewal_training_data_first<-readRDS("../../data/input/npv/renewal_training_data_first")
renewal_price_map<-readRDS("../../data/input/npv/renewal_price_map")
standard_renewal_prices<-readRDS("../../data/input/npv/standard_renewal_prices")


test_data<-readRDS("../../data/input/npv/test_data")

###############Summary files#####################

# summary(renewal_training_data)
# renewal_type       renewed_count       expiry_date           domain_id           domain          creation_date       
# Length:2242496     Length:2242496     Min.   :2018-01-01   Min.   :  949901   Length:2242496     Min.   :2012-10-12  
# Class :character   Class :character   1st Qu.:2018-09-20   1st Qu.:49230248   Class :character   1st Qu.:2017-07-06  
# Mode  :character   Mode  :character   Median :2019-04-19   Median :61849610   Mode  :character   Median :2018-02-26  
# Mean   :2019-03-14   Mean   :61280273                      Mean   :2017-12-28  
# 3rd Qu.:2019-10-01   3rd Qu.:76908236                      3rd Qu.:2018-09-18  
# Max.   :2019-12-31   Max.   :89298007                      Max.   :2018-12-31  
# 
# status              tld             registrar           reseller         reseller_country      region         
# Length:2242496     Length:2242496     Length:2242496     Length:2242496     Length:2242496     Length:2242496    
# Class :character   Class :character   Class :character   Class :character   Class :character   Class :character  
# Mode  :character   Mode  :character   Mode  :character   Mode  :character   Mode  :character   Mode  :character  
# 
# 
# 
# 
# reg_period       renewal_status     renew_type        autorenew_type       renew_date         renew_registrar   
# Min.   :1.000   Not Renewd:1836453   Length:2242496     Length:2242496     Min.   :2014-01-11   Length:2242496    
# 1st Qu.:1.000   Renewed   : 398111   Class :character   Class :character   1st Qu.:2018-09-16   Class :character  
# Median :1.000   Transfered:   7932   Mode  :character   Mode  :character   Median :2019-04-17   Mode  :character  
# Mean   :1.011                                                              Mean   :2019-03-10                     
# 3rd Qu.:1.000                                                              3rd Qu.:2019-09-29                     
# Max.   :6.000                                                              Max.   :2020-02-14                     
# NA's   :24087                          
#  renew_reseller      reg_revenue         reg_arpt        renew_period   renew_domain_revenue   renew_arpt    
#  Length:2242496     Min.   :-35.000   Min.   :-35.000   Min.   :1.000   Min.   :  0.00       Min.   :  0.00  
#  Class :character   1st Qu.:  0.440   1st Qu.:  0.440   1st Qu.:1.000   1st Qu.:  8.00       1st Qu.:  8.00  
#  Mode  :character   Median :  0.500   Median :  0.500   Median :1.000   Median : 15.00       Median : 15.00  
#                     Mean   :  3.403   Mean   :  3.245   Mean   :1.007   Mean   : 16.05       Mean   : 16.04  
#                     3rd Qu.:  1.500   3rd Qu.:  1.500   3rd Qu.:1.000   3rd Qu.: 20.00       3rd Qu.: 20.00  
#                     Max.   :295.000   Max.   : 65.000   Max.   :9.000   Max.   :441.00       Max.   :250.00  
#                                                         NA's   :24087   NA's   :7329         NA's   :24087   
# reg_arpt_org     tld_registrar_index
# Min.   :-44.490   Length:2242496     
# 1st Qu.:  0.440   Class :character   
# Median :  0.500   Mode  :character   
# Mean   :  1.192                      
# 3rd Qu.:  1.000                      
# Max.   : 65.000  


#########################################Build Models#########################################
renewal_training_data$renewal_status<-as.factor(renewal_training_data$renewal_status)
summary(as.factor(renewal_training_data$renewal_type))
summary(as.factor(renewal_training_data$renew_type))
#######Build First Renewal Model##############
renewal_training_data_first<-renewal_training_data %>%
  dplyr::filter(renewal_type == "FirstTime")

renewal_training_data_first<-mass_prep_data(renewal_training_data_first)

first_renewal_model<-mass_build_model_first_renewal(renewal_training_data_first)
#############################build second renewal model##############

renewal_training_data_second<-renewal_training_data %>% 
  dplyr::filter(renewal_type == "Second")

second_renewal_model<-mass_build_model_second_renewal(split(renewal_training_data_second, 
                                                            renewal_training_data_second$tld_registrar_index))


#########################Build a simplified third renewal reference table###############

third_renewal_model<-renewal_training_data %>%
  filter(renewal_type == "Subsequent") %>%
  group_by(tld, registrar, reseller) %>%
  summarise(expiring_domains = length(domain),
            renewed_domains = length(domain[renewal_status %in% c("Renewed", "Transfered")])) %>%
  mutate(renewal_rate = round(renewed_domains/expiring_domains,3),
         tld_registrar_index = paste(tld, reseller, sep = ""))

##############predict first renewal##############

test_data_prepped<-mass_prep_data(test_data)

test_data_op<-mass_predict_first_renewal(test_data_prepped, first_renewal_model)


test_data_op<-mass_predict_second_renewal(split(test_data_op, 
                                             test_data_op$tld_registrar_index), 
                                       second_renewal_model)

test_data_op$third_renewal_prediction<-third_renewal_model$renewal_rate[match(test_data_op$tld_registrar_index,
                                                                              third_renewal_model$tld_registrar_index)]

test_data_op<-test_data_op %>%
  mutate(renewal_price_index = paste(tld, registrar, reseller, sep = ""))

test_data_op$projected_renew_arpt<-renewal_price_map$renewal_arpt[
  match(test_data_op$renewal_price_index, 
        renewal_price_map$renewal_price_index)]

test_data_op<-test_data_op %>%
  select(-renewal_price_index)

test_data_op<-test_data_op %>%
  mutate(std_renewal_price_index = paste(tld, region, sep = ""))

standard_renewal_prices<-readRDS("/home/radmin/npv_project/standard_renewal_prices")
test_data_op$projected_renew_arpt[is.na(test_data_op$projected_renew_arpt)]<-
  standard_renewal_prices$price[match(test_data_op$std_renewal_price_index[is.na(test_data_op$projected_renew_arpt)], 
                                      standard_renewal_prices$index)]



test_data_op$zdns_reg<-0
test_data_op$zdns_ren<-0

test_data_op$zdns_reg[test_data_op$region == "China"]<-0.15
test_data_op$zdns_ren[test_data_op$region == "China"]<-0.15


test_data_op<-get_df_npv(test_data_op)

rm(standard_renewal_prices)
rm(renewal_price_map)
rm(second_renewal_model)
rm(third_renewal_model)



saveRDS(test_data_op, "./test_data_op")

write_csv(renewal_training_data, "./luba_gloukhova_docket/renewal_training_data.csv")

