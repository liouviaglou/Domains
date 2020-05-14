##############################################
#############              ###################
############# HOUSEKEEPING ###################
#############              ###################
##############################################


library(dplyr)
library(ggplot2)
library(data.table)
library(reshape)
library(lubridate)
library(clipr)
library(reshape2)
library(plotly)
library(tidyr)
library(magrittr)


setwd("/Users/lubagloukhov/Documents/Consulting/Radix/Domains_202003/scripts/newdata_202005")
inputdir <- "../../data/input/newdata_202005/datashare"
outputtdir <- "../../data/output/newdata_202005"

##############################################

# npv_actuals_recalc_2017_2020	
#   A comparison of our probability predictions for 
#   all domains registered  2017-01-01 - 2020-03-31 
#   with the actual renewal status outcomes

##############################################

npv_actuals <- readRDS(file.path(inputdir, 
                                  "npv_actuals_recalc_2017-2020"))
dim(npv_actuals)
# [1] 10645751       26
names(npv_actuals)
summary(npv_actuals$npv_actual)
summary(npv_actuals$npv)

npv_actuals_001= data.frame(npv_actuals[sample(nrow(npv_actuals), 
                                    round(nrow(npv_actuals)*.001)), ])

ggplot(npv_actuals_001, aes(x=npv, y=npv_actual)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(npv_actuals_001, aes(x=npv, y=npv_actual, color=tld)) + 
  geom_point(alpha = 0.25)

table(npv_actuals$tld)
table(npv_actuals$reseller)

# residuals vs fit?
# used to detect non-linearity, unequal error variances, and outliers.
# showing heteroskedasticity
# If you look at this plot, you’ll see that there’s a clear “funneling” phenomenon. The distribution of the residuals is quite well concentrated around 0 for small fitted values, but they get more and more spread out as the fitted values increase. This is an instance of “increasing variance”. The standard linear regression assumption is that the variance is constant across the entire range. When this assumption isn’t valid, such as in this example, we shouldn’t believe our confidence intervals, prediction bands, or the p-values in our regression.
ggplot(npv_actuals_001, aes(y=npv-npv_actual, x=npv, color=tld)) + 
  geom_point(alpha = 0.25)


head(npv_actuals[,c("first_renewal_prediction",
                    "old_first_renewal_prediction")],20)

# Questions:
# variable for actual renewal status outcomes?
# difference b/w npv_actual and npv vars --
#    is one predicted and the other actual>
# zdns_reg, zdns_ren, reseller_am, 
# first_renewal_prediction vs. old_first_renewal_prediction, etc.


##############################################

# renewal_training_data_2020_02_25	
#   training data used to predict first_renewal_probability 
#   in the npv_data_op_refreshed_model dataset

##############################################

renewal_training_data <- readRDS(file.path(inputdir, 
                                           "renewal_training_data_2020_02_25"))
dim(renewal_training_data)
# [1] 5553487      37



##############################################

# npv_data_op_refreshed_model	
#   our predictions based on the model built from  
#   new training data (renewal_training_data_2020_02_25)

##############################################

npv_data_op <- readRDS(file.path(inputdir, 
                                 "npv_data_op_refreshed_model"))
dim(npv_data_op)
# [1] 5122360      43

str(npv_data_op)


# npv_fallback_tables	
#   fallback tables where we dont have enough training 
#   data for a particular tld-registrar combination

npv_fallback_names <- load(file.path(inputdir, 
                                  "npv_fallback_tables"))
npv_fallback_names
# [1] "npv_fallback_first_final"        "npv_fallback_first_geo_arpt"    
# [3] "npv_fallback_first_geo_arpt_tld" "npv_fallback_first_tld_arpt"    
# [5] "npv_fallback_second_final"       "npv_fallback_second_geo"        
# [7] "npv_fallback_second_tld_geo"     "npv_fallback_third_final"       
# [9] "npv_fallback_third_geo"          "npv_fallback_third_tld_geo" 

dim(npv_fallback_first_geo_arpt_tld)
View(npv_fallback_first_geo_arpt_tld)

# Questions:
# not enough training means we also don't have "day_domains"?
# npv_fallback_first_final, _geo_arpt? renewal rate by country & buy price
# npv_fallback_first_final$reg_arpt_slab? reg_arpt: the price at which the domain is bought by the reseller 

# Using a decision tree means you don't need to retrain a model but we can do better than simply using a "fall back table" -- can we confirm this with historical data?


##############################################

# my_model_tables	
#   fallback tables for multi year domain registrations 
#   for which we generally dont have enough data

##############################################


my_model_names <- load(file.path(inputdir, 
                                 "my_model_tables"))
my_model_names
# [1] "my_fallback_1" "my_fallback_2" "my_summary" 

# renewal rate for multi-year by region (C/nC) & tld
dim(my_fallback_1)
View(my_fallback_1)

ggplot(my_fallback_1, aes(y=renewal_rate, x=renewed_count, 
                          color=region)) + 
  geom_point(alpha = 0.25)

ggplot(my_fallback_1, aes(y=renewal_rate, x=renewed_count, 
                          color=region)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16,
             outlier.size=2, notch=FALSE)



# renewal rate for multi-year by region (C/nC) 
dim(my_fallback_2)
View(my_fallback_2)

ggplot(my_fallback_2, aes(y=renewal_rate, x=renewed_count, 
                          color=region)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE)

##############################################

# npv_historic_renewal_data	
#   historic data for domains which have gone through 
#   renewal cycles to calculate the fallback tables


##############################################

npv_historic_ren <- readRDS(file.path(inputdir, 
                                      "npv_historic_renewal_data"))
dim(npv_historic_ren)
# [1] 4212458      28

View(npv_historic_ren)

str(npv_historic_ren)
table(npv_historic_ren$region)
table(npv_historic_ren$reseller_country)
table(npv_historic_ren$tld)

# for each reseller_countryXtld, calculate dtree estimate
# first, mass_prep_data (test_data_prepped<-mass_prep_data(test_data))
# then feed to decision tree. compare to actual (renewal_status)


##############################################

# second_renewal_model_simplified	
#   a simplified reference table to calculate second 
#   renewal probability

##############################################


second_renewal_model <- readRDS(file.path(inputdir, 
                                      "second_renewal_model_simplified"))
dim(second_renewal_model)
# [1] 443   7
