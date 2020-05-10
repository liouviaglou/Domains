###############Housekeeping#####################
library(car)
library(olsrr)

library(party)
library(partykit)
library(rpart)
library(caret) 
library(e1071)

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

getwd()
setwd("/Users/lubagloukhov/Documents/Consulting/Radix/Domains_202003/scripts/dtree")

###############Source Functions#####################

source('../orig/functions.R')
source('../orig/functions_models.R')

python.load("../orig/gibb_detect/gib_detect_train.py",TRUE)

###############Read files#####################

# renewal_training_data<-readRDS("../../data/input/npv/renewal_training_data")
# renewal_training_data_first<-readRDS("../../data/input/npv/renewal_training_data_first")
# renewal_price_map<-readRDS("../../data/input/npv/renewal_price_map")
# standard_renewal_prices<-readRDS("../../data/input/npv/standard_renewal_prices")
# 
# 
# test_data<-readRDS("../../data/input/npv/test_data")
# test_data_prepped<-readRDS("../../data/input/npv/test_data_prepped")
# test_data_op<-readRDS("../../data/input/npv/test_data_op")
train_data <- readRDS("../../data/output/npv/first_renewal_preds")

first_renewal_model <- readRDS("../../data/output/npv/first_renewal_model")

############Verify Assumptions##################
# http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/

# 1. The outcome is a binary or dichotomous variable like yes vs no, positive vs negative, 1 vs 0.
# Yup! 

summary(train_data$renewal_status)
# Not Renewd    Renewed 
# 1729907     219175 

# 2. There is a linear relationship between the logit of the outcome and each predictor variables -- visually inspecting the scatter plot between each predictor and the logit values.

predictors <- c("pattern_domain_count", 
                "log_reg_arpt",
                "sld_length", 
                "gibb_score",
                "sld_type", 
                "day_domains",
                "reg_period")
predicted <- "first_renewal_prediction"
response <- "renewal_status"

# Subset only relevant columns
# .05% of the data for plotting
mydata <- subset(train_data,,c(predictors,predicted,response)) 
mydata_sub <- mydata[sample(.N,nrow(mydata)*.0005)]

# All numeric except for  sld_type (and response var) which is a factor
sapply(mydata, is.numeric) 
sapply(mydata, is.factor) 


# Bind the logit and tidying the data for plot
mydata_sub <- mydata_sub %>%
  mutate(logit = log(first_renewal_prediction/(1-first_renewal_prediction))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
# Warning message: attributes are not identical across measure variables; they will be dropped 



# Plot
png(file = "../../data/output/dtree/ass_2_20200507.png")
output.ass_2 <- ggplot(filter(mydata_sub, predictors %in% predictors), 
                       aes(logit, predictor.value))+
  geom_point()+#(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")
plot(output.ass_2)
dev.off()

# Verify w/ B-T text
mydata$first_renewal_logodds = log(mydata$first_renewal_prediction/(1-mydata$first_renewal_prediction))
predictors_num <- predictors[sapply(subset(mydata,,predictors) , is.numeric)]
as.formula(paste("first_renewal_logodds", 
                 paste(predictors_num, collapse=" + "),  sep=" ~ "))

boxTidwell(first_renewal_logodds ~ pattern_domain_count, data=mydata)
# MLE of lambda Score Statistic (z)  Pr(>|z|)    
# 0.3077              224.03 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# iterations =  4 

boxTidwell(first_renewal_logodds ~ log_reg_arpt, data=mydata)
# Error in boxTidwell.default(y, X1, X2, max.iter = max.iter, tol = tol,  : 
# the variables to be transformed must have only positive values
# > summary(mydata$log_reg_arpt)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -9.2103 -0.8210 -0.6931 -0.8255 -0.5276  4.0775 
log_reg_arpt_10 = mydata$log_reg_arpt+10
boxTidwell(mydata$first_renewal_logodds ~ log_reg_arpt_10)
# MLE of lambda Score Statistic (z)  Pr(>|z|)    
# 5.3227              534.06 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# iterations =  26 

boxTidwell(first_renewal_logodds ~ sld_length, data=mydata)
# Error in lm.fit(cbind(1, x.log.x, x1.p, x2), y, ...) : NA/NaN/Inf in 'x'
summary(mydata$sld_length)

boxTidwell(first_renewal_logodds ~ gibb_score, data=mydata)
# Error in boxTidwell.default(y, X1, X2, max.iter = max.iter, tol = tol,  : 
#                               the variables to be transformed must have only positive values
summary(mydata$gibb_score)
gibb_score_001 = mydata$gibb_score+.00001
boxTidwell(mydata$first_renewal_logodds ~ gibb_score_001)
# MLE of lambda Score Statistic (z)  Pr(>|z|)    
# -20.997             -436.79 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# iterations =  26 

boxTidwell(first_renewal_logodds ~ day_domains, data=mydata)
# MLE of lambda Score Statistic (z)  Pr(>|z|)    
# 0.59841              264.24 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# iterations =  3 
boxTidwell(first_renewal_logodds ~ reg_period, data=mydata)
# MLE of lambda Score Statistic (z)  Pr(>|z|)    
# -2.9359             -58.527 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# iterations =  5 


# The statistically significant score tests indicate that transformations are needed for both variables. The MLE of lambda suggests that 
# reg_period should be transformed by a power of -3 
# day_domains should be transformed by a power of .6
# pattern_domain_count should be transformed by a power of .3
# log_reg_arpt_10 = (log_reg_arpt + 10) should be transformed by a power of 5
# gibb_score_001 = (gibb_score + .00001) should be transformed by a power of -21

# You need other methods to build the model such as including 2 or 3-power terms, fractional polynomials and spline function (Chapter @ref(polynomial-and-spline-regression)).


# 3. There is no influential values (extreme values or outliers) in the continuous predictors. Influential values are extreme individual data points that can alter the quality of the logistic regression model. The most extreme values in the data can be examined by visualizing the Cook’s distance values. Here we label the top 3 largest values:
names(first_renewal_model)
var = "websitegmo"
summary(first_renewal_model[[var]]$residuals)
sum(is.na(first_renewal_model[[var]]$residuals))

summary_glm = summary(first_renewal_model[[var]])
round( 1 - ( summary_glm$deviance / summary_glm$null.deviance ), 2 )


summary(first_renewal_model[[var]]$df.residuals)
sum(is.na(first_renewal_model[[var]]$residuals))
# only 3% of the variance is explained 

plot(first_renewal_model[[var]])#, which = 4, id.n = 3)
cooks.distance(first_renewal_model[[var]])
outlierTest(first_renewal_model[[var]])

# Note that, not all outliers are influential observations. To check whether the data contains potential influential observations, the standardized residual error can be inspected. Data points with an absolute standardized residuals above 3 represent possible outliers and may deserve closer attention.
# 
# The following R code computes the standardized residuals (.std.resid) and the Cook’s distance (.cooksd) using the R function augment() [broom package].

# Extract model results
model.data <- augment(model) %>% 
  mutate(index = 1:n()) 

# The data for the top 3 largest values, according to the Cook’s distance, can be displayed as follow:
model.data %>% top_n(3, .cooksd)

# Plot the standardized residuals:
png(file = "../../data/output/dtree/ass_3_20200507.png",
    width = 3000, height = 750,)
output.ass_3 <- ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = diabetes), alpha = .5) +
  theme_bw()
plot(output.ass_3)
dev.off()

# Filter potential influential data points with abs(.std.res) > 3:
model.data %>% 
  filter(abs(.std.resid) > 3)

# When you have outliers in a continuous predictor, potential solutions include:
# - Removing the concerned records
# - Transform the data into log scale
# - Use non parametric methods


# 4. There is no high intercorrelations (i.e. multicollinearity) among the predictors.

# Multicollinearity corresponds to a situation where the data contain highly correlated predictor variables. Read more in Chapter @ref(multicollinearity).

# Multicollinearity is an important issue in regression analysis and should be fixed by removing the concerned variables. It can be assessed using the R function vif() [car package], which computes the variance inflation factors:

names_list <- names(first_renewal_model)
problems <- c("hostgmo", "storegmo")
names_list <- names_list[names_list != "hostgmo"]
names_list <- names_list[names_list != "storegmo"]

for (var in names_list){
  print(var)
  test_df = as.data.frame(car::vif(first_renewal_model[[var]], 
                                   singular.ok=TRUE))
  print(test_df)
}
>10 "pwgo daddy" sld_type

  
## pregnant  glucose pressure  triceps  insulin     mass pedigree      age 
##     1.89     1.38     1.19     1.64     1.38     1.83     1.03     1.97

# As a rule of thumb, a VIF value that exceeds 5 or 10 indicates a problematic amount of collinearity. In our example, there is no collinearity: all variables have a value of VIF well below 5.


# Accuracy fallacy

train <- readRDS("../../data/output/dtree/train")
test <- readRDS("../../data/output/dtree/test")

test_df = rbindlist(test)
prop.table( table( test_df$renewal_status ) )

