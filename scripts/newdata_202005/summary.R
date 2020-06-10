require(scales)

# p-hacking
# number of segments, vars/seg, obs/seg in initial training data (already mass_prepped)
renewal_training_data_first <- readRDS("../../data/input/npv/renewal_training_data_first")
length(renewal_training_data_first)
obs_seg <- sapply(renewal_training_data_first,function(x) dim(x)[1])
summary(obs_seg)
ggplot(data.frame(obs_seg), aes(x=obs_seg)) + 
  geom_histogram() +
  labs(x="number of observations per segment", y = "count")+ 
  scale_x_continuous(labels = comma)


first_renewal_model <- readRDS("../../data/output/npv/first_renewal_model")
first_renewal_model["hostgo daddy"]["hostgo daddy"]
first_renewal_model["fungmo"] # 7 actual variables (one factor variable) used

# evaluating model performance
# evaluate test scenario provided by Parag on Flock
first_renewal_model_test_predict <- readRDS("../../data/output/dtree/first_renewal_model_test_predict")
renew_date
first_renewal_model_test_predict_sub <- first_renewal_model_test_predict %>% 
  filter((tld=="online" & registrar == "go daddy"))
first_renewal_model_test_predict_sub_summ <- first_renewal_model_test_predict_sub %>%
  mutate(month = format(expiry_date, "%m"), year = format(expiry_date, "%Y")) %>%
  group_by(month, year) %>%
  summarise(predicted = sum(first_renewal_prediction),
            actual = sum(renewal_status=="Renewed"),
            error_dom = (round(predicted-actual)),
            accuracy_1 = 1-error_dom/actual)
data.frame(first_renewal_model_test_predict_sub_summ)

# comparing to standard accuracy
first_renewal_model_test_predict_sub_summ2 <- first_renewal_model_test_predict_sub %>%
  mutate(first_renewal_predictionTF = ifelse(first_renewal_prediction > 0.5, "Renewed", "Not Renewd"),
         first_renewal_correct = ifelse(renewal_status==first_renewal_predictionTF, 1, 0),
         month = format(expiry_date, "%m"), year = format(expiry_date, "%Y")) %>%
  group_by(month, year) %>%
  summarise(error_dom = sum(first_renewal_correct==0),
            count = length(first_renewal_correct),
            accuracy_2 = 1-error_dom/count)
data.frame(first_renewal_model_test_predict_sub_summ2)

# compare -- scatter & box plot
first_renewal_model_test_predict_sub_summJ <- left_join(first_renewal_model_test_predict_sub_summ, 
                                                        first_renewal_model_test_predict_sub_summ2, 
                                                        by = c("month","year"))
# wtf? months w/ higher Radix accuracy measures tend to have lower statistical accuracy measures
ggplot(first_renewal_model_test_predict_sub_summJ, aes(x=accuracy_1,y=accuracy_2))+
  geom_point()  +
  labs(y = "Radix accuracy", x="traditional accuracy" )
dat.m <- melt(first_renewal_model_test_predict_sub_summJ ,id.vars=c('year','month'), 
              measure.vars=c('accuracy_1','accuracy_2'))
ggplot(dat.m, aes(y=value, x=variable))+
  geom_boxplot()  + scale_x_discrete(labels=c("Radix accuracy", "traditional accuracy"))

# evauate Radix accuracy across the entire dataset
first_renewal_model_test_predict_summ <- first_renewal_model_test_predict %>%
  mutate(month = format(expiry_date, "%m"), year = format(expiry_date, "%Y")) %>%
  group_by(month, year, tld, registrar) %>%
  summarise(predicted = sum(first_renewal_prediction),
            actual = sum(renewal_status=="Renewed"),
            error_dom = abs(round(predicted-actual)),
            accuracy_1 = 1-error_dom/actual)
data.frame(first_renewal_model_test_predict_summ)

first_renewal_model_test_predict_summ2 <- first_renewal_model_test_predict %>%
  mutate(first_renewal_predictionTF = ifelse(first_renewal_prediction > 0.5, "Renewed", "Not Renewd"),
         first_renewal_correct = ifelse(renewal_status==first_renewal_predictionTF, 1, 0),
         month = format(expiry_date, "%m"), year = format(expiry_date, "%Y")) %>%
  group_by(month, year, tld, registrar) %>%
  summarise(error_dom = sum(first_renewal_correct==0),
            count = length(first_renewal_correct),
            accuracy_2 = 1-error_dom/count)
data.frame(first_renewal_model_test_predict_sub_summ2)

# compare -- scatter & box plot
first_renewal_model_test_predict_summJ <- left_join(first_renewal_model_test_predict_summ, 
                                                        first_renewal_model_test_predict_summ2, 
                                                        by = c("month","year","tld","registrar"))
summary(first_renewal_model_test_predict_summJ$accuracy_1)
summary(first_renewal_model_test_predict_summJ$accuracy_2)
# wtf? months w/ higher Radix accuracy measures tend to have lower statistical accuracy measures
ggplot(first_renewal_model_test_predict_summJ, aes(x=accuracy_1,y=accuracy_2))+
  geom_point()  +
  labs(y="traditional accuracy", x = "Radix accuracy")
dat.m2 <- melt(first_renewal_model_test_predict_summJ ,id.vars=c('year','month'), 
              measure.vars=c('accuracy_1','accuracy_2'))
ggplot(dat.m2, aes(y=value, x=variable))+
  geom_boxplot()  + scale_x_discrete(labels=c("Radix accuracy", "traditional accuracy"))

# ROC
library(plotROC)
ggROC01 <- ggplot(first_renewal_model_test_predict,
       aes(m = first_renewal_prediction, d = renewal_status))+ 
  geom_roc(n.cuts=20,labels=FALSE) +
  style_roc(theme = theme_grey) 

calc_auc(ggROC01)
    

# ROC for decision tree model
load("../../data/output/dtree2/outputtree_07")
load("../../data/output/dtree2/outputtree_04")

# prediction & confuson matrix -- test dataset
t_predict_07 <- predict(outputtree_07, test_df, type = "prob")
t_predict_04 <- predict(outputtree_04, test_df, type = "prob")

dtree_df <- data.frame(data.frame(t_predict_07)$Renewed, 
                       data.frame(t_predict_04)$Renewed, 
                       first_renewal_model_test_predict$first_renewal_prediction,
                       first_renewal_model_test_predict$renewal_status)

ggROC02 <- ggplot(dtree_df,
)+ 
  geom_roc(n.cuts=20,labels=FALSE,
           aes(m = data.frame.t_predict_07..Renewed, 
               d = first_renewal_model_test_predict.renewal_status)) +
  style_roc(theme = theme_grey) 

calc_auc(ggROC02)

ggROC03 <- ggplot(dtree_df,
)+ 
  geom_roc(n.cuts=20,labels=FALSE,
           aes(m = data.frame.t_predict_04..Renewed, 
               d = first_renewal_model_test_predict.renewal_status)) +
  style_roc(theme = theme_grey) 

calc_auc(ggROC03)

ggplot(dtree_df)+ 
  geom_roc(n.cuts=20,labels=FALSE,
           aes(m = data.frame.t_predict_04..Renewed, 
               d = first_renewal_model_test_predict.renewal_status,
               color="red"))+
  geom_roc(n.cuts=20,labels=FALSE,
           aes(m = data.frame.t_predict_07..Renewed, 
               d = first_renewal_model_test_predict.renewal_status,
               color="green"))+
  geom_roc(n.cuts=20,labels=FALSE,
           aes(m = first_renewal_model_test_predict.first_renewal_prediction, 
               d = first_renewal_model_test_predict.renewal_status,
               color="blue")) +
  style_roc(theme = theme_grey) 

# calculate lift
# top 5% most likely to renew (based on predicted prob)
# that may contain 15% of  renewals 
# so this model has a lift of 3

head(dtree_df)
names(dtree_df) <- c("dtree_w", "dtree", "seg_glm", "actual")


dim(dtree_df)
dim(subset(dtree_df,dtree_df$actual=="Renewed")) #44100 obs
var <- "seg_glm"
perc <-  .05
dtree_df_01 <- dtree_df[order(dtree_df[var],
                              decreasing = TRUE),][1:round(dim(dtree_df)[1]*perc),]
dim(subset(dtree_df_01,dtree_df_01$actual=="Renewed"))[1]/(dim(subset(dtree_df,dtree_df$actual=="Renewed"))[1])/perc


# dtree_w & dtree: top 5% of those most likely to renew contain 18.30612% of all renewals ... giving a lift of 3.66
# seg_glm: top 5% of thos emost likely to renew contain 19.12925% of all renewals... giving a lift of 3.83
