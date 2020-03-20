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

#############Needs Python###############
python.load("./gibb_detect/gib_detect_train.py",TRUE)

############Misc Functions##############

na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}


intersect2 <- function (x, y)
{
  y <- as.vector(y)
  y[match(as.vector(x), y, 0L)]
}

change.colnames<-function(temp.data, curr.name, new.name)
{
  colnames(temp.data)[colnames(temp.data) == curr.name] <-new.name
  return(temp.data)  
}

unzip.file<-function(){
  tar.file<-file.choose()
  gunzip(tar.file,remove=FALSE)
  
}

save_object<-function(obj) {
  saveRDS(obj, paste("/home/radmin/npv_project/backup_data/", deparse(substitute(obj)), Sys.time(), sep = ""))
}



newreg_data_prep<-function(newreg_data) {
  newreg_data<-newreg_data_rename(newreg_data)
  newreg_data<-newreg_data_select(newreg_data)
  return(newreg_data)
}

newreg_data_rename<-function(newreg_data){
  names(newreg_data)<-c("creation_date", "domain_id", "domain", "status","mbg", "tld", "registrar", "reseller",
                        "reseller_country", "region", "reg_period", "reg_revenue","reg_arpt", "first_renewal_prediction", 
                        "sld_length", "sld_type","sld_type2", "day_domains", "logarpt", "gibb_score", "coeff_variation", "gross_profit",
                        "gd_less_icann_fixed")
  newreg_data$creation_date<-as.Date(newreg_data$creation_date, "%Y-%m-%d")
  newreg_data$reg_period<-as.integer(newreg_data$reg_period)
  newreg_data$first_renewal_prediction<-as.numeric(newreg_data$first_renewal_prediction)
  newreg_data$sld_length<-as.integer(newreg_data$sld_length)
  newreg_data$day_domains<-as.integer(newreg_data$day_domains)
  newreg_data$logarpt<-as.numeric(newreg_data$logarpt)
  newreg_data$gibb_score<-as.numeric(newreg_data$gibb_score)
  newreg_data$coeff_variation<-as.numeric(newreg_data$coeff_variation)
  newreg_data$sld_type<-factor(newreg_data$sld_type, exclude = "\\N")
  newreg_data$sld_type2<-factor(newreg_data$sld_type2, exclude = "\\N")
  newreg_data$registrar<-tolower(newreg_data$registrar)
  newreg_data$reseller<-tolower(newreg_data$reseller)
  newreg_data$reg_arpt_org<-newreg_data$reg_arpt
  newreg_data$renewal_status<-"Not Renewd"
  return(newreg_data)
}



newreg_data_select<-function(newreg_data) {
  newreg_data<-newreg_data %>% select(domain_id, domain, creation_date, status,mbg, tld, registrar, reseller, reseller_country, region, reg_period, 
                                      reg_revenue, reg_arpt, first_renewal_prediction, sld_length, sld_type, gibb_score, coeff_variation,
                                      reg_arpt_org, renewal_status)
}



get_renewal_status<-function(renewal_type, autorenew_type){
  if(renewal_type == "renewal" |
     renewal_type == "transfer" |
     autorenew_type == "realized") {
    return("Renewed")
  } else {
    return("Not Renewd")
  }
}




get_domain_type<-function(domain_data) {
  domain_data$sld<-substr(domain_data$domain, 1, regexpr("\\.", domain_data$domain)-1)
  domain_data$sld_type<-"ln"
  domain_data$sld_type[!grepl('[[:alpha:]]', domain_data$sld)]<-"n"
  domain_data$sld_type[!grepl('[[:digit:]]', domain_data$sld)]<-"l"
  domain_data$sld_type[grepl("-" , domain_data$sld)] <- "hyphen-l"
  domain_data$sld_type[grepl("-" , domain_data$sld) & grepl('[[:digit:]]', domain_data$sld)] <- "hyphen-ln"
  domain_data$sld_type[grepl("xn--", domain_data$sld)] <- "idn"
  domain_data$sld_length<-str_length(domain_data$sld)
  domain_data$sld_type2<-str_c(domain_data$sld_length,domain_data$sld_type)
  return(domain_data)
}

basic_prep_domain_data<-function(domain_data) {
  domain_data<-get_domain_type(domain_data)
  domain_data$renewal_status[domain_data$renewal_status == "Transfered"]<-"Renewed"
  domain_data$renewal_status<-as.factor(domain_data$renewal_status)
  domain_data$tld<-as.factor(domain_data$tld)
  domain_data$sld_type<-as.factor(domain_data$sld_type)
  
  domain_data<-domain_data %>%
    mutate(day_domains_index = paste(creation_date,
                                     reg_arpt_org, sep = ""))
  
  day_domains_data<-domain_data %>% 
    group_by(creation_date,
             reg_arpt_org) %>% 
    summarise(day_domains = n_distinct(domain)) %>%
    mutate(day_domains_index = paste(creation_date, 
                                     reg_arpt_org, sep = ""))
  domain_data$day_domains<-
    day_domains_data$day_domains[match(domain_data$day_domains_index, 
                                       day_domains_data$day_domains_index)]
  domain_data<-domain_data %>%
    select(-day_domains_index)
  
  domain_data$reg_arpt[domain_data$reg_arpt <= 0]<-0.0001
  domain_data$log_reg_arpt<-log(domain_data$reg_arpt)
  domain_data$tld_registrar_index<-tolower(paste(domain_data$tld,
                                                 domain_data$reseller,sep=""))
  return(domain_data)
}


prep_domain_data<-function(domain_data) {
  domain_data<-basic_prep_domain_data(domain_data)
  python.load("/home/radmin/npv_project/gibb_detect/gib_detect.py",TRUE)
  #Get Gibberish Scores for all SLDs
  
  domain_data$gibb_score<-mapply(python.call, "gibberish_test", domain_data$sld)
  
  #Get Coeff-Variation on all days
  domain_data<-pblapply(split(domain_data, domain_data$creation_date), 
                        function(i) get_cluster_large(i, 0.3, 30000))
  domain_data<-rbindlist(domain_data)
  domain_data$gibb_score<-round(domain_data$gibb_score*100,2)
  
  return(domain_data)
}





get_pattern_score<-function(date_data) {
  pattern_summary<-date_data %>%
    group_by(pattern) %>%
    summarise(domain_count = length(unique(domain)))
  
  day_domains<-length(unique(date_data$domain))
  
  pattern_summary<-pattern_summary %>%
    mutate(pattern_score = domain_count/(day_domains*day_domains))
  
  date_data$pattern_score<-pattern_summary$pattern_score[match(date_data$pattern,
                                                               pattern_summary$pattern)]
  date_data$pattern_domain_count<-pattern_summary$domain_count[match(date_data$pattern,
                                                                     pattern_summary$pattern)]
  date_data$day_domains<-day_domains
  
  
  date_data$pattern_score<-date_data$pattern_score
  return(date_data)
  
}

get_cluster_large<-function(date_data, cluster_granularity, split_chunk_size) {
  date_data$sld<-substr(date_data$domain, 1, regexpr("\\.", date_data$domain)-1)
  big_cluster_data<-date_data
  big_cluster_data<-big_cluster_data[0,]
  for(i in seq(1,nrow(date_data), split_chunk_size)){
    chunk_data<-date_data %>% filter(row_number()>=i, row_number()<=(i+ split_chunk_size -1))
    chunk_data<-get_cluster_data(chunk_data, cluster_granularity)
    big_cluster_data<-rbind(big_cluster_data, chunk_data)
  }
  
  big_cluster_data<-get_pattern_score(big_cluster_data)
  return(big_cluster_data)
}


get_cluster_data<-function(chunk_data, cluster_granularity){
  #create clusters
  chunk_data$sld<-substr(chunk_data$domain, 1, regexpr("\\.", chunk_data$domain)-1)
  
  print(paste("length is ", length(chunk_data$sld), sep = ":"))
  if(length(chunk_data$sld) < 2) {
    chunk_data$pattern<-chunk_data$sld
    chunk_data$cluster<-1
    return(chunk_data)
  }
  
  distance_matrix <- stringdistmatrix(chunk_data$sld,
                                      chunk_data$sld,method = "jw")
  rownames(distance_matrix) <- chunk_data$sld
  
  hc <- hclust(as.dist(distance_matrix))
  #cut the tree
  dfClust <- data.frame(chunk_data$sld, 
                        cutree(hc, 
                               h=cluster_granularity))
  names(dfClust) <- c('sld','cluster')
  chunk_data$cluster<-dfClust$cluster[match(chunk_data$sld,
                                            dfClust$sld)]
  #detect patterns and add them
  pattern_data<-chunk_data %>% 
    group_by(cluster) %>% 
    summarise(sld = n_distinct(sld))
  
  pattern_data$pattern<-0
  for (x in pattern_data$cluster){
    temp_pattern<-chunk_data %>% 
      filter(cluster == x)
    pattern_data$pattern[match(x, 
                               pattern_data$cluster)]<-
      paste(Reduce(intersect2, 
                   strsplit(temp_pattern$sld, NULL)), 
            collapse = '')
  }
  chunk_data$pattern<-pattern_data$pattern[match(chunk_data$cluster,
                                                 pattern_data$cluster)]
  return(chunk_data)
}



mass_prep_data<-function(domain_data) {
  domain_data$tld_registrar_index<-tolower(paste(domain_data$tld, 
                                                 domain_data$reseller,sep=""))
  tld_registrar_data<-split(domain_data, 
                            domain_data$tld_registrar_index)
  prepared_data<-pblapply(tld_registrar_data,
                          prep_domain_data)
  #prepared_data<-rbindlist(prepared_data)
  return(prepared_data)
}

###############################Build Models###################################
##########first renewal model###########

mass_build_model_first_renewal<-function(prepared_data){
  data_models<-lapply(prepared_data, 
                      function(i) build_model_first_renewal(as.data.frame(i)))
  return(data_models)
}


build_model_first_renewal<-function(train_data){
  print(paste("TLD-Registrar",
              train_data$tld_registrar_index[1]))
  print(paste("Renewal Levels",
              nlevels(train_data$renewal_status)))
  train_data$renewal_status<-factor(train_data$renewal_status)
  train_data$sld_type<-factor(train_data$sld_type)
  if(nlevels(train_data$renewal_status) < 2 ) {
    print(paste("Less Renewal Status Levels",train_data$tld_registrar_index[1]))
    print(paste("Total Levels Returning NA",nlevels(train_data$renewal_status)))
    return(NA)
  }
  print(paste("SLD Type Levels",nlevels(train_data$sld_type)))
  ifelse(nlevels(train_data$sld_type) < 2, 
         build_data<-subset(train_data,
                            select=c(renewal_status,
                                     pattern_domain_count, 
                                     log_reg_arpt, 
                                     sld_length, 
                                     day_domains,
                                     gibb_score,
                                     reg_period)),  
         build_data<-subset(train_data,
                            select=c(renewal_status,
                                     pattern_domain_count, 
                                     log_reg_arpt, 
                                     sld_length, 
                                     sld_type, 
                                     day_domains, 
                                     gibb_score,
                                     reg_period))) 
  ########################reduced model##########################################
  #ifelse(nlevels(train.data$SLD.Type) < 2, build.data<-subset(train.data,select=c(Renewal.Status,Coeff.Variation, SLD.Length, Day.Domains)),  build.data<-subset(train.data,select=c(Renewal.Status,Coeff.Variation, SLD.Length, SLD.Type, Day.Domains))) 
  ###############################reduced model#####################################
  #build.data<-subset(train.data,select=c(Renewal.Status,logarpt))
  model<-glm(renewal_status ~.,
             family=binomial(link='logit'),
             data=build_data, 
             y = FALSE, model = FALSE)
  return(model)
}






predict_first_renewal<-function(test_data, model){
  test_data$sld_type[!(test_data$sld_type %in% model$xlevels$sld_type)]<-NA
  
  #test.data$probabilities <- predict(model,newdata=subset(test.data,select=c(Coeff.Variation, LogArpt, SLD.Length, SLD.Type, Day.Domains, Gibb.Score)),type='response');
  test_data$probabilities<-predict(model,
                                   newdata=subset(test_data,
                                                  select=c(pattern_domain_count, 
                                                           log_reg_arpt,
                                                           sld_length, 
                                                           gibb_score,
                                                           sld_type, 
                                                           day_domains,
                                                           reg_period)),type='response');
  
  test_data$first_renewal_prediction[test_data$Status == "Deleted"]<-0
  test_data$first_renewal_prediction<-round(test_data$first_renewal_prediction,3)
  return(test_data)
}




list_predict_first_renewal<-function(tld_registrar,test_data, data_models) {
  ########BEGIN:for substitute TLD#######################
  #registrar.name<-test.data[[tld.registrar]]$`Client Shortname`[1]
  #new.tld.registrar<-paste("site_",registrar.name,sep="")
  #new.tld.registrar<-gsub("\\s+","_",new.tld.registrar)
  #model<-data.models[[new.tld.registrar]]
  
  ########END:for substitute TLD#######################
  tld_registrar_data<-test_data[[tld_registrar]]
  model<-data_models[[tld_registrar]]
  if(is.null(tld_registrar_data) | is.null(model) | is.logical(tld_registrar_data) | is.logical(model)) { return(NA)}
  # tld.registrar.data$SLD.Type[tld.registrar.data$SLD.Type == "IDN"]<-NA
  
  tld_registrar_data<-predict_first_renewal(tld_registrar_data,model)
  return(tld_registrar_data)
}



mass_predict_first_renewal<-function(test_data, data_models) {
  tld_registrar_list<-names(test_data)
  prediction_list<-pblapply(tld_registrar_list, function(i) list_predict_first_renewal(i, test_data, data_models))
  prediction_list<-prediction_list[!is.na(prediction_list)]
  prediction_op<-rbindlist(prediction_list)
  test_data<-rbindlist(test_data)
  test_data$first_renewal_prediction<-prediction_op$probabilities[match(test_data$domain_id,
                                                                        prediction_op$domain_id)]
  return(test_data)
}



mass_build_model_second_renewal<-function(second_renewal_train_data) {
  data_models<-pblapply(second_renewal_train_data, 
                        build_model_second_renewal)
}


build_model_second_renewal<-function(second_renewal_data) {
  model<-glm(renewal_status~reg_arpt_org, 
             family = binomial(link = 'logit'), 
             data = second_renewal_data %>% 
               select(renewal_status, 
                      reg_arpt_org), 
             model = FALSE)
}



predict_second_renewal<-function(test_data, renewal_model) {
  test_data$second_renewal_prediction<-round(predict(renewal_model, 
                                                     newdata = subset(test_data, 
                                                                      select = c(reg_arpt_org)), 
                                                     type='response'),3)
  return(test_data)
}


mass_predict_second_renewal<-function(second_renewal_test_data, second_renewal_model) {
  tld_registrar_list<-names(second_renewal_test_data)
  prediction_list<-pblapply(tld_registrar_list, 
                            function(i) list_predict_second_renewal(i, second_renewal_test_data, 
                                                                    second_renewal_model))
  prediction_list<-prediction_list[!is.na(prediction_list)]
  prediction_op<-rbindlist(prediction_list)
  second_renewal_test_data<-rbindlist(second_renewal_test_data)
  second_renewal_test_data$second_renewal_prediction<-prediction_op$second_renewal_prediction[match(second_renewal_test_data$domain_id,
                                                                                                    prediction_op$domain_id)]
  return(second_renewal_test_data)
} 

list_predict_second_renewal<-function(tld_registrar, second_renewal_test_data, second_renewal_model) {
  tld_registrar_data<-second_renewal_test_data[[tld_registrar]]
  model<-second_renewal_model[[tld_registrar]]
  if(is.null(tld_registrar_data) | 
     is.null(model) | 
     is.logical(tld_registrar_data) | 
     is.logical(model)) 
  { return(NA)}
  tld_registrar_data<-predict_second_renewal(tld_registrar_data,model)
  return(tld_registrar_data)
}





get_domain_npv<-function(regArpt=0,
                         renArpt=0, 
                         firstRenewal=0, 
                         secondRenewal=0, 
                         thirdRenewal =0, 
                         period = 1,
                         zdns_reg = 0, 
                         zdns_ren = 0) {
  
  if(is.na(renArpt)) {return(NA)}
  if(regArpt == 0) {
    techOpsCostReg<-0
  } else {
    techOpsCostReg<-min(regArpt*0.2, 0.45)
  }
  techOpsCostReg<-techOpsCostReg + 0.045 + zdns_reg
  techOpsCostReg<-techOpsCostReg*period
  
  
  if(renArpt == 0){
    techOpsCostRen<-0
  } else {
    
    techOpsCostRen<-min(renArpt*0.2,0.45)
  }
  techOpsCostRen<-techOpsCostRen + 0.045 + zdns_ren
  
  ICANNCost<-0.25
  
  npv<-(regArpt - techOpsCostReg - ICANNCost*period + 
          (renArpt - techOpsCostRen - ICANNCost)*firstRenewal/(1.15^period)  + 
          ((renArpt - techOpsCostRen - ICANNCost)*firstRenewal*secondRenewal/(1.15^(period+1)))/(1 - (thirdRenewal/1.15)))
  
  return(npv)
}


get_df_npv<-function(df) {
  
  df$npv<-round(mapply(get_domain_npv, df$reg_revenue, df$projected_renew_arpt, df$first_renewal_prediction, 
                       df$second_renewal_prediction, df$third_renewal_prediction, df$reg_period, df$zdns_reg, df$zdns_ren),2)
  return(df)
  
}




apply_standard_adjustments<-function(revision_template, standard_adjustments) {
  revision_template<-revision_template %>%
    mutate(standard_adjustment_index = paste(tld, reseller, registrar, reg_period, sep = ""))
  
  match_index<-match(revision_template$standard_adjustment_index, 
                     standard_adjustments$standard_adjustment_index, 
                     nomatch = 0)
  revision_template$revised_first_renewal_rate[match_index != 0 ]<-standard_adjustments$revised_first_renewal_rate[match_index]
  revision_template$revised_second_renewal_rate[match_index != 0 ]<-standard_adjustments$revised_second_renewal_rate[match_index]
  revision_template$revised_third_renewal_rate[match_index != 0 ]<-standard_adjustments$revised_third_renewal_rate[match_index]
  revision_template$revised_mean_ren_arpt[match_index != 0 ]<-standard_adjustments$revised_mean_ren_arpt[match_index]
  return(revision_template)
}





###############Read files#####################

renewal_training_data<-readRDS("./luba_gloukhova_docket/renewal_training_data")
renewal_training_data_first<-readRDS("./luba_gloukhova_docket/renewal_training_data_first")
renewal_price_map<-readRDS("./luba_gloukhova_docket/renewal_price_map")
standard_renewal_prices<-readRDS("./luba_gloukhova_docket/standard_renewal_prices")




#########################################Build Models#########################################
renewal_training_data$renewal_status<-as.factor(renewal_training_data$renewal_status)
#######Build First Renewal Model##############
renewal_training_data_first<-renewal_training_data %>%
  filter(renewal_type == "FirstTime")

renewal_training_data_first<-mass_prep_data(renewal_training_data_first)

first_renewal_model<-mass_build_model_first_renewal(renewal_training_data_first)
#############################build second renewal model##############

renewal_training_data_second<-renewal_training_data %>% 
  filter(renewal_type == "Second")

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

