
###############################Build Models###################################
##########first renewal model###########

mass_build_model_first_renewal<-function(prepared_data){
  data_models<-lapply(prepared_data, 
                      function(i) build_model_first_renewal(as.data.frame(i)))
  return(data_models)
}

# LVG added the following for predicting at the registrar level (including tld as a predictor)
# mass_build_model_first_renewal_reg<-function(prepared_data){
#   data_models<-lapply(prepared_data, 
#                       function(i) build_model_first_renewal_reg(as.data.frame(i)))
#   return(data_models)
# }


build_model_first_renewal<-function(train_data){
#   print(paste("TLD-Registrar",
#               train_data$tld_registrar_index[1]))
#   print(paste("Renewal Levels",
#               nlevels(train_data$renewal_status)))
  train_data$renewal_status<-factor(train_data$renewal_status)
  train_data$sld_type<-factor(train_data$sld_type)
  if(nlevels(train_data$renewal_status) < 2 ) {
#     print(paste("Less Renewal Status Levels",train_data$tld_registrar_index[1]))
#     print(paste("Total Levels Returning NA",nlevels(train_data$renewal_status)))
    return(NA)
  }
#   print(paste("SLD Type Levels",nlevels(train_data$sld_type)))
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

# LVG added the following for predicting at the registrar level (including tld as a predictor)
build_model_first_renewal_reg <-function(train_data){
#   print(paste("TLD-Registrar",
#               train_data$tld_registrar_index[1]))
#   print(paste("Renewal Levels",
#               nlevels(train_data$renewal_status)))
  train_data$renewal_status<-factor(train_data$renewal_status)
  train_data$sld_type<-factor(train_data$sld_type)
  if(nlevels(train_data$renewal_status) < 2 ) {
#     print(paste("Less Renewal Status Levels",train_data$tld_registrar_index[1]))
#     print(paste("Total Levels Returning NA",nlevels(train_data$renewal_status)))
    return(NA)
  }
#   print(paste("SLD Type Levels",nlevels(train_data$sld_type)))
  ifelse(nlevels(train_data$sld_type) < 2, 
         build_data<-subset(train_data,
                            select=c(renewal_status,
                                     pattern_domain_count, 
                                     log_reg_arpt, 
                                     sld_length, 
                                     day_domains,
                                     gibb_score,
                                     reg_period, tld)),  
         build_data<-subset(train_data,
                            select=c(renewal_status,
                                     pattern_domain_count, 
                                     log_reg_arpt, 
                                     sld_length, 
                                     sld_type, 
                                     day_domains, 
                                     gibb_score,
                                     reg_period, tld))) 
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
                      
# LVG added the following for predicting at the aggregate level (including tld & registrar as a predictor)
build_model_first_renewal_agg <-function(train_data){
#   print(paste("TLD-Registrar",
#               train_data$tld_registrar_index[1]))
#   print(paste("Renewal Levels",
#               nlevels(train_data$renewal_status)))
  train_data$renewal_status<-factor(train_data$renewal_status)
  train_data$sld_type<-factor(train_data$sld_type)
  if(nlevels(train_data$renewal_status) < 2 ) {
#     print(paste("Less Renewal Status Levels",train_data$tld_registrar_index[1]))
#     print(paste("Total Levels Returning NA",nlevels(train_data$renewal_status)))
    return(NA)
  }
#   print(paste("SLD Type Levels",nlevels(train_data$sld_type)))
  ifelse(nlevels(train_data$sld_type) < 2, 
         build_data<-subset(train_data,
                            select=c(renewal_status,
                                     pattern_domain_count, 
                                     log_reg_arpt, 
                                     sld_length, 
                                     day_domains,
                                     gibb_score,
                                     reg_period, tld, reseller)),  
         build_data<-subset(train_data,
                            select=c(renewal_status,
                                     pattern_domain_count, 
                                     log_reg_arpt, 
                                     sld_length, 
                                     sld_type, 
                                     day_domains, 
                                     gibb_score,
                                     reg_period, tld, reseller))) 
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

# LVG added the following for predicting at the aggregate level (including tld & registrar as a predictor plus custom vars)
build_model_first_renewal_agg_plus <-function(train_data){
#   print(paste("TLD-Registrar",
#               train_data$tld_registrar_index[1]))
#   print(paste("Renewal Levels",
#               nlevels(train_data$renewal_status)))
  train_data$renewal_status<-factor(train_data$renewal_status)
  train_data$sld_type<-factor(train_data$sld_type)
  if(nlevels(train_data$renewal_status) < 2 ) {
#     print(paste("Less Renewal Status Levels",train_data$tld_registrar_index[1]))
#     print(paste("Total Levels Returning NA",nlevels(train_data$renewal_status)))
    return(NA)
  }
#   print(paste("SLD Type Levels",nlevels(train_data$sld_type)))
  ifelse(nlevels(train_data$sld_type) < 2, 
         build_data<-subset(train_data,
                            select=c(renewal_status,
                                     pattern_domain_count, 
                                     log_reg_arpt, 
                                     sld_length, 
                                     day_domains,
                                     gibb_score,
                                     reg_period, 
                                     tld, reseller,
                                    reg_durY,mday,wday,qday,yday)),  
         build_data<-subset(train_data,
                            select=c(renewal_status,
                                     pattern_domain_count, 
                                     log_reg_arpt, 
                                     sld_length, 
                                     sld_type, 
                                     day_domains, 
                                     gibb_score,
                                     reg_period, 
                                     tld, reseller,
                                    reg_durY,mday,wday,qday,yday
                                    ))) 
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
  
    
  if (is.na(model) | is.null(test_data)){
      test_data$probabilities<- rep(NA, dim(test_data)[1])
  }else{
      test_data$sld_type[!(test_data$sld_type %in% model$xlevels$sld_type)]<-NA
      test_data$probabilities<-predict(model,
                                   newdata=subset(test_data,
                                                  select=c(pattern_domain_count, 
                                                           log_reg_arpt,
                                                           sld_length, 
                                                           gibb_score,
                                                           sld_type, 
                                                           day_domains,
                                                           reg_period)),type='response');
  }
  
  #test.data$probabilities <- predict(model,newdata=subset(test.data,select=c(Coeff.Variation, LogArpt, SLD.Length, SLD.Type, Day.Domains, Gibb.Score)),type='response');
  
  # had to comment out the following to get predition list to work
  test_data$first_renewal_prediction[test_data$Status == "Deleted"]<-0
#   test_data$first_renewal_prediction<-round(test_data$first_renewal_prediction,3)
  test_data$first_renewal_prediction<-round(test_data$probabilities,3)
  return(test_data)
}

# LVG added for registrar level predictions (with tld as a predictor)
predict_first_renewal_reg<-function(test_data, model){
  
  if (is.na(model) | is.null(test_data)){
      test_data$probabilities<- rep(NA, dim(test_data)[1])
  }else{
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
                                                           reg_period, tld)),type='response');
      }
  # had to comment out the following to get predition list to work
#   test_data$first_renewal_prediction[test_data$Status == "Deleted"]<-0
#   test_data$first_renewal_prediction<-round(test_data$first_renewal_prediction,3)
  # made the following mods
  test_data$first_renewal_prediction<-round(test_data$probabilities,3)
  test_data$first_renewal_prediction[test_data$Status == "Deleted"]<-0
  return(test_data)
}
                      

# LVG added for agg predictions (with tld & registrar as a predictors)
predict_first_renewal_agg<-function(test_data, model){
  test_data$sld_type[!(test_data$sld_type %in% model$xlevels$sld_type)]<-NA
  test_data$reseller[!(test_data$reseller %in% model$xlevels$reseller)]<-NA # LVG added
  
  #test.data$probabilities <- predict(model,newdata=subset(test.data,select=c(Coeff.Variation, LogArpt, SLD.Length, SLD.Type, Day.Domains, Gibb.Score)),type='response');
  test_data$probabilities<-predict(model,
                                   newdata=subset(test_data,
                                                  select=c(pattern_domain_count, 
                                                           log_reg_arpt,
                                                           sld_length, 
                                                           gibb_score,
                                                           sld_type, 
                                                           day_domains,
                                                           reg_period, 
                                                           tld, reseller)),type='response');
  # had to comment out the following to get predition list to work
#   test_data$first_renewal_prediction[test_data$Status == "Deleted"]<-0
#   test_data$first_renewal_prediction<-round(test_data$first_renewal_prediction,3)
  # made the following mods
  test_data$first_renewal_prediction<-round(test_data$probabilities,3)
  test_data$first_renewal_prediction[test_data$Status == "Deleted"]<-0
  return(test_data)
}
                      
# LVG added for agg predictions  (including tld & registrar as a predictor plus custom vars)
predict_first_renewal_agg_plus<-function(test_data, model){
  test_data$sld_type[!(test_data$sld_type %in% model$xlevels$sld_type)]<-NA
  test_data$reseller[!(test_data$reseller %in% model$xlevels$reseller)]<-NA # LVG added
  
  #test.data$probabilities <- predict(model,newdata=subset(test.data,select=c(Coeff.Variation, LogArpt, SLD.Length, SLD.Type, Day.Domains, Gibb.Score)),type='response');
  test_data$probabilities<-predict(model,
                                   newdata=subset(test_data,
                                                  select=c(pattern_domain_count, 
                                                           log_reg_arpt,
                                                           sld_length, 
                                                           gibb_score,
                                                           sld_type, 
                                                           day_domains,
                                                           reg_period, 
                                                           tld, reseller,
                                                           reg_durY,mday,wday,qday,yday)),type='response');
  # had to comment out the following to get predition list to work
#   test_data$first_renewal_prediction[test_data$Status == "Deleted"]<-0
#   test_data$first_renewal_prediction<-round(test_data$first_renewal_prediction,3)
  # made the following mods
  test_data$first_renewal_prediction<-round(test_data$probabilities,3)
  test_data$first_renewal_prediction[test_data$Status == "Deleted"]<-0
  return(test_data)
}
                      


list_predict_first_renewal<-function(tld_registrar,test_data, data_models) {
#   cat(tld_registrar)
  ########BEGIN:for substitute TLD#######################
  #registrar.name<-test.data[[tld.registrar]]$`Client Shortname`[1]
  #new.tld.registrar<-paste("site_",registrar.name,sep="")
  #new.tld.registrar<-gsub("\\s+","_",new.tld.registrar)
  #model<-data.models[[new.tld.registrar]]
#   print(tld_registrar)
  ########END:for substitute TLD#######################
  tld_registrar_data<-test_data[[tld_registrar]]
  model<-data_models[[tld_registrar]]
    
    # LVG removed the following and replaced with NA-fill in predict_first_renewal()
#   if(is.null(tld_registrar_data) | is.null(model) | is.logical(tld_registrar_data) | is.logical(model)) { return(NA)}
    
    
  # tld.registrar.data$SLD.Type[tld.registrar.data$SLD.Type == "IDN"]<-NA
  
  tld_registrar_data<-predict_first_renewal(tld_registrar_data,model)
  return(tld_registrar_data)
}

# test_data = test_data_prepped
# data_models = first_renewal_model 

mass_predict_first_renewal<-function(test_data, data_models) {
  tld_registrar_list<-names(test_data)
  tld_registrar_list <- tld_registrar_list[!is.na(tld_registrar_list)]
  prediction_list<-pblapply(tld_registrar_list, function(i) list_predict_first_renewal(i, test_data, data_models))
  prediction_list<-prediction_list[!is.na(prediction_list)]
  prediction_op<-rbindlist(prediction_list,use.names=TRUE, fill=TRUE) 
  test_data<-rbindlist(test_data,  use.names=TRUE, fill=TRUE) # modified as per error on 20200715 (see README)
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

