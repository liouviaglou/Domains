# XX time Rscript prep_expiry.R >> /home/jupyter/local/Domains_202003/data/output/prep_expiry.log 2>&1

fallback_gen <- function ( npv_historic_renewal_data = expiry_train_df_1, # training data w/ reseller geo
                           reseller_am_geo_map = geoLookupDF){
    
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
    
    npv_fallback_list = list(npv_fallback_first_geo_arpt_tld,
                             npv_fallback_first_geo_arpt,
           npv_fallback_first_tld_arpt,
           npv_fallback_second_tld_geo,
           npv_fallback_second_geo,
           npv_fallback_third_tld_geo,
           npv_fallback_third_geo,
           npv_fallback_first_final,
           npv_fallback_second_final, 
           npv_fallback_third_final)
    
    names(npv_fallback_list) = c('npv_fallback_first_geo_arpt_tld',
                                 'npv_fallback_first_geo_arpt',
           'npv_fallback_first_tld_arpt',
           'npv_fallback_second_tld_geo',
           'npv_fallback_second_geo',
           'npv_fallback_third_tld_geo',
           'npv_fallback_third_geo',
           'npv_fallback_first_final',
           'npv_fallback_second_final', 
           'npv_fallback_third_final')
    
    return(npv_fallback_list)
}

fallback_app_1 <- function (test_data_op=expiry_test_predictions,
                          in_col='pred_df_seg2_glm',
                          out_col='pred_df_seg2_glm_fb'){
    

    test_data_op$reg_arpt_slab<-cut(test_data_op$reg_arpt_org, 
                                             breaks = c(-Inf,0,0.3,1,3,5,10,15,25,35,Inf),
                                             right = TRUE)

    test_data_op[[out_col]] <- test_data_op[[in_col]]
    
    test_data_op<-test_data_op %>%
    mutate(temp_index = paste(tld, reseller_geo, reg_arpt_slab, sep = ""))

    test_data_op[[out_col]][is.na(test_data_op[[out_col]])]<-
    npv_fallback_first_geo_arpt_tld$renewal_rate[match(test_data_op$temp_index[is.na(test_data_op[[out_col]])],
                                                     npv_fallback_first_geo_arpt_tld$index)]
    
    test_data_op<-test_data_op %>%
    mutate(temp_index = paste(reseller_geo, reg_arpt_slab, sep = ""))

    test_data_op[[out_col]][is.na(test_data_op[[out_col]])]<-
    npv_fallback_first_geo_arpt$renewal_rate[match(test_data_op$temp_index[is.na(test_data_op[[out_col]])],
                                                 npv_fallback_first_geo_arpt$index)]
    
    test_data_op<-test_data_op %>%
    mutate(temp_index = paste(tld, reg_arpt_slab, sep = ""))

    test_data_op[[out_col]][is.na(test_data_op[[out_col]])]<-
    npv_fallback_first_tld_arpt$renewal_rate[match(test_data_op$temp_index[is.na(test_data_op[[out_col]])],
                                                 npv_fallback_first_tld_arpt$index)]
    
    test_data_op<-test_data_op %>%
      mutate(temp_index = paste(region, reg_arpt_slab, sep = ""))

    test_data_op[[out_col]][is.na(test_data_op[[out_col]])]<-
      npv_fallback_first_final$renewal_rate[match(test_data_op$temp_index[is.na(test_data_op[[out_col]])],
                                                  npv_fallback_first_final$index)]

    test_data_op<-test_data_op %>%
      select(-temp_index)
    
    return(test_data_op)
}
                                                  
fallback_app_2 <- function (test_data_op=expiry_test_predictions,
                          in_col,
                          out_col){
    

    test_data_op$reg_arpt_slab<-cut(test_data_op$reg_arpt_org, 
                                             breaks = c(-Inf,0,0.3,1,3,5,10,15,25,35,Inf),
                                             right = TRUE)

    test_data_op[[out_col]] <- test_data_op[[in_col]]
    
    
    test_data_op<-test_data_op %>%
    mutate(temp_index = paste(tld, reseller_geo, sep = ""))

    test_data_op[[out_col]][is.na(test_data_op[[out_col]])]<-
    npv_fallback_second_tld_geo$renewal_rate[match(test_data_op$temp_index[is.na(test_data_op$second_renewal_prediction)],
                                                 npv_fallback_second_tld_geo$index)]
    
    test_data_op[[out_col]][is.na(test_data_op[[out_col]])]<-
    npv_fallback_second_geo$renewal_rate[match(test_data_op$reseller_geo[is.na(test_data_op$second_renewal_prediction)],
                                             npv_fallback_second_geo$reseller_geo)]
    
    test_data_op<-test_data_op %>%
      mutate(temp_index = paste(region, reg_arpt_slab, sep = ""))

    test_data_op[[out_col]][is.na(test_data_op[[out_col]])]
      npv_fallback_second_final$renewal_rate[match(test_data_op$region[is.na(test_data_op[[out_col]])],
                                                   npv_fallback_second_final$region)]

    test_data_op<-test_data_op %>%
      select(-temp_index)
    
    return(test_data_op)
}



fallback_app_3 <- function (test_data_op=expiry_test_predictions,
                          in_col,
                          out_col){
    

    test_data_op$reg_arpt_slab<-cut(test_data_op$reg_arpt_org, 
                                             breaks = c(-Inf,0,0.3,1,3,5,10,15,25,35,Inf),
                                             right = TRUE)

    test_data_op[[out_col]] <- test_data_op[[in_col]]
    
    test_data_op<-test_data_op %>%
      mutate(temp_index = paste(tld, reseller_geo, sep = ""))

    test_data_op[[out_col]][is.na(test_data_op[[out_col]])]<-
      npv_fallback_third_tld_geo$renewal_rate[match(test_data_op$temp_index[is.na(test_data_op[[out_col]])],
                                                    npv_fallback_third_tld_geo$index)]


    test_data_op[[out_col]][is.na(test_data_op[[out_col]])]<-
      npv_fallback_third_geo$renewal_rate[match(test_data_op$reseller_geo[is.na(test_data_op[[out_col]])],
                                                npv_fallback_third_geo$reseller_geo)]

 
    test_data_op<-test_data_op %>%
      mutate(temp_index = paste(region, reg_arpt_slab, sep = ""))

    test_data_op[[out_col]][is.na(test_data_op[[out_col]])]<-
      npv_fallback_third_final$renewal_rate[match(test_data_op$region[is.na(test_data_op[[out_col]])],
                                                  npv_fallback_third_final$region)]

    test_data_op<-test_data_op %>%
      select(-temp_index)
    
    return(test_data_op)
}
    