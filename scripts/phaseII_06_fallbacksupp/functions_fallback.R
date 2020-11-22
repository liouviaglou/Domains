# XX time Rscript prep_expiry.R >> /home/jupyter/local/Domains_202003/data/output/prep_expiry.log 2>&1

geo_suppl <- function (expiry_df, # train or test data w/o reseller_geo information
                       geoLookupDF = geoLookupDF # from google sheets PredictiveModelAnalysis_ResellerGeoMap
                      ){
    
    cat("Expiry data originally has", expiry_df %>% nrow(),"rows and", expiry_df %>% nrow(),"missing geo's.\n")
    
    geoLookupDF <- geoLookupDF %>% filter_all(any_vars(!is.na(.)))
    
    # use a lookup with duplicate rows removed, remove duplicate rows in geo_lookup due to registrar level segmentation
    geoLookupDF_2 <- geoLookupDF %>% distinct(reseller,reseller_country, reseller_geo)
    
    # PASS #1: merge on reseller & reseller country to obtain reseller_geo from lookupo

    # prep expiry variables for merging
    expiry_df$reseller <- factor(expiry_df$reseller)
    expiry_df$reseller_country <- factor(expiry_df$reseller_country)
    expiry_df <- as.data.frame(expiry_df)
    
    # prep geolookup variables for merging
    geoLookupDF$reseller <- factor(geoLookupDF$reseller)
    geoLookupDF$reseller_country <- factor(geoLookupDF$reseller_country)
    geoLookupDF <- as.data.frame(geoLookupDF)
    
    # execute merge
    expiry_df <- merge(expiry_df,
                       geoLookupDF_2,
                       on=c('reseller','reseller_country'), 
                       all.x = TRUE)
    
    cat("... after intial merge on reseller & _country, expiry has", expiry_df %>% nrow(),
        "rows and", expiry_df %>% filter(is.na(reseller_geo)) %>% nrow(),"missing geo's.\n")
    
    # PASS #2: unmatched geos get assigned based on reseller_country alone (not reseller name info)
    
    # first, create a new lookup where we drop everything except for reseller_country and _geo and have NA map to Others
    geoLookupDF_2 <- geoLookupDF_2 %>% distinct(reseller_country, reseller_geo) %>% 
      mutate(reseller_geo = as.character(reseller_geo)) %>%
      mutate ( reseller_geo = if_else(is.na(reseller_country), 'Others', reseller_geo) ) %>% 
      distinct(reseller_country, reseller_geo) %>% 
      mutate(reseller_geo = as.factor(reseller_geo))

    # second, use this new lookup to fill missing reseller_geo based just on reseller_country
    expiry_df[['reseller_geo']][is.na(expiry_df[['reseller_geo']])]<-
      geoLookupDF_2$reseller_geo[match( expiry_df$reseller_country[is.na(expiry_df[['reseller_geo']])],
                                       geoLookupDF_2$reseller_country)]
    
    cat("... after secondary fill with _country, expiry has", expiry_df %>% nrow(),
        "rows and", expiry_df %>% filter(is.na(reseller_geo)) %>% nrow(),"missing geo's.\n")


    # third, some manual tweaks
    expiry_df[['reseller_geo']][expiry_df[['reseller_country']]=='Southafrica']<-'South Africa'
    expiry_df[['reseller_geo']][expiry_df[['reseller_country']]=='Delaware']<-'United States'
    
    cat("... after manual tweaks with _country, expiry has", expiry_df %>% nrow(),
        "rows and", expiry_df %>% filter(is.na(reseller_geo)) %>% nrow(),"missing geo's.\n")
    
    return(expiry_df)
    
}

fallback_gen <- function ( npv_historic_renewal_data = expiry_train_df_1, # training data w/ reseller geo (gen w/ geo_supll funct.)
                           reseller_am_geo_map = geoLookupDF){
    
    geoLookupDF <- geoLookupDF %>% filter_all(any_vars(!is.na(.)))
    
    npv_historic_renewal_data$creation_date <- as.Date(npv_historic_renewal_data$creation_date)
    
    npv_historic_renewal_data$reseller_am<-
    reseller_am_geo_map$reseller_am[match(npv_historic_renewal_data$reseller,
                                          reseller_am_geo_map$reseller)]
                                      
    npv_historic_renewal_data$reseller_geo<-
    reseller_am_geo_map$reseller_geo[match(npv_historic_renewal_data$reseller,
                                           reseller_am_geo_map$reseller)]

    npv_historic_renewal_data$reseller_geo[is.na(npv_historic_renewal_data$reseller_geo)]<-"Others"
    
    # LVG added 11/16
    if(!("reg_arpt_org" %in% colnames(npv_historic_renewal_data))){
      npv_historic_renewal_data$reg_arpt_org <- npv_historic_renewal_data$reg_arpt
    }
    
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

fallback_app_1_dplyr <- function (test_data_op=expiry_test_predictions,
                          out_col='pred_df_seg2_glm'){
    

    if (!('reg_arpt_org' %in% names(test_data_op))){
        test_data_op$reg_arpt_org <- test_data_op$reg_arpt
    }
    
    test_data_op$reg_arpt_slab<-cut(test_data_op$reg_arpt_org, 
                                             breaks = c(-Inf,0,0.3,1,3,5,10,15,25,35,Inf),
                                             right = TRUE)

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
    
    return(test_data_op[[out_col]])
}

fallback_app_1 <- function (test_data_op=expiry_test_predictions,
                          in_col='pred_df_seg2_glm',
                          out_col='pred_df_seg2_glm_fb'){
    

    if (!('reg_arpt_org' %in% names(test_data_op))){
        test_data_op$reg_arpt_org <- test_data_op$reg_arpt
    }
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
    