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
  prepared_data<-pblapply(tld_registrar_data,prep_domain_data)
  # prepared_data<-lapply(tld_registrar_data,
  #                       prep_domain_data)
  # prepared_data<-prep_domain_data(tld_registrar_data$websitenamecheap)
  #prepared_data<-rbindlist(prepared_data)
  return(prepared_data)
}

prep_domain_data<-function(domain_data) {
  domain_data<-basic_prep_domain_data(domain_data)
  # python.load("/home/radmin/npv_project/gibb_detect/gib_detect.py",TRUE)
  
  python.load("./gibb_detect/gib_detect.py",TRUE)
  
  #Get Gibberish Scores for all SLDs
  
  domain_data$gibb_score<-mapply(python.call, "gibberish_test", domain_data$sld)
  
  #Get Coeff-Variation on all days
  domain_data<-pblapply(split(domain_data, domain_data$creation_date), 
                        function(i) get_cluster_large(i, 0.3, 30000))
  domain_data<-rbindlist(domain_data)
  domain_data$gibb_score<-round(domain_data$gibb_score*100,2)
  
  return(domain_data)
}
