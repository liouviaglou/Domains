
library(dplyr)
library(modeest)
library(RMySQL)
library(tidyverse)
library(R.utils)
library(data.table)
library(lubridate)
library(reshape2)
library(dbplyr)
library(bigrquery)





bq_get_expiry_data<-function(expiry_date_start, expiry_date_end) {
  
  bq_auth(token = readRDS("/home/radmin/npv_project/.secrets/fc7a2051d6f88188d75fad3f7c357acb_bi-team@radix.email"))

  con <- DBI::dbConnect(
    bigquery(),
    project = "radixbi-249015",
    dataset = "radix",
    billing = "radixbi-249015",
    use_legacy_sql = TRUE
  )
  

  
  rs = dbSendQuery(con, 
                   paste("SELECT * 
FROM   (SELECT 'FirstTime'  AS renewal_type, 
               1 AS renewed_count, 
               n.expiry_date AS Expiry_date, 
               n.domain_id AS domain_id, 
               n.domain, 
               n.creation_date AS creation_date1, 
               
               CASE 
                 WHEN n.deleted_date IS NOT NULL 
                      AND DATE(n.expiry_date) > SAFE.PARSE_DATE('%Y-%m-%d',n.deleted_date) THEN 'Deleted' 
                 ELSE 'Active' 
               END AS status, 
               
               n.tld AS tld, 
               n.registrar_shortname AS registrar_shortname, 
               n.client_shortname AS client_shortname, 
               n.client_country AS client_country, 
               n.registrant_country AS registrant_country,

               CASE 
                 WHEN n.client_am IN ( 'Kenneth', 'Kenneth HK' ) THEN 'China' 
                 ELSE 'Non China' 
               END AS region,

               n.period AS noofyears,

               CASE 
                 WHEN ( n.r_renew_type IN ( 'renewal', 'transfer' ) 
                         OR n.r_autorenew_type = 'realized' ) THEN 'Renewed' 
                 ELSE 'Not Renewd' 
               END AS renewal_status, 
               
               n.r_mbg AS renew_mbg,
               n.r_renewal_id as renewal_item_id,
               n.r_renew_type AS renew_type, 
               n.r_autorenew_type AS autorenew_type, 
               DATE(n.r_renew_date) AS renew_date, 
               n.r_registrar_shortname AS Renew_Registrar_Shortname, 
               n.r_client_shortname AS Renew_Client_Shortname, 
               Round(n.net_revenue, 2) AS domain_revenue, 
               Round(( SAFE_DIVIDE(n.net_revenue, n.period )), 2) AS arpt, 
               
              
               Round(( n.net_revenue - ( ( CASE 
                                             WHEN n.centralnic_comm IS NULL THEN 
                                             0.00 
                                             ELSE n.centralnic_comm 
                                           END ) + ( CASE 
                                                       WHEN n.icann_comm IS NULL 
                                                     THEN 
                                                       0.00 
                                                       ELSE 
                                         n.icann_comm 
                                                     END ) ) ), 2)   AS gross_profit, 
               Round(( n.net_revenue - ( ( CASE 
                                             WHEN n.centralnic_comm IS NULL THEN 
                                             0.00 
                                             ELSE n.centralnic_comm 
                                           END ) + ( CASE 
                                                       WHEN n.icann_comm IS NULL 
                                                     THEN 
                                                       0.00 
                                                       ELSE n.icann_comm 
                                                     END ) ) ), 2)   AS gp_less_icann_fixed, 
               n.r_period AS renew_domain_years, 
               Round(n.r_net_revenue, 2) AS renew_domain_revenue, 
               Round( SAFE_DIVIDE(n.r_net_revenue, CAST(n.r_period as INT64)), 2) AS renew_arpt, 

              
               Round(( SAFE_DIVIDE(n.net_revenue, n.period )), 2) AS registration_arpt 


               
        FROM   prediction_vendors.newreg n 
              
        WHERE  n.mbg = 0 
               AND n.expiry_date >= '", expiry_date_start,"' 
               AND n.expiry_date <= '", expiry_date_end,"' 
               AND n.period > 0 
        UNION ALL
        
        SELECT CASE 
                 WHEN renewed_count > 1 THEN 'Subsequent' 
                 ELSE 'Second' 
               END AS renewal_type, 
               
               (renewed_count + 1 ) AS renewed_count, 
               n.transaction_expiry AS Expiry_date, 
               n.domain_id AS domain_id, 
               n.domain AS domain, 
               pr.creation_date AS creation_date1, 
               CASE 
                 WHEN n.deleted_date IS NOT NULL 
                      AND DATE(n.transaction_expiry) > SAFE.PARSE_DATE('%Y-%m-%d',n.deleted_date) THEN 'Deleted' 
                 ELSE 'Active' 
               END AS status, 
               
               n.tld AS tld, 
               n.registrar_shortname AS registrar_shortname, 
               n.client_shortname AS client_shortname, 
               n.client_country AS client_country, 
               n.registrant_country AS registrant_country,
               
               CASE 
                 WHEN n.client_am IN ( 'Kenneth', 'Kenneth HK' ) THEN 'China' 
                 ELSE 'Non China' 
               END AS region, 
               n.period AS noofyears, 
               
               CASE 
                 WHEN ( n.r_renew_type IN ( 'renewal', 'transfer' ) 
                         OR n.r_autorenew_type = 'realized' ) THEN 'Renewed' 
                 ELSE 'Not Renewd' 
               END AS renewal_status,
               
               
               CAST(n.r_mbg as INT64) AS renew_mbg,
                n.r_renewal_id as renewal_item_id,
               n.r_renew_type AS renew_type, 
               n.r_autorenew_type AS autorenew_type, 
               PARSE_DATE('%Y-%m-%d',n.r_renew_date) AS renew_date, 
               n.r_registrar_shortname AS Renew_Registrar_Shortname, 
               n.r_client_shortname AS Renew_Client_Shortname, 
               Round(n.net_revenue, 2) AS domain_revenue, 
               Round(( SAFE_DIVIDE(n.net_revenue ,n.period )), 2) AS arpt, 
               
               Round(( n.net_revenue - ( ( CASE 
                                             WHEN n.centralnic_comm IS NULL THEN 
                                             0.00 
                                             ELSE n.centralnic_comm 
                                           END ) + ( CASE 
                                                       WHEN n.icann_comm IS NULL 
                                                     THEN 
                                                       0.00 
                                                       ELSE 
                                         n.icann_comm 
                                                     END ) ) ), 2)   AS gross_profit, 
               Round(( n.net_revenue - ( ( CASE 
                                             WHEN n.centralnic_comm IS NULL THEN 
                                             0.00 
                                             ELSE n.centralnic_comm 
                                           END ) + ( CASE 
                                                       WHEN n.icann_comm IS NULL 
                                                     THEN 
                                                       0.00 
                                                       ELSE n.icann_comm 
                                                     END ) ) ), 2)   AS gp_less_icann_fixed, 
               CAST(n.r_period as INT64) AS renew_domain_years, 
               Round(n.r_net_revenue, 2) AS renew_domain_revenue, 
               Round( SAFE_DIVIDE(n.r_net_revenue , CAST(n.r_period as INT64)), 2) AS renew_arpt, 
               
               
               CASE 
                 WHEN n.newreg_period IS NOT NULL THEN Round(( 
                 SAFE_DIVIDE(n.newreg_net_revenue, n.newreg_period )), 2) 
                 ELSE 0 
               END AS 
               registration_arpt 
        FROM   prediction_vendors.renews n 
               LEFT OUTER JOIN prediction_vendors.newreg pr 
                            ON n.domain_id = pr.domain_id 
                             
        WHERE  n.mbg = 0 
               AND n.transaction_expiry >= '",expiry_date_start,"'
               AND n.transaction_expiry <= '",expiry_date_start,"'  
               AND n.period > 0 
               AND ( n.renew_type IN ( 'renewal', 'transfer' ) 
                      OR n.autorenew_type = 'realized' )) AS sub 
ORDER  BY expiry_date ", sep = ""))

  expiry_data<-dbFetch(rs, n=-1)
  dbDisconnect(con)
  
  
  expiry_data<-expiry_data %>%
    select(renewal_type, renewed_count, expiry_date = Expiry_date, domain_id, domain,
           creation_date = creation_date1, status, tld, registrar = registrar_shortname,
           reseller = client_shortname, reseller_country = client_country, region, reg_period = noofyears,
           registrant_country, renewal_status, renew_mbg, renewal_item_id, 
           renew_type, autorenew_type, renew_date, renew_registrar = Renew_Registrar_Shortname, 
           renew_reseller = Renew_Client_Shortname, reg_revenue = domain_revenue, reg_arpt = arpt,
           renew_period = renew_domain_years, renew_domain_revenue, renew_arpt,reg_arpt_org = registration_arpt)
  expiry_data$expiry_date<-as.Date(expiry_data$expiry_date, "%Y-%m-%d")
  expiry_data$creation_date<-as.Date(expiry_data$creation_date, "%Y-%m-%d")
  expiry_data$renew_date<-as.Date(expiry_data$renew_date, "%Y-%m-%d")
  expiry_data$registrar<-tolower(expiry_data$registrar)
  expiry_data$reseller<-tolower(expiry_data$reseller)
  
  expiry_data$renewal_status[!(expiry_data$renew_mbg == 0)]<-"Not Renewd"
  expiry_data$autorenew_type[!(expiry_data$renew_mbg == 0)]<-"unrealized"
  return(expiry_data)
}


bq_expiry_data<-bq_get_expiry_data("2020-02-01", "2020-02-03")
