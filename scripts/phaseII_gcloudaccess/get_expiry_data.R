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
#library(factoextra)
library(reshape)
library(lubridate)
library(clipr)
library(reshape2)
library(plotly)
#library(vegan)
library(tidyr)
library(cluster)
#library(esquisse)
library(dbplyr)
library(zoo)



get_expiry_data<-function(expiry_date_start, expiry_date_end) {
  radixDB = dbConnect(MySQL(), user='opsuser', password='&UBaW?5EaY', dbname='pwlocal', host='radixdb')
  rs = dbSendQuery(radixDB, 
                   paste("SELECT * 
FROM   (SELECT 'FirstTime'  AS renewal_type, 
               '1' AS renewed_count, 
               n.expiry_date AS Expiry_date, 
               n.id AS domain_id, 
               n.domain, 
               n.creation_date AS creation_date1, 
               
               CASE 
                 WHEN n.deleted_date IS NOT NULL 
                      AND n.expiry_date > n.deleted_date THEN 'Deleted' 
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
                 WHEN n.r_transfer = 1 THEN 'Transfered' 
                 WHEN ( n.r_renew_type IN ( 'renewal', 'transfer' ) 
                         OR n.r_autorenew_type = 'realized' ) THEN 'Renewed' 
                 ELSE 'Not Renewd' 
               END AS renewal_status, 
               
               n.r_mbg AS renew_mbg,
               n.r_renew_type AS renew_type, 
               n.r_autorenew_type AS autorenew_type, 
               n.r_renewed_on AS renew_date, 
               n.r_registrar_shortname AS Renew_Registrar_Shortname, 
               n.r_client_shortname AS Renew_Client_Shortname, 
               Round(n.net_revenue, 2) AS domain_revenue, 
               Round(( n.net_revenue / n.period ), 2) AS arpt, 
               
               CASE 
                 WHEN n.deleted_date IS NOT NULL 
                      AND n.expiry_date > n.deleted_date THEN 0 
                 ELSE pr.revised_prediction 
               END AS prediction,

               pr.sld_length AS sld_length, 
               pr.sld_type AS sld_type, 
               pr.sld_type2 AS sld_type2, 
               pr.day_domains AS day_domains, 
               pr.logarpt AS logarpt, 
               pr.gibb_score AS gibb_score, 
               pr.coeff_variation AS coeff_variation, 
               Round(( n.net_revenue - ( ( CASE 
                                             WHEN n.centralnic_comm IS NULL THEN 
                                             0.00 
                                             ELSE n.centralnic_comm 
                                           END ) + ( CASE 
                                                       WHEN n.icann_comm IS NULL 
                                                     THEN 
                                                       0.00 
                                                       ELSE 
                                         n.icann_comm + n.icann_fixed 
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
               Round(( n.r_net_revenue / n.r_period ), 2) AS renew_arpt, 
               Round(( n.r_net_revenue - ( ( CASE 
                                               WHEN n.r_centralnic_comm IS NULL 
                                             THEN 
                                               0.00 
                                               ELSE n.r_centralnic_comm 
                                             END ) + ( CASE 
                                                         WHEN 
                                           n.r_icann_comm IS NULL 
                                                       THEN 0.00 
                                                         ELSE 
                                           n.r_icann_comm + n.icann_fixed 
                                                       END ) ) ), 2) AS renew_gross_profit, 
               Round(( n.r_net_revenue - ( ( CASE 
                                               WHEN n.r_centralnic_comm IS NULL 
                                             THEN 
                                               0.00 
                                               ELSE n.r_centralnic_comm 
                                             END ) + ( CASE 
                                                         WHEN 
                                           n.r_icann_comm IS NULL 
                                                       THEN 0.00 
                                                         ELSE n.r_icann_comm 
                                                       END ) ) ), 2) AS renew_gp_less_icann_fixed, 
               Round(( n.net_revenue / n.period ), 2) AS registration_arpt 


               
        FROM   pwlocal.newreg n 
               LEFT OUTER JOIN pwlocal.predictions pr 
                            ON n.id = pr.domain_id 
                               AND pr.type = 'FirstTime' 
        WHERE  n.mbg = 0 
               AND n.expiry_date >= '", expiry_date_start,"' 
               AND n.expiry_date <= '", expiry_date_end, "' 
               AND n.period > 0 
        UNION ALL 
        SELECT CASE 
                 WHEN renewed_count > 1 THEN 'Subsequent' 
                 ELSE 'Second' 
               END AS renewal_type, 
               ( renewed_count + 1 ) AS renewed_count, 
               n.transaction_expiry AS Expiry_date, 
               n.id AS domain_id, 
               n.domain AS domain, 
               n.registered_on AS creation_date1, 
               CASE 
                 WHEN n.deleted_date IS NOT NULL 
                      AND n.transaction_expiry > n.deleted_date THEN 'Deleted' 
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
                 WHEN n.r_transfer = 1 THEN 'Transfered' 
                 WHEN ( n.r_renew_type IN ( 'renewal', 'transfer' ) 
                         OR n.r_autorenew_type = 'realized' ) THEN 'Renewed' 
                 ELSE 'Not Renewd' 
               END AS renewal_status, 
               n.r_mbg AS renew_mbg,
               n.r_renew_type AS renew_type, 
               n.r_autorenew_type AS autorenew_type, 
               n.r_renewed_on AS renew_date, 
               n.r_registrar_shortname AS Renew_Registrar_Shortname, 
               n.r_client_shortname AS Renew_Client_Shortname, 
               Round(n.net_revenue, 2) AS domain_revenue, 
               Round(( n.net_revenue / n.period ), 2) AS arpt, 
               CASE 
                 WHEN n.deleted_date IS NOT NULL 
                      AND n.expiry_date > n.deleted_date THEN 0 
                 ELSE pr.revised_prediction 
               END AS prediction, 
               pr.sld_length AS sld_length, 
               pr.sld_type AS sld_type, 
               pr.sld_type2 AS sld_type2, 
               pr.day_domains AS day_domains, 
               pr.logarpt AS logarpt, 
               pr.gibb_score AS gibb_score, 
               pr.coeff_variation AS coeff_variation, 
               Round(( n.net_revenue - ( ( CASE 
                                             WHEN n.centralnic_comm IS NULL THEN 
                                             0.00 
                                             ELSE n.centralnic_comm 
                                           END ) + ( CASE 
                                                       WHEN n.icann_comm IS NULL 
                                                     THEN 
                                                       0.00 
                                                       ELSE 
                                         n.icann_comm + n.icann_fixed 
                                                     END ) ) ), 2)   AS 
               gross_profit, 
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
               Round(( n.r_net_revenue / n.r_period ), 2) AS renew_arpt, 
               Round(( n.r_net_revenue - ( ( CASE 
                                               WHEN n.r_centralnic_comm IS NULL 
                                             THEN 
                                               0.00 
                                               ELSE n.r_centralnic_comm 
                                             END ) + ( CASE 
                                                         WHEN 
                                           n.r_icann_comm IS NULL 
                                                       THEN 0.00 
                                                         ELSE 
                                           n.r_icann_comm + n.icann_fixed 
                                                       END ) ) ), 2) AS renew_gross_profit, 
               Round(( n.r_net_revenue - ( ( CASE 
                                               WHEN n.r_centralnic_comm IS NULL 
                                             THEN 
                                               0.00 
                                               ELSE n.r_centralnic_comm 
                                             END ) + ( CASE 
                                                         WHEN 
                                           n.r_icann_comm IS NULL 
                                                       THEN 0.00 
                                                         ELSE n.r_icann_comm 
                                                       END ) ) ), 2) AS renew_gp_less_icann_fixed, 
               CASE 
                 WHEN n.newreg_period IS NOT NULL THEN Round(( 
                 n.newreg_net_revenue / n.newreg_period ), 2) 
                 ELSE 0 
               END AS 
               registration_arpt 
        FROM   pwlocal.renews n 
               LEFT OUTER JOIN pwlocal.predictions pr 
                            ON n.id = pr.domain_id 
                               AND pr.type = 'Renew' 
        WHERE  n.mbg = 0 
               AND n.transaction_expiry >= '",expiry_date_start, "' 
               AND n.transaction_expiry <= '",expiry_date_end,"' 
               AND n.period > 0 
               AND ( n.renew_type IN ( 'renewal', 'transfer' ) 
                      OR n.autorenew_type = 'realized' )) AS sub 
ORDER  BY expiry_date ", sep = ""))
  
  expiry_data<-fetch(rs, n=-1)
  dbDisconnect(radixDB)
  expiry_data<-expiry_data %>%
    select(renewal_type, renewed_count, expiry_date = Expiry_date, domain_id, domain,
           creation_date = creation_date1, status, tld, registrar = registrar_shortname,
           reseller = client_shortname, reseller_country = client_country, region, reg_period = noofyears,
           registrant_country,renewal_status,renew_mbg, renew_type, autorenew_type, renew_date, renew_registrar = Renew_Registrar_Shortname, 
           renew_reseller = Renew_Client_Shortname, reg_revenue = domain_revenue, reg_arpt = arpt,
           renew_period = renew_domain_years, renew_domain_revenue, renew_arpt,reg_arpt_org = registration_arpt)
  expiry_data$expiry_date<-as.Date(expiry_data$expiry_date, "%Y-%m-%d")
  expiry_data$creation_date<-as.Date(expiry_data$creation_date, "%Y-%m-%d")
  expiry_data$renew_date<-as.Date(expiry_data$renew_date, "%Y-%m-%d")
  expiry_data$registrar<-tolower(expiry_data$registrar)
  expiry_data$reseller<-tolower(expiry_data$reseller)
  return(expiry_data)
}



                                

get_premium_expiry_data<-function(expiry_date_start, expiry_date_end) {
  radixDB = dbConnect(MySQL(), user='opsuser', password='&UBaW?5EaY', dbname='pwlocal', host='radixdb')
  rs = dbSendQuery(radixDB, 
                   paste("SELECT 
                                * 
                                  FROM 
                                (
                                  select 
                                  'FirstTime' as renewal_type, 
                                  '1' as renewed_count, 
                                  n.expiry as Expiry_date, 
                                  n.addid as domain_id, 
                                  n.domain, 
                                  n.source, 
                                  n.salesdate as creation_date1, 
                                  CASE WHEN n.deleted_date is not null 
                                  AND n.expiry > n.deleted_date THEN 'Deleted' ELSE 'Active' END as status, 
                                  n.tld as tld, 
                                  n.registrar_shortname as registrar_shortname, 
                                  n.client_shortname as client_shortname, 
                                  n.client_country as client_country, 

                                  CASE WHEN n.client_am in ('Kenneth', 'Kenneth HK') THEN 'China' ELSE 'Non China' END as region, 
                                  n.years as noofyears, 
                                  CASE WHEN n.r_transfer = 1 THEN 'Transfered' WHEN (
                                    n.r_renew_type in ('renewal', 'transfer') 
                                    or n.r_autorenew_type = 'realized'
                                  ) THEN 'Renewed' ELSE 'Not Renewd' END as renewal_status, 
                                  
                                  n.r_mbg AS renew_mbg,

                                  n.r_renew_type as renew_type, 
                                  n.r_autorenew_type as autorenew_type, 
                                  n.r_renewedon as renew_date, 
                                  n.r_registrar_shortname as Renew_Registrar_Shortname, 
                                  n.r_client_shortname as Renew_Client_Shortname, 
                                  round(n.net_revenue, 2) as domain_revenue, 
                                  round(
                                    (n.net_revenue / n.years), 
                                    2
                                  ) as arpt, 
                                  CASE WHEN n.deleted_date is not null 
                                  AND n.expiry > n.deleted_date THEN 0 ELSE pr.revised_prediction END as prediction, 
                                  pr.sld_length as sld_length, 
                                  pr.sld_type as sld_type, 
                                  pr.sld_type2 as sld_type2, 
                                  pr.day_domains as day_domains, 
                                  pr.logarpt as logarpt, 
                                  pr.gibb_score as gibb_score, 
                                  pr.coeff_variation as coeff_variation, 
                                  round(
                                    (
                                      n.net_revenue - (
                                        (
                                          CASE WHEN n.afternicshare is null THEN 0.00 ELSE n.afternicshare END
                                        ) + (
                                          CASE WHEN n.centralnicshare is null THEN 0.00 ELSE n.centralnicshare END
                                        ) + (
                                          CASE WHEN n.icann_comm is null THEN 0.00 ELSE n.icann_comm + n.icann_fixed END
                                        )
                                      )
                                    ), 
                                    2
                                  ) as gross_profit, 
                                  round(
                                    (
                                      n.net_revenue - (
                                        (
                                          CASE WHEN n.afternicshare is null THEN 0.00 ELSE n.afternicshare END
                                        ) + (
                                          CASE WHEN n.centralnicshare is null THEN 0.00 ELSE n.centralnicshare END
                                        ) + (
                                          CASE WHEN n.icann_comm is null THEN 0.00 ELSE n.icann_comm END
                                        )
                                      )
                                    ), 
                                    2
                                  ) as gp_less_icann_fixed, 
                                  n.r_years as renew_domain_years, 
                                  round(n.r_net_revenue, 2) as renew_domain_revenue, 
                                  CASE WHEN round(
                                    (n.r_net_revenue / n.r_years), 
                                    2
                                  ) is null THEN pp.amount_usd ELSE round(
                                    (n.r_net_revenue / n.r_years), 
                                    2
                                  ) END as renew_arpt, 
                                  round(
                                    (
                                      n.r_net_revenue - (
                                        (
                                          CASE WHEN n.r_afternicshare is null THEN 0.00 ELSE n.r_afternicshare END
                                        ) + (
                                          CASE WHEN n.r_centralnicshare is null THEN 0.00 ELSE n.r_centralnicshare END
                                        ) + (
                                          CASE WHEN n.r_icann_comm is null THEN 0.00 ELSE n.r_icann_comm + n.icann_fixed END
                                        )
                                      )
                                    ), 
                                    2
                                  ) as renew_gross_profit, 
                                  round(
                                    (
                                      n.r_net_revenue - (
                                        (
                                          CASE WHEN n.r_afternicshare is null THEN 0.00 ELSE n.r_afternicshare END
                                        ) + (
                                          CASE WHEN n.r_centralnicshare is null THEN 0.00 ELSE n.r_centralnicshare END
                                        ) + (
                                          CASE WHEN n.r_icann_comm is null THEN 0.00 ELSE n.r_icann_comm END
                                        )
                                      )
                                    ), 
                                    2
                                  ) as renew_gp_less_icann_fixed, 
                                  round(
                                    (n.net_revenue / n.years), 
                                    2
                                  ) as registration_arpt 
                                  from 
                                  pwlocal.premium_auction n 
                                  left outer join pwlocal.predictions pr on n.addid = pr.domain_id 
                                  and pr.type = 'PreFirstTime' 
                                  left outer join db_radix.premium_price pp on n.domain = pp.domain 
                                  and pp.type = 'Renewal' 
                                  WHERE 
                                  n.mbg = 0 
                                  and n.expiry >= '",expiry_date_start, "'  
                                  and n.expiry <= '",expiry_date_end, "'  
                                  and n.years > 0 
                                  UNION ALL 
                                  select 
                                  CASE WHEN renewed_count > 1 THEN 'Subsequent' ELSE 'Second' END as renewal_type, 
                                  (renewed_count + 1) as renewed_count, 
                                  n.transaction_expiry as Expiry_date, 
                                  n.addid as domain_id, 
                                  n.domain as domain, 
                                  n.source as Source, 
                                  n.registered_on as creation_date1, 
                                  CASE WHEN n.deleted_date is not null 
                                  AND n.transaction_expiry > n.deleted_date THEN 'Deleted' ELSE 'Active' END as status, 
                                  n.tld as tld, 
                                  n.registrar_shortname as registrar_shortname, 
                                  n.client_shortname as client_shortname, 
                                  n.client_country as client_country,

                                  CASE WHEN n.client_am in ('Kenneth', 'Kenneth HK') THEN 'China' ELSE 'Non China' END as region, 
                                  n.years as noofyears, 
                                  CASE WHEN n.r_transfer = 1 THEN 'Transfered' WHEN (
                                    n.r_renew_type in ('renewal', 'transfer') 
                                    or n.r_autorenew_type = 'realized'
                                  ) THEN 'Renewed' ELSE 'Not Renewd' END as renewal_status, 
                                  n.r_mbg AS renew_mbg,

                                  n.r_renew_type as renew_type, 
                                  n.r_autorenew_type as autorenew_type, 
                                  n.r_renewedon as renew_date, 
                                  n.r_registrar_shortname as Renew_Registrar_Shortname, 
                                  n.r_client_shortname as Renew_Client_Shortname, 
                                  round(n.net_revenue, 2) as domain_revenue, 
                                  round(
                                    (n.net_revenue / n.years), 
                                    2
                                  ) as arpt, 
                                  CASE WHEN n.deleted_date is not null 
                                  AND n.expiry > n.deleted_date THEN 0 ELSE pr.revised_prediction END as prediction, 
                                  pr.sld_length as sld_length, 
                                  pr.sld_type as sld_type, 
                                  pr.sld_type2 as sld_type2, 
                                  pr.day_domains as day_domains, 
                                  pr.logarpt as logarpt, 
                                  pr.gibb_score as gibb_score, 
                                  pr.coeff_variation as coeff_variation, 
                                  round(
                                    (
                                      n.net_revenue - (
                                        (
                                          CASE WHEN n.afternicshare is null THEN 0.00 ELSE n.afternicshare END
                                        ) + (
                                          CASE WHEN n.centralnicshare is null THEN 0.00 ELSE n.centralnicshare END
                                        ) + (
                                          CASE WHEN n.icann_comm is null THEN 0.00 ELSE n.icann_comm + n.icann_fixed END
                                        )
                                      )
                                    ), 
                                    2
                                  ) as gross_profit, 
                                  round(
                                    (
                                      n.net_revenue - (
                                        (
                                          CASE WHEN n.afternicshare is null THEN 0.00 ELSE n.afternicshare END
                                        ) + (
                                          CASE WHEN n.centralnicshare is null THEN 0.00 ELSE n.centralnicshare END
                                        ) + (
                                          CASE WHEN n.icann_comm is null THEN 0.00 ELSE n.icann_comm END
                                        )
                                      )
                                    ), 
                                    2
                                  ) as gp_less_icann_fixed, 
                                  n.r_years as renew_domain_years, 
                                  round(n.r_net_revenue, 2) as renew_domain_revenue, 
                                  CASE WHEN round(
                                    (n.r_net_revenue / n.r_years), 
                                    2
                                  ) is null THEN pp.amount_usd ELSE round(
                                    (n.r_net_revenue / n.r_years), 
                                    2
                                  ) END as renew_arpt, 
                                  round(
                                    (
                                      n.r_net_revenue - (
                                        (
                                          CASE WHEN n.r_afternicshare is null THEN 0.00 ELSE n.r_afternicshare END
                                        ) + (
                                          CASE WHEN n.r_centralnicshare is null THEN 0.00 ELSE n.r_centralnicshare END
                                        ) + (
                                          CASE WHEN n.r_icann_comm is null THEN 0.00 ELSE n.r_icann_comm + n.icann_fixed END
                                        )
                                      )
                                    ), 
                                    2
                                  ) as renew_gross_profit, 
                                  round(
                                    (
                                      n.r_net_revenue - (
                                        (
                                          CASE WHEN n.r_afternicshare is null THEN 0.00 ELSE n.r_afternicshare END
                                        ) + (
                                          CASE WHEN n.r_centralnicshare is null THEN 0.00 ELSE n.r_centralnicshare END
                                        ) + (
                                          CASE WHEN n.r_icann_comm is null THEN 0.00 ELSE n.r_icann_comm END
                                        )
                                      )
                                    ), 
                                    2
                                  ) as renew_gp_less_icann_fixed, 
                                  CASE WHEN n.newreg_period is not null THEN round(
                                    (
                                      n.newreg_net_revenue / n.newreg_period
                                    ), 
                                    2
                                  ) ELSE 0 END as registration_arpt 
                                  from 
                                  pwlocal.premium_renews n 
                                  left outer join pwlocal.predictions pr on n.addid = pr.domain_id 
                                  and pr.type = 'PreRenew' 
                                  left outer join db_radix.premium_price pp on n.domain = pp.domain 
                                  and pp.type = 'Renewal' 
                                  WHERE 
                                  n.mbg = 0 
                                  and n.transaction_expiry >= '",expiry_date_start, "'  
                                  and n.transaction_expiry <= '",expiry_date_end, "'  
                                  and n.years > 0 
                                  and (
                                    n.renew_type in ('renewal', 'transfer') 
                                    or n.autorenew_type = 'realized'
                                  )
                                ) as sub 
                                order by 
                                Expiry_date", sep = ""))
  
  
  expiry_data<-fetch(rs, n=-1)
  dbDisconnect(radixDB)
  
  expiry_data<-expiry_data %>%
    select(renewal_type, renewed_count, expiry_date = Expiry_date, domain_id, domain,
           creation_date = creation_date1, status, tld , registrar = registrar_shortname,
           reseller = client_shortname, reseller_country = client_country, region, reg_period = noofyears,
           renewal_status, renew_mbg, renew_type, 
           autorenew_type , renew_date, renew_registrar = Renew_Registrar_Shortname, 
           renew_reseller = Renew_Client_Shortname, reg_revenue = domain_revenue, reg_arpt = arpt,
           renew_period = renew_domain_years, renew_domain_revenue, renew_arpt , reg_arpt_org = registration_arpt)
  expiry_data$expiry_date<-as.Date(expiry_data$expiry_date, "%Y-%m-%d")
  expiry_data$creation_date<-as.Date(expiry_data$creation_date, "%Y-%m-%d")
  expiry_data$renew_date<-as.Date(expiry_data$renew_date, "%Y-%m-%d")
  expiry_data$registrar<-tolower(expiry_data$registrar)
  expiry_data$reseller<-tolower(expiry_data$reseller)
  
  
  con<-DBI::dbConnect(RMySQL::MySQL(), 
                      dbname = "db_radix",
                      host = "172.16.140.199",
                      user = "opsuser",
                      password = "&UBaW?5EaY"
  )
  
  
  premium_inventory_tbl<-tbl(con, "premium_inventory")
  premium_inventory<-premium_inventory_tbl %>%
    filter(domain %in% expiry_data$domain) %>%
    collect()
  
  dbDisconnect(con)
  rm(con)
  rm(premium_inventory_tbl)
  
  
  expiry_data$tier_price<-0
  expiry_data$tier_price<-premium_inventory$tier_price[match(expiry_data$domain, 
                                                       premium_inventory$domain)]
  
  return(expiry_data)
}








####################Base Data Creation######################
if(FALSE) {
  
start_date<-"2015-01-01"
end_date<-"2017-12-31"

base_expiry_data<-get_expiry_data(start_date, end_date)

saveRDS(base_expiry_data, "/home/radmin/npv_project/backup_data/base_expiry_data_2")



base_data_daily_summary_2<-base_expiry_data %>%
  group_by(tld ,registrar, reseller, renewed_count, reg_arpt_org, 
           reg_period, region, reseller_country, expiry_date, renew_date) %>%
  summarise(expiring_domains = length(unique(domain_id)),
            renewed_domains = length(unique(domain_id[renewal_status %in% c("Renewed", "Transfered")])),
            renewed_domain_years = sum(renew_period[renewal_status %in% c("Renewed", "Transfered")],na.rm = TRUE),
            renewal_revenue = sum(renew_domain_revenue[renewal_status %in% c("Renewed", "Transfered")], na.rm = TRUE)) %>%
  mutate(renewal_rate = round(renewed_domains/expiring_domains,3),
         renewal_arpt = round(renewal_revenue/renewed_domain_years, 2))

base_data_daily_summary_2$renewal_arpt[is.nan(base_data_daily_summary_2$renewal_arpt)]<-0

base_data_daily_summary_2<-as.data.frame(base_data_daily_summary_2)


#################base data update to a db table##################

expiry_base_data_upload<-function(expiry_base_data_summary) {
  radixDB = dbConnect(MySQL(), user='opsuser', password='&UBaW?5EaY', dbname='db_radix', host='radixdb')
  dbWriteTable(radixDB, "expiry_data_daily_summary_1", expiry_base_data_summary, append = TRUE, row.names = FALSE)
  dbDisconnect(radixDB)
}

saveRDS(base_data_daily_summary_2, "/home/radmin/npv_project/backup_data/base_data_daily_summary_2")
expiry_base_data_upload(base_data_daily_summary_2)
########################################################

}

######################Daily Run#########################
start_date<-Sys.Date() - 50

end_date<-Sys.Date() %m+% years(1)

incremental_data<-get_expiry_data(start_date, end_date)

incremental_data_daily_summary<-incremental_data %>%
  group_by(tld ,registrar, reseller, renewed_count, reg_arpt_org, 
           reg_period, region, reseller_country, expiry_date, renew_date) %>%
  summarise(expiring_domains = length(unique(domain_id)),
            renewed_domains = length(unique(domain_id[renewal_status %in% c("Renewed", "Transfered")])),
            renewed_domain_years = sum(renew_period[renewal_status %in% c("Renewed", "Transfered")],na.rm = TRUE),
            renewal_revenue = sum(renew_domain_revenue[renewal_status %in% c("Renewed", "Transfered")], na.rm = TRUE)) %>%
  mutate(renewal_rate = round(renewed_domains/expiring_domains,3),
         renewal_arpt = round(renewal_revenue/renewed_domain_years, 2))

incremental_data_daily_summary$renewal_arpt[is.nan(incremental_data_daily_summary$renewal_arpt)]<-0



#############upload to table###############################


expiry_incremental_data_upload<-function(expiry_incremental_data_summary) {
  radixDB = dbConnect(MySQL(), user='opsuser', password='&UBaW?5EaY', dbname='db_radix', host='radixdb')
  
  dbWriteTable(radixDB, "expiry_data_daily_import", expiry_incremental_data_summary, append = TRUE, row.names = FALSE)
  
  incremental_data_update_delete_query<-paste("DELETE p FROM db_radix.expiry_data_daily_summary_1 p inner join db_radix.expiry_data_daily_import i ON p.expiry_date = i.expiry_date
                                              WHERE TRUE ", sep = "")
  rs = dbSendQuery(radixDB, incremental_data_update_delete_query)
  
  
  ########DELETE from the daily update table###########
  dbWriteTable(radixDB, "expiry_data_daily_summary_1", expiry_incremental_data_summary, append = TRUE, row.names = FALSE)
  
  import_table_delete_query<-paste("DELETE FROM db_radix.expiry_data_daily_import", sep = "")
  rs = dbSendQuery(radixDB, import_table_delete_query)
  dbDisconnect(radixDB)
}


expiry_incremental_data_upload(incremental_data_daily_summary)






##################################################Premium Base Data#########################################


premium_expiry_data<-get_premium_expiry_data("2015-01-01", "2021-04-27")


premium_expiry_base_data_upload<-function(premium_base_data) {
  radixDB = dbConnect(MySQL(), user='opsuser', password='&UBaW?5EaY', dbname='db_radix', host='radixdb')
  dbWriteTable(radixDB, "premium_expiry_data", premium_base_data, append = TRUE, row.names = FALSE)
  dbDisconnect(radixDB)
}


premium_expiry_base_data_upload(premium_expiry_data)
################################premium incremental data#######################



start_date<-Sys.Date() - 50

end_date<-Sys.Date() %m+% years(1)

premium_incremental_data<-get_premium_expiry_data(start_date, end_date)

premium_incremental_data$renewal_status[!(premium_incremental_data$renew_mbg == 0)]<-"Not Renewd"
premium_incremental_data$autorenew_type[!(premium_incremental_data$renew_mbg == 0)]<-"unrealized"



#############upload to table###############################


premium_expiry_incremental_data_upload<-function(premium_incremental_expiry_data, cut_off_date) {
  radixDB = dbConnect(MySQL(), user='opsuser', password='&UBaW?5EaY', dbname='db_radix', host='radixdb')
  incremental_data_update_delete_query<-paste("DELETE FROM db_radix.premium_expiry_data WHERE expiry_date >= '",cut_off_date,"'" , sep = "")
  rs = dbSendQuery(radixDB, incremental_data_update_delete_query)
  dbWriteTable(radixDB, "premium_expiry_data", premium_incremental_expiry_data, append = TRUE, row.names = FALSE)
  dbDisconnect(radixDB)
}


premium_expiry_incremental_data_upload(premium_incremental_data, start_date)





