get_expiry_data_str <- paste("SELECT sub.renewal_type, sub.renewed_count, DATE(sub.Expiry_date) as expiry_date, DATE(sub.creation_date1) as creation_date,
      sub.domain_id, sub.domain,sub.registrar_shortname as registrar, sub.client_shortname as reseller,  sub.client_country as reseller_country, 
      sub.registrant_country as registrant_country, sub.noofyears as reg_period, sub.domain_revenue as reg_revenue, sub.arpt as reg_arpt, 
      sub.renew_mbg as renew_mbg, sub.renewal_item_id, 
      sub.renew_type, sub.autorenew_type,  sub.renewal_status, DATE(sub.renew_date) as renew_date, sub.renew_arpt, 
      npv_prepped_data.gibb_score, npv_prepped_data.pattern, npv_prepped_data.pattern_domain_count, 
      npv_prepped_data.day_domains, npv_prepped_data.sld_length, 
      npv_prepped_data.sld_type, npv_prepped_data.sld_type2
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


               
        FROM   `radixbi-249015.prediction_vendors.newreg` n 
              
        WHERE  n.mbg = 0 
               AND n.expiry_date >= '",expiry_date_start,"'
               AND n.expiry_date <= '",expiry_date_start,"'
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
               
               
               CASE 
                 WHEN n.r_mbg = 'NULL' THEN NULL 
                 ELSE SAFE_CAST(n.r_mbg as INT64) 
               END AS renew_mbg,
               
               n.r_renewal_id as renewal_item_id,
               n.r_renew_type AS renew_type, 
               n.r_autorenew_type AS autorenew_type, 
               CASE 
                 WHEN n.r_renew_date = 'NULL' THEN NULL 
                 ELSE SAFE.PARSE_DATE('%Y-%m-%d',n.r_renew_date) 
               END AS renew_date,
               
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
               CASE 
                 WHEN n.r_period = 'NULL' THEN NULL 
                 ELSE SAFE_CAST(n.r_period as INT64) 
               END AS renew_domain_years,                                     
               
               Round(n.r_net_revenue, 2) AS renew_domain_revenue, 
               Round( SAFE_DIVIDE(n.r_net_revenue , SAFE_CAST(n.r_period as INT64)), 2) AS renew_arpt, 
               
               
               CASE 
                 WHEN n.newreg_period IS NOT NULL THEN Round(( 
                 SAFE_DIVIDE(n.newreg_net_revenue, n.newreg_period )), 2) 
                 ELSE 0 
               END AS 
               registration_arpt 
        FROM   `radixbi-249015.prediction_vendors.renews` n 
               LEFT OUTER JOIN `radixbi-249015.prediction_vendors.newreg` pr 
                            ON n.domain_id = pr.domain_id 
                             
        WHERE  n.mbg = 0 
               AND n.transaction_expiry >= '",expiry_date_start,"'
               AND n.transaction_expiry <= '",expiry_date_start,"'
               AND n.period > 0 
               AND ( n.renew_type IN ( 'renewal', 'transfer' ) 
                      OR n.autorenew_type = 'realized' )
              ) AS sub 
                      
       left join (select domain_id, gibb_score, pattern_domain_count, pattern, 
            day_domains, sld_length, sld_type, sld_type2, 
                  from `radixbi-249015.data_science.npv_prepped_data`) as npv_prepped_data
       on sub.domain_id = npv_prepped_data.domain_id
                      
ORDER  BY expiry_date", sep="")

count_expiry_data_str <- paste("SELECT COUNT(*) FROM   (SELECT 'FirstTime'  AS renewal_type, 
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


               
        FROM   `radixbi-249015.prediction_vendors.newreg` n 
              
        WHERE  n.mbg = 0 
               AND n.expiry_date >= '",expiry_date_start,"'
               AND n.expiry_date <= '",expiry_date_start,"'
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
               
               
               CASE 
                 WHEN n.r_mbg = 'NULL' THEN NULL 
                 ELSE SAFE_CAST(n.r_mbg as INT64) 
               END AS renew_mbg,
               
               n.r_renewal_id as renewal_item_id,
               n.r_renew_type AS renew_type, 
               n.r_autorenew_type AS autorenew_type, 
               CASE 
                 WHEN n.r_renew_date = 'NULL' THEN NULL 
                 ELSE SAFE.PARSE_DATE('%Y-%m-%d',n.r_renew_date) 
               END AS renew_date,
               
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
               CASE 
                 WHEN n.r_period = 'NULL' THEN NULL 
                 ELSE SAFE_CAST(n.r_period as INT64) 
               END AS renew_domain_years,                                     
               
               Round(n.r_net_revenue, 2) AS renew_domain_revenue, 
               Round( SAFE_DIVIDE(n.r_net_revenue , SAFE_CAST(n.r_period as INT64)), 2) AS renew_arpt, 
               
               
               CASE 
                 WHEN n.newreg_period IS NOT NULL THEN Round(( 
                 SAFE_DIVIDE(n.newreg_net_revenue, n.newreg_period )), 2) 
                 ELSE 0 
               END AS 
               registration_arpt 
        FROM   `radixbi-249015.prediction_vendors.renews` n 
               LEFT OUTER JOIN `radixbi-249015.prediction_vendors.newreg` pr 
                            ON n.domain_id = pr.domain_id 
                             
        WHERE  n.mbg = 0 
               AND n.transaction_expiry >= '",expiry_date_start,"'
               AND n.transaction_expiry <= '",expiry_date_start,"'
               AND n.period > 0 
               AND ( n.renew_type IN ( 'renewal', 'transfer' ) 
                      OR n.autorenew_type = 'realized' )
              ) AS sub 
                      
       left join (select domain_id, gibb_score, pattern_domain_count, pattern, 
            day_domains, sld_length, sld_type, sld_type2, 
                  from `radixbi-249015.data_science.npv_prepped_data`) as npv_prepped_data
       on sub.domain_id = npv_prepped_data.domain_id", sep="")

        