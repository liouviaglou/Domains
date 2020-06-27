# based on get_expiry_data.R
# changed pwlocal to radixbi-249015.prediction_vendors (db ref)
# changed n.id (renews table) to n.domain_id (need to check with client)
expiry_date_start='2020-01-01'
expiry_date_end='2020-05-31'

get_expiry_data_str1 <- sprintf("SELECT 'FirstTime'  AS renewal_type, 
               '1' AS renewed_count, 
               n.expiry_date AS Expiry_date, 
               n.domain_id AS domain_id, 
               n.domain, 
               n.creation_date AS creation_date1,
               
               CASE 
                 WHEN n.deleted_date IS NOT NULL 
                      AND n.expiry_date > PARSE_DATETIME('%Y-%m-%d', n.deleted_date) THEN 'Deleted' 
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
                      AND n.expiry_date > PARSE_DATETIME('%Y-%m-%d', n.deleted_date) THEN 0 
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


               
        FROM   radixbi-249015.prediction_vendors.newreg n 
               LEFT OUTER JOIN radixbi-249015.prediction_vendors.predictions pr 
                            ON n.domain_id = pr.domain_id 
                              
        WHERE  n.mbg = 0 
               AND n.expiry_date >= DATETIME('%s')
               AND n.expiry_date <= DATETIME('%s')
               AND n.period > 0 ", 
                                expiry_date_start, expiry_date_end)

get_expiry_data_str2 <- sprintf("SELECT CASE 
                 WHEN renewed_count > 1 THEN 'Subsequent' 
                 ELSE 'Second' 
               END AS renewal_type, 
               ( renewed_count + 1 ) AS renewed_count, 
               n.transaction_expiry AS Expiry_date, 
               n.domain_id AS domain_id, 
               n.domain AS domain, 
               n.registered_on AS creation_date1, 
               CASE 
                 WHEN n.deleted_date IS NOT NULL 
                      AND n.transaction_expiry > PARSE_DATETIME('%Y-%m-%d', n.deleted_date) THEN 'Deleted' 
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
                      AND n.expiry_date > PARSE_DATETIME('%Y-%m-%d', n.deleted_date) THEN 0 
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

        FROM   radixbi-249015.prediction_vendors.renews n 
               LEFT OUTER JOIN radixbi-249015.prediction_vendors.predictions pr 
                            ON n.domain_id = pr.domain_id 
                               
        WHERE  n.mbg = 0 
               AND n.transaction_expiry >= DATETIME('%s')
               AND n.transaction_expiry <= DATETIME('%s')
               AND n.period > 0 
               AND ( n.renew_type IN ( 'renewal', 'transfer' ) 
                      OR n.autorenew_type = 'realized' )", 
                                expiry_date_start, expiry_date_end)

get_expiry_data_str <-  paste("SELECT * FROM (",
                               get_expiry_data_str1, 
                               "UNION ALL", 
                               get_expiry_data_str2, 
                               ") AS sub ORDER  BY expiry_date", sep=" \n ")


count_expiry_data_str <- paste("SELECT count(*) FROM (",
                               get_expiry_data_str1, 
                               "UNION ALL", 
                               get_expiry_data_str2, 
                               ") AS sub ORDER  BY expiry_date", sep=" \n ")

        