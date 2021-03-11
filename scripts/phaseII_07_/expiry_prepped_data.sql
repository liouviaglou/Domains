SELECT
  sub.domain_id,
  sub.renewed_count,
  sub.renewal_type,
  DATE(sub.Expiry_date) AS expiry_date,
  DATE(sub.creation_date) AS creation_date,
  sub.creation_time AS creation_time,
  sub.tld AS tld,
  sub.domain,
  sub.registrar AS registrar,
  sub.reseller AS reseller,
  sub.reseller_country AS reseller_country,
  sub.reg_period AS reg_period,
  sub.reg_revenue AS reg_revenue,
  sub.reg_arpt AS reg_arpt,
  sub.renewal_id,
  sub.renew_type,
  sub.autorenew_type,
  sub.updated_renewal_status as renewal_status,
  sub.renew_period AS renew_period,
  sub.renew_revenue AS renew_revenue,
  DATE(sub.renew_date) AS renew_date,
  sub.renew_arpt,
  npv_prepped_data.gibb_score,
  npv_prepped_data.pattern,
  npv_prepped_data.pattern_domain_count,
  npv_prepped_data.day_domains,
  npv_prepped_data.sld_length,
  npv_prepped_data.sld_type,
  npv_prepped_data.sld_type2,
  prediction_data.predicted_renewal_rate,
  sub.region,
FROM (
  SELECT
    * EXCEPT (renewal_status,
      custom_domain_id,
      custom_transaction_expiry),
    CASE
      WHEN (renewal_status IS NOT NULL) THEN renewal_status
    ELSE
    'Not Renewd'
  END
    AS updated_renewal_status
  FROM ( (
      SELECT
        'FirstTime' AS renewal_type,
        1 AS renewed_count,
        DATE(n.expiry_date) AS expiry_date,
        n.domain_id,
        n.domain,
        DATE(n.creation_date) AS creation_date,
        n.creationtime AS creation_time,
        CASE
          WHEN n.deleted_date IS NOT NULL AND DATE(n.expiry_date) > SAFE.PARSE_DATE('%Y-%m-%d', n.deleted_date) THEN 'Deleted'
        ELSE
        'Active'
      END
        AS status,
        n.tld,
        n.registrar_shortname AS registrar,
        n.client_shortname AS reseller,
        n.client_country AS reseller_country,
        CASE
          WHEN n.client_am IN ( 'Kenneth', 'Kenneth HK' ) THEN 'China'
        ELSE
        'Non China'
      END
        AS region,
        n.period AS reg_period,
        ROUND(n.net_revenue, 2) AS reg_revenue,
        ROUND(SAFE_DIVIDE(n.net_revenue,
            n.period), 2) AS reg_arpt
      FROM
        `radixbi-249015.prediction_vendors.newreg` n
      WHERE
        DATE(n.expiry_date) BETWEEN '2018-01-01'
        AND '2021-12-31'
        AND n.period > 0
        AND n.mbg = 0)
    UNION ALL (
      SELECT
        CASE
          WHEN renewed_count > 1 THEN 'Subsequent'
        ELSE
        'Second'
      END
        AS renewal_type,
        (r.renewed_count + 1) AS renewed_count,
        DATE(r.transaction_expiry) AS expiry_date,
        r.domain_id,
        r.domain,
        DATE(n.creation_date) AS creation_date,
        n.creationtime AS creation_time,
        CASE
          WHEN n.deleted_date IS NOT NULL AND DATE(r.transaction_expiry) > SAFE.PARSE_DATE('%Y-%m-%d', n.deleted_date) THEN 'Deleted'
        ELSE
        'Active'
      END
        AS status,
        r.tld,
        r.registrar_shortname AS registrar,
        r.client_shortname AS reseller,
        r.client_country AS reseller_country,
        CASE
          WHEN n.client_am IN ( 'Kenneth', 'Kenneth HK' ) THEN 'China'
        ELSE
        'Non China'
      END
        AS region,
        n.period AS reg_period,
        n.net_revenue AS reg_revenue,
        ROUND(SAFE_DIVIDE(n.net_revenue,
            n.period), 2) AS reg_arpt
      FROM
        radixbi-249015.prediction_vendors.renews r
      LEFT JOIN
        `radixbi-249015.prediction_vendors.newreg` n
      ON
        r.domain_id = n.domain_id
      WHERE
        DATE(r.transaction_expiry) BETWEEN '2018-01-01'
        AND '2021-12-31'
        AND r.period > 0
        AND r.mbg = 0
        AND ( r.renew_type IN ( 'renewal',
            'transfer' )
          OR r.autorenew_type = 'realized' ))) e
  LEFT JOIN (
    SELECT
      domain_id AS custom_domain_id,
      renewal_id,
      renewed_count as custom_renewed_count,
      transaction_expiry AS custom_transaction_expiry,
      CASE
        WHEN (renew_type IN ( 'renewal', 'transfer' ) OR autorenew_type = 'realized' ) THEN 'Renewed'
      ELSE
      'Not Renewd'
    END
      AS renewal_status,
      mbg AS renew_mbg,
      renew_type,
      autorenew_type,
      period AS renew_period,
      renew_date,
      net_revenue AS renew_revenue,
      ROUND(SAFE_DIVIDE(net_revenue,
          period), 2) AS renew_arpt,
    FROM
      radixbi-249015.prediction_vendors.renews
    WHERE
      mbg = 0
      AND period > 0 ) r
  ON
    e.domain_id = r.custom_domain_id
    AND e.renewed_count = r.custom_renewed_count
  ORDER BY
    domain ) AS sub
LEFT JOIN (
  SELECT
    domain_id,
    gibb_score,
    pattern_domain_count,
    pattern,
    day_domains,
    sld_length,
    sld_type,
    sld_type2,
  FROM
    radixbi-249015.prediction_vendors.npv_prepped_data) AS npv_prepped_data
ON
  sub.domain_id = npv_prepped_data.domain_id
LEFT JOIN (
  SELECT
    domain_id,
    'FirstTime' AS renewal_type,
    first_renewal_prediction AS predicted_renewal_rate
  FROM
    radixbi-249015.prediction_vendors.predictions
  UNION ALL
  SELECT
    domain_id,
    'Second' AS renewal_type,
    second_renewal_prediction AS predicted_renewal_rate
  FROM
    radixbi-249015.prediction_vendors.predictions
  UNION ALL
  SELECT
    domain_id,
    'Subsequent' AS renewal_type,
    subsequent_renewal_prediction AS predicted_renewal_rate
  FROM
    radixbi-249015.prediction_vendors.predictions ) AS prediction_data
ON
  sub.domain_id = prediction_data.domain_id
  AND sub.renewal_type = prediction_data.renewal_type