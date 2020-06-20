install.packages("bigrquery")
library(bigrquery)

# create a connection to the BigQuery project and the data set

projectid<-'radixbi-249015'
datasetid<-'prediction_vendors'
bq_conn <-  dbConnect(bigquery(), 
                      project = projectid,
                      dataset = datasetid, 
                      use_legacy_sql = FALSE
)

# view the list of tables in BigQuery dataset

bigrquery::dbListTables(bq_conn) 

# Is it OK to cache OAuth access credentials in the folder '/Users/lubagloukhov/.R/gargle/gargle-oauth' between R sessions?
#   
# 1: Yes
# 2: No

# Selecting 1:
# launches browser with google login for Tidyverse API Packages. 

# Choosing personal email account (which ahs access to project):
# Grant Tidyverse API Packages permission
# View and manage your data across Google Cloud Platform services
# View and manage your data across all Google Cloud Platform services, such as:
# View and manage your tables, datasets, and jobs in Google BigQuery
# View and manage your data in Google Cloud Storage
# View and manage your instances in Google Cloud SQL



