# README

## Time Log


PHASE 2
Now in google sheets


PHASE 1
| Date     | Time     | Desc   |
|----------|----------|--------|
| March    | 3.0h     | preliminary exploration     |
| 20200507 | 1.5h     | additional data exploration |
| 20200507 | 1.5h     | assumption verificication   |   
| 20200508 | 1.0h     | assumption verificication   |   
| 20200509 | 1.0h     | plan (gslides)   |    
| 20200512 | 4.0h     | plan, compare   |     
| 20200514 | 2.0h     | new data - who dis?  |    
| 20200514 | 1.5h     | explore dtree as sub for lookup tables  |   
+ ~15 hours


## Lab Notebook 

### 20200623

Evaluated max depth ctree & pre-pruned max depth ctree with criterion .999. both barely outperform shallow tree (maybe overfitting?) except for when it comes to lift of >5 @ 10%.

next up: random forests. need to increase RAM of machine.

### 20200622_2

0. COMPLETED set up git on gcp vm via [link](https://cloud.google.com/ai-platform/notebooks/docs/save-to-github)
1. COMPLETED <s>spin up</s> modify to larger instance: n1-highmem-2 2 vCPUs 13GB mem via [link](https://cloud.google.com/compute/docs/instances/changing-machine-type-of-stopped-instance)
2. COMPLETED upload phase 1 data to <s>bigquery</s> cloud storage (RDS files)
3. COMPLETED load above into R via <s>[link](https://cran.r-project.org/web/packages/googleCloudStorageR/vignettes/googleCloudStorageR.html)</s> 
    gsutil cp gs://data_input/* data/
4. COMPLETED try max-depth but self-pruned tree (cart?) on phase 1 data

=======

### 20200622_2

0. COMPLETED set up git on gcp vm
1. spin up larger instance
2. upload phase 1 data to bigquery
3. try max-depth but self-pruned tree (cart?) on phase 1 data

### 20200622


0. COMPLETED Query BigQuery from [GCP interface](https://console.cloud.google.com/bigquery?project=radixbi-249015] using personal email - no glitches
1. COMPLETED Query BigQuery from local R session - followed [this rpubs doc](https://rpubs.com/shivanandiyer/BigRQuery) in phaseII_gcloudaccess/bigquery_from_local.R
2. COMPLETED Query BigQuery from GCP R session - seamless after client set up BigQuery permissions for service account 446988597652-compute@developer.gserviceaccount.com under project radixbi-249015


### 20200620

2. Query BigQuery from GCP R session (continued)
  - tried to explicitly query table in one project from a different project. same permission issues
  - tried to copy table between projects as in [link](https://cloud.google.com/bigquery/docs/copying-datasets) but don't have sufficient permissions
  - tried configuring IAM permisions as in CASE 2 of [link](https://wideops.com/understanding-gcp-service-accounts-three-common-use-cases/): "in each of the projects executing the queries, assign the [IAM permissions](https://cloud.google.com/bigquery/docs/access-control#predefined_roles_details) required to run queries against the BigQuery datasets to the application’s service account. For more information on configuring the permissions for this scenario, see this [resource](https://cloud.google.com/bigquery/docs/access-control?authuser=0#read_access_to_data_in_a_different_project)."
    - "When you assign roles at the organization and project level, you provide permission to run BigQuery jobs or to manage all of a project's BigQuery resources."
    - See "Read access to data in a different project" in [link](https://cloud.google.com/bigquery/docs/access-control-examples?hl=tr#read_access_to_data_in_a_different_project) GAVE UP
      - On project radixbi-249015  
        - Add OperationsServiceAccount to the predefined role bigquery.admin. (SKIP, irrelevant)
        - Add AnalystGroup to the predefined role bigquery.dataViewer.
      - On project radix2020 
        - Add AnalystGroup to the predefined role bigquery.user.
    - "You should define the role bigquery.admin on your service account and it would do the trick." [link](https://stackoverflow.com/questions/61895265/bigquery-cross-project-access-via-cloud-functions) 
      - via IAM page, added 446988597652-compute@developer.gserviceaccount.com (the Compute Engine default service account) role of bigquery.admin -- didn't change much
  - okay, i think i need to get access granted to radixbi-249015 project for 446988597652-compute@developer.gserviceaccount.com (the Compute Engine default service account). before checking with client, i will test on a dataset in a different project within my own account. Tested under a different project within my personal account -- confirming that this should solve the problem. Asked client to provide permissions. 

### 20200619

0. Query BigQuery from [GCP interface](https://console.cloud.google.com/bigquery?project=radixbi-249015] using personal email
1. Query BigQuery from local R session
  - [from rstudio](https://db.rstudio.com/databases/big-query/)
      - two options for connecting to Google BigQuery: (1) odbc package with a database driver (2) bigrquery package
  - [link2](https://bigrquery.r-dbi.org/)
  - ultimately, followed [this rpubs doc](https://rpubs.com/shivanandiyer/BigRQuery) in phaseII_gcloudaccess/bigquery_from_local.R
2. Query BigQuery from GCP R session
  - signed up for free $300 credit
    - Your free trial credit applies to all Google Cloud resources, with the following exceptions:
      You can't have more than 8 cores (or virtual CPUs) running at the same time.
      You can't add GPUs to your VM instances.
      You can't request a quota increase. For an overview of Compute Engine quotas, see Resource quotas.
      You can't create VM instances that are based on Windows Server images.
    - in US region (next time -- faster & cheaper in same reagion as client data (where?)?)
  - created project Radix2020 **will this project be able to access data in project radixbi-249015??**
  - [Pricing Calc](https://cloud.google.com/products/calculator/?_ga=2.17761273.-1180705002.1592430657)
  - [link](https://cloud.google.com/ai-platform/notebooks/docs/use-r-bigquery)
    - enabled Compute Engine for Notebooks
    - local machine hw.physicalcpu: 2 hw.logicalcpu: 4 system_profiler SPHardwareDataType | grep "  Memory:" Memory: 8 GB (comparable to n1-standard-2)
    - an instance 2x my local machne would cost ~$100/mo running continuously
    - created instance w/ R 3.6 of type n1-standard-1 [1 vCPU, 3.75 GB RAM]
    - running sample query as set up in [link](https://cloud.google.com/ai-platform/notebooks/docs/use-r-bigquery) failed due to permission issues. trying method that worked localy. same problem.
    - modifying dataset permissions via [link](https://cloud.google.com/bigquery/docs/dataset-access-controls#controlling_access_to_a_dataset). i don't have necessary permissions under personal email.
    - Enabled BigQuery Connection API, tried to connect to radixbi-249015
    - Added pin to radixbi-249015 project from within bigquery under the Radix2020 project




### 20200609

Added AUC calculation and plotting of mult gains curve on one set of axes (need to define colors & legends still)
Should work for any number of lift tables 

### 20200608

In 06/08 phase II kick off meeting, we succesfully zeroed in on lift, gains curve & its AUC as appropriate performance metrics. this script establishes benchmakrs based on original segmented glm model & initial simple decision tree.

started work in /paseII_perfm/

wrote code to compute lift at various percentiles for a given pred_df and to output a cumulative gains curve

need to calculate area under gains curbe and wrote code two plot mult gains curve in one




### 20200513_2

Using renewal_training_data_2020_02_25 to test dtree as an alternative to lookup tables.

mass_prep_data() on full 5.5M obs dataset would crash R, so subsetting to just 5% obs

predict() running into problem with new reseller levels, even when training data has all those factor levels

### 20200513

#### 202005 data

##### TODO: 
* test out decision tree on data used to generate fall back tables (used when not enough training data for seg-glm)
* follow up with questions in scripts/newdata_202005/exlore.R

|  dataset                           |  description                                                                                                                                       |
|------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------|
|  npv_actuals_recalc_2017_2020      |  A comparison of our probability predictions for all domains registered between 2017-01-01 and 2020-03-31 with the actual renewal status outcomes  |
|  npv_data_op_refreshed_model       |  our predictions based on the model built from  new training data                                                                                  |
|  npv_fallback_tables               |  fallback tables where we dont have enough training data for a particular tld-registrar combination                                                |
|  my_model_tables                   |  fallback tables for multi year domain registrations for which we generally dont have enough data                                                  |
|  npv_historic_renewal_data         |  historic data for domains which have gone through renewal cycles to calculate the fallback tables                                                 |
|  second_renewal_model_simplified   |  a simplified reference table to calculate second renewal probability                                                                              |
|  renewal_training_data_2020_02_25  |  training data used to predict first_renewal_probability in the npv_data_op_refreshed_model dataset                                                |


### 20200512

Final comparison of orig npv model to decision tree w/ and w/o tld & registrar/reseller

|                                 | Sensitivity  |             | Specificity  |             |
|---------------------------------|--------------|-------------|--------------|-------------|
|                                 | Full Dataset | Tr/Te split | Full Dataset | Tr/Te split |
| Orig sgmented glm               | 0.064040     | 0.079730    | 0.993300     | 0.990430    |
| DTree (no tld_reg)              | 0.038978     | 0.044921    | 0.996416     | 0.995603    |
| DTree (w tld_reg)               | 0.032527     | 0.029909    | 0.997084     | 0.997385    |
| DTree (no tld_reg) weighted_8_1 |              | 0.921800    |              | 0.282800    |
| DTree (no tld_reg) weighted_4_3 |              | 0.085941    |              | 0.988123    |
| DTree (w tld_reg) weighted_4_3  |              | 0.082676    |              | 0.989436    |
| DTree all vars  (dtree)         |              | 0.033152    |              | 0.997053    |

### 20200507_02

NPV model is specified as glm with family set to binomial (& link = 'logit' -- the default for binomial).

Logistic Reg Assumptions
1. The outcome is a binary or dichotomous variable like yes vs no, positive vs negative, 1 vs 0.
  a. Yup! 
  > summary(train_data$renewal_status)
    Not Renewd    Renewed 
       1729907     219175 
2. There is a linear relationship between the logit of the outcome and each predictor variables -- visually inspecting the scatter plot between each predictor and the logit values.
3. There is no influential values (extreme values or outliers) in the continuous predictors
4. There is no high intercorrelations (i.e. multicollinearity) among the predictors.

- what about unbalanced classes? First renewal rate only 11.25% for initial dataset
- regularization?
- compare varimp in d-tree with those of glm -- maybe varimp across tld_registrar_index are approx the same 
- could be that dtree does just as well as segmented glm, without the overhead
- can improve with random forest,

### 20200507

Joined flock chanel.
Downloaded data update
Checking to see if requested items exist. If not, FUP

Requests (listed in order of priority):

- (3a) performance metrics of existing models on the data subsets provided
- (2) a data dictionary for all other variables in originally provided training dataset, renewal_training_data which gets manipulated to form first_renewal_preds. *bolded* vars used in initial npv model. 
    -- [1] "renewal_type"        
    -- [2] "renewed_count"       
    -- [3] "expiry_date"         
    -- [4] "domain_id"           
    -- [5] "domain": the url. Normally formed as <second level domain(SLD)>.<top level domain(TLD)>  char          
    -- [6] "creation_date"       
    -- [7] "status"              
    -- [8] "tld": url extension(right side of the dot in the a url). eg. online, tech, site, store etc..) char            
    -- [9] "registrar":  Retail Partners who sell domains to end users  char (20200507)         
    -- [10] "reseller": Retail Partner/Client (Q: SUBSTE/FINER GRAIN OF REGISTRAR? 20200507) char        
    -- [11] "reseller_country"    
    -- [12] "region"              
    -- [13] *"reg_period"*: the period or the term of the registration of the domain name           
    -- [14] "renewal_status"      
    -- [15] "renew_type"          
    -- [16] "autorenew_type"      
    -- [17] "renew_date"          
    -- [18] "renew_registrar"     
    -- [19] "renew_reseller"      
    -- [20] "reg_revenue"         
    -- [21] "reg_arpt": the price at which the domain is bought by the reseller  double         
    -- [22] "renew_period": the period or the term of the registration of the domain name   int        
    -- [23] "renew_domain_revenue"
    -- [24] "renew_arpt": price at which the domain was renewed (20200507) float         
    -- [25] "reg_arpt_org"        
    -- [26] "tld_registrar_index" 

    Additional variables in train_data not in renewal_training_data

    -- [1] "sld": second level domain (see domain var above)                     
    -- [2] "sld_type": the type of the sld in terms of how many letters, numbers or hyphen the sld has. Eg: my char            
    -- [3] *"sld_length"*: character length of the SLD or the string on the left side of the dot in the domain name  int              
    -- [4] "sld_type2"               
    -- [5] *"day_domains"*: number of domains that are registered [BY THE RESELLER] on the day a particular domain was registered int 
    -- [6] *"log_reg_arpt"*: log of the reg_arpt i.e the price at which the domain is bought by the reseller double           
    -- [7] *"gibb_score"*: gibberish score. Higher score meaning more meaningful words or less gibberish double             
    -- [8] "cluster"                 
    -- [9] "pattern"                 
    -- [10] "pattern_score"           
    -- [11] *"pattern_domain_count"*: the number of domains in the pattern that the domain sld is a part of. For eg. A pattern score of 100 would mean that there are 99 other domains containing the same pattern as this domain registered on that day  int   
    -- [12] "first_renewal_prediction"

- (1) A broader view provided via a dataset that spans more resellers would be a great place to start. Whatever data we end up settling on, I'll be sure to include its limitations into consideration in my analysis. 
- (3b) beyond that, so I can get a sense of where else we can up level performance next. 
- (4) I know we discussed additional data sources that may be available for some resellers but not for others. As much information on this as possible would be great as I start thinking about next steps. This may well lie outside the scope of what's possible in this initial engagement but it will be helpful in brainstorming next phases.


SUMMARY: No information provided on performance metrics & data dicts missing a lot of variables above. Maybe request was interpreted in ascending versus descending level of importance?


Investigated *day_domains*. not clear whether this count is client specific from the code (doesn't look like it) but visual examination confirms this -- on "2017-04-06", "gmo" had a day_domains of 35 whereas "godaddy" had a day_domains of 160. These two values would be equivalent if it was calculated across all resellers. 

PLAN: 
 - p-hacking statement
 - validating assumptions of existing models (glm, logit)
 - unbalanced data (for logit models, wfor dtree {weighted, "_W8"})
 - validating assumption to subset by tld_registrar_index (a concatenation of tld and registrar)
    * intial dtree exploration determined that day_domains is by far the most important variable to subset on, then reg_period and sld_type. interestingly, reg_period is not an important variable when weighting to account of imbalanced data.
    * variable assumptions for decision tree
    * also do variable importance tests for glm? 
 - incorporatng other variables, feature engineering. auto feature engineering w/ auto encoders?



### 20200320

Working on getting orig script npv_script_share to work. Fixing paths, package namespaces (overlapping function names, needing to specify package explicitly, ie dplyr::), 

not clear on mass_predict_first_renewal()'s prediction_list generation... currently applying prediction fucntion on each column of test data. doesn't make sense. needs to aply it for each unique registrar. well, the problem lies in this: test_data_prepped only contains one registrar: namecheap due to mass_prep_data() which  subset's and ends up messing up the rest of the prediction functions because the resulting object is no longer a list of dataframes but just a dataframe. mass_data_prep() takes a long time when its implemented for all registrars, not just namecheap... so.. 

ended up just reading in test_data_prepped RDS.

what is the accuracy of the existing model?

### 20200321

Saved first renewal prediction model object to output data 
confusion matrices: true values on the left margin and predicted values on the top margin. In actuality, 11.25% of domains are renewed in the first round wheras the model only predicts 1.31 renewal rate.

Confusion Matrix and Statistics

          actuality
prediction   FALSE    TRUE
     FALSE 1718317  205139
     TRUE    11590   14036
                                          
               Accuracy : 0.8888          
                 95% CI : (0.8884, 0.8892)
    No Information Rate : 0.8875          
    P-Value [Acc > NIR] : 1.401e-08       
                                          
                  Kappa : 0.0933          
                                          
 Mcnemar's Test P-Value : < 2.2e-16       
                                          
            Sensitivity : 0.064040        
            Specificity : 0.993300        
         Pos Pred Value : 0.547725        
         Neg Pred Value : 0.893349        
             Prevalence : 0.112450        
         Detection Rate : 0.007201        
   Detection Prevalence : 0.013148        
      Balanced Accuracy : 0.528670        
                                          
       'Positive' Class : TRUE  


* Heavily Imbalanced Dataset
* Missing Values?

## 20200322

Generated initial classification tree, saved to output data.
Higher specificity but lower sensitivity -- better at identifying negatives & worse at identifying positives.

|       | FALSE    | TRUE   | class.error |   |
|-------|----------|--------|-------------|---|
| FALSE | 1722184  | 7723   | 0.004464402 |   |
| TRUE  | 205519   | 13656  | 0.935959849 |   |
|       |          |        |             |   |


#### pre-pruned to maxdepth5

Confusion Matrix and Statistics

             
t_predict_md5 Not Renewd Renewed
   Not Renewd    1723707  210632
   Renewed          6200    8543
                                          
               Accuracy : 0.8888          
                 95% CI : (0.8883, 0.8892)
    No Information Rate : 0.8875          
    P-Value [Acc > NIR] : 5.213e-08       
                                          
                  Kappa : 0.0597          
                                          
 Mcnemar's Test P-Value : < 2.2e-16       
                                          
            Sensitivity : 0.038978        
            Specificity : 0.996416        
         Pos Pred Value : 0.579461        
         Neg Pred Value : 0.891109        
             Prevalence : 0.112450        
         Detection Rate : 0.004383        
   Detection Prevalence : 0.007564        
      Balanced Accuracy : 0.517697        
                                          
       'Positive' Class : Renewed 

## 20200324

Split data into train (80%) and test set. retrained orig model and evaled performance. DECISION TREE does better than ORIG MODEL at predicting negatives but not positives. Look into working with imbalanced data.

ORIG MODEL 
Confusion Matrix and Statistics

                                
first_renewal_model_test_predBin Not Renewd Renewed
                      Not Renewd     342410   40584
                      Renewed          3307    3516
                                          
               Accuracy : 0.8874          
                 95% CI : (0.8864, 0.8884)
    No Information Rate : 0.8869          
    P-Value [Acc > NIR] : 0.1459          
                                          
                  Kappa : 0.1111          
                                          
 Mcnemar's Test P-Value : <2e-16          
                                          
            Sensitivity : 0.07973         
            Specificity : 0.99043         
         Pos Pred Value : 0.51532         
         Neg Pred Value : 0.89403         
             Prevalence : 0.11313         
         Detection Rate : 0.00902         
   Detection Prevalence : 0.01750         
      Balanced Accuracy : 0.53508         
                                          
       'Positive' Class : Renewed     

MAXDEPTH5 DTREE
Confusion Matrix and Statistics

                            
first_dtree_md5_test_predict Not Renewd Renewed
                  Not Renewd     344197   42119
                  Renewed          1520    1981
                                         
               Accuracy : 0.8881         
                 95% CI : (0.8871, 0.889)
    No Information Rate : 0.8869         
    P-Value [Acc > NIR] : 0.009866       
                                         
                  Kappa : 0.0677         
                                         
 Mcnemar's Test P-Value : < 2.2e-16      
                                         
            Sensitivity : 0.044921       
            Specificity : 0.995603       
         Pos Pred Value : 0.565838       
         Neg Pred Value : 0.890973       
             Prevalence : 0.113130       
         Detection Rate : 0.005082       
   Detection Prevalence : 0.008981       
      Balanced Accuracy : 0.520262       
                                         
       'Positive' Class : Renewed        
                                         

setting the weights for Renewed class to 8x the non-Renewed class:
              Accuracy : 0.3551 
              Sensitivity : 0.9218          
              Specificity : 0.2828  

setting the weights for Renewed class to 2x the non-Renewed class:
              Accuracy : 0.8801  
              Sensitivity : 0.12308        
              Specificity : 0.97663   


Examine Var Imp:
- ORIG MODEL: across ext & reseller
- ... vs. TREE Models
- Upsampled vs. Reg trees:
    splittong varibales the same for DUH! non-renewed classification
    (i.e. day_domains > 219/228)
    but for renewed but diff for renwed classification
