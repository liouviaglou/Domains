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

## 20201016

- why seg2_glm predictions the same as seg2_rf predictions in head(5) testcase?

- test pred_all function

BEFORE RUNNING SCRIPT, DONT FORGET TO LOAD AGG MODELS, RENAME R OBJECTS & SAVE IN NEW FOLDER
REPLACE AGG MODEL NULL IN SCRIPT UNLESS YOU WANT TO RERUN THE WHOLE THING(S)

## 20201014

Rescripting tld_reg segmented model training & testing
join predictions with test data for output
left off at predictions
need to script up pulling of model based on directory
make pred script more efficient -- no looping load & remove for agg model

## 20201012

Generating Google Doc summary for meeting

Incorporate renew_type into metalearning (auto_renew proportion)


## 20201011

multiclass RF (total of 7 classes in test's training data => reference 0.14 random accuracy): 

0.5071 prediction accuracy (auc_win02) heavily unbalanced: 65% seg2
VARIMP
sldlen_kurt	21.65149908 _(kurt of sld len, heavy on outliers)
tld_rat	20.92758790  _(number of distinct tld as a ratio of total volume)
gibbs_kurt	20.71630376 _(kurt of gibb score, heavy on outliers)
ren_prp	17.95157847
gibbs_rng	17.45164277
sldlen_rng	13.66291174
sldlen_skew	13.56014294
regarpt_kurt	13.31758581
gibbs_std	11.64584779
gibbs_skew	10.93235341
country_maj	10.88234742

0.6441 prediction accuracy (l10_win02) heavily unbalanced: 49% seg2
heavily unbalanced. mostly predicts seg2_glm
VARIMP
ren_prp	15.406023
tld_rat	14.904183
gibbs_rng	13.959882
gibbs_kurt	12.568853
sldlen_kurt	12.559483
country_maj	11.580764
sldlen_skew	9.963272
regarpt_skew	9.643958
regarpt_kurt	8.808535
gibbs_skew	8.470457
gibbs_std	8.376095


binary RF 
seg2_glm_fb vs agg_rf_fb (total of 2 classes in test's training data => reference 0.50 random accuracy): 
0.7011  prediction accuracy (auc_win_01) heavily unbalanced: 68% seg2
VARIMP
tld_rat	18.697204
sldlen_kurt	18.212754
gibbs_kurt	15.917581
sldlen_skew	13.335850
gibbs_rng	11.982880
country_maj	11.957433
ren_prp	10.363165
sldlen_rng	9.835484
pdcnt_skew	9.558258
regarpt_skew	9.460302
regarpt_kurt	9.254175
daydom_kurt	8.738817
gibbs_mean	8.495020
daydom_skew	8.473886
sldlen_std	8.416247


0.8185  prediction accuracy (l10_win_01) heavily unbalanced: 82% seg2
mostly predicts seg2_glm
VARIMP
tld_rat	9.563950
country_maj	8.889265
sldlen_kurt	8.255048
gibbs_kurt	7.220285
gibbs_rng	7.007295
sldlen_rng	6.422216
ren_prp	6.420688
daydom_std	6.103789
gibbs_skew	6.093788
sldlen_skew	6.002812
sldlen_std	5.996812
regarpt_kurt	5.800360
regarpt_skew	5.720854
daydom_kurt	5.601121
gibbs_max	5.442951


***if we are to pick models according to binary RF***

Ultimately, doesn't do muchbetter than picking one model for all tld_regs. Slight improvement for lift @ 10%

|   P | metamodel      | seg2_glm_fb       | agg_rf_fb       |
|----:|---------------:|------------------:|----------------:|
| 0.1 | 3.853071       |          3.895960 |        3.848921 |
| 0.2 | 3.058937       |          3.010515 |        3.073464 |
| 0.3 | 2.469102       |          2.513835 |        2.480631 |
| 0.4 | 2.177988       |          2.180410 |        2.167958 |
|-----|---------------:|------------------:|----------------:|
| AUC |      0.7993082 |         0.7993774 |       0.8031682 |

***Need new data***

models trained on 80% subset of expiry
tested on remaining 20%

metalearning trained on 80% subset of 20% subset of expiry
tested on 20% subset of 20% of expiry

So left with very small data (60K rows)




***NOTES***
STILL NEED TO GO BACK AND SUPPLEMENT RESELLER_GEO MATCH NA'S

ALSO why OVERALL \_FB outperforms original SEG2_GLM only 13(4) times at the TLD-REG level but OVERALL, does better
SInce fallbacks primarily affecting NAs, may be how NAs are factoring into TLD-REG level metric

Examine metric calc for a single, small tld-reg with NAs

ALSO.. FB values are low, why would they be affecting lift at 10% as much as they are overall?

## 20201010

Compared performance w/ and w/o FB across models. Most noticeable improvement (across both metrics) comes from using fallback tables for seg2_glm, as this approach generates the most NA predictions (1356 vs. 184 for seg_glm and 88 for all other models). ***NOW (20201011) ANSWERED: the 88 domains in expiry_test not in predictions_df are those for which training data does not exist***

| l10_orig |   l10_fb |          model |       imprv |
|---------:|---------:|---------------:|------------:|
| 4.715899 | 4.765959 | lift_ seg2_glm | 0.010615218 |
| 3.925896 | 3.945993 |  lift_ seg_glm | 0.005119136 |
| 4.666204 | 4.671685 |  lift_ agg_glm | 0.001174628 |
| 4.619798 | 4.624548 |  lift_ seg2_rf | 0.001028237 |
| 4.857675 | 4.865714 |   lift_ seg_rf | 0.001654882 |
| 4.829539 | 4.835751 |   lift_ agg_rf | 0.001286222 |

|  auc_orig |    auc_fb |        model |       imprv |
|----------:|----------:|-------------:|------------:|
| 0.8167190 | 0.8238152 | auc_seg2_glm | 0.008688591 |
| 0.8027497 | 0.8061041 |  auc_seg_glm | 0.004178644 |
| 0.8113440 | 0.8126338 |  auc_agg_glm | 0.001589800 |
| 0.8190357 | 0.8203329 |  auc_seg2_rf | 0.001583793 |
| 0.8315727 | 0.8328480 |   auc_seg_rf | 0.001533550 |
| 0.8314704 | 0.8327530 |   auc_agg_rf | 0.001542528 |

Ultimately, seg_rf does better than agg_rf, should we use it instead?
When do we use 


## 20201009

Scripted up Fallback table generation
Generated predictions w/ and w/o fallback for al models
can do metlarning now

but would like to do metalearning to determine when to use fall backs 
for this, create a new column with predicitons generated by each fall back table

## 20201008

***what are the 391 domain_id==NA in predicitons_df?*** -- DONE!
    - tld_res combinations in tain with no obs in test data 
***what are the 88 domains in expiry_test not in predictions_df?***
    - those for which training data does not exist

New Data Query - works fine for 2020/10/06-2020/10/10
Fallback generation - works fine, 
Fallback application - works  & understand up until MY Caluclation

## 20201006

Client claims to have used fallback for tld-res <50 or <100 domains
We see a decrease in performance (over not supplementing with fallback) for tld-res <50
Metalearning seg2_glm vs fallback -- need to wrap up feature eng


## 20201001-05

1. domain level predictions DONE
    - https://machinelearningmastery.com/threshold-moving-for-imbalanced-classification/
2. groundwork for glm v. rf v. fall back prediction DONE



## 20200929

- Random forest for metalearning multicalss
    - AUC_WIN Accuracy : 0.3821  (0.3164, 0.4511)
    - L10_WIN Accuracy : 0.5943  (0.5249, 0.6611)
    - *NOTE: geo variables rank very low on variable importance... trry country variables instead?*
        - AUC_WIN Accuracy : 0.3915  (0.3254, 0.4607)
        - L10_WIN Accuracy : 0.5943  (0.5249, 0.6611)
        - *country_maj now /#3 most important variable! though accuracy hasn't improved much*
    - BINARY: AGG_RF > SEG2_GLM : 
- Run through agg_rf vs. seg2_glm for 
    - DTree, 
    - XGBoost, 
    - SVM [link](https://www.ana

## 20200927

First pass at using Logistic regression to predict renewals by tld-registrar/reseller

## 20200926

- Multiclass w/ reseller_geo 
    - DTree, 
    - XGBoost, 
    - SVM [link](https://www.analyticsvidhya.com/blog/2016/08/practicing-machine-learning-techniques-in-r-with-mlr-package/)

- Update client with metrics
- Leverage metalearning insights for prediciting renewals - which model to use when

- ALSO: 
    - predict all non-renewals by tld & registrar-- eliminate 665 NA-metric causing combinations by first pass
    - geo cluseting by performance metrics

## 20200926

Ranger NA predictions DONE why?? no renewals, all non-renewals

## 20200924

Matched reseller_country to fallback table country DONE

## 20200829

Finished munging output metrics, ready for feature engineering & metalearning

## 20200828

Completed tld_reseller combonations 0001-0817
However, bug in underlying data of seg_rf (used tld-granular data). So reruning just seg_rf and saving list in separate file 

Ran all results, munged them to create single list of dataframes containing predicted values at the tld_reseller level for the following 6 models:
- seg2_glm
- seg_glm
- agg_glm
- seg2_rf
- seg_rf
- agg_rf


## 20200827

Previous metalearning experiement at reseller level only had some ~200 observations to work with (~200 unique resellers)
Reruning metalearning at finer granularity -- tld-reseller level -- lending itself to 1723 observations (1723 unique tld-reseller combinations)
Instead of returning the resulting lift_df... return the predicted value df (pred_df)
Also running for agg models

## 20200809-10

CONCLUSIONS SO FAR:
- seg2_glm does better than agg_rf (for ~ 50% of resellers (AUC)) where ONE of the following conditions hold
    - the primary reseller country  is one of 
      - Belgium, India, Ireland, Israel, Lithuania, Luxembourg, Portugal, Russia, Singapore, Sweden, Turkey, USA
    - if not those countries, then if count of domains <15
- agg_glm does better than seg2_glm which does better than agg_rf (for ~ 8% of resellers (AUC)) where BOTH conditions hold 
    - the primary reseller country  is one of 
      - Argentina,	Bangladesh,	Belarus,	Brazil,	Finland,	Gibraltor,	Greece,	Hong Kong,	Hungary,	Indonesia,	Italy,	Malaysia,	New Zealand,	Norway,	Others,	Portugal,	Russia,	Singapore,	Southafrica,	Thailand,	Turkey,
    - AND average day_domains is <5.87
    
- MULTICLASS MODEL


TODO:
- When does agg_glm > seg2_glm > agg_rf?
- Segregate modeling based on metalearning reasults (so agg_rf>seg2_glm when country not in list and count>15)
- Should we do this on a tld-reseller level? MORE DATA!!! but first, confirm that the above seg actually improves results

From Parag:
- Fall Back tables (incorporate into the analysis -- when is fallback table > models?)
    - dropbox folder 
    - fallback table creation use a data frame called npv_historic_renewal_data (basically a data frame we get from the get_expiry_data function with the reseller_geo field added and the reg_arpt field cut into slabs to create look keys). uploaded that data frame as an rds as well for your reference
- Gibb-Score 
    - bigquery dataset new table: prediction_vendors.npv_prepped_data
- anomaly registrar notes
    - https://docs.google.com/spreadsheets/d/1ybBC_KLGKYG3D54px3e2k0LP0LF5FjJ2uMAXTAUnzuU/edit#gid=427645846
- Data provider data
    - bigquery table prediction_vendors.dp_unique
    - query file to access the data provider data under the Data Provider folder in the dropbox folder shared with you



## 20200808

Finished feature engineering for reseller level comparison
Kicked off modeling effort for meta learning -- need to interpret but pretty much, trying to learn lift@10 is impossible but reseller country majority and count are major predictors of auc when examining agg_rf vs. seg2_glm. Interestingly, seg2_glm does better when count is <15!

still need to figure out why reweighting doesn't help. maybe try again now with the factor issue resolved?

interestingly, agg rf does better than seg2 rf... but this is in line wirh 

## 20200807

Compiled results for all 309 resellers
Brainstormed some more reseller level metrics for metalearning (need to implement)
Added agg_glm and agg_rf (i.e. single model across all resellers and tlds) reseller-level results to compiled results

Next: do more feature enginering as per plan & train a model? compare seg2 glm & rf

## 20200806_2

running reseller-compare script for the rest of the resellers (51:n() in order of domain count)
added agg_glm and agg_rf to metrics via reseller_compare.ipynb


## 20200806

Investigating cases where RF bets GLM 
- smaller observation counts (auc only)
- very small proportion of renewals, i.e. highly unbalanced (both metrics)
- higher number of tld counts (auc only)
- china resellers (more so for auc than lift_10)

Can we somehow do this with a predictive model?
How can we incorporate distributional information?
Look into China observation in more detail
Investigate resellers eranet & zhengzhou




## 20200725

Leveraged: https://win-vector.com/2016/05/30/on-ranger-respect-unordered-factors/
tested respect.unordered.factors=TRUE on a gmo and namecheap reseller segments -- rando forest a lot better!!!
Reran full RF w/ respect.unordered.factors=TRUE

|   model               |   AUC             | lift_10  | filename_model                   | filename_testpredict       |
|-----------------------|-------------------|----------|----------------------------------|----------------------------|
|   seg_glm             | 0.816507088933898 | 4.708893 | first_renewal_model_expiry_train | seg_glm_expiry_exptest     |
|   agg_rf              | 0.759681458294973 | 3.973486 | ranger_03_expiry2                | predict_ranger_03_expiry2  |
| **agg_rf_f            | 0.832666175658258 | 4.837415 | ranger_03_expiry2_f              | predict_ranger_03_expiry2_f**|
|   seg_glm+agg_rf_15   | 0.796116737249125 | 4.198490 |                                  |                            |
|   seg_glm+agg_glm_15  | 0.82069048057448  | 4.645185 |                                  |                            |
|   agg_glm_basic       | 0.812320014730252 | 4.673173 | agg_glm_basic_model              |  agg_glm_basic_pred        |
|   agg_glm_plus        | 0.812076965568035 | 4.661020 | agg_glm_plus_model               |  agg_glm_plus_pred         |


## 20200724

1. Explored additional vars: can pull creation time, convert to local time or use as proxy for country
2. Feature engineered new vars based on expiry_date and creation_date
3. reran agg glm pre & post addition of new variables
    - w/ NA as estimated coeff for reg_durY : “prediction from a rank-deficient fit may be misleading”
    - lower preformance may be due to reg_durY, removing this var


4. rerun agg_glm_plus w/o reg_durY?



1. try seg_glm with just reseller level partitions
2. Aside from small data segments, on what kind of segments does random forest outperform glm?
3. Statistically, where is random forest more approriate than glm?
4. Is segmentation on tld-registrar level necessary? Can we get by with registrar level segmentation, incorporating tld as a predictor?



## 20200723

1. Verified both prepped expiry & expiry glm test output is segmented on tld-reseller
2. Incorporated reseller (not registrar) as a predictor in RF
3. Reran through 07/22 analysis with reseller subbed for registrar
4. Validated seg_glm performance from scratch on tld_registrar sitegmo
5. Can we structure segmented glm based on random forest determined segments?

|   model               |   AUC             | lift_10  | filename_model                   | filename_testpredict       |
|-----------------------|-------------------|----------|----------------------------------|----------------------------|
|   seg_glm             | 0.816507088933898 | 4.708893 | first_renewal_model_expiry_train | seg_glm_expiry_exptest     |
|   agg_rf              | 0.759681458294973 | 3.973486 | ranger_03_expiry2                | predict_ranger_03_expiry2  |
|   seg_glm+agg_rf_15   | 0.796116737249125 | 4.198490 |                                  |                            |
|   seg_glm+agg_glm_15  | 0.82069048057448  | 4.645185 |                                  |                            |

**Note: `test_data$first_renewal_prediction[test_data$Status == "Deleted"]<-0` there is no column Status so this doesn't do anything**



## 20200722

moving forward with fusion approach that combines segmented glm and aggregate random forest

1. Does aggregate glm outperform aggregate random forest? at slit of top 15 tld_registrars - ***YES***
    a) agg_glm note: set registrars as N/A for ones in test data not present in training data
    b) seg_glm + agg_rf generated 1647 fewer rows than expected. seg_glm + agg_glm met expectations -- missing from traning data
    c) seg_glm + agg_rg: AUC 0.795160213618157; lift_10 4.240840 (w/ reg not res)
    c) seg_glm + agg_glm: AUC 0.811307309887682; lift_10 4.299024
    d) seg_glm: AUC 0.816507088933898; lift_10 4.708893
    e) **tld_registrar_index is actually a concatenation of tld and reseller** Need to rerun above




## 20200720_2

trying segmented RF 

top 15 tld_registrars by domain count contain 48% of all domains... identical performance across models
seg_glm: auc=0.83, lift_10=4.7
rf: auc=0.68, lift_10=2.3


combining seg_glm for top 15 rld_registrars & rf for the  other 51.6% of domains leads to an auc lower (.8) than dor seg_glm alone (.83)

top 100 ultimate does better in AUC (but slightly worse on lift) (account for 84% obs)
.8198 vs .8165 AUC & 4.386 vs. 4.709 lift_10

upsample
.8183 vs .8165 AUC & 4.356644 vs. 4.709 lift_10

smote, upsampling degrades performance

top 150 (account for 89% obs)
0.823117608168662 AUC, 4.5 lift_10

top 200 (account for 92%)
0.823790438018895, 4.5 lift_10

top 300 (account for 96%)
0.826105524921613, 4.6 lift_10



## 20200720

because auc is not calculated for very small registrars, ranger actually outperforms seg_glm on most registrars (based on accuracy) (86 versus 51) but still lags behind in terms of domain count (since these are small registrars) 

seg_glm outperforms ranger_03 on 51 registrars:  0.3290323 
ranger_03 outperforms seg_glm on 86 registrars:  0.5548387 

seg_glm outperforms ranger_03 on 152087 domains:  0.5124277 
ranger_03 outperforms seg_glm on 133261 domains:  0.4489971 


On the  114  registrars w/ less than  600  domains renewing.... (note higher count due to new performance metirc)

seg_glm outperforms ranger_03 on 28 registrars:  0.245614 
ranger_03 outperforms seg_glm on 69 registrars:  0.6052632 

seg_glm outperforms ranger_03 on 3009 domains:  0.3418153 
ranger_03 outperforms seg_glm on 5485 domains:  0.623083 




### 20200718-9

Examining performance on a regstrar level, as per client's recommendation

seg_glm fails to generate a prediction for  .5% of observationsl, resulting in a slightly lower AUC (.817 vs .823) and lift at 10% (4.71 vs. 4.74)

random forest generates a prediction for all observations but does this overall with lower accuracy (much lower true positive rate but slightly higher true negative rate)

seg_glm does better on larger registrars whereas ranger_03 does better on smaller registrars. 

seg_glm outperforms ranger_03 on 58 registrars:  0.5225225 
ranger_03 outperforms seg_glm on 47 registrars:  0.4234234 

seg_glm outperforms ranger_03 on 218975 domains:  0.7385628 
ranger_03 outperforms seg_glm on 77438 registrars:  0.2611843 

On the  70  registrars w/ less than  600  domains renewing....

seg_glm outperforms ranger_03 on 29 registrars:  0.4142857 
ranger_03 outperforms seg_glm on 35 registrars:  0.5 

seg_glm outperforms ranger_03 on 3791 domains:  0.446315 
ranger_03 outperforms seg_glm on 4628 domains:  0.5448552 

Investigate 44 registrars which have missisng perf comparisons -- these are the ones where seg_glm produced NA's/ obs count was too small to calc AUC

### 20200717

Ranger_03 tested in training data
but dtree_xl 1 &2 had good perf on test data 
but w/ leaked variables. now testing w/o leaked varibales
good performance was only because working on limited test data (after having removed all rows where any variable is na)
shitty performance xltree_03, in dtree_xl_2.ipynb

now trying to salvage ranger by doing one last hyper param tuning of class.weights

for some reason, class.weights have NO IMPACT on Sensitivity/Specificity/etc. even at extreme settings like c(1,1000)

Ultimately, training ranger_03 with default hyper parameters resulted in higher lift (from 4.0 to 4.3) on new data



why were so many missing values generated in the mass_data_prep() of expiry data?

0. Choose optimal cutoff point [link](http://ethen8181.github.io/machine-learning/unbalanced/unbalanced.html)



### 20200716 (2core 16 GB machine)



Need to geerate performance metrics for the following models

|               |           | tr_phase1, pf_phase1             | tr_phase1, pf_expiry      | tr_expiry, pf_expiry                 | 
|---------------|-----------|----------------------------------|---------------------------|--------------------------------------|
|**MODELS**     | seg_glm   | first_renewal_model_train        | first_renewal_model_train | first_renewal_model_expiry_train     |   
|               | rf_500    | ranger_03_                       | ranger_03_                | ranger_03_expiry                     |  
|---------------|-----------|----------------------------------|---------------------------|--------------------------------------|
| **PREDS**     | seg_glm   | first_renewal_model_test_predict | seg_glm_exptest           | seg_glm_expiry_exptest               |   
|               | rf_500    | predict_ranger_03_               | ranger_predict_03_exptest | ranger_predict_03_expiry             |   
|---------------|-----------|----------------------------------|---------------------------|--------------------------------------|
| **LIFT_DF**   | seg_glm   | lift_df_segglm                   | lift_df_seg_glm_exptest   | lift_df_seg_glm_expiry_exptest       |   
|               | rf_500    | lift_df_ranger_03_               | lift_df_ranger_03         | lift_df_ranger_03_expiry             |   

Metrics include: Gains Curves; Lift values; ~~for each tld-registrar-reg_arpt: sum(prob) vs count (renewed)~~ Confusion Matrix


### 20200715_2 (4core 32GB machine)

Re-Training RF on new data

Prep training data (data: expiry_train_prepped_2, expiry_test_prepped_2)
real    159m36.456s
user    177m20.376s
sys     0m58.628s

Retrain Ranger_03 (model: ranger_03_expiry; lift: lift_df_ranger_03_expiry)
model fitting: 952.151 sec elapsed
model saving: 163.762 sec elapsed
model predict: 65.851 sec elapsed
eval saving: 0.132 sec elapsed

Retrain seg_glm (model: first_renewal_model_expiry_train)
model fitting: 38.794 sec elapsed
model saving: 48.062 sec elapsed

now only 0.3% missing predicted values

Lifts at 10% very close for seg_glm and ranger_03 trained on this new data: **3.9** vs **3.8** 

why is the lift so awesome when only looking at the big registrars?
longer time range? so more data per registrar?

also incorporated reg_arpt into the list of predictors




### 20200715

when running mass_predict_first_renewal() on list of df's w/ new expiry data, got error 

Column 37 ['cluster'] of item 3 appears in position 36 in item 2. Set use.names=TRUE to match by column name, or use.names=FALSE to ignore column names. use.names='check' (default from v1.12.2) emits this message and proceeds as if use.names=FALSE for  backwards compatibility. See news item 5 in v1.12.2 for options to control this message.

Looks like columns pattern and cluster as swapped for ["websitezhengzhou century connect"]
added use.names=TRUE argument to rbind list call of mass_predict_first_renewal()

This did the trick....

Predict results in 63% missing predicted values (these would need to be looked up using lookup tables)

Lift @ 10% is **2.2** and then quickly nosedives due to inability to predict for untrained tldXregistrar combos.

Ultimately, client uses lookup tables to fill in the predictions for these missing values. Asked for script.




### 20200714 (2core 16GB machine)

prepping 20% subset of new expiry data for run through model
finished. 81 less observations 12 more columns

dim(expiry_test)
dim(expiry_test_prepped)
[1] 348672     27
[1] 348591     39

real    28m12.419s
user    31m21.036s
sys     0m13.256s

model predict: 14.285 sec elapsed
eval saving: 0.009 sec elapsed

No magic here, lift of **1.8*** on 20% subset of new data vs. **6.3*** on test data
Seg_glm achieved lift on 3.3 on test data. how well does it do on new data?

but really, didn't implement fll scope of data prep (where a list of dataframes is created)
not sure if it's equivalent so regenerating prepped test data as list of dfs

also, tetsing performance of seg_glm on this new data

real    58m23.127s
user    72m5.680s
sys     0m13.688s




### 20200705

get_expiry query is working.  1,743,356 rowsrows for expiry_dates in Jan01-May31 2020
QAing get_expiry_data.R



### 20200628

completed QA of decision tree.

### 20200628

Reran hypertuned RF on subset not containing potential data leaks, achieved .905 AUC, 6.33 lift at 10% (compared to seg_glm 0.734, 3.30 lift at 10%).
Still need to QA for leaked vars, model issues, etc.

Analyzed maxdepth=15 tree. Performs better on test because it doesn't overfit. Is 1/3 the size od xl_dtree based on numberof terminal nodes. Getting list of splitting rules via partykit:::.list.rules.party(). saved to gs://data_outputt/output/xltree_01_d15_rules.

Fed train & test data through a node on the tree and didn't get the right number of observations.

### 20200627

New data queries need a lot of work, backburning/offloading.

For now:
- QA xl_tree with test data. 
    - Pick node with highest conversion and manually calc renewal rate for that node.
        * identified node, getting splitting rules
- correlation analysis for 20-some variables in DTree/RF
- run RF with just the vars in seg_glm -- compare performance. 
- other ways to identify "leaky" variables [link](https://www.researchgate.net/publication/221653692_Leakage_in_Data_Mining_Formulation_Detection_and_Avoidance)

Questions for client:

1. reg_arpt_org: what does it mean for it to be negative? 
    - calculated as: CASE WHEN n.newreg_period is not null THEN round((n.newreg_net_revenue / n.newreg_period), 2) ELSE 0 END
    - defined ad "original price of registration of the domain"
    - however, negative values exist. haow can original price be negative? (net revenue can surely be negative)
            Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
            -35.0000   0.4400   0.5000   0.9828   0.5900  59.0000 
    - should it be the otherway around from what the datadisctionary says? ie.
        * reg_arpt_org: revenue per year of registraton. = reg_revenue/reg_period
        * reg_arpt: original price of registration of the domain
        * this is confirmed by the definition of log_reg_arpt as the log of the original registration price i.e. log(reg_arpt)
        * BUT! reg_arpt = arpt = Round(( n.net_revenue / n.period ), 2) according to *get_expiry_data.R* how can this be the orig price? 
2. what is the definition/calculation of 'pattern_score' variable?
3. what is the definition/calculation of 'cluster' variable?

RERUN hyperparam-tuned RF w/o  reg_revenue, reg_arpt_org, reg_arpt, log_reg_arpt, pattern_score, cluster, expiry_M , creation_M, expiry_Y , creation_Y, (though these temporal vars may be interesting), 



### 20200626_2 NOTES

QA decision tree on existing data (test)
pipe through high renewal node and calc actual renewal
missing values?
corr variables

variable importance alone could be very useful -- how is client putng this kind of info to use?

deliver excellent lift + "time off" in one go. 

### 20200626

QAing DTree/RF results by 
1. setting up framework for pulling new data
2. picking a DTree node/segment with high renewal
2. "manually" calc predicted renewal for that segment -- match model?

are there "leaked" variables?
correlated with renewal?

ISSUES TO BRING UP WITH CLIENT:
1. no variable 'id' in renews table (n) or newreg table (n). instead, there exist a domain_id and a renewal_id variable. Since in get_expiry_data query, the id variable in renews get joined with with domain_id variable in predictions, I'm assuming that we should be referencing the domain_id varibale in renews. (Other 'id' vars in newreg:'domain_id' 'registrar_userid' 'registrantid' 'r_row_id' 'r_renewal_id')

2. no variable 'type' in predictions table (pr). There are no seemingly comperable variables and it looks to be pretty crucial in the query. I've removed the corresponding WHERE clauses for now for now. Maybe the tables in bigquery have already been subset appropriately?

3. variable 'deleted_date' in renews table (n) and newreg table (n) is a string and doesn't comprare well with transaction_expiry/expiry_date in the appropriate tables  which are both datetime. Converted deleted_date to DATETIME in comparison via PARSE_DATETIME('%Y-%m-%d', n.deleted_date).

4. no variable 'r_transfer' in newreg table (n) (but there is such a column in renews table). There are no seemingly comperable variables and it looks to be pretty crucial in the query. I've removed the corresponding clauses for now. Please let me know if "Transferred" domains are of interest in this project and, if so, how to obtain this info. 

5. no variable 'r_renewed_on' in renews table (n) OR newreg_table (n). But r_renew_date variable does exist in both tables-- is this what you meant? There's also a renew_date variable in the renew table. For now, I am using r_renew_date instead of r_renwed_on when it comes to both tables in the query. Please let me know if I should proceed otherwise. 

6. no variable 'sld_length', 'sld_type', 'sld_type2', 'day_domains' in predictions table (pr). I've removed these from the query. Please let me know how I can go about obtaining them.

7. no variable 'icann_fixed' in newreg table (n) or renews table (n). I've removed these from the query. Please let me know how I can go about obtaining them.

8. no variable 'registered_on' in renews table (n). I've removed this from the query. Please let me know how I can go about obtaining it.

9. no variable 'r_transfer' in renews table (n) or in newreg table (n). There is however a 'transfer' variable in renews table ONLY. I've removed the newreg table's references to 'r_transfer' and replaced the renews table's references with 'transfer'. Please let me know if I should proceed otherwise. 


10. no variable 'expiry_date' in renews table (n) (but it does exist in newreg table (n)).  I've removed the renews table's references to 'expiry_date'. Please let me know if I should proceed otherwise. 

11. variable 'r_period' is a string in renews table. Converted to int64 using cast(n.r_period as int64) for arithmetic operations. 

12. variables 'r_net_revenue', 'r_period','r_centralnic_comm', 'r_icann_comm' are string in renews table.  Converted to float using cast(n.r_net_revenue as float64) for arithmetic operations. 

13. no variable 'r_centralnic_commas' in renews table (n) or in newreg table (n).  There are no seemingly comperable variables. It looks crucial for calculating gross profit but I've gone ahead and removed all references to 'r_centralnic_commas', eliminating the calculation of variables 'renew_gp_less_icann_fixed' and 'renew_gross_profit'. Please let me know how these can be obtained.

14. no variable 'creation_date' in one of the tables (renews), I removed references to it in with respect to both of the 'n' tables in order to make the union work. Please let me know how I can obtain this info. 

15. converted 'renewed_count' value of 1 to int in first SELECT clause so the uNION ALL would work with type int in second SELECT clause. 

16. converted 'r_mbg' to int in renews SELECT clause so the uNION ALL would work with type int in newreg SELECT clause. 

17. converted 'r_renew_date' to DATETIME via PARSE_DATETIME('%Y-%m-%d', n.r_renew_date) in renews SELECT clause so the uNION ALL would work with type DATETIME in newreg SELECT clause. 

### 20200625

Finished hyper param tuning in high memory 8-core machine.
Finished optimal random forest.

***random forest is far less tunable than other algorithms such as support vector machines*** [link](https://arxiv.org/pdf/1804.03515.pdf)

***numtree is not a tuning param: higher values are generally preferable to smaller values with respect to performance*** [link](https://arxiv.org/pdf/1804.03515.pdf)

[tuneRanger](https://arxiv.org/pdf/1804.03515.pdf)

Best params to tune according to Probst et al. (2018):
1. mtry provides the biggest avg improvement of the AUC (0.006)
2. sample size provides the 2md biggest avg improvement of the AUC  (0.004)
3. Changing the replace parameter from drawing with replacement to drawing without replacement  had a small positive effect (0.002)
4. node size had only a small effect (0.001)

Similar results were observed in the work of van
Rijn and Hutter (2018). As outlined in Section 2.1.4, the number of trees cannot be seen as tuning parameter: . If the performance of RF with default
values of the hyp

Notes: 
increasing mtry beyond 11 or so is v. memory intensive
we can get gains by staying <11 increasing sample_size
it looks like, holding mtry constant, incr sample_size decr error
so, expand the grid to include sample_size beyon .8 (also .9,1)
what about sampling w/ replacement? 

Notes about splitting:
- Gini impurity & weighted variance methods both favor continuous and many-level factors (over, say binary vars). Ranger grows probability classification trees as regression trees and splits based on estimated response variance
- To increase computational efficiency, splitting rules can be randomized (Geurts et al., 2006). To this end, only a randomly selected subset of possible splitting values are considered for a variable. The size of these subsets is specified by the hyperparameter **num.random.splits** in ranger. If this value is set to 1, this variant is called extremely randomized trees (Geurts et al., 2006). In addition to runtime reduction, randomized splitting might also be used to add a further component of randomness to the trees, similar to mtry and the sample size.

Other vars for hyperparam tuning:
- When tests are performed for split selection, it may only make sense to split if the p-values fall below a certain threshold, which should then be considered as a hyperparameter. In ranger the hyperparameter **alpha** is the p-value threshold.

### 20200624

- brush up on ranger package & hyper parameters
    - [link](https://arxiv.org/pdf/1508.04409.pdf)
        - optimized for high dimensional (long & wide) data by extensive runtime and memory profiling.
        - idetified bottlenecks & optimied algos for diff variable types
        - biggest bottleneck: node splitting: algos used instead of eval all values of all mtry candidate features
        - second bb: "drawing the mtry candidate splitting features in each node". 
            - sampling w/o replacement via Knuth
        - mem efficiency: avoiding copies of the original data, 
            - saving node information in simple data structures and freeing memory early, where possible.
            
    - parameters [link](https://uc-r.github.io/random_forests#tune) [link](https://arxiv.org/pdf/1804.03515.pdf)
    
        num.trees = 500,
            number of trees. 
            We want enough trees to stabalize the error but using too many trees 
            is unncessarily inefficient, especially when using large data sets.
            
        mtry = NULL, (sqrt number of cols.. sqrt(23)=4.8)
            the number of variables to randomly sample as candidates at each split. 
            When mtry=p the model equates to bagging. 
            When mtry=1 the split variable is completely random, so all variables 
            get a chance but can lead to overly biased results. 
            A common suggestion is to start with 5 values evenly spaced across the range from 2 to p.
            
            Trade-off between stability and accuracy of the single trees.
            Lower values of mtry:
                more different, less correlated trees -> better stability when aggregating. 
                better exploit variables with moderate effect on the response variable, 
                that would be masked by variables with strong effect if those had been candidates 
                for splitting. 
                perform on average worse, since they are built based on suboptimal variables 
                (that were selected out of a small set of randomly drawn candidates): 
                possibly non-important variables are chosen. 
            
            the real number of relevant predictor variables highly influences the optimal mtry:
                IF many relevant predictor variables THEN mtry should be set small s.t. 
                not only the strongest influential variables are chosen in the splits.
            
                IF only a few relevant variables out of many THEN mtry should be set high s.t. the 
                algorithm can find the relevant variables. A large mtry ensures that there is (with 
                high probability) at least one strong variable in the set of mtry candidate variables.
            
            Computation time decreases approximately linearly with lower mtry values
            
        replace = TRUE, # do we have to set this to false for sample.fraction to come into play? 
                        # see if hyperparam tunning leads to same vlues across diff values of sample.fraction
        sample.fraction = ifelse(replace, 1, 0.632),
            (sampsize): the number of samples to train on. 
            The default value is 63.25% of the training set since this is the expected value of 
            unique observations in the bootstrap sample. 
            Lower sample sizes can reduce the training time but may introduce more bias than necessary. 
            Increasing the sample size can increase performance but at the risk of overfitting 
            because it introduces more variance. Typically, when tuning this parameter we stay 
            near the 60-80% range.
            
            a similar effect as the mtry parameter
            
            Decreasing the sample -> more diverse trees -> lower correlation b/w trees -> 
            positive effect on the prediction accuracy when aggregating the trees
            (However, the accuracy of the single trees decreases, since fewer observations 
            are used for training.)
            
            trade-off between stability and accuracy of the trees
            
            optimal value is problem dependent
            no substantial perf difference b/w sampling w/ or w/o replacement when parameter is set optimally
            w/ replacement may induce a slight variable selection bias when categorical variables 
            with varying number of categories are considered.
            
            

        min.node.size = NULL, (10. 1: classification, 5: regression, 3: survival, 10: probability.)
            minimum number of samples within the terminal nodes. 
            Controls the complexity of the trees. Smaller node size allows for deeper, 
            more complex trees and smaller node results in shallower trees. 
            This is another bias-variance tradeoff where deeper trees introduce more variance 
            (risk of overfitting) and shallower trees introduce more bias 
            (risk of not fully capturing unique patters and relatonships in the data).
            
            defaults usually good.
            higher number of noise variables -> higher optimal node size.
            
            Computation time decreases ~exponentially with increasing node size.
            suggestion: set this parameter to a value higher than the default

        importance = "none",
        write.forest = TRUE,
        probability = FALSE,
        max.depth = NULL,
        
        case.weights = NULL,
        class.weights = NULL,
        splitrule = NULL,
        num.random.splits = 1,
        alpha = 0.5,
        minprop = 0.1,
        split.select.weights = NULL,
        always.split.variables = NULL,
        respect.unordered.factors = NULL,
        scale.permutation.importance = FALSE,
        local.importance = FALSE,
        regularization.factor = 1,
        regularization.usedepth = FALSE,
        keep.inbag = FALSE,
        inbag = NULL,
        holdout = FALSE,
        quantreg = FALSE,
        oob.error = TRUE,
        num.threads = NULL,
        save.memory = FALSE,
        verbose = TRUE,
        seed = NULL,
        dependent.variable.name = NULL,
        status.variable.name = NULL,
        classification = NULL,
        x = NULL,
        y = NULL
        

- plan for execution of hyper param tuning
    - mtry: Since ranger is optimized for high mtry values , 
        why not use more than sqrt(ncols) for each tree? or would it be better to keep mtry 
        low and increase th enumber of trees?
     

- brush up on hyper param tuning in R
    - [link](https://uc-r.github.io/random_forests#tune)
    

- IMPROVE on gibersih score.. disect domain alone
- how many conconants in a row
- raio of con to vow
- "word morphology"
- does it have rare letters
- third party tool for pronouncability
- feed into [decision tree/neuralnetwrok] with churn as the predicted. inut predicted score as another variable into rando forest model

- predicting premium domains pricing



### 20200623_2

WOW! random forests via ranger perform phenomenaly. .89 AUC (vs .73 AUC for their seg_glm model). also super fast -- just 10 trees fitted to max depth in 46 seconds (instead of 46 min for max depth single tree using ctree)

Next: investigate assumptions. what is maxdepth really? why i it so much faster? what other default parameters? also invetigate variable importance. maybe more trees. hyper param tuning. 

### 20200623

Evaluated max depth ctree & pre-pruned max depth ctree with criterion .999. both barely outperform shallow tree (maybe overfitting?) except for when it comes to lift of >5 @ 10%.

next up: random forests.

Parameter Tuning randomForest

Note:  "When RF model explained variance is lower than 40%(seemingly noisy data), one can lower samplesize to ~10-50% and increase trees to e.g. 5000(usually unnecessary many). The ensemble error will converge later as a function of trees. But, due to lower tree correlation, the model becomes more robust and will reach a lower OOB error level converge plateau"

Note: "An article from Oshiro et al. (2012) pointed out that, based on their test with 29 data sets, after 128 of trees there is no significant improvement(which is inline with the graph from Soren)."

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

