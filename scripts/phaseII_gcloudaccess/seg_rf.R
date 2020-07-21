# Rscript seg_rf.R >> /home/jupyter/local/Domains_202003/data/output/seg_rf.log 2>&1
# 8 min on 8 core machine
# error: rbind list, last element (aggregate) has 12 columns

suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(ranger))

# # load & prep input data
source('load_prep_data_expiry.R')


seg_rf <- function (reg_list = c('namecheap', 'gmo', 'alibaba', 'go daddy'),
                    num_trees = 10,
                    train = expiry_train_df_sub,
                    test = expiry_test_df_sub) { 
    


    li = list() 
    for (reg_name in reg_list){
        cat("\nTraining & Testing ",reg_name," forest.\n")
        train_reg <- train %>% filter(registrar==reg_name)
        test_reg <- test %>% filter(registrar==reg_name)

        ranger_03_reg <- ranger(
        formula         = renewal_status ~ ., 
        data            = train_reg, 
        importance      = 'impurity', 
        num.trees       = num_trees,
        probability     = TRUE,
#         mtry            = 3,
#         min.node.size   = 10,
#         replace         = FALSE,
#         sample.fraction = .8,
        seed            = 123
        )

        ranger_predict_03_reg <- predict(ranger_03_reg, 
                                         data = test_reg,
                                         type="response")$predictions
        ranger_predict_03_reg <- as.data.frame(ranger_predict_03_reg)$Renewed

        test_reg$renewal_prediction <- ranger_predict_03_reg

        li[[reg_name]] = test_reg
    }

    cat("\nTraining & Testing remaining aggregate forest.\n")
    train_reg <- train %>% filter(!(registrar %in% reg_list))
    test_reg <- test %>% filter(!(registrar %in% reg_list))

    ranger_03_reg <- ranger(
    formula         = renewal_status ~ ., 
    data            = train_reg, 
    importance      = 'impurity', 
    num.trees       = num_trees,
    probability     = TRUE,
#     mtry            = 3,
#     min.node.size   = 10,
#     replace         = FALSE,
#     sample.fraction = .8,
    seed            = 123
    )

    ranger_predict_03_reg <- predict(ranger_03_reg, 
                                     data = test_reg,
                                     type="response")$predictions

    ranger_predict_03_reg <- as.data.frame(ranger_predict_03_reg)$Renewed

    test_reg$renewal_prediction <- ranger_predict_03_reg

    li[['Other']] = test_reg
    
    test_results = rbindlist(li, fill=TRUE)
    
    return(li, test_results)
    }

list_results, test_results <- seg_rf(reg_list = c('namecheap', 'gmo', 'alibaba', 'go daddy'),
                    num_trees = 1000,
                    train = expiry_train_df_sub,
                    test = expiry_test_df_sub)

write.csv(test_results, "../../data/output/seg_rf_results.csv")
save(list_results, "../../data/output/list_results")

system("gsutil cp /home/jupyter/local/Domains_202003/data/output/* gs://data_outputt/output/ ")
system("mv /home/jupyter/local/Domains_202003/data/output/* /home/jupyter/local/Domains_202003/data/")
