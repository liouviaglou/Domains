# Rscript ranger_hyperparam.R >> /home/jupyter/local/Domains_202003/data/output/hyper_grid_std_exiry.log 2>&1

suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(ranger))

# # load & prep input data
source('load_prep_data_expiry.R')

# hyper param grid construct
hyper_grid <- expand.grid(
  mtry       = seq(1,10),
  sample.fraction = c(.5,.6,.7,.8,.9), 
  replace = c(TRUE,FALSE),
#                     # should we sample w. rep at 1?
#   w1 = c(.1),
#   w2 = 1,
  OOB_RMSE   = 0,
  timestamp = 0
)

# # load completed grid
fname = "/home/jupyter/local/Domains_202003/data/output/hyper_grid_expiry.log"
# hyper_grid_comp <- read.csv(file = fname, header = FALSE)[,2:5]
# names(hyper_grid_comp) <- c("w1","w2","OOB_RMSE","timestamp")

# construct todo grid
# join orig & completed, remove completed rows, rename columns
# hyper_grid_todo <- full_join(hyper_grid, hyper_grid_comp, 
#                                     by = c("w1","w2")
#                                    ) %>% select(w1, w2,OOB_RMSE.y, timestamp.y
#                                                ) %>% filter(is.na(timestamp.y)
#                                                            ) %>% arrange(w1)

hyper_grid_todo <- hyper_grid 
names(hyper_grid_todo) <- c("mtry","sample.fraction","replace","OOB_RMSE","timestamp")
hyper_grid_todo <- transform(hyper_grid_todo, timestamp = as.character(timestamp))

log_con <- file(fname)
# add header: only necessary when constructing new fname
# write.table(data.frame(t(c('row','mtry','sampe_size','OOB_RMSE','timestamp'))), 
#             file = fname, append=FALSE,row.names = FALSE, col.names = FALSE, sep = ",")
for(i in 1:nrow(hyper_grid_todo)) {
    
  ts <- format(Sys.time(), tz="America/Los_Angeles",usetz=TRUE)
  sample.fraction <-  hyper_grid_todo$sample.fraction[i]
  mtry <- hyper_grid_todo$mtry[i]
  replace <- hyper_grid_todo$replace[i]
#   w1 <- hyper_grid_todo$w1[i]
#   w2 <- hyper_grid_todo$w2[i]
    
  cat(paste("\n"))
  cat(paste(ts))
  cat(paste("\n"))
  cat(paste(sprintf("Processing i=%f out of %f; \n", i, dim(hyper_grid_todo)[1])))
  
  
  # train model
  model <- ranger(
    formula         = renewal_status ~ ., 
    data            = expiry_train_df_sub, 
    num.trees       = 500,
#     class.weights = c(3,4),
    probability = TRUE,
    mtry            = mtry,
    min.node.size   = 10,
    replace = replace,
    sample.fraction = sample.fraction,
    seed            = 123
  )
    
#    model <-   ranger(
#     formula         = renewal_status ~ ., 
#     data            = train_df_f_sub, 
#     num.trees       = 500,
#     class.weights = c(w1,w2),
#     probability = TRUE,
#     mtry            = 5,
#     min.node.size   = 10,
#     replace = FALSE,
#     sample.fraction = .725,
#     seed            = 123
# )

    
  ts <- format(Sys.time(), tz="America/Los_Angeles",usetz=TRUE)
  cat(paste(ts))
    
  cat(paste("\n"))  
  cat(paste("\n"))
  
  # add OOB error to grid
  hyper_grid_todo$OOB_RMSE[i] <- sqrt(model$prediction.error)
    
  # add TS  to grid
  hyper_grid_todo$timestamp[i] <- ts
    
  # write to logfile
  write.table(unname(hyper_grid_todo[i,]), file = fname, append=TRUE, sep = ",",row.names = TRUE)
}

