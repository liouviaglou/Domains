# Rscript ranger_hyperparam.R >> /home/jupyter/local/Domains_202003/data/output/hyper_grid_std.log 2>&1

suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(ranger))

# # load & prep input data
source('load_prep_data.R')

# hyper param grid construct
hyper_grid <- expand.grid(
  mtry       = c(4,5),
  sampe_size = c(.725,.75, .775, .90, 1), 
                    # should we sample w. rep at 1?
  OOB_RMSE   = 0,
  timestamp = 0
)

# load completed grid
fname = "/home/jupyter/local/Domains_202003/data/output/hyper_grid.log"
hyper_grid_comp <- read.csv(file = fname)[,2:5]

# construct todo grid
# join orig & completed, remove completed rows, rename columns
hyper_grid_todo <- full_join(hyper_grid, hyper_grid_comp, 
                                    by = c("mtry","sampe_size")
                                   ) %>% select(mtry, sampe_size,OOB_RMSE.y, timestamp.y
                                               ) %>% filter(is.na(timestamp.y)
                                                           ) %>% arrange(mtry
                                                                        ) 
names(hyper_grid_todo) <- c("mtry","sampe_size","OOB_RMSE","timestamp")
hyper_grid_todo <- transform(hyper_grid_todo, timestamp = as.character(timestamp))

log_con <- file(fname)
# add header: only necessary when constructing new fname
# write.table(data.frame(t(c('row','mtry','sampe_size','OOB_RMSE','timestamp'))), 
#             file = fname, append=FALSE,row.names = FALSE, col.names = FALSE, sep = ",")
for(i in 1:nrow(hyper_grid_todo)) {
    
  ts <- format(Sys.time(), tz="America/Los_Angeles",usetz=TRUE)
  mtry <- hyper_grid_todo$mtry[i]
  sample.fraction <- hyper_grid_todo$sampe_size[i]
    
  cat(paste("\n"))
  cat(paste(ts))
  cat(paste("\n"))
  cat(paste(sprintf("Processing mtry=%f; sample.fraction=%f \n", mtry, sample.fraction)))
  
  
  # train model
  model <- ranger(
    formula         = renewal_status ~ ., 
    data            = subset(train_df_f, select = -c(renew_M,renew_Y) ), 
    num.trees       = 500,
    class.weights = c(3,4),
    probability = TRUE,
    mtry            = mtry,
    min.node.size   = 10,
    replace = FALSE,
    sample.fraction = sample.fraction,
    seed            = 123
  )
    
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

