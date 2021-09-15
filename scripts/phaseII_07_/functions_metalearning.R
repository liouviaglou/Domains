suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(ranger))
suppressMessages(library(pbapply))
suppressMessages(library(zoo))
# suppressMessages(library(missRanger))

# load & prep input data
# source('../orig/functions_models.R')

#########################################################################################  
#
# I. TRAIN MODELS, SAVE MODEL OBJECTS
#
#########################################################################################   

# Helper functions

remove_unknown_factor_levels <- function(dat, model) {
    fctr <- which(sapply(dat, is.factor))
    mod_levs <- model$xlevels
    for (col in names(mod_levs)){
#         model$xlevels[[col]] <- union(model$xlevels[[col]], levels(dat[[col]]))
        mask <- (!dat[[col]] %in% mod_levs[[col]])
        dat[mask, col] <- NA
    }
    dat
}


debug_contr_error <- function (dat, subset_vec = NULL) {
  # Credit: https://stackoverflow.com/a/44201384
  # For debugging factor errors
  if (!is.null(subset_vec)) {
    ## step 0
    if (mode(subset_vec) == "logical") {
      if (length(subset_vec) != nrow(dat)) {
        stop("'logical' `subset_vec` provided but length does not match `nrow(dat)`")
        }
      subset_log_vec <- subset_vec
      } else if (mode(subset_vec) == "numeric") {
      ## check range
      ran <- range(subset_vec)
      if (ran[1] < 1 || ran[2] > nrow(dat)) {
        stop("'numeric' `subset_vec` provided but values are out of bound")
        } else {
        subset_log_vec <- logical(nrow(dat))
        subset_log_vec[as.integer(subset_vec)] <- TRUE
        } 
      } else {
      stop("`subset_vec` must be either 'logical' or 'numeric'")
      }
    dat <- base::subset(dat, subset = subset_log_vec)
    } else {
    ## step 1
    dat <- stats::na.omit(dat)
    }
  if (nrow(dat) == 0L) warning("no complete cases")
  ## step 2
  var_mode <- sapply(dat, mode)
  if (any(var_mode %in% c("complex", "raw"))) stop("complex or raw not allowed!")
  var_class <- sapply(dat, class)
  if (any(var_mode[var_class == "AsIs"] %in% c("logical", "character"))) {
    stop("matrix variables with 'AsIs' class must be 'numeric'")
    }
  ind1 <- which(var_mode %in% c("logical", "character"))
  dat[ind1] <- lapply(dat[ind1], as.factor)
  ## step 3
  fctr <- which(sapply(dat, is.factor))
  if (length(fctr) == 0L) warning("no factor variables to summary")
  ind2 <- if (length(ind1) > 0L) fctr[-ind1] else fctr
  dat[ind2] <- lapply(dat[ind2], base::droplevels.factor)
    
  l <- sapply(dat, function(x) length(unique(x)))
  for (i in seq(length(l))) {
      cat(paste0(names(l)[i], ": ", l[i], "\n"))
  }
  ## step 4
  lev <- lapply(dat[fctr], base::levels.default)
  nl <- lengths(lev)
  ## return
  list(nlevels = nl, levels = lev)
}

build_model_first_renewal<-function(train_data,f){

  t <- as.data.frame(train_data)           

  var_mode <- sapply(t, mode)
  print(f)
  model<-glm(f,
             family=binomial(link='logit'),
             data=t, 
             y = FALSE, model = FALSE)
  return(model)
}

predict_first_renewal<-function(test_data, model){
  
    
  if (isTRUE(is.na(model))){
      print('missing model')
      test_data$probabilities<- rep(NA, dim(test_data)[1])
  } else if (is.null(test_data)) {
      print('missing test_data')
      test_data$probabilities<- rep(NA, dim(test_data)[1])
  }  else{
      test_data$probabilities<-predict(model,newdata=test_data,type='response')
  
  # had to comment out the following to get predition list to work
  if("Status" %in% colnames(test_data))
    {
      test_data$first_renewal_prediction[test_data$Status == "Deleted"]<-0
    }
  test_data$first_renewal_prediction<-round(test_data$probabilities,3)
  return(test_data)
  }
}

impute_zero <- function(vec) {
    # For use in feature_creation--imputes zeros
    vec[is.na(vec)] <- 0
    vec
}

feature_creation <- function(dat, drop_rare=TRUE, xlevels=NULL, rel_vars=NULL) {
    # Create features and imputes missing values
    dat <- as.data.frame(dat)
    
    # mxdomain isn't null or domain name
    dat[,'mxdomain_diff'] <- as.integer((as.character(dat$domain) != as.character(dat$mxdomain)) & (!is.na(dat$mxdomain)) & (dat$mxdomain != ""))
    dat[,'nameserver_diff'] <- as.integer((as.character(dat$domain) != as.character(dat$mxdomain)) & (!is.na(dat$mxdomain)) & (dat$mxdomain != ""))

    # Filter to relevant features
    if (!is.null(rel_vars)) {
        dat <- dat[, rel_vars]
    }
    
    # Fix missing values--adds factor or imputes zero
    var_mode <- sapply(dat, mode)
    ind1 <- which(var_mode %in% c("logical", "character"))
    dat[ind1] <- lapply(dat[ind1], as.factor)
    fctr <- which(sapply(dat, is.factor))
    num <- which(sapply(dat, is.numeric))
    dat[num] <- lapply(dat[num], impute_zero)
    
    # Remove infrequent factor levels
    if (drop_rare) {
        min_rate = 0.01
#         min_num = 10
        for (i in fctr) {
            name <- names(dat)[i]
            if (name == "renewal_status") next
#             print(name)
            mask <- dat %>% 
                       group_by(get(name)) %>% 
                       mutate(n=n()) %>% 
                       pull(n) < min_rate * nrow(dat)
#                        pull(n) < min_num
            drop_levels <- unique(dat[mask,i])
            dat[,i] <- droplevels(dat[,i], exclude=drop_levels)
            
            # Replace least frequent factor with NA if no NA
            if (sum(is.na(dat[,i])) == 0) {
                print(name)
                print(sum(is.na(dat[,i])))
                rarest_val <- dat %>% 
                                group_by(get(name)) %>%
                                mutate(n=n()) %>% 
                                ungroup() %>%
                                slice(which.min(n)) %>%
                                pull(get(name))
                dat[dat[[name]] == rarest_val, name] = NA
                print(sum(is.na(dat[,i])))
            }
        }        
    } 
    if (!is.null(xlevels)) {
        for (i in fctr) {
            name <- names(dat)[i]
            dat[[name]] <- factor(dat[[name]], levels=xlevels[[name]])
            if (NA %in% xlevels[[name]]) {
                dat[[name]] <- addNA(dat[[name]])
            }
        }
    }
    dat[fctr] <- lapply(dat[fctr], addNA)
    dat
}

kurtosis <- function(x, na.rm = F) {  
 m4 <- mean((x - mean(x, na.rm = na.rm))^4, na.rm = na.rm) 
 kurtosis <- m4/(sd(x, na.rm = na.rm)^4) - 3  
 kurtosis
}

skewness <-  function(x, na.rm = F) {
 m3 <- mean((x - mean(x, na.rm = na.rm))^3, na.rm = na.rm)
 skewness <- m3/(sd(x, na.rm = na.rm)^3)
 skewness
}

rng <- function(x, na.rm = F) {
    max(x, na.rm = na.rm) - min(x, na.rm = na.rm)
}

not.na <- function(x, na.rm = F) {
    return(sum(!is.na(x)))
}

not.na.perc <- function(x, na.rm = F) {
    return(mean(!is.na(x)))
}

get_formula <- function(df,dp,scope,rf) {
    vars <- c('pattern_domain_count','log_reg_arpt','sld_length','gibb_score','sld_type','day_domains','reg_period')
    if (dp) {
        dp_vars <- c('response','websitetype','economy_footprint','online','ecommerce','ecommercequality','country',
                      'hosting_country','renewalprobability','siccode','sicdivision','sicmajorgroup',#'mxdomain','nameserver',
                      'ssl','renewed_count', 'mxdomain_diff')
        vars <- c(vars, dp_vars)
    }
    if (scope == "agg") { vars <- c(vars, c("tld", "reseller")) }
    else if (scope == "seg") { vars <- c(vars, "tld") }
    else if (scope == "seg2") { }
    else { print(paste("Unrecognized scope", scope)) }
    
    # Character to factor
    df <- as.data.frame(df)
    df <- stats::na.omit(df)
    var_mode <- sapply(df, mode)
    ind1 <- which(var_mode %in% c("logical", "character"))
    df[ind1] <- lapply(df[ind1], as.factor)
    
    # Less than 2 unique values
    fctr <- which(sapply(df, is.factor))
    df[fctr] <- lapply(df[fctr], base::droplevels.factor)
    
    drop_vars <- which(sapply(df, function(x) length(unique(x)) < 2))
    cat(paste0("Drop vars: ", paste(names(drop_vars), collapse=","), "\n"))
    vars <- vars[!vars %in% names(drop_vars)]                          
    cat(paste0("Remaining vars: ", paste(vars, collapse=","), "\n"))
                              
    if (rf & (length(vars) == 0)) vars <- c('response')
    
    right_side <- paste(c("1", vars), collapse="+")
    f <- as.formula(paste0("renewal_status ~ ", right_side))
    f
}
                              
# Generate Metalearning features
gen_meta_df <- function(df, vars=NULL) {
    # Define variables
    if (is.null(vars)) {
        vars= c('pattern_domain_count','log_reg_arpt','sld_length','gibb_score','sld_type','day_domains','reg_period', 
                'response','websitetype','economy_footprint','online','ecommerce','ecommercequality','country',
                'hosting_country','renewalprobability','siccode','sicdivision','sicmajorgroup',
                'ssl','renewed_count', 'mxdomain_diff')
#         vars=c('sld_length', 'day_domains', 'log_reg_arpt', 'response', 'websitetype', 'mxdomain_diff')
    }
    
    # Generate features
    new_df <- feature_creation(df, rel_vars=vars)

    # Summary functions
    summary_funcs <- list(
        min=min,
        max=max,
        mean=mean,
        rng=rng,
        sd=sd,
        skewness=skewness,
        kurtosis=kurtosis,
        not.na=not.na,
        not.na.perc=not.na.perc
    )
    binary_funcs <- list(
        mean=mean,
        not.na=not.na,
        not.na.perc=not.na.perc
    )

    # Create numerical matrix
    
    keep_vars <- which(sapply(new_df, function(x) length(unique(x)) >= 2))
    keep_vars <- names(keep_vars)[keep_vars & (names(keep_vars) %in% vars)]
                              
    print(keep_vars)
    new_df <- new_df[, keep_vars]         
    options(na.action='na.pass')
    newdata <- data.frame(model.matrix(~. + 0, new_df[, keep_vars]))

    # Split into numerical and binary columns
    binary_cols <- newdata %>% summarise_all(n_distinct) <= 2
    binary_df <- newdata[, binary_cols]
    binary_df[, 'tld_registrar_index'] <- df[, 'tld_registrar_index']
    num_df <- newdata[, !binary_cols]
    num_df[, 'tld_registrar_index'] <- df[, 'tld_registrar_index']

    # Create summarized abt
    num_abt <- num_df %>% group_by(tld_registrar_index) %>%
        summarise_all(summary_funcs, na.rm=TRUE) %>%
        ungroup()
    binary_abt <- binary_df %>% group_by(tld_registrar_index) %>%
        summarise_all(binary_funcs, na.rm=TRUE) %>%
        ungroup()
    abt <- merge(num_abt, binary_abt, by="tld_registrar_index")
                              
    # Imputing with missRanger
    print("Imputing with missRanger")
    abt <- missRanger(abt, num.trees=100)
#     abt[is.na(abt)] <- -1
    abt
}

# I. A) AGGREGATE MODELS ################################################################   

train_agg_glm <- function(train_list,tld_reseller_list,dp) {
    # agg glm (aggregarted glm (including tld and reseller as predictors))
    
    train_list = train_list[tld_reseller_list]
    train_df =  rbindlist(train_list,use.names=TRUE)
    
    train_df <- feature_creation(train_df, drop_rare=TRUE)
    
    f <- get_formula(train_df, dp, "agg", FALSE)
    
    model = build_model_first_renewal(train_df,f)

    return(model)
}


train_agg_glm_ALL <- function(train_list,tld_reseller_list,dp) {
    # agg glm (aggregarted glm (including tld and reseller as predictors))
    
    train_list = train_list[tld_reseller_list]
    train_df =  rbindlist(train_list,use.names=TRUE)
    
    
    train_df <- feature_creation(train_df, drop_rare=TRUE)
    
    f <- get_formula(train_df,dp, "agg", FALSE)
    
    model = build_model_first_renewal(train_df,f)

    return(model)
}

train_agg_rf <- function(train_list,tld_reseller_list,dp) {
    # agg rf (aggregarted rf (including tld and reseller as predictors))
    
    train_list = train_list[tld_reseller_list]
    train_df =  rbindlist(train_list, use.names=TRUE)
    
    train_df <- feature_creation(train_df, drop_rare=FALSE)

    if(dim(train_df)[1]==1){
    # if train data only has one observation, 
    # ... sample_fraction must be 1 (cant sample fraction of 1 observation)
        sample_fraction=1
    }else{
        sample_fraction=.8
    }
    
    f <- get_formula(train_df, dp,"agg",TRUE)
    
    suppressMessages(model <- ranger(
        formula         = f, 
        data            = train_df, 
        importance      = 'impurity', 
        num.trees       = 1000,
        probability     = TRUE,
        replace         = FALSE,
        sample.fraction = sample_fraction,
        seed            = 123,
        respect.unordered.factors=TRUE)  )
    
    return(model)
}
   

# I. B) RESELLER MODELS #################################################################

train_seg_glm <- function(train_list, reseller_str, dp) {
    # seg glm (reseller-segmented glm (including tld as predictor))

    # subset data for seg models
    tld_registrars = names(train_list)[endsWith(names(train_list),tolower(reseller_str))]
    train_list_reseller = train_list[tld_registrars]
    train_df_reseller =  rbindlist(train_list_reseller,use.names=TRUE)

    train_df_reseller <- feature_creation(train_df_reseller, drop_rare=TRUE)

    if((nlevels(train_df_reseller$tld) < 2)){
        # if there are not enough tlds to segment-on, 
        # ... do not include tld as predictor
        # ... i.e. build standard Radix model
        f <- get_formula(train_df_reseller, dp, "seg2",FALSE)
    }else{
        f <- get_formula(train_df_reseller, dp, "seg",FALSE)
    }
        
    model = build_model_first_renewal(train_df_reseller, f)

    return(model)
}

train_seg_rf <- function(train_list, reseller_str, dp) {
    # seg rf (reseller-segmented rf)
    
    # subset data for seg models
    tld_registrars = names(train_list)[endsWith(names(train_list),tolower(reseller_str))]
    train_list_reseller = train_list[tld_registrars]
    train_df_reseller =  rbindlist(train_list_reseller,use.names=TRUE)
     
    train_df_reseller <- feature_creation(train_df_reseller, drop_rare=FALSE)
   
    if(dim(train_df_reseller)[1]==1){
        # if train data only has one observation, 
        # ... sample_fraction must be 1 (cant sample fraction of 1 observation)
        sample_fraction=1
    }else{
        sample_fraction=.8
    }
    
    f <- get_formula(train_df_reseller, dp,"seg",TRUE)

    suppressMessages(model <- ranger(
        formula         = f, 
        data            = train_df_reseller, 
        importance      = 'impurity', 
        num.trees       = 1000,
        probability     = TRUE,
        replace         = FALSE,
        sample.fraction = sample_fraction,
        seed            = 123,
        respect.unordered.factors=TRUE)  )
    
    return(model)
}


# I. C) TLD-RESELLER MODELS #############################################################

train_seg2_glm <- function(train_list, tld_reseller_str, dp) {
    # seg2 glm (tld-reseller-segmented glm)
    
    # subset data for seg2 models
    train_list_tld_reseller = train_list[tld_reseller_str]
    train_df_tld_reseller =  rbindlist(train_list_tld_reseller,use.names=TRUE)
    print(colnames(train_df_tld_reseller))
    
    train_df_tld_reseller <- feature_creation(train_df_tld_reseller, drop_rare=TRUE)
    print(colnames(train_df_tld_reseller))

    f <- get_formula(train_df_tld_reseller, dp, "seg2",FALSE)
    print(colnames(train_df_tld_reseller))

    model = build_model_first_renewal(train_df_tld_reseller, f)
    return(model)
    
}

train_seg2_rf <- function(train_list, tld_reseller_str, dp) {
    # seg2 rf (tld-reseller-segmented rf)
    
    # subset data for seg2 models
    train_list_tld_reseller = train_list[tld_reseller_str]
    train_df_tld_reseller =  rbindlist(train_list_tld_reseller,use.names=TRUE)   
    
    train_df_tld_reseller <- feature_creation(train_df_tld_reseller, drop_rare=FALSE)
    
    if(dim(train_df_tld_reseller)[1]==1){
        # if train data only has one observation, 
        # ... sample_fraction must be 1 (cant sample fraction of 1 observation)
        sample_fraction=1
    }else{
        sample_fraction=.8
    }    
    
    f <- get_formula(train_df_tld_reseller, dp,"seg2",TRUE)

    suppressMessages(model <- ranger(
        formula         = f, 
                        data            = train_df_tld_reseller, 
                        importance = 'impurity', 
                        num.trees       = 1000,
                        probability = TRUE,
                        replace = FALSE,
                        sample.fraction = sample_fraction,
                        seed            = 123,
                        respect.unordered.factors=TRUE))
    return(model)
    
}

#########################################################################################  
#
# II. GEN PREDICTIONS w/ MODEL OBJECTS 
#
#########################################################################################   

# I. A) AGGREGATE MODELS ################################################################   

pred_agg_glm <- function(model, test_list, tld_reseller_str) {
    
    print(tld_reseller_str)
    # agg glm (aggregarted glm (including tld and reseller as predictors))
    
    test_list_tld_reseller = test_list[tld_reseller_str]
    test_df_tld_reseller =  rbindlist(test_list_tld_reseller,use.names=TRUE)
    test_df_tld_reseller = feature_creation(test_df_tld_reseller, drop_rare=FALSE, xlevels=model$xlevels)

    # if test data contains no observations, skip!
    if ( (dim(test_df_tld_reseller)[1]==0)|(!exists("model")) ){
        pred_df_agg_glm =  data.frame("actual" = rep(NA, nrow(test_df_tld_reseller)),
                              "predicted" = rep(NA, nrow(test_df_tld_reseller)))
    } else {
        pred = predict_first_renewal(test_df_tld_reseller, model)
    
        pred_df_agg_glm = data.frame("actual" = pred$renewal_status,
                                      "predicted" = pred$first_renewal_prediction)
    } 
 
    return(pred_df_agg_glm)
}

pred_agg_rf <- function(model, test_list, tld_reseller_str) {
    
    print(tld_reseller_str)
    # agg rf (aggregarted rf (including tld and reseller as predictors))
    
    test_list_tld_reseller = test_list[tld_reseller_str]
    test_df_tld_reseller =  rbindlist(test_list_tld_reseller,use.names=TRUE)
    
    test_df_tld_reseller = feature_creation(test_df_tld_reseller, drop_rare=FALSE)

    # if test data contains no observations, skip!
     if ((dim(test_df_tld_reseller)[1]==0) |(!exists("model")) ){
        pred_df_agg_rf =  data.frame("actual" = rep(NA, nrow(test_df_tld_reseller)),
                              "predicted" = rep(NA, nrow(test_df_tld_reseller)))
    }  else {
        pred <- predict(model, 
                        data = test_df_tld_reseller,
                        type="response")$predictions

        # if all Renewed col doesn't exist in predictions, create it with value 0
        if(is.null(as.data.frame(pred)$Renewed)){
            pred <- as.data.frame(pred)
            pred$Renewed <- 0
        }

        pred_df_agg_rf = data.frame("actual" = test_df_tld_reseller$renewal_status,
                          "predicted" = as.data.frame(pred)$Renewed)
    }
    
    return(pred_df_agg_rf)
}
   

# I. B) RESELLER MODELS #################################################################

pred_seg_glm <- function(test_list, tld_reseller_str) {
    
    print(tld_reseller_str)
    # seg glm (reseller-segmented glm (including tld as predictor))
    
    test_list_tld_reseller = test_list[tld_reseller_str]
    test_df_tld_reseller =  rbindlist(test_list_tld_reseller,use.names=TRUE)    

    # if test data contains no observations, skip!
    if ((dim(test_df_tld_reseller)[1]==0) ){
        pred_df_seg_glm =  data.frame("actual" = rep(NA, nrow(test_df_tld_reseller)),
                              "predicted" = rep(NA, nrow(test_df_tld_reseller)))
    } else {
        
        reseller_str = test_df_tld_reseller %>% filter(tld_registrar_index==tld_reseller_str) %>% 
        distinct(reseller) %>% pull(reseller)

        model_name <- paste0('model_seg_glm_',str_replace_all(reseller_str, "[^[:alnum:]]", ""))
        cat(paste0(model_name, "\n"))
        
        if ((!exists(model_name))) {
            pred_df_seg_glm = NA
        } else {
            model <- get(model_name)
            test_df_tld_reseller = feature_creation(test_df_tld_reseller, drop_rare=FALSE, xlevels=model$xlevels)
            pred = predict_first_renewal(test_df_tld_reseller, model)

            pred_df_seg_glm = data.frame("actual" = pred$renewal_status,
                                      "predicted" = pred$first_renewal_prediction)
        }


        
    } 
    return(pred_df_seg_glm)
}

pred_seg_rf <- function(test_list, tld_reseller_str) {
    
    print(tld_reseller_str)
    # seg rf (reseller-segmented rf)
    
    test_list_tld_reseller = test_list[tld_reseller_str]
    test_df_tld_reseller =  rbindlist(test_list_tld_reseller,use.names=TRUE)
    test_df_tld_reseller = feature_creation(test_df_tld_reseller, drop_rare=FALSE)
    
     # if test data contains no observations, skip!
    if ((dim(test_df_tld_reseller)[1]==0)) {
        pred_df_seg_rf = NA
    } else {
            
        reseller_str = test_df_tld_reseller %>% filter(tld_registrar_index==tld_reseller_str) %>% 
        distinct(reseller) %>% pull(reseller)

        model_name <- paste0('model_seg_rf_',str_replace_all(reseller_str, "[^[:alnum:]]", ""))
        cat(paste0(model_name, "\n"))
        
        if ((!exists(model_name))){
            pred_df_seg_rf =  data.frame("actual" = rep(NA, nrow(test_df_tld_reseller)),
                              "predicted" = rep(NA, nrow(test_df_tld_reseller)))
        } else {
            
             model <- get(model_name)
             pred <- predict(model, 
                    data = test_df_tld_reseller,
                    type="response")$predictions
            

            if (ncol(pred) < 2) {
                preds <- rep(0, nrow(test_df_tld_reseller))
            } else {
                preds <- as.data.frame(pred)[, 2]
            }
            
            pred_df_seg_rf = data.frame("actual" = test_df_tld_reseller$renewal_status,
                          "predicted" = preds)
            
        }

       

         } 
    return(pred_df_seg_rf)
}


# I. C) TLD-RESELLER MODELS #############################################################

pred_seg2_glm <- function(test_list, tld_reseller_str) {
    
    print(tld_reseller_str)
    # seg2 glm (tld-reseller-segmented glm)

    test_list_tld_reseller = test_list[tld_reseller_str]
    test_df_tld_reseller =  rbindlist(test_list_tld_reseller,use.names=TRUE)
    
    # if test data contains no observations, skip!
    if ((dim(test_df_tld_reseller)[1]==0)){
        pred_df_seg2_glm = NA
    } else {
        
        model_name <- paste0('model_seg2_glm_',str_replace_all(tld_reseller_str, "[^[:alnum:]]", ""))
        
        if ((!exists(model_name))){
            pred_df_seg2_glm = data.frame("actual" = rep(NA, nrow(test_df_tld_reseller)),
                              "predicted" = rep(NA, nrow(test_df_tld_reseller)))
        } else{
            model <- get(model_name)
            test_df_tld_reseller = feature_creation(test_df_tld_reseller, drop_rare=FALSE, xlevels=model$xlevels)
            pred = predict_first_renewal(test_df_tld_reseller, model)
    
            pred_df_seg2_glm = data.frame("actual" = pred$renewal_status,
                                      "predicted" = pred$first_renewal_prediction)
        }

        
    }
    
    return(pred_df_seg2_glm)
    
}

pred_seg2_rf <- function(test_list, tld_reseller_str) {
    
    print(tld_reseller_str)
    # seg2 rf (tld-reseller-segmented rf)
    
    test_list_tld_reseller = test_list[tld_reseller_str]
    test_df_tld_reseller =  rbindlist(test_list_tld_reseller,use.names=TRUE)
    test_df_tld_reseller = feature_creation(test_df_tld_reseller, drop_rare=FALSE)
    
    if ((dim(test_df_tld_reseller)[1]==0)){
        pred_df_seg2_rf = NA
    } else {
      
        model_name <- paste0('model_seg2_rf_',str_replace_all(tld_reseller_str, "[^[:alnum:]]", ""))
        
        if((!exists(model_name))){
            pred_df_seg2_rf =  data.frame("actual" = rep(NA, nrow(test_df_tld_reseller)),
                              "predicted" = rep(NA, nrow(test_df_tld_reseller)))
        } else {
            model <- get(model_name)
            pred <- predict(model, 
                        data = test_df_tld_reseller,
                        type="response")$predictions

            if (ncol(pred) < 2) {
                preds <- rep(0, nrow(test_df_tld_reseller))
            } else {
                preds <- as.data.frame(pred)[, 2]
            }
            
            pred_df_seg2_rf = data.frame("actual" = test_df_tld_reseller$renewal_status,
                          "predicted" = preds)
        }

        
    }
    
    # need to combine all predictions into one dataframe, rbind with test data
    return(pred_df_seg2_rf)

    
}


geo_lookup <- function(geoLookupDF,
                         preds_df) {
        
    # 1. use geo lookup with duplicate rows removed
    #    remove duplicate rows in geo_lookup due to registrar level segmentation
    #    results in dims matching for expiry_test

    geoLookupDF <- geoLookupDF %>% distinct(reseller,reseller_country, reseller_geo)

    preds_df$reseller <- factor(preds_df$reseller)
    preds_df$reseller_country <- factor(preds_df$reseller_country)
    preds_df <- as.data.frame(preds_df)

    preds_df <- merge(preds_df,
                      geoLookupDF,
                      on=c('reseller','reseller_country'), 
                      all.x = TRUE)
    
    
    
    # 2. create a new lookup where we drop everything except for 
    #    reseller_country and _geo and have NA map to Others
    
    geoLookupDF <- geoLookupDF %>% distinct(reseller_country, reseller_geo) %>% 
                    mutate(reseller_geo = as.character(reseller_geo)) %>%
                    mutate(eseller_geo = if_else(is.na(reseller_country), 'Others', reseller_geo) ) %>% 
                    distinct(reseller_country, reseller_geo) %>% 
                    mutate(reseller_geo = as.factor(reseller_geo))

    # use new lookup to fill reseller_geo based just on reseller_country
    preds_df[['reseller_geo']][is.na(preds_df[['reseller_geo']])]<-
                    geoLookupDF$reseller_geo[match(
                        preds_df$reseller_country[is.na(preds_df[['reseller_geo']])],
                        geoLookupDF$reseller_country)]

    # manual fix for reseller geo 
    preds_df[['reseller_geo']][preds_df[['reseller_country']]=='Southafrica']<-'South Africa'

    # Print remaining NA reseller_geos
    rem_res <- preds_df %>% filter(is.na(reseller_geo)) %>% 
          distinct(reseller,reseller_country, reseller_geo) %>% pull(reseller)
    cat("Resellers with unmatched reseller_geo's: ",paste0(rem_res, sep=", "))
    
    return(preds_df)
}


#########################################################################################  
#
# IV. AGG Functions
#
#########################################################################################   

train_all <- function (tld_reseller_list,
                       tld_registrar_excl_list,
                       train_list = expiry_train_prepped_2_1,
                       test_list = expiry_test_prepped_2_1,
                       skipModels=c(),
                       fullDir='../../data/output/models_20201015',
                       dp = FALSE){
    
    # keep list of all tld-re's
    cat("keep list\n")
    tld_reseller_list_ALL = tld_reseller_list
    reseller_list_ALL = rbindlist(test_list, ,use.names=TRUE) %>% 
      filter(tld_registrar_index %in% tld_reseller_list_ALL) %>% 
      distinct(reseller)  %>%  pull(reseller)
    
    allModels <-c("model_agg_rf_ALL", "model_agg_glm_ALL", "model_seg2_glm_ALL",
                  "model_seg_glm_ALL", "model_seg_rf_ALL", "model_seg2_rf_ALL")
    useModels <- allModels[(!allModels %in% skipModels)]

    
    if ("model_agg_glm_ALL" %in%  useModels) {
        
        cat("\n\nTraining model_agg_glm_ALL\n")
        model_agg_glm_ALL = train_agg_glm(train_list,tld_reseller_list_ALL,dp)
        save(model_agg_glm_ALL, 
             file=file.path(fullDir, 'model_agg_glm_ALL.Rdata'))
        
        rm(model_agg_glm_ALL)
        gc()
    }    
    
    if("model_agg_glm" %in%  useModels) {
        
        cat("\n\nTraining model_agg_glm\n")
        model_agg_glm = train_agg_glm(train_list,tld_reseller_list,dp)
        save(model_agg_glm, 
             file=file.path(fullDir, 'model_agg_glm.Rdata'))
        rm(model_agg_glm)
        gc()
    }    
    
    if("model_agg_rf_ALL" %in% useModels) {
        cat("\n\nTraining model_agg_rf_ALL\n")
        model_agg_rf_ALL = train_agg_rf(train_list,tld_reseller_list_ALL,dp)   
        save(model_agg_rf_ALL, 
             file=file.path(fullDir, 'model_agg_rf_ALL.Rdata')
            )
        rm(model_agg_rf_ALL)
        gc()
        
    } 
    
    if("model_agg_rf" %in% useModels) {
        cat("\n\nTraining model_agg_rf\n")
        model_agg_rf = train_agg_rf(train_list,tld_reseller_list,dp)   
        save(model_agg_rf, 
             file=file.path(fullDir, 'model_agg_rf.Rdata')
            )
        rm(model_agg_rf)
        gc()
        
    } 
    
    
    cat("\n\nTraining model_seg_glm & model_seg_rf\n")
    for (reseller_str in reseller_list_ALL) {
               
        if ("model_seg_glm_ALL" %in% useModels) {
            model_name <- paste0('model_seg_glm_',str_replace_all(reseller_str, "[^[:alnum:]]", ""))
            print(model_name)
            print(reseller_str)
            assign(model_name,train_seg_glm(train_list, reseller_str,dp) )
            save(list=model_name, 
                 file=file.path(fullDir, paste0(model_name,'.Rdata'))
                )
            rm(list=model_name)
            gc()
        }

        if ("model_seg_rf_ALL" %in% useModels) {        
            model_name <- paste0('model_seg_rf_',str_replace_all(reseller_str, "[^[:alnum:]]", ""))
            print(model_name)
            print(reseller_str)

            assign(model_name,train_seg_rf(train_list, reseller_str,dp)  )
            save(list=model_name, 
                 file=file.path(fullDir, paste0(model_name,'.Rdata'))
                )
            rm(list=model_name)
            gc()
        }
    } 
    
    
    cat("\n\nTraining model_seg2_glm & model_seg2_rf\n")
    for (tld_reseller_str in tld_reseller_list_ALL) {

        if ("model_seg2_glm_ALL" %in% useModels) {
            model_name <- paste0('model_seg2_glm_',str_replace_all(tld_reseller_str, "[^[:alnum:]]", ""))
            print(model_name)

            assign(model_name,train_seg2_glm(train_list, tld_reseller_str,dp) )
            save(list=model_name, 
                 file=file.path(fullDir, paste0(model_name,'.Rdata'))
                )
            rm(list=model_name)
            gc()
        }

        if ("model_seg2_rf_ALL" %in% useModels) {
            model_name <- paste0('model_seg2_rf_',str_replace_all(tld_reseller_str, "[^[:alnum:]]", ""))
            print(model_name)

            assign(model_name,train_seg2_rf(train_list, tld_reseller_str,dp)  )
            save(list=model_name, 
                 file=file.path(fullDir, paste0(model_name,'.Rdata'))
                )
            rm(list=model_name)
            gc()
        }
    }
    
    return(tld_reseller_list_ALL) # return all, predict for all and then exclude

}
    

pred_all <- function (tld_reseller_list, 
                      tld_registrar_excl_list,
                      test_list = expiry_test_prepped_2_1,
                      modelDir='../../data/output/models_20201015', # dir of models
                      fullDir='../../data/output/models_20201015', # dir of output
                      pred_folder='preds',
                      skipPred=c(), # Models to skip predicting 
                      skipReturn=c()  # Models to not return
                      ){   
    # Get models to use
    allModels <-c("model_agg_rf_ALL", "model_agg_glm_ALL", "model_seg2_glm_ALL",
                  "model_seg_glm_ALL", "model_seg_rf_ALL", "model_seg2_rf_ALL")
    useModels <- allModels[(!allModels %in% skipPred)]
    returnModels <- allModels[(!allModels %in% skipReturn)]

    predDir = file.path(fullDir, pred_folder)
    dir.create(predDir, recursive=TRUE)
    
    tld_reseller_list_ALL = tld_reseller_list
    
    # exclude low-volume tld-re's      
    tld_reseller_list = tld_reseller_list[!(tld_reseller_list %in% tld_registrar_excl_list)]

    if ("model_seg2_glm_ALL" %in% useModels) {
        cat("\n\nPredicting model_seg2_glm_ALL\n")
        lapply(Sys.glob(file.path(modelDir,'model_seg2_glm_*')),load,.GlobalEnv)
        preds_seg2_glm_ALL = lapply(tld_reseller_list_ALL, 
               function(tld_reseller_str) pred_seg2_glm(
                   test_list, 
                   tld_reseller_str)
               )
        rm(list=ls(pattern='^model_seg2_glm_'))
        gc()

        save(preds_seg2_glm_ALL, file=file.path(predDir, 'preds_seg2_glm_ALL.RData'))
    }

    if ("model_agg_rf_ALL" %in% useModels) {
        cat("\n\nPredicting model_agg_rf_ALL\n")
        load(file.path(modelDir, 'model_agg_rf_ALL.Rdata'))
        test_df <- bind_rows(test_list)
        print(colnames(test_df))
        test_df = feature_creation(test_df, drop_rare=FALSE)
        print(colnames(test_df))

        pred <- predict(model_agg_rf_ALL, 
                        data = test_df,
                        type="response")$predictions
        print(as.data.frame(pred)[1:10,])
        preds_agg_rf_df = data.frame("actual" = test_df$renewal_status,
                          "predicted" = as.data.frame(pred)[, 2])
        preds_agg_rf_ALL = list()
        i = 1
        for (tld_reseller_str in tld_reseller_list_ALL) {
            preds_agg_rf_ALL[[i]] = preds_agg_rf_df[test_df$tld_registrar_index == tld_reseller_str, ]
            i = i + 1
        }
        rm(model_agg_rf)
        gc() 

        save(preds_agg_rf_ALL, file=file.path(predDir, 'preds_agg_rf_ALL.RData'))
    }
    
    if ("model_agg_glm_ALL" %in% useModels) {
        cat("\n\nPredicting model_agg_glm_ALL\n")
        load(file.path(modelDir, 'model_agg_glm_ALL.Rdata'))

        test_df <- bind_rows(test_list)
        tld_res <- test_df$tld_registrar_index
        test_df = feature_creation(test_df, drop_rare=FALSE, xlevels=model_agg_glm_ALL$xlevels)
        test_df$probabilities<-predict(model_agg_glm_ALL,newdata=test_df,type='response')
        preds_agg_glm_df = data.frame("actual" = test_df$renewal_status,
                                      "predicted" = test_df$probabilities)

        preds_agg_glm_ALL = list()
        i = 1
        for (tld_reseller_str in tld_reseller_list_ALL) {
            preds_agg_glm_ALL[[i]] = preds_agg_glm_df[tld_res == tld_reseller_str, ]
            i = i + 1
        }
        rm(model_agg_glm_ALL)
        gc()

        save(preds_agg_glm_ALL, file=file.path(predDir, 'preds_agg_glm_ALL.RData'))
    }
    
    if ("model_seg_glm_ALL" %in% useModels) {
        cat("\n\nPredicting model_seg_glm_ALL\n")   
        lapply(Sys.glob(file.path(modelDir,'model_seg_glm_*')),load,.GlobalEnv)
        preds_seg_glm_ALL = lapply(tld_reseller_list_ALL, 
               function(tld_reseller_str) pred_seg_glm(
                   test_list, 
                   tld_reseller_str)
               )
        rm(list=ls(pattern='^model_seg_glm_'))
        gc()

        save(preds_seg_glm_ALL, file=file.path(predDir, 'preds_seg_glm_ALL.RData'))
    }
    
    if ("model_seg_rf_ALL" %in% useModels) {
        cat("\n\nPredicting model_seg_rf_ALL\n")  
        lapply(Sys.glob(file.path(modelDir,'model_seg_rf_*')),load,.GlobalEnv)
        preds_seg_rf_ALL = lapply(tld_reseller_list_ALL, 
               function(tld_reseller_str) pred_seg_rf(
                   test_list, 
                   tld_reseller_str)
               )
        rm(list=ls(pattern='^model_seg_rf_'))
        gc()

        save(preds_seg_rf_ALL, file=file.path(predDir, 'preds_seg_rf_ALL.RData'))    
    }


    if ("model_seg2_rf_ALL" %in% useModels) {
        cat("\n\nPredicting model_seg2_rf_ALL\n")     
        lapply(Sys.glob(file.path(modelDir,'model_seg2_rf_*')),load,.GlobalEnv)
        preds_seg2_rf_ALL = lapply(tld_reseller_list_ALL, 
               function(tld_reseller_str) pred_seg2_rf(
                   test_list, 
                   tld_reseller_str)
               )
        rm(list=ls(pattern='^model_seg2_rf_'))
        gc()

        save(preds_seg2_rf_ALL, file=file.path(predDir, 'preds_seg2_rf_ALL.RData'))
    }
    
    # Load all prediction data into the environment
    preds_str <- gsub("model", "preds", returnModels)
    pred_str <- gsub("model", "pred", returnModels)
    for (i in seq(length(returnModels))) {
        model <- returnModels[i]
        preds <- preds_str[i]
        if (!model %in% useModels) {
            load(file.path(predDir, paste0(preds, '.RData')))
        }
    }

    
    # combine all preds into one list
    preds_list = list()
    i=1
    for (tld_reseller_str in tld_reseller_list_ALL) {
        tmp <- test_list[[tld_reseller_str]]
        cat(paste(tld_reseller_str, "Table Dimensions", dim(tmp), "\n"))
        for (it in seq(length(returnModels))) {
            curr_pred_str = pred_str[it]
            print(i)
            print(it)
            cat(paste("curr_pred_str:", curr_pred_str, "\n"))
            cat(paste("pred length:", length(get(preds_str[it])), "\n"))

            if (is.na(get(preds_str[it])[[i]])) fill_preds <- NA
            else {
                fill_preds = get(preds_str[it])[[i]]$predicted
            }
            tmp[, curr_pred_str] = fill_preds
        }
        
        tmp <- tmp %>% select(-contains(".actual"))
        names(tmp) <- c(names(test_list[[tld_reseller_str]]), pred_str)
        preds_list[[tld_reseller_str]] <- tmp
        i=i+1
    }

    na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }
    preds_list <- na.omit.list(preds_list)
    preds_df <- rbindlist(preds_list,use.names=TRUE,fill=TRUE)                           

    # Delete files and free up memory
    rm(list=preds_str)
    rm(preds_list)
    gc()                                           
    
    return(preds_df)
}

########################################################################################################
#
# V. METALEARNING TRAINING/TESTING 
#
########################################################################################################
  
                                                   

l10_dplyr <- function (pred_df, pred_var = "first_renewal_prediction", p=0.10) {
    # Of the top 10% of predictions, what factor more renewals did we get than expected?
    prop <- mean(pred_df$renewal_status)
    n_rows <- max(round(p * nrow(pred_df)), 1)
    m <- pred_df %>% arrange(desc(get(pred_var))) %>%
        slice_head(n = n_rows) %>%
        pull(renewal_status)# == "Renewed"
    l10_prop <- mean(m)
    return(ifelse(prop == 0, 0, l10_prop / prop))
}

                                                   
mape_dplyr <- function (pred_df, pred_var = "first_renewal_prediction") {
    # Mean absolute percentage error when summing predictions
    p <- sum(pred_df[, pred_var])
    a <- sum(pred_df$renewal_status)# == "Renewed")
    return(ifelse(a == 0, 100, 100 * abs(p - a) / a))
}
                                                   
auc_dplyr <- function (pred_df,
                              pred_var = "first_renewal_prediction") {
    
    actu_renwd <- sum(pred_df[["renewal_status"]])
    if (actu_renwd > 0) {
        auc = pred_df %>% arrange(desc(get(pred_var))) %>%
            mutate(cs = cumsum(renewal_status), gain = cs / actu_renwd) %>%
            pull(gain) %>%
            sum() / nrow(pred_df)
    } else {
        auc = 0
    }
    auc
}
