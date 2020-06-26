
library(tidyr)
library(dplyr)
library(ggplot2)

# lift_funct <- function(depvar, predcol, groups=10) {
#   if(!require(dplyr)){
#     install.packages("dplyr")
#     library(dplyr)}
#   if(is.factor(depvar)) depvar <- as.integer(depvar)
#   if(is.factor(predcol)) predcol <- as.integer((predcol)
#   helper = data.frame(cbind(depvar, predcol))
#   helper[,"bucket"] = ntile(-helper[,"predcol"], groups)
#   gaintable = helper %>% group_by(bucket)  %>%
#     summarise_at(vars(depvar), funs(total = n(),
#                                     totalresp=sum(., na.rm = TRUE))) %>%
#     mutate(Cumresp = cumsum(totalresp),
#            Gain=Cumresp/sum(totalresp)*100,
#            Cumlift=Gain/(bucket*(100/groups)))
#   return(gaintable)
# }
                                               
chart_lift <- function (pred_df=first_renewal_model_test_predict,
                        dep_var = "renewal_status",
                        pred_var = "first_renewal_prediction") {
  N <- 10  # total number of rows to preallocate--possibly an overestimate
  lift_df <- data.frame(P =rep(NA, N), 
                        actu_renwd2=rep(NA, N), 
                        gain=rep(NA, N), 
                        lift=rep(NA, N), 
                        stringsAsFactors=FALSE)          # you don't know levels yet
  actu_renwd <- sum(pred_df[[dep_var]]=='Renewed')
  
  i = 1
  for(P in seq(.1,1,length=10)){
    temp_df <- data.frame(pred_df)[c(dep_var,pred_var)]
    ttmp_df <- temp_df[order(temp_df[pred_var],decreasing = TRUE),][1:round(dim(temp_df)[1]*P),]
    actu_renwd2 <-  sum(ttmp_df[[dep_var]] == 'Renewed')
    gain = actu_renwd2/actu_renwd
    lift = gain/(P)
    
    lift_df[i, ] <- list(P, actu_renwd2, gain, lift)
    i = i+1
  }
  return(lift_df)
}
                                               
plot_gains <- function (lift_df=lift_df) {
  lift_df2 <- lift_df %>%
    add_row(P = 0, gain =0) %>%
    arrange(P)
  
  gains_plot <- ggplot(lift_df2, aes(P,  gain)) +
    geom_point() +
    geom_line() +
    # This makes another geom_line that only sees the first and last row of the data
    geom_line(data = lift_df2 %>% slice(1, n())) +
    
    scale_y_continuous(breaks = seq(0, 1, by = .1), limits = c(0,1)) +
    scale_x_continuous(breaks = seq(0, 1, by = .1)) +
    labs(title = "Cumulative Gains Plot",
         y = "Cumulative Gain %")
  
  return(gains_plot)
}


calc_auc <- function (lift_df=lift_df) {
  lift_df2 <- as.data.table(lift_df)
  lift_df2 <- subset(lift_df2, select = c("P","gain"))
  names(lift_df2) <- c("x0", "y0")
  lift_df2 <- rbind(list(0, 0), lift_df2)
  lift_df2[, c("x1","y1") := list(x0, y0)]
  lift_df2[, c("x1","y1") := shift(.SD, 1, 0, "lead"), .SDcols=c("x0","y0")]
  lift_df2[, area:=(x1-x0)*y0+.5*(y1-y0)*(x1-x0)]
  lift_df2 <- lift_df2[-nrow(lift_df2),]
  auc <- sum(lift_df2$area)
  return(auc)
}

plot_multigains <- function (lift_df_list=list(seg_glm = lift_df, 
                                               ben_dtr = lift_df_2),
                             auc_list = list(seg_glm = auc, 
                                             ben_dtr=auc_2),
                             prop_positive = 0.1121167) {
  
  optimal_df <- data.frame( "P"=c(0,prop_positive,1.0),
                            "actu_renwd2"=c(NA,NA,NA),
                            "gain"=c(0,1.0,1.0),
                            "lift"=c(NA,NA,NA))
  
  lift_df_list <- lapply(lift_df_list, function(df) {
    df <- df %>%
      add_row(P = 0, gain =0) %>%
      arrange(P)
  })
  
  # colors <- brewer.pal(n = length(lift_df_list), name = "Set2")
  # colors <- colors[1:length(lift_df_list)]
  # name_map = paste(names(lift_df_list),colors, sep="=")[1:length(lift_df_list)]
  
  auc_list = lapply(auc_list, round, 4)
  auc_map = paste(names(lift_df_list),auc_list, sep=" = ")[1:length(lift_df_list)]
  
  
  gains_plot <- ggplot(NULL, aes(P,  gain)) +
    geom_line(data = lift_df_list[[1]] %>% slice(1, n())) +
    
    list(geom_line(data=optimal_df), geom_point(data=optimal_df)) +
    
    scale_y_continuous(breaks = seq(0, 1, by = .1), limits = c(0,1)) +
    scale_x_continuous(breaks = seq(0, 1, by = .1)) +
    labs(title = "Cumulative Gains Plot",
         y = "Cumulative Gain",
         x = "Percentile")
  
  for(i in seq(length(lift_df_list))){
    name = names(lift_df_list)[[i]]
    df = lift_df_list[[i]]
#     color=colors[[i]]
    auc = auc_list[[i]]
    gains_plot <- gains_plot + list(geom_line(data=df), 
                                    geom_point(data=df))+ 
      annotate("text", x = .8, y = .6-i*.075, hjust = 0, label = auc_map[[i]])
  }
  
  
  return(gains_plot)
}
