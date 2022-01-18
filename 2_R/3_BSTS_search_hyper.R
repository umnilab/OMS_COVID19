library(bsts)
# cross-validation loop

policy_nyc<- list(paste('S', 1:nyc.policy_num,sep=""),
                  paste('L', 1:nyc.policy_num, sep=""),
                  paste('E', 1:nyc.policy_num, sep=""))

policy_chicago<- list(paste('S', 1:chicago.policy_num,sep=""),
                      paste('L', 1:chicago.policy_num, sep=""),
                      paste('E', 1:chicago.policy_num, sep=""))


bsts.cv.loop <- function(data,
                         df,
                         ridership,
                         k,
                         name,
                         varlist,
                         horizon,
                         number_of_folds,
                         level_sigma_guess,
                         level_sigma_max,
                         slope_sigma_guess,
                         slope_sigma_max,
                         niter=1000,
                         burn=100,
                         verbose=TRUE,
                         debug=FALSE) {
  mse_v <- c()
  mae_v <- c()
  ind_v <- c()
  for (ind in c(1:k)) {
    mae_tmp<-0
    mse_tmp<-0
    for (fold in 1:number_of_folds) {
      # construct data_train/data_test
      l <- (fold-1)*horizon+180
      df_train <- df[1:l,]
      df_test <- df[(l+1):(l+horizon),]
      # fit model & predict
  
      # update the formulation
      var_list <- c('RAIN', 'TMAX','TMIN')
      # var_list <- c(var_list, unlist(varlist))
      f<-as.formula(paste('COMPONENT', paste(var_list, collapse = "+"), sep="~"))
      data_train <- rescale(data[,ind])
      data_test <- data_train[(l+1):(l+horizon)]
      df_train$COMPONENT<-data_train[1:l]
      if(slope_sigma_guess>0){
        ss.function<-function(data_train, sdy) {
          sd.prior <- SdPrior(sigma.guess=level_sigma_guess*sdy,
                              upper.limit=level_sigma_max*sdy)
          slope.sigma.prior <- SdPrior(sigma.guess=slope_sigma_guess*sdy,
                                       upper.limit=slope_sigma_max*sdy)
          ss <- AddLocalLinearTrend(list(),
                                    data_train,
                                    level.sigma.prior=sd.prior,
                                    slope.sigma.prior=slope.sigma.prior)
          return(AddSeasonal(ss, df_train$COMPONENT, nseasons = 7))
        }
      }
      else{
        ss.function<-function(data_train, sdy) {
          sd.prior <- SdPrior(sigma.guess=level_sigma_guess*sdy,
                              upper.limit=level_sigma_max*sdy)
          ss <- AddLocalLevel(list(),
                              data_train,
                              sigma.prior=sd.prior)
          return(AddSeasonal(ss, df_train$COMPONENT, nseasons = 7))
        }
      }
      model <- bsts(formula = f, data = df_train,
                    state.specification=ss.function(data_train,sd(data_train[1:365])),
                    niter=1000,
                    ping=0)
      pred <- predict(model, df_test, horizon=horizon, burn=burn)
      errors <- data_test-pred$mean
      mse <- sqrt(mean(errors^2))
      mse_tmp<-mse_tmp+mse
      mae <- mean(abs(errors))
      mae_tmp<-mae_tmp+mae
      if (verbose) print(paste0("fold ", fold,"ind:",ind, ": mse ", mse, " / mae ", mae))
    }
    mse_v <- c(mse_v, mse_tmp/number_of_folds)
    mae_v <- c(mae_v, mae_tmp/number_of_folds)
    ind_v <- c(ind_v, ind)
    # evaluation, update to multi-dimensional
    #if(name=='nyc'){
    #  fitted <- preds %*% diag(nyc.ssa2$sigma[1:nyc.k]) %*% t(calc.v(nyc.ssa2,1:nyc.k))
    #}
    #else{
    #  fitted <- preds %*% diag(chicago.ssa2$sigma[1:nyc.k]) %*% t(calc.v(chicago.ssa2,1:nyc.k))
    #}
    # plot(data_test[1,], fitted[1,])
    
    # varname <- paste(name,fold, level_sigma_max*1000, level_sigma_guess*2000 ,
    #                  slope_sigma_max*1000,  slope_sigma_guess*2000, sep="_")
    # save(errors, file = paste(varname, "cv_result.Rdata", sep="_"))
    
  }
  return(data.frame(ind = ind_v, mse=mse_v,mae=mae_v))
}

# 0.2. general parameters for the prediction
horizon=60 # 2 months
number_of_folds=3

res_df1 <- c()
for (level_sigma_max in 4^(-10:-1)) {
    level_sigma_guess<- level_sigma_max/2
      res <-
        bsts.cv.loop(nyc.ssa2$U, nyc.df_,nyc.ridership_,nyc.k,'nyc',policy_nyc,
                     horizon=horizon,
                     number_of_folds=number_of_folds,
                     level_sigma_guess,
                     level_sigma_max,
                     0,0,
                     debug=FALSE,
                     verbose=TRUE)
      print(paste0("sigma_max ", level_sigma_max,
                   ": sigma_guess ", level_sigma_guess,
                   ": mean(mse) ", mean(res$mse),
                   " / mean(mae) ", mean(res$mae)))
      res_row <- data.frame(sigma_max=level_sigma_max,
                            sigma_guess=level_sigma_guess,
                            ind = res$ind,
                            mse=res$mse,
                            mae=res$mae)
      res_df1 <- rbind(res_df1, res_row)
}

res_df1
write.csv(res_df1, file="nyc_cv_res_local_level2.csv",
          row.names=FALSE)


# cross-validation loop
res_df1 <- c()
for (level_sigma_max in 4^(-10:-1)) {
      level_sigma_guess<- level_sigma_max/2
      res <-bsts.cv.loop(chicago.ssa2$U, chicago.df_,chicago.ridership_,chicago.k,
                     'chicago',policy_chicago,
                     horizon=horizon,
                     number_of_folds=number_of_folds,
                     level_sigma_guess,
                     level_sigma_max,
                     0,0,
                     debug=FALSE,
                     verbose=TRUE)
      print(paste0("sigma_max ", level_sigma_max,
                   ": sigma_guess ", level_sigma_guess,
                   ": mean(mse) ", mean(res$mse),
                   " / mean(mae) ", mean(res$mae)))
      res_row <- data.frame(sigma_max=level_sigma_max,
                            sigma_guess=level_sigma_guess,
                            ind = res$ind,
                            mse= res$mse,
                            mae= res$mae)
      res_df1 <- rbind(res_df1, res_row)
}

res_df1
write.csv(res_df1, file="chicago_cv_res_local_level2.csv",
          row.names=FALSE)

# slope
res_df1 <- c()
for (slope_sigma_max in 4^(-10:-5)) {
    slope_sigma_guess<-slope_sigma_max/2
    for (level_sigma_max in 4^(-10:-3)) {
        level_sigma_guess<- level_sigma_max/2
        if(level_sigma_guess<level_sigma_max && slope_sigma_max>slope_sigma_guess){
          res <-
            bsts.cv.loop(nyc.ssa2$U, nyc.df_,nyc.ridership_,nyc.k,'nyc',policy_nyc,
                         horizon=horizon,
                         number_of_folds=number_of_folds,
                         level_sigma_guess,
                         level_sigma_max,
                         slope_sigma_guess,slope_sigma_max,
                         debug=FALSE,
                         verbose=TRUE)
          print(paste0("sigma_max ", level_sigma_max,
                       ": sigma_guess ", level_sigma_guess,
                       ": slope_sigma_max ", slope_sigma_max,
                       ": slope_sigma_guess ", slope_sigma_guess,
                       ": mean(mse) ", mean(res$mse),
                       " / mean(mae) ", mean(res$mae)))
          res_row <- data.frame(slope_sigma_max=slope_sigma_max,
                                slope_sigma_guess=slope_sigma_guess,
                                sigma_max=level_sigma_max,
                                sigma_guess=level_sigma_guess,
                                ind=res$ind,
                                mse=res$mse,
                                mae=res$mae)
          res_df1 <- rbind(res_df1, res_row)
        }
    }
}


res_df1
write.csv(res_df1, file="nyc_cv_res_linear_trend2.csv",
          row.names=FALSE)

for (slope_sigma_max in 4^(-10:-5)) {
  slope_sigma_guess<-slope_sigma_max/2
  for (level_sigma_max in 4^(-10:-3)) {
    level_sigma_guess<- level_sigma_max/2
        if(level_sigma_guess<level_sigma_max && slope_sigma_max>slope_sigma_guess){
          res <-
            bsts.cv.loop(chicago.ssa2$U, chicago.df_,chicago.ridership_,chicago.k,'chicago',policy_chicago,
                         horizon=horizon,
                         number_of_folds=number_of_folds,
                         level_sigma_guess,
                         level_sigma_max,
                         slope_sigma_guess,slope_sigma_max,
                         debug=FALSE,
                         verbose=TRUE)
          print(paste0("sigma_max ", level_sigma_max,
                       ": sigma_guess ", level_sigma_guess,
                       ": slope_sigma_max ", slope_sigma_max,
                       ": slope_sigma_guess ", slope_sigma_guess,
                       ": mean(mse) ", mean(res$mse),
                       " / mean(mae) ", mean(res$mae)))
          res_row <- data.frame(slope_sigma_max=slope_sigma_max,
                                slope_sigma_guess=slope_sigma_guess,
                                sigma_max=level_sigma_max,
                                sigma_guess=level_sigma_guess,
                                ind = res$ind,
                                mse=res$mse,
                                mae=res$mae)
          res_df1 <- rbind(res_df1, res_row)
        }
    }
}


res_df1
write.csv(res_df1, file="chicago_cv_res_linear_trend2.csv",
          row.names=FALSE)