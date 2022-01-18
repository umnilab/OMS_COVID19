# Start the clock!
ptm <- proc.time()

library(bsts)
library(ggplot2)
library(reshape2)
library(dplyr)  

seed <- 52

nyc.df_[is.na(nyc.df_)] <- 0
chicago.df_[is.na(chicago.df_)] <- 0

# policy names
nyc.policy_num <- 10
chicago.policy_num <- 9

policy_nyc<- list(paste('S', 1:nyc.policy_num,sep=""),
                     paste('L', 1:nyc.policy_num, sep=""),
                     paste('E', 1:nyc.policy_num, sep=""))

policy_chicago<- list(paste('S', 1:chicago.policy_num,sep=""),
                     paste('L', 1:chicago.policy_num, sep=""),
                     paste('E', 1:chicago.policy_num, sep=""))

###Data preparation----
# Step 2, use this to first test the performance of different pre-processing using a simple case
###Train function ----
# Wanna test the influence of including local linear trend, COVID-19 cases, policies
train_bsts<-function(df, nseason, TREND_OPTION, WEATHER_OPTION, COVID_OPTION, POLICY_OPTION, CITY_OPTION, ind, seed){
  if(TREND_OPTION){
    if(CITY_OPTION){
      # question, how to choose these prior, assume there is no change, 
      level_sigma_guess <- SdPrior(nyc_parameters2$sigma_guess[ind]*sd(df$COMPONENT[1:365]), fixed = FALSE, upper.limit = nyc_parameters2$sigma_max[ind]*sd(df$COMPONENT[1:365]))
      slope_sigma_guess <- SdPrior(nyc_parameters2$slope_sigma_guess[ind]*sd(df$COMPONENT[1:365]), fixed = FALSE, upper.limit = nyc_parameters2$slope_sigma_max[ind]*sd(df$COMPONENT[1:365]))
    }else{
      # question, how to choose these prior, assume there is no change, 
      level_sigma_guess <- SdPrior(chicago_parameters2$sigma_guess[ind]*sd(df$COMPONENT[1:365]), fixed = FALSE, upper.limit = chicago_parameters2$sigma_max[ind]*sd(df$COMPONENT[1:365]))
      slope_sigma_guess <- SdPrior(chicago_parameters2$slope_sigma_guess[ind]*sd(df$COMPONENT[1:365]), fixed = FALSE, upper.limit = chicago_parameters2$slope_sigma_max[ind]*sd(df$COMPONENT[1:365]))
    }
    ss <- AddLocalLinearTrend(list(), df$COMPONENT, 
                              level.sigma.prior =level_sigma_guess,
                              slope.sigma.prior = slope_sigma_guess)
  }
  else{
    if(CITY_OPTION){
      level_sigma_guess <- SdPrior(nyc_parameters$sigma_guess[ind]*sd(df$COMPONENT[1:365]), fixed = FALSE, upper.limit = nyc_parameters$sigma_max[ind]*sd(df$COMPONENT[1:365]))
      ss <- AddLocalLevel(list(), df$COMPONENT, sigma.prior =level_sigma_guess)
    }
    else{
      level_sigma_guess <- SdPrior(chicago_parameters$sigma_guess[ind]*sd(df$COMPONENT[1:365]), fixed = FALSE, upper.limit = chicago_parameters$sigma_max[ind]*sd(df$COMPONENT[1:365]))
      ss <- AddLocalLevel(list(), df$COMPONENT, sigma.prior =level_sigma_guess)
    }
  }
  if(nseason>0){
    ss <- AddSeasonal(ss, df$COMPONENT, nseasons = nseason)
  }
  var_list <- c()
  if(WEATHER_OPTION){
    var_list <- c(var_list, 'RAIN','RAIN_LOG', 'TMAX','TMIN')
  }
  if(COVID_OPTION){
    var_list <- c(var_list, 'CASE', 'VAC')
  }
  if(POLICY_OPTION){
    if(CITY_OPTION){
      var_list <- c(var_list, unlist(policy_nyc))
    }
    else{
      var_list <- c(var_list, unlist(policy_chicago))
    }
  }
  if(length(var_list) == 0){
    bsts.model <- bsts(df$COMPONENT, state.specification = ss, niter = 1000, ping=0)
  }
  else{
    f<-as.formula(paste('COMPONENT', paste(var_list, collapse = "+"), sep="~"))
    bsts.model <- bsts(formula = f, state.specification = ss, data = df, niter = 1000, ping=0,seed=seed)
                       #expected.model.size = 5)
  }
  return(bsts.model)
}

model_name <- c("N","T","C","P","TC","TP","CP","TCP", 
                "LN","LT","LC","LP","LTC","LTP","LCP","LTCP")

name_ind<-1
# Step 4.2 train models
for(model_ind in 1:8){
  if(model_ind==1){
    WEATHER_OPTION <- F
    COVID_OPTION <- F
    POLICY_OPTION <- F
  }
  else{
    if(model_ind==2){
      WEATHER_OPTION <- T
      COVID_OPTION <- F
      POLICY_OPTION <- F
    }
    if(model_ind==3){
      WEATHER_OPTION <- F
      COVID_OPTION <- T
      POLICY_OPTION <- F
    }
    if(model_ind==4){
      WEATHER_OPTION <- F
      COVID_OPTION <- F
      POLICY_OPTION <- T
    }
    if(model_ind==5){
      WEATHER_OPTION <- T
      COVID_OPTION <- T
      POLICY_OPTION <- F
    }
    if(model_ind==6){
      WEATHER_OPTION <- T
      COVID_OPTION <- F
      POLICY_OPTION <- T
    }
    if(model_ind==7){
      WEATHER_OPTION <- F
      COVID_OPTION <- T
      POLICY_OPTION <- T
    }
    if(model_ind==8){
      WEATHER_OPTION <- T
      COVID_OPTION <- T
      POLICY_OPTION <- T
    }
  }
    model <- list()
   #varname <- model_name[name_ind]
    #load(paste(varname, "BSTS_model_52.Rdata", sep="_"))
    fits <- list()
    seasons_chicago<- rep(7, chicago.k)
    seasons_nyc <- rep(7, nyc.k)
    #trends_chicago <- rep(trend, chicago.k)
    #trends_nyc <- rep(trend, nyc.k)
    
    for(ind in 1:nyc.k){
      trend<-(nyc_parameters$mse[ind]>nyc_parameters2$mse[ind])
      y<-rescale(nyc.ssa2$U[,ind])
      df<- nyc.df_
      df$COMPONENT<-y
      bsts.model<-train_bsts(df, seasons_nyc[ind], trend, WEATHER_OPTION, COVID_OPTION, POLICY_OPTION, TRUE, ind, seed)
      model[[paste('nyc',ind,sep="_")]]<-bsts.model
      #bsts.model<-model[[paste('nyc',ind,sep="_")]]
      burn <- 200
      if(length(fits)==0){
        fits <- nyc.ssa2$U[,ind]+as.numeric(-colMeans(bsts.model$one.step.prediction.errors[-(1:burn),]))*(max(nyc.ssa2$U[,ind])-min(nyc.ssa2$U[,ind]))/100
      }
      else{
        fits <- cbind(fits, nyc.ssa2$U[,ind]+as.numeric(-colMeans(bsts.model$one.step.prediction.errors[-(1:burn),]))*(max(nyc.ssa2$U[,ind])-min(nyc.ssa2$U[,ind]))/100)
      }
    }
    
    actual<-nyc.ridership_
    fitted <- fits %*% diag(nyc.ssa2$sigma[1:nyc.k]) %*% t(calc.v(nyc.ssa2,1:nyc.k))
    one_step_nyc <- (actual-fitted)
    fits <- list()
    
    # Chicago -----------------
    for(ind in 1:chicago.k){
      trend<-(chicago_parameters$mse[ind]>chicago_parameters2$mse[ind])
      y<-rescale(chicago.ssa2$U[,ind])
      df<- chicago.df_
      df$COMPONENT<-y
      bsts.model<-train_bsts(df, seasons_chicago[ind], trend, WEATHER_OPTION, COVID_OPTION, POLICY_OPTION, FALSE, ind,seed)
      model[[paste('chicago',ind,sep="_")]]<-bsts.model
      #bsts.model<-model[[paste('chicago',ind,sep="_")]]
      burn <- 200
      if(length(fits)==0){
        fits <- chicago.ssa2$U[,ind]+as.numeric(-colMeans(bsts.model$one.step.prediction.errors[-(1:burn),]))*(max(chicago.ssa2$U[,ind])-min(chicago.ssa2$U[,ind]))/100
      }
      else{
        fits <- cbind(fits, chicago.ssa2$U[,ind]+as.numeric(-colMeans(bsts.model$one.step.prediction.errors[-(1:burn),]))*(max(chicago.ssa2$U[,ind])-min(chicago.ssa2$U[,ind]))/100)
      }
    }
    
    actual<-chicago.ridership_
    fitted <- fits %*% diag(chicago.ssa2$sigma[1:chicago.k]) %*% t(calc.v(chicago.ssa2,1:chicago.k))
    one_step_chicago <- (actual-fitted)
    results <- list()
    varname <- model_name[name_ind]
    results[['chicago_one_step']] <- one_step_chicago
    results[['nyc_one_step']] <- one_step_nyc
    
    save(results, file = paste(varname, "BSTS_result_52_new.Rdata", sep="_"))
    save(model, file = paste(varname, "BSTS_model_52_new.Rdata", sep="_"))
    name_ind<-name_ind+1
} 

# Stop the clock
proc.time() - ptm
