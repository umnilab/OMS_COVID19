library(Rssa)
library(bsts)

train_bsts<-function(df, nseason, TREND_OPTION, CITY_OPTION, select_policy, ind, seed){
  if(TREND_OPTION){
    # question, how to choose these prior, assume there is no change, 
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
  var_list <- unlist(select_policy)
  
  if(length(var_list) == 0){
    bsts.model <- bsts(df$COMPONENT, state.specification = ss, niter = 1000, ping=0,seed= seed)
  }
  else{
    f<-as.formula(paste('COMPONENT', paste(var_list, collapse = "+"), sep="~"))
    bsts.model <- bsts(formula = f, state.specification = ss, data = df, niter = 1000, ping=0,seed=seed)
  }
  return(bsts.model)
}

varname = 'TCP'
# Inclusive probability, coefficient
load(paste(varname, "BSTS_model_52_new.Rdata", sep="_"))
# model<-res
policy_nyc<- c('RAIN', 'RAIN_LOG', 'TMAX','TMIN',
               'CASE', 'VAC',
               unlist(list(paste('S', 1:nyc.policy_num,sep=""),
                  paste('L', 1:nyc.policy_num, sep=""),
                  paste('E', 1:nyc.policy_num, sep=""))))

policy_chicago<-  c('RAIN', 'RAIN_LOG', 'TMAX','TMIN',
                    'CASE', 'VAC',
                    unlist(list(paste('S', 1:chicago.policy_num,sep=""),
                      paste('L', 1:chicago.policy_num, sep=""),
                      paste('E', 1:chicago.policy_num, sep=""))))

# load(paste(varname, "BSTS_model_52_revised.Rdata", sep="_"))
res<-list()

for(k in 1:nyc.k){
  print(k)
  y<-rescale(nyc.ssa2$U[,k])
  df<- nyc.df_
  df$COMPONENT<-y
  bsts.model<-model[[paste('nyc', k, sep="_")]]
  # prev_error <- sum(bsts.model$one.step.prediction.errors^2)
  coef<-summary(bsts.model, burn=200)$coefficients
  # print(coef)
  select_policy <- names(coef[coef[,5]>1/(length(coef[,5])-1),5]) # Drop the factors with P_inc < P_uniform to gain more efficiency
  flag<-TRUE
  j <- length(select_policy)
  trend<-(nyc_parameters$mse[k]>nyc_parameters2$mse[k])
  bsts.model <- train_bsts(df, 7, trend,FALSE, select_policy,k, 52)
  while(flag){
    flag<-FALSE
    coef<-summary(bsts.model, burn=200)$coefficients
    select_policy <- names(coef[,5])[1:(length(coef[,5])-1)]
    j <- length(select_policy)
    if(j<1){
      break
    }
    for(policy in select_policy[j:1]){
      if(coef[policy,5]<0.5){
        print(j)
        # print(length(select_policy))
        new_select <- select_policy[-j]
        trend<-(nyc_parameters$mse[k]>nyc_parameters2$mse[k])
        bsts.model <- train_bsts(df, 7, trend,TRUE, new_select,k, 52)
        flag<-TRUE
        # print(sum(tmp_model$one.step.prediction.errors^2))
        # print(prev_error)
        j<-length(select_policy)
        break
      }
    }
  }
  res[[paste('nyc',k,sep="_")]]<-bsts.model
}

# 
for(k in 1:chicago.k){
  print(k)
  y<-rescale(chicago.ssa2$U[,k])
  df<- chicago.df_
  df$COMPONENT<-y
  bsts.model<-model[[paste('chicago', k, sep="_")]]
  prev_error <- sum(bsts.model$one.step.prediction.errors^2)
  coef<-summary(bsts.model, burn=200)$coefficients
  select_policy <-  names(coef[coef[,5]>1/(length(coef[,5])-1),5])
  # if(k==1){
  #   select_policy<-select_policy[select_policy!='CASE']
  # }
  flag<-TRUE
  j <- length(select_policy)
  trend<-(chicago_parameters$mse[k]>chicago_parameters2$mse[k])
  bsts.model <- train_bsts(df, 7, trend,FALSE, select_policy,k, 52)
  while(flag){
    flag<-FALSE
    coef<-summary(bsts.model, burn=200)$coefficients
    select_policy <- names(coef[,5])[1:(length(coef[,5])-1)]
    j <-length(select_policy)
    if(j<1){
      break
    }
    for(policy in select_policy[j:1]){
      if(coef[policy,5]<0.5){
        print(j)
        # print(length(select_policy))
        new_select <- select_policy[-j]
        trend<-(chicago_parameters$mse[k]>chicago_parameters2$mse[k])
        #if(k==1){
        #  trend = FALSE
        #}
        bsts.model <- train_bsts(df, 7, trend,FALSE, new_select,k, 52)
        flag<-TRUE
        # print(sum(tmp_model$one.step.prediction.errors^2))
        # print(prev_error)
        j<-length(select_policy)
        break
      }
    }
  }
  res[[paste('chicago',k,sep="_")]]<-bsts.model
}


save(res, file = paste(varname, "BSTS_model_52_revised_new2.Rdata", sep="_"))

