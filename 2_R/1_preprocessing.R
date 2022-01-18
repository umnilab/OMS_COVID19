library(GMCM)
library(zoo)

# Calculate the total number of trips for each mode
train_ind_nyc <- 366:(365+365+366+31+28+31+30+31+30+31)
date_shift_nyc <- 17896
train_ind_chicago <- 366:(365+365+366+31+28+31+30+31+30+31)
date_shift_chicago <- 17896
nyc.policy_num <- 10
chicago.policy_num <- 9

alpha <- 7 # For policy lag, not used since it complicates things too much!
chicago_bike <-chicago_bike[train_ind_chicago,]
chicago_fhv <-  chicago_fhv[train_ind_chicago,]
chicago_taxi <- chicago_taxi[train_ind_chicago,]

chicago_shocks <- chicago_shocks[train_ind_chicago,]#, as.numeric(chicago_vacc_agg[train_ind_chicago]>0)) # shocks
chicago_shocks2 <- matrix(0,dim(chicago_shocks)[1],dim(chicago_shocks)[2])
chicago_shocks3 <- chicago_shocks 
chicago_shocks4 <- matrix(0,dim(chicago_shocks)[1],dim(chicago_shocks)[2])
chicago_shocks5 <- matrix(0,dim(chicago_shocks)[1],dim(chicago_shocks)[2]) 

for(i in 1:dim(chicago_shocks)[2]){
  num2 = 0
  num3 = 0
  num4 = 0
  num5 = 0
  for(j in 1:dim(chicago_shocks)[1]){
    if(chicago_shocks[j,i] ==1 && num2 ==0){
      num2 <- 1
      chicago_shocks3[j:dim(chicago_shocks)[1],i] = 1
    }
    
    if((chicago_shocks[j,i] ==0) && (num2 > 0 && num3==0)){
      num3 <-1
      if(sum(chicago_shocks[j,])-sum(chicago_shocks[j-1,])<0){
        num5 <- 1
      }
    }
    if(num2>0){
      num4 <- num2/sum(chicago_shocks[,i])
      if(num3>0){
        num4 <- 1
      }
    }
    
    chicago_shocks2[j,i] <- num2
    chicago_shocks4[j,i] <- num4
    chicago_shocks5[j,i] <- num5
      
    if(num2>0){
      num2 <- num2 +1
    }
    if(num3>0){
      num3 <- num3 + 1
    }
  }
}
# 
# for(i in 1:(dim(chicago_shocks)[2]-1)){
#   if(sum(chicago_shocks[,i])==sum(chicago_shocks3[,i])){
#     chicago_shocks4[,i] <- 0
#   }
# }

clip<-function(x, min=0, max=1) {
  x[x<min]<-min; 
  x[x>max]<-max;
}

#shock_chicago<-(1-exp(-chicago_shocks2/alpha))*(sign(chicago_shocks2))
shock_chicago<-chicago_shocks3
shock_chicago<-cbind(shock_chicago, chicago_shocks4)
shock_chicago<-cbind(shock_chicago, chicago_shocks5)

# chicago_case_log<- log10(chicago_case_agg[train_ind_chicago]+1)
# chicago_vacc_log<- log10(chicago_vacc_agg[train_ind_chicago]+1)
chicago_case_agg <- chicago_case_agg[train_ind_chicago]
chicago_vacc_agg <- chicago_vacc_agg[train_ind_chicago]
# chicago_case_agg<-log10(chicago_case_agg+1)
chicago_tmax <- chicago_temp_max[train_ind_chicago,]
chicago_tmin <- chicago_temp_max[train_ind_chicago,]- chicago_temp_min[train_ind_chicago,]
# Standardize tmax and tmin, we use rescaling
# chicago_tmax_mean <- mean(chicago_tmax)
# chicago_tmax_std<- sd(chicago_tmax)
# chicago_tmin_mean <- mean(chicago_tmin)
# chicago_tmin_std <- sd(chicago_tmin)
# chicago_tmax <- (chicago_tmax-chicago_tmax_mean)/chicago_tmax_std
# chicago_tmin <- (chicago_tmin-chicago_tmin_mean)/chicago_tmin_std
chicago_rain = chicago_rain[train_ind_chicago,]
chicago_rain_log <- log10(chicago_rain+1)
chicago_rain_log[is.na(chicago_rain_log)] <- 0

# Balance the error weight of different modes
nyc_bike <- nyc_bike[train_ind_nyc,]
nyc_fhv <-  nyc_fhv[train_ind_nyc,]
nyc_taxi <-  nyc_taxi[train_ind_nyc,]

nyc_shocks <- nyc_shocks[train_ind_nyc,] #, as.numeric(nyc_vacc_agg[train_ind_nyc]>0)) # shocks
nyc_shocks2 <- matrix(0,dim(nyc_shocks)[1],dim(nyc_shocks)[2])
nyc_shocks3 <- nyc_shocks 
nyc_shocks4 <- matrix(0,dim(nyc_shocks)[1],dim(nyc_shocks)[2])
nyc_shocks5 <- matrix(0,dim(nyc_shocks)[1],dim(nyc_shocks)[2]) 

for(i in 1:dim(nyc_shocks)[2]){
  num2 = 0
  num3 = 0
  num4 = 0
  num5 = 0
  for(j in 1:dim(nyc_shocks)[1]){
    if(nyc_shocks[j,i] ==1 && num2 ==0){
      num2 <- 1
      nyc_shocks3[j:dim(nyc_shocks)[1],i] = 1
    }
    
    if((nyc_shocks[j,i] ==0) && (num2 > 0 && num3==0)){
      num3 <-1
      if(sum(nyc_shocks[j,])-sum(nyc_shocks[j-1,])<0){
        num5 <- 1
      }
    }
    if(num2>0){
      num4 <- num2/sum(nyc_shocks[,i])
      if(num3>0){
        num4 <- 1
      }
    }
    nyc_shocks2[j,i] <- num2
    nyc_shocks4[j,i] <- num4
    nyc_shocks5[j,i] <- num5
    
    if(num2>0){
      num2 <- num2 +1
    }
    if(num3>0){
      num3 <- num3 + 1
    }
  }
}

clip<-function(x, min=0, max=1) {
  x[x<min]<-min; 
  x[x>max]<-max;
}

# for(i in 1:(dim(nyc_shocks)[2]-1)){
#   if(sum(nyc_shocks[,i])==sum(nyc_shocks3[,i])){
#     nyc_shocks4[,i] <- 0
#   }
# }

#shock_nyc<-(1-exp(-nyc_shocks2/alpha))*(sign(nyc_shocks2))
shock_nyc<-nyc_shocks3
shock_nyc<-cbind(shock_nyc, nyc_shocks4)
shock_nyc<-cbind(shock_nyc, nyc_shocks5)

#nyc_case_log<- log10(nyc_case_agg[train_ind_nyc]+1)
#nyc_vacc_log<- log10(nyc_vacc_agg[train_ind_nyc]+1)

nyc_case_agg <- nyc_case_agg[train_ind_nyc]
nyc_vacc_agg <- nyc_vacc_agg[train_ind_nyc]
# nyc_case_agg<-log10(nyc_case_agg+1)

nyc_tmax <- nyc_temp_max[train_ind_nyc,]
nyc_tmin <- nyc_temp_max[train_ind_nyc,] - nyc_temp_min[train_ind_nyc,] # change this to TRNG

# nyc_tmax_mean <- mean(nyc_tmax)
# nyc_tmax_std<- sd(nyc_tmax)
# nyc_tmin_mean <- mean(nyc_tmin)
# nyc_tmin_std <- sd(nyc_tmin)
# 
# nyc_tmax <- (nyc_tmax-nyc_tmax_mean)/nyc_tmax_std
# nyc_tmin <- (nyc_tmin-nyc_tmin_mean)/nyc_tmin_std

nyc_rain = nyc_rain[train_ind_nyc,]
nyc_rain_log <- log10(nyc_rain+1)
nyc_rain_log[is.na(nyc_rain_log)] <- 0


nyc.ridership<-cbind(nyc_fhv, nyc_taxi, nyc_bike)
chicago.ridership<-cbind(chicago_fhv, chicago_taxi, chicago_bike)

#get data frame for predictors
policy_nyc <- list(paste('S', 1:nyc.policy_num, sep=""),
                   paste('L', 1:nyc.policy_num, sep=""),
                   paste('E', 1:nyc.policy_num, sep=""))
nyc.df<-data.frame(nyc_rain, nyc_rain_log, 
                   nyc_tmax, 
               nyc_tmin, nyc_case_agg, 
               nyc_vacc_agg, #nyc_case_log, 
               #nyc_vacc_log,
               shock_nyc)
# 'RAIN', 'RAIN_LOG','TMAX','TMIN','CASE', 'VAC','CASE_LOG','VAC_LOG'
names(nyc.df)<-c(c('RAIN','RAIN_LOG', 'TMAX','TMIN','CASE', 'VAC'), unlist(policy_nyc))

policy_chicago <- list(paste('S', 1:chicago.policy_num, sep=""),
                   paste('L', 1:chicago.policy_num, sep=""),
                   paste('E', 1:chicago.policy_num, sep=""))
chicago.df<-data.frame(chicago_rain, chicago_rain_log, 
                       chicago_tmax, 
                   chicago_tmin, chicago_case_agg, 
                   chicago_vacc_agg, 
                   #chicago_case_log, 
                   #chicago_vacc_log,
                   shock_chicago)
names(chicago.df)<-c(c('RAIN','RAIN_LOG', 'TMAX','TMIN','CASE', 'VAC'), unlist(policy_chicago))

# rescale the data to [0,100], note scale should not impact the regression result if there is intercept
nyc.df_min <- lapply(nyc.df, min)
nyc.df_max <- lapply(nyc.df, max)
chicago.df_min <- lapply(chicago.df, min)
chicago.df_max <- lapply(chicago.df, max)

rescale <- function(x) (x-min(x))/(max(x) - min(x)) * 100
nyc.df_ <- data.frame(lapply(nyc.df, rescale))
chicago.df_ <- data.frame(lapply(chicago.df, rescale))

chicago.mean<-c(rep(mean(chicago_fhv),154),
                rep(mean(chicago_taxi),154),
                rep(mean(chicago_bike),154))
chicago.ridership_ <- sweep(chicago.ridership,2,chicago.mean,FUN='/')
# chicago.ridership_[which(!is.finite(chicago.ridership_))] <- 0

nyc.mean <- c(rep(mean(nyc_fhv),142),
                rep(mean(nyc_taxi),142),
                rep(mean(nyc_bike),142))
nyc.ridership_ <- sweep(nyc.ridership,2,nyc.mean,FUN='/')
# nyc.ridership_[which(!is.finite(nyc.ridership_))] <- 0

nyc.df_[is.na(nyc.df_)] <- 0
chicago.df_[is.na(chicago.df_)] <- 0

# For evaluation
nyc.keep_ind<-colMeans(nyc.ridership)>1.0
chicago.keep_ind<-colMeans(chicago.ridership)>1.0

