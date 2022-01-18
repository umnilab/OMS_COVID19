# Step 6, predict the next one year
# then understand model coefficients
library(Rssa)
library(bsts)

varname = 'TCP'
# Inclusive probability, coefficient
load(paste(varname, "BSTS_model_52_revised_new.Rdata", sep="_"))
model<-res
nyc.df_[is.na(nyc.df_)] <- 0
chicago.df_[is.na(chicago.df_)] <- 0

day <- 365

# nyc
df <- nyc.df_
for(i in 1:day){
  df<-rbind(df, df[nrow(df), ]+(df[nrow(df), ]-df[nrow(df)-1, ]))
}
rownames(df) = seq(length=nrow(df))

df[(nrow(df)-364):(nrow(df)),c('RAIN','RAIN_LOG', 'TMAX','TMIN','CASE'
                               )]<-
  df[(nrow(df)-364-365):(nrow(df)-365),c('RAIN','RAIN_LOG', 'TMAX','TMIN','CASE'
                                 )]

# Policy after 2020-7-30 got repeated again
# policy_after_july<-policy_nyc[df[(nrow(df)-365-365), policy_nyc]==0]
# 
# df[(nrow(df)-364):(nrow(df)),policy_after_july]<-df[(nrow(df)-364):(nrow(df)),policy_after_july]+
# df[(nrow(df)-364-365):(nrow(df)-365),policy_after_july]

# Policy during early 2020 got repeated again
# policy_before_july<-policy_nyc[df[(nrow(df)-365-365), policy_nyc]>0]
# 
# df[(nrow(df)-210):(nrow(df)),policy_before_july]<-df[(nrow(df)-210):(nrow(df)),policy_before_july]+
#   df[(nrow(df)-210-365-366):(nrow(df)-365-366),policy_before_july]


df <- df[(nrow(df)-364):(nrow(df)), ]
df[,'CASE']<-mean(df[335:365,'CASE'])
# df[,'CASE_LOG']<-mean(df[335:365,'CASE_LOG'])
df[,'VAC']<-100
# df[,'VAC_LOG']<-100
rownames(df) = seq(length=nrow(df))

preds <- list()
preds_lower<-list()
preds_uppper <- list()
for(i in 1:nyc.k){
        bsts.model <- model[[paste("nyc",i,sep="_")]]
        burn<-200
        # rolling to get prediction result
        p<-predict.bsts(bsts.model, newdata = df, horizon = nrow(df), burn = burn, quantiles = c(.025, .975))
        if(length(preds)==0){
                preds <- p$mean* (max(nyc.ssa2$U[,i])-min(nyc.ssa2$U[,i]))/100 + min(nyc.ssa2$U[,i])
                preds_lower <- p$interval[1,]* (max(nyc.ssa2$U[,i])-min(nyc.ssa2$U[,i]))/100 + min(nyc.ssa2$U[,i])
                preds_upper <- p$interval[2,]* (max(nyc.ssa2$U[,i])-min(nyc.ssa2$U[,i]))/100 + min(nyc.ssa2$U[,i])
        }
        else{
                preds<-cbind(preds, p$mean* (max(nyc.ssa2$U[,i])-min(nyc.ssa2$U[,i]))/100+min(nyc.ssa2$U[,i]))
                preds_lower <- cbind(preds_lower,  p$interval[1,]* (max(nyc.ssa2$U[,i])-min(nyc.ssa2$U[,i]))/100+min(nyc.ssa2$U[,i]))
                preds_upper <- cbind(preds_upper, p$interval[2,]* (max(nyc.ssa2$U[,i])-min(nyc.ssa2$U[,i]))/100+min(nyc.ssa2$U[,i]))
        }
}
predicted<- t(t((preds %*% diag(nyc.ssa2$sigma[1:nyc.k]) %*% t(calc.v(nyc.ssa2,1:nyc.k)))) * nyc.mean)
predicted_upper <- t(t((preds_upper %*% diag(nyc.ssa2$sigma[1:nyc.k]) %*% t(calc.v(nyc.ssa2,1:nyc.k)))) * nyc.mean)
predicted_lower <- t(t((preds_lower %*% diag(nyc.ssa2$sigma[1:nyc.k]) %*% t(calc.v(nyc.ssa2,1:nyc.k)))) * nyc.mean)

write.csv(predicted, "result/nyc_pred.csv")
write.csv(predicted_upper, "result/nyc_pred_upper.csv")
write.csv(predicted_lower, "result/nyc_pred_lower.csv")

## chicago
# Calculate test error
df <- chicago.df_
for(i in 1:day){
  df<-rbind(df, df[nrow(df), ]+(df[nrow(df), ]-df[nrow(df)-1, ]))
}
rownames(df) = seq(length=nrow(df))

df[(nrow(df)-364):(nrow(df)),c('RAIN','RAIN_LOG','TMAX','TMIN','CASE'
)]<-
  df[(nrow(df)-364-365):(nrow(df)-365),c('RAIN','RAIN_LOG','TMAX','TMIN','CASE'
  )]

# Policy after 2020-7-30 got repeated again
# policy_after_july<-policy_chicago[df[(nrow(df)-365-365), policy_chicago]==0]
# 
# df[(nrow(df)-364):(nrow(df)),policy_after_july]<-df[(nrow(df)-364):(nrow(df)),policy_after_july]+
# df[(nrow(df)-364-365):(nrow(df)-365),policy_after_july]

# Policy during early 2020 got repeated again
# policy_before_july<-policy_chicago[df[(nrow(df)-365-365), policy_chicago]>0]
# 
# df[(nrow(df)-210):(nrow(df)),policy_before_july]<-df[(nrow(df)-210):(nrow(df)),policy_before_july]+
#   df[(nrow(df)-210-365-366):(nrow(df)-365-366),policy_before_july]


df <- df[(nrow(df)-364):(nrow(df)), ]
df[,'CASE']<-mean(df[335:365,'CASE'])
# df[,'CASE_LOG']<-mean(df[335:365,'CASE_LOG'])
df[,'VAC']<-100
# df[,'VAC_LOG']<-100
rownames(df) = seq(length=nrow(df))

preds <- list()
preds_lower<-list()
preds_uppper <- list()
for(i in 1:chicago.k){
  bsts.model <- model[[paste("chicago",i,sep="_")]]
  burn<-200
  # rolling to get prediction result
  p<-predict.bsts(bsts.model, newdata = df, horizon = nrow(df), burn = burn, quantiles = c(.025, .975))
  if(length(preds)==0){
    preds <- p$mean* (max(chicago.ssa2$U[,i])-min(chicago.ssa2$U[,i]))/100 + min(chicago.ssa2$U[,i])
    preds_lower <- p$interval[1,]* (max(chicago.ssa2$U[,i])-min(chicago.ssa2$U[,i]))/100 + min(chicago.ssa2$U[,i])
    preds_upper <- p$interval[2,]* (max(chicago.ssa2$U[,i])-min(chicago.ssa2$U[,i]))/100 + min(chicago.ssa2$U[,i])
  }
  else{
    preds<-cbind(preds, p$mean* (max(chicago.ssa2$U[,i])-min(chicago.ssa2$U[,i]))/100+min(chicago.ssa2$U[,i]))
    preds_lower <- cbind(preds_lower,  p$interval[1,]* (max(chicago.ssa2$U[,i])-min(chicago.ssa2$U[,i]))/100+min(chicago.ssa2$U[,i]))
    preds_upper <- cbind(preds_upper, p$interval[2,]* (max(chicago.ssa2$U[,i])-min(chicago.ssa2$U[,i]))/100+min(chicago.ssa2$U[,i]))
  }
}
predicted<- t(t((preds %*% diag(chicago.ssa2$sigma[1:chicago.k]) %*% t(calc.v(chicago.ssa2,1:chicago.k)))) * chicago.mean)
predicted_upper <- t(t((preds_upper %*% diag(chicago.ssa2$sigma[1:chicago.k]) %*% t(calc.v(chicago.ssa2,1:chicago.k)))) * chicago.mean)
predicted_lower <- t(t((preds_lower %*% diag(chicago.ssa2$sigma[1:chicago.k]) %*% t(calc.v(chicago.ssa2,1:chicago.k)))) * chicago.mean)

write.csv(predicted, "result/chicago_pred.csv")
write.csv(predicted_upper, "result/chicago_pred_upper.csv")
write.csv(predicted_lower, "result/chicago_pred_lower.csv")

# write.csv(nyc.df_, "result/nyc_df.csv")
# write.csv(chicago.df_, "result/chicago_df.csv")

# write.csv(nyc.ridership, "result/nyc_ridership.csv")
# write.csv(chicago.ridership, "result/chicago_ridership.csv")

df <- nyc.df_
for(i in 1:day){
  df<-rbind(df, df[nrow(df), ]+(df[nrow(df), ]-df[nrow(df)-1, ]))
}
rownames(df) = seq(length=nrow(df))

df[(nrow(df)-364):(nrow(df)),c('RAIN','RAIN_LOG','TMAX','TMIN',
                               'CASE')]<-
  df[(nrow(df)-364-365):(nrow(df)-365),c('RAIN','RAIN_LOG','TMAX','TMIN',
                                         'CASE')]

# Policy after 2020-7-30 got repeated again
policy_after_july<-unlist(policy_nyc)[df[(nrow(df)-365-365), unlist(policy_nyc)]==0]

df[(nrow(df)-364):(nrow(df)),policy_after_july]<-df[(nrow(df)-364):(nrow(df)),policy_after_july]+
  df[(nrow(df)-364-365):(nrow(df)-365),policy_after_july]

# Policy during early 2020 got repeated again
# policy_before_july<-policy_nyc[df[(nrow(df)-365-365), policy_nyc]>0]
# 
# df[(nrow(df)-210):(nrow(df)),policy_before_july]<-df[(nrow(df)-210):(nrow(df)),policy_before_july]+
#   df[(nrow(df)-210-365-366):(nrow(df)-365-366),policy_before_july]


df <- df[(nrow(df)-364):(nrow(df)), ]
df[,'CASE']<-mean(df[335:365,'CASE'])
# df[,'CASE_LOG']<-mean(df[335:365,'CASE_LOG'])
df[,'VAC']<-100
# df[,'VAC_LOG']<-100
rownames(df) = seq(length=nrow(df))

preds <- list()
preds_lower<-list()
preds_uppper <- list()
for(i in 1:nyc.k){
  bsts.model <- model[[paste("nyc",i,sep="_")]]
  burn<-200
  # rolling to get prediction result
  p<-predict.bsts(bsts.model, newdata = df, horizon = nrow(df), burn = burn, quantiles = c(.025, .975))
  if(length(preds)==0){
    preds <- p$mean* (max(nyc.ssa2$U[,i])-min(nyc.ssa2$U[,i]))/100 + min(nyc.ssa2$U[,i])
    preds_lower <- p$interval[1,]* (max(nyc.ssa2$U[,i])-min(nyc.ssa2$U[,i]))/100 + min(nyc.ssa2$U[,i])
    preds_upper <- p$interval[2,]* (max(nyc.ssa2$U[,i])-min(nyc.ssa2$U[,i]))/100 + min(nyc.ssa2$U[,i])
  }
  else{
    preds<-cbind(preds, p$mean* (max(nyc.ssa2$U[,i])-min(nyc.ssa2$U[,i]))/100+min(nyc.ssa2$U[,i]))
    preds_lower <- cbind(preds_lower,  p$interval[1,]* (max(nyc.ssa2$U[,i])-min(nyc.ssa2$U[,i]))/100+min(nyc.ssa2$U[,i]))
    preds_upper <- cbind(preds_upper, p$interval[2,]* (max(nyc.ssa2$U[,i])-min(nyc.ssa2$U[,i]))/100+min(nyc.ssa2$U[,i]))
  }
}
predicted<- t(t((preds %*% diag(nyc.ssa2$sigma[1:nyc.k]) %*% t(calc.v(nyc.ssa2,1:nyc.k)))) * nyc.mean)
predicted_upper <- t(t((preds_upper %*% diag(nyc.ssa2$sigma[1:nyc.k]) %*% t(calc.v(nyc.ssa2,1:nyc.k)))) * nyc.mean)
predicted_lower <- t(t((preds_lower %*% diag(nyc.ssa2$sigma[1:nyc.k]) %*% t(calc.v(nyc.ssa2,1:nyc.k)))) * nyc.mean)

write.csv(predicted, "result/nyc_pred2.csv")
write.csv(predicted_upper, "result/nyc_pred_upper2.csv")
write.csv(predicted_lower, "result/nyc_pred_lower2.csv")

## chicago
# Calculate test error
df <- chicago.df_
for(i in 1:day){
  df<-rbind(df, df[nrow(df), ]+(df[nrow(df), ]-df[nrow(df)-1, ]))
}
rownames(df) = seq(length=nrow(df))

df[(nrow(df)-364):(nrow(df)),c('RAIN','RAIN_LOG','TMAX','TMIN',
                               'CASE')]<-
  df[(nrow(df)-364-365):(nrow(df)-365),c('RAIN','RAIN_LOG','TMAX','TMIN',
                                         'CASE')]

# Policy after 2020-7-30 got repeated again
policy_after_july<-unlist(policy_chicago)[df[(nrow(df)-365-365), unlist(policy_chicago)]==0]

df[(nrow(df)-364):(nrow(df)),policy_after_july]<-df[(nrow(df)-364):(nrow(df)),policy_after_july]+
  df[(nrow(df)-364-365):(nrow(df)-365),policy_after_july]

# Policy during early 2020 got repeated again
# policy_before_july<-policy_chicago[df[(nrow(df)-365-365), policy_chicago]>0]
# 
# df[(nrow(df)-210):(nrow(df)),policy_before_july]<-df[(nrow(df)-210):(nrow(df)),policy_before_july]+
#   df[(nrow(df)-210-365-366):(nrow(df)-365-366),policy_before_july]


df <- df[(nrow(df)-364):(nrow(df)), ]
df[,'CASE']<-mean(df[335:365,'CASE'])
# df[,'CASE_LOG']<-mean(df[335:365,'CASE_LOG'])
df[,'VAC']<-100
# df[,'VAC_LOG']<-100
rownames(df) = seq(length=nrow(df))

preds <- list()
preds_lower<-list()
preds_uppper <- list()
for(i in 1:chicago.k){
  bsts.model <- model[[paste("chicago",i,sep="_")]]
  burn<-200
  # rolling to get prediction result
  p<-predict.bsts(bsts.model, newdata = df, horizon = nrow(df), burn = burn, quantiles = c(.025, .975))
  if(length(preds)==0){
    preds <- p$mean* (max(chicago.ssa2$U[,i])-min(chicago.ssa2$U[,i]))/100 + min(chicago.ssa2$U[,i])
    preds_lower <- p$interval[1,]* (max(chicago.ssa2$U[,i])-min(chicago.ssa2$U[,i]))/100 + min(chicago.ssa2$U[,i])
    preds_upper <- p$interval[2,]* (max(chicago.ssa2$U[,i])-min(chicago.ssa2$U[,i]))/100 + min(chicago.ssa2$U[,i])
  }
  else{
    preds<-cbind(preds, p$mean* (max(chicago.ssa2$U[,i])-min(chicago.ssa2$U[,i]))/100+min(chicago.ssa2$U[,i]))
    preds_lower <- cbind(preds_lower,  p$interval[1,]* (max(chicago.ssa2$U[,i])-min(chicago.ssa2$U[,i]))/100+min(chicago.ssa2$U[,i]))
    preds_upper <- cbind(preds_upper, p$interval[2,]* (max(chicago.ssa2$U[,i])-min(chicago.ssa2$U[,i]))/100+min(chicago.ssa2$U[,i]))
  }
}
predicted<- t(t((preds %*% diag(chicago.ssa2$sigma[1:chicago.k]) %*% t(calc.v(chicago.ssa2,1:chicago.k)))) * chicago.mean)
predicted_upper <- t(t((preds_upper %*% diag(chicago.ssa2$sigma[1:chicago.k]) %*% t(calc.v(chicago.ssa2,1:chicago.k)))) * chicago.mean)
predicted_lower <- t(t((preds_lower %*% diag(chicago.ssa2$sigma[1:chicago.k]) %*% t(calc.v(chicago.ssa2,1:chicago.k)))) * chicago.mean)

write.csv(predicted, "result/chicago_pred2.csv")
write.csv(predicted_upper, "result/chicago_pred_upper2.csv")
write.csv(predicted_lower, "result/chicago_pred_lower2.csv")

