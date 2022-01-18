# Step 5, first understand the model performance
# then understand model coefficients
library(Rssa)
library(bsts)
library(plyr)
library(data.table)

varname = 'TCP'
# Inclusive probability, coefficient
load(paste(varname, "BSTS_model_52_new.Rdata", sep="_"))
df_nyc<-summary(model[[paste('nyc', 1, sep="_")]])$coefficients
for(k in 2:nyc.k){
  df_nyc<-rbind(df_nyc,summary(model[[paste('nyc', k, sep="_")]])$coefficients)
}
df_chicago<-summary(model[[paste('chicago', 1, sep="_")]])$coefficients
for(k in 2:chicago.k){
  df_chicago<-rbind(df_chicago,summary(model[[paste('chicago', k, sep="_")]])$coefficients)
}
#
write.csv(df_nyc,paste("result/coef_nyc.csv", sep=""))
write.csv(df_chicago, paste("result/coef_chicago.csv", sep=""))


# Daily contribution to mobility, mean, median and percentile
varname = 'TCP'
# Inclusive probability, coefficient
load(paste(varname, "BSTS_model_52_revised_new.Rdata", sep="_"))
model<-res
k<-1

nyc_policy <- dimnames(model[[paste('nyc', k, sep="_")]]$coefficients)[[2]]
nyc_policy <- nyc_policy[2:length(nyc_policy)]

nyc_coef_75 <- stack(lapply(data.frame(model[[paste('nyc', k, sep="_")]]$coefficients)[-(1:200),nyc_policy],quantile,0.75, names = TRUE))$values
nyc_coef_25 <- stack(lapply(data.frame(model[[paste('nyc', k, sep="_")]]$coefficients)[-(1:200),nyc_policy],quantile,0.25, names = TRUE))$values
nyc_coef_95 <- stack(lapply(data.frame(model[[paste('nyc', k, sep="_")]]$coefficients)[-(1:200),nyc_policy],quantile,0.95, names = TRUE))$values
nyc_coef_5 <- stack(lapply(data.frame(model[[paste('nyc', k, sep="_")]]$coefficients)[-(1:200),nyc_policy],quantile,0.05, names = TRUE))$values
nyc_coef_mean <-  stack(lapply(data.frame(model[[paste('nyc', k, sep="_")]]$coefficients)[-(1:200),nyc_policy],mean, names = TRUE))$values
nyc_coef_median <-  stack(lapply(data.frame(model[[paste('nyc', k, sep="_")]]$coefficients)[-(1:200),nyc_policy],quantile,0.5, names = TRUE))$values

nyc_u_75 <- data.frame(t(data.frame(colMeans(t(t(as.matrix(nyc.df_)[,nyc_policy]) * nyc_coef_75) * (max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100))))
nyc_u_25 <- data.frame(t(data.frame(colMeans(t(t(as.matrix(nyc.df_)[,nyc_policy]) * nyc_coef_25) * (max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100))))
nyc_u_95 <- data.frame(t(data.frame(colMeans(t(t(as.matrix(nyc.df_)[,nyc_policy]) * nyc_coef_95) * (max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100))))
nyc_u_5 <- data.frame(t(data.frame(colMeans(t(t(as.matrix(nyc.df_)[,nyc_policy]) * nyc_coef_5) * (max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100))))
nyc_u_mean <- data.frame(t(data.frame(colMeans(t(t(as.matrix(nyc.df_)[,nyc_policy]) * nyc_coef_mean) * (max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100))))
nyc_u_median <- data.frame(t(data.frame(colMeans(t(t(as.matrix(nyc.df_)[,nyc_policy]) * nyc_coef_median) * (max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100))))


chicago_policy <- dimnames(model[[paste('chicago', k, sep="_")]]$coefficients)[[2]]
chicago_policy <- chicago_policy[2:length(chicago_policy)]

chicago_coef_75 <- stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:200),chicago_policy],quantile,0.75, names = TRUE))$values
chicago_coef_25 <- stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:200),chicago_policy],quantile,0.25, names = TRUE))$values
chicago_coef_95 <- stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:200),chicago_policy],quantile,0.95, names = TRUE))$values
chicago_coef_5 <- stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:200),chicago_policy],quantile,0.05, names = TRUE))$values
chicago_coef_mean <-  stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:200), chicago_policy],mean, names = TRUE))$values
chicago_coef_median <- stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:200),chicago_policy],quantile,0.5, names = TRUE))$values

chicago_u_75 <- data.frame(t(data.frame(colMeans(t(t(as.matrix(chicago.df_)[,chicago_policy]) * chicago_coef_75) * (max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100))))
chicago_u_25 <- data.frame(t(data.frame(colMeans(t(t(as.matrix(chicago.df_)[,chicago_policy]) * chicago_coef_25) * (max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100))))
chicago_u_95 <- data.frame(t(data.frame(colMeans(t(t(as.matrix(chicago.df_)[,chicago_policy]) * chicago_coef_95) * (max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100))))
chicago_u_5 <- data.frame(t(data.frame(colMeans(t(t(as.matrix(chicago.df_)[,chicago_policy]) * chicago_coef_5) * (max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100))))
chicago_u_mean <-  data.frame(t(data.frame(colMeans(t(t(as.matrix(chicago.df_)[,chicago_policy]) * chicago_coef_mean) * (max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100))))
chicago_u_median <-  data.frame(t(data.frame(colMeans(t(t(as.matrix(chicago.df_)[,chicago_policy]) * chicago_coef_median)* (max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100))))

for(k in 2:nyc.k){
  nyc_policy <- dimnames(model[[paste('nyc', k, sep="_")]]$coefficients)[[2]]
  nyc_policy <- nyc_policy[2:length(nyc_policy)]
  
  nyc_coef_75 <- stack(lapply(data.frame(model[[paste('nyc', k, sep="_")]]$coefficients)[-(1:200),nyc_policy],quantile,0.75, names = TRUE))$values
  nyc_coef_25 <- stack(lapply(data.frame(model[[paste('nyc', k, sep="_")]]$coefficients)[-(1:200),nyc_policy],quantile,0.25, names = TRUE))$values
  nyc_coef_95 <- stack(lapply(data.frame(model[[paste('nyc', k, sep="_")]]$coefficients)[-(1:200),nyc_policy],quantile,0.95, names = TRUE))$values
  nyc_coef_5 <- stack(lapply(data.frame(model[[paste('nyc', k, sep="_")]]$coefficients)[-(1:200),nyc_policy],quantile,0.05, names = TRUE))$values
  nyc_coef_mean <-  stack(lapply(data.frame(model[[paste('nyc', k, sep="_")]]$coefficients)[-(1:200),nyc_policy],mean, names = TRUE))$values
  nyc_coef_median <-  stack(lapply(data.frame(model[[paste('nyc', k, sep="_")]]$coefficients)[-(1:200),nyc_policy],quantile,0.5, names = TRUE))$values
  
  
  nyc_u_75 <- rbind.fill(nyc_u_75, data.frame(t(data.frame(colMeans(t(t(as.matrix(nyc.df_)[,nyc_policy]) * nyc_coef_75) * (max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100)))))
  nyc_u_25 <- rbind.fill(nyc_u_25,data.frame(t(data.frame(colMeans(t(t(as.matrix(nyc.df_)[,nyc_policy]) * nyc_coef_25) * (max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100)))))
  nyc_u_95 <- rbind.fill(nyc_u_95,data.frame(t(data.frame(colMeans(t(t(as.matrix(nyc.df_)[,nyc_policy]) * nyc_coef_95) * (max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100)))))
  nyc_u_5 <- rbind.fill(nyc_u_5,data.frame(t(data.frame(colMeans(t(t(as.matrix(nyc.df_)[,nyc_policy]) * nyc_coef_5) * (max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100)))))
  nyc_u_mean <- rbind.fill(nyc_u_mean,data.frame(t(data.frame(colMeans(t(t(as.matrix(nyc.df_)[,nyc_policy]) * nyc_coef_mean) * (max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100)))))
  nyc_u_median <- rbind.fill(nyc_u_median,data.frame(t(data.frame(colMeans(t(t(as.matrix(nyc.df_)[,nyc_policy]) * nyc_coef_median) * (max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100)))))
}

for(k in 2:chicago.k){
  chicago_policy <- dimnames(model[[paste('chicago', k, sep="_")]]$coefficients)[[2]]
  chicago_policy <- chicago_policy[2:length(chicago_policy)]
  if(length(chicago_policy)==0){
    # do nothing
  }
  else if(length(chicago_policy)==1){
    chicago_coef_75 <- stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:200),],quantile,0.75, names = TRUE))[2,]
    chicago_coef_25 <- stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:200),],quantile,0.25, names = TRUE))[2,]
    chicago_coef_95 <- stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:200),],quantile,0.95, names = TRUE))[2,]
    chicago_coef_5 <- stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:200),],quantile,0.05, names = TRUE))[2,]
    chicago_coef_mean <-  stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:200), ],mean, names = TRUE))[2,]
    chicago_coef_median <- stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:200),],quantile,0.5, names = TRUE))[2,]
  }
  else{
    #SggestBurn(0.2, model[[paste('chicago', k, sep="_")]]
    chicago_coef_75 <- stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:200),chicago_policy],quantile,0.75, names = TRUE))
    chicago_coef_25 <- stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:200),chicago_policy],quantile,0.25, names = TRUE))
    chicago_coef_95 <- stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:200),chicago_policy],quantile,0.95, names = TRUE))
    chicago_coef_5 <- stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:200),chicago_policy],quantile,0.05, names = TRUE))
    chicago_coef_mean <-  stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:200), chicago_policy],mean, names = TRUE))
    chicago_coef_median <- stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:200),chicago_policy],quantile,0.5, names = TRUE))
  }
    
  chicago_u_75 <- rbind.fill(chicago_u_75, data.frame(t(data.frame(colMeans(t(t(as.matrix(chicago.df_)[,chicago_policy]) * chicago_coef_75$values) * (max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100)))))
  chicago_u_25 <- rbind.fill(chicago_u_25, data.frame(t(data.frame(colMeans(t(t(as.matrix(chicago.df_)[,chicago_policy]) * chicago_coef_25$values) * (max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100)))))
  chicago_u_95 <- rbind.fill(chicago_u_95, data.frame(t(data.frame(colMeans(t(t(as.matrix(chicago.df_)[,chicago_policy]) * chicago_coef_95$values) * (max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100)))))
  chicago_u_5 <- rbind.fill(chicago_u_5, data.frame(t(data.frame(colMeans(t(t(as.matrix(chicago.df_)[,chicago_policy]) * chicago_coef_5$values) * (max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100)))))
  chicago_u_mean <-  rbind.fill(chicago_u_mean, data.frame(t(data.frame(colMeans(t(t(as.matrix(chicago.df_)[,chicago_policy]) * chicago_coef_mean$values) * (max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100)))))
  chicago_u_median <-  rbind.fill(chicago_u_median, data.frame(t(data.frame(colMeans(t(t(as.matrix(chicago.df_)[,chicago_policy]) * chicago_coef_median$values)* (max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100)))))
}

write.csv(chicago_u_75, "result/chicago_u_75.csv")
write.csv(nyc_u_75, "result/nyc_u_75.csv")
write.csv(chicago_u_25, "result/chicago_u_25.csv")
write.csv(nyc_u_25, "result/nyc_u_25.csv")
write.csv(chicago_u_95, "result/chicago_u_95.csv")
write.csv(nyc_u_95, "result/nyc_u_95.csv")
write.csv(chicago_u_5, "result/chicago_u_5.csv")
write.csv(nyc_u_5, "result/nyc_u_5.csv")
write.csv(chicago_u_mean, "result/chicago_u_mean.csv")
write.csv(nyc_u_mean, "result/nyc_u_mean.csv")
write.csv(chicago_u_median, "result/chicago_u_median.csv")
write.csv(nyc_u_median, "result/nyc_u_median.csv")



k<-1
nyc_contrib_mean <- colSums(colMeans(model[['nyc_1']]$state.contributions))*(max(nyc.ssa2$U[,1])-min(nyc.ssa2$U[,1]))/100+min(nyc.ssa2$U[,1])
for(k in 2:nyc.k){
  nyc_contrib_mean<-cbind(nyc_contrib_mean, colSums(colMeans(model[[paste('nyc', k, sep="_")]]$state.contributions))*(max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100+min(nyc.ssa2$U[,k]))
}

k<-1
chicago_contrib_mean <- colSums(colMeans(model[['chicago_1']]$state.contributions))*(max(chicago.ssa2$U[,1])-min(chicago.ssa2$U[,1]))/100+min(chicago.ssa2$U[,1])
for(k in 2:chicago.k){
  chicago_contrib_mean<-cbind(chicago_contrib_mean, colSums(colMeans(model[[paste('chicago', k, sep="_")]]$state.contributions))*(max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100+min(chicago.ssa2$U[,k]))
}
write.csv(nyc_contrib_mean, "result/nyc_contrib_mean.csv")
write.csv(chicago_contrib_mean, "result/chicago_contrib_mean.csv")

# nyc_trend<-list()
# nyc_seasonal<- list()
# nyc_regression <- list()
# 
# chicago_trend<-list()
# chicago_seasonal<- list()
# chicago_regression <- list()
# 
# for(k in 1:nyc.k){
#   burn <- SuggestBurn(.1, model[[paste('nyc', k, sep="_")]])
#   state <- model[[paste('nyc', k, sep="_")]]$state.contributions
#   state <- state[-(1:burn), , , drop = FALSE]
#   state <- apply(state,c(2,3),mean)
#   if(length(nyc_trend)==0){
#     nyc_trend<-state[1,]
#   }
#   else{
#     nyc_trend<-rbind(nyc_trend,state[1,])
#   }
#   if(length(nyc_seasonal)==0){
#     nyc_seasonal<-state[2,]
#   }
#   else{
#     nyc_seasonal<-rbind(nyc_seasonal,state[2,])
#   }
#   if(length(nyc_regression)==0){
#     nyc_regression<-state[3,]
#   }
#   else{
#     nyc_regression<-rbind(nyc_regression,state[3,])
#   }
# }
# 
# for(k in 1:chicago.k){
#   burn <- SuggestBurn(.1, model[[paste('chicago', k, sep="_")]])
#   state <- model[[paste('chicago', k, sep="_")]]$state.contributions
#   state <- state[-(1:burn), , , drop = FALSE]
#   state <- apply(state,c(2,3),mean)
#   if(length(chicago_trend)==0){
#     chicago_trend<-state[1,]
#   }
#   else{
#     chicago_trend<-rbind(chicago_trend,state[1,])
#   }
#   if(length(chicago_seasonal)==0){
#     chicago_seasonal<-state[2,]
#   }
#   else{
#     chicago_seasonal<-rbind(chicago_seasonal,state[2,])
#   }
#   if(length(chicago_regression)==0){
#     chicago_regression<-state[3,]
#   }
#   else{
#     chicago_regression<-rbind(chicago_regression,state[3,])
#   }
# }
# 
# dehankelize<-function(y, L = 0){
#   if(is.null(dim(y))){
#     return(y)
#   }
#   if(L>0){
#     res <- dehankelize(y[,1:(L)])
#     for(i in 2:(dim(y)[2]/(L))){
#       res <- cbind(res,dehankelize(y[,((i-1)*(L)+1):(i*(L))]))
#     }
#     return(res)
#   }
#   else{
#     if(is.null(dim(y))){
#       y<-t(matrix(y)) # 1 * 7
#     }
#     res <- rep(0, dim(y)[1]+dim(y)[2]-1)
#     for(i in 1:dim(y)[2]){
#       res[i:(dim(y)[1]+i-1)] <- res[i:(dim(y)[1]+i-1)] + y[,i]
#     }
#     if(dim(y)[1]>dim(y)[2]+2){
#       res[(dim(y)[2]+1):(dim(y)[1]-1)] <-  res[(dim(y)[2]+1):(dim(y)[1]-1)]/(dim(y)[2])
#     }
#     for(i in 1:dim(y)[2]){
#       res[i+dim(y)[1]-1] <-  res[i+dim(y)[1]-1]/min((dim(y)[2]-i+1),dim(y)[1])
#       res[i] <-  res[i]/min(dim(y)[1],i)
#     }
#     return(res)
#   }
# }

# nyc_trend <- t(nyc_trend) %*% diag(nyc.ssa2$sigma[1:nyc.k]) %*% t(calc.v(nyc.ssa2,1:nyc.k))
# nyc_trend2<-dehankelize(nyc_trend,7)
# 
# nyc_seasonal <- t(nyc_seasonal) %*% diag(nyc.ssa2$sigma[1:nyc.k]) %*% t(calc.v(nyc.ssa2,1:nyc.k))
# nyc_seasonal2<-dehankelize(nyc_seasonal,7)
# 
# nyc_regression <- t(nyc_regression) %*% diag(nyc.ssa2$sigma[1:nyc.k]) %*% t(calc.v(nyc.ssa2,1:nyc.k))
# nyc_regression2<-dehankelize(nyc_regression,7)
# 
# chicago_trend <- t(chicago_trend) %*% diag(chicago.ssa2$sigma[1:chicago.k]) %*% t(calc.v(chicago.ssa2,1:chicago.k))
# chicago_trend2<-dehankelize(chicago_trend,7)
# 
# chicago_seasonal <- t(chicago_seasonal) %*% diag(chicago.ssa2$sigma[1:chicago.k]) %*% t(calc.v(chicago.ssa2,1:chicago.k))
# chicago_seasonal2<-dehankelize(chicago_seasonal,7)
# 
# chicago_regression <- t(chicago_regression) %*% diag(chicago.ssa2$sigma[1:chicago.k]) %*% t(calc.v(chicago.ssa2,1:chicago.k))
# chicago_regression2<-dehankelize(chicago_regression,7)

nyc_v <- calc.v(nyc.ssa2,1:nyc.k) %*% diag(nyc.ssa2$sigma[1:nyc.k])
chicago_v <- calc.v(chicago.ssa2, 1:chicago.k) %*% diag(chicago.ssa2$sigma[1:chicago.k])

nyc_v <- nyc_v * nyc.mean
chicago_v <- chicago_v * chicago.mean

write.csv(nyc_v, "result/nyc_v.csv")
write.csv(chicago_v, "result/chicago_v.csv")
# 
# write.csv(nyc_trend2,paste("result/nyc_trend2.csv", sep=""))
# write.csv(nyc_trend, paste("result/nyc_trend.csv", sep=""))
# 
# write.csv(nyc_seasonal2,paste("result/nyc_seasonal2.csv", sep=""))
# write.csv(nyc_seasonal, paste("result/nyc_seasonal.csv", sep=""))
# 
# write.csv(nyc_regression2,paste("result/nyc_regression2.csv", sep=""))
# write.csv(nyc_regression, paste("result/nyc_regression.csv", sep=""))
# 
# write.csv(chicago_trend2,paste("result/chicago_trend2.csv", sep=""))
# write.csv(chicago_trend, paste("result/chicago_trend.csv", sep=""))
# 
# write.csv(chicago_seasonal2,paste("result/chicago_seasonal2.csv", sep=""))
# write.csv(chicago_seasonal, paste("result/chicago_seasonal.csv", sep=""))
# 
# write.csv(chicago_regression2,paste("result/chicago_regression2.csv", sep=""))
# write.csv(chicago_regression, paste("result/chicago_regression.csv", sep=""))


# k<-1
# 
# nyc_coef_75 <- stack(lapply(data.frame(res[[paste('nyc', k, sep="_")]]$coefficients)[-(1:SuggestBurn(0.1, model[[paste('nyc', k, sep="_")]])),2:dim(res[[paste('nyc', k, sep="_")]]$coefficients)[2]],quantile,0.75, names = TRUE))$values
# nyc_coef_25 <- stack(lapply(data.frame(res[[paste('nyc', k, sep="_")]]$coefficients)[-(1:SuggestBurn(0.1, model[[paste('nyc', k, sep="_")]])),2:dim(res[[paste('nyc', k, sep="_")]]$coefficients)[2]],quantile,0.25, names = TRUE))$values
# nyc_coef_95 <- stack(lapply(data.frame(res[[paste('nyc', k, sep="_")]]$coefficients)[-(1:SuggestBurn(0.1, model[[paste('nyc', k, sep="_")]])),2:dim(res[[paste('nyc', k, sep="_")]]$coefficients)[2]],quantile,0.95, names = TRUE))$values
# nyc_coef_5 <- stack(lapply(data.frame(res[[paste('nyc', k, sep="_")]]$coefficients)[-(1:SuggestBurn(0.1, model[[paste('nyc', k, sep="_")]])),2:dim(res[[paste('nyc', k, sep="_")]]$coefficients)[2]],quantile,0.05, names = TRUE))$values
# nyc_coef_mean <-  stack(lapply(data.frame(res[[paste('nyc', k, sep="_")]]$coefficients)[-(1:SuggestBurn(0.1, model[[paste('nyc', k, sep="_")]])),2:dim(res[[paste('nyc', k, sep="_")]]$coefficients)[2]],mean, names = TRUE))$values
# nyc_coef_median <-  stack(lapply(data.frame(res[[paste('nyc', k, sep="_")]]$coefficients)[-(1:SuggestBurn(0.1, model[[paste('nyc', k, sep="_")]])),2:dim(res[[paste('nyc', k, sep="_")]]$coefficients)[2]],quantile,0.5, names = TRUE))$values
# 
# nyc_u_75 <- colMeans(t(t(as.matrix(nyc.df_)[,c(-5,-6,-7,-8)]) * nyc_coef_75) * (max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100)
# nyc_u_25 <- colMeans(t(t(as.matrix(nyc.df_)[,c(-5,-6,-7,-8)]) * nyc_coef_25) * (max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100)
# nyc_u_95 <- colMeans(t(t(as.matrix(nyc.df_)[,c(-5,-6,-7,-8)]) * nyc_coef_95) * (max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100)
# nyc_u_5 <- colMeans(t(t(as.matrix(nyc.df_)[,c(-5,-6,-7,-8)]) * nyc_coef_5) * (max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100)
# nyc_u_mean <- colMeans(t(t(as.matrix(nyc.df_)[,c(-5,-6,-7,-8)]) * nyc_coef_mean) * (max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100)
# nyc_u_median <- colMeans(t(t(as.matrix(nyc.df_)[,c(-5,-6,-7,-8)]) * nyc_coef_median) * (max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100)
# 
# chicago_coef_75 <- stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:SuggestBurn(0.1, model[[paste('chicago', k, sep="_")]])),2:dim(res[[paste('chicago', k, sep="_")]]$coefficients)[2]],quantile,0.75, names = TRUE))$values
# chicago_coef_25 <- stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:SuggestBurn(0.1, model[[paste('chicago', k, sep="_")]])),2:dim(res[[paste('chicago', k, sep="_")]]$coefficients)[2]],quantile,0.25, names = TRUE))$values
# chicago_coef_95 <- stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:SuggestBurn(0.1, model[[paste('chicago', k, sep="_")]])),2:dim(res[[paste('chicago', k, sep="_")]]$coefficients)[2]],quantile,0.95, names = TRUE))$values
# chicago_coef_5 <- stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:SuggestBurn(0.1, model[[paste('chicago', k, sep="_")]])),2:dim(res[[paste('chicago', k, sep="_")]]$coefficients)[2]],quantile,0.05, names = TRUE))$values
# chicago_coef_mean <-  stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:SuggestBurn(0.1, model[[paste('chicago', k, sep="_")]])), 2:dim(res[[paste('chicago', k, sep="_")]]$coefficients)[2]],mean, names = TRUE))$values
# chicago_coef_median <- stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:SuggestBurn(0.1, model[[paste('chicago', k, sep="_")]])),2:dim(res[[paste('chicago', k, sep="_")]]$coefficients)[2]],quantile,0.5, names = TRUE))$values
# 
# chicago_u_75 <- colMeans(t(t(as.matrix(chicago.df_)[,c(-5,-6,-7,-8)]) * chicago_coef_75) * (max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100)
# chicago_u_25 <- colMeans(t(t(as.matrix(chicago.df_)[,c(-5,-6,-7,-8)]) * chicago_coef_25) * (max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100)
# chicago_u_95 <- colMeans(t(t(as.matrix(chicago.df_)[,c(-5,-6,-7,-8)]) * chicago_coef_95) * (max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100)
# chicago_u_5 <- colMeans(t(t(as.matrix(chicago.df_)[,c(-5,-6,-7,-8)]) * chicago_coef_5) * (max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100)
# chicago_u_mean <-  colMeans(t(t(as.matrix(chicago.df_)[,c(-5,-6,-7,-8)]) * chicago_coef_mean) * (max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100)
# chicago_u_median <-  colMeans(t(t(as.matrix(chicago.df_)[,c(-5,-6,-7,-8)]) * chicago_coef_median)* (max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100)
# 
# for(k in 2:nyc.k){
#   nyc_coef_75 <- stack(lapply(data.frame(model[[paste('nyc', k, sep="_")]]$coefficients)[-(1:SuggestBurn(0.1, model[[paste('nyc', k, sep="_")]])),2:35],quantile,0.75, names = TRUE))$values
#   nyc_coef_25 <- stack(lapply(data.frame(model[[paste('nyc', k, sep="_")]]$coefficients)[-(1:SuggestBurn(0.1, model[[paste('nyc', k, sep="_")]])),2:35],quantile,0.25, names = TRUE))$values
#   nyc_coef_95 <- stack(lapply(data.frame(model[[paste('nyc', k, sep="_")]]$coefficients)[-(1:SuggestBurn(0.1, model[[paste('nyc', k, sep="_")]])),2:35],quantile,0.95, names = TRUE))$values
#   nyc_coef_5 <- stack(lapply(data.frame(model[[paste('nyc', k, sep="_")]]$coefficients)[-(1:SuggestBurn(0.1, model[[paste('nyc', k, sep="_")]])),2:35],quantile,0.05, names = TRUE))$values
#   nyc_coef_mean <-  stack(lapply(data.frame(model[[paste('nyc', k, sep="_")]]$coefficients)[-(1:SuggestBurn(0.1, model[[paste('nyc', k, sep="_")]])),2:35],mean, names = TRUE))$values
#   nyc_coef_median <-  stack(lapply(data.frame(model[[paste('nyc', k, sep="_")]]$coefficients)[-(1:SuggestBurn(0.1, model[[paste('nyc', k, sep="_")]])),2:35],quantile,0.5, names = TRUE))$values
#   
#   nyc_u_75 <- cbind(nyc_u_75, colMeans(t(t(as.matrix(nyc.df_)[,c(-5,-6,-7,-8)]) * nyc_coef_75) * (max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100))
#   nyc_u_25 <- cbind(nyc_u_25,colMeans(t(t(as.matrix(nyc.df_)[,c(-5,-6,-7,-8)]) * nyc_coef_25) * (max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100))
#   nyc_u_95 <- cbind(nyc_u_95,colMeans(t(t(as.matrix(nyc.df_)[,c(-5,-6,-7,-8)]) * nyc_coef_95) * (max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100))
#   nyc_u_5 <- cbind(nyc_u_5,colMeans(t(t(as.matrix(nyc.df_)[,c(-5,-6,-7,-8)]) * nyc_coef_5) * (max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100))
#   nyc_u_mean <- cbind(nyc_u_mean,colMeans(t(t(as.matrix(nyc.df_)[,c(-5,-6,-7,-8)]) * nyc_coef_mean) * (max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100))
#   nyc_u_median <- cbind(nyc_u_median,colMeans(t(t(as.matrix(nyc.df_)[,c(-5,-6,-7,-8)]) * nyc_coef_median) * (max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100))
#   
# }
# 
# for(k in 2:chicago.k){
#   chicago_coef_75 <- stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:SuggestBurn(0.1, model[[paste('chicago', k, sep="_")]])),2:32],quantile,0.75, names = TRUE))$values
#   chicago_coef_25 <- stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:SuggestBurn(0.1, model[[paste('chicago', k, sep="_")]])),2:32],quantile,0.25, names = TRUE))$values
#   chicago_coef_95 <- stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:SuggestBurn(0.1, model[[paste('chicago', k, sep="_")]])),2:32],quantile,0.95, names = TRUE))$values
#   chicago_coef_5 <- stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:SuggestBurn(0.1, model[[paste('chicago', k, sep="_")]])),2:32],quantile,0.05, names = TRUE))$values
#   chicago_coef_mean <-  stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:SuggestBurn(0.1, model[[paste('chicago', k, sep="_")]])), 2:32],mean, names = TRUE))$values
#   chicago_coef_median <- stack(lapply(data.frame(model[[paste('chicago', k, sep="_")]]$coefficients)[-(1:SuggestBurn(0.1, model[[paste('chicago', k, sep="_")]])),2:32],quantile,0.5, names = TRUE))$values
#   
#   chicago_u_75 <- cbind(chicago_u_75, colMeans(t(t(as.matrix(chicago.df_)[,c(-5,-6,-7,-8)]) * chicago_coef_75) * (max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100))
#   chicago_u_25 <- cbind(chicago_u_25, colMeans(t(t(as.matrix(chicago.df_)[,c(-5,-6,-7,-8)]) * chicago_coef_25) * (max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100))
#   chicago_u_95 <- cbind(chicago_u_95, colMeans(t(t(as.matrix(chicago.df_)[,c(-5,-6,-7,-8)]) * chicago_coef_95) * (max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100))
#   chicago_u_5 <- cbind(chicago_u_5, colMeans(t(t(as.matrix(chicago.df_)[,c(-5,-6,-7,-8)]) * chicago_coef_5) * (max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100))
#   chicago_u_mean <-  cbind(chicago_u_mean, colMeans(t(t(as.matrix(chicago.df_)[,c(-5,-6,-7,-8)]) * chicago_coef_mean) * (max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100))
#   chicago_u_median <-  cbind(chicago_u_median, colMeans(t(t(as.matrix(chicago.df_)[,c(-5,-6,-7,-8)]) * chicago_coef_median)* (max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100))
# }
# 
# write.csv(chicago_u_75, "result/chicago_u_75.csv")
# write.csv(nyc_u_75, "result/nyc_u_75.csv")
# write.csv(chicago_u_25, "result/chicago_u_25.csv")
# write.csv(nyc_u_25, "result/nyc_u_25.csv")
# write.csv(chicago_u_95, "result/chicago_u_95.csv")
# write.csv(nyc_u_95, "result/nyc_u_95.csv")
# write.csv(chicago_u_5, "result/chicago_u_5.csv")
# write.csv(nyc_u_5, "result/nyc_u_5.csv")
# write.csv(chicago_u_mean, "result/chicago_u_mean.csv")
# write.csv(nyc_u_mean, "result/nyc_u_mean.csv")
# write.csv(chicago_u_median, "result/chicago_u_median.csv")
# write.csv(nyc_u_median, "result/nyc_u_median.csv")
# 
# 
# k<-1
# nyc_contrib_mean <- colSums(colMeans(model[['nyc_1']]$state.contributions))*(max(nyc.ssa2$U[,1])-min(nyc.ssa2$U[,1]))/100+min(nyc.ssa2$U[,1])
# for(k in 2:nyc.k){
#   nyc_contrib_mean<-cbind(nyc_contrib_mean, colSums(colMeans(model[[paste('nyc', k, sep="_")]]$state.contributions))*(max(nyc.ssa2$U[,k])-min(nyc.ssa2$U[,k]))/100+min(nyc.ssa2$U[,k]))
# }
# 
# k<-1
# chicago_contrib_mean <- colSums(colMeans(model[['chicago_1']]$state.contributions))*(max(chicago.ssa2$U[,1])-min(chicago.ssa2$U[,1]))/100+min(chicago.ssa2$U[,1])
# for(k in 2:chicago.k){
#   chicago_contrib_mean<-cbind(chicago_contrib_mean, colSums(colMeans(model[[paste('chicago', k, sep="_")]]$state.contributions))*(max(chicago.ssa2$U[,k])-min(chicago.ssa2$U[,k]))/100+min(chicago.ssa2$U[,k]))
# }
# write.csv(nyc_contrib_mean, "result/nyc_contrib_mean.csv")
# write.csv(chicago_contrib_mean, "result/chicago_contrib_mean.csv")