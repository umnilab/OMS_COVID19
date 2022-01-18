sub<-'52'

ind<-1
y<-nyc.ssa2$U[,ind]
model<-auto.arima(y)
fit_n<-nyc.ssa2$U[,ind]+model$residuals
for(ind in 2:nyc.k){
  y<-nyc.ssa2$U[,ind]
  model<-auto.arima(y)
  fit_n<-cbind(fit_n, nyc.ssa2$U[,ind]+model$residuals)
}

actual<-nyc.ridership_
fitted <- fit_n %*% diag(nyc.ssa2$sigma[1:nyc.k]) %*% t(calc.v(nyc.ssa2,1:nyc.k))
one_step_nyc <- (actual-fitted)

ind<-1
y<-chicago.ssa2$U[,ind]
model<-auto.arima(y)
fit_c<-chicago.ssa2$U[,ind]+model$residuals
for(ind in 2:chicago.k){
  y<-chicago.ssa2$U[,ind]
  model<-auto.arima(y)
  fit_c<-cbind(fit_c, chicago.ssa2$U[,ind]+model$residuals)
}

actual<-chicago.ridership_
fitted <- fit_c %*% diag(chicago.ssa2$sigma[1:chicago.k]) %*% t(calc.v(chicago.ssa2,1:chicago.k))
one_step_chicago <- (actual-fitted)

one_step_c <-c()
one_step_n <-c()
for(h in 0:2){
  keep_ind <- ((142*h+1):((h+1)*142))[nyc.keep_ind[(142*h+1):((h+1)*142)]]
  if(length(one_step_n) == 0){
    one_step_n <- rowMeans(abs((one_step_nyc)[,keep_ind]))
  }else{
    one_step_n <- cbind(one_step_n, rowMeans(abs((one_step_nyc)[,keep_ind])))
  }
  keep_ind <- ((154*h+1):((h+1)*154))[chicago.keep_ind[(154*h+1):((h+1)*154)]]
  if(length(one_step_c) == 0){
    one_step_c <- rowMeans(abs((one_step_chicago)[,keep_ind]))
  }else{
    one_step_c <- cbind(one_step_c, rowMeans(abs((one_step_chicago)[,keep_ind])))
  }
}
write.csv(one_step_n, paste("result/nyc_one_step_ARIMA",sub,".csv", sep=""))
write.csv(one_step_c, paste("result/chicago_one_step_ARIMA",sub,".csv", sep=""))


#BSTS
varname = 'TCP'
# Inclusive probability, coefficient
load(paste(varname, "BSTS_model_52_revised.Rdata", sep="_"))
model<-res

ind<-1
y<-nyc.ssa2$U[,ind]
#model<-auto.arima(y)
burn <- 200
fit_n<-y+colMeans(model[[paste("nyc",ind,sep="_")]]$one.step.prediction.errors[-(1:burn),])*(max(nyc.ssa2$U[,ind])-min(nyc.ssa2$U[,ind]))/100
for(ind in 2:nyc.k){
  y<-nyc.ssa2$U[,ind]
  fit_n<-cbind(fit_n, y+colMeans(model[[paste("nyc",ind,sep="_")]]$one.step.prediction.errors[-(1:burn),])*(max(nyc.ssa2$U[,ind])-min(nyc.ssa2$U[,ind]))/100)
}

actual<-nyc.ridership_
fitted <- fit_n %*% diag(nyc.ssa2$sigma[1:nyc.k]) %*% t(calc.v(nyc.ssa2,1:nyc.k))
one_step_nyc <- (actual-fitted)

ind<-1
y<-chicago.ssa2$U[,ind]
model<-auto.arima(y)
fit_c<-y+colMeans(model[[paste("chicago",ind,sep="_")]]$one.step.prediction.errors[-(1:burn),])*(max(chicago.ssa2$U[,ind])-min(chicago.ssa2$U[,ind]))/100
for(ind in 2:chicago.k){
  y<-chicago.ssa2$U[,ind]
  fit_c<-cbind(fit_c, y+colMeans(model[[paste("chicago",ind,sep="_")]]$one.step.prediction.errors[-(1:burn),])*(max(chicago.ssa2$U[,ind])-min(chicago.ssa2$U[,ind]))/100)
}


actual<-chicago.ridership_
fitted <- fit_c %*% diag(chicago.ssa2$sigma[1:chicago.k]) %*% t(calc.v(chicago.ssa2,1:chicago.k))
one_step_chicago <- (actual-fitted)

one_step_c <-c()
one_step_n <-c()
for(h in 0:2){
  keep_ind <- ((142*h+1):((h+1)*142))[nyc.keep_ind[(142*h+1):((h+1)*142)]]
  if(length(one_step_n) == 0){
    one_step_n <- rowMeans(abs((one_step_nyc)[,keep_ind]))
  }else{
    one_step_n <- cbind(one_step_n, rowMeans(abs((one_step_nyc)[,keep_ind])))
  }
  keep_ind <- ((154*h+1):((h+1)*154))[chicago.keep_ind[(154*h+1):((h+1)*154)]]
  if(length(one_step_c) == 0){
    one_step_c <- rowMeans(abs((one_step_chicago)[,keep_ind]))
  }else{
    one_step_c <- cbind(one_step_c, rowMeans(abs((one_step_chicago)[,keep_ind])))
  }
}
write.csv(one_step_n, paste("result/nyc_one_step_BSTS",sub,".csv", sep=""))
write.csv(one_step_c, paste("result/chicago_one_step_BSTS",sub,".csv", sep=""))

