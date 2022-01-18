# QQ plot of estimated residuals
# Get one-step-error of the final model
library(Rssa)
library(bsts)

varname <- 'TCP'
load(paste(varname, "BSTS_model_52_revised_new.Rdata", sep="_"))
model <- res

nyc.one_step_error<-colMeans(model[['nyc_1']]$one.step.prediction.errors)
for (k in 2:10){
  nyc.one_step_error<- rbind(nyc.one_step_error,
                             colMeans(model[[paste("nyc",k,sep="_")]]$one.step.prediction.errors))
}

chicago.one_step_error<-colMeans(model[['chicago_1']]$one.step.prediction.errors)
for (k in 2:10){
  chicago.one_step_error<- rbind(chicago.one_step_error,
                             colMeans(model[[paste("chicago",k,sep="_")]]$one.step.prediction.errors))
}

write.csv(nyc.one_step_error, "result/nyc.one_step_error.csv")
write.csv(chicago.one_step_error, "result/chicago.one_step_error.csv")

# KS normality 
k<- 5
ks.test(nyc.one_step_error[k,], "pnorm", mean=mean(nyc.one_step_error[k,]), 
        sd=sd(nyc.one_step_error[k,]))