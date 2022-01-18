library(bsts)
library(ggplot2)
library(reshape2)
library(dplyr)

# ------
subs <- c("_52") #c("_20","_52","_20_1")
model_ind <-1
model_name <- c("N","T","C","P","TC","TP","CP","TCP")#, 
                #"LN","LT","LC","LP","LTC","LTP","LCP","LTCP")

varname1<-'nyc'
varname2<-'chicago'

for(sub in subs){
  one_step_c <- c()
  one_step_n <- c()
  for(model_ind in 1:8){
    varname <- model_name[model_ind]
    # Load model
    load(paste(varname, paste("BSTS_result",sub,"_new.Rdata", sep=""), sep="_"))
    # Calculate train error
    for(h in 0:2){
      keep_ind <- ((142*h+1):((h+1)*142))[nyc.keep_ind[(142*h+1):((h+1)*142)]]
      if(length(one_step_n) == 0){
        one_step_n <- rowMeans(abs(results$nyc_one_step[,keep_ind]))
      }else{
        one_step_n <- cbind(one_step_n, rowMeans(abs(results$nyc_one_step[,keep_ind])))
      }
      keep_ind <- ((154*h+1):((h+1)*154))[chicago.keep_ind[(154*h+1):((h+1)*154)]]
      if(length(one_step_c) == 0){
        one_step_c <- rowMeans(abs(results$chicago_one_step[,keep_ind]))
      }else{
        one_step_c <- cbind(one_step_c, rowMeans(abs(results$chicago_one_step[,keep_ind])))
      }
    }
    write.csv(one_step_n, paste("result/nyc_one_step",sub,".csv", sep=""))
    write.csv(one_step_c, paste("result/chicago_one_step",sub,".csv", sep=""))
  }
}

varname1<-'nyc'
varname2<-'chicago'

for(sub in subs){
  one_step_c <- c()
  one_step_n <- c()
  for(model_ind in 1:8){
    varname <- model_name[model_ind]
    # Load model
    load(paste(varname, paste("BSTS_result",sub,"_new.Rdata", sep=""), sep="_"))
    # Calculate train error
    for(h in 0:2){
      keep_ind <- ((142*h+1):((h+1)*142))[nyc.keep_ind[(142*h+1):((h+1)*142)]]
      if(length(one_step_n) == 0){
        one_step_n <- rowMeans(abs((results$nyc_one_step)[,keep_ind]))
      }else{
        one_step_n <- cbind(one_step_n, rowMeans(abs((results$nyc_one_step)[,keep_ind])))
      }
      keep_ind <- ((154*h+1):((h+1)*154))[chicago.keep_ind[(154*h+1):((h+1)*154)]]
      if(length(one_step_c) == 0){
        one_step_c <- rowMeans(abs((results$chicago_one_step)[,keep_ind]))
      }else{
        one_step_c <- cbind(one_step_c, rowMeans(abs((results$chicago_one_step)[,keep_ind])))
      }
    }
    write.csv(one_step_n, paste("result/nyc_one_step_MAE",sub,".csv", sep=""))
    write.csv(one_step_c, paste("result/chicago_one_step_MAE",sub,".csv", sep=""))
  }
}

varname1<-'nyc'
varname2<-'chicago'

for(sub in subs){
  one_step_c <- c()
  one_step_n <- c()
  for(model_ind in 1:8){
    varname <- model_name[model_ind]
    # Load model
    load(paste(varname, paste("BSTS_result",sub,"_new.Rdata", sep=""), sep="_"))
    # Calculate train error
    for(h in 0:2){
      keep_ind <- ((142*h+1):((h+1)*142))[nyc.keep_ind[(142*h+1):((h+1)*142)]]
      if(length(one_step_n) == 0){
        one_step_n <- rowMeans(abs((results$nyc_one_step*nyc.mean)[,keep_ind])^2)
      }else{
        one_step_n <- cbind(one_step_n, rowMeans(abs((results$nyc_one_step*nyc.mean)[,keep_ind])^2))
      }
      keep_ind <- ((154*h+1):((h+1)*154))[chicago.keep_ind[(154*h+1):((h+1)*154)]]
      if(length(one_step_c) == 0){
        one_step_c <- rowMeans(abs((results$chicago_one_step*chicago.mean)[,keep_ind])^2)
      }else{
        one_step_c <- cbind(one_step_c, rowMeans(abs((results$chicago_one_step*chicago.mean)[,keep_ind])^2))
      }
    }
    write.csv(one_step_n, paste("result/nyc_one_step_MSE",sub,".csv", sep=""))
    write.csv(one_step_c, paste("result/chicago_one_step_MSE",sub,".csv", sep=""))
  }
}

for(sub in subs){
  one_step_c <- c()
  one_step_n <- c()
  for(model_ind in 1:8){
    varname <- model_name[model_ind]
    # Load model
    load(paste(varname, paste("BSTS_result",sub,"_new.Rdata", sep=""), sep="_"))
    # Calculate train error
    for(h in 0:2){
      keep_ind <- ((142*h+1):((h+1)*142))[nyc.keep_ind[(142*h+1):((h+1)*142)]]
      if(length(one_step_n) == 0){
        one_step_n <- rowMeans(abs((results$nyc_one_step*nyc.mean/(nyc.ridership+100))[,keep_ind]))
      }else{
        one_step_n <- cbind(one_step_n, rowMeans(abs((results$nyc_one_step*nyc.mean/(nyc.ridership+100))[,keep_ind])))
      }
      keep_ind <- ((154*h+1):((h+1)*154))[chicago.keep_ind[(154*h+1):((h+1)*154)]]
      if(length(one_step_c) == 0){
        one_step_c <- rowMeans(abs((results$chicago_one_step*chicago.mean/(chicago.ridership+100))[,keep_ind]))
      }else{
        one_step_c <- cbind(one_step_c, rowMeans(abs((results$chicago_one_step*chicago.mean/(chicago.ridership+100))[,keep_ind])))
      }
    }
    write.csv(one_step_n, paste("result/nyc_one_step_MAPE",sub,".csv", sep=""))
    write.csv(one_step_c, paste("result/chicago_one_step_MAPE",sub,".csv", sep=""))
  }
}


varname1<-'nyc'
varname2<-'chicago'

for(sub in subs){
  one_step_c <- c()
  one_step_n <- c()
  for(model_ind in 1:8){
    varname <- model_name[model_ind]
    # Load model
    load(paste(varname, paste("BSTS_result",sub,"_new.Rdata", sep=""), sep="_"))
    # Calculate train error
    for(h in 0:2){
      keep_ind <- ((142*h+1):((h+1)*142))[nyc.keep_ind[(142*h+1):((h+1)*142)]]
      if(length(one_step_n) == 0){
        one_step_n <- mean(diag(cor(nyc.ridership_, nyc.ridership_+results$nyc_one_step))[keep_ind])
      }else{
        one_step_n <- cbind(one_step_n, mean(diag(cor(nyc.ridership_, nyc.ridership_+results$nyc_one_step))[keep_ind]))
      }
      keep_ind <- ((154*h+1):((h+1)*154))[chicago.keep_ind[(154*h+1):((h+1)*154)]]
      if(length(one_step_c) == 0){
        one_step_c <- mean(diag(cor(chicago.ridership_, chicago.ridership_+results$chicago_one_step))[keep_ind])
      }else{
        one_step_c <- cbind(one_step_c, mean(diag(cor(chicago.ridership_, chicago.ridership_+results$chicago_one_step))[keep_ind]))
      }
    }
    write.csv(one_step_n, paste("result/nyc_one_step_Pearson",sub,".csv", sep=""))
    write.csv(one_step_c, paste("result/chicago_one_step_Pearson",sub,".csv", sep=""))
  }
}

dates <- as.Date(time(nyc_fhv[,1], date_shift_nyc))
PlotBstsComponents(model[['nyc_5']], time = dates)

