# STEP 1:
library(Rssa)

# # replace this with SSA, try different number and recovery quality
# for(day in c(0,7,365)){a
# 
# day<-1
# nyc.ssa<-ssa(nyc.ridership_, L = min(length(train_ind_nyc)-day+1,length(train_ind_nyc)), kind = "mssa")
# # # choose a proper number of signal
# h<-0
# fhv_ind<-((142*h+1):((h+1)*142))[nyc.keep_ind[(142*h+1):((h+1)*142)]]
# h<-1
# taxi_ind<-((142*h+1):((h+1)*142))[nyc.keep_ind[(142*h+1):((h+1)*142)]]
# h<-2
# bike_ind<-((142*h+1):((h+1)*142))[nyc.keep_ind[(142*h+1):((h+1)*142)]]
# MSE.fhv<-c(mean(nyc.ridership[,fhv_ind])^2)
# MSE.taxi<-c(mean(nyc.ridership[,taxi_ind])^2)
# MSE.bike<-c(mean(nyc.ridership[,bike_ind])^2)
# MAXAE.fhv<-c(max(nyc.ridership[,fhv_ind]))
# MAXAE.taxi<-c(max(nyc.ridership[,taxi_ind]))
# MAXAE.bike<-c(max(nyc.ridership[,bike_ind]))
# MAE.fhv <- c(mean(nyc.ridership[,fhv_ind]))
# MAE.taxi <- c(mean(nyc.ridership[,taxi_ind]))
# MAE.bike <- c(mean(nyc.ridership[,bike_ind]))
# MAPE.fhv <- c(1)
# MAPE.taxi <- c(1)
# MAPE.bike <- c(1)
# 
# for(k in 1:50){
#   h <- 0
#   recon <- sweep(Reduce('+', reconstruct(nyc.ssa, groups = list(1:k))),2,nyc.mean,FUN='*')
#   keep_ind <- ((142*h+1):((h+1)*142))[nyc.keep_ind[(142*h+1):((h+1)*142)]]
#   MSE.fhv  <- c(MSE.fhv,mean((recon[,keep_ind] -
#                                 nyc.ridership[,keep_ind])^2))
#   MAXAE.fhv  <- c(MAXAE.fhv,max(abs(recon[,keep_ind] -
#                                       nyc.ridership[,keep_ind])))
#   MAE.fhv <- c(MAE.fhv,mean(abs((recon[,keep_ind] -
#                                    nyc.ridership[,keep_ind])[,nyc.keep_ind[keep_ind]])))
#   MAPE.fhv <- c(MAPE.fhv,mean(abs(recon[,keep_ind] -
#                                     nyc.ridership[,keep_ind])[,nyc.keep_ind[keep_ind]]/
#                                 (nyc.ridership[,keep_ind][,nyc.keep_ind[keep_ind]]+10)))
#   h <- 1
#   keep_ind <- ((142*h+1):((h+1)*142))[nyc.keep_ind[(142*h+1):((h+1)*142)]]
#   MSE.taxi  <- c(MSE.taxi,mean((recon[,keep_ind] -
#                                   nyc.ridership[,keep_ind])^2))
#   MAXAE.taxi  <- c(MAXAE.taxi,max(abs(recon[,keep_ind] -
#                                         nyc.ridership[,keep_ind])))
#   MAE.taxi<- c(MAE.taxi,mean(abs((recon[,keep_ind] -
#                                     nyc.ridership[,keep_ind])[,nyc.keep_ind[keep_ind]])))
#   MAPE.taxi <- c(MAPE.taxi,mean(abs(recon[,keep_ind] -
#                                       nyc.ridership[,keep_ind])[,nyc.keep_ind[keep_ind]]/
#                                   (nyc.ridership[,keep_ind][,nyc.keep_ind[keep_ind]]+10)))
#   h <- 2
#   keep_ind <- ((142*h+1):((h+1)*142))[nyc.keep_ind[(142*h+1):((h+1)*142)]]
#   MSE.bike  <- c(MSE.bike,mean((recon[,keep_ind] -
#                                   nyc.ridership[,keep_ind])^2))
#   MAXAE.bike  <- c(MAXAE.bike,max(abs(recon[,keep_ind] -
#                                         nyc.ridership[,keep_ind])))
#   MAE.bike <- c(MAE.bike,mean(abs((recon[,keep_ind] -
#                                      nyc.ridership[,keep_ind])[,nyc.keep_ind[keep_ind]])))
#   MAPE.bike <- c(MAPE.bike,mean(abs(recon[,keep_ind] -
#                                       nyc.ridership[,keep_ind])[,nyc.keep_ind[keep_ind]]/
#                                   (nyc.ridership[,keep_ind][,nyc.keep_ind[keep_ind]]+10)))
# }
# #
# df <- data.frame(k=0:50, MSE.fhv,MSE.taxi,MSE.bike,MAXAE.fhv,MAXAE.taxi,MAXAE.bike,MAE.fhv,MAE.taxi,MAE.bike,
#                  MAPE.fhv,MAPE.taxi,MAPE.bike)
# write.csv(df,paste("result/nyc_ssa_",day,".csv", sep=""))
# write.csv(nyc.ssa$U, paste("result/nyc_signal_",day,".csv", sep=""))
# # # #
# chicago.ssa<-ssa(chicago.ridership_, L = min(length(train_ind_chicago)-day,length(train_ind_chicago)), kind = "mssa")
# # # choose a proper number of signal
# h<-0
# fhv_ind<-((154*h+1):((h+1)*154))[chicago.keep_ind[(154*h+1):((h+1)*154)]]
# h<-1
# taxi_ind<-((154*h+1):((h+1)*154))[chicago.keep_ind[(154*h+1):((h+1)*154)]]
# h<-2
# bike_ind<-((154*h+1):((h+1)*154))[chicago.keep_ind[(154*h+1):((h+1)*154)]]
# 
# MSE.fhv<-c(mean(chicago.ridership[,fhv_ind])^2)
# MSE.taxi<-c(mean(chicago.ridership[,taxi_ind])^2)
# MSE.bike<-c(mean(chicago.ridership[,bike_ind])^2)
# MAXAE.fhv<-c(max(chicago.ridership[,fhv_ind]))
# MAXAE.taxi<-c(max(chicago.ridership[,taxi_ind]))
# MAXAE.bike<-c(max(chicago.ridership[,bike_ind]))
# MAE.fhv <- c(mean(chicago.ridership[,fhv_ind]))
# MAE.taxi <- c(mean(chicago.ridership[,taxi_ind]))
# MAE.bike <- c(mean(chicago.ridership[,bike_ind]))
# MAPE.fhv <- c(1)
# MAPE.taxi <- c(1)
# MAPE.bike <- c(1)
# 
# # #
# for(k in 1:50){
#   h <- 0
#   recon <- sweep(Reduce('+', reconstruct(chicago.ssa, groups = list(1:k))),2,chicago.mean,FUN='*')
#   keep_ind <- ((154*h+1):((h+1)*154))[chicago.keep_ind[(154*h+1):((h+1)*154)]]
#   MSE.fhv <- c(MSE.fhv,mean(((recon[,keep_ind] -
#                                 chicago.ridership[,keep_ind]))^2))
# 
#   MAXAE.fhv  <- c(MAXAE.fhv,max(abs(recon[,keep_ind] -
#                                       chicago.ridership[,keep_ind])))
#   MAE.fhv <- c(MAE.fhv,mean(abs((recon[,keep_ind] -
#                                    chicago.ridership[,keep_ind]))))
#   MAPE.fhv <- c(MAPE.fhv,mean(abs(recon[,keep_ind] -
#                                     chicago.ridership[,keep_ind])/
#                                 (chicago.ridership[,keep_ind]+10)))
#   h <- 1
#   keep_ind <- ((154*h+1):((h+1)*154))[chicago.keep_ind[(154*h+1):((h+1)*154)]]
#   MSE.taxi<- c(MSE.taxi,mean(((recon[,keep_ind] -
#                                  chicago.ridership[,keep_ind]))^2))
# 
#   MAXAE.taxi  <- c(MAXAE.taxi,max(abs(recon[,keep_ind] -
#                                         chicago.ridership[,keep_ind])))
#   MAE.taxi<- c(MAE.taxi,mean(abs((recon[,keep_ind] -
#                                     chicago.ridership[,keep_ind]))))
#   MAPE.taxi <- c(MAPE.taxi,mean(abs(recon[,keep_ind] -
#                                       chicago.ridership[,keep_ind])/
#                                   (chicago.ridership[,keep_ind]+10)))
#   h <- 2
#   keep_ind <- ((154*h+1):((h+1)*154))[chicago.keep_ind[(154*h+1):((h+1)*154)]]
#   MSE.bike <- c(MSE.bike,mean(((recon[,keep_ind] -
#                                   chicago.ridership[,keep_ind])))^2)
#   MAXAE.bike  <- c(MAXAE.bike,max(abs(recon[,keep_ind] -
#                                         chicago.ridership[,keep_ind])))
#   MAE.bike <- c(MAE.bike,mean(abs((recon[,keep_ind] -
#                                      chicago.ridership[,keep_ind]))))
#   MAPE.bike <- c(MAPE.bike,mean(abs(recon[,keep_ind] -
#                                       chicago.ridership[,keep_ind])/
#                                   (chicago.ridership[,keep_ind]+10)))
# 
# }
# # #
# df <- data.frame(k=0:50, MSE.fhv,MSE.taxi,MSE.bike, MAXAE.fhv,MAXAE.taxi,MAXAE.bike,MAE.fhv,MAE.taxi,MAE.bike,
#                  MAPE.fhv,MAPE.taxi,MAPE.bike)
# write.csv(df,paste("result/chicago_ssa_",day,".csv", sep=""))
# write.csv(chicago.ssa$U, paste("result/chicago_signal_",day,".csv", sep=""))

# Go SSA with 1 days delay == SVD
nyc.ssa2<-ssa(nyc.ridership_, L = min(length(train_ind_nyc),length(train_ind_nyc)), kind = "mssa")
nyc.k = 10
# grouping the signal together
chicago.ssa2<-ssa(chicago.ridership_, L = min(length(train_ind_chicago),length(train_ind_chicago)), kind = "mssa")
chicago.k = 10

write.csv(nyc.ridership_, "result/nyc_true.csv")
write.csv(chicago.ridership_, "result/chicago_true.csv")

write.csv(Reduce('+', reconstruct(nyc.ssa2, groups = list(1:nyc.k))), "result/nyc_reconstruct.csv")
write.csv(Reduce('+', reconstruct(chicago.ssa2, groups = list(1:chicago.k))), "result/chicago_reconstruct.csv")

# Our targets are nyc.ssa$U[,1:nyc.k], chicago.ssa$U[,1:chicago.k]

# grouping the signal together
# chicago.lst <- grouping.auto(chicago.ssa, grouping.method = "wcor", 
#                          groups = list(1:chicago.k), nclust = 3)
# w <- wcor(chicago.ssa, groups = chicago.lst)
# plot(w)

#print(nyc.list$`3`)
#plot(1:365,nyc.ssa$U[,nyc.lst$`3`][,4])
# r <- reconstruct(nyc.ssa, groups = nyc.lst$lst)

# try component generated from training period
# chicago.components <- svd(chicago.ridership)
# chicago.k <- 5
# chicago.project <- chicago.components$v[,1:chicago.k] %*% diag(1/chicago.components$d[1:chicago.k])
# chicago.recover <- diag(chicago.components$d[1:chicago.k]) %*% t(chicago.components$v[,1:chicago.k])
# chicago.u <- chicago.ridership %*% chicago.project
# 
# nyc.components <- svd(nyc.ridership)
# nyc.k <- 30
# nyc.project <- nyc.components$v[,1:nyc.k] %*% diag(1/nyc.components$d[1:nyc.k])
# nyc.recover <- diag(nyc.components$d[1:nyc.k]) %*% t(nyc.components$v[,1:nyc.k])
# nyc.u <- nyc.ridership %*% nyc.project

# Two things: precision of different k by MAPE, and visualize the first three dimension
# nyc.MAPE <- rep(0,30)
# for(i in 2:30){
#   tmp <- nyc.u[,1:i] %*% nyc.recover[1:i,]
#   nyc.MAPE[i] <- mean(abs(tmp-nyc.ridership)/(1+nyc.ridership))
# }

# library("plotly")
# library("RColorBrewer")
# fig <- plot_ly(x= nyc.u[305:1096,1], y = nyc.u[305:1096,2], z = nyc.u[305:1096,3], type = "scatter3d", mode = "lines",
#                line = list(width = 6, color =  colorRampPalette(brewer.pal(10,"Spectral"))(dim(nyc.u)[1]), 
#                            reverscale = FALSE)) 
# fig <- fig %>% layout(scene = list(xaxis=list(title="p1"),
#                                    yaxis=list(title="p2"),
#                                    zaxis=list(title="p3")))
# 
# 
# fig <- plot_ly(x= chicago.u[305:1247,1], y = chicago.u[305:1247,2], z = chicago.u[305:1247,3], type = "scatter3d", mode = "lines",
#                line = list(width = 6, color =  colorRampPalette(brewer.pal(10,"Spectral"))(dim(chicago.u)[1]), reverscale = FALSE))
# fig <- fig %>% layout(scene = list(xaxis=list(title="p1"),
#                                    yaxis=list(title="p2"),
#                                    zaxis=list(title="p3")))