library(readr)

# clear all data
rm(list = ls())

nyc_parameters <- read_csv("nyc_parameters.csv", col_names = TRUE)

chicago_parameters <- read_csv("chicago_parameters.csv", col_names = TRUE)

nyc_parameters2 <- read_csv("nyc_parameters2.csv", col_names = TRUE)

chicago_parameters2 <- read_csv("chicago_parameters2.csv", col_names = TRUE)

df <- read_csv("data/chicago_pickup.csv", col_names = FALSE)
chicago_pickup <- matrix(unlist(df), ncol = length(df[0,]), byrow=FALSE)

df <- read_csv("data/chicago_dropoff.csv", col_names = FALSE)
chicago_dropoff <- matrix(unlist(df), ncol = length(df[0,]), byrow=FALSE)

df <- read_csv("data/chicago_daily_cases.csv", col_names = FALSE)
chicago_case_agg <- zoo::rollmean(matrix(unlist(df), ncol = length(df[0,]), byrow=FALSE),k=7,fill=0, align='right')

df <- read_csv("data/chicago_shocks.csv", col_names = FALSE)
chicago_shocks <- matrix(unlist(df), ncol = length(df[0,]), byrow=FALSE)

df <- read_csv("data/chicago_total_doses.csv", col_names = FALSE)
chicago_vacc_agg <- matrix(unlist(df), ncol = length(df[0,]), byrow=FALSE)

df <- read_csv("data/nyc_pickup.csv", col_names = FALSE)
nyc_pickup <- matrix(unlist(df), ncol = length(df[0,]), byrow=FALSE)

df <- read_csv("data/nyc_dropoff.csv", col_names = FALSE)
nyc_dropoff <- matrix(unlist(df), ncol = length(df[0,]), byrow=FALSE)

df <- read_csv("data/nyc_daily_cases.csv", col_names = FALSE)
nyc_case_agg <- zoo::rollmean(matrix(unlist(df), ncol = length(df[0,]), byrow=FALSE),k=7,fill=0, align='right')

df <- read_csv("data/nyc_total_doses.csv", col_names = FALSE)
nyc_vacc_agg <- matrix(unlist(df), ncol = length(df[0,]), byrow=FALSE)

df <- read_csv("data/nyc_shocks.csv", col_names = FALSE)
nyc_shocks <- matrix(unlist(df), ncol = length(df[0,]), byrow=FALSE)

chicago_taxi <- cbind(chicago_pickup[,2:(234/3)], chicago_dropoff[,2:(234/3)])
chicago_fhv <- cbind(chicago_pickup[,(234/3+2):(2*234/3)], chicago_dropoff[,(234/3+2):(2*234/3)])
chicago_bike <- cbind(chicago_pickup[,(2*234/3+2):(3*234/3)], chicago_dropoff[,(2*234/3+2):(3*234/3)])

nyc_taxi <- cbind(nyc_pickup[,1:(213/3)], nyc_dropoff[,1:(213/3)])
nyc_fhv <- cbind(nyc_pickup[,(213/3+1):(2*213/3)], nyc_dropoff[,(213/3+1):(2*213/3)])
nyc_bike <- cbind(nyc_pickup[,(2*213/3+1):(3*213/3)], nyc_dropoff[,(2*213/3+1):(3*213/3)])

chicago_temp_min <- read.csv("data/chicago_tmp_min.csv",  sep = ",", header = F, stringsAsFactors=F)
chicago_temp_max <- read.csv("data/chicago_tmp_max.csv",  sep = ",", header = F, stringsAsFactors=F)
chicago_rain <- read.csv("data/chicago_rain.csv",  sep = ",", header = F, stringsAsFactors=F)

nyc_temp_min <- read.csv("data/nyc_tmp_min.csv",  sep = ",", header = F, stringsAsFactors=F)
nyc_temp_max <- read.csv("data/nyc_tmp_max.csv",  sep = ",", header = F, stringsAsFactors=F)
nyc_rain <- read.csv("data/nyc_rain.csv",  sep = ",", header = F, stringsAsFactors=F)

