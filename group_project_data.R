############################################################
# BUS 462 | Spring 2021 
# Group Project 
# Team Omega
# I pledge on my honor that I have neither received nor given unauthorized assistance on this deliverable.
############################################################

#### PREAMBLE : ## Clearing buffers ####
cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all
gc()

# libraries

require(data.table)
require(stargazer)
require(ggplot2)
require(PerformanceAnalytics)
require(sqldf)

#### LOAD DATA
laptimes <- fread("C:/Users/chloe/Desktop/BUS 462/group project/f1db_csv/lap_times.csv")
drivers <- fread("C:/Users/chloe/Desktop/BUS 462/group project/f1db_csv/drivers.csv")

dt <- merge(laptimes,drivers,by="driverId") #merge 2 data frame
dt <- subset(dt, select = -c(url)) #drop url column
