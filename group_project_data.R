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


#### LOAD DATA locally
#laptimes <- fread("C:/Users/chloe/Desktop/BUS 462/group project/f1db_csv/lap_times.csv")
#drivers <- fread("C:/Users/chloe/Desktop/BUS 462/group project/f1db_csv/drivers.csv")

#Load data from Github
circuits <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/circuits.csv?token=ARGCUJVFLHKMHJWC6XIH7BDALPFH2")
constructor_results <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/constructor_results.csv?token=ARGCUJVTEA7CUOKHQGVEMSTALPFUQ")
constructor_standings <-fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/constructor_standings.csv?token=ARGCUJVL4UAEH6G4MG6FX5DALPGEE")
constructors <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/constructors.csv?token=ARGCUJRM5BXALZ4LNR3HBZTALPGOW")
driver_standings <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/driver_standings.csv?token=ARGCUJUEKGGVYUIJUPBIEF3ALPGRS")
drivers <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/drivers.csv?token=ARGCUJVAAFN42GEWVT53LTDALPGU2")
lap_times <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/lap_times.csv?token=ARGCUJRBHFWPLGAR6JIU65TALPGZ4")
pit_stops <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/pit_stops.csv?token=ARGCUJSONNTEXU74NYPGU3DALPG7Q")
qualifying <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/qualifying.csv?token=ARGCUJX6ZEK3OU273Z62EETALPHFA")
races <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/races.csv?token=ARGCUJTJIFZSYPYKCUAZJ2TALPHJG")
results <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/results.csv?token=ARGCUJVOFVMB7TWWVUIK3DDALPHNC")
seasons <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/seasons.csv?token=ARGCUJRUCR5MDWODLISRETLALPHYI")
status <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/status.csv?token=ARGCUJSK3IMT54XZMKATKG3ALPH4M")

head(results)

dt <- merge(lap_times,drivers,by="driverId") #merge 2 data frame
dt <- subset(dt, select = -c(url)) #drop url column
