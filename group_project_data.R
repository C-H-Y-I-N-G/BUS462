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

#load data
lap_times <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/lap_times.csv?token=ARGCUJRBHFWPLGAR6JIU65TALPGZ4")
pit_stops <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/pit_stops.csv?token=ARGCUJSONNTEXU74NYPGU3DALPG7Q")
qualifying <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/qualifying.csv?token=ARGCUJX6ZEK3OU273Z62EETALPHFA")
constructor_results <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/constructor_results.csv?token=ARGCUJVTEA7CUOKHQGVEMSTALPFUQ")
constructor_standings <-fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/constructor_standings.csv?token=ARGCUJVL4UAEH6G4MG6FX5DALPGEE")
driver_standings <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/driver_standings.csv?token=ARGCUJUEKGGVYUIJUPBIEF3ALPGRS")
races <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/races.csv?token=ARGCUJTJIFZSYPYKCUAZJ2TALPHJG")
results <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/results.csv?token=ARGCUJVOFVMB7TWWVUIK3DDALPHNC")
status <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/status.csv?token=ARGCUJSK3IMT54XZMKATKG3ALPH4M")
# races table replace seasons table
# driver standings table replace drivers table
# constructor results and constructor standings replaces constructors table

# organize races and results tables for merging
races <- subset(races, select = -c(round,name,date,time,url))#drop columns
results <- subset(results, select = -c(positionText,positionOrder))
names(results)[names(results) == "position"] <- "race_position"
names(results)[names(results) == "points"] <- "race_points"
dt <- merge(races,results,by="raceId") #merge 2 data frame

# merge status table
dt <- merge(dt,status,by="statusId")

# organize driver standings tables for merging
driver_standings <- subset(driver_standings, select = -c(positionText))
names(driver_standings)[names(driver_standings) == "position"] <- "season_position"
names(driver_standings)[names(driver_standings) == "points"] <- "season_points"
dt <- merge(dt,driver_standings,by=c("driverId","raceId"))

# organize constructor standings and constructor results tables for merging
constructor_standings <- subset(constructor_standings, select = -c(positionText))
names(constructor_standings)[names(constructor_standings) == "position"] <- "constructor_position"
names(constructor_standings)[names(constructor_standings) == "points"] <- "constructor_points"
names(constructor_standings)[names(constructor_standings) == "wins"] <- "constructor_wins"
names(constructor_results)[names(constructor_results) == "status"] <- "constructorResults_status"
names(constructor_results)[names(constructor_results) == "points"] <- "constructorResults_points"
dt <- merge(dt,constructor_standings, by=c("constructorId","raceId"))
dt <- merge(dt,constructor_results, by=c("constructorId","raceId"))

# organize qualifying tables for merging
qualifying <- subset(qualifying, select = -c(number))
names(qualifying)[names(qualifying) == "position"] <- "qualifying_position"
dt <- merge(dt,qualifying,by=c("driverId","raceId","constructorId"))

# organize pit stop tables for merging
pit_stops <- subset(pit_stops, select = -c(duration))#duration has same number as milliseconds
names(pit_stops)[names(pit_stops) == "stop"] <- "pit_stop"
names(pit_stops)[names(pit_stops) == "lap"] <- "pit_stops_lap"
names(pit_stops)[names(pit_stops) == "time"] <- "pit_stops_time"
names(pit_stops)[names(pit_stops) == "milliseconds"] <- "pit_stops_milliseconds"
dt <- merge(dt,pit_stops, by=c("driverId","raceId"))

# organize lap times tables for merging
names(lap_times)[names(lap_times) == "position"] <- "lap_times_position"
names(lap_times)[names(lap_times) == "lap"] <- "lap_times_lap"
names(lap_times)[names(lap_times) == "time"] <- "lap_times_time"
names(lap_times)[names(lap_times) == "milliseconds"] <- "lap_times_milliseconds"
dt <- merge(dt,lap_times, by=c("driverId","raceId"))

# replace all the \N race position to 0
dt$race_position <- as.integer(dt$race_position)
dt$race_position[is.na(dt$race_position)] = 0

dt[duplicated(dt)]#check duplication
dt[!duplicated(dt)]#remove duplication

#look at data
head(dt)
View(dt)

#check data type of columns
str(dt)

#create function that checks if any NAs are in a column
check_na <- function(my_col){
  any(is.na(my_col))
}

#apply function to each column in the set
apply(dt, 2, check_na)

#no NAs in dataset  after conversions

#NEXT STEPS
#drop times, keep milliseconds
#cut to 2015 onwards




