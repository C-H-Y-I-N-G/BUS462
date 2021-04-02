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
require(dyplr)
require(pastecs)

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
results <- subset(results, select = -c(positionText,positionOrder,time,number))
names(results)[names(results) == "position"] <- "finishing_position"
names(results)[names(results) == "points"] <- "finishing_points"
names(results)[names(results) == "milliseconds"] <- "finishing_milliseconds"
names(results)[names(results) == "laps"] <- "lap_finished"
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
#include position and qs for models 
#NOTE: changed q1-3 times to milliseconds in Excel
qualifying <- subset(qualifying, select = -c(number,q1,q2,q3,q1_formatted,q2_formatted,q3_formatted))
names(qualifying)[names(qualifying) == "position"] <- "qualifying_position"
qualifying$qmean <- ((qualifying$q1_milliseconds+qualifying$q2_milliseconds+qualifying$q3_milliseconds)/3)
dt <- merge(dt,qualifying,by=c("driverId","raceId","constructorId"))

# organize pit stop tables for merging
pit_stops <- subset(pit_stops, select = -c(duration,time))#duration has same number as milliseconds
names(pit_stops)[names(pit_stops) == "stop"] <- "pit_stop"
names(pit_stops)[names(pit_stops) == "lap"] <- "pit_stops_lap"
names(pit_stops)[names(pit_stops) == "milliseconds"] <- "pit_stops_milliseconds"
dt <- merge(dt,pit_stops, by=c("driverId","raceId"))

# organize lap times tables for merging
lap_times <- subset(lap_times, select = -c(time))
names(lap_times)[names(lap_times) == "position"] <- "lap_times_position" #could cut this
names(lap_times)[names(lap_times) == "lap"] <- "lap_times_lap"
names(lap_times)[names(lap_times) == "milliseconds"] <- "lap_times_milliseconds"
dt <- merge(dt,lap_times, by=c("driverId","raceId"))

# replace all the \N race position to 0
dt$finishing_position <- as.integer(dt$finishing_position)
dt$finishing_position[is.na(dt$finishing_position)] = 23 #can change to 50

dt[duplicated(dt)]#check duplication
dt[!duplicated(dt)]#remove duplication

dt <- subset(dt, year > 2014)#shrink the table to year 2015-2020

#look at data
head(dt)
View(dt)

#create function that checks if any NAs are in a column
check_na <- function(my_col){
  any(is.na(my_col))
}

#apply function to each column in the set
apply(dt, 2, check_na)

#no NAs in dataset  after conversions

#check data type of columns
str(dt)

#convert milliseconds from chr to int 
dt$finishing_milliseconds <- as.integer(dt$finishing_milliseconds)
dt$finishing_milliseconds[is.na(dt$finishing_milliseconds)] = 20000000 #setting dummy value to punish dnf

#convert rank to int
dt$rank <- as.integer(dt$rank)

#convert fastest lap speed
dt$fastestLapSpeed <- as.numeric(dt$fastestLapSpeed)
dt$fastestLapSpeed[is.na(dt$fastestLapSpeed)] = 0 #for NAs set to 0

#BEGINNING OF PRELIMINARY ANALYSIS 
#NOTE: add comparative histograms and boxplots between groups (both distributions on one)

#summary stats of dataset
summary(dt)
stat.desc(dt)
stargazer(dt,type="text",omit=c("driverId","raceId","constructorId","resultId","statusId","year","circuitId","qualifyId"),summary.stat = c("min", "p25", "median","mean", "p75", "max","sd")) #stargazer best for visual

#start of correlation chart
dt_numeric <-

#COMPARING POINTS VS NO POINTS POSITIONS
#split data into points (>=10) and no points(<10) positions
dt_points <- dt[dt$finishing_position<=10]
dt_nopoints <- dt[dt$finishing_position>10]

#summary stats of points
stargazer(dt_points,type="text",omit=c("driverId","raceId","constructorId","resultId","statusId","year","circuitId","qualifyId"),summary.stat = c("min", "p25", "median","mean", "p75", "max","sd"))

#summary stats of no points
stargazer(dt_nopoints,type="text",omit=c("driverId","raceId","constructorId","resultId","statusId","year","circuitId","qualifyId"),summary.stat = c("min", "p25", "median","mean", "p75", "max","sd"))

#notable differences (CK wants us to include sd as well as mean when discussing summary)
#points finishers had higher median and mean fastest lap speed
#points finishers had MUCH lower(better) qualifying position, determines grid and is likely important
#no points finishers surprisingly had lower mean and median pit stop milliseconds
#lap time median and means surprisingly close between the two groups 
#

#graphing variable differences



#COMPARING PODIUM VS THE REST
dt_podium <- dt[dt$finishing_position<=3]
dt_nopodium <- dt[dt$finishing_position>3]

#summary stats of points
stargazer(dt_podium,type="text",omit=c("driverId","raceId","constructorId","resultId","statusId","year","circuitId","qualifyId"),summary.stat = c("min", "p25", "median","mean", "p75", "max","sd"))

#summary stats of no points
stargazer(dt_nopodium,type="text",omit=c("driverId","raceId","constructorId","resultId","statusId","year","circuitId","qualifyId"),summary.stat = c("min", "p25", "median","mean", "p75", "max","sd"))

#notable differences
#even bigger in qualifying position, mean around 3 for podium finishers, qualifying strong determinant
#pit times again lower in the worse performing group, worth looking into, maybe something to do with
#more thorough team
#lap times again slightly lower for higher performing group 


#MODELS
#control for circuit ID and year

