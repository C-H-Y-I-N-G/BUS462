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

#Load data from Github
circuits <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/circuits.csv?token=ARGCUJVFLHKMHJWC6XIH7BDALPFH2")
constructor_results <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/constructor_results.csv?token=ARGCUJVTEA7CUOKHQGVEMSTALPFUQ")
constructor_standings <-fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/constructor_standings.csv?token=ARGCUJVL4UAEH6G4MG6FX5DALPGEE")
constructors <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/constructors.csv?token=ARGCUJRM5BXALZ4LNR3HBZTALPGOW")
driver_standings <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/driver_standings.csv?token=ARGCUJUEKGGVYUIJUPBIEF3ALPGRS")
drivers <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/drivers.csv?token=ARGCUJVAAFN42GEWVT53LTDALPGU2")
lap_times <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/lap_times.csv?token=ARGCUJRBHFWPLGAR6JIU65TALPGZ4")
pit_stops <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/pit_stops.csv?token=ARGCUJSONNTEXU74NYPGU3DALPG7Q")
qualifying <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/qualifying.csv?token=ARGCUJX6ZEK3OU273Z62EETALPHFA")
races <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/races.csv?token=ARGCUJTJIFZSYPYKCUAZJ2TALPHJG")
results <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/results.csv?token=ARGCUJVOFVMB7TWWVUIK3DDALPHNC")
seasons <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/seasons.csv?token=ARGCUJRUCR5MDWODLISRETLALPHYI")
status <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/status.csv?token=ARGCUJSK3IMT54XZMKATKG3ALPH4M")

head(results)

dt <- merge(lap_times,drivers,by="driverId") #merge 2 data frame
dt <- subset(dt, select = -c(url)) #drop url column

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

dt[duplicated(dt)]#check duplication
dt[!duplicated(dt)]#remove duplication

head(dt)
<<<<<<< Updated upstream
=======
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

#summary stats of dataset
summary(dt)
stat.desc(dt)
stargazer(dt,type="text",summary.stat = c("min", "p25", "median","mean", "p75", "max","sd")) #stargazer best for visual


#split data into points (>=10) and no points(<10) positions
dt_points <- dt[dt$finishing_position<=10]
dt_nopoints <- dt[dt$finishing_position>10]

#summary stats of points
stargazer(dt_points,type="text",summary.stat = c("min", "p25", "median","mean", "p75", "max","sd"))

#summary stats of no points
stargazer(dt_nopoints,type="text",summary.stat = c("min", "p25", "median","mean", "p75", "max","sd"))

#notable differences (CK wants us to include sd as well as mean when discussing summary)

>>>>>>> Stashed changes

View(dt) #we will need to convert /N race position to 0, also convert for other tables
