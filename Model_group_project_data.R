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
require(dyplr)
require(pastecs)
require(pscl)
require(nnet)
require(ggcorrplot)
require(jtools)
require(ggstance)
require(broom.mixed)
require(sqldf)
require(tidyr)

#load data
lap_times <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/lap_times.csv")
pit_stops <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/pit_stops.csv")
qualifying <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/qualifying.csv")
constructor_results <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/constructor_results.csv")
constructor_standings <-fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/constructor_standings.csv")
driver_standings <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/driver_standings.csv")
races <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/races.csv")
results <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/results.csv")
status <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/status.csv")
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
constructor_results <- subset(constructor_results, select = -c(status))
names(constructor_results)[names(constructor_results) == "points"] <- "constructorResults_points"
dt <- merge(dt,constructor_standings, by=c("constructorId","raceId"))
dt <- merge(dt,constructor_results, by=c("constructorId","raceId"))

# organize qualifying tables for merging
#include position and qs for models 
#NOTE: changed q1-3 times to milliseconds in Excel
qualifying <- subset(qualifying, select = -c(number,q1,q2,q3,q1_formatted,q2_formatted,q3_formatted))
names(qualifying)[names(qualifying) == "position"] <- "qualifying_position"
#code to omit nas without losing all qualifying positions that did not make it to round 3
#first convert to int
qualifying$q1_milliseconds <- as.integer(qualifying$q1_milliseconds)
qualifying$q2_milliseconds <- as.integer(qualifying$q2_milliseconds)
qualifying$q3_milliseconds <- as.integer(qualifying$q3_milliseconds)
#convert na to 0 for qmean calc
df %>% dplyr::mutate(x = replace_na(x, 0))
#calculate qmean
qualifying$qmean <- (qualifying$q1_milliseconds+qualifying$q2_milliseconds+qualifying$q3_milliseconds)/3
#qualifying<- subset(dt, select = -c(q1_milliseconds,q2_milliseconds,q3_milliseconds))
qualifying <- na.omit(qualifying)
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


dt[duplicated(dt)]#check duplication
dt[!duplicated(dt)]#remove duplication

dt <- subset(dt, year > 2014)#shrink the table to year 2015-2020

#DATA MERGING COMPLETE

#look at data
head(dt)
#View(dt)

#check data type of columns
str(dt)

#convert milliseconds from chr to num
dt$finishing_milliseconds <- as.numeric(dt$finishing_milliseconds) 

#convert columns to int
dt$rank <- as.integer(dt$rank)
#dt$q1_milliseconds <- as.integer(dt$q1_milliseconds)
#dt$q2_milliseconds <- as.integer(dt$q2_milliseconds)
#dt$q3_milliseconds <- as.integer(dt$q3_milliseconds)
dt$finishing_position <- as.integer(dt$finishing_position) #convert  to integer

#create qmean to summarize quaifying times
#dt$qmean <- (dt$q1_milliseconds+dt$q2_milliseconds+dt$q3_milliseconds)/3
#dt <- subset(dt, select = -c(q1_milliseconds,q2_milliseconds,q3_milliseconds))

#convert fastest lap speed
dt$fastestLapSpeed <- as.numeric(dt$fastestLapSpeed)

#Create new columns with seconds instead of milliseconds for easier interpretation of key variables
dt$pit_stops_seconds <- (dt$pit_stops_milliseconds)/1000
dt$lap_times_seconds <- (dt$lap_times_milliseconds)/1000
dt$finishing_seconds <- (dt$finishing_milliseconds)/1000

#create function that checks if any NAs are in a column
check_na <- function(my_col){
  any(is.na(my_col))
}

#apply function to each column in the set
apply(dt, 2, check_na)

#omit nas 
dt <- na.omit(dt)


#Create new variable for podium, will be primary dv
dt$podium <- ifelse(dt$finishing_position>3,0,1)


View(dt)#check completed table



#BEGINNING OF PRELIMINARY ANALYSIS 
#NOTE: add comparative histograms and boxplots between groups (both distributions on one)

#summary stats of dataset
stargazer(dt,type="text",omit=c("driverId","raceId","constructorId","resultId","statusId","year","circuitId","qualifyId"),summary.stat = c("min", "p25", "median","mean", "p75", "max","sd")) #stargazer best for visual


#library(writexl) #export to dt to excel
#write_xlsx(dt, "c:/Users/chloe/Desktop/dt.xlsx")
#write_xlsx(dt_numeric, "c:/Users/chloe/Desktop/dt_numeric.xlsx")


#COMPARING PODIUM VS THE REST
dt_podium <- dt[dt$finishing_position<=3]
dt_nopodium <- dt[dt$finishing_position>3]

#summary stats of podium
stargazer(dt_podium,type="text",omit=c("driverId","raceId","constructorId","resultId","statusId","year","circuitId","qualifyId"),summary.stat = c("min", "p25", "median","mean", "p75", "max","sd"))

#summary stats of no podium
stargazer(dt_nopodium,type="text",omit=c("driverId","raceId","constructorId","resultId","statusId","year","circuitId","qualifyId"),summary.stat = c("min", "p25", "median","mean", "p75", "max","sd"))

#notable differences
#even bigger in qualifying position, mean around 3 for podium finishers, qualifying strong determinant
#pit times again lower in the worse performing group, worth looking into, maybe something to do with
#more thorough team
#lap times again slightly lower for higher performing group 

#subset of data with variables of interest, based on prelim analysis
dt_model <- dt[,c("podium","qmean","lap_times_seconds","qualifying_position","pit_stops_seconds","fastestLapSpeed","finishing_seconds","circuitId","year")]

#summary stats of key variables
stargazer(dt_model,type="text")

#correlation chart for variables considered for model 
chart.Correlation(dt_model,histogram=TRUE, pch=19)

#ggplot correlations
dt_model_corr <- round(cor(dt_model),2)
ggcorrplot(dt_model_corr, hc.order = FALSE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of Variables Considered for Model", 
           ggtheme=theme_bw)

#visualizing highlights of prelim analysis

#qualifying position for podium and non-podium places, shows little to no overlap
#-> those in podium almost always qualify very high
ggplot(aes(y = qualifying_position, x = factor(podium)), data = dt) + geom_boxplot(fill="plum") + 
  labs(title="Box Plot",
       subtitle="Qualifying Positions for Podium and Non-Podium Placings",
       x="Podium Status (0=no, 1=yes)",
       y="Qualifying Position")

#pit times for podium and non-podium
#when outliers removed pit stop times between the two are extremely close
ggplot(aes(y = pit_stops_seconds, x = factor(podium)), data = dt) + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box Plot",
       subtitle="Pit Stop Seconds for Podium and Non-Podium Placings",
       x="Podium Status (0=no, 1=yes)",
       y="Pit Stop Seconds") + 
  scale_y_continuous(limits=c(0,50))

#Fastest lap speed, again very close, though podium skews higher with lower variance
#could indicate consistent performance important
ggplot(aes(y = fastestLapSpeed, x = factor(podium)), data = dt) + geom_boxplot(fill="plum") + 
  labs(title="Box Plot",
       subtitle="Fastest Lap Speeds for Podium and Non-Podium Placings",
       x="Podium Status (0=no, 1=yes)",
       y="Fastest Lap Speed")

#lap  times, lots of outliers, difficult to narrow down
ggplot(aes(y = lap_times_seconds, x = factor(podium)), data = dt) + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box Plot",
       subtitle="Lap Time Seconds for Podium and Non-Podium Placings",
       x="Podium Status (0=no, 1=yes)",
       y="Lap Time Seconds")  

#lap times gain but limiting scale, ignoring outliers
ggplot(aes(y = lap_times_seconds, x = factor(podium)), data = dt) + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box Plot",
       subtitle="Lap Time Seconds for Podium and Non-Podium Placings (Scaled)",
       x="Podium Status (0=no, 1=yes)",
       y="Lap Time Seconds") + 
  scale_y_continuous(limits=c(0,200)) 



#CREATE SUBSET TO DO alpine-SPECIFIC COMPARISON, IMPORTANT TO CHECK IF ANY MAJOR DIFFERENCES
dt_alpine = sqldf("SELECT * FROM dt WHERE constructorID = 4")

#qualifying position for podium and non-podium places, very similar result to dataset overall
ggplot(aes(y = qualifying_position, x = factor(podium)), data = dt_alpine) + geom_boxplot(fill="plum") + 
  labs(title="Box Plot ",
       subtitle="Qualifying Positions for Podium and Non-Podium Placings (Alpine)",
       x="Podium Status (0=no, 1=yes)",
       y="Qualifying Position")

#pit times for podium and non-podium
ggplot(aes(y = pit_stops_seconds, x = factor(podium)), data = dt_alpine) + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box Plot",
       subtitle="Pit Stop Seconds for Podium and Non-Podium Placings",
       x="Podium Status (0=no, 1=yes)",
       y="Pit Stop Seconds") + 
  scale_y_continuous(limits=c(0,50))

#again very close, though podium skews higher with lower variance
#could indicate consistent performance important
ggplot(aes(y = fastestLapSpeed, x = factor(podium)), data = dt_alpine) + geom_boxplot(fill="plum") + 
  labs(title="Box Plot",
       subtitle="Fastest Lap Speeds for Podium and Non-Podium Placings",
       x="Podium Status (0=no, 1=yes)",
       y="Fastest Lap Speed")

#lots of outliers, difficult to narrow down
ggplot(aes(y = lap_times_seconds, x = factor(podium)), data = dt_alpine) + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box Plot",
       subtitle="Lap Time Seconds for Podium and Non-Podium Placings",
       x="Podium Status (0=no, 1=yes)",
       y="Lap Time Seconds")  

#lap times gain but limiting scale, ignoring outliers
ggplot(aes(y = lap_times_seconds, x = factor(podium)), data = dt_alpine) + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box Plot",
       subtitle="Lap Time Seconds for Podium and Non-Podium Placings (Scaled)",
       x="Podium Status (0=no, 1=yes)",
       y="Lap Time Seconds") + 
  scale_y_continuous(limits=c(0,200)) 


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
 

#MODELS
#control for circuit ID and year

#OLS Models
#used only to test linear relationships with finishing position, podium is our primary dv for real models

#first model with key variables
OLS_A <- lm(finishing_position~lap_times_seconds+qualifying_position+pit_stops_seconds+fastestLapSpeed+year+circuitId,data=dt)

#adding finishing milliseconds
OLS_B <- lm(finishing_position~lap_times_seconds+qualifying_position+pit_stops_seconds+fastestLapSpeed+year+circuitId+finishing_seconds,data=dt)



#summary stats for models
summary(OLS_A)
summary(OLS_B)


#compare the three
stargazer(OLS_A,OLS_B, type="text")

#residuals for each

par(mfrow = c(2, 2))
plot(OLS_A, main = "OLS_A")

plot(OLS_B, main = "OLS_B")

plot(OLS_C, main = "OLS_C")
par(mfrow = c(1,1))

#compare AIC
AIC(OLS_A)
AIC(OLS_B)


#OLS_B is the best model.Year and qualifying position have strong linear relationship with finishing position


#LOGIT Models, podium as binary dv
#THESE ARE THE PRIMARY MODELS FOR INTERPRETATION AND ANALYSIS 
#note: for variables like finishing seconds and lap time seconds, we have chosen to use 
#one at a time. There are no logical two-way interactions in our variables, but 
#several one-way interactions which could double count effects and hinder analysis
#if included

#convert podium to factor for LOGIT
dt$podium <- as.factor(dt$podium)

#create models themselves
LOGIT_podKS <- glm(podium~lap_times_seconds+qualifying_position+pit_stops_seconds+fastestLapSpeed+year+circuitId+finishing_seconds+season_position+wins+qmean+lap_times_position+season_points+finishing_points,data=dt,family="binomial")
#KS has strong stats but many variables that do not make logical sense

#to start, create minimal model just qualifying position and controls 
LOGIT_podA <- glm(podium~qualifying_position+year+circuitId,data=dt_model,family="binomial")

#add fastest lap speed
LOGIT_podB <- glm(podium~qualifying_position+fastestLapSpeed+year+circuitId,data=dt_model,family = "binomial")

#add pit stop and lap time seconds
LOGIT_podC <- glm(podium~qualifying_position+fastestLapSpeed+pit_stops_seconds+lap_times_seconds+year+circuitId,data=dt_model,family="binomial")

#create model with finishing seconds instead of lap times, lap times had no significance
LOGIT_podD<- glm(podium~qualifying_position+pit_stops_seconds+fastestLapSpeed+finishing_seconds+year+circuitId,data=dt_model,family="binomial")

#create model with neither finishing seconds nor lap time seconds
LOGIT_podE <- glm(podium~qualifying_position+pit_stops_seconds+fastestLapSpeed+year+circuitId,data=dt_model,family="binomial")


#summary stats for models
summary(LOGIT_podA)
summary(LOGIT_podB)
summary(LOGIT_podC)
summary(LOGIT_podD)
summary(LOGIT_podE)

#compare the three
stargazer(LOGIT_podA,LOGIT_podB,LOGIT_podC,LOGIT_podD,LOGIT_podE, type="text")


#residuals for each

par(mfrow = c(2, 2))
plot(LOGIT_podA, main = "LOGIT_podA")

plot(LOGIT_podB, main = "LOGIT_podB")

plot(LOGIT_podC, main = "LOGIT_podC")

plot(LOGIT_podD, main = "LOGIT_podD")

plot(LOGIT_podE, main = "LOGIT_podE")
par(mfrow = c(1,1))

#McFadden's pseudo r2 for three
pR2(LOGIT_podA)
pR2(LOGIT_podB)
pR2(LOGIT_podC) #pod C is the highest
pR2(LOGIT_podD)
pR2(LOGIT_podE)

#compare AIC
AIC(LOGIT_podA)
AIC(LOGIT_podB)
AIC(LOGIT_podC) 
AIC(LOGIT_podD)
AIC(LOGIT_podE)#pod E is barely lowest

#LOGIT_podC is the best model with the secondlowest AIC and highest R2
#all variables included have highly significant relationship
#KS has high stats but many of the correlations cannot logically be included
stargazer(LOGIT_podC,type="text")

#convert to odds
exp(cbind(OR = coef(LOGIT_podC), confint(LOGIT_podC)))

#scale coefficients, this is just for analysis
#referred to https://cran.r-project.org/web/packages/jtools/vignettes/summ.html#plot_summs_and_plot_coefs
summ(LOGIT_podC,scale=TRUE)

#plot coefficients
plot_summs(LOGIT_podC,plot.distributions=TRUE,rescale.distributions = TRUE)

#LOGIT Models, points as binary dv
#will compare all the same models to see if there are any notable differences in how
#the variables interact 

#create variable for points to compare
dt$finishing_position <- as.integer(dt$finishing_position) #convert back to integer
dt$points <- ifelse(dt$finishing_position>10,0,1)
dt$points <- as.factor(dt$points)

#to start, create minimal model just qualifying position and controls 
LOGIT_poiA <- glm(points~qualifying_position+year+circuitId,data=dt,family="binomial")

#add fastest lap speed
LOGIT_poiB <- glm(points~qualifying_position+fastestLapSpeed+year+circuitId,data=dt,family = "binomial")

#add pit stop and lap time seconds
LOGIT_poiC <- glm(points~qualifying_position+fastestLapSpeed+pit_stops_seconds+lap_times_seconds+year+circuitId,data=dt,family="binomial")

#create model with finishing seconds instead of lap times, lap times had no significance
LOGIT_poiD<- glm(points~qualifying_position+pit_stops_seconds+fastestLapSpeed+finishing_seconds+year+circuitId,data=dt,family="binomial")

#create model with neither finishing seconds nor lap time seconds
LOGIT_poiE <- glm(points~qualifying_position+pit_stops_seconds+fastestLapSpeed+year+circuitId,data=dt,family="binomial")

#summary stats for models
summary(LOGIT_poiA)
summary(LOGIT_poiB)
summary(LOGIT_poiC)
summary(LOGIT_poiD)
summary(LOGIT_poiE)

#compare the three
stargazer(LOGIT_poiA,LOGIT_poiB,LOGIT_poiC,LOGIT_poiD,LOGIT_poiE, type="text")

#residuals for each

par(mfrow = c(2, 2))
plot(LOGIT_poiA, main = "LOGIT_poiA")

plot(LOGIT_poiB, main = "LOGIT_poiB")

plot(LOGIT_poiC, main = "LOGIT_poiC")

plot(LOGIT_poiD, main = "LOGIT_poiD")

plot(LOGIT_poiE, main = "LOGIT_poiE")
par(mfrow = c(1,1))

#McFadden's pseudo r2 for three
pR2(LOGIT_poiA)
pR2(LOGIT_poiB)
pR2(LOGIT_poiC)
pR2(LOGIT_poiD) #highest R2
pR2(LOGIT_poiE)

#compare AIC
AIC(LOGIT_poiA)
AIC(LOGIT_poiB)
AIC(LOGIT_poiC)
AIC(LOGIT_poiD) #lowest AIC
AIC(LOGIT_poiE)

#for points, LOGIT_poiD is the best model 
#notable difference is finishing seconds model has highest predictive power

#compare model c for both
stargazer(LOGIT_podC,LOGIT_poiC,type="text")
#smaller coefficient for qualifying for points, shows to get top 3 its much more vital

#plot coefficients for both
plot_summs(LOGIT_podC,LOGIT_poiC,model.names = c("LOGIT_podium", "LOGIT_points"))

#MLR Models
#NOTE: RAN AS EXPERIMENT, NOT BEING USED FOR INTERPRETATION
MLR_A <- multinom(finishing_position~lap_times_seconds+qualifying_position+pit_stops_seconds+fastestLapSpeed+year+circuitId, data = dt)
MLR_B <- multinom(finishing_position~lap_times_seconds+qualifying_position+pit_stops_seconds+fastestLapSpeed+year+circuitId+finishing_seconds, data = dt)

summary(MLR_A)
summary(MLR_B)

stargazer(MLR_A, type = "text")#report is too long so separate each model
stargazer(MLR_B, type = "text")


AIC(MLR_A)
AIC(MLR_B)


#MLR_A is the best model

#LOGIT podium model for alpine specifically, less effective but checking for differences

#to start, create minimal model just qualifying position and controls 
LOGIT_podAR <- glm(podium~qualifying_position+year+circuitId,data=dt_alpine,family="binomial")

#add fastest lap speed
LOGIT_podBR <- glm(podium~qualifying_position+fastestLapSpeed+year+circuitId,data=dt_alpine,family = "binomial")

#add pit stop and lap time seconds
LOGIT_podCR <- glm(podium~qualifying_position+fastestLapSpeed+pit_stops_seconds+lap_times_seconds+year+circuitId,data=dt_alpine,family="binomial")

#create model with finishing seconds instead of lap times, lap times had no significance
LOGIT_podDR<- glm(podium~qualifying_position+pit_stops_seconds+fastestLapSpeed+finishing_seconds+year+circuitId,data=dt_alpine,family="binomial")

#create model with neither finishing seconds nor lap time seconds
LOGIT_podER <- glm(podium~qualifying_position+pit_stops_seconds+fastestLapSpeed+year+circuitId,data=dt_alpine,family="binomial")


#summary stats for models
summary(LOGIT_podAR)
summary(LOGIT_podBR)
summary(LOGIT_podCR)
summary(LOGIT_podDR)
summary(LOGIT_podER)

#compare the three
stargazer(LOGIT_podAR,LOGIT_podBR,LOGIT_podCR,LOGIT_podDR,LOGIT_podER, type="text")


#residuals for each

par(mfrow = c(2, 2))
plot(LOGIT_podAR, main = "LOGIT_podAR")

plot(LOGIT_podBR, main = "LOGIT_podBR")

plot(LOGIT_podCR, main = "LOGIT_podCR")

plot(LOGIT_podDR, main = "LOGIT_podDR")

plot(LOGIT_podER, main = "LOGIT_podER")
par(mfrow = c(1,1))

#McFadden's pseudo r2 for three
pR2(LOGIT_podAR)
pR2(LOGIT_podBR)
pR2(LOGIT_podCR) #pod C is the highest
pR2(LOGIT_podDR)
pR2(LOGIT_podER)

#compare AIC
AIC(LOGIT_podAR)
AIC(LOGIT_podBR)
AIC(LOGIT_podCR) 
AIC(LOGIT_podDR)
AIC(LOGIT_podER)#pod E is barely lowest

stargazer(LOGIT_podC,LOGIT_podCR,type="text")



