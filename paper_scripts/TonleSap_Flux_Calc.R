#----
# TonleSap_Flux_Calc.R
# Jackson Swan, 2022-8-24
# 
# Last Modified by: Mark Yamane, 2023-11-15
#---

#NECESSARYPACKAGES################################
{
library(doBy)
library(magrittr)
library(dplyr)
library(ggpubr)
library(vegan)
library(tidyverse)
library(lubridate)
library(plotly)
library(reshape2)
library(hms)
library(gtable)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(scales)
library(ggthemes)
library(rstatix)
library(broom)
library(AICcmodavg)
library(lubridate)
library(qpcR)
library(data.table)
library(plyr)
library(ggh4x)
library(segmented)
}
#2020-21##################################################
##Direcory_Definitions##################################
input_all_data<- "Z:/fishproj/Cambodia Dai project/Analytic/Analytic Products/TS2020_Data_Exports/"
full_moon_dates<- as.Date(c('2020-11-30', '2020-12-30', '2021-01-29'))
data_analysis_output = "Z:/fishproj/Cambodia Dai project/Papers/Analysis Paper/Pics, Plot, Figures/figure_check/"
input_transect_data = "Z:/fishproj/Cambodia Dai project/Analytic/Analytic Products/TS2021-22_Data_Exports/Transects/"

SAVE_WKSPC = FALSE

##FLux_Dataframe_Creation###################################################
###transects###################################################
setwd(input_transect_data)
Dai3D_evening_allBNR<- list.files(path = input_transect_data,                               #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                                  pattern = "^Dai3D_ABC_10mbin_\\d{8}_fullwatercolumn_evening_BNR*.*csv", full.names = TRUE) %>%
  map_dfr(read_csv) %>%
  mutate(BNR  = str_c("BNR", match(Region_ID, unique(Region_ID))))

setwd(input_all_data)
attach(Dai3D_evening_allBNR)

sa_weights_evening_3D<- data.frame(Dai3D_evening_allBNR %>%
                                     group_by(BNR, PRC_ABC) %>%
                                     dplyr::summarise(Count = n()) %>%
                                     group_by(BNR) %>%
                                     summarise(weight = (sum(PRC_ABC/max(PRC_ABC)))))
library('plyr')
Weighted_Prop_Occupied_evening_3D<- data.frame(1/nrow(Dai3D_evening_allBNR)*sum(sa_weights_evening_3D$weight)*Dai3D_evening_allBNR$Proportion_occupied)
colnames(Weighted_Prop_Occupied_evening_3D) <- c(ncol(1), 'Weighted_Prop_Occ')
Dai3D_evening_allBNR<- cbind(Dai3D_evening_allBNR, Weighted_Prop_Occupied_evening_3D[c('Weighted_Prop_Occ')]) 

dai3D_evening_bnr1mean<- mean(Dai3D_evening_allBNR$Weighted_Prop_Occ[Dai3D_evening_allBNR$BNR == 'BNR1'])
dai3D_evening_bnr2mean<- mean(Dai3D_evening_allBNR$Weighted_Prop_Occ[Dai3D_evening_allBNR$BNR == 'BNR2']) 
dai3D_evening_bnr3mean<- mean(Dai3D_evening_allBNR$Weighted_Prop_Occ[Dai3D_evening_allBNR$BNR == 'BNR3']) 
wpo_dai3D_evening<- (sum(dai3D_evening_bnr1mean, dai3D_evening_bnr2mean)/3)

Dai15E_evening_allBNR<- list.files(path = input_transect_data,                               #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                                   pattern = "^Dai15E_ABC_10mbin_\\d{8}_fullwatercolumn_evening_BNR*.*csv", full.names = TRUE) %>%
  map_dfr(read_csv) %>%
  mutate(BNR  = str_c("BNR", match(Region_ID, unique(Region_ID))))

setwd(input_all_data)
attach(Dai15E_evening_allBNR)

sa_weights_evening_15E<- data.frame(Dai15E_evening_allBNR %>%
                                      group_by(BNR, PRC_ABC) %>%
                                      dplyr::summarise(Count = n()) %>%
                                      group_by(BNR) %>%
                                      summarise(weight = (sum(PRC_ABC/max(PRC_ABC)))))
library('plyr')
Weighted_Prop_Occupied_evening_15E<- data.frame(1/nrow(Dai15E_evening_allBNR)*sum(sa_weights_evening_15E$weight)*Dai15E_evening_allBNR$Proportion_occupied)
colnames(Weighted_Prop_Occupied_evening_15E) <- c(ncol(1), 'Weighted_Prop_Occ')
Dai15E_evening_allBNR<- cbind(Dai15E_evening_allBNR, Weighted_Prop_Occupied_evening_15E[c('Weighted_Prop_Occ')]) 

dai15E_evening_bnr1mean<- mean(Dai15E_evening_allBNR$Weighted_Prop_Occ[Dai15E_evening_allBNR$BNR == 'BNR1'])
dai15E_evening_bnr2mean<- mean(Dai15E_evening_allBNR$Weighted_Prop_Occ[Dai15E_evening_allBNR$BNR == 'BNR2']) 
dai15E_evening_bnr3mean<- mean(Dai15E_evening_allBNR$Weighted_Prop_Occ[Dai15E_evening_allBNR$BNR == 'BNR3']) 
wpo_dai15E_evening<- (sum(dai15E_evening_bnr1mean, dai15E_evening_bnr2mean, dai15E_evening_bnr3mean)/3)

Dai15E_afternoon_allBNR<- list.files(path = input_transect_data,                               #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                                     pattern = "^Dai15E_ABC_10mbin_\\d{8}_fullwatercolumn_afternoon_BNR*.*csv", full.names = TRUE) %>%
  map_dfr(read_csv) %>%
  mutate(BNR  = str_c("BNR", match(Region_ID, unique(Region_ID))))

setwd(input_all_data)
attach(Dai15E_afternoon_allBNR)

sa_weights_afternoon_15E<- data.frame(Dai15E_afternoon_allBNR %>%
                                        group_by(BNR, PRC_ABC) %>%
                                        dplyr::summarise(Count = n()) %>%
                                        group_by(BNR) %>%
                                        summarise(weight = (sum(PRC_ABC/max(PRC_ABC)))))
library('plyr')
Weighted_Prop_Occupied_afternoon_15E<- data.frame(1/nrow(Dai15E_afternoon_allBNR)*sum(sa_weights_afternoon_15E$weight)*Dai15E_afternoon_allBNR$Proportion_occupied)
colnames(Weighted_Prop_Occupied_afternoon_15E) <- c(ncol(1), 'Weighted_Prop_Occ')
Dai15E_afternoon_allBNR<- cbind(Dai15E_afternoon_allBNR, Weighted_Prop_Occupied_afternoon_15E[c('Weighted_Prop_Occ')]) 

dai15E_afternoon_bnr1mean<- mean(Dai15E_afternoon_allBNR$Weighted_Prop_Occ[Dai15E_afternoon_allBNR$BNR == 'BNR1'])
dai15E_afternoon_bnr2mean<- mean(Dai15E_afternoon_allBNR$Weighted_Prop_Occ[Dai15E_afternoon_allBNR$BNR == 'BNR2']) 
dai15E_afternoon_bnr3mean<- mean(Dai15E_afternoon_allBNR$Weighted_Prop_Occ[Dai15E_afternoon_allBNR$BNR == 'BNR3']) 
wpo_dai15E_afternoon<- (sum(dai15E_afternoon_bnr1mean, dai15E_afternoon_bnr2mean, dai15E_afternoon_bnr3mean)/3)

Dai3D_afternoon_allBNR<- list.files(path = input_transect_data,                               #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                                    pattern = "^Dai3D_ABC_10mbin_\\d{8}_fullwatercolumn_afternoon_BNR*.*csv", full.names = TRUE) %>%
  map_dfr(read_csv) %>%
  mutate(BNR  = str_c("BNR", match(Region_ID, unique(Region_ID))))

setwd(input_all_data)
attach(Dai3D_afternoon_allBNR)

sa_weights_afternoon_3D<- data.frame(Dai3D_afternoon_allBNR %>%
                                       group_by(BNR, PRC_ABC) %>%
                                       dplyr::summarise(Count = n()) %>%
                                       group_by(BNR) %>%
                                       summarise(weight = (sum(PRC_ABC/max(PRC_ABC)))))
library('plyr')
Weighted_Prop_Occupied_afternoon_3D<- data.frame(1/nrow(Dai3D_afternoon_allBNR)*sum(sa_weights_afternoon_3D$weight)*Dai3D_afternoon_allBNR$Proportion_occupied)
colnames(Weighted_Prop_Occupied_afternoon_3D) <- c(ncol(1), 'Weighted_Prop_Occ')
Dai3D_afternoon_allBNR<- cbind(Dai3D_afternoon_allBNR, Weighted_Prop_Occupied_afternoon_3D[c('Weighted_Prop_Occ')]) 

dai3D_afternoon_bnr1mean<- mean(Dai3D_afternoon_allBNR$Weighted_Prop_Occ[Dai3D_afternoon_allBNR$BNR == 'BNR1'])
dai3D_afternoon_bnr2mean<- mean(Dai3D_afternoon_allBNR$Weighted_Prop_Occ[Dai3D_afternoon_allBNR$BNR == 'BNR2']) 
dai3D_afternoon_bnr3mean<- mean(Dai3D_afternoon_allBNR$Weighted_Prop_Occ[Dai3D_afternoon_allBNR$BNR == 'BNR3']) 
wpo_dai3D_afternoon<- (sum(dai3D_afternoon_bnr1mean, dai3D_afternoon_bnr2mean)/3)

###RIVER_AREA###############################################
setwd(input_all_data)
depth_15E<- list.files(path = input_all_data,     #identifies all .csv files associated with Dai15 depth measurements and compiles them into one data frame
                       pattern = "*Bottom Line 15E.line.csv", full.names = TRUE) %>% 
  map_dfr(read_csv) %>%                                            # Store all files in list
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Ping_date),"%Y-%m-%d"), Ping_time, sep=" "), format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")) # combines the date and time columns into one date/time column


desiredGroupingUnit15E = cut(depth_15E$Date_time, breaks = "24 hour") # places the depth data into groups organised by 24 hours
aggregate(depth_15E$Depth, by = list(desiredGroupingUnit15E), FUN = mean) # creates a value frame based on the means of the grouped data
avg_depth_15E<- data.frame(aggregate(depth_15E$Depth, by = list(desiredGroupingUnit15E), FUN = mean)) # creates a data frame out of the means of the data
avg_depth_15E$Date_POSIXct <- as.POSIXct(avg_depth_15E$Group.1, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok") # renames the the date/time column and ensures it is the correct format  
avg_depth_15E$day <- 1:nrow(avg_depth_15E)  #adds a day number column
avg_depth_15E$AMPM<- as.factor(format(avg_depth_15E$Date_POSIXct, "%p")) #creates a factor column called AM/PM based on the date/time column


Dai15_max_area<- (4335.252)
Dai15_area_loss<- (15.437)
Dai3_max_area<- (4508.351)
Dai3_area_loss<- (13.812)

dates <- as.Date(avg_depth_15E$Date_POSIXct)

my_values_area15 <- numeric(length(dates))
my_values_area15[1] <- Dai15_max_area
my_values_area3 <- numeric(length(dates))
my_values_area3[1] <- Dai3_max_area



for (i in 2:length(dates)) {
  days_diff <- as.integer(dates[i] - dates[i-1])
  my_values_area15[i] <- my_values_area15[i-1] - Dai15_area_loss * days_diff
}

area_avg_15E<- cbind.data.frame(avg_depth_15E$Date_POSIXct, avg_depth_15E$day, my_values_area15) 
colnames(area_avg_15E)[1] <- 'Date_time' 
colnames(area_avg_15E)[2] <- 'Day'
colnames(area_avg_15E)[3] <- 'Area'
area_avg_15E$Dai<- paste0('15E')

for (i in 2:length(dates)) {
  days_diff <- as.integer(dates[i] - dates[i-1])
  my_values_area3[i] <- my_values_area3[i-1] - Dai3_area_loss * days_diff
}

area_avg_3D<- cbind.data.frame(avg_depth_15E$Date_POSIXct, avg_depth_15E$day, my_values_area3) 
colnames(area_avg_3D)[1] <- 'Date_time' 
colnames(area_avg_3D)[2] <- 'Day'
colnames(area_avg_3D)[3] <- 'Area'
area_avg_3D$Dai<- paste0('3D')

river_area_stack<- rbind(area_avg_15E, area_avg_3D) 
river_area_stack$Date<- as.Date(river_area_stack$Date)
#river_area_stack<- river_area_stack[-17,]
river_area_split<- split(river_area_stack, river_area_stack$Dai)



###DENSITY_CALC###################################################
rm(exports_15E, exports_3D)  
exports_15E<- list.files(path = input_all_data,                               #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                         pattern = "^15E.*FullWaterColumn_100m.sv.csv", full.names = TRUE) %>% 
  map_dfr(read_csv) %>%
  filter(Layer !=0) %>%                                                       #eliminates any data layers above layer 1 that may have been accidentally exported 
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Date_M),"%Y%m%d"), Time_M, sep=" "),format = "%Y-%m-%d %H:%M", tz="Asia/Bangkok")) %>%#combines the time and date columns into one column for proper time teries analysis
  mutate(sa_Ln = 10^(Area_Backscatter_Strength/10)) %>%
  mutate(DayNight = if_else(as.numeric(substring(Time_M, 1, 2)) >= 18 | as.numeric(substring(Time_M, 1, 2)) < 06, 'Night', "Day")) %>%
  mutate(TOD = case_when(endsWith(DayNight, 'Night') ~ 2, endsWith(DayNight, 'Day') ~ 1))

exports_3D<- list.files(path = input_all_data,      #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                        pattern = "*^3C.*FullWaterColumn_100m.sv.csv", full.names = TRUE) %>% 
  map_dfr(read_csv) %>%                                            # Store all files in list
  filter(Layer !=0) %>%     
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Date_M),"%Y%m%d"), Time_M, sep=" "),format = "%Y-%m-%d %H:%M", tz="Asia/Bangkok")) %>%
  mutate(sa_Ln = 10^(Area_Backscatter_Strength/10)) %>%
  mutate(DayNight = if_else(as.numeric(substring(Time_M, 1, 2)) >= 18 | as.numeric(substring(Time_M, 1, 2)) < 06, 'Night', "Day")) %>%
  mutate(TOD = case_when(endsWith(DayNight, 'Night') ~ 2, endsWith(DayNight, 'Day') ~ 1))
#slice(-c(1:19))#setting the mean time-lag between both dais to be 29 hours apart. The data was already offset by 10 hours so the subtraction of an additional 19 brings it to the average.

###FLOW_RATE####################################
setwd(input_transect_data)
flow_rate_15E<- read.csv('flow_rate_15E_5week_v2.csv') %>%
  # filter(Flow_rate !=0 & Flow_rate > 0.5 & Flow_rate < 3) %>%
  # #filter(rowMeans(select(., where(is.numeric))) > 0.5) %>%
  # #filter(rowMeans(select(., where(is.numeric))) < 3) %>%
  # mutate(ms_flowrate = (Flow_rate/60000)/(pi*(((11.42/2)/1000)^2))) %>%
  mutate(Time = as.POSIXct(as_datetime(as.character(Date), "%m/%d/%Y %H:%M", tz = 'Asia/Bangkok')))#%>%

desiredGroupingUnitstn1 = cut(flow_rate_15E$Time, breaks = "24 hour") # places the depth data into groups organised by 24 hours
flow_avg_15E<- aggregate(flow_rate_15E$Velocity, by = list(desiredGroupingUnitstn1), FUN = mean) # cre
flow_avg_15E$days<- as.numeric(paste0(seq(1,nrow(flow_avg_15E), by = 1)))
colnames(flow_avg_15E)[2] <- 'Velocity'

depthline15<- lm(Velocity ~ days, data = flow_avg_15E)
summary(depthline15)
coef1<- coef(depthline15)[1]
coef2<- coef(depthline15)[2]

dates <- as.Date(avg_depth_15E$Date_POSIXct)
my_values <- numeric(length(dates))
my_values[1] <- max(flow_avg_15E$Velocity)

for (i in 2:length(dates)) {
  days_diff <- as.integer(dates[i] - dates[i-1])
  my_values[i] <- my_values[i-1] + coef2 * days_diff
}

flow_avg_15E<- cbind.data.frame(avg_depth_15E$Date_POSIXct, avg_depth_15E$day, my_values) 
colnames(flow_avg_15E)[1] <- 'Date' 
colnames(flow_avg_15E)[2] <- 'Day'
colnames(flow_avg_15E)[3] <- 'Velocity'

time<- data.frame(36000/flow_avg_15E$Velocity) #The distance between point 1 and 2 divided by flow rate
time$week<- as.factor(1:nrow(time)) 
colnames(time)[1]<- 'time'

time<- data.frame((time$time/3600)) #divided by number of seconds in an hour
colnames(time)[1]<- 'time'

colnames(time)[1]<- 'time'
time$week<- as.factor(1:nrow(time))
time_2<- cbind(time, avg_depth_15E)
offsetTABLE<- time_2[, c('Date_POSIXct', 'time')]
colnames(offsetTABLE)[1]<- 'Date_Time'
colnames(offsetTABLE)[2]<- 'Dai3_OffsetHours'
offsetTABLE$Dai3_OffsetHours<- ceiling(offsetTABLE$Dai3_OffsetHours)
offsetTABLE[nrow(offsetTABLE) + 1,] = c('2021-01-31 18:00:00', 52 )
offsetTABLE<- transform(offsetTABLE, Dai3_OffsetHours = as.numeric(Dai3_OffsetHours))
###TIME_OFFSET################################################################################

#GENERATE THE DATAFRAME THAT WILL ACT AS THE LOOKUP TABLE
#offsetTABLE <- data.frame(matrix(nrow = 7, ncol = 2))  #declare the dataframe that will hold the datetime and offset
#colnames(offsetTABLE) <- c("Date_time", "Dai3_OffsetHours") #assign column names
#populate DateTime column with the START of each 5-day segment 
#Given that you said the you already have the DateTime as one column, 1'll leave the population of the DateTime column to you.
#Populate the 2nd column ("Dai3_OffsetHours") with the number of offset hours associated with the 5-day period that starts with the entry in the DateTime column
#CRUDE EXAMPLE of the contents of the lookup table (1.e. this is not code)
#DateTime         Dai3_OffsetHours
#Nov 10, 2021 6pm      29               #this is the number of hours to offset Dai 3 for DateTime >= Nov 10 and < Nov 15
#Nov 15, 2021 6pm      29.5             #this is the number of hours to offset Dai 3 for DateTime >= Nov 15 and < Nov 20
#Nov 20, 2021 6pm      30.25
#Nov 25, 2021 6pm      31
#Nov 30, 2021 6pm      31.8


#Then you'll cycle through the data subsetting the Dai3 and Dai15 dataframes based on the contents of the offsetTABLE columns




# Note: the rbind command, as written, will create a separate dataframe each time through the loop.
# 1 simply have the labeled consecutively (hence the as.character(zz))
setwd(input_all_data)

rm(list = ls(pattern = 'Dai15_Dai3_'))
for (zz in 1:(nrow(offsetTABLE)-1)){    #you want zz to start at 1 and end 1 row before the last row in offsetTABLE for (zz in 1:(nrow(offsetTABLE)-1)){
  Dai15 <- subset(exports_15E, Date_time >= offsetTABLE$Date_Time[zz] & Date_time < offsetTABLE$Date_Time[zz+1])  #pull from dataframe15 all the rows for which the datatime is between the consecutive start Date_Times
  Dai15$OffsetHours <- 0 #add column to document the # of hours included as offset
  Dai15$Dai<- 'Dai15E'
  Dai3 <- subset(exports_3D, Date_time >= (offsetTABLE$Date_Time[zz]+ hours(offsetTABLE$Dai3_OffsetHours[zz])) & Date_time < (offsetTABLE$Date_Time[zz+1]+hours(offsetTABLE$Dai3_OffsetHours[zz])))  #pull from dataframe3 all the rows for which the datetime is between the consecutive start datetimes inclusive of the offset hours
  Dai3$OffsetHours <- offsetTABLE$Dai3_OffsetHours[zz] #add column to document the # of hours included as offset
  Dai3$Dai<- 'Dai3D'
  
  eval(parse(text = paste0("Dai15_Dai3_", as.numeric(zz) + 10, " <- rbind(Dai15, Dai3)")))  #create a new dataframe for which the contents are the rows you saved inot data15 and data3
  rm(Dai15, Dai3)  #clear data15 and data3 ....and cycle through the loop again
  if (nrow(get0(paste0("Dai15_Dai3_", as.numeric(zz) + 10))) == 0) {
    rm(list=paste0("Dai15_Dai3_", as.numeric(zz) + 10))
  } else if (length(unique(get0(paste0("Dai15_Dai3_", as.numeric(zz) + 10))$Dai)) != 2) {
    rm(list=paste0("Dai15_Dai3_", as.numeric(zz) + 10))
  } 
} #end for


exports_all<- mget(ls(pattern = 'Dai15_Dai3_')) %>%
  setNames((seq_along(.))) %>%
  bind_rows(.id = 'Week') %>%
  relocate(Week, .after = last_col())
exports_all$Week<- as.numeric(exports_all$Week)

x<- split(exports_all, exports_all$Dai)
list2env(x, envir = .GlobalEnv)

exports_15E<- Dai15E
exports_3D<- Dai3D

daily_avg_15E_dates<- data.frame(aggregate(list(avgsa_Ln = exports_15E$sa_Ln), 
                                           list(Date_time = cut(exports_15E$Date_time, "24 hour"), 
                                                category = exports_15E$DayNight), 
                                           FUN = function(x) c(data_avg = mean(x))))
daily_avg_15E_dates$Date_time <- as.POSIXct(daily_avg_15E_dates$Date_time, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")

daily_avg_15E<- data.frame(aggregate(sa_Ln ~ cut(Date_time, "24 hour") + DayNight, exports_15E, mean))
colnames(daily_avg_15E)[1]<- 'Date_time'
daily_avg_15E$Date_time<- as.POSIXct(daily_avg_15E$Date_time, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")
daily_avg_15E$sa_log = 10*log10(daily_avg_15E$sa_Ln)
daily_avg_15E$date_column<- as.Date(daily_avg_15E$Date_time, format = "%Y-%m-%d")
daily_avg_15E$peak_status <- ifelse(daily_avg_15E$date_column %in% full_moon_dates, "Peak", "Off-Peak")

daily_avg_15E$Days <- as.numeric(difftime(daily_avg_15E$date_column, min(daily_avg_15E$date_column), units = "days")) + 1
dailyavg_15E_split<- split(daily_avg_15E, daily_avg_15E$DayNight)
daily_avg_15E$Date_time<- as.POSIXct(daily_avg_15E$Date_time, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")
daily_avg_15E$Date<- as.Date(daily_avg_15E$Date_time)
daily_avg_15E$sa_log = 10*log10(daily_avg_15E$sa_Ln)



#renames and sets the column with dates and times to the preferred format
daily_avg_3D_dates<- data.frame(aggregate(list(avgsa_Ln = exports_3D$sa_Ln), 
                                          list(Date_time = cut(exports_3D$Date_time, "24 hour"), 
                                               category = exports_3D$DayNight), 
                                          FUN = function(x) c(data_avg = mean(x))))
daily_avg_3D_dates$Date_time <- as.POSIXct(daily_avg_3D_dates$Date_time, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")

daily_avg_3D<- data.frame(aggregate(sa_Ln ~ cut(Date_time, "24 hour") + DayNight, exports_3D, mean))
colnames(daily_avg_3D)[1]<- 'Date_time'
daily_avg_3D$Date_time<- as.POSIXct(daily_avg_3D$Date_time, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")
daily_avg_3D$sa_log = 10*log10(daily_avg_3D$sa_Ln) 
daily_avg_3D$date_column<- as.Date(daily_avg_3D$Date_time, format = "%Y-%m-%d")
daily_avg_3D$peak_status <- ifelse(daily_avg_3D$date_column %in% full_moon_dates, "Peak", "Off-Peak")

daily_avg_3D$Days <- as.numeric(difftime(daily_avg_3D$date_column, min(daily_avg_3D$date_column), units = "days")) + 1
dailyavg_3D_split<- split(daily_avg_3D, daily_avg_3D$DayNight)

#renames and sets the column with dates and times to the preferred format



dailyavg_15E_split<- split(daily_avg_15E, daily_avg_15E$DayNight)

dailyavg_3D_split<- split(daily_avg_3D, daily_avg_3D$DayNight)
###Dai3RatioCorrection#############################################################
ratio_3D_dataframe<- data.frame((dailyavg_3D_split[['Night']]$sa_Ln)/(dailyavg_3D_split[['Day']]$sa_Ln)) #%>% 
# mutate(Week = row_number())
ratio_3D_dataframe$Week<- seq_along(ratio_3D_dataframe[,1]) 
colnames(ratio_3D_dataframe)[1] <- 'ratio'
ratio_3D<- split(ratio_3D_dataframe, seq(nrow(ratio_3D_dataframe)))


jj<- mget(ls(pattern = 'Dai15_Dai3_'))
rm(transposed_data)
n = length(jj)
transposed_data = list()
for (i in seq(n)){
  transposed_data[[i]] <- split(jj[[i]], jj[[i]]$Dai)
  transposed_data[[i]] <- lapply(transposed_data[[i]], head, min(sapply(transposed_data[[i]], nrow)))
  transposed_data[[i]] <- do.call(cbind, transposed_data[[i]])
  transposed_data[[i]]$x <- case_when(transposed_data[[i]]$Dai15E.TOD - transposed_data[[i]]$Dai3D.TOD == 1 ~ transposed_data[[i]]$Dai3D.sa_Ln * ratio_3D[[i]]$ratio,
                                      transposed_data[[i]]$Dai15E.TOD - transposed_data[[i]]$Dai3D.TOD == -1 ~ transposed_data[[i]]$Dai3D.sa_Ln/ratio_3D[[i]]$ratio,
                                      transposed_data[[i]]$Dai15E.TOD - transposed_data[[i]]$Dai3D.TOD == 0 ~ transposed_data[[i]]$Dai3D.sa_Ln + 0)
} 

adj_data<- bind_rows(transposed_data, .id = 'Week')
adj_data$Date_time<- adj_data$Dai3D.Date_time
exports_3D<- exports_3D %>% semi_join(adj_data, by = 'Date_time')
exports_3D$x<- (adj_data$x)

colnames(exports_3D)[81]<- 'adj_sa'
daily_avg_3D<- data.frame(aggregate(adj_sa ~ cut(Date_time, '24 hour') + DayNight, exports_3D, mean)) %>%
  rename_at(1,~'Date_time')
daily_avg_3D$Date_time<- as.POSIXct(daily_avg_3D$Date_time, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")
daily_avg_3D$Date<- as.Date(daily_avg_3D$Date_time)
daily_avg_3D$sa_log = 10*log10(daily_avg_3D$adj_sa) #
daily_avg_3D$date_column<- as.Date(daily_avg_3D$Date_time, format = "%Y-%m-%d")
daily_avg_3D$peak_status <- ifelse(daily_avg_3D$date_column %in% full_moon_dates, "Peak", "Off-Peak")

daily_avg_3D$Days <- as.numeric(difftime(daily_avg_3D$date_column, min(daily_avg_3D$date_column), units = "days")) + 1
dailyavg_3D_split<- split(daily_avg_3D, daily_avg_3D$DayNight)

###FLUX_CALC##########################################
setwd(data_analysis_output)


dailyavg_15E_split[['Day']]<- dailyavg_15E_split[['Day']] %>% semi_join(dailyavg_15E_split[['Night']], by = 'Date')
dailyavg_15E_split[['Night']]<- dailyavg_15E_split[['Night']] %>% semi_join(dailyavg_15E_split[['Day']], by = 'Date')
river_area_split[['15E']]<- river_area_split[['15E']] %>% semi_join(dailyavg_15E_split[['Day']], by = 'Date')

Dai15E_evening_Flux<- data.frame(dailyavg_15E_split[['Night']]$sa_Ln * river_area_split[['15E']]$Area * wpo_dai15E_evening) %>% #[1:length(dailyavg_15E_split), 'Area']
  map_dfr(as.data.frame) %>%
  rename_at(1,~'Flux') %>%
  mutate(Days = dailyavg_15E_split[['Night']]$Days) %>%
  rename_at(2,~'Days') %>%
  mutate(peak_status = dailyavg_15E_split[['Night']]$peak_status) %>%
  rename_at(3,~'peak_status') %>%
  mutate(Dates = dailyavg_15E_split[['Night']]$Date_time) %>%
  rename_at(4, ~'Dates') %>%
  mutate(flux_alt = Flux*((4)))


Dai15E_afternoon_Flux<- data.frame(dailyavg_15E_split[['Day']]$sa_Ln * river_area_split[['15E']]$Area * wpo_dai15E_afternoon) %>% #[1:length(dailyavg_15E_split), 'Area']
  map_dfr(as.data.frame) %>%
  rename_at(1,~'Flux') %>%
  mutate(Days = dailyavg_15E_split[['Day']]$Days) %>%
  rename_at(2,~'Days') %>%
  mutate(peak_status = dailyavg_15E_split[['Day']]$peak_status) %>%
  rename_at(3,~'peak_status') %>%
  mutate(Dates = dailyavg_15E_split[['Day']]$Date_time) %>%
  rename_at(4, ~'Dates') %>%
  mutate(flux_alt = Flux*((4))) 


dailyavg_3D_split[['Day']]<- dailyavg_3D_split[['Day']] %>% semi_join(dailyavg_3D_split[['Night']], by = 'Date')
dailyavg_3D_split[['Night']]<- dailyavg_3D_split[['Night']] %>% semi_join(dailyavg_3D_split[['Day']], by = 'Date')
river_area_split[['3D']]<- river_area_split[['3D']] %>% semi_join(dailyavg_3D_split[['Day']], by = 'Date')


Dai3D_evening_Flux<- data.frame(dailyavg_3D_split[['Night']]$adj_sa * river_area_split[['3D']]$Area * wpo_dai3D_evening) %>%
  map_dfr(as.data.frame) %>%
  rename_at(1,~'Flux') %>%
  mutate(Days = dailyavg_3D_split[['Night']]$Days) %>%
  rename_at(2,~'Days') %>%
  mutate(peak_status = dailyavg_3D_split[['Night']]$peak_status) %>%
  rename_at(3,~'peak_status') %>%
  mutate(Dates = dailyavg_3D_split[['Night']]$Date_time) %>%
  rename_at(4, ~'Dates') %>%
  mutate(flux_alt = Flux*((4)))


Dai3D_afternoon_Flux<- data.frame(dailyavg_3D_split[['Day']]$adj_sa * river_area_split[['3D']]$Area * wpo_dai3D_afternoon) %>%
  map_dfr(as.data.frame) %>%
  rename_at(1,~'Flux') %>%
  mutate(Days = dailyavg_3D_split[['Day']]$Days) %>%
  rename_at(2,~'Days') %>%
  mutate(peak_status = dailyavg_3D_split[['Day']]$peak_status) %>%
  rename_at(3,~'peak_status') %>%
  mutate(Dates = dailyavg_3D_split[['Day']]$Date_time) %>%
  rename_at(4, ~'Dates') %>%
  mutate(flux_alt = Flux*((4)))

##Flux_Figure_Creation##################################################################
evening_flux_compare<- rbind((mutate(Dai15E_evening_Flux, sentiment = factor("Dai15E"))),
                             (mutate(Dai3D_evening_Flux, sentiment = factor("Dai3D")))) %>%
  rename_at(6,~'Dai')
#write.csv(evening_flux_compare, 'evening_flux_compare.csv', row.names = T)

afternoon_flux_compare<- rbind((mutate(Dai15E_afternoon_Flux, sentiment = factor("Dai15E"))),
                               (mutate(Dai3D_afternoon_Flux, sentiment = factor("Dai3D")))) %>%
  rename_at(6,~'Dai')


Dai3D_TOD_flux_compare<- rbind((mutate(Dai3D_afternoon_Flux, sentiment = factor("Day"))),
                               (mutate(Dai3D_evening_Flux, sentiment = factor("Night")))) %>%
  rename_at(6,~'TOD')


Dai15E_TOD_flux_compare<- rbind((mutate(Dai15E_afternoon_Flux, sentiment = factor("Day"))),
                                (mutate(Dai15E_evening_Flux, sentiment = factor("Night")))) %>%
  rename_at(6,~'TOD')

facet_labels <- c(
  'Dai15E' = 'a',
  'Dai3C' = 'b'
)
Flux_All_2020<- rbind((mutate(Dai15E_TOD_flux_compare, sentiment = factor("Dai15E"))),
                      (mutate(Dai3D_TOD_flux_compare, sentiment = factor("Dai3D")))) %>% 
  rename_at(7, ~'Dai') %>%
  mutate(Dai = str_replace(Dai, 'Dai3D', 'Dai3C')) %>%
  mutate(date_column = as.Date(Dates))

labels <- data.frame(Dai = c("Dai15E", "Dai3C", "r"), label = c("a", "b"))

flux_comparison_All_facet_2020<- ggplot(Flux_All_2020, aes(x = date_column, y = flux_alt, color = TOD)) + 
  facet_grid(Dai ~ ., switch = 'y', labeller=as_labeller(facet_labels)) +
  geom_vline(xintercept = full_moon_dates, color = "black") +
  geom_point(size = 2) +
  ylab('') + xlab('') + labs(color = 'Time of Day') + 
  scale_x_date(limits = c(as.Date("2020-11-27"), as.Date("2021-01-30")), date_breaks = '7 days', date_labels = '%b-%d') +
  scale_color_discrete(name = "Time of Day", labels = c("Day", "Night")) +
  scale_y_continuous(breaks = seq(0, 0.15, by= 0.03), limits = c(0, .15, by = 0.03)) +
  labs(tag='a') +
  theme_bw() + theme(axis.text.x = element_text(color = 'black', size=10), 
                     axis.text.y = element_text(color = 'black', size = 10), 
                     legend.position = c(0,1), # top left position
                     legend.justification = c(0, 1), # top left justification
                     legend.box.margin = margin(5, l = 20, unit = "mm"), # small margin
                     axis.line = element_line(color = 'black'),
                     panel.border = element_rect(colour = "black", fill=NA),
                     legend.title = element_text(size = 10), 
                     legend.text = element_text(size = 10), 
                     panel.grid.minor.y = element_blank(),
                     strip.background= element_blank(),
                     strip.text = element_blank())

Dai15E_mod <- Dai15E_TOD_flux_compare %>%
  mutate(date_column = as.Date(Dates))
flux_compare_Dai15E <- ggplot(Dai15E_mod, aes(x = date_column, y = flux_alt, color = TOD)) +
  geom_vline(xintercept = full_moon_dates, color = "black") +
  geom_point(size = 2) +
  ylab(~ paste(Phi *" (m"^"2"*"h"^"-1"*")")) + xlab('Date') + labs(color = 'Time of Day') + 
  scale_x_date(limits = c(as.Date("2020-11-27"), as.Date("2021-01-30")), date_breaks = '7 days', date_labels = '%b-%d') +
  scale_color_discrete(name = "Time of Day", labels = c("Day", "Night")) +
  scale_y_continuous(breaks = seq(0, 0.15, by= 0.03), limits = c(0, .15, by = 0.03)) +
  theme_bw() + theme(axis.text.x = element_text(color = 'black', size=10), 
                     axis.text.y = element_text(color = 'black', size = 10), 
                     legend.position = c(0,1), # top left position
                     legend.justification = c(0, 1), # top left justification
                     legend.box.margin = margin(5, l = 20, unit = "mm"), # small margin
                     axis.line = element_line(color = 'black'),
                     panel.border = element_rect(colour = "black", fill=NA),
                     legend.title = element_text(size = 10), 
                     legend.text = element_text(size = 10), 
                     panel.grid.minor.y = element_blank(),
                     strip.background= element_blank(),
                     strip.text = element_blank())
plot(flux_compare_Dai15E)

Dai3C_mod <- Dai3D_TOD_flux_compare %>%
  mutate(date_column = as.Date(Dates))
flux_compare_Dai3C <- ggplot(Dai3C_mod, aes(x = date_column, y = flux_alt, color = TOD)) +
  geom_vline(xintercept = full_moon_dates, color = "black") +
  geom_point(size = 2) +
  ylab(~ paste(Phi *" (m"^"2"*"h"^"-1"*")")) + xlab('Date') + labs(color = 'Time of Day') + 
  scale_x_date(limits = c(as.Date("2020-11-27"), as.Date("2021-01-30")), date_breaks = '7 days', date_labels = '%b-%d') +
  scale_color_discrete(name = "Time of Day", labels = c("Day", "Night")) +
  scale_y_continuous(breaks = seq(0, 0.15, by= 0.03), limits = c(0, .15, by = 0.03)) +
  theme_bw() + theme(axis.text.x = element_text(color = 'black', size=10), 
                     axis.text.y = element_text(color = 'black', size=10), 
                     legend.position = c(0,1), # top left position
                     legend.justification = c(0, 1), # top left justification
                     legend.box.margin = margin(5, l = 20, unit = "mm"), # small margin
                     axis.line = element_line(color = 'black'),
                     panel.border = element_rect(colour = "black", fill=NA),
                     legend.title = element_text(size = 10), 
                     legend.text = element_text(size = 10), 
                     panel.grid.minor.y = element_blank(),
                     strip.background= element_blank(),
                     strip.text = element_blank())
plot(flux_compare_Dai3C)

flux_comparison_All_facet_2020<- annotate_figure(flux_comparison_All_facet_2020, left = textGrob(~ paste(Phi*" (m"^"2"*"h"^"-1"*")"), rot = 90, vjust = 1, gp = gpar(cex = 1, fontsize=12)),
                                                 bottom = textGrob('Date', gp = gpar(cex = 1, fontsize=12)))+ bgcolor('white')
comb <- ggarrange(flux_compare_Dai15E, flux_compare_Dai3C, labels=c('a', 'b'), ncol=1, nrow=2, align='v')
plot(comb)
ggsave(filename = "fig_11.eps", plot = comb, width = 170, height = 127, units = c("mm"), dpi = 1200)

plot(flux_comparison_All_facet_2020)
ggsave('fig_11.eps', device='eps', plot = flux_comparison_All_facet_2020, width = 190, height = 135, units = c("mm"), dpi = 1200)


##MORTALITY###################################################################################################################
Dai3D_TOD_flux_compare$alt_dates<- as.Date(Dai3D_TOD_flux_compare$Dates)
Dai15E_TOD_flux_compare$alt_dates<- as.Date(Dai15E_TOD_flux_compare$Dates)
Dai15E_TOD_flux_compare_2<- Dai15E_TOD_flux_compare %>% semi_join(Dai3D_TOD_flux_compare, by = 'alt_dates')


subtract<- (Dai15E_TOD_flux_compare$flux_alt - Dai3D_TOD_flux_compare$flux_alt)
subtraction_2020<- cbind(Dai15E_TOD_flux_compare, subtract)


Mortality_Index_2020<- ggplot(subtraction_2020, aes(x = alt_dates, y = subtract, color = TOD)) +  geom_hline(yintercept = 0, color= 'black', size= 0.5) + 
  geom_vline(xintercept = full_moon_dates, color = "black") +
  geom_point(size= 2) +
  scale_x_date(limits = c(as.Date("2020-11-27"), as.Date("2021-01-30")), date_breaks = '7 days', date_labels = '%b-%d') +
  scale_y_continuous(breaks = seq(-0.05, .1, by = 0.02), limits = c(-0.05, .1, by = 0.02)) + 
  scale_color_discrete(name = "Time of Day", labels = c("Day", "Night")) +
  xlab('Date') + ylab(~ paste(Delta , Phi *" (m"^"2"*"h"^"-1"*")")) + 
  #scale_fill_discrete(name= 'Day/Night') +
  theme_bw() + theme(axis.text.x = element_text(color = 'black', size = 10), 
                     axis.text.y = element_text(color = 'black', size = 10), 
                     legend.position = c(0,1), # top left position
                     legend.justification = c(0, 1), # top left justification
                     legend.box.margin = margin(5, l = 20, unit = "mm"), # small margin
                     legend.title = element_text(size = 10), 
                     legend.text = element_text(size = 10), axis.line = element_line(color = 'black'),
                     panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.minor.y = element_blank()) 
plot(Mortality_Index_2020)
ggsave('fig_12.eps', device='eps', plot = Mortality_Index_2020, width = 190, height = 95, units = c("mm"), dpi = 1200)

##VERBOSITY#####
if (SAVE_WKSPC) {
  setwd(data_analysis_output)
  write.csv(Dai3D_evening_allBNR, 'Dai3D_evening_allBNR.csv', row.names = T)
  setwd(data_analysis_output)
  write.csv(Dai15E_evening_allBNR, 'Dai15E_evening_allBNR.csv', row.names = T)
  setwd(data_analysis_output)
  write.csv(Dai15E_afternoon_allBNR, 'Dai15E_afternoon_allBNR.csv', row.names = T)
  setwd(data_analysis_output)
  write.csv(Dai3D_afternoon_allBNR, 'Dai3D_afternoon_allBNR.csv', row.names = T)
  
  write.csv(Dai15E_evening_Flux, 'Dai15E_eveing_flux.csv', row.names = T)
  write.csv(Dai15E_afternoon_Flux, 'Dai15E_afternoon_flux.csv', row.names = T)
  write.csv(Dai3D_evening_Flux, 'Dai3D_eveing_flux.csv', row.names = T)
  write.csv(Dai3D_afternoon_Flux, 'Dai3D_afternoon_flux.csv', row.names = T)
  write.csv(Flux_All, 'Flux_All.csv', row.names = T)
  write.csv(afternoon_flux_compare, 'afternoon_flux_compare.csv', row.names = T)
  write.csv(subtraction, 'mortality_index.csv', row.names = T)
}