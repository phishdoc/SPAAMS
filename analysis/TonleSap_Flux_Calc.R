#----
# TonleSap_Flux_Calc.R
# Jackson Swan, 2023-04-11
# 
# Last Modified by: Mark Yamane, 2023-10-26
#---

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(lubridate)
library(qpcR)
library(data.table)
library(plyr)
{Sys.sleep(5)}
message("Running script #4, Flux Calculation")
{Sys.sleep(5)}
setwd(input_all_data)
setwd(input_transect_data)
###DAI3######################################################################################################################################################################################
Dai3D_evening_allBNR<- list.files(path = input_transect_data,                               #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                                  pattern = "^Dai3D_ABC_10mbin_20221206_fullwatercolumn_evening_BNR*.*csv", full.names = TRUE) %>%
  map_dfr(read_csv, show_col_types = FALSE) %>%
  mutate(BNR  = str_c("BNR", match(Region_ID, unique(Region_ID))))
setwd(data_analysis_output)
write.csv(Dai3D_evening_allBNR, 'Dai3D_evening_allBNR.csv', row.names = T)
setwd(input_all_data)
attach(Dai3D_evening_allBNR)

sa_weights_evening_3D<- data.frame(Dai3D_evening_allBNR %>%
                                     group_by(BNR, PRC_ABC) %>%
                                     dplyr::summarise(Count = n()) %>%
                                     group_by(BNR) %>%
                                     summarise(weight = (sum(PRC_ABC/max(PRC_ABC)))))

Weighted_Prop_Occupied_evening_3D<- data.frame(1/nrow(Dai3D_evening_allBNR)*sum(sa_weights_evening_3D$weight)*Dai3D_evening_allBNR$Proportion_occupied)
colnames(Weighted_Prop_Occupied_evening_3D) <- c(ncol(1), 'Weighted_Prop_Occ')
Dai3D_evening_allBNR<- cbind(Dai3D_evening_allBNR, Weighted_Prop_Occupied_evening_3D[c('Weighted_Prop_Occ')]) 

dai3D_evening_bnr1mean<- mean(Dai3D_evening_allBNR$Weighted_Prop_Occ[Dai3D_evening_allBNR$BNR == 'BNR1'])
dai3D_evening_bnr2mean<- mean(Dai3D_evening_allBNR$Weighted_Prop_Occ[Dai3D_evening_allBNR$BNR == 'BNR2']) 
#dai3D_evening_bnr3mean<- mean(Dai3D_evening_allBNR$Weighted_Prop_Occ[Dai3D_evening_allBNR$BNR == 'BNR3']) 
wpo_dai3D_evening<- (sum(dai3D_evening_bnr1mean, dai3D_evening_bnr2mean)/2)
####DAI15####################################################################################################################################################################################################
Dai15E_evening_allBNR<- list.files(path = input_transect_data,                               #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                                   pattern = "^Dai15E_ABC_10mbin_20221204_fullwatercolumn_evening_BNR*.*csv", full.names = TRUE) %>%
  map_dfr(read_csv, show_col_types = FALSE) %>%
  mutate(BNR  = str_c("BNR", match(Region_ID, unique(Region_ID))))
setwd(data_analysis_output)
write.csv(Dai15E_evening_allBNR, 'Dai15E_evening_allBNR.csv', row.names = T)
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

#afternoon################
Dai15E_afternoon_allBNR<- list.files(path = input_transect_data,                               #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                                     pattern = "^Dai15E_ABC_10mbin_20221204_fullwatercolumn_afternoon_BNR*.*csv", full.names = TRUE) %>%
  map_dfr(read_csv, show_col_types = FALSE) %>%
  mutate(BNR  = str_c("BNR", match(Region_ID, unique(Region_ID))))
setwd(data_analysis_output)
write.csv(Dai15E_afternoon_allBNR, 'Dai15E_afternoon_allBNR.csv', row.names = T)
setwd(input_all_data)
attach(Dai15E_afternoon_allBNR)

sa_weights_afternoon_15E<- data.frame(Dai15E_afternoon_allBNR %>%
                                        group_by(BNR, PRC_ABC) %>%
                                        dplyr::summarise(Count = n()) %>%
                                        group_by(BNR) %>%
                                        summarise(weight = (sum(PRC_ABC/max(PRC_ABC)))))

Weighted_Prop_Occupied_afternoon_15E<- data.frame(1/nrow(Dai15E_afternoon_allBNR)*sum(sa_weights_afternoon_15E$weight)*Dai15E_afternoon_allBNR$Proportion_occupied)
colnames(Weighted_Prop_Occupied_afternoon_15E) <- c(ncol(1), 'Weighted_Prop_Occ')
Dai15E_afternoon_allBNR<- cbind(Dai15E_afternoon_allBNR, Weighted_Prop_Occupied_afternoon_15E[c('Weighted_Prop_Occ')]) 

dai15E_afternoon_bnr1mean<- mean(Dai15E_afternoon_allBNR$Weighted_Prop_Occ[Dai15E_afternoon_allBNR$BNR == 'BNR1'])
dai15E_afternoon_bnr2mean<- mean(Dai15E_afternoon_allBNR$Weighted_Prop_Occ[Dai15E_afternoon_allBNR$BNR == 'BNR2']) 
dai15E_afternoon_bnr3mean<- mean(Dai15E_afternoon_allBNR$Weighted_Prop_Occ[Dai15E_afternoon_allBNR$BNR == 'BNR3']) 
wpo_dai15E_afternoon<- (sum(dai15E_afternoon_bnr1mean, dai15E_afternoon_bnr2mean, dai15E_afternoon_bnr3mean)/3)

##afternoon#####
Dai3D_afternoon_allBNR<- list.files(path = input_transect_data,                               #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                                    pattern = "^Dai3D_ABC_10mbin_20221206_fullwatercolumn_afternoon_BNR*.*csv", full.names = TRUE) %>%
  map_dfr(read_csv, show_col_types = FALSE) %>%
  mutate(BNR  = str_c("BNR", match(Region_ID, unique(Region_ID))))
setwd(data_analysis_output)
write.csv(Dai3D_afternoon_allBNR, 'Dai3D_afternoon_allBNR.csv', row.names = T)
setwd(input_all_data)
attach(Dai3D_afternoon_allBNR)

sa_weights_afternoon_3D<- data.frame(Dai3D_afternoon_allBNR %>%
                                       group_by(BNR, PRC_ABC) %>%
                                       dplyr::summarise(Count = n()) %>%
                                       group_by(BNR) %>%
                                       summarise(weight = (sum(PRC_ABC/max(PRC_ABC)))))

Weighted_Prop_Occupied_afternoon_3D<- data.frame(1/nrow(Dai3D_afternoon_allBNR)*sum(sa_weights_afternoon_3D$weight)*Dai3D_afternoon_allBNR$Proportion_occupied)
colnames(Weighted_Prop_Occupied_afternoon_3D) <- c(ncol(1), 'Weighted_Prop_Occ')
Dai3D_afternoon_allBNR<- cbind(Dai3D_afternoon_allBNR, Weighted_Prop_Occupied_afternoon_3D[c('Weighted_Prop_Occ')]) 

dai3D_afternoon_bnr1mean<- mean(Dai3D_afternoon_allBNR$Weighted_Prop_Occ[Dai3D_afternoon_allBNR$BNR == 'BNR1'])
dai3D_afternoon_bnr2mean<- mean(Dai3D_afternoon_allBNR$Weighted_Prop_Occ[Dai3D_afternoon_allBNR$BNR == 'BNR2']) 
#dai3D_afternoon_bnr3mean<- mean(Dai3D_afternoon_allBNR$Weighted_Prop_Occ[Dai3D_afternoon_allBNR$BNR == 'BNR3']) 
wpo_dai3D_afternoon<- (sum(dai3D_afternoon_bnr1mean, dai3D_afternoon_bnr2mean)/2)

####RIVERDEPTHLOSSCALC##################################################################################################################################
#depth_3D <- list.files(path = 'Z:/fishproj/Cambodia Dai project/Analytic/Analytic Products/TS2021-22_Data_Exports',     #identifies all .csv files associated with Dai3 depth measurements and compiles them into one data frame
#                       pattern = "*Bottom Line 3C.line.csv", full.names = TRUE) %>% 
#  map_dfr(read_csv) %>%                                            # Store all files in list
#  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Ping_date),"%Y-%m-%d"), Ping_time, sep=" "), format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")) # combines the date and time columns into one date/time column 
# Print data to RStudio console
depth_15E<- list.files(path = input_all_data,     #identifies all .csv files associated with Dai15 depth measurements and compiles them into one data frame
                       pattern = "*Bottom Line stn1.line.csv", full.names = TRUE) %>% 
  map_dfr(read_csv, show_col_types = FALSE) %>%                                            # Store all files in list
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Depth_date),"%Y-%m-%d"), Depth_time, sep=" "), format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")) # combines the date and time columns into one date/time column
setwd(input_transect_data)
river_area_evening<- read.csv('total_slice_area_weekly_2022-23.csv')
river_area_split<- split(river_area_evening, river_area_evening$Dai)

desiredGroupingUnit15E = cut(depth_15E$Date_time, breaks = "24 hour") # places the depth data into groups organised by 24 hours
aggregate(depth_15E$Depth_meters, by = list(desiredGroupingUnit15E), FUN = mean) # creates a value frame based on the means of the grouped data
avg_depth_15E<- data.frame(aggregate(depth_15E$Depth_meters, by = list(desiredGroupingUnit15E), FUN = mean)) # creates a data frame out of the means of the data
avg_depth_15E$Date_POSIXct <- as.POSIXct(avg_depth_15E$Group.1, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok") # renames the the date/time column and ensures it is the correct format  
avg_depth_15E$day <- 1:nrow(avg_depth_15E)  #adds a day number column
avg_depth_15E$AMPM<- as.factor(format(avg_depth_15E$Date_POSIXct, "%p")) #creates a factor column called AM/PM based on the date/time column


Dai15_max_area<- (4781.349+20.068)
Dai15_area_loss<- (20.068)
Dai3_max_area<- (5082.595+9.418)
Dai3_area_loss<- (9.418)

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

#desiredGroupingUnit3D = cut(depth_3D$Date_time, breaks = "120 hour") #places the depth data into groups organised by 24 hours
#aggregate(depth_3D$Depth, by = list(desiredGroupingUnit3D), FUN = mean) # creates a value frame based on the means of the grouped data
#avg_depth_3D<- data.frame(aggregate(depth_3D$Depth, by = list(desiredGroupingUnit3D), FUN = mean)) # creates a data frame out of the means of the data
#avg_depth_3D$Date_POSIXct <- as.POSIXct(avg_depth_3D$Group.1, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok") # renames the the date/time column and ensures it is the correct format
#avg_depth_3D$day <- 1:nrow(avg_depth_3D) #adds a day number column
#avg_depth_3D$AMPM<- as.factor(format(avg_depth_3D$Date_POSIXct, "%p")) #creates a factor column called AM/PM based on the date/time column

#avg_depth_15E<- data.frame(avg_depth_15E[-c(59),]) #Removes a row from a dataframe, optional line: if two dataframes dont have equal numbers of rows, enter the dataframe name and the row to be deleted.
#depth_stack<- rbind((mutate(avg_depth_15E, sentiment = factor("Dai 15E"))), (mutate(avg_depth_3D, sentiment = factor("Dai 3D")))) #Stacks the Dai 15 and 3 dataframes into one dataframe
#depth_stack$AMPM<- as.factor(format(depth_stack$Date_POSIXct, "%p")) #Creates an day or night factor column in the dataframe 
#colnames(depth_stack)[5] <- "AMPM" #Renames column 5 "AMPM"
#colnames(depth_stack)[6] <- "Dai" #Renames column 5 "Dai"

###DENSITY_CALC###################################################
rm(exports_15E, exports_3D)  
exports_15E<- list.files(path = input_all_data,                               #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                         pattern = "^15E.*FullWaterColumn_100m.sv.csv", full.names = TRUE) %>% 
  map_dfr(read_csv, show_col_types = FALSE) %>%
  filter(Layer !=0) %>%                                                       #eliminates any data layers above layer 1 that may have been accidentally exported 
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Date_M),"%Y%m%d"), Time_M, sep=" "),format = "%Y-%m-%d %H:%M", tz="Asia/Bangkok")) %>%#combines the time and date columns into one column for proper time teries analysis
  mutate(sa_Ln = 10^(Area_Backscatter_Strength/10)) %>%
  mutate(DayNight = if_else(as.numeric(substring(Time_M, 1, 2)) >= 18 | as.numeric(substring(Time_M, 1, 2)) < 06, 'Night', "Day")) %>%
  mutate(TOD = case_when(endsWith(DayNight, 'Night') ~ 2, endsWith(DayNight, 'Day') ~ 1))

exports_3D<- list.files(path = input_all_data,      #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                        pattern = "*^3D.*FullWaterColumn_100m.sv.csv", full.names = TRUE) %>% 
  map_dfr(read_csv, show_col_types = FALSE) %>%                                            # Store all files in list
  filter(Layer !=0) %>%     
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Date_M),"%Y%m%d"), Time_M, sep=" "),format = "%Y-%m-%d %H:%M", tz="Asia/Bangkok")) %>%
  mutate(sa_Ln = 10^(Area_Backscatter_Strength/10)) %>%
  mutate(DayNight = if_else(as.numeric(substring(Time_M, 1, 2)) >= 18 | as.numeric(substring(Time_M, 1, 2)) < 06, 'Night', "Day")) %>%
  mutate(TOD = case_when(endsWith(DayNight, 'Night') ~ 2, endsWith(DayNight, 'Day') ~ 1))
#slice(-c(1:19))#setting the mean time-lag between both dais to be 29 hours apart. The data was already offset by 10 hours so the subtraction of an additional 19 brings it to the average.

###FLOW_RATE####################################
setwd(input_transect_data)
flow_rate_15E<- read.csv('flowrate_15E.csv') %>%
  filter(Flow_rate !=0 & Flow_rate > 0.5 & Flow_rate < 3) %>%
  #filter(rowMeans(select(., where(is.numeric))) > 0.5) %>%
  #filter(rowMeans(select(., where(is.numeric))) < 3) %>%
  mutate(ms_flowrate = (Flow_rate/60000)/(pi*(((11.42/2)/1000)^2))) %>%
  mutate(Time = as.POSIXct(as_datetime(as.character(Time), "%m/%d/%Y %H:%M", tz = 'Asia/Bangkok')))#%>%
  #mutate(Date = as.Date(Date),'%y-%m-%d' )
#flow_rate_15E$Date_2<- mdy(flow_rate_15E$Date, format= '%y-%m-%d')
#flow_rate_15E$Date_2<- as.character.POSIXt(flow_rate_15E$Date, format = '%y-%m-%d')

#flow_rate_15E$Date <- mutate(Date = as.POSIXct(paste(as.Date(as.character(Date),"%Y-%m-%d"), Date, sep=" "), format = "%Y-%m-%d"))
#desiredGroupingUnit15E = cut(flow_rate_15E$Date, breaks = '1 day') #groups linear data into 12 hour time blocks from 6am to 6pm 
#aggregate(flow_rate_15E$value, by = list(desiredGroupingUnit15E), FUN = mean) #averages the data that has been grouped and saves it as a value
#flow_avg_15E<- data.frame(aggregate(flow_rate_15E$value, by = list(desiredGroupingUnit15E), FUN = mean)) #adds the grouped avarged value to the dataframe #converts the nnewly averaged linear data back to logarithmic
#flow_avg_15E$Date <- as.Date(flow_avg_15E$Group.1, format = "%Y-%m-%d", tz="Asia/Bangkok") #renames and sets the column with dates and times to the preferred format
#flow_avg_15E$week<- as.factor(1:nrow(flow_avg_15E))
desiredGroupingUnitstn1 = cut(flow_rate_15E$Time, breaks = "24 hour") # places the depth data into groups organised by 24 hours
flow_avg_15E<- aggregate(flow_rate_15E$ms_flowrate, by = list(desiredGroupingUnitstn1), FUN = mean) # cre
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
#time<- data.frame(time$time - 10) #The data exports were done at a universal 10 hour offset between dais. The new offset times have 10 hours subtracted from them to account for this. 
colnames(time)[1]<- 'time'
time$week<- as.factor(1:nrow(time))
#avg_depth_3C<- read.csv('Z:/fishproj/Cambodia Dai project/Analytic/Analytic Products/TS2020_Exports_Scripts_Output/avg_depth_3C.csv')
time_2<- cbind(time, avg_depth_15E)
offsetTABLE<- time_2[, c('Date_POSIXct', 'time')]
colnames(offsetTABLE)[1]<- 'Date_Time'
colnames(offsetTABLE)[2]<- 'Dai3_OffsetHours'
offsetTABLE$Dai3_OffsetHours<- ceiling(offsetTABLE$Dai3_OffsetHours)
offsetTABLE[nrow(offsetTABLE) + 1,] = c('2023-01-23 18:00:00', 52 )
offsetTABLE<- transform(offsetTABLE, Dai3_OffsetHours = as.numeric(Dai3_OffsetHours))
###TIME_OFFSET################################################################################

#Some dummy code for Jackson 
#Purpose: give Jackson a starting point for pulling data from the Dai 3 and Dai 15 dataframes based on DateTime ....and offset hours as appropriate for Dai 3



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


for (zz in 1:(nrow(offsetTABLE)-1)){    #you want zz to start at 1 and end 1 row before the last row in offsetTABLE
  Dai15 <- subset(exports_15E, Date_time >= offsetTABLE$Date_Time[zz] & Date_time < offsetTABLE$Date_Time[zz+1])  #pull from dataframe15 all the rows for which the datatime is between the consecutive start Date_Times
  Dai15$OffsetHours <- 0 #add column to document the # of hours included as offset 
  Dai15$Dai<- 'Dai15E'
  Dai3 <- subset(exports_3D, Date_time >= (offsetTABLE$Date_Time[zz]+ hours(offsetTABLE$Dai3_OffsetHours[zz])) & Date_time < (offsetTABLE$Date_Time[zz+1]+hours(offsetTABLE$Dai3_OffsetHours[zz])))  #pull from dataframe3 all the rows for which the datetime is between the consecutive start datetimes inclusive of the offset hours
  Dai3$OffsetHours <- offsetTABLE$Dai3_OffsetHours[zz] #add column to document the # of hours included as offset
  Dai3$Dai<- 'Dai3D'
  
  eval(parse(text = paste0("Dai15_Dai3_", as.numeric(zz) + 10, " <- rbind(Dai15, Dai3)")))  #create a new dataframe for which the contents are the rows you saved inot data15 and data3
  rm(Dai15, Dai3)  #clear data15 and data3 ....and cycle through the loop again
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
daily_avg_15E$sa_log = 10*log10(daily_avg_15E$sa_Ln) #
full_moon_dates<- as.Date(c('2022-12-08', '2023-01-07'))
daily_avg_15E$date_column<- as.Date(daily_avg_15E$Date_time, format = "%Y-%m-%d")
daily_avg_15E$peak_status <- ifelse(daily_avg_15E$date_column %in% full_moon_dates, "Peak", "Off-Peak")

daily_avg_15E$Days <- as.numeric(difftime(daily_avg_15E$date_column, min(daily_avg_15E$date_column), units = "days")) + 1
dailyavg_15E_split<- split(daily_avg_15E, daily_avg_15E$DayNight)
daily_avg_15E$Date_time<- as.POSIXct(daily_avg_15E$Date_time, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")
daily_avg_15E$Date<- as.Date(daily_avg_15E$Date_time)
daily_avg_15E$sa_log = 10*log10(daily_avg_15E$sa_Ln) #
#converts logarithmic data to linear 
#daily_avg_15E_test <- data.frame(aggregate(sa_Ln ~ cut(Date_time, "24 hour") + DayNight, exports_15E, mean))



#renames and sets the column with dates and times to the preferred format
daily_avg_3D_dates<- data.frame(aggregate(list(avgsa_Ln = exports_3D$sa_Ln), 
                                          list(Date_time = cut(exports_3D$Date_time, "24 hour"), 
                                               category = exports_3D$DayNight), 
                                          FUN = function(x) c(data_avg = mean(x))))
daily_avg_3D_dates$Date_time <- as.POSIXct(daily_avg_3D_dates$Date_time, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")

#daily_avg_3D<- data.frame(aggregate(sa_Ln ~ DayNight + Week, exports_3D, mean))
#daily_avg_3D$Date_time<- (daily_avg_3D_dates$Date_time)
daily_avg_3D<- data.frame(aggregate(sa_Ln ~ cut(Date_time, "24 hour") + DayNight, exports_3D, mean))
colnames(daily_avg_3D)[1]<- 'Date_time'#data.frame(aggregate(sa_Ln ~ DayNight + Week, exports_3D, mean))
daily_avg_3D$Date_time<- as.POSIXct(daily_avg_3D$Date_time, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")
daily_avg_3D$sa_log = 10*log10(daily_avg_3D$sa_Ln) #
full_moon_dates<- as.Date(c('2022-12-08', '2023-01-07'))
daily_avg_3D$date_column<- as.Date(daily_avg_3D$Date_time, format = "%Y-%m-%d")
daily_avg_3D$peak_status <- ifelse(daily_avg_3D$date_column %in% full_moon_dates, "Peak", "Off-Peak")

daily_avg_3D$Days <- as.numeric(difftime(daily_avg_3D$date_column, min(daily_avg_3D$date_column), units = "days")) + 1
daily_avg_3D<- daily_avg_3D[-31,]
dailyavg_3D_split<- split(daily_avg_3D, daily_avg_3D$DayNight)

#renames and sets the column with dates and times to the preferred format



dailyavg_15E_split<- split(daily_avg_15E, daily_avg_15E$DayNight)

dailyavg_3D_split<- split(daily_avg_3D, daily_avg_3D$DayNight)
common_datetimes<- intersect(dailyavg_3D_split[['Day']]$Date_time, dailyavg_3D_split[['Night']]$Date_time)
for (i in seq_along(dailyavg_3D_split)) {
  dailyavg_3D_split[[i]] <- dailyavg_3D_split[[i]][dailyavg_3D_split[[i]]$Date_time %in% common_datetimes, ]
}
ratio_3D<- data.frame((dailyavg_3D_split[['Night']]$sa_Ln)/(dailyavg_3D_split[['Day']]$sa_Ln)) #%>% 
 # mutate(Week = row_number())
ratio_3D$Week<- seq_along(ratio_3D[,1]) 
colnames(ratio_3D)[1] <- 'ratio'
ratio_3D<- split(ratio_3D, seq(nrow(ratio_3D)))


jj<- mget(ls(pattern = 'Dai15_Dai3_'))
#jj <- lapply(jj, function(jj) {
#  jj$Dai <- factor(jj$Dai)
#  return(jj)
#})
n = nrow(time)
transposed_data = list()
for (i in seq(n)) {
  transposed_data[[i]]<- split(jj[[i]], jj[[i]]$Dai)
  transposed_data[[i]] <- lapply(transposed_data[[i]], head, min(sapply(transposed_data[[i]], nrow)))
  transposed_data[[i]] <- do.call(cbind, transposed_data[[i]])
  #print(transposed_data[[i]]$Dai15E.TOD)
  #print(transposed_data[[i]]$Dai3D.TOD)
  #print(ratio_3D[[i]]$ratio) # add this line to print the dataframe before modification
  transposed_data[[i]]$x <- case_when(transposed_data[[i]]$Dai15E.TOD - transposed_data[[i]]$Dai3D.TOD == 1 ~ transposed_data[[i]]$Dai3D.sa_Ln * ratio_3D[[i]]$ratio,
                                      transposed_data[[i]]$Dai15E.TOD - transposed_data[[i]]$Dai3D.TOD == -1 ~ transposed_data[[i]]$Dai3D.sa_Ln/ratio_3D[[i]]$ratio,
                                      transposed_data[[i]]$Dai15E.TOD - transposed_data[[i]]$Dai3D.TOD == 0 ~ transposed_data[[i]]$Dai3D.sa_Ln + 0)
  print('made it this far')  # add this line to print the dataframe after modification
}

adj_data<- bind_rows(transposed_data, .id = 'Week')
colnames(adj_data)[202]<- 'Date_time'
exports_3D<- exports_3D %>% semi_join(adj_data, by = 'Date_time')
exports_3D$x<- (adj_data$x)

colnames(exports_3D)[105]<- 'adj_sa'
#attach(exports_3D)
daily_avg_3D<- data.frame(aggregate(adj_sa ~ cut(Date_time, '24 hour') + DayNight, exports_3D, mean)) %>%
  rename_at(1,~'Date_time')
daily_avg_3D$Date_time<- as.POSIXct(daily_avg_3D$Date_time, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")
daily_avg_3D$Date<- as.Date(daily_avg_3D$Date_time)
daily_avg_3D$sa_log = 10*log10(daily_avg_3D$adj_sa) #
daily_avg_3D$date_column<- as.Date(daily_avg_3D$Date_time, format = "%Y-%m-%d")
daily_avg_3D$peak_status <- ifelse(daily_avg_3D$date_column %in% full_moon_dates, "Peak", "Off-Peak")

daily_avg_3D$Days <- as.numeric(difftime(daily_avg_3D$date_column, min(daily_avg_3D$date_column), units = "days")) + 1
daily_avg_3D<- daily_avg_3D[-31,]
dailyavg_3D_split<- split(daily_avg_3D, daily_avg_3D$DayNight)

#daily_avg_3D<- data.frame(aggregate(adj_sa ~ DayNight + Week, exports_3D, mean))
#daily_avg_3D$Date_time<- (daily_avg_3D_dates$Date_time)
#daily_avg_3D$sa_log = 10*log10(daily_avg_3D$adj_sa) #converts the nnewly averaged linear data back to logarithmic
#daily_avg_3D<- daily_avg_3D[-31,]
#dailyavg_3D_split<- split(daily_avg_3D, daily_avg_3D$DayNight)
###FLUX_CALC##########################################
setwd(data_analysis_output)
Dai15E_evening_Flux<- data.frame(dailyavg_15E_split[['Night']]$sa_Ln * river_area_split[['15E']]$Area * wpo_dai15E_evening) %>%
  map_dfr(as.data.frame, show_col_types = FALSE) %>%
  rename_at(1,~'Flux') %>%
  mutate(Days = dailyavg_15E_split[['Night']]$Days) %>%
  rename_at(2,~'Days') %>%
  mutate(peak_status = dailyavg_15E_split[['Night']]$peak_status) %>%
  rename_at(3,~'peak_status') %>%
  mutate(Dates = dailyavg_15E_split[['Night']]$Date_time) %>%
  rename_at(4, ~'Dates') %>%
  mutate(flux_alt = Flux*((4)*12))
 
write.csv(Dai15E_evening_Flux, paste0(prefix,"_Dai15E_eveing_flux.csv"), row.names = T)

Dai15E_afternoon_Flux<- data.frame(dailyavg_15E_split[['Day']]$sa_Ln * river_area_split[['15E']]$Area * wpo_dai15E_afternoon) %>%
  map_dfr(as.data.frame, show_col_types = FALSE) %>%
  rename_at(1,~'Flux') %>%
  mutate(Days = dailyavg_15E_split[['Day']]$Days) %>%
  rename_at(2,~'Days') %>%
  mutate(peak_status = dailyavg_15E_split[['Day']]$peak_status) %>%
  rename_at(3,~'peak_status') %>%
  mutate(Dates = dailyavg_15E_split[['Day']]$Date_time) %>%
  rename_at(4, ~'Dates') %>%
  mutate(flux_alt = Flux*((4)*12)) 
  
write.csv(Dai15E_afternoon_Flux, paste0(prefix,"_Dai15E_afternoon_flux.csv"), row.names = T)

Dai15E_Night_Flux<- ggplot(Dai15E_evening_Flux, aes(x = Dates, y = flux_alt)) + geom_point(size = 4, color = 'grey') + 
  geom_vline(xintercept = (Dai15E_evening_Flux$Dates[Dai15E_evening_Flux$peak_status == "Peak"]), color = "blue") +
  ggtitle('Dai15E Night')  +
  scale_x_datetime(date_breaks = '5 days', date_labels = '%b-%d') +
  scale_y_continuous(breaks = ~ seq(0, max(.x), 0.01))
plot(Dai15E_Night_Flux)
ggsave(paste0(prefix,"_Dai15E_Evening_Flux.pdf"), plot = Dai15E_Night_Flux, width =10, height =6, units = c("in"), dpi = 600) 

Dai15E_Afternoon_Flux<- ggplot(Dai15E_afternoon_Flux, aes(x = Dates, y = flux_alt)) + geom_point(size = 4, color = 'black', shape = 17) + 
  geom_vline(xintercept = (Dai15E_afternoon_Flux$Dates[Dai15E_afternoon_Flux$peak_status == "Peak"]), color = "blue") +
  ggtitle('Dai15E Day') +
  #scale_x_continuous(breaks = ~ seq(1, max(.x), 9)) + 
  scale_x_datetime(date_breaks = '5 days', date_labels = '%b-%d') +
  scale_y_continuous(breaks = ~ seq(0, max(.x), 0.01))
plot(Dai15E_Afternoon_Flux)
ggsave(paste0(prefix,"_Dai15E_Afternoon_Flux.pdf"), plot = Dai15E_Afternoon_Flux, width =10, height =6, units = c("in"), dpi = 600) 


###3D#####################################################################################################
common_datetimes_2<- intersect(dailyavg_3D_split[['Day']]$Date, river_area_split[['3D']]$Date)
for (i in seq_along(river_area_split[['3D']])) {
  river_area_split[['3D']] <- river_area_split[['3D']][river_area_split[['3D']]$Date %in% common_datetimes_2, ]
}

Dai3D_evening_Flux<- data.frame(dailyavg_3D_split[['Night']]$adj_sa * river_area_split[['3D']]$Area * wpo_dai3D_evening) %>%
  map_dfr(as.data.frame, show_col_types = FALSE) %>%
  rename_at(1,~'Flux') %>%
  mutate(Days = dailyavg_3D_split[['Day']]$Days) %>%
  rename_at(2,~'Days') %>%
  mutate(peak_status = dailyavg_3D_split[['Day']]$peak_status) %>%
  rename_at(3,~'peak_status') %>%
  mutate(Dates = dailyavg_3D_split[['Day']]$Date_time) %>%
  rename_at(4, ~'Dates') %>%
  mutate(flux_alt = Flux*((4)*12))
write.csv(Dai3D_evening_Flux, paste0(prefix,"_Dai3D_eveing_flux.csv"), row.names = T)


Dai3D_afternoon_Flux<- data.frame(dailyavg_3D_split[['Day']]$adj_sa * river_area_split[['3D']]$Area * wpo_dai3D_afternoon) %>%
  map_dfr(as.data.frame, show_col_types = FALSE) %>%
  rename_at(1,~'Flux') %>%
  mutate(Days = dailyavg_3D_split[['Day']]$Days) %>%
  rename_at(2,~'Days') %>%
  mutate(peak_status = dailyavg_3D_split[['Day']]$peak_status) %>%
  rename_at(3,~'peak_status') %>%
  mutate(Dates = dailyavg_3D_split[['Day']]$Date_time) %>%
  rename_at(4, ~'Dates') %>%
  mutate(flux_alt = Flux*((4)*12))
write.csv(Dai3D_afternoon_Flux, paste0(prefix,"_Dai3D_afternoon_flux.csv"), row.names = T)


Dai3D_Evening_Flux<- ggplot(Dai3D_evening_Flux, aes(x = Dates, y = flux_alt)) + geom_point(size = 4, color = 'darkgrey') + 
  geom_vline(xintercept = (Dai3D_evening_Flux$Dates[Dai3D_evening_Flux$peak_status == "Peak"]), color = "blue") +
  ggtitle('Dai3D Night') +
  #scale_x_continuous(breaks = ~ seq(1, max(.x), 9)) + 
  scale_x_datetime(date_breaks = '5 days', date_labels = '%b-%d') +
  scale_y_continuous(breaks = ~ seq(0, max(.x), 0.01))
plot(Dai3D_Evening_Flux)
ggsave(paste0(prefix,"_Dai3D_evening_Flux.png"), plot = Dai3D_Evening_Flux, width =10, height =6, units = c("in"), dpi = 600) 


Dai3D_Afternoon_Flux<- ggplot(Dai3D_afternoon_Flux, aes(x = Dates, y = flux_alt)) + geom_point(size = 4, shape =17) + 
  geom_vline(xintercept = (Dai3D_afternoon_Flux$Dates[Dai3D_afternoon_Flux$peak_status == "Peak"]), color = "blue") +
  ggtitle('Dai3D Day') +
  #scale_x_continuous(breaks = ~ seq(1, max(.x), 9)) +
  scale_x_datetime(date_breaks = '5 days', date_labels = '%b-%d') +
  scale_y_continuous(breaks = ~ seq(0, max(.x), 0.01))
plot(Dai3D_Afternoon_Flux)
ggsave(paste0(prefix,"_Dai3D_afternoon_Flux.png"), plot = Dai3D_Afternoon_Flux, width =10, height =6, units = c("in"), dpi = 600) 

###COMPARISONS######################################################################################################################################################################
evening_flux_compare<- rbind((mutate(Dai15E_evening_Flux, sentiment = factor("Dai15E"))),
                             (mutate(Dai3D_evening_Flux, sentiment = factor("Dai3D")))) %>%
  rename_at(6,~'Dai')
write.csv(evening_flux_compare, paste0(prefix,"_evening_flux_compare.csv"), row.names = T)


afternoon_flux_compare<- rbind((mutate(Dai15E_afternoon_Flux, sentiment = factor("Dai15E"))),
                               (mutate(Dai3D_afternoon_Flux, sentiment = factor("Dai3D")))) %>%
  rename_at(6,~'Dai')
write.csv(afternoon_flux_compare, paste0(prefix,"_afternoon_flux_compare.csv"), row.names = T)

Evening_Flux_Compare<- ggplot(evening_flux_compare, aes(x = Dates, y = flux_alt, color = Dai)) + geom_point(aes(shape=Dai),size= 4) + 
  geom_vline(xintercept = (Dai3D_evening_Flux$Dates[Dai3D_evening_Flux$peak_status == "Peak"]), color = "blue") +
  ggtitle('Night FLux') + scale_color_manual(values = c('darkgrey', 'black')) +
  #scale_x_continuous(breaks = ~ seq(1, max(.x), 1)) + 
  scale_x_datetime(date_breaks = '5 days', date_labels = '%b-%d') +
  scale_y_continuous(breaks = ~ seq(0, max(.x), 0.01))
plot(Evening_Flux_Compare)
ggsave(paste0(prefix,"_15E_night_comparison.pdf"), plot = Evening_Flux_Compare, width =10, height =6, units = c("in"), dpi = 600)


Afternoon_Flux_Compare<- ggplot(afternoon_flux_compare, aes(x = Dates, y = flux_alt, color = Dai)) + geom_point(aes(shape=Dai),size = 4) + 
  geom_vline(xintercept = (Dai3D_evening_Flux$Dates[Dai3D_evening_Flux$peak_status == "Peak"]), color = "blue") +
  ggtitle('Day FLux') + scale_color_manual(values = c('darkgrey', 'black')) +
  #scale_x_continuous(breaks = ~ seq(1, max(.x), 1)) + 
  scale_x_datetime(date_breaks = '5 days', date_labels = '%b-%d') +
  scale_y_continuous(breaks = ~ seq(0, max(.x), 0.01))
plot(Afternoon_Flux_Compare)
ggsave(paste0(prefix,"_15E_day_comparison.pdf"), plot = Afternoon_Flux_Compare, width =10, height =6, units = c("in"), dpi = 600)


Dai3D_TOD_flux_compare<- rbind((mutate(Dai3D_evening_Flux, sentiment = factor("Night"))),
                               (mutate(Dai3D_afternoon_Flux, sentiment = factor("Day")))) %>%
  rename_at(6,~'TOD')
write.csv(Dai3D_TOD_flux_compare, paste0(prefix,"_Dai3D_TOD_flux_compare.csv"), row.names = T)


Dai3D_TOD_Flux_Comparison<- ggplot(Dai3D_TOD_flux_compare, aes(x = Dates, y = flux_alt, color = TOD)) + geom_point(aes(shape=TOD),size = 4) +
  geom_vline(xintercept = (Dai3D_TOD_flux_compare$Dates[Dai3D_TOD_flux_compare$peak_status == "Peak"]), color = "blue") +
  ggtitle('Dai3 TOD Flux') + scale_color_manual(values = c('darkgrey', 'black')) +
  #scale_x_continuous(breaks = ~ seq(1, max(.x), 9)) +
  scale_x_datetime(date_breaks = '5 days', date_labels = '%b-%d') +
  scale_y_continuous(breaks = ~ seq(0, max(.x), 0.01))
plot(Dai3D_TOD_Flux_Comparison)
ggsave(paste0(prefix,"_Dai3D_TOD_Flux_Compare.png"), plot = Dai3D_TOD_Flux_Comparison, width =10, height =6, units = c("in"), dpi = 600) 

Dai15E_TOD_flux_compare<- rbind((mutate(Dai15E_evening_Flux, sentiment = factor("Night"))),
                                (mutate(Dai15E_afternoon_Flux, sentiment = factor("Day")))) %>%
  rename_at(6,~'TOD')
write.csv(Dai15E_TOD_flux_compare, paste0(prefix,"_Dai15E_TOD_flux_compare.csv"), row.names = T)


Dai15E_TOD_Flux_Comparison<- ggplot(Dai15E_TOD_flux_compare, aes(x = Dates, y = flux_alt, color = TOD)) + geom_point(aes(shape=TOD),size = 4) +
  geom_vline(xintercept = (Dai15E_TOD_flux_compare$Dates[Dai15E_TOD_flux_compare$peak_status == "Peak"]), color = "blue") +
  ggtitle('Dai15 TOD Flux') + scale_color_manual(values = c('darkgrey', 'black')) + #cale_shape_manual(values = c(1,2)) +
  scale_x_datetime(date_breaks = '5 days', date_labels = '%b-%d') +#scale_x_continuous(breaks = ~ seq(1, max(.x), 9)) + 
  scale_y_continuous(breaks = ~ seq(0, max(.x), 0.01))
plot(Dai15E_TOD_Flux_Comparison)
ggsave(paste0(prefix,"_Dai15E_TOD_Flux_Compare.png"), plot = Dai15E_TOD_Flux_Comparison, width =10, height =6, units = c("in"), dpi = 600) 


Flux_All<- rbind((mutate(Dai15E_TOD_flux_compare, sentiment = factor("Dai15E"))),
                 (mutate(Dai3D_TOD_flux_compare, sentiment = factor("Dai3D")))) %>% 
  rename_at(7, ~'Dai')
write.csv(Flux_All, paste0(prefix,"_Flux_All.csv"), row.names = T)

flux_comparison_All_facet<- ggplot(Flux_All, aes(x = Dates, y = flux_alt, color = TOD, shape = Dai)) + facet_grid(Dai ~ ., switch = 'y') + geom_point(size = 4) +
  geom_vline(xintercept = (Dai15E_TOD_flux_compare$Dates[Dai15E_TOD_flux_compare$peak_status == "Peak"]), color = "blue") +
  ggtitle('Both Dais FLux Day/Night') + scale_color_manual(values = c('darkgrey', 'black')) +
  #scale_x_continuous(breaks = ~ seq(1, max(.x), 1)) + 
  scale_x_datetime(date_breaks = '5 days', date_labels = '%b-%d') +
  scale_y_continuous(breaks = ~ seq(0, max(.x), 0.01))
plot(flux_comparison_All_facet)
ggsave(paste0(prefix,"_bothDai_TOD_comparison_facet.pdf"), plot = flux_comparison_All_facet, width =10, height =6, units = c("in"), dpi = 600)

flux_comparison_All<- ggplot(Flux_All, aes(x = Dates, y = flux_alt, color = TOD, shape = Dai)) + geom_point(size = 4) +
  geom_vline(xintercept = (Dai15E_TOD_flux_compare$Dates[Dai15E_TOD_flux_compare$peak_status == "Peak"]), color = "blue") +
  ggtitle('Both Dais FLux Day/Night') + scale_color_manual(values = c('darkgrey', 'black')) +
  scale_x_datetime(date_breaks = '5 days', date_labels = '%b-%d') +
  scale_y_continuous(breaks = ~ seq(0, max(.x), 0.01))
plot(flux_comparison_All)
ggsave(paste0(prefix,"_bothDai_TOD_comparison.pdf"), plot = flux_comparison_All, width =10, height =6, units = c("in"), dpi = 600)

#subtraction<- data.frame(Dai15E_TOD_flux_compare_2$Flux - Dai3D_TOD_flux_compare$Flux) %>%
#  rename_at(1, ~'Flux') %>%
#  mutate(Week = seq_along(Flux))
#subtraction$Week<- c(1:nrow(time))
#colnames(subtraction)[1]<- 'Flux'
#subtraction<- cbind(subtraction, TOD = Dai3D_TOD_flux_compare$TOD)
Dai3D_TOD_flux_compare$alt_dates<- as.Date(Dai3D_TOD_flux_compare$Dates)
Dai15E_TOD_flux_compare$alt_dates<- as.Date(Dai15E_TOD_flux_compare$Dates)
common_datetimes_3<- intersect(Dai3D_TOD_flux_compare$alt_dates, Dai15E_TOD_flux_compare$alt_dates)
for (i in seq_along(Dai15E_TOD_flux_compare)) {
  Dai15E_TOD_flux_compare_2 <- Dai15E_TOD_flux_compare[Dai15E_TOD_flux_compare$alt_dates %in% common_datetimes_3, ]
}

subtract<- (Dai15E_TOD_flux_compare_2$flux_alt - Dai3D_TOD_flux_compare$flux_alt)
subtraction<- cbind(Dai15E_TOD_flux_compare_2, subtract)
write.csv(subtraction, 'mortality_index.csv', row.names = T)
Mortality_Index<- ggplot(subtraction, aes(x = Dates, y = subtract, color = TOD)) + geom_point(size= 4) +
  geom_vline(xintercept = (Dai15E_TOD_flux_compare$Dates[Dai15E_TOD_flux_compare$peak_status == "Peak"]), color = "blue") +
  scale_x_datetime(date_breaks = '2 days', date_labels = '%b-%d') + scale_color_manual(values = c('darkgrey', 'black')) +
  scale_y_continuous(breaks = function(y) seq(floor(min(y, digits = 1)), ceiling(max(y, digits = 1)), by = 0.01)) +
  ggtitle('2022-23 Mortality Index') + xlab('Day') + ylab(Delta ~ 'Flux') +
  scale_fill_discrete(name= 'Day/Night') +
  geom_hline(yintercept = 0, color= 'blue', size= 2, alpha= .4) 
plot(Mortality_Index)
ggsave(paste0(prefix,"_2022-23 Mortality_Index.png"), plot = Mortality_Index, width =10, height =6, units = c("in"), dpi = 600)

###CDF########################################################################################################################################################################################################################

dai15E_flux_split<- split(Dai15E_TOD_flux_compare, Dai15E_TOD_flux_compare$TOD)

sum15E<- sum(dai15E_flux_split[['Day']]$Flux) #creates a factor that is the sum of the linear Sv data
div15E<- (dai15E_flux_split[['Day']]$Flux/sum15E) #Divides the each point of the linear Sv data by the sum pf the linear Sv data
CDF15E<- cumsum(div15E) #creates a factor of the cumulative sum of each linear Sv data point after it is divided by the total sum
CDF15E<- data.frame(CDF15E) #turns the sumulative sum data into a data frame
dai15E_flux_split[['Day']] <- cbind(dai15E_flux_split[['Day']], CDF15E) #binds the cumulative sum data to the exports_15E data frame

ggplot(dai15E_flux_split[['Day']], aes(x = Week, y = CDF15E)) +  #creates the continuous density function plot for Dai15
  annotate("segment", x= 1, xend = 1, y=-Inf, yen=Inf, color = "blue", alpha = 1) +
  annotate("segment", x= 6, xend = 6, y=-Inf, yen=Inf, color = "blue", alpha = 1) +
  geom_point(size = 4) + 
  labs(y = "CDF", x = "Week") + ggtitle("Day Flux CDF Dai 15E") +
  scale_x_continuous(breaks = ~ seq(1, max(.x), 1)) + scale_y_continuous(breaks = ~ seq(0, max(.x), .1)) +
  theme(axis.text.x = element_text())

sum15E<- sum(dai15E_flux_split[['Night']]$Flux) #creates a factor that is the sum of the linear Sv data
div15E<- (dai15E_flux_split[['Night']]$Flux/sum15E) #Divides the each point of the linear Sv data by the sum pf the linear Sv data
CDF15E<- cumsum(div15E) #creates a factor of the cumulative sum of each linear Sv data point after it is divided by the total sum
CDF15E<- data.frame(CDF15E) #turns the sumulative sum data into a data frame
dai15E_flux_split[['Night']] <- cbind(dai15E_flux_split[['Night']], CDF15E) #binds the cumulative sum data to the exports_15E data frame

ggplot(dai15E_flux_split[['Night']], aes(x = Week, y = CDF15E)) +  #creates the continuous density function plot for Dai15
  annotate("segment", x= 1, xend = 1, y=-Inf, yen=Inf, color = "blue", alpha = 1) +
  annotate("segment", x= 6, xend = 6, y=-Inf, yen=Inf, color = "blue", alpha = 1) +
  geom_point(size = 4) + 
  labs(y = "CDF", x = "Week") + ggtitle("Night Flux CDF Dai 15E") +
  scale_x_continuous(breaks = ~ seq(1, max(.x), 1)) + scale_y_continuous(breaks = ~ seq(0, max(.x), .1)) +
  theme(axis.text.x = element_text())

dai3D_flux_split<- split(Dai3D_TOD_flux_compare, Dai3D_TOD_flux_compare$TOD)

sum3D<- sum(dai3D_flux_split[['Day']]$Flux) #creates a factor that is the sum of the linear Sv data
div3D<- (dai3D_flux_split[['Day']]$Flux/sum3D) #Divides the each point of the linear Sv data by the sum pf the linear Sv data
CDF3D<- cumsum(div3D) #creates a factor of the cumulative sum of each linear Sv data point after it is divided by the total sum
CDF3D<- data.frame(CDF3D) #turns the sumulative sum data into a data frame
dai3D_flux_split[['Day']] <- cbind(dai3D_flux_split[['Day']], CDF3D) #binds the cumulative sum data to the exports_3D data frame

ggplot(dai3D_flux_split[['Day']], aes(x = Week, y = CDF3D)) +  #creates the continuous density function plot for Dai15
  annotate("segment", x= 1, xend = 1, y=-Inf, yen=Inf, color = "blue", alpha = 1) +
  annotate("segment", x= 6, xend = 6, y=-Inf, yen=Inf, color = "blue", alpha = 1) +
  geom_point(size = 4) + 
  labs(y = "CDF", x = "Week") + ggtitle("Day Flux CDF Dai 3D") +
  scale_x_continuous(breaks = ~ seq(1, max(.x), 1)) + scale_y_continuous(breaks = ~ seq(0, max(.x), .1)) +
  theme(axis.text.x = element_text())

sum3D<- sum(dai3D_flux_split[['Night']]$Flux) #creates a factor that is the sum of the linear Sv data
div3D<- (dai3D_flux_split[['Night']]$Flux/sum3D) #Divides the each point of the linear Sv data by the sum pf the linear Sv data
CDF3D<- cumsum(div3D) #creates a factor of the cumulative sum of each linear Sv data point after it is divided by the total sum
CDF3D<- data.frame(CDF3D) #turns the sumulative sum data into a data frame
dai3D_flux_split[['Night']] <- cbind(dai3D_flux_split[['Night']], CDF3D) #binds the cumulative sum data to the exports_3D data frame

ggplot(dai3D_flux_split[['Night']], aes(x = Week, y = CDF3D)) +  #creates the continuous density function plot for Dai15
  annotate("segment", x= 1, xend = 1, y=-Inf, yen=Inf, color = "blue", alpha = 1) +
  annotate("segment", x= 6, xend = 6, y=-Inf, yen=Inf, color = "blue", alpha = 1) +
  geom_point(size = 4) + 
  labs(y = "CDF", x = "Week") + ggtitle("Night Flux CDF Dai 3D") +
  scale_x_continuous(breaks = ~ seq(1, max(.x), 1)) + scale_y_continuous(breaks = ~ seq(0, max(.x), .1)) +
  theme(axis.text.x = element_text())




save.image(paste0(prefix,"_TS_Flux_Calc_Final.RData"))
{Sys.sleep(5)}
message("Finished running scripts, data output successful, leahaey!")
{Sys.sleep(5)}