#NecessaryPackages##################################
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
{
#2020-21#################################
##DirectoryScripting########################
input_all_data<- "Z:/fishproj/Cambodia Dai project/Analytic/Analytic Products/TS2020_Data_Exports/"
full_moon_dates<- as.Date(c('2020-11-30', '2020-12-30', '2021-01-29'))
input_transect_data = "Z:/fishproj/Cambodia Dai project/Analytic/Analytic Products/TS2021-22_Data_Exports/Transects/"
data_analysis_output = "Z:/fishproj/Cambodia Dai project/Papers/Analysis Paper/Pics, Plot, Figures/2020-21/"
##Transects_and_Weighted_Averages########################
setwd(input_transect_data)
Dai3D_evening_allBNR<- list.files(path = input_transect_data,                               #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                                  pattern = "^Dai3D_ABC_10mbin_\\d{8}_fullwatercolumn_evening_BNR*.*csv", full.names = TRUE) %>%
  map_dfr(read_csv) %>%
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


Dai15E_afternoon_allBNR<- list.files(path = input_transect_data,                               #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                                     pattern = "^Dai15E_ABC_10mbin_\\d{8}_fullwatercolumn_afternoon_BNR*.*csv", full.names = TRUE) %>%
  map_dfr(read_csv) %>%
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
setwd(data_analysis_output)
write.csv(Dai3D_afternoon_allBNR, 'Dai3D_afternoon_allBNR.csv', row.names = T)
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

setwd(input_all_data)
rm(exports_15E)
{
exports_15E<- list.files(path = input_all_data,                               #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                         pattern = "^15E.*FullWaterColumn_100m.sv.csv", full.names = TRUE) %>% 
  map_dfr(read_csv) %>%
  filter(Layer !=0) %>%                                                       #eliminates any data layers above layer 1 that may have been accidentally exported 
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Date_M),"%Y%m%d"), Time_M, sep=" "),format = "%Y-%m-%d %H:%M", tz="Asia/Bangkok")) %>%#combines the time and date columns into one column for proper time teries analysis
  mutate(sa_Ln = 10^(Area_Backscatter_Strength/10)) %>%
  mutate(DayNight = if_else(as.numeric(substring(Time_M, 1, 2)) >= 18 | as.numeric(substring(Time_M, 1, 2)) < 06, 'Night', "Day")) %>%
  mutate(TOD = case_when(endsWith(DayNight, 'Night') ~ 2, endsWith(DayNight, 'Day') ~ 1))


daily_avg_15E<- data.frame(aggregate(sa_Ln ~ cut(Date_time, "24 hour"), exports_15E, mean))
colnames(daily_avg_15E)[1]<- 'Date_time'#data.frame(aggregate(sa_Ln ~ DayNight + Week, exports_15E, mean))
#daily_avg_15E$Date_time<- (daily_avg_15E_dates$Date_time)
daily_avg_15E$Date_time<- as.POSIXct(daily_avg_15E$Date_time, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")
daily_avg_15E$sa_log = 10*log10(daily_avg_15E$sa_Ln) #
#full_moon_dates<- as.Date(c('2022-12-08', '2023-01-07'))
daily_avg_15E$date_column<- as.Date(daily_avg_15E$Date_time, format = "%Y-%m-%d")
daily_avg_15E$peak_status <- ifelse(daily_avg_15E$date_column %in% full_moon_dates, "Peak", "Off-Peak")

daily_avg_15E$Days <- as.numeric(difftime(daily_avg_15E$date_column, min(daily_avg_15E$date_column), units = "days")) + 1
dailyavg_15E_split<- daily_avg_15E#split(daily_avg_15E, daily_avg_15E$DayNight)

depth_15E<- list.files(path = input_all_data,     #identifies all .csv files associated with Dai15 depth measurements and compiles them into one data frame
                       pattern = "*Bottom Line 15E.line.csv", full.names = TRUE) %>% 
  map_dfr(read_csv) %>%                                            # Store all files in list
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Ping_date),"%Y-%m-%d"), Ping_time, sep=" "), format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")) # combines the date and time columns into one date/time column
setwd(input_transect_data)
#river_area_evening<- read.csv('total_slice_area_weekly_2022-23.csv')
#river_area_split<- split(river_area_evening, river_area_evening$Dai)

desiredGroupingUnit15E = cut(depth_15E$Date_time, breaks = "24 hour") # places the depth data into groups organised by 24 hours
aggregate(depth_15E$Depth, by = list(desiredGroupingUnit15E), FUN = mean) # creates a value frame based on the means of the grouped data
avg_depth_15E<- data.frame(aggregate(depth_15E$Depth, by = list(desiredGroupingUnit15E), FUN = mean)) # creates a data frame out of the means of the data
avg_depth_15E$Date_POSIXct <- as.POSIXct(avg_depth_15E$Group.1, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok") # renames the the date/time column and ensures it is the correct format  
avg_depth_15E$day <- 1:nrow(avg_depth_15E)  #adds a day number column
avg_depth_15E$AMPM<- as.factor(format(avg_depth_15E$Date_POSIXct, "%p")) #creates a factor column called AM/PM based on the date/time column


Dai15_max_area<- (4335.252)
Dai15_area_loss<- (15.437)

dates <- as.Date(avg_depth_15E$Date_POSIXct)

my_values_area15 <- numeric(length(dates))
my_values_area15[1] <- Dai15_max_area



for (i in 2:length(dates)) {
  days_diff <- as.integer(dates[i] - dates[i-1])
  my_values_area15[i] <- my_values_area15[i-1] - Dai15_area_loss * days_diff
}

area_avg_15E<- cbind.data.frame(avg_depth_15E$Date_POSIXct, avg_depth_15E$day, my_values_area15) 
colnames(area_avg_15E)[1] <- 'Date' 
colnames(area_avg_15E)[2] <- 'Day'
colnames(area_avg_15E)[3] <- 'Area'
area_avg_15E$Dai<- paste0('15E')
# common_datetimes_2<- intersect(dailyavg_15E_split[['Night']]$Date_time, area_avg_15E$Date)
# for (i in seq_along(area_avg_15E)) {
#   area_avg_15E <- area_avg_15E[area_avg_15E$Date %in% common_datetimes_2, ]
# }
# common_datetimes<- intersect(dailyavg_15E_split[['Day']]$Date_time, dailyavg_15E_split[['Night']]$Date_time)
# for (i in seq_along(dailyavg_15E_split)) {
#   dailyavg_15E_split[[i]] <- dailyavg_15E_split[[i]][dailyavg_15E_split[[i]]$Date_time %in% common_datetimes, ]
# }

wpo_dai15E<- ((wpo_dai15E_afternoon + wpo_dai15E_evening)/2)

Dai15E_Flux<- data.frame(dailyavg_15E_split$sa_Ln * area_avg_15E$Area * wpo_dai15E) %>%
  map_dfr(as.data.frame) %>%
  rename_at(1,~'Flux') %>%
  mutate(Days = dailyavg_15E_split$Days) %>%
  rename_at(2,~'Days') %>%
  mutate(peak_status = dailyavg_15E_split$peak_status) %>%
  rename_at(3,~'peak_status') %>%
  mutate(Dates = dailyavg_15E_split$Date_time) %>%
  rename_at(4, ~'Dates') %>%
  mutate(flux_alt = Flux*((4)))

dai15E_flux_split<- Dai15E_Flux#split(Dai15E_TOD_flux_compare, Dai15E_TOD_flux_compare$TOD)

sum15E<- sum(dai15E_flux_split$Flux) #creates a factor that is the sum of the linear Sv data
div15E<- (dai15E_flux_split$Flux/sum15E) #Divides the each point of the linear Sv data by the sum pf the linear Sv data
CDF15E<- cumsum(div15E) #creates a factor of the cumulative sum of each linear Sv data point after it is divided by the total sum
CDF15E<- data.frame(CDF15E) #turns the sumulative sum data into a data frame
dai15E_flux_split <- cbind(dai15E_flux_split, CDF15E) #binds the cumulative sum data to the exports_15E data frame
}

setwd(input_all_data)
{
rm(exports_3D)
exports_3D<- list.files(path = input_all_data,                               #identifies all .csv files associated with Dai3 full water column Sv measurements and compiles them into one data frame
                        pattern = "^3C.*FullWaterColumn_100m.sv.csv", full.names = TRUE) %>% 
  map_dfr(read_csv) %>%
  filter(Layer !=0) %>%                                                       #eliminates any data layers above layer 1 that may have been accidentally exported 
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Date_M),"%Y%m%d"), Time_M, sep=" "),format = "%Y-%m-%d %H:%M", tz="Asia/Bangkok")) %>%#combines the time and date columns into one column for proper time teries analysis
  mutate(sa_Ln = 10^(Area_Backscatter_Strength/10)) %>%
  mutate(DayNight = if_else(as.numeric(substring(Time_M, 1, 2)) >= 18 | as.numeric(substring(Time_M, 1, 2)) < 06, 'Night', "Day")) %>%
  mutate(TOD = case_when(endsWith(DayNight, 'Night') ~ 2, endsWith(DayNight, 'Day') ~ 1))


daily_avg_3D<- data.frame(aggregate(sa_Ln ~ cut(Date_time, "24 hour"), exports_3D, mean))
colnames(daily_avg_3D)[1]<- 'Date_time'#data.frame(aggregate(sa_Ln ~ DayNight + Week, exports_3D, mean))
#daily_avg_3D$Date_time<- (daily_avg_3D_dates$Date_time)
daily_avg_3D$Date_time<- as.POSIXct(daily_avg_3D$Date_time, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")
daily_avg_3D$sa_log = 10*log10(daily_avg_3D$sa_Ln) #
#full_moon_dates<- as.Date(c('2022-12-08', '2023-01-07'))
daily_avg_3D$date_column<- as.Date(daily_avg_3D$Date_time, format = "%Y-%m-%d")
daily_avg_3D$peak_status <- ifelse(daily_avg_3D$date_column %in% full_moon_dates, "Peak", "Off-Peak")

daily_avg_3D$Days <- as.numeric(difftime(daily_avg_3D$date_column, min(daily_avg_3D$date_column), units = "days")) + 1
dailyavg_3D_split<- daily_avg_3D#split(daily_avg_3D, daily_avg_3D$DayNight)

depth_3D<- list.files(path = input_all_data,     #identifies all .csv files associated with Dai3 depth measurements and compiles them into one data frame
                      pattern = "*Bottom Line 3C.line.csv", full.names = TRUE) %>% 
  map_dfr(read_csv) %>%                                            # Store all files in list
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Ping_date),"%Y-%m-%d"), Ping_time, sep=" "), format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")) # combines the date and time columns into one date/time column
setwd(input_transect_data)
#river_area_evening<- read.csv('total_slice_area_weekly_2022-23.csv')
#river_area_split<- split(river_area_evening, river_area_evening$Dai)

desiredGroupingUnit3D = cut(depth_3D$Date_time, breaks = "24 hour") # places the depth data into groups organised by 24 hours
aggregate(depth_3D$Depth, by = list(desiredGroupingUnit3D), FUN = mean) # creates a value frame based on the means of the grouped data
avg_depth_3D<- data.frame(aggregate(depth_3D$Depth, by = list(desiredGroupingUnit3D), FUN = mean)) # creates a data frame out of the means of the data
avg_depth_3D$Date_POSIXct <- as.POSIXct(avg_depth_3D$Group.1, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok") # renames the the date/time column and ensures it is the correct format  
avg_depth_3D$day <- 1:nrow(avg_depth_3D)  #adds a day number column
avg_depth_3D$AMPM<- as.factor(format(avg_depth_3D$Date_POSIXct, "%p")) #creates a factor column called AM/PM based on the date/time column


Dai3_max_area<- (4508.351)
Dai3_area_loss<- (13.812)

dates <- as.Date(avg_depth_3D$Date_POSIXct)

my_values_area3 <- numeric(length(dates))
my_values_area3[1] <- Dai3_max_area



for (i in 2:length(dates)) {
  days_diff <- as.integer(dates[i] - dates[i-1])
  my_values_area3[i] <- my_values_area3[i-1] - Dai3_area_loss * days_diff
}

area_avg_3D<- cbind.data.frame(avg_depth_3D$Date_POSIXct, avg_depth_3D$day, my_values_area3) 
colnames(area_avg_3D)[1] <- 'Date' 
colnames(area_avg_3D)[2] <- 'Day'
colnames(area_avg_3D)[3] <- 'Area'
area_avg_3D$Dai<- paste0('3D')
# common_datetimes_2<- intersect(dailyavg_3D_split[['Night']]$Date_time, area_avg_3D$Date)
# for (i in seq_along(area_avg_3D)) {
#   area_avg_3D <- area_avg_3D[area_avg_3D$Date %in% common_datetimes_2, ]
# }
# common_datetimes<- intersect(dailyavg_3D_split[['Day']]$Date_time, dailyavg_3D_split[['Night']]$Date_time)
# for (i in seq_along(dailyavg_3D_split)) {
#   dailyavg_3D_split[[i]] <- dailyavg_3D_split[[i]][dailyavg_3D_split[[i]]$Date_time %in% common_datetimes, ]
# }


wpo_dai3D<- ((wpo_dai3D_afternoon + wpo_dai3D_evening)/2)

Dai3D_Flux<- data.frame(dailyavg_3D_split$sa_Ln * area_avg_3D$Area * wpo_dai3D_evening) %>%
  map_dfr(as.data.frame) %>%
  rename_at(1,~'Flux') %>%
  mutate(Days = dailyavg_3D_split$Days) %>%
  rename_at(2,~'Days') %>%
  mutate(peak_status = dailyavg_3D_split$peak_status) %>%
  rename_at(3,~'peak_status') %>%
  mutate(Dates = dailyavg_3D_split$Date_time) %>%
  rename_at(4, ~'Dates') %>%
  mutate(flux_alt = Flux*((4)*12))



setwd(data_analysis_output)


dai3D_flux_split<- Dai3D_Flux#split(Dai3D_TOD_flux_compare, Dai3D_TOD_flux_compare$TOD)

sum3D<- sum(dai3D_flux_split$Flux) #creates a factor that is the sum of the linear Sv data
div3D<- (dai3D_flux_split$Flux/sum3D) #Divides the each point of the linear Sv data by the sum pf the linear Sv data
CDF3D<- cumsum(div3D) #creates a factor of the cumulative sum of each linear Sv data point after it is divided by the total sum
CDF3D<- data.frame(CDF3D) #turns the sumulative sum data into a data frame
dai3D_flux_split <- cbind(dai3D_flux_split, CDF3D) #binds the cumulative sum data to the exports_3D data frame
}
##CDF_Creation####################################################
setwd(data_analysis_output)
library(segmented)
attach(dai15E_flux_split)
y<- dai15E_flux_split$CDF15E
x<- dai15E_flux_split$Dates
pw_reg_15E_2020<- data.frame(x = x, y = y)
out.lm <- lm(y ~ x, data = pw_reg_15E_2020)
br <- as.POSIXct(c('2020-12-20 18:00:00','2020-12-28 18:00:00', '2021-01-15 18:00:00', '2021-01-23 18:00:00'))
o <- segmented(out.lm, seg.Z = ~x, psi = list(x = br))
dat2 = data.frame(x = pw_reg_15E_2020$x, y = predict(o))

pw_reg_15E_2020_21<- ggplot(pw_reg_15E_2020, aes(x = x, y = y)) +
geom_point() +
geom_line(data = dat2, color = 'blue', linewidth = 2) +
scale_x_datetime(date_breaks = '14 days', date_labels = '%b-%d') +
scale_y_continuous(breaks = ~ seq(0, max(.x), .1)) +
annotate('segment', x= as.POSIXct("2021-01-29"), xend= as.POSIXct("2021-01-29"), yend=-Inf, y=Inf, colour = "black", alpha = 1, linewidth = 1) +
annotate('segment', x= as.POSIXct("2020-11-30"), xend= as.POSIXct("2020-11-30"), yend=-Inf, y=Inf, colour = "black", alpha = 1, linewidth = 1) +
annotate('rect', xmin = c(as.POSIXct('2020-12-20 13:14:11'), as.POSIXct('2021-01-14 12:22:15')), xmax = c(as.POSIXct('2020-12-26 15:35:47'), as.POSIXct('2021-01-23 21:34:05')), 
         ymin = c(0.19047932, 0.72899546), ymax = c(0.57782704, 0.96324831), alpha = 0.2, color = 'red', fill = 'red') +
geom_vline(xintercept = (dai15E_flux_split$Dates[dai15E_flux_split$peak_status == "Peak"]), color = "black", linewidth = 1) +
geom_vline(xintercept = o$psi[, 'Est.'], color = 'red') +
geom_point(size = 2) + theme_bw() + 
labs(x = "Date") + ylab(~paste('CDF', Phi)) + 
theme(axis.text.x = element_text(color = 'black', size = 12), 
      axis.title = element_blank(), 
      axis.text.y = element_text(color = 'black', size = 12),
      axis.line = element_line(color = 'black'),
      panel.border = element_rect(colour = "black", fill=NA),
      panel.grid.minor.y = element_blank())
plot(pw_reg_15E_2020_21)
as.POSIXct(o$psi[, 'Est.'])
ggsave("2020-21_Dai15E_Flux_CDF_piecewise_regression.png", plot = pw_reg_15E_2020_21, width =10, height =6, units = c("in"), dpi = 600) 

y<- dai3D_flux_split$CDF3D
x<- dai3D_flux_split$Dates
br<- as.POSIXct(c('2020-12-15 18:00:00' ,'2020-12-30 18:00:00', '2021-01-18 18:00:00'))
pw_reg_3D_2020<- data.frame(x = x, y = y)
out.lm <- lm(y ~ x, data = pw_reg_3D_2020)

p <- segmented(out.lm, seg.Z = ~x, psi = list(x = br))
dat3 = data.frame(x = pw_reg_3D_2020$x, y = predict(p))

pw_reg_3C_2020_21<- ggplot(pw_reg_3D_2020, aes(x = x, y = y)) +
geom_point() +
geom_line(data = dat3, color = 'blue', linewidth = 2) +
#geom_line(data = dat3, color = 'blue') +
scale_x_datetime(date_breaks = '14 days', date_labels = '%b-%d') +
scale_y_continuous(breaks = ~ seq(0, max(.x), .1)) +
annotate('segment', x= as.POSIXct("2021-01-29"), xend= as.POSIXct("2021-01-29"), yend=-Inf, y=Inf, colour = "black", alpha = 1, linewidth = 1) +
annotate('segment', x= as.POSIXct("2020-11-30"), xend= as.POSIXct("2020-11-30"), yend=-Inf, y=Inf, colour = "black", alpha = 1, linewidth = 1) +
annotate('rect', xmin = c(as.POSIXct('2020-12-21 00:37:26'), as.POSIXct('2021-01-14 21:28:20')), xmax = c(as.POSIXct('2020-12-28 08:29:26'), as.POSIXct('2021-01-27 05:00:00')), 
         ymin = c(0.41738251, 0.74943178), ymax = c(0.57583120, 1.0), alpha = 0.2, color = 'red', fill = 'red') +
geom_vline(xintercept = (dai15E_flux_split$Dates[dai15E_flux_split$peak_status == "Peak"]), color = "black", linewidth = 1, alpha = 1) +
geom_point(size = 2) + theme_bw() + geom_vline(xintercept = p$psi[, 'Est.'], color = 'red') +
labs(x = "Date") + ylab(~paste('CDF', Phi)) +
theme(axis.text.x = element_text(color = 'black', size = 12), 
      axis.title = element_blank(), 
      axis.text.y = element_text(color = 'black', size = 12),
      axis.line = element_line(color = 'black'),
      panel.border = element_rect(colour = "black", fill=NA),
      panel.grid.minor.y = element_blank())
plot(pw_reg_3C_2020_21)
as.POSIXct(p$psi[, 'Est.'])
ggsave("2020-21_Dai3C_Flux_CDF_piecewise_regression.png", plot = pw_reg_3C_2020_21, width =10, height =6, units = c("in"), dpi = 600) 

#2021-22###########################################
##DirectoryScripting########################
input_all_data = "Z:/fishproj/Cambodia Dai project/Analytic/Analytic Products/TS2021-22_Data_Exports/"
full_moon_dates<- as.Date(c('2021-11-19', '2021-12-19', '2022-01-18', '2022-02-16'))
input_transect_data = "Z:/fishproj/Cambodia Dai project/Analytic/Analytic Products/TS2021-22_Data_Exports/Transects/"
data_analysis_output = "Z:/fishproj/Cambodia Dai project/Papers/Analysis Paper/Pics, Plot, Figures/2021-22/"
##Transects_and_Weighted_Averages########################
setwd(input_transect_data)
Dai3D_evening_allBNR<- list.files(path = input_transect_data,                               #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                                  pattern = "^Dai3D_ABC_10mbin_\\d{8}_fullwatercolumn_evening_BNR*.*csv", full.names = TRUE) %>%
  map_dfr(read_csv) %>%
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

Dai15E_afternoon_allBNR<- list.files(path = input_transect_data,                               #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                                     pattern = "^Dai15E_ABC_10mbin_\\d{8}_fullwatercolumn_afternoon_BNR*.*csv", full.names = TRUE) %>%
  map_dfr(read_csv) %>%
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
setwd(data_analysis_output)
write.csv(Dai3D_afternoon_allBNR, 'Dai3D_afternoon_allBNR.csv', row.names = T)
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

setwd(input_all_data)
{
rm(exports_15E)
exports_15E<- list.files(path = input_all_data,                               #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                         pattern = "^15E.*FullWaterColumn_100m.sv.csv", full.names = TRUE) %>% 
  map_dfr(read_csv) %>%
  filter(Layer !=0) %>%                                                       #eliminates any data layers above layer 1 that may have been accidentally exported 
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Date_M),"%Y%m%d"), Time_M, sep=" "),format = "%Y-%m-%d %H:%M", tz="Asia/Bangkok")) %>%#combines the time and date columns into one column for proper time teries analysis
  mutate(sa_Ln = 10^(Area_Backscatter_Strength/10)) %>%
  mutate(DayNight = if_else(as.numeric(substring(Time_M, 1, 2)) >= 18 | as.numeric(substring(Time_M, 1, 2)) < 06, 'Night', "Day")) %>%
  mutate(TOD = case_when(endsWith(DayNight, 'Night') ~ 2, endsWith(DayNight, 'Day') ~ 1))


daily_avg_15E<- data.frame(aggregate(sa_Ln ~ cut(Date_time, "24 hour"), exports_15E, mean))
colnames(daily_avg_15E)[1]<- 'Date_time'#data.frame(aggregate(sa_Ln ~ DayNight + Week, exports_15E, mean))
#daily_avg_15E$Date_time<- (daily_avg_15E_dates$Date_time)
daily_avg_15E$Date_time<- as.POSIXct(daily_avg_15E$Date_time, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")
daily_avg_15E$sa_log = 10*log10(daily_avg_15E$sa_Ln) #
#full_moon_dates<- as.Date(c('2022-12-08', '2023-01-07'))
daily_avg_15E$date_column<- as.Date(daily_avg_15E$Date_time, format = "%Y-%m-%d")
daily_avg_15E$peak_status <- ifelse(daily_avg_15E$date_column %in% full_moon_dates, "Peak", "Off-Peak")

daily_avg_15E$Days <- as.numeric(difftime(daily_avg_15E$date_column, min(daily_avg_15E$date_column), units = "days")) + 1
dailyavg_15E_split<- daily_avg_15E#split(daily_avg_15E, daily_avg_15E$DayNight)

depth_15E<- list.files(path = input_all_data,     #identifies all .csv files associated with Dai15 depth measurements and compiles them into one data frame
                       pattern = "*Bottom Line 15E.line.csv", full.names = TRUE) %>% 
  map_dfr(read_csv) %>%                                            # Store all files in list
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Ping_date),"%Y-%m-%d"), Ping_time, sep=" "), format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")) # combines the date and time columns into one date/time column
setwd(input_transect_data)
#river_area_evening<- read.csv('total_slice_area_weekly_2022-23.csv')
#river_area_split<- split(river_area_evening, river_area_evening$Dai)

desiredGroupingUnit15E = cut(depth_15E$Date_time, breaks = "24 hour") # places the depth data into groups organised by 24 hours
aggregate(depth_15E$Depth, by = list(desiredGroupingUnit15E), FUN = mean) # creates a value frame based on the means of the grouped data
avg_depth_15E<- data.frame(aggregate(depth_15E$Depth, by = list(desiredGroupingUnit15E), FUN = mean)) # creates a data frame out of the means of the data
avg_depth_15E$Date_POSIXct <- as.POSIXct(avg_depth_15E$Group.1, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok") # renames the the date/time column and ensures it is the correct format  
avg_depth_15E$day <- 1:nrow(avg_depth_15E)  #adds a day number column
avg_depth_15E$AMPM<- as.factor(format(avg_depth_15E$Date_POSIXct, "%p")) #creates a factor column called AM/PM based on the date/time column


Dai15_max_area<- (4335.252)
Dai15_area_loss<- (15.437)

dates <- as.Date(avg_depth_15E$Date_POSIXct)

my_values_area15 <- numeric(length(dates))
my_values_area15[1] <- Dai15_max_area



for (i in 2:length(dates)) {
  days_diff <- as.integer(dates[i] - dates[i-1])
  my_values_area15[i] <- my_values_area15[i-1] - Dai15_area_loss * days_diff
}

area_avg_15E<- cbind.data.frame(avg_depth_15E$Date_POSIXct, avg_depth_15E$day, my_values_area15) 
colnames(area_avg_15E)[1] <- 'Date' 
colnames(area_avg_15E)[2] <- 'Day'
colnames(area_avg_15E)[3] <- 'Area'
area_avg_15E$Dai<- paste0('15E')
# common_datetimes_2<- intersect(dailyavg_15E_split[['Night']]$Date_time, area_avg_15E$Date)
# for (i in seq_along(area_avg_15E)) {
#   area_avg_15E <- area_avg_15E[area_avg_15E$Date %in% common_datetimes_2, ]
# }
# common_datetimes<- intersect(dailyavg_15E_split[['Day']]$Date_time, dailyavg_15E_split[['Night']]$Date_time)
# for (i in seq_along(dailyavg_15E_split)) {
#   dailyavg_15E_split[[i]] <- dailyavg_15E_split[[i]][dailyavg_15E_split[[i]]$Date_time %in% common_datetimes, ]
# }

wpo_dai15E<- ((wpo_dai15E_afternoon + wpo_dai15E_evening)/2)

Dai15E_Flux<- data.frame(dailyavg_15E_split$sa_Ln * area_avg_15E$Area * wpo_dai15E) %>%
  map_dfr(as.data.frame) %>%
  rename_at(1,~'Flux') %>%
  mutate(Days = dailyavg_15E_split$Days) %>%
  rename_at(2,~'Days') %>%
  mutate(peak_status = dailyavg_15E_split$peak_status) %>%
  rename_at(3,~'peak_status') %>%
  mutate(Dates = dailyavg_15E_split$Date_time) %>%
  rename_at(4, ~'Dates') %>%
  mutate(flux_alt = Flux*((4)))

dai15E_flux_split<- Dai15E_Flux#split(Dai15E_TOD_flux_compare, Dai15E_TOD_flux_compare$TOD)

sum15E<- sum(dai15E_flux_split$Flux) #creates a factor that is the sum of the linear Sv data
div15E<- (dai15E_flux_split$Flux/sum15E) #Divides the each point of the linear Sv data by the sum pf the linear Sv data
CDF15E<- cumsum(div15E) #creates a factor of the cumulative sum of each linear Sv data point after it is divided by the total sum
CDF15E<- data.frame(CDF15E) #turns the sumulative sum data into a data frame
dai15E_flux_split <- cbind(dai15E_flux_split, CDF15E) #binds the cumulative sum data to the exports_15E data frame
}

setwd(input_all_data)
{
rm(exports_3D)
exports_3D<- list.files(path = input_all_data,                               #identifies all .csv files associated with Dai3 full water column Sv measurements and compiles them into one data frame
                        pattern = "^3D.*FullWaterColumn_100m.sv.csv", full.names = TRUE) %>% 
  map_dfr(read_csv) %>%
  filter(Layer !=0) %>%                                                       #eliminates any data layers above layer 1 that may have been accidentally exported 
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Date_M),"%Y%m%d"), Time_M, sep=" "),format = "%Y-%m-%d %H:%M", tz="Asia/Bangkok")) %>%#combines the time and date columns into one column for proper time teries analysis
  mutate(sa_Ln = 10^(Area_Backscatter_Strength/10)) %>%
  mutate(DayNight = if_else(as.numeric(substring(Time_M, 1, 2)) >= 18 | as.numeric(substring(Time_M, 1, 2)) < 06, 'Night', "Day")) %>%
  mutate(TOD = case_when(endsWith(DayNight, 'Night') ~ 2, endsWith(DayNight, 'Day') ~ 1))


daily_avg_3D<- data.frame(aggregate(sa_Ln ~ cut(Date_time, "24 hour"), exports_3D, mean))
colnames(daily_avg_3D)[1]<- 'Date_time'#data.frame(aggregate(sa_Ln ~ DayNight + Week, exports_3D, mean))
#daily_avg_3D$Date_time<- (daily_avg_3D_dates$Date_time)
daily_avg_3D$Date_time<- as.POSIXct(daily_avg_3D$Date_time, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")
daily_avg_3D$sa_log = 10*log10(daily_avg_3D$sa_Ln) #
#full_moon_dates<- as.Date(c('2022-12-08', '2023-01-07'))
daily_avg_3D$date_column<- as.Date(daily_avg_3D$Date_time, format = "%Y-%m-%d")
daily_avg_3D$peak_status <- ifelse(daily_avg_3D$date_column %in% full_moon_dates, "Peak", "Off-Peak")

daily_avg_3D$Days <- as.numeric(difftime(daily_avg_3D$date_column, min(daily_avg_3D$date_column), units = "days")) + 1
dailyavg_3D_split<- daily_avg_3D#split(daily_avg_3D, daily_avg_3D$DayNight)

depth_3D<- list.files(path = input_all_data,     #identifies all .csv files associated with Dai3 depth measurements and compiles them into one data frame
                      pattern = "*Bottom Line 3C.line.csv", full.names = TRUE) %>% 
  map_dfr(read_csv) %>%                                            # Store all files in list
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Ping_date),"%Y-%m-%d"), Ping_time, sep=" "), format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")) # combines the date and time columns into one date/time column
setwd(input_transect_data)
#river_area_evening<- read.csv('total_slice_area_weekly_2022-23.csv')
#river_area_split<- split(river_area_evening, river_area_evening$Dai)

desiredGroupingUnit3D = cut(depth_3D$Date_time, breaks = "24 hour") # places the depth data into groups organised by 24 hours
aggregate(depth_3D$Depth, by = list(desiredGroupingUnit3D), FUN = mean) # creates a value frame based on the means of the grouped data
avg_depth_3D<- data.frame(aggregate(depth_3D$Depth, by = list(desiredGroupingUnit3D), FUN = mean)) # creates a data frame out of the means of the data
avg_depth_3D$Date_POSIXct <- as.POSIXct(avg_depth_3D$Group.1, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok") # renames the the date/time column and ensures it is the correct format  
avg_depth_3D$day <- 1:nrow(avg_depth_3D)  #adds a day number column
avg_depth_3D$AMPM<- as.factor(format(avg_depth_3D$Date_POSIXct, "%p")) #creates a factor column called AM/PM based on the date/time column


Dai3_max_area<- (4508.351)
Dai3_area_loss<- (13.812)

dates <- as.Date(avg_depth_3D$Date_POSIXct)

my_values_area3 <- numeric(length(dates))
my_values_area3[1] <- Dai3_max_area



for (i in 2:length(dates)) {
  days_diff <- as.integer(dates[i] - dates[i-1])
  my_values_area3[i] <- my_values_area3[i-1] - Dai3_area_loss * days_diff
}

area_avg_3D<- cbind.data.frame(avg_depth_3D$Date_POSIXct, avg_depth_3D$day, my_values_area3) 
colnames(area_avg_3D)[1] <- 'Date' 
colnames(area_avg_3D)[2] <- 'Day'
colnames(area_avg_3D)[3] <- 'Area'
area_avg_3D$Dai<- paste0('3D')
# common_datetimes_2<- intersect(dailyavg_3D_split[['Night']]$Date_time, area_avg_3D$Date)
# for (i in seq_along(area_avg_3D)) {
#   area_avg_3D <- area_avg_3D[area_avg_3D$Date %in% common_datetimes_2, ]
# }
# common_datetimes<- intersect(dailyavg_3D_split[['Day']]$Date_time, dailyavg_3D_split[['Night']]$Date_time)
# for (i in seq_along(dailyavg_3D_split)) {
#   dailyavg_3D_split[[i]] <- dailyavg_3D_split[[i]][dailyavg_3D_split[[i]]$Date_time %in% common_datetimes, ]
# }


wpo_dai3D<- ((wpo_dai3D_afternoon + wpo_dai3D_evening)/2)

Dai3D_Flux<- data.frame(dailyavg_3D_split$sa_Ln * area_avg_3D$Area * wpo_dai3D_evening) %>%
  map_dfr(as.data.frame) %>%
  rename_at(1,~'Flux') %>%
  mutate(Days = dailyavg_3D_split$Days) %>%
  rename_at(2,~'Days') %>%
  mutate(peak_status = dailyavg_3D_split$peak_status) %>%
  rename_at(3,~'peak_status') %>%
  mutate(Dates = dailyavg_3D_split$Date_time) %>%
  rename_at(4, ~'Dates') %>%
  mutate(flux_alt = Flux*((4)))



setwd(data_analysis_output)


dai3D_flux_split<- Dai3D_Flux#split(Dai3D_TOD_flux_compare, Dai3D_TOD_flux_compare$TOD)

sum3D<- sum(dai3D_flux_split$Flux) #creates a factor that is the sum of the linear Sv data
div3D<- (dai3D_flux_split$Flux/sum3D) #Divides the each point of the linear Sv data by the sum pf the linear Sv data
CDF3D<- cumsum(div3D) #creates a factor of the cumulative sum of each linear Sv data point after it is divided by the total sum
CDF3D<- data.frame(CDF3D) #turns the sumulative sum data into a data frame
dai3D_flux_split <- cbind(dai3D_flux_split, CDF3D) #binds the cumulative sum data to the exports_3D data frame
}
##CDF_Creation####################################################
setwd(data_analysis_output)
library(segmented)
attach(dai15E_flux_split)
y<- dai15E_flux_split$CDF15E
x<- dai15E_flux_split$Dates
pw_reg_15E_2021<- data.frame(x = x, y = y)
out.lm <- lm(y ~ x, data = pw_reg_15E_2021)
br<- as.POSIXct(c('2021-12-01 18:00:00','2021-12-24 18:00:00', '2022-01-03 18:00:00', '2022-01-16 18:00:00'))
o <- segmented(out.lm, seg.Z = ~ x, psi = list(x = br))
dat2 = data.frame(x = pw_reg_15E_2021$x , y = predict(o))

pw_reg_15E_2021_22<- ggplot(pw_reg_15E_2021, aes(x = x, y = y)) +
geom_point() +
geom_line(data = dat2, color = 'blue', linewidth = 2) +
#geom_line(data = dat3, color = 'blue') +
scale_x_datetime(date_breaks = '14 days', date_labels = '%b-%d') +
scale_y_continuous(breaks = ~ seq(0, max(.x), .1)) +
annotate('segment', x= as.POSIXct("2021-11-19"), xend= as.POSIXct("2021-11-19"), yend=-Inf, y=Inf, colour = "black", alpha = 1, linewidth = 1) +
annotate('segment', x= as.POSIXct("2022-01-18"), xend= as.POSIXct("2022-01-18"), yend=-Inf, y=Inf, colour = "black", alpha = 1, linewidth = 1) +
annotate('segment', x= as.POSIXct("2022-02-16"), xend= as.POSIXct("2022-02-16"), yend=-Inf, y=Inf, colour = "black", alpha = 1, linewidth = 1) +
annotate('rect', xmin = c(as.POSIXct('2021-12-10 07:42:48'), as.POSIXct('2022-01-05 15:12:34')), xmax = c(as.POSIXct('2021-12-14 05:56:34'), as.POSIXct('2022-01-09 18:12:55')), 
         ymin = c(0.33364967, 0.73098583), ymax = c(0.60923356, 0.87092962), alpha = 0.2, color = 'red', fill = 'red') +
geom_vline(xintercept = (dai15E_flux_split$Dates[dai15E_flux_split$peak_status == "Peak"]), color = "black", linewidth = 1, alpha = 1) +
geom_point(size = 2) + theme_bw() +  geom_vline(xintercept = o$psi[, 'Est.'], color = 'red') + 
labs(x = "Date") + ylab(~paste('CDF', Phi)) +
theme(axis.text.x = element_text(color = 'black', size = 12), 
      axis.title = element_blank(), 
      axis.text.y = element_text(color = 'black', size = 12),
      axis.line = element_line(color = 'black'),
      panel.border = element_rect(colour = "black", fill=NA), 
      panel.grid.minor.y = element_blank())
plot(pw_reg_15E_2021_22)
as.POSIXct(o$psi[, 'Est.'])
ggsave("2021-22_Dai15E_Flux_CDF_piecewise_regression.png", plot = pw_reg_15E_2021_22, width =10, height =6, units = c("in"), dpi = 600) 

y<- dai3D_flux_split$CDF3D
x<- dai3D_flux_split$Dates
br<- as.POSIXct(c('2021-12-15 18:00:00', '2022-01-10 18:00:00'))
pw_reg_3D_2021<- data.frame(x = x, y = y)
out.lm <- lm(y ~ x, data = pw_reg_3D_2021)
p <- segmented(out.lm, seg.Z = ~x, psi = list(x = br))
dat3 = data.frame(x = pw_reg_3D_2021$x, y = predict(p))

pw_reg_3D_2021_22<- ggplot(pw_reg_3D_2021, aes(x = x, y = y)) +
geom_point() +
geom_line(data = dat3, color = 'blue', linewidth = 2) +
#geom_line(data = dat3, color = 'blue') +
scale_x_datetime(date_breaks = '14 days', date_labels = '%b-%d') +
scale_y_continuous(breaks = ~ seq(0, max(.x), .1)) +
annotate('segment', x= as.POSIXct("2022-01-18"), xend= as.POSIXct("2022-01-18"), yend=-Inf, y=Inf, colour = "black", alpha = 1, linewidth = 1) +
annotate('rect', xmin = c(as.POSIXct('2021-12-13 12:00:00'), as.POSIXct('2022-01-05 07:32:09')), xmax = c(as.POSIXct('2021-12-15 16:43:07'), as.POSIXct('2022-01-14 14:00:00')), 
         ymin = c(0.0, 0.54660414), ymax = c(0.20729998, 1.0), alpha = 0.2, color = 'red', fill = 'red') +
geom_vline(xintercept = (dai15E_flux_split$Dates[dai15E_flux_split$peak_status == "Peak"]), color = "black", linewidth = 1) +
geom_point(size = 2) + theme_bw() + geom_vline(xintercept = p$psi[, 'Est.'], color = 'red') +
labs(x = "Date") + ylab(~paste('CDF', Phi)) +
theme(axis.text.x = element_text(color = 'black', size = 12), 
      axis.title = element_blank(), 
      axis.text.y = element_text(color = 'black', size = 12),
      axis.line = element_line(color = 'black'),
      panel.border = element_rect(colour = "black", fill=NA),
      panel.grid.minor.y = element_blank())
plot(pw_reg_3D_2021_22)
as.POSIXct(p$psi[, 'Est.'])
ggsave("2021-22_Dai3D_Flux_CDF_piecewise_regression.png", plot = pw_reg_3D_2021_22, width =10, height =6, units = c("in"), dpi = 600) 

#2022-23###################################################
##DirectoryScripting########################
input_all_data = "Z:/fishproj/Cambodia Dai project/Analytic/Analytic Products/TS2022-23_Data_Exports/"#stn1 ONLY/TS_Data_-80/
full_moon_dates<- as.Date(c('2022-11-08', '2022-12-07', '2023-01-06', '2023-02-05'))
input_transect_data = "Z:/fishproj/Cambodia Dai project/Analytic/Analytic Products/TS2022-23_Data_Exports/Transects/"
data_analysis_output = "Z:/fishproj/Cambodia Dai project/Papers/Analysis Paper/Pics, Plot, Figures/2022-23/"
##Transects_and_Weighted_Averages########################
setwd(input_transect_data)
Dai3D_evening_allBNR<- list.files(path = input_transect_data,                               #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                                  pattern = "^Dai3D_ABC_10mbin_\\d{8}_fullwatercolumn_evening_BNR*.*csv", full.names = TRUE) %>%
  map_dfr(read_csv) %>%
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
library('plyr')
Weighted_Prop_Occupied_evening_3D<- data.frame(1/nrow(Dai3D_evening_allBNR)*sum(sa_weights_evening_3D$weight)*Dai3D_evening_allBNR$Proportion_occupied)
colnames(Weighted_Prop_Occupied_evening_3D) <- c(ncol(1), 'Weighted_Prop_Occ')
Dai3D_evening_allBNR<- cbind(Dai3D_evening_allBNR, Weighted_Prop_Occupied_evening_3D[c('Weighted_Prop_Occ')]) 

dai3D_evening_bnr1mean<- mean(Dai3D_evening_allBNR$Weighted_Prop_Occ[Dai3D_evening_allBNR$BNR == 'BNR1'])
dai3D_evening_bnr2mean<- mean(Dai3D_evening_allBNR$Weighted_Prop_Occ[Dai3D_evening_allBNR$BNR == 'BNR2']) 
#dai3D_evening_bnr3mean<- mean(Dai3D_evening_allBNR$Weighted_Prop_Occ[Dai3D_evening_allBNR$BNR == 'BNR3']) 
wpo_dai3D_evening<- (sum(dai3D_evening_bnr1mean, dai3D_evening_bnr2mean)/2)

Dai15E_evening_allBNR<- list.files(path = input_transect_data,                               #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                                   pattern = "^Dai15E_ABC_10mbin_\\d{8}_fullwatercolumn_evening_BNR*.*csv", full.names = TRUE) %>%
  map_dfr(read_csv) %>%
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

Dai15E_afternoon_allBNR<- list.files(path = input_transect_data,                               #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                                     pattern = "^Dai15E_ABC_10mbin_\\d{8}_fullwatercolumn_afternoon_BNR*.*csv", full.names = TRUE) %>%
  map_dfr(read_csv) %>%
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
setwd(data_analysis_output)
write.csv(Dai3D_afternoon_allBNR, 'Dai3D_afternoon_allBNR.csv', row.names = T)
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
#dai3D_afternoon_bnr3mean<- mean(Dai3D_afternoon_allBNR$Weighted_Prop_Occ[Dai3D_afternoon_allBNR$BNR == 'BNR3']) 
wpo_dai3D_afternoon<- (sum(dai3D_afternoon_bnr1mean, dai3D_afternoon_bnr2mean)/2)


setwd(input_all_data)
rm(exports_15E)
exports_15E<- list.files(path = input_all_data,                               #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                         pattern = "^15E.*FullWaterColumn_100m.sv.csv", full.names = TRUE) %>% 
  map_dfr(read_csv) %>%
  filter(Layer !=0) %>%                                                       #eliminates any data layers above layer 1 that may have been accidentally exported 
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Date_M),"%Y%m%d"), Time_M, sep=" "),format = "%Y-%m-%d %H:%M", tz="Asia/Bangkok")) %>%#combines the time and date columns into one column for proper time teries analysis
  mutate(sa_Ln = 10^(Area_Backscatter_Strength/10)) %>%
  mutate(DayNight = if_else(as.numeric(substring(Time_M, 1, 2)) >= 18 | as.numeric(substring(Time_M, 1, 2)) < 06, 'Night', "Day")) %>%
  mutate(TOD = case_when(endsWith(DayNight, 'Night') ~ 2, endsWith(DayNight, 'Day') ~ 1))


daily_avg_15E<- data.frame(aggregate(sa_Ln ~ cut(Date_time, "24 hour"), exports_15E, mean))
colnames(daily_avg_15E)[1]<- 'Date_time'#data.frame(aggregate(sa_Ln ~ DayNight + Week, exports_15E, mean))
#daily_avg_15E$Date_time<- (daily_avg_15E_dates$Date_time)
daily_avg_15E$Date_time<- as.POSIXct(daily_avg_15E$Date_time, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")
daily_avg_15E$sa_log = 10*log10(daily_avg_15E$sa_Ln) #
#full_moon_dates<- as.Date(c('2022-12-08', '2023-01-07'))
daily_avg_15E$date_column<- as.Date(daily_avg_15E$Date_time, format = "%Y-%m-%d")
daily_avg_15E$peak_status <- ifelse(daily_avg_15E$date_column %in% full_moon_dates, "Peak", "Off-Peak")

daily_avg_15E$Days <- as.numeric(difftime(daily_avg_15E$date_column, min(daily_avg_15E$date_column), units = "days")) + 1
dailyavg_15E_split<- daily_avg_15E#split(daily_avg_15E, daily_avg_15E$DayNight)

depth_15E<- list.files(path = input_all_data,     #identifies all .csv files associated with Dai15 depth measurements and compiles them into one data frame
                       pattern = "*Bottom Line stn1.line.csv", full.names = TRUE) %>% 
  map_dfr(read_csv) %>%                                            # Store all files in list
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Depth_date),"%Y-%m-%d"), Depth_time, sep=" "), format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")) # combines the date and time columns into one date/time column
setwd(input_transect_data)
#river_area_evening<- read.csv('total_slice_area_weekly_2022-23.csv')
#river_area_split<- split(river_area_evening, river_area_evening$Dai)

desiredGroupingUnit15E = cut(depth_15E$Date_time, breaks = "24 hour") # places the depth data into groups organised by 24 hours
aggregate(depth_15E$Depth_meters, by = list(desiredGroupingUnit15E), FUN = mean) # creates a value frame based on the means of the grouped data
avg_depth_15E<- data.frame(aggregate(depth_15E$Depth_meters, by = list(desiredGroupingUnit15E), FUN = mean)) # creates a data frame out of the means of the data
avg_depth_15E$Date_POSIXct <- as.POSIXct(avg_depth_15E$Group.1, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok") # renames the the date/time column and ensures it is the correct format  
avg_depth_15E$day <- 1:nrow(avg_depth_15E)  #adds a day number column
avg_depth_15E$AMPM<- as.factor(format(avg_depth_15E$Date_POSIXct, "%p")) #creates a factor column called AM/PM based on the date/time column


Dai15_max_area<- (4781.349+20.068)
Dai15_area_loss<- (20.068)

dates <- as.Date(avg_depth_15E$Date_POSIXct)

my_values_area15 <- numeric(length(dates))
my_values_area15[1] <- Dai15_max_area



for (i in 2:length(dates)) {
  days_diff <- as.integer(dates[i] - dates[i-1])
  my_values_area15[i] <- my_values_area15[i-1] - Dai15_area_loss * days_diff
}

area_avg_15E<- cbind.data.frame(avg_depth_15E$Date_POSIXct, avg_depth_15E$day, my_values_area15) 
colnames(area_avg_15E)[1] <- 'Date' 
colnames(area_avg_15E)[2] <- 'Day'
colnames(area_avg_15E)[3] <- 'Area'
area_avg_15E$Dai<- paste0('15E')
# common_datetimes_2<- intersect(dailyavg_15E_split[['Night']]$Date_time, area_avg_15E$Date)
# for (i in seq_along(area_avg_15E)) {
#   area_avg_15E <- area_avg_15E[area_avg_15E$Date %in% common_datetimes_2, ]
# }
# common_datetimes<- intersect(dailyavg_15E_split[['Day']]$Date_time, dailyavg_15E_split[['Night']]$Date_time)
# for (i in seq_along(dailyavg_15E_split)) {
#   dailyavg_15E_split[[i]] <- dailyavg_15E_split[[i]][dailyavg_15E_split[[i]]$Date_time %in% common_datetimes, ]
# }

wpo_dai15E<- ((wpo_dai15E_afternoon + wpo_dai15E_evening)/2)

Dai15E_Flux<- data.frame(dailyavg_15E_split$sa_Ln * area_avg_15E$Area * wpo_dai15E) %>%
  map_dfr(as.data.frame) %>%
  rename_at(1,~'Flux') %>%
  mutate(Days = dailyavg_15E_split$Days) %>%
  rename_at(2,~'Days') %>%
  mutate(peak_status = dailyavg_15E_split$peak_status) %>%
  rename_at(3,~'peak_status') %>%
  mutate(Dates = dailyavg_15E_split$Date_time) %>%
  rename_at(4, ~'Dates') %>%
  mutate(flux_alt = Flux*((4)))

dai15E_flux_split<- Dai15E_Flux#split(Dai15E_TOD_flux_compare, Dai15E_TOD_flux_compare$TOD)

sum15E<- sum(dai15E_flux_split$Flux) #creates a factor that is the sum of the linear Sv data
div15E<- (dai15E_flux_split$Flux/sum15E) #Divides the each point of the linear Sv data by the sum pf the linear Sv data
CDF15E<- cumsum(div15E) #creates a factor of the cumulative sum of each linear Sv data point after it is divided by the total sum
CDF15E<- data.frame(CDF15E) #turns the sumulative sum data into a data frame
dai15E_flux_split <- cbind(dai15E_flux_split, CDF15E) #binds the cumulative sum data to the exports_15E data frame

setwd(input_all_data)
rm(exports_3D)
exports_3D<- list.files(path = input_all_data,                               #identifies all .csv files associated with Dai3 full water column Sv measurements and compiles them into one data frame
                        pattern = "^3D.*FullWaterColumn_100m.sv.csv", full.names = TRUE) %>% 
  map_dfr(read_csv) %>%
  filter(Layer !=0) %>%                                                       #eliminates any data layers above layer 1 that may have been accidentally exported 
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Date_M),"%Y%m%d"), Time_M, sep=" "),format = "%Y-%m-%d %H:%M", tz="Asia/Bangkok")) %>%#combines the time and date columns into one column for proper time teries analysis
  mutate(sa_Ln = 10^(Area_Backscatter_Strength/10)) %>%
  mutate(DayNight = if_else(as.numeric(substring(Time_M, 1, 2)) >= 18 | as.numeric(substring(Time_M, 1, 2)) < 06, 'Night', "Day")) %>%
  mutate(TOD = case_when(endsWith(DayNight, 'Night') ~ 2, endsWith(DayNight, 'Day') ~ 1))


daily_avg_3D<- data.frame(aggregate(sa_Ln ~ cut(Date_time, "24 hour"), exports_3D, mean))
colnames(daily_avg_3D)[1]<- 'Date_time'#data.frame(aggregate(sa_Ln ~ DayNight + Week, exports_3D, mean))
#daily_avg_3D$Date_time<- (daily_avg_3D_dates$Date_time)
daily_avg_3D$Date_time<- as.POSIXct(daily_avg_3D$Date_time, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")
daily_avg_3D$sa_log = 10*log10(daily_avg_3D$sa_Ln) #
#full_moon_dates<- as.Date(c('2022-12-08', '2023-01-07'))
daily_avg_3D$date_column<- as.Date(daily_avg_3D$Date_time, format = "%Y-%m-%d")
daily_avg_3D$peak_status <- ifelse(daily_avg_3D$date_column %in% full_moon_dates, "Peak", "Off-Peak")

daily_avg_3D$Days <- as.numeric(difftime(daily_avg_3D$date_column, min(daily_avg_3D$date_column), units = "days")) + 1
dailyavg_3D_split<- daily_avg_3D#split(daily_avg_3D, daily_avg_3D$DayNight)

depth_3D<- list.files(path = input_all_data,     #identifies all .csv files associated with Dai3 depth measurements and compiles them into one data frame
                      pattern = "*Bottom Line stn2.line.csv", full.names = TRUE) %>% 
  map_dfr(read_csv) %>%                                            # Store all files in list
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Depth_date),"%Y-%m-%d"), Depth_time, sep=" "), format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")) # combines the date and time columns into one date/time column
setwd(input_transect_data)
#river_area_evening<- read.csv('total_slice_area_weekly_2022-23.csv')
#river_area_split<- split(river_area_evening, river_area_evening$Dai)

desiredGroupingUnit3D = cut(depth_3D$Date_time, breaks = "24 hour") # places the depth data into groups organised by 24 hours
aggregate(depth_3D$Depth_meters, by = list(desiredGroupingUnit3D), FUN = mean) # creates a value frame based on the means of the grouped data
avg_depth_3D<- data.frame(aggregate(depth_3D$Depth_meters, by = list(desiredGroupingUnit3D), FUN = mean)) # creates a data frame out of the means of the data
avg_depth_3D$Date_POSIXct <- as.POSIXct(avg_depth_3D$Group.1, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok") # renames the the date/time column and ensures it is the correct format  
avg_depth_3D$day <- 1:nrow(avg_depth_3D)  #adds a day number column
avg_depth_3D$AMPM<- as.factor(format(avg_depth_3D$Date_POSIXct, "%p")) #creates a factor column called AM/PM based on the date/time column


Dai3_max_area<- (5082.595+9.912)
#Dai3_area_loss<- (9.912)
Dai3_area_loss<- (3.243)


dates <- as.Date(avg_depth_3D$Date_POSIXct)

my_values_area3 <- numeric(length(dates))
my_values_area3[1] <- Dai3_max_area



for (i in 2:length(dates)) {
  days_diff <- as.integer(dates[i] - dates[i-1])
  my_values_area3[i] <- my_values_area3[i-1] - Dai3_area_loss * days_diff
}

area_avg_3D<- cbind.data.frame(avg_depth_3D$Date_POSIXct, avg_depth_3D$day, my_values_area3) 
colnames(area_avg_3D)[1] <- 'Date' 
colnames(area_avg_3D)[2] <- 'Day'
colnames(area_avg_3D)[3] <- 'Area'
area_avg_3D$Dai<- paste0('3D')
# common_datetimes_2<- intersect(dailyavg_3D_split[['Night']]$Date_time, area_avg_3D$Date)
# for (i in seq_along(area_avg_3D)) {
#   area_avg_3D <- area_avg_3D[area_avg_3D$Date %in% common_datetimes_2, ]
# }
# common_datetimes<- intersect(dailyavg_3D_split[['Day']]$Date_time, dailyavg_3D_split[['Night']]$Date_time)
# for (i in seq_along(dailyavg_3D_split)) {
#   dailyavg_3D_split[[i]] <- dailyavg_3D_split[[i]][dailyavg_3D_split[[i]]$Date_time %in% common_datetimes, ]
# }


wpo_dai3D<- ((wpo_dai3D_afternoon + wpo_dai3D_evening)/2)

Dai3D_Flux<- data.frame(dailyavg_3D_split$sa_Ln * area_avg_3D$Area * wpo_dai3D_evening) %>%
  map_dfr(as.data.frame) %>%
  rename_at(1,~'Flux') %>%
  mutate(Days = dailyavg_3D_split$Days) %>%
  rename_at(2,~'Days') %>%
  mutate(peak_status = dailyavg_3D_split$peak_status) %>%
  rename_at(3,~'peak_status') %>%
  mutate(Dates = dailyavg_3D_split$Date_time) %>%
  rename_at(4, ~'Dates') %>%
  mutate(flux_alt = Flux*((4)))

setwd(data_analysis_output)

dai3D_flux_split<- Dai3D_Flux#split(Dai3D_TOD_flux_compare, Dai3D_TOD_flux_compare$TOD)

sum3D<- sum(dai3D_flux_split$Flux) #creates a factor that is the sum of the linear Sv data
div3D<- (dai3D_flux_split$Flux/sum3D) #Divides the each point of the linear Sv data by the sum pf the linear Sv data
CDF3D<- cumsum(div3D) #creates a factor of the cumulative sum of each linear Sv data point after it is divided by the total sum
CDF3D<- data.frame(CDF3D) #turns the sumulative sum data into a data frame
dai3D_flux_split <- cbind(dai3D_flux_split, CDF3D) #binds the cumulative sum data to the exports_3D data frame
##CDF_Creation####################################################
library(segmented)
attach(dai15E_flux_split)
setwd(data_analysis_output)
{
y<- dai15E_flux_split$CDF15E
x<- dai15E_flux_split$Dates
pw_reg_15E_2022<- data.frame(x = x, y = y)
out.lm <- lm(y ~ x, data = pw_reg_15E_2022)
br <- as.POSIXct(c('2022-12-22 14:00:00', '2023-01-10 14:00:00'))
o <- segmented(out.lm, seg.Z = ~x, psi = list(x = br))
dat2 = data.frame(x = pw_reg_15E_2022$x, y = predict(o))

pw_reg_15E_2022_23<- ggplot(pw_reg_15E_2022, aes(x = x, y = y)) +
  geom_point() +
  geom_line(data = dat2, color = 'blue', linewidth = 2) +
  scale_x_datetime(limits = as.POSIXct(c('2022-11-15', '2023-02-06')), date_breaks = '14 days', date_labels = '%b-%d') + #  scale_x_datetime(breaks= seq(min(pw_reg_15E_2022$x), max(pw_reg_15E_2022$x), by = '14 days'), date_breaks = '14 days', date_labels = '%b-%d') +
  scale_y_continuous(breaks = ~ seq(0, max(.x), .1)) +
  #annotate('segment', x= as.POSIXct("2023-01-06"), xend= as.POSIXct("2023-01-06"), yend=-Inf, y=Inf, colour = "black", alpha = 1, linewidth = 1) + 
  annotate('segment', x= as.POSIXct("2023-02-05"), xend= as.POSIXct("2023-02-05"), yend=-Inf, y=Inf, colour = "black", alpha = 1, linewidth = 1) +
  #annotate('rect', xmin = c(as.POSIXct('2020-12-20 13:14:11')), xmax = c(as.POSIXct('2020-12-26 15:35:47')), ymin = c())
  geom_vline(xintercept = (dai15E_flux_split$Dates[dai15E_flux_split$peak_status == "Peak"]), color = "black", linewidth = 1, alpha = 1) +
  #geom_vline(xintercept = o$psi[, 'Est.'], color = 'red') +
  geom_point(size = 2) + theme_bw() + 
  labs(x = "Date") + ylab(~paste('CDF', Phi)) +
  theme(axis.text.x = element_text(color = 'black', size = 12), 
        axis.title = element_blank(), 
        axis.text.y = element_text(color = 'black', size = 12),
        axis.line = element_line(color = 'black'),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.grid.minor.y = element_blank())
plot(pw_reg_15E_2022_23)
}
as.POSIXct(o$psi[, 'Est.'])
ggsave("2022-23_Dai15E_Flux_CDF_piecewise_regression.png", plot = pw_reg_15E_2022_23, width =10, height =6, units = c("in"), dpi = 600) 

y<- dai3D_flux_split$CDF3D
x<- dai3D_flux_split$Dates
br<- as.POSIXct(c('2023-01-03 14:00:00', '2023-01-25 14:00:00'))
pw_reg_3D<- data.frame(x = x, y = y)
out.lm <- lm(y ~ x, data = pw_reg_3D)
p <- segmented(out.lm, seg.Z = ~x, psi = list(x = br))
dat3 = data.frame(x = pw_reg_3D$x, y = predict(p))

pw_reg_3D_2022_23<- ggplot(pw_reg_3D, aes(x = x, y = y)) +
geom_point() +
geom_line(data = dat3, color = 'blue', linewidth = 2) +
#geom_line(data = dat3, color = 'blue') +
scale_x_datetime(date_breaks = '14 days', date_labels = '%b-%d') +
scale_y_continuous(breaks = ~ seq(0, max(.x), .1)) +
#annotate('segment', x= as.POSIXct("2023-01-06"), xend= as.POSIXct("2023-01-06"), yend=-Inf, y=Inf, colour = "black", alpha = 1, linewidth = 1) +
annotate('segment', x= as.POSIXct("2023-02-05"), xend= as.POSIXct("2023-02-05"), yend=-Inf, y=Inf, colour = "black", alpha = 1, linewidth = 1) +
annotate('rect', xmin = c(as.POSIXct('2023-01-01 00:21:15'), as.POSIXct('2023-01-25 01:04:18')), xmax = c(as.POSIXct('2023-01-04 13:21:15'), as.POSIXct('2023-01-28 05:04:18')), 
         ymin = c(0.0, 0.77492574), ymax = c(0.27310719, 1.0), alpha = 0.2, color = 'red', fill = 'red') +
geom_vline(xintercept = (dai15E_flux_split$Dates[dai15E_flux_split$peak_status == "Peak"]), color = "black", linewidth = 1, alpha = 1) +
geom_point(size = 2) + theme_bw() + geom_vline(xintercept = p$psi[, 'Est.'], color = 'red') +
labs(x = "Date") + ylab(~paste('CDF', Phi)) +
theme(axis.text.x = element_text(color = 'black', size = 12), 
      axis.title = element_blank(), 
      axis.text.y = element_text(color = 'black', size = 12),
      axis.line = element_line(color = 'black'),
      panel.border = element_rect(colour = "black", fill=NA),
      panel.grid.minor.y = element_blank())
plot(pw_reg_3D_2022_23)
as.POSIXct(p$psi[, 'Est.'])
ggsave("2022-23_Dai3D_Flux_CDF_piecewise_regression.png", plot = pw_reg_3D_2022_23, width =10, height =6, units = c("in"), dpi = 600) 

#FINALFIGURECREATION##########################################
data_analysis_output = "Z:/fishproj/Cambodia Dai project/Papers/Analysis Paper/Pics, Plot, Figures/"

setwd(data_analysis_output)
{
cdf_fig<- ggarrange(pw_reg_15E_2020_21 + rremove("ylab") + rremove("xlab"),
                    pw_reg_3C_2020_21 + rremove("ylab") + rremove("xlab") + rremove('y.text') + rremove('y.ticks'),
                    pw_reg_15E_2021_22 + rremove("ylab") + rremove("xlab"),
                    pw_reg_3D_2021_22 + rremove("ylab") + rremove("xlab") + rremove('y.text') + rremove('y.ticks'),
                    pw_reg_15E_2022_23 + rremove("ylab") + rremove("xlab"),
                    pw_reg_3D_2022_23 + rremove("ylab") + rremove("xlab") + rremove('y.text') + rremove('y.ticks'),
                    ncol = 2, nrow = 3, common.legend = F, legend = 'none', widths = c(1.04,1))
cdf_fig<- annotate_figure(cdf_fig, left = textGrob(~paste('CDF', Phi), rot = 90, vjust = 1, gp = gpar(cex = 1)),
                          bottom = textGrob("Date", gp = gpar(cex = 1), vjust = -0.4))
plot(cdf_fig)
ggsave("Figure_5.png", plot = cdf_fig, width =10, height =6, units = c("in"), dpi = 1200) 
}
}
