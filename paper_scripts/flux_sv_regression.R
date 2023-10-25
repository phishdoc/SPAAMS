input_all_data<- "Z:/fishproj/Cambodia Dai project/Analytic/Analytic Products/TS2020_Data_Exports/"
full_moon_dates<- as.Date(c('2020-11-30', '2020-12-30', '2021-01-29'))
data_analysis_output = "Z:/fishproj/Cambodia Dai project/Papers/Analysis Paper/Pics, Plot, Figures/2020-21/"
input_transect_data = "Z:/fishproj/Cambodia Dai project/Analytic/Analytic Products/TS2021-22_Data_Exports/Transects/"
setwd(input_all_data)

{
exports_stn1<- list.files(path = input_all_data,                               #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                          pattern = "^15E.*FullWaterColumn_100m.sv.csv", full.names = TRUE) %>% 
  map_dfr(read_csv) %>%
  filter(Layer !=0) %>% 
  filter(Sv_mean !=-999) %>% #eliminates any data layers above layer 1 that may have been accidentally exported 
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Date_M),"%Y%m%d"), Time_M, sep=" "),format = "%Y-%m-%d %H:%M", tz="Asia/Bangkok")) %>%
  mutate(DayNight = if_else(as.numeric(substring(Time_M, 1, 2)) >= 18 | as.numeric(substring(Time_M, 1, 2)) < 06, 'Night', "Day")) %>%
  mutate(date_column = as.Date(Date_time, format = "%Y-%m-%d")) %>%
  mutate(peak_status = ifelse(date_column %in% full_moon_dates, "Peak", "Off-Peak"))


exports_stn2<- list.files(path = input_all_data,      #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                          pattern = "*3C.*FullWaterColumn_100m.sv.csv", full.names = TRUE) %>% 
  map_dfr(read_csv) %>%                                            # Store all files in list
  filter(Layer !=0) %>%  
  filter(Sv_mean !=-999) %>%
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Date_M),"%Y%m%d"), Time_M, sep=" "),format = "%Y-%m-%d %H:%M", tz="Asia/Bangkok")) %>%
  mutate(DayNight = if_else(as.numeric(substring(Time_M, 1, 2)) >= 18 | as.numeric(substring(Time_M, 1, 2)) < 06, 'Night', "Day")) %>%
  mutate(date_column = as.Date(Date_time, format = "%Y-%m-%d")) %>%
  mutate(peak_status = ifelse(date_column %in% full_moon_dates, "Peak", "Off-Peak"))
#rename_at(3,~'peak_status') %>%#identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
exports_stn1$Sv_Ln = 10^(exports_stn1$Sv_mean/10)
daily_avg_stn1<- data.frame(aggregate(Sv_Ln ~ cut(Date_time, "24 hour"), exports_stn1, mean))
colnames(daily_avg_stn1)[1]<- 'Date_time'
daily_avg_stn1$Date_time<- as.POSIXct(daily_avg_stn1$Date_time, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")
daily_avg_stn1$Sv_log = 10*log10(daily_avg_stn1$Sv_Ln) #
#full_moon_dates<- as.Date(c('2022-12-08', '2023-01-07'))
daily_avg_stn1$date_column<- as.Date(daily_avg_stn1$Date_time, format = "%Y-%m-%d")
daily_avg_stn1$peak_status <- ifelse(daily_avg_stn1$date_column %in% full_moon_dates, "Peak", "Off-Peak")
daily_avg_stn1$Days <- as.numeric(difftime(daily_avg_stn1$date_column, min(daily_avg_stn1$date_column), units = "days")) + 1


exports_stn2$Sv_Ln = 10^(exports_stn2$Sv_mean/10)
daily_avg_stn2<- data.frame(aggregate(Sv_Ln ~ cut(Date_time, "24 hour"), exports_stn2, mean))
colnames(daily_avg_stn2)[1]<- 'Date_time'
daily_avg_stn2$Date_time<- as.POSIXct(daily_avg_stn2$Date_time, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")
daily_avg_stn2$Sv_log = 10*log10(daily_avg_stn2$Sv_Ln) #
#full_moon_dates<- as.Date(c('2022-12-08', '2023-01-07'))
daily_avg_stn2$date_column<- as.Date(daily_avg_stn2$Date_time, format = "%Y-%m-%d")
daily_avg_stn2$peak_status <- ifelse(daily_avg_stn2$date_column %in% full_moon_dates, "Peak", "Off-Peak")
daily_avg_stn2$Days <- as.numeric(difftime(daily_avg_stn2$date_column, min(daily_avg_stn2$date_column), units = "days")) + 1
}

Sv_time_plot_stn1<- ggplot(daily_avg_stn1, aes(x = date_column, y = Sv_log)) + geom_point(size = 3, show.legend = T) + 
  # annotate('segment', x= as.Date("2021-11-19"), xend= as.Date("2021-11-19"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  annotate('segment', x= as.Date("2020-11-30"), xend= as.Date("2020-11-30"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  annotate('segment', x= as.Date("2020-12-30"), xend= as.Date("2020-12-30"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  annotate('segment', x= as.Date("2021-01-29"), xend= as.Date("2021-01-29"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  # annotate('segment', x= as.Date("2022-01-18"), xend= as.Date("2022-01-18"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  # annotate('segment', x= as.Date("2022-02-16"), xend= as.Date("2022-02-16"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  # annotate('segment', x= as.Date("2022-12-07"), xend= as.Date("2022-12-07"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  # annotate('segment', x= as.Date("2023-01-06"), xend= as.Date("2023-01-06"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  # annotate('segment', x= as.Date("2023-02-05"), xend= as.Date("2023-02-05"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  geom_vline(xintercept = (daily_avg_stn1$date_column[daily_avg_stn1$peak_status == "Peak"]), color = "black") +
  scale_x_date(date_breaks = '5 days', date_labels = '%b-%d') + #scale_color_manual(values = c('darkgrey', 'black')) +
  #scale_y_continuous(breaks = function(y) seq(floor(min(y, digits = 1)), ceiling(max(y, digits = 1)), by = 1)) +
  scale_y_continuous(limits = c(-75,-50, by =3)) +
  labs(y = "Sv mean 12 Hour Avg (dB)", x = "Date") + labs(color = 'Time of Day') + theme_bw() +#ggtitle("2020-21 Dai 15E Sv mean 12 hour Day/Night") + 
  theme(axis.text.x = element_text()) 
#ggsave("Sv_12hour_Plot_stn1.pdf", plot = Sv_time_plot_stn1, width =10, height =6, units = c("in"), dpi = 600) #saves the graph as a pdf
plot(Sv_time_plot_stn1)




Sv_time_plot_stn2<- ggplot(daily_avg_stn2, aes(x = date_column, y = Sv_log)) + geom_point(size = 3, show.legend = T) + 
  annotate('segment', x= as.Date("2020-11-30"), xend= as.Date("2020-11-30"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  annotate('segment', x= as.Date("2021-01-29"), xend= as.Date("2021-01-29"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  # annotate('segment', x= as.Date("2021-11-19"), xend= as.Date("2021-11-19"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  # annotate('segment', x= as.Date("2022-01-18"), xend= as.Date("2022-01-18"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  # annotate('segment', x= as.Date("2022-01-18"), xend= as.Date("2022-01-18"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  # annotate('segment', x= as.Date("2022-12-07"), xend= as.Date("2022-12-07"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  # annotate('segment', x= as.Date("2023-01-06"), xend= as.Date("2023-01-06"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  # annotate('segment', x= as.Date("2023-02-05"), xend= as.Date("2023-02-05"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  geom_vline(xintercept = (daily_avg_stn2$date_column[daily_avg_stn2$peak_status == "Peak"]), color = "black") +
  labs(y = "Sv Mean 12 Hour Avg (dB)", x = "Date") + #ggtitle("Dai 3C Sv mean 12 hour Day/Night") +
  scale_x_date(date_breaks = '5 days', date_labels = '%b-%d') + #scale_color_manual(values = c('darkgrey', 'black')) +
  #scale_y_continuous(breaks = function(y) seq(floor(min(y, digits = 1)), ceiling(max(y, digits = 1)), by = 1)) +
  scale_y_continuous(limits = c(-75,-50, by =3)) + labs(color = 'Time of Day') + theme_bw() +
  theme(axis.text.x = element_text())
#ggsave("Sv_12hour_Plot_stn2.pdf", plot = Sv_time_plot_stn2,  width =10, height =6, units = c("in"), dpi = 600) #saves the graph as a pdf
plot(Sv_time_plot_stn2)

setwd(input_transect_data)
###DAI3######################################################################################################################################################################################
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
####DAI15####################################################################################################################################################################################################
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

#afternoon################
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

##afternoon#####
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

cdf_15E<- ggplot(dai15E_flux_split, aes(x = Dates, y = CDF15E)) +  
annotate('segment', x= as.POSIXct("2020-11-30"), xend= as.POSIXct("2020-11-30"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +#creates the continuous density function plot for Dai15
annotate('segment', x= as.POSIXct("2021-01-29"), xend= as.POSIXct("2021-01-29"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +#creates the continuous density function plot for Dai15
geom_vline(xintercept = (dai15E_flux_split$Dates[dai15E_flux_split$peak_status == "Peak"]), color = "black") +
geom_point(size = 4) + theme_bw() + ylab(~paste('CDF', Phi)) +
labs(x = "Date") + 
#stat_regline_equation(aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")), label.x.npc = "centre", label.y.npc = "centre") +#ggtitle("2020-21 Day Flux CDF Dai 15E") +
#scale_x_continuous(breaks = ~ seq(1, max(.x), 9)) + 
scale_x_datetime(date_breaks = '5 days', date_labels = '%b-%d') +
scale_y_continuous(breaks = ~ seq(0, max(.x), .1)) +
theme(axis.text.x = element_text())
plot(cdf_15E)


flux_sv_depth_stn1<- cbind.data.frame(daily_avg_stn1$Date_time, 
                                      daily_avg_stn1$Sv_log, 
                                      dai15E_flux_split$flux_alt, 
                                      avg_depth_15E$x, 
                                      daily_avg_stn1$Days) %>%
  rename_at(1,~'Date_time') %>%
  rename_at(2,~'Sv_log') %>%
  rename_at(3,~'Flux') %>%
  rename_at(4,~'Depth') %>%
  rename_at(5,~'Days')

model.lm<- lm(Sv_log ~ Flux + Depth + sin(Days), data = flux_sv_depth_stn1)
summary(model.lm)
a<- aov(model.lm)
summary(a)
plot(model.lm)
ggplot(flux_sv_depth_stn1, aes(y = Flux, x= Depth)) + geom_point() #+ geom_point(aes(y = Flux, x = Depth), data = flux_sv_depth_stn1, color = 'blue') 
ggplot(flux_sv_depth_stn1, aes(y = Sv_log, x= Depth)) + geom_point()


flux_sv_depth_stn1$Flux*sin(flux_sv_depth_stn1$Days) + flux_sv_depth_stn1$Depth


