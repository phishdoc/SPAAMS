#----
# TonleSap_OGIVE_CDF_PDF.R
# Jackson Swan, 2022-8-24
# 
# Last Modified by: Mark Yamane, 2023-10-26
#---

library("tidyverse")
library("ggpubr")
library("lubridate")
#{Sys.sleep(5)}
message("Configuration successful, running script #1, backscatter CDF, PDF, and time-series plots")
#{Sys.sleep(5)}
setwd(input_all_data)

{
exports_stn1<- list.files(path = input_all_data,                               #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                         pattern = "^15E.*FullWaterColumn_100m.sv.csv", full.names = TRUE) %>% 
  map_dfr(read_csv, show_col_types = FALSE) %>%
  filter(Layer !=0) %>% 
  filter(Sv_mean !=-999) %>% #eliminates any data layers above layer 1 that may have been accidentally exported 
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Date_M),"%Y%m%d"), Time_M, sep=" "),format = "%Y-%m-%d %H:%M", tz="Asia/Bangkok")) %>%
  mutate(DayNight = if_else(as.numeric(substring(Time_M, 1, 2)) >= 18 | as.numeric(substring(Time_M, 1, 2)) < 06, 'Night', "Day")) %>%
  mutate(date_column = as.Date(Date_time, format = "%Y-%m-%d")) %>%
  mutate(peak_status = ifelse(date_column %in% full_moon_dates, "Peak", "Off-Peak"))
  #combines the time and date columns into one column for proper time teries analysis


exports_stn2<- list.files(path = input_all_data,      #identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
                       pattern = "*3D.*FullWaterColumn_100m.sv.csv", full.names = TRUE) %>% 
  map_dfr(read_csv, show_col_types = FALSE) %>%                                            # Store all files in list
  filter(Layer !=0) %>%  
  filter(Sv_mean !=-999) %>%
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Date_M),"%Y%m%d"), Time_M, sep=" "),format = "%Y-%m-%d %H:%M", tz="Asia/Bangkok")) %>%
  mutate(DayNight = if_else(as.numeric(substring(Time_M, 1, 2)) >= 18 | as.numeric(substring(Time_M, 1, 2)) < 06, 'Night', "Day")) %>%
  mutate(date_column = as.Date(Date_time, format = "%Y-%m-%d")) %>%
  mutate(peak_status = ifelse(date_column %in% full_moon_dates, "Peak", "Off-Peak"))
  #rename_at(3,~'peak_status') %>%#identifies all .csv files associated with Dai15 full water column Sv measurements and compiles them into one data frame
exports_stn1$Sv_Ln = 10^(exports_stn1$Sv_mean/10)
daily_avg_stn1<- data.frame(aggregate(Sv_Ln ~ cut(Date_time, "24 hour") + DayNight, exports_stn1, mean))
colnames(daily_avg_stn1)[1]<- 'Date_time'
daily_avg_stn1$Date_time<- as.POSIXct(daily_avg_stn1$Date_time, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")
daily_avg_stn1$Sv_log = 10*log10(daily_avg_stn1$Sv_Ln)
daily_avg_stn1$date_column<- as.Date(daily_avg_stn1$Date_time, format = "%Y-%m-%d")
daily_avg_stn1$peak_status <- ifelse(daily_avg_stn1$date_column %in% full_moon_dates, "Peak", "Off-Peak")
daily_avg_stn1$Days <- as.numeric(difftime(daily_avg_stn1$date_column, min(daily_avg_stn1$date_column), units = "days")) + 1


exports_stn2$Sv_Ln = 10^(exports_stn2$Sv_mean/10)
daily_avg_stn2<- data.frame(aggregate(Sv_Ln ~ cut(Date_time, "24 hour") + DayNight, exports_stn2, mean))
colnames(daily_avg_stn2)[1]<- 'Date_time'
daily_avg_stn2$Date_time<- as.POSIXct(daily_avg_stn2$Date_time, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")
daily_avg_stn2$Sv_log = 10*log10(daily_avg_stn2$Sv_Ln) #
#full_moon_dates<- as.Date(c('2022-12-08', '2023-01-07'))
daily_avg_stn2$date_column<- as.Date(daily_avg_stn2$Date_time, format = "%Y-%m-%d")
daily_avg_stn2$peak_status <- ifelse(daily_avg_stn2$date_column %in% full_moon_dates, "Peak", "Off-Peak")
daily_avg_stn2$Days <- as.numeric(difftime(daily_avg_stn2$date_column, min(daily_avg_stn2$date_column), units = "days")) + 1


}
#########################################################################################################################################################################################################
setwd(data_analysis_output) #sets working directory to where the graphs are to be saved
#DAILY#
Sv_time_plot_stn1<- ggplot(daily_avg_stn1, aes(x = date_column, y = Sv_log, color = DayNight)) + geom_point(size = 3, show.legend = T) + 
  geom_vline(xintercept = full_moon_dates, color = "black") +
  geom_vline(xintercept = (daily_avg_stn1$date_column[daily_avg_stn1$peak_status == "Peak"]), color = "black") +
  scale_x_date(date_breaks = '5 days', date_labels = '%b-%d') + #scale_color_manual(values = c('darkgrey', 'black')) +
  #scale_y_continuous(breaks = function(y) seq(floor(min(y, digits = 1)), ceiling(max(y, digits = 1)), by = 1)) +
  scale_y_continuous(limits = c(-75,-50, by =3)) +
  labs(y = "Sv mean 12 Hour Avg (dB)", x = "Date") + labs(color = 'Time of Day') + theme_bw() +#ggtitle("2020-21 Dai 15E Sv mean 12 hour Day/Night") + 
  theme(axis.text.x = element_text(), legend.position = c(0.1,0.90)) 
#ggsave("Sv_12hour_Plot_stn1.pdf", plot = Sv_time_plot_stn1, width =10, height =6, units = c("in"), dpi = 600) #saves the graph as a pdf
#plot(Sv_time_plot_stn1)
ggsave(paste0(prefix,"_Sv_12hour_Plot_15Eonly.png"), plot = Sv_time_plot_stn1, width =10, height =6, units = c("in"), dpi = 600) #saves the graph as a png
message("Exported stn1 12hour plot: Sv_12hour_Plot_15Eonly.png")


#new_row<- c('2021-01-28 18:00:00', 'Day', 0, 0, '2021-01-28', 'Peak', 59)
#daily_avg_stn2<- rbind(daily_avg_stn2, new_row)  
#daily_avg_stn2$Sv_log<- as.numeric(daily_avg_stn2$Sv_log)
Sv_time_plot_stn2<- ggplot(daily_avg_stn2, aes(x = date_column, y = Sv_log, color = DayNight)) + geom_point(size = 3, show.legend = T) + 
  geom_vline(xintercept = full_moon_dates, color = "black") +
  geom_vline(xintercept = (daily_avg_stn2$date_column[daily_avg_stn2$peak_status == "Peak"]), color = "black") +
  labs(y = "Sv Mean 12 Hour Avg (dB)", x = "Date") + #ggtitle("Dai 3C Sv mean 12 hour Day/Night") +
  scale_x_date(date_breaks = '5 days', date_labels = '%b-%d') + #scale_color_manual(values = c('darkgrey', 'black')) +
  #scale_y_continuous(breaks = function(y) seq(floor(min(y, digits = 1)), ceiling(max(y, digits = 1)), by = 1)) +
  scale_y_continuous(limits = c(-75,-50, by =3)) + labs(color = 'Time of Day') + theme_bw() +
  theme(axis.text.x = element_text(), legend.position = c(0.1,0.90))
#ggsave(prefix+"_Sv_12hour_Plot_stn2.pdf", plot = Sv_time_plot_stn2,  width =10, height =6, units = c("in"), dpi = 600) #saves the graph as a pdf
#plot(Sv_time_plot_stn2)
ggsave(paste0(prefix,"_Sv_12hour_Plot_Dai3C_only.png"), plot = Sv_time_plot_stn2,  width =10, height =6, units = c("in"), dpi = 600) #saves the graph as a png
message("Exported stn2 12hour plot: Sv_12hour_Plot_Dai3C_only.png")

#HOURLY################################################################################

Sv_hourly_stn1<- ggplot(exports_stn1, aes(x = Date_time, y = Sv_mean)) + geom_point() + 
  geom_vline(xintercept = (exports_stn1$date_column[exports_stn1$peak_status == "Peak"]), color = "darkgrey") +
  geom_smooth(formula = y ~ x, method = loess, se = T) + labs(y = "Sv mean Hourly (dB)", x = "Date/Time") + ggtitle("Dai 15E Sv mean Hourly") +  
  scale_x_datetime(name = "Date/Time", breaks = c("1 week"), date_labels = "%b-%d") +
  scale_y_continuous(limits = c(-75,-50, by =3)) +
  #scale_y_continuous(breaks = function(y) seq(floor(min(y, digits = 1)), ceiling(max(y, digits = 1)), by = 1)) +
  theme(axis.text.x = element_text())
ggsave(paste0(prefix,"_Sv_Hourly_Plot_stn1.pdf"), plot = Sv_hourly_stn1,  width =10, height = 6, units = c("in"), dpi = 600) #saves the graph as a pdf
ggsave(paste0(prefix,"_Sv_Hourly_Plot_stn1.png"), plot = Sv_hourly_stn1,  width =10, height = 6, units = c("in"), dpi = 600) #saves the graph as a png
#plot(Sv_hourly_stn1)
message("Exported hourly stn1 plot (png and pdf): Sv_Hourly_Plot_stn1.png Sv_Hourly_Plot_stn1.pdf")

Sv_hourly_stn2<- ggplot(exports_stn2, aes(x = Date_time, y = Sv_mean)) + geom_point() + 
  geom_vline(xintercept = (exports_stn1$date_column[exports_stn1$peak_status == "Peak"]), color = "darkgrey") +
  geom_smooth(formula = y ~ x, method = loess, se = T) + 
  labs(y = "Sv mean Hourly (dB)", x = "Date/Time") + ggtitle("Dai 3C Sv mean Hourly") +
  scale_x_datetime(name = "Date/Time", breaks = c("1 week"), date_labels = "%b-%d") +
  scale_y_continuous(limits = c(-75,-50, by =3)) +
  #scale_y_continuous(breaks = function(y) seq(floor(min(y, digits = 1)), ceiling(max(y, digits = 1)), by = 1)) +
  theme(axis.text.x = element_text())
ggsave(paste0(prefix,"_Sv_Hourly_Plot_stn2.pdf"), plot = Sv_hourly_stn2,  width =10, height = 6, units = c("in"), dpi = 600) #saves the graph as a pdf
ggsave(paste0(prefix,"_Sv_Hourly_Plot_stn2.png"), plot = Sv_hourly_stn2,  width =10, height = 6, units = c("in"), dpi = 600) #saves the graph as a png
#plot(Sv_hourly_stn2)
message("Exported hourly stn2 plot (png and pdf): Sv_Hourly_Plot_stn2.png Sv_Hourly_Plot_stn2.pdf")



#######CDF#########################################################################################################################################

# sumstn1<- sum(exports_stn1$Sv_Ln) #creates a factor that is the sum of the linear Sv data
# divstn1<- (exports_stn1$Sv_Ln/sumstn1) #Divides the each point of the linear Sv data by the sum pf the linear Sv data
# CDFstn1<- cumsum(divstn1) #creates a factor of the cumulative sum of each linear Sv data point after it is divided by the total sum
# CDFstn1<- data.frame(CDFstn1) #turns the sumulative sum data into a data frame
# exports_stn1 <- cbind(exports_stn1, CDFstn1) #binds the cumulative sum data to the exports_stn1 data frame
# 
# cdfstn1_plot<- ggplot(exports_stn1, aes(x = Date_time, y = CDFstn1)) +  #creates the continuous density function plot for Dai15
#   period1 +
#   period2 +
#   geom_point() +
#   labs(y = "CDF", x = "Date/Time") + ggtitle("CDF Dai stn1") +
#   scale_x_datetime(name = "Date/Time", breaks = c("1 week"), date_labels = "%b-%d") +
#   theme(axis.text.x = element_text())
# ggsave(prefix+"_CDF_Plot_stn1.pdf", plot = cdfstn1_plot,  width =10, height = 6, units = c("in"), dpi = 600) #saves the graph as a pdf
# ggsave(prefix+"_CDF_Plot_stn1.png", plot = cdfstn1_plot,  width =10, height = 6, units = c("in"), dpi = 600) #saves the graph as a png
# #plot(cdfstn1_plot)
# message("Exported stn1 CDF plot (png and pdf): CDF_Plot_stn1.png CDF_Plot_stn1.pdf")
# 
# sumstn2<- sum(exports_stn2$Sv_Ln) #creates a factor that is the sum of the linear Sv data
# divstn2<- (exports_stn2$Sv_Ln/sumstn2) #Divides the each point of the linear Sv data by the sum pf the linear Sv data
# CDFstn2<- cumsum(divstn2) #creates a factor of the cumulative sum of each linear Sv data point after it is divided by the total sum
# CDFstn2<- data.frame(CDFstn2) #turns the sumulative sum data into a data frame
# exports_stn2 <- cbind(exports_stn2, CDFstn2) #binds the cumulative sum data to the exports_stn2 data frame
# 
# cdfstn2_plot<- ggplot(exports_stn2, aes(x = Date_time, y = CDFstn2)) + #creates the continuous density function plot for Dai3
#   period1 +
#   period2 +
#   geom_point() +
#   scale_x_datetime(name = "Date/Time", breaks = c("1 week"), date_labels = "%b-%d") +
#   labs(y = "CDF", x = "Date/Time") + ggtitle("CDF Dai stn2") +
#   theme(axis.text.x = element_text())
# ggsave(paste0(prefix,"_CDF_Plot_stn2.pdf"), plot = cdfstn2_plot,  width =10, height = 6, units = c("in"), dpi = 600) #saves the graph as a pdf
# ggsave(paste0(prefix,"_CDF_Plot_stn2.png"), plot = cdfstn2_plot,  width =10, height = 6, units = c("in"), dpi = 600) #saves the graph as a png
# #plot(cdfstn2_plot)
# message("Exported stn2 CDF plot (png and pdf): CDF_Plot_stn2.png CDF_Plot_stn2.pdf")

  
write.csv(exports_stn1, paste0(prefix,"_Sv_exports_stn1.csv")) #saves the exports_stn1 dataframe as a .csv file for later viewing
message("Exported stn1 dataframe: Sv_exports_stn1.csv")
write.csv(exports_stn2, paste0(prefix,"_Sv_exports_stn2.csv")) #saves the exports_stn2 dataframe as a .csv file for later viewing
message("Exported stn2 dataframe: Sv_exports_stn2.csv")
write.csv(daily_avg_stn1, paste0(prefix,"_Sv_daily_avg_stn1.csv")) #saves the SV_daily_avg_stn1.csv dataframe as a .csv file for later viewing
message("Exported daily avg stn1 dataframe: Sv_daily_avg_stn1.csv")
write.csv(daily_avg_stn2, paste0(prefix,"_v_daily_avg_stn2.csv")) #saves the SV_daily_avg_stn2.csv dataframe as a .csv file for later viewing
message("Exported daily avg stn2 dataframe: Sv_daily_avg_stn2.csv")
save.image(paste0(prefix,"_OGIVE_CDF_PDF_Final.RData")) #saves the R workspace to a file for later viewing
message("Exported workspace as TS2021-22_OGIVE_CDF_PDF_Final.RData")

#{Sys.sleep(5)} #causes the script to pause for 5 seconds
message("Finished running script #1, runnng next script\n\n") #message about the next script to be run
#{Sys.sleep(5)} #causes the script to sleep for 5 seconds
source(bottom_line) #runs the next script