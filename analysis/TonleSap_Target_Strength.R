#----
# TonleSap_Target_Strength.R
# Jackson Swan, 2023-03-27
# 
# Last Modified by: Mark Yamane, 2023-11-15
#---

library("tidyverse")
library("ggpubr")
library("lubridate")
#{Sys.sleep(5)}
message("Running script #3, Target Strength")
#{Sys.sleep(5)}
setwd(input_all_data)

#### stn 1 ####

single_targets_full_stn1 <- list.files(path = input_all_data,     # Identify all csv files in folder
                                       pattern = "^15E.*60TargetStrength_100m.sv.csv", full.names = TRUE) %>%
  map_dfr(read_csv, show_col_types = FALSE) %>%
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Date_M),"%Y%m%d"), Time_M, sep=" "),format = "%Y-%m-%d %H:%M", tz="Asia/Bangkok")) %>%
  mutate(DayNight = if_else(as.numeric(substring(Time_M, 1, 2)) >= 18 | as.numeric(substring(Time_M, 1, 2)) < 06, 'Night', "Day"))

single_targets_full_stn1$TS_Ln = 10^(single_targets_full_stn1$TS_mean/10)
desiredGroupingUnitTSstn1 = cut(single_targets_full_stn1$Date_time, breaks = "12 hour") #You can use substr for that also 
aggregate(single_targets_full_stn1$TS_Ln, by = list(desiredGroupingUnitTSstn1), FUN = mean)
hour12_TSonly_avg_stn1<- data.frame(aggregate(single_targets_full_stn1$TS_Ln, by = list(desiredGroupingUnitTSstn1), FUN = mean))
hour12_TSonly_avg_stn1$TS_log = 10*log10(hour12_TSonly_avg_stn1$x)
desiredGroupingUnitdepthstn1 = cut(single_targets_full_stn1$Date_time, breaks = "12 hour") #You can use substr for that also 
aggregate(single_targets_full_stn1$Target_depth_mean, by = list(desiredGroupingUnitdepthstn1), FUN = mean)
hour12_depth_avg_stn1<- data.frame(aggregate(single_targets_full_stn1$Target_depth_mean, by = list(desiredGroupingUnitdepthstn1), FUN = mean))
names(hour12_depth_avg_stn1)[2]<- "TS_depth"
hour12_TS_avg_stn1<- cbind(hour12_depth_avg_stn1[2], hour12_TSonly_avg_stn1)
hour12_TS_avg_stn1$Date_time <- as.POSIXct(hour12_TS_avg_stn1$Group.1, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")
hour12_TS_avg_stn1$AMPM<- as.factor(format(hour12_TS_avg_stn1$Date_time, "%p"))
hour12_TS_avg_stn1<- hour12_TS_avg_stn1 %>%
  mutate(Date_time = as_datetime(Date_time, tz ="Asia/Bangkok"),
         Week = as.numeric(floor(difftime(Date_time, min(Date_time), units = "weeks") + 1)))

setwd(data_analysis_output)

hourly_TS<- ggplot(single_targets_full_stn1, aes(x = Date_time, y = TS_mean, color = DayNight)) + 
  geom_point() + ggtitle("Hourly Mean Single Target Detections Full Water Column stn1") +
  #scale_y_continuous(breaks = seq(-75,-50, by = 3)) +
  scale_y_continuous(breaks = function(y) seq(floor(min(y, digits = 1)), ceiling(max(y, digits = 1)), by = 1)) +
  scale_x_datetime(name = "Date/Time", breaks = c("1 week"), date_labels = "%b-%d") 
ggsave(paste0(prefix,"_TS_Hourly_Plot_stn1.pdf"), plot = hourly_TS,  width =10, height = 6, units = c("in"), dpi = 600)
#plot(hourly_TS)
message("Exported stn1 hourly TS plot: TS_Hourly_Plot_stn1.pdf")

hour12_TS_plot <- ggplot(hour12_TS_avg_stn1, aes(x = as.POSIXct(Date_time, tz="Asia/Bangkok"), y = TS_log, color = AMPM)) + 
  geom_vline(xintercept = as.POSIXct(full_moon_dates, tz="Asia/Bangkok"), color = "black") +
  geom_point(size=2) +
  scale_color_discrete(name = "Time of Day", labels = c("Day", "Night")) +
  scale_y_continuous(limits = c(-70, -37), breaks = seq(-70,-34, by = 6), name=bquote('Target Strength (dB re 1 '~m^2*')')) +
  #scale_y_continuous(breaks = function(y) seq(floor(min(y, digits = 1)), ceiling(max(y, digits = 1)), by = 1)) +
  scale_x_datetime(limits = c(as.POSIXct('2020-11-29 20:00:00', tz='Asia/Bangkok'), as.POSIXct('2021-01-29 20:00:00', tz='Asia/Bangkok')), name = "Date", breaks = c("10 days"), date_labels = "%b-%d")  + labs(color = 'Time of Day') + theme_bw() +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        panel.grid.minor = element_blank(), 
        axis.text.y = element_text(color = 'black', size = 10), 
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10), 
        axis.title = element_text(size = 12, color = 'black'),
        legend.position = c(0.14,0.82))
ggsave(paste0(prefix,"_TS_12Hourly_Plot_stn1.eps"), device='eps', plot = hour12_TS_plot,  width = 170, height = 85, units = c("mm"), dpi = 1200)
plot(hourly_TS)
message("Exported stn1 hourly TS plot: TS_12hourly_Plot_stn1.pdf")

#load("week_avg_stn1.RData")
weekly_TS_PDF<- ggplot(hour12_TS_avg_stn1, aes(x= TS_log, y = TS_depth, color= AMPM)) + scale_y_reverse(limits= c(16,2), breaks= seq(16,2)) + 
  scale_x_continuous(breaks = function(y) seq(floor(min(y, digits = 1)), ceiling(max(y, digits = 1)), by = 3)) +
  geom_point() + facet_grid(.~ Week) + 
  geom_hline(data = week_avg_stn1, aes(yintercept = x), linetype = "solid") + theme(axis.text.x = element_text(vjust = 0.5), plot.margin = unit(c(1,1,1,1),"cm"), element_line(size =1)) +
  ylab("Target Depth (m)") + xlab("Mean Target Strength (dB)") + ggtitle("Mean TS by Depth by Sampling Period (12 hour resolution)")
ggsave(paste0(prefix,"_TS_12hourly_Daily_Facet_Plot_stn1.pdf", sep=""), plot = weekly_TS_PDF, width =10, height = 6, units = c("in"), dpi = 600)
#plot(weekly_TS_PDF)  
message("Exported stn1 12hourly-daily facet plot: TS_12hourly_Daily_Facet_Plot_stn1.pdf")

setwd(input_all_data)


temp <- lapply(list.files(path = input_all_data, pattern = "^15E.*TargetStrengthAll1Far-Field Line stn1.sv.csv", full.names = TRUE), read.csv)
tryCatch({
  output <- do.call("rbind", temp) %>%
    filter(Samples_above_PLDL !=-1) %>%# Store all files in list
    mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Ping_date),"%Y-%m-%d"), Ping_time, sep=" "), format = "%Y-%m-%d %H:%M", tz="Asia/Bangkok"))
  target_detections_all_stn1 <<- output
  }
  , error=function(e) {
    # if fileEncoding causes an error
    temp <- lapply(list.files(path = input_all_data, pattern = "^15E.*TargetStrengthAll1Far-Field Line stn1.sv.csv", full.names = TRUE),
                   function(i) {
                     read.csv(i, fileEncoding="UTF-8-BOM")
                   })
    output <- do.call("rbind", temp) %>%
      filter(Samples_above_PLDL !=-1) %>%# Store all files in list
      mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Ping_date),"%Y-%m-%d"), Ping_time, sep=" "), format = "%Y-%m-%d %H:%M", tz="Asia/Bangkok"))
    target_detections_all_stn1 <<- output
  },
  warning=function(w){
    print(w)
  }
)

target_detections_all_stn1<- target_detections_all_stn1 %>%
  mutate(Date_time = as_datetime(Date_time, tz ="Asia/Bangkok"),
         Week = as.numeric(floor(difftime(Date_time, min(Date_time), units = "days") + 1)))
GroupingUnitstn1TSA = cut(target_detections_all_stn1$Date_time, breaks = "24 hour") #You can use substr for that also 
aggregate(target_detections_all_stn1$TS_comp, by = list(GroupingUnitstn1TSA), FUN = median)
TS_median<- data.frame(aggregate(target_detections_all_stn1$TS_comp, by = list(GroupingUnitstn1TSA), FUN = median))
TS_median$Date_time <- as.POSIXct(TS_median$Group.1, format = "%Y-%m-%d", tz="Asia/Bangkok")
TS_median<- TS_median %>%
  mutate(Date_time = as_datetime(Date_time, tz ="Asia/Bangkok"),
         Week = as.numeric(floor(difftime(Date_time, min(Date_time), units = "days") + 1)))

setwd(data_analysis_output)

PDF_T<- ggplot(target_detections_all_stn1, mapping = aes(x= TS_comp)) + 
  geom_histogram(aes(y= ((..count..)/sum(..count..))), fill="blue",color="white",alpha=0.7) +
  scale_y_continuous(breaks = seq(0, .15, .01), labels = scales::percent) +
  scale_x_continuous(breaks = seq(-100,-40, by = 10)) +
  facet_wrap(.~ Week) + geom_vline(data = TS_median, aes(xintercept = x), linetype= "dashed", colour= "red4") +
  ylab("Relative Frequencies") + xlab("Compensated Target Strength (dB)") + ggtitle("Proportion of TS at Dai stn1 by Week (Full Season)")
ggsave(paste0(prefix,"_TS_Weekly_PDF_Plot_stn1.pdf"), plot = PDF_T, width =10, height = 6.5, units = c("in"), dpi = 800) 
#plot(PDF_T)
message("Exported stn1 weekly TS plot: TS_Weekly_PDF_Plot_stn1.pdf")

##VERBOSITY#####
if (SAVE_WKSPC) {
  write.csv(single_targets_full_stn1, paste0(prefix,"_Single_targets_full_water_column_stn1.csv"))
  message("Exported stn1 single targets dataframe: Single_targets_full_water_column_stn1.csv")
  write.csv(target_detections_all_stn1, paste0(prefix,"_target_detections_all_stn1.csv"))
  message("Exported stn1 all targets dataframe: target_detections_all_stn1.csv")
  write.csv(hour12_TS_avg_stn1, paste0(prefix,"_hour12_TS_avg_stn1.csv"))
  message("Exported stn1 12hourly TS dataframe: hour12_TS_avg_stn1.csv")
  save.image(paste0(prefix,"_Target_Strength_Final.RData"))
  message("Exported workspace as *_Target_Strength_Final.RData")
}

#{Sys.sleep(5)}
message("Finished running scripts, data output successful, leahaey!\n\n")