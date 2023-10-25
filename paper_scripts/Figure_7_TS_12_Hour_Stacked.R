#NECESSARYPACKAGES###########################
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
#2020-21#####################################################################################
##Directoru_Definitions##########################
input_all_data<- "Z:/fishproj/Cambodia Dai project/Analytic/Analytic Products/TS2020_Data_Exports/TS_Data_-80/" #TS_Data_-80/
full_moon_dates<- as.Date(c('2020-11-30', '2020-12-30', '2021-01-29'))
data_analysis_output = "Z:/fishproj/Cambodia Dai project/Papers/Analysis Paper/Pics, Plot, Figures/2020-21/"
##Target_Strength_Calculations####################################
setwd(input_all_data)
{
single_targets_full_stn1 <- list.files(path = input_all_data,     # Identify all csv files in folder
                                       pattern = "^15E.*60TargetStrength_100m.sv.csv", full.names = TRUE) %>% 
  map_dfr(read_csv) %>%
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Date_M),"%Y%m%d"), Time_M, sep=" "),format = "%Y-%m-%d %H:%M", tz="Asia/Bangkok")) %>%
  mutate(DayNight = if_else(as.numeric(substring(Time_M, 1, 2)) >= 18 | as.numeric(substring(Time_M, 1, 2)) < 06, 'Night', "Day")) %>%
  mutate(date_column = as.Date(Date_time, format = "%Y-%m-%d")) %>%
  mutate(peak_status = ifelse(date_column %in% full_moon_dates, "Peak", "Off-Peak"))
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
hour12_TS_avg_stn1$TOD<- as.factor(format(hour12_TS_avg_stn1$Date_time, "%p"))
hour12_TS_avg_stn1_2020<- hour12_TS_avg_stn1 %>%
  mutate(Date_time = as.POSIXct(Date_time, format = "%Y-%m-%d %H:%M:%S",tz ="Asia/Bangkok"),
         Week = as.numeric(floor(difftime(Date_time, min(Date_time), units = "weeks") + 1))) %>%
  #mutate(TOD = if_else(as.numeric(substring(Date_time, 1, 2)) >= 18 | as.numeric(substring(Date_time, 1, 2)) < 06, 'Night', "Day")) %>%
  mutate(date_column = as.Date(Date_time, format = "%Y-%m-%d")) %>%
  mutate(peak_status = ifelse(date_column %in% full_moon_dates, "Peak", "Off-Peak"))
}

hour_12_TS_2020<- ggplot(hour12_TS_avg_stn1_2020, aes(x = date_column, y = TS_log, color = TOD)) +
  # annotate('segment', x= as.Date("2020-11-30"), xend= as.Date("2020-11-30"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  # annotate('segment', x= as.Date("2020-12-30"), xend= as.Date("2020-12-30"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  # annotate('segment', x= as.Date("2021-01-29"), xend= as.Date("2021-01-29"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  geom_point(size = 3) + labs(color = 'Time of Day') + scale_color_discrete(name = "Time of Day", labels = c("Day", "Night")) +#ggtitle("2020-21 12 Hour Single Target Detections Full Water Column Dai 15E") + #scale_color_manual(values = c('darkgrey', 'black')) + 
  #geom_vline(xintercept = (single_targets_full_stn1$date_column[single_targets_full_stn1$peak_status == "Peak"]), color = "black") +
  geom_vline(xintercept = full_moon_dates, color = "black") +
  #scale_y_continuous(breaks = function(y) seq(floor(min(y, digits = 1)), ceiling(max(y, digits = 1)), by = 1)) +
  scale_y_continuous(limits = c(-70, -50, by = 3), breaks = seq(-70,-50, by = 3)) + #geom_hline(yintercept = -50) +
  ylab(~paste('Target Strength (dB re 1 m'^'2'*')')) + theme_bw() + #scale_y_continuous(limits = c(-75,-50, by =3)) + 
  scale_x_date(name = "Date", breaks = c("7 days"), date_labels = "%b-%d", limits = c(as.Date("2020-11-11"), NA)) + 
  theme(axis.text.x = element_text(color = 'black', size = 10), 
        axis.text.y = element_text(color = 'black', size = 12),
        axis.title.y = element_text(hjust = 0.2),
        legend.position = c('none'), legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12), panel.border = element_rect(colour = "black", fill=NA), 
        panel.grid.minor.y = element_blank())
plot(hour_12_TS_2020)
ggsave("2020-21_TS_12Hour_Plot_stn1.png", plot = hour_12_TS_2020,  width =10, height = 6, units = c("in"), dpi = 600)
#2021-22################################
##Directory_Definitions##########################
input_all_data = "Z:/fishproj/Cambodia Dai project/Analytic/Analytic Products/TS2021-22_Data_Exports/Dai 15 only/TS_Data_-80/"
full_moon_dates<- as.Date(c('2021-11-19', '2021-12-19', '2022-01-18', '2022-02-16'))
data_analysis_output = "Z:/fishproj/Cambodia Dai project/Papers/Analysis Paper/Pics, Plot, Figures/2021-22/"
##Target_Strength_Calculations##############################################
setwd(input_all_data)
{
single_targets_full_stn1 <- list.files(path = input_all_data,     # Identify all csv files in folder
                                       pattern = "^15E.*60TargetStrength_100m.sv.csv", full.names = TRUE) %>% 
  map_dfr(read_csv) %>%
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Date_M),"%Y%m%d"), Time_M, sep=" "),format = "%Y-%m-%d %H:%M", tz="Asia/Bangkok")) %>%
  mutate(DayNight = if_else(as.numeric(substring(Time_M, 1, 2)) >= 18 | as.numeric(substring(Time_M, 1, 2)) < 06, 'Night', "Day")) %>%
  mutate(date_column = as.Date(Date_time, format = "%Y-%m-%d")) %>%
  mutate(peak_status = ifelse(date_column %in% full_moon_dates, "Peak", "Off-Peak"))
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
hour12_TS_avg_stn1$TOD<- as.factor(format(hour12_TS_avg_stn1$Date_time, "%p"))
hour12_TS_avg_stn1_2021<- hour12_TS_avg_stn1 %>%
  mutate(Date_time = as.POSIXct(Date_time, format = "%Y-%m-%d %H:%M:%S",tz ="Asia/Bangkok"),
         Week = as.numeric(floor(difftime(Date_time, min(Date_time), units = "weeks") + 1))) %>%
  #mutate(TOD = if_else(as.numeric(substring(Date_time, 1, 2)) >= 18 | as.numeric(substring(Date_time, 1, 2)) < 06, 'Night', "Day")) %>%
  mutate(date_column = as.Date(Date_time, format = "%Y-%m-%d")) %>%
  mutate(peak_status = ifelse(date_column %in% full_moon_dates, "Peak", "Off-Peak"))
}

hour_12_TS_2021<- ggplot(hour12_TS_avg_stn1_2021, aes(x = date_column, y = TS_log, color = TOD)) +
  # annotate('segment', x= as.Date("2021-11-19"), xend= as.Date("2021-11-19"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  # annotate('segment', x= as.Date("2021-12-19"), xend= as.Date("2021-12-19"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  # annotate('segment', x= as.Date("2022-01-18"), xend= as.Date("2022-01-18"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  # annotate('segment', x= as.Date("2022-02-16"), xend= as.Date("2022-02-16"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  geom_point(size = 3) + labs(color = 'Time of Day') + scale_color_discrete(name = "Time of Day", labels = c("Day", "Night")) +#ggtitle("2020-21 12 Hour Single Target Detections Full Water Column Dai 15E") + #scale_color_manual(values = c('darkgrey', 'black')) + 
  #geom_vline(xintercept = (single_targets_full_stn1$date_column[single_targets_full_stn1$peak_status == "Peak"]), color = "black") +
  geom_vline(xintercept = full_moon_dates, color = "black") +
  #scale_y_continuous(breaks = function(y) seq(floor(min(y, digits = 1)), ceiling(max(y, digits = 1)), by = 1)) +
  scale_y_continuous(limits = c(-70, -50, by = 3), breaks = seq(-70,-50, by = 3)) +
  ylab(~paste('Target Strength (dB re 1 m'^'2'*')')) + theme_bw() + #scale_y_continuous(limits = c(-75,-50, by =3)) + 
  scale_x_date(name = "Date", breaks = c("7 days"), date_labels = "%b-%d", limits = c(as.Date("2021-11-11"), NA)) + #geom_hline(yintercept = -50) + 
  theme(axis.text.x = element_text(color = 'black', size = 10), 
        axis.text.y = element_text(color = 'black', size = 12),
        axis.title.y = element_text(hjust = 0.2),
        legend.position = c('none'), legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12), axis.line = element_line(color = 'black'),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.grid.minor.y = element_blank()) #legend.position = c(.06,0.90)
plot(hour_12_TS_2021)
ggsave("TEST2021-22_TS_12Hour_Plot_stn1.png", plot = hour_12_TS_2021,  width =10, height = 6, units = c("in"), dpi = 600)
#2022-23####################################################################
##Directory_Definitions#####################################################
input_all_data = "Z:/fishproj/Cambodia Dai project/Analytic/Analytic Products/TS2022-23_Data_Exports/stn1 ONLY/TS_Data_-80/"
full_moon_dates<- as.Date(c('2022-11-08', '2022-12-07', '2023-01-06', '2023-02-05'))
data_analysis_output = "Z:/fishproj/Cambodia Dai project/Papers/Analysis Paper/Pics, Plot, Figures/2022-23/"
##Target_Strength_Calculations##############################################
setwd(input_all_data)
{
single_targets_full_stn1 <- list.files(path = input_all_data,     # Identify all csv files in folder
                                       pattern = "^15E.*60TargetStrength_100m.sv.csv", full.names = TRUE) %>% 
  map_dfr(read_csv) %>%
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Date_M),"%Y%m%d"), Time_M, sep=" "),format = "%Y-%m-%d %H:%M", tz="Asia/Bangkok")) %>%
  mutate(DayNight = if_else(as.numeric(substring(Time_M, 1, 2)) >= 18 | as.numeric(substring(Time_M, 1, 2)) < 06, 'Night', "Day")) %>%
  mutate(date_column = as.Date(Date_time, format = "%Y-%m-%d")) %>%
  mutate(peak_status = ifelse(date_column %in% full_moon_dates, "Peak", "Off-Peak"))
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
hour12_TS_avg_stn1$TOD<- as.factor(format(hour12_TS_avg_stn1$Date_time, "%p"))
hour12_TS_avg_stn1_2022<- hour12_TS_avg_stn1 %>%
  mutate(Date_time = as.POSIXct(Date_time, format = "%Y-%m-%d %H:%M:%S",tz ="Asia/Bangkok"),
         Week = as.numeric(floor(difftime(Date_time, min(Date_time), units = "weeks") + 1))) %>%
  #mutate(TOD = if_else(as.numeric(substring(Date_time, 1, 2)) >= 18 | as.numeric(substring(Date_time, 1, 2)) < 06, 'Night', "Day")) %>%
  mutate(date_column = as.Date(Date_time, format = "%Y-%m-%d")) %>%
  mutate(peak_status = ifelse(date_column %in% full_moon_dates, "Peak", "Off-Peak"))
}
setwd(data_analysis_output)

hour_12_TS_2022<- ggplot(hour12_TS_avg_stn1_2022, aes(x = date_column, y = TS_log, color = TOD)) +
  # annotate('segment', x= as.Date("2020-11-30"), xend= as.Date("2020-11-30"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  # annotate('segment', x= as.Date("2023-01-06"), xend= as.Date("2023-01-06"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  # annotate('segment', x= as.Date("2022-11-08"), xend= as.Date("2022-11-08"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  geom_point(size = 3) + labs(color = 'Time of Day') + scale_color_discrete(name = "Time of Day", labels = c("Day", "Night")) + #ggtitle("2020-21 12 Hour Single Target Detections Full Water Column Dai 15E") + #scale_color_manual(values = c('darkgrey', 'black')) + 
  #geom_vline(xintercept = (single_targets_full_stn1$date_column[single_targets_full_stn1$peak_status == "Peak"]), color = "black") +
  geom_vline(xintercept = full_moon_dates, color = "black") +
  scale_y_continuous(limits = c(-70, -50, by = 3), breaks = seq(-70,-50, by = 3)) + 
  scale_x_date(name = "Date", breaks = c("7 days"), date_labels = "%b-%d", limits = c(as.Date("2022-11-07"), NA)) +
  #scale_y_continuous(breaks = function(y) seq(floor(min(y, digits = 1)), ceiling(max(y, digits = 1)), by = 1)) +
  ylab(~paste('Target Strength (dB re 1 m'^'2'*')')) + theme_bw() + #geom_hline(yintercept = -50) + 
  theme(axis.text.x = element_text(color = 'black', size = 10), 
        axis.text.y = element_text(color = 'black', size = 12),
        axis.title.y = element_text(hjust = 0.2),
        legend.position = c('none'), legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12), axis.line = element_line(color = 'black'),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.grid.minor.y = element_blank())
#theme(axis.text.x = element_text(), legend.position = c(.1,0.90))
plot(hour_12_TS_2022)
ggsave("2022-23_TS_12Hour_Plot_stn1.png", plot = hour_12_TS_2022,  width =10, height = 6, units = c("in"), dpi = 600)
#ggarrange##############################################################################
{
fig<- ggarrange(hour_12_TS_2020 + rremove("ylab") + rremove("xlab"), 
                hour_12_TS_2021 + rremove("ylab") + rremove("xlab"),
                hour_12_TS_2022 + rremove("ylab") + rremove("xlab"),
                ncol = 1, nrow = 3, common.legend = T, legend = 'top') #labels = c('A', 'B', 'C'), 
fig<- annotate_figure(fig, left = textGrob(~paste('Target Strength (dB re 1 m'^'2'*')'), rot = 90, vjust = 1, gp = gpar(cex = 1)),
                      bottom = textGrob("Date", gp = gpar(cex = 1)))
plot(fig)
ggsave("Z:/fishproj/Cambodia Dai project/Papers/Analysis Paper/Pics, Plot, Figures/Figure_7.png", plot = fig, width =12, height =8, units = c("in"), dpi = 800) 
}
}
