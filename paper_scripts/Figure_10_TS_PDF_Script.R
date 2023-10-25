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
target_detections_all_stn1 <- list.files(path = input_all_data,     # Identify all csv files in folder
                                         pattern = "^15E.*TargetStrengthAll1Far-Field Line 15E.sv.csv", full.names = TRUE) %>%
  map_dfr(read_csv) %>% 
  filter(Samples_above_PLDL !=-1) %>%# Store all files in list
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Ping_date),"%Y-%m-%d"), Ping_time, sep=" "), format = "%Y-%m-%d %H:%M", tz="Asia/Bangkok")) %>%
  mutate(DayNight = if_else(as.numeric(substring(Ping_time, 1, 2)) >= 18 | as.numeric(substring(Ping_time, 1, 2)) < 06, 'Night', "Day")) %>%
  mutate(Date_time = as_datetime(Date_time, tz ="Asia/Bangkok"),
         Day = as.numeric(floor(difftime(Date_time, min(Date_time), units = "days") + 1))) %>%
  mutate(date_column = as.Date(Date_time, format = "%Y-%m-%d"))

full_moon_dates_1<- seq.Date(as.Date('2020-12-20'), as.Date( '2020-12-26'), 'days') #seq.Date(as.Date('2021-12-10'), as.Date( '2021-12-14'), 'days')#
full_moon_dates_2<- seq.Date(as.Date('2021-01-14'), as.Date( '2021-01-23'), 'days') #seq.Date(as.Date('2022-01-05'), as.Date( '2022-01-09'), 'days')#
full_moon_dates_15E<- c(full_moon_dates_1, full_moon_dates_2)
target_detections_all_stn1<- target_detections_all_stn1 %>%
  mutate(peak_status = ifelse(date_column %in% full_moon_dates_15E, "Peak", "Off-Peak"))

TS_median_DN<- data.frame(aggregate(target_detections_all_stn1$TS_comp, by = list(target_detections_all_stn1$DayNight), FUN = median)) %>%
  rename_with(~ c('DayNight'), 1)

TS_median_peak<- data.frame(aggregate(target_detections_all_stn1$TS_comp, by = list(target_detections_all_stn1$peak_status), FUN = median)) %>%
  rename_with(~ c('peak'), 1)
}

setwd(data_analysis_output)

PDF_TS_DN_2020<- ggplot(target_detections_all_stn1, mapping = aes(x= TS_comp)) +
  geom_histogram(aes(y= (((..count..)/sum(..count..)))*100), fill="blue",color="white",alpha=0.7) + 
  #scale_x_continuous(breaks = function(x){seq(round(min(target_detections_all_stn1$TS_comp), -1), round(max(target_detections_all_stn1$TS_comp), -1), 3)}) + 
  #scale_y_continuous(breaks = function(y){seq(round(min(y), .01), max(y), .01)}, labels = scales::percent) + 
  #scale_y_continuous(breaks = seq(0, .15, .03), limits = c(0,.15, .03), labels = scales::percent) +
  scale_y_continuous(breaks = seq(0, 10, 3), limits = c(0,10, 3)) +
  scale_x_continuous(breaks = seq(-80,-40, by = 5), limits = c(-80,-40,5)) +
  #scale_x_continuous(breaks = ~ seq(min(.x), max(.x), 10)) +
  #scale_x_continuous(breaks = function(y) seq(floor(min(y, digits = 1)), ceiling(max(y, digits = 1)), by = 10)) +
  facet_wrap(.~ DayNight, dir = 'v') + geom_vline(data = TS_median_DN, aes(xintercept = x), linetype= "dashed", colour= "red4") +
  ylab("Relative Frequencies (%)") + xlab(~paste('Target Strength (dB re 1 m'^'2'*')')) + theme_bw() + 
  theme(axis.text.x = element_text(color = 'black', size = 12), 
        #axis.title = element_blank(), 
        axis.text.y = element_text(color = 'black', size = 12),
        axis.line = element_line(color = 'black'),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.grid.minor.y = element_blank())#+ ggtitle("Proportion of TS at Daistn1 by Week (Full Season)") + theme_bw()
plot(PDF_TS_DN_2020)
ggsave("2020-21_TS_Total_PDF_Plot_stn1_DN_-80.png", plot = PDF_TS_DN_2020, width =10, height = 6.5, units = c("in"), dpi = 800) 

model.lm = lm(TS_comp ~ DayNight, data=target_detections_all_stn1)
hist(resid(model.lm))
a1 <- aov(model.lm)
summary(a1)
a1_tukey_txt<- (TukeyHSD(a1, 'DayNight', conf.level=0.95))
print(a1_tukey_txt)
kruskal.test(TS_comp ~ DayNight, data = target_detections_all_stn1)


full_moon_dates_1<- seq.Date(as.Date('2020-12-20'), as.Date( '2020-12-26'), 'days') 
full_moon_dates_2<- seq.Date(as.Date('2021-01-14'), as.Date( '2021-01-23'), 'days') 
full_moon_dates_15E<- c(full_moon_dates_1, full_moon_dates_2)
target_detections_all_stn1<- target_detections_all_stn1 %>%
  mutate(peak_status = ifelse(date_column %in% full_moon_dates_15E, "Peak", "Off-Peak"))

TS_median_peak<- data.frame(aggregate(target_detections_all_stn1$TS_comp, by = list(target_detections_all_stn1$peak_status), FUN = median)) %>%
  rename_with(~ c('peak'), 1)

PDF_TS_peak_2020<- ggplot(target_detections_all_stn1, mapping = aes(x= TS_comp)) + 
  geom_histogram(aes(y= (((..count..)/sum(..count..)))*100), fill="blue",color="white",alpha=0.7) +
  #scale_y_continuous(breaks = seq(0, .15, .03), limits = c(0,.15, .03), labels = scales::percent) +
  scale_y_continuous(breaks = seq(0, 10, 3), limits = c(0,10, 3)) +
  scale_x_continuous(breaks = seq(-80,-40, by = 5), limits = c(-80,-40,5)) +
  #scale_x_continuous(breaks = function(x){seq(round(min(target_detections_all_stn1$TS_comp), -1), round(max(target_detections_all_stn1$TS_comp), -1), 3)}) + 
  facet_wrap(.~ peak, dir = 'v') + geom_vline(data = TS_median_peak, aes(xintercept = x), linetype= "dashed", colour= "red4") +
  ylab("Relative Frequencies (%)") + xlab(~paste('Target Strength (dB re 1 m'^'2'*')')) + theme_bw() + 
  theme(axis.text.x = element_text(color = 'black', size = 12), 
        #axis.title = element_blank(), 
        axis.text.y = element_text(color = 'black', size = 12),
        axis.line = element_line(color = 'black'),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.grid.minor.y = element_blank()) #+ ggtitle("Proportion of TS at Daistn1 by Week (Full Season)") + theme_bw()
plot(PDF_TS_peak_2020)
ggsave("2020-21_TS_Total_PDF_Plot_stn1_peak_-80.png", plot = PDF_TS_peak_2020, width =10, height = 6.5, units = c("in"), dpi = 800) 


model.lm = lm(TS_comp ~ peak_status, data=target_detections_all_stn1)
hist(resid(model.lm))
a1 <- aov(model.lm)
summary(a1)
a1_tukey_txt<- (TukeyHSD(a1, 'peak_status', conf.level=0.95))
print(a1_tukey_txt)
kruskal.test(TS_comp ~ peak_status, data = target_detections_all_stn1)

#2021-22################################
##Directory_Definitions##########################
input_all_data = "Z:/fishproj/Cambodia Dai project/Analytic/Analytic Products/TS2021-22_Data_Exports/Dai 15 only/TS_Data_-80/"
full_moon_dates<- as.Date(c('2021-11-19', '2021-12-19', '2022-01-18', '2022-02-16'))
data_analysis_output = "Z:/fishproj/Cambodia Dai project/Papers/Analysis Paper/Pics, Plot, Figures/2021-22/"
##Target_Strength_Calculations##############################################
setwd(input_all_data)

{
target_detections_all_stn1 <- list.files(path = input_all_data,     # Identify all csv files in folder
                                         pattern = "^15E.*TargetStrengthAll1Far-Field Line 15E.sv.csv", full.names = TRUE) %>%
  map_dfr(read_csv) %>% 
  filter(Samples_above_PLDL !=-1) %>%# Store all files in list
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Ping_date),"%Y-%m-%d"), Ping_time, sep=" "), format = "%Y-%m-%d %H:%M", tz="Asia/Bangkok")) %>%
  mutate(DayNight = if_else(as.numeric(substring(Ping_time, 1, 2)) >= 18 | as.numeric(substring(Ping_time, 1, 2)) < 06, 'Night', "Day")) %>%
  mutate(Date_time = as_datetime(Date_time, tz ="Asia/Bangkok"),
         Week = as.numeric(floor(difftime(Date_time, min(Date_time), units = "weeks") + 1))) %>%
  mutate(date_column = as.Date(Date_time, format = "%Y-%m-%d")) 

#target_detections_all_stn1<- target_detections_all_stn1 %>%
# mutate(Date_time = as_datetime(Date_time, tz ="Asia/Bangkok"),
#        Week = as.numeric(floor(difftime(Date_time, min(Date_time), units = "weeks") + 1))) %>%
#   mutate(date_column = as.Date(Date_time, format = "%Y-%m-%d")) 
# GroupingUnitstn1TSA = cut(target_detections_all_stn1$Date_time, breaks = "168 hour") #You can use substr for that also 
# aggregate(target_detections_all_stn1$TS_comp, by = list(GroupingUnitstn1TSA), FUN = median)
# TS_median<- data.frame(aggregate(target_detections_all_stn1$TS_comp, by = list(GroupingUnitstn1TSA), FUN = median))
# TS_median$Date_time <- as.POSIXct(TS_median$Group.1, format = "%Y-%m-%d", tz="Asia/Bangkok")
# TS_median<- TS_median %>%
#   mutate(Date_time = as_datetime(Date_time, tz ="Asia/Bangkok"),
#          Week = as.numeric(floor(difftime(Date_time, min(Date_time), units = "weeks") + 1)))
full_moon_dates_1<- seq.Date(as.Date('2021-12-10'), as.Date( '2021-12-14'), 'days')#seq.Date(as.Date('2020-12-20'), as.Date( '2020-12-26'), 'days') 
full_moon_dates_2<- seq.Date(as.Date('2022-01-05'), as.Date( '2022-01-09'), 'days')#seq.Date(as.Date('2021-01-14'), as.Date( '2021-01-23'), 'days') 
full_moon_dates_15E<- c(full_moon_dates_1, full_moon_dates_2)
target_detections_all_stn1<- target_detections_all_stn1 %>%
  mutate(peak_status = ifelse(date_column %in% full_moon_dates_15E, "Peak", "Off-Peak"))

TS_median_DN<- data.frame(aggregate(target_detections_all_stn1$TS_comp, by = list(target_detections_all_stn1$DayNight), FUN = median)) %>%
  rename_with(~ c('DayNight'), 1)

TS_mean_DN<- data.frame(aggregate(target_detections_all_stn1$TS_comp, by = list(target_detections_all_stn1$DayNight), FUN = mean)) %>%
  rename_with(~ c('DayNight'), 1)

TS_median_peak<- data.frame(aggregate(target_detections_all_stn1$TS_comp, by = list(target_detections_all_stn1$peak_status), FUN = median)) %>%
  rename_with(~ c('peak'), 1)

TS_mean_peak<- data.frame(aggregate(target_detections_all_stn1$TS_comp, by = list(target_detections_all_stn1$peak_status), FUN = mean)) %>%
  rename_with(~ c('peak'), 1)
}
setwd(data_analysis_output)
PDF_TS_DN_2021<- ggplot(target_detections_all_stn1, mapping = aes(x= TS_comp)) + 
  geom_histogram(aes(y= (((..count..)/sum(..count..)))*100), fill="blue",color="white",alpha=0.7) +
  #scale_y_continuous(breaks = seq(0, .15, .03), limits = c(0,.15, .03), labels = scales::percent) +
  scale_y_continuous(breaks = seq(0, 10, 3), limits = c(0,10, 3)) +
  #scale_x_continuous(breaks = function(x){seq(round(min(target_detections_all_stn1$TS_comp), -1), round(max(target_detections_all_stn1$TS_comp), -1), 3)}) + 
  #scale_y_continuous(breaks = function(y){seq(round(min(y), .01), max(y), .01)}, labels = scales::percent) + 
  scale_x_continuous(breaks = seq(-80,-40, by = 5), limits = c(-80,-40,5)) +
  facet_wrap(.~ DayNight, dir = 'v') + 
  geom_vline(data = TS_median_DN, aes(xintercept = x), linetype= "dashed", colour= "red4") +
  #geom_vline(data = TS_mean_DN, aes(xintercept = x), linetype= "solid", colour= "red4") +
  theme_bw() + ylab("Relative Frequencies (%)") + xlab(~paste('Target Strength (dB re 1 m'^'2'*')')) + 
  theme(axis.text.x = element_text(color = 'black', size = 12), 
        #axis.title = element_blank(), 
        axis.text.y = element_text(color = 'black', size = 12),
        axis.line = element_line(color = 'black'),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.grid.minor.y = element_blank())#+ ggtitle("Proportion of TS at Daistn1 by Week (Full Season)")
plot(PDF_TS_DN_2021)
ggsave("2021-22_TS_Total_PDF_Plot_stn1_DN_-80.png", plot = PDF_TS_DN_2021, width =10, height = 6.5, units = c("in"), dpi = 800) 


model.lm = lm(TS_comp ~ DayNight, data=target_detections_all_stn1)
hist(resid(model.lm))
a1 <- aov(model.lm)
summary(a1)
a1_tukey_txt<- (TukeyHSD(a1, 'DayNight', conf.level=0.95))
print(a1_tukey_txt)
kruskal.test(TS_comp ~ peak_status, data = target_detections_all_stn1)


PDF_TS_peak_2021<- ggplot(target_detections_all_stn1, mapping = aes(x= TS_comp)) + 
  geom_histogram(aes(y= (((..count..)/sum(..count..)))*100), fill="blue",color="white",alpha=0.7) +
  #scale_y_continuous(breaks = seq(0, .15, .03), limits = c(0,.15, .03), labels = scales::percent) +
  scale_y_continuous(breaks = seq(0, 10, 3), limits = c(0,10, 3)) +
  scale_x_continuous(breaks = seq(-80,-40, by = 5), limits = c(-80,-40,5)) +
  #scale_x_continuous(breaks = function(x){seq(round(min(target_detections_all_stn1$TS_comp), -1), round(max(target_detections_all_stn1$TS_comp), -1), 3)}) + 
  #scale_y_continuous(breaks = function(y){seq(round(min(y), .01), max(y), .02)}, labels = scales::percent) + 
  facet_wrap(.~ peak, dir = 'v') + 
  geom_vline(data = TS_median_peak, aes(xintercept = x), linetype= "dashed", colour= "red4") + 
  #geom_vline(data = TS_mean_peak, aes(xintercept = x), linetype= "solid", colour= "red4") + 
  theme_bw() + ylab("Relative Frequencies (%)") + xlab(~paste('Target Strength (dB re 1 m'^'2'*')')) + 
  theme(axis.text.x = element_text(color = 'black', size = 12), 
        #axis.title = element_blank(), 
        axis.text.y = element_text(color = 'black', size = 12),
        axis.line = element_line(color = 'black'),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.grid.minor.y = element_blank())#+ ggtitle("Proportion of TS at Daistn1 by Week (Full Season)")
plot(PDF_TS_peak_2021)
ggsave("2021-22_TS_Total_PDF_Plot_stn1_peak_-80.png", plot = PDF_TS_peak_2021, width =10, height = 6.5, units = c("in"), dpi = 800) 

model.lm = lm(TS_comp ~ peak_status, data=target_detections_all_stn1)
hist(resid(model.lm))
a1 <- aov(model.lm)
summary(a1)
a1_tukey_txt<- (TukeyHSD(a1, 'peak_status', conf.level=0.95))
print(a1_tukey_txt)
kruskal.test(TS_comp ~ peak_status, data = target_detections_all_stn1)
#2022-23####################################################################
##Directory_Definitions#####################################################
input_all_data = "Z:/fishproj/Cambodia Dai project/Analytic/Analytic Products/TS2022-23_Data_Exports/stn1 ONLY/TS_Data_-80/"
full_moon_dates<- as.Date(c('2022-11-08', '2022-12-07', '2023-01-06', '2023-02-05'))
data_analysis_output = "Z:/fishproj/Cambodia Dai project/Papers/Analysis Paper/Pics, Plot, Figures/2022-23/"
##Target_Strength_Calculations##############################################
setwd(input_all_data)

{
  target_detections_all_stn1 <- list.files(path = input_all_data,     # Identify all csv files in folder
                                           pattern = "^15E.*TargetStrengthAll1Far-Field Line stn1.sv.csv", full.names = TRUE) %>%
    map_dfr(read_csv) %>% 
    filter(Samples_above_PLDL !=-1) %>%# Store all files in list
    mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Ping_date),"%Y-%m-%d"), Ping_time, sep=" "), format = "%Y-%m-%d %H:%M", tz="Asia/Bangkok")) %>%
    mutate(DayNight = if_else(as.numeric(substring(Ping_time, 1, 2)) >= 18 | as.numeric(substring(Ping_time, 1, 2)) < 06, 'Night', "Day")) %>%
    mutate(Date_time = as_datetime(Date_time, tz ="Asia/Bangkok"),
           Week = as.numeric(floor(difftime(Date_time, min(Date_time), units = "weeks") + 1))) %>%
    mutate(date_column = as.Date(Date_time, format = "%Y-%m-%d"))
  # GroupingUnitstn1TSA = cut(target_detections_all_stn1$Date_time, breaks = "168 hour") #You can use substr for that also 
  # aggregate(target_detections_all_stn1$TS_comp, by = list(GroupingUnitstn1TSA), FUN = median)
  # TS_median<- data.frame(aggregate(target_detections_all_stn1$TS_comp, by = list(GroupingUnitstn1TSA), FUN = median))
  # TS_median$Date_time <- as.POSIXct(TS_median$Group.1, format = "%Y-%m-%d", tz="Asia/Bangkok")
  # TS_median<- TS_median %>%
  #   mutate(Date_time = as_datetime(Date_time, tz ="Asia/Bangkok"),
  #          Week = as.numeric(floor(difftime(Date_time, min(Date_time), units = "weeks") + 1)))
  
  TS_median_DN<- data.frame(aggregate(target_detections_all_stn1$TS_comp, by = list(target_detections_all_stn1$DayNight), FUN = median)) %>%
    rename_with(~ c('DayNight'), 1)  
  
  TS_mean_DN<- data.frame(aggregate(target_detections_all_stn1$TS_comp, by = list(target_detections_all_stn1$DayNight), FUN = mean)) %>%
    rename_with(~ c('DayNight'), 1)
}
setwd(data_analysis_output)

PDF_TS_DN_2022<- ggplot(target_detections_all_stn1, mapping = aes(x= TS_comp)) + 
  geom_histogram(aes(y= (((..count..)/sum(..count..)))*100), fill="blue",color="white",alpha=0.7) +
  #scale_y_continuous(breaks = seq(0, .15, .03), limits = c(0,.15, .03), labels = scales::percent) +
  scale_y_continuous(breaks = seq(0, 10, 3), limits = c(0, 10, 3)) +
  #scale_x_continuous(breaks = function(x){seq(round(min(target_detections_all_stn1$TS_comp), -1), round(max(target_detections_all_stn1$TS_comp), -1), 3)}) + 
  scale_x_continuous(breaks = seq(-80,-40, by = 5), limits = c(-80,-40,5)) +
  facet_wrap(.~ DayNight, dir = 'v') + 
  geom_vline(data = TS_median_DN, aes(xintercept = x), linetype= "dashed", colour= "red4") + 
  #geom_vline(data = TS_mean_DN, aes(xintercept = x), linetype= "solid", colour= "red4") + 
  theme_bw() + ylab("Relative Frequencies (%)") + xlab(~paste('Target Strength (dB re 1 m'^'2'*')')) + 
  theme(axis.text.x = element_text(color = 'black', size = 12), 
        #axis.title = element_blank(), 
        axis.text.y = element_text(color = 'black', size = 12),
        axis.line = element_line(color = 'black'),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.grid.minor.y = element_blank()) #+ ggtitle("Proportion of TS at Daistn1 by Week (Full Season)")
plot(PDF_TS_DN_2022)
ggsave("2022-23_TS_Total_PDF_Plot_stn1_DN_-80.png", plot = PDF_TS_DN_2022, width =10, height = 6.5, units = c("in"), dpi = 800) 

model.lm = lm(TS_comp ~ DayNight, data=target_detections_all_stn1)

a1 <- aov(model.lm)
summary(a1)
a1_txt<- summary(a1)
#capture.output(a1_txt, file= "stn1fishcatch_DN_significance_output.txt")
a1_tukey_txt<- (TukeyHSD(a1, 'DayNight', conf.level=0.95))
print(a1_tukey_txt)

kruskal.test(TS_comp ~ DayNight, data = target_detections_all_stn1)
########TSSTACKEDGRAPHS############################################################################
{
TS_hist<- ggarrange(PDF_TS_DN_2020 + rremove("ylab") + rremove("xlab"), 
                    PDF_TS_peak_2020 + rremove("ylab") + rremove("xlab") + rremove('y.text') + rremove('y.ticks'),
                    PDF_TS_DN_2021 + rremove("ylab") + rremove("xlab"), 
                    PDF_TS_peak_2021 + rremove("ylab") + rremove("xlab") + rremove('y.text') + rremove('y.ticks'), 
                    PDF_TS_DN_2022 + rremove("ylab") + rremove("xlab"),
                    ncol = 2, nrow = 3, common.legend = F, legend = 'none', widths = c(1.03, 1)) #labels = c('A', 'B', 'C'), 
TS_hist<- annotate_figure(TS_hist, left = textGrob('Relative Frequencies (%)', rot = 90, vjust = 1, gp = gpar(cex = 1)),
                          bottom = textGrob(~paste('Target Strength (dB re 1 m'^'2'*')'), gp = gpar(cex = 1)))
plot(TS_hist)
ggsave("Z:/fishproj/Cambodia Dai project/Papers/Analysis Paper/Pics, Plot, Figures/Figure_10.png", plot = TS_hist, width =12, height =8, units = c("in"), dpi = 800) 
}
}


