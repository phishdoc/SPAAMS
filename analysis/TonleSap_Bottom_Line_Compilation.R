#----
# TonleSap_Bottom_Line_Compilation.R
# Jackson Swan, Last Updated: 2022-08-24
# 
# Last Modified by: Mark Yamane, 2023-10-26
#---

library("tidyverse")
library("ggpubr")
library("lubridate")
#{Sys.sleep(5)}
message("Running script #2, Bottom Lines")
#{Sys.sleep(5)}
setwd(input_all_data)

# Ping_date/time = Depth_date/time for earlier exports!
depth_stn1<- list.files(path = input_all_data,     #identifies all .csv files associated with Dai3 depth measurements and compiles them into one data frame
                       pattern = "*Bottom Line stn1.line.csv", full.names = TRUE) %>% 
  map_dfr(read_csv, show_col_types = FALSE) %>%                                            # Store all files in list
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Depth_date),"%Y-%m-%d"), Depth_time, sep=" "), format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")) %>%
  mutate(date_column = as.Date(Date_time, format = "%Y-%m-%d")) # combines the date and time columns into one date/time column 
                                                         # Print data to RStudio console
depth_stn2<- list.files(path = input_all_data,     #identifies all .csv files associated with Dai15 depth measurements and compiles them into one data frame
                       pattern = "*Bottom Line stn2.line.csv", full.names = TRUE) %>% 
  map_dfr(read_csv, show_col_types = FALSE) %>%                                            # Store all files in list
  mutate(Date_time = as.POSIXct(paste(as.Date(as.character(Depth_date),"%Y-%m-%d"), Depth_time, sep=" "), format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok")) %>%
  mutate(date_column = as.Date(Date_time, format = "%Y-%m-%d")) # combines the date and time columns into one date/time column

setwd(data_analysis_output) # sets the working directory to the folder where the graphs are to be saved

desiredGroupingUnitstn1 = cut(depth_stn1$Date_time, breaks = "24 hour") # places the depth data into groups organised by 24 hours
aggregate(depth_stn1$Depth_meters, by = list(desiredGroupingUnitstn1), FUN = mean) # creates a value frame based on the means of the grouped data
avg_depth_stn1<- data.frame(aggregate(depth_stn1$Depth_meters, by = list(desiredGroupingUnitstn1), FUN = mean)) # creates a data frame out of the means of the data
avg_depth_stn1$Date_POSIXct <- as.POSIXct(avg_depth_stn1$Group.1, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok") # renames the the date/time column and ensures it is the correct format  
avg_depth_stn1$day <- 1:nrow(avg_depth_stn1)  #adds a day number column
avg_depth_stn1$AMPM<- as.factor(format(avg_depth_stn1$Date_POSIXct, "%p")) #creates a factor column called AM/PM based on the date/time column
avg_depth_stn1$day_of_year <- as.numeric(format(avg_depth_stn1$Date_POSIXct, "%j"))
avg_depth_stn1 <- avg_depth_stn1 %>% 
  mutate(Date_POSIXct = as.POSIXct(Date_POSIXct, format = "%Y-%m-%d %H:%M:%S")) %>% 
  mutate(date_column = as.Date(Date_POSIXct, format = "%Y-%m-%d")) %>%
  mutate(seq = dense_rank(Date_POSIXct))#cr

#attach(depth_stn1)
GroupingUnitstn1 = cut(depth_stn1$Date_time, breaks = "5 day") #Groups the data points by 7 day intervals 
aggregate(depth_stn1$Depth_meters, by = list(GroupingUnitstn1), FUN = mean) #Calculates the means of the 7 day interval grouped data
week_avg_stn1<- data.frame(aggregate(depth_stn1$Depth_meters, by = list(GroupingUnitstn1), FUN = mean)) #Turns the mean data into a data frame
week_avg_stn1$Date_time <- as.POSIXct(week_avg_stn1$Group.1, format = "%Y-%m-%d", tz="Asia/Bangkok") #Creates a datetime column on the weekly mean dataframe
week_avg_stn1<- week_avg_stn1 %>% #Uses the weekly datetime data column to create a column numbering each week relative to eachother. The first week of data will be numbered 1 and so on...
  mutate(Date_time = as_datetime(Date_time, tz ="Asia/Bangkok"),
         Week = as.numeric(floor(difftime(Date_time, min(Date_time), units = "week") + 1)))
#save(week_avg_stn1, file = "week_avg_stn1.RData") #Saves the dai15 avergae weekly depth dataframe to a .csv file for later use

desiredGroupingUnitstn2 = cut(depth_stn2$Date_time, breaks = "24 hour") #places the depth data into groups organised by 24 hours
aggregate(depth_stn2$Depth_meters, by = list(desiredGroupingUnitstn2), FUN = mean) # creates a value frame based on the means of the grouped data
avg_depth_stn2<- data.frame(aggregate(depth_stn2$Depth_meters, by = list(desiredGroupingUnitstn2), FUN = mean)) # creates a data frame out of the means of the data
avg_depth_stn2$Date_POSIXct <- as.POSIXct(avg_depth_stn2$Group.1, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok") # renames the the date/time column and ensures it is the correct format
avg_depth_stn2$day <- 1:nrow(avg_depth_stn2) #adds a day number column
avg_depth_stn2$AMPM<- as.factor(format(avg_depth_stn2$Date_POSIXct, "%p")) #creates a factor column called AM/PM based on the date/time column
avg_depth_stn2$day_of_year <- as.numeric(format(avg_depth_stn2$Date_POSIXct, "%j"))
avg_depth_stn2 <- avg_depth_stn2 %>% 
  mutate(Date_POSIXct = as.POSIXct(Date_POSIXct, format = "%Y-%m-%d %H:%M:%S")) %>% 
  mutate(date_column = as.Date(Date_POSIXct, format = "%Y-%m-%d")) %>%
  mutate(seq = dense_rank(Date_POSIXct))#cr

#attach(depth_stn2)
GroupingUnitstn2 = cut(depth_stn2$Date_time, breaks = "7 day") #Groups the data points by 7 day intervals 
aggregate(depth_stn2$Depth_meters, by = list(GroupingUnitstn2), FUN = mean) #Calculates the means of the 7 day interval grouped data
week_avg_stn2<- data.frame(aggregate(depth_stn2$Depth_meters, by = list(GroupingUnitstn2), FUN = mean)) #Turns the mean data into a data frame
week_avg_stn2$Date_time <- as.POSIXct(week_avg_stn2$Group.1, format = "%Y-%m-%d", tz="Asia/Bangkok") #Creates a datetime column on the weekly mean dataframe
week_avg_stn2<- week_avg_stn2 %>% #Uses the weekly datetime data column to create a column numbering each week relative to eachother. The first week of data will be numbered 1 and so on...
  mutate(Date_time = as_datetime(Date_time, tz ="Asia/Bangkok"),
         Week = as.numeric(floor(difftime(Date_time, min(Date_time), units = "weeks") + 1)))
#save(week_avg_stn2, file = "week_avg_stn2.RData") #Saves the dai15 avergae weekly depth dataframe to a .csv file for later use

#avg_depth_stn1<- data.frame(avg_depth_stn1[-c(59),]) #Removes a row from a dataframe, optional line: if two dataframes dont have equal numbers of rows, enter the dataframe name and the row to be deleted.
depth_stack<- rbind((mutate(avg_depth_stn1, sentiment = factor("Dai stn1"))), (mutate(avg_depth_stn2, sentiment = factor("Dai stn2")))) #Stacks the Dai 15 and 3 dataframes into one dataframe
depth_stack$AMPM<- as.factor(format(depth_stack$Date_POSIXct, "%p")) #Creates an day or night factor column in the dataframe 
colnames(depth_stack)[5] <- "AMPM" #Renames column 5 "AMPM"
colnames(depth_stack)[9] <- "Dai" #Renames column 5 "Dai"


reg_graph<- ggscatter(depth_stack, x = 'date_column', y = "x", color = "Dai", add = "reg.line") + geom_point(aes(color= Dai),show.legend = F) +
  scale_color_discrete(name = "Dai", labels = c("15E", "3C")) + #Creates a regression graph of both Dai 3 and 15 data, also lists regression equation and Adj. R-squared
 # stat_regline_equation(aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"), color = Dai), label.x.npc = "centre", label.y.npc = "centre") +
  scale_y_reverse(limits = c(20, 12), breaks = seq(20, 12, by = -1)) + 
  scale_x_date(date_breaks = '5 days', date_labels = '%b-%d') +
  ylab("Depth (m)") + xlab("Date") + theme_bw() + theme(
                                             legend.direction = "vertical", legend.position = c(0.1,0.90))
#plot(reg_graph)
ggsave(filename = paste0(prefix,paste0(prefix,"_depth_comparison.png")), plot = reg_graph, width =10, height = 6, units = c("in"), dpi = 600)
message("Exported depth comparison: depth_comparison.png")

##VERBOSITY#####
if (SAVE_WKSPC) {
  depthline15<- lm(x ~ day, data = avg_depth_stn1)
  summarydl15<- summary(depthline15)
  capture.output(summarydl15, file = paste0(prefix,"_depthlinestn1_stat_output.txt"))
  coef(depthline15)[2]
  
  depthline3<- lm(x ~ day, data = avg_depth_stn2)
  summarydl3<- summary(depthline3)
  capture.output(summarydl3, file= paste0(prefix,"_depthlinestn2_stat_output.txt"))
  coef(depthline3)[2]
  
  depthline_stack<- lm(day ~ x + Dai + x:Dai, data = depth_stack)
  summary(depthline_stack)
  depth_aov<- aov(depthline_stack)
  capture.output(depth_aov, file = paste0(prefix,"_depthline_comparison_stat_output.txt"))
  
  write.csv(depth_stack, paste0(prefix,"_depth_stack.csv"), row.names = T)
  
  write.csv(avg_depth_stn1, paste0(prefix,"_avg_depth_stn1.csv"), row.names = T)
  message("Exported stn1 avg depth dataframe: avg_depth_stn1.csv")
  write.csv(avg_depth_stn2, paste0(prefix,"_avg_depth_stn2.csv"), row.names = T)
  message("Exported stn2 avg depth dataframe: avg_depth_stn2.csv")
  write.csv(depth_stn1, paste0(prefix,"_depth_stn1.csv"), row.names = T)
  message("Exported stn1 depth dataframe: depth_stn1.csv")
  write.csv(depth_stn2, paste0(prefix,"_depth_stn2.csv"), row.names = T)
  message("Exported stn2 depth dataframe: depth_stn2.csv")
  write.csv(week_avg_stn1, paste0(prefix,"_week_avg_stn1.csv"), row.names = T)
  message("Exported stn1 weekly avg depth dataframe: week_avg_stn1.csv")
  save.image(paste0(prefix,"_Bottom_Line_Compilation_Final.RData"))
  message("Exported workspace as TS2021-22_Bottom_Line_Compilation_Final.RData")
}

#{Sys.sleep(5)}
message("Finished running script #2, running next script\n\n")
#{Sys.sleep(5)}
source(target_strength)