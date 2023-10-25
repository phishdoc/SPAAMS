library(tidyverse)
library(lubridate)
library(plotly)
library(ggplot2)
library(ggh4x)
setwd('Z:/fishproj/Cambodia Dai project/Papers/Analysis Paper')
dates<- read.csv('deployment_dates.csv') %>%
  mutate(Start = as.POSIXct(mdy(Start))) %>%
  mutate(End = as.POSIXct(mdy(End))) %>%
  mutate(fm1 = as.POSIXct(mdy(fm1))) %>%
  mutate(fm2 = as.POSIXct(mdy(fm2))) %>%
  mutate(fm3 = as.POSIXct(mdy(fm3))) %>%
  mutate(fm4 = as.POSIXct(mdy(fm4))) %>%
  mutate(ps1 = as.POSIXct(mdy(ps1))) %>%
  mutate(pe1 = as.POSIXct(mdy(pe1))) %>%
  mutate(ps2 = as.POSIXct(mdy(ps2))) %>%
  mutate(pe2 = as.POSIXct(mdy(pe2))) 

#############testarea#################################################################################################################################################
gg2<- ggplot(dates, aes(x=Start, xend= End, y=Dai, yend=Dai, color = Dai)) + geom_segment(linewidth= 8) + facet_wrap(.~Year, dir = 'v', scales = 'free_y') +
  scale_x_datetime(name = "Date", breaks = c("1 week"), date_labels = "%b-%d") +
  geom_vline(data=filter(dates, Year=="2020-21"), aes(xintercept=fm1), colour="blue") + 
  geom_vline(data=filter(dates, Year=="2020-21"), aes(xintercept=fm2), colour="blue") + 
  geom_vline(data=filter(dates, Year=="2020-21"), aes(xintercept=fm3), colour="blue") + 
  geom_vline(data=filter(dates, Year=="2020-21"), aes(xintercept=fm4), colour="blue") + 
  #annotate("point", y = 1, x = as.POSIXct('2020-11-27'), colour = "blue") +
  geom_point(data = filter(dates, Year == '2020-21'), aes(y = 1, x = as.POSIXct('2020-11-27')), color = 'black') +
  geom_point(data = filter(dates, Year == '2020-21'), aes(y = 1, x = as.POSIXct('2020-12-15')), color = 'black') +
  geom_point(data = filter(dates, Year == '2020-21'), aes(y = 1, x = as.POSIXct('2020-12-24')), color = 'black') +
  geom_point(data = filter(dates, Year == '2020-21'), aes(y = 1, x = as.POSIXct('2021-01-08')), color = 'black') +
  geom_point(data = filter(dates, Year == '2020-21'), aes(y = 1, x = as.POSIXct('2021-01-23')), color = 'black') +
  geom_point(data = filter(dates, Year == '2020-21'), aes(y = 1, x = as.POSIXct('2021-02-05')), color = 'black') +
  geom_point(data = filter(dates, Year == '2020-21'), aes(y = 2, x = as.POSIXct('2020-11-27')), color = 'black') +
  geom_point(data = filter(dates, Year == '2020-21'), aes(y = 2, x = as.POSIXct('2020-12-15')), color = 'black') +
  geom_point(data = filter(dates, Year == '2020-21'), aes(y = 2, x = as.POSIXct('2020-12-24')), color = 'black') +
  geom_point(data = filter(dates, Year == '2020-21'), aes(y = 2, x = as.POSIXct('2021-01-08')), color = 'black') +
  geom_point(data = filter(dates, Year == '2020-21'), aes(y = 2, x = as.POSIXct('2021-01-23')), color = 'black') +
  geom_vline(data=filter(dates, Year=="2020-21", Dai == '15E'), aes(xintercept=ps1), colour="#F8766D", linetype = 'dashed', linewidth = 1)+ 
  geom_vline(data=filter(dates, Year=="2020-21", Dai == '15E'), aes(xintercept=pe1), colour="#F8766D", linetype = 'dashed', linewidth = 1) + 
  geom_vline(data=filter(dates, Year=="2020-21", Dai == '3D'), aes(xintercept=ps1), colour="#00BFC4", linetype = 'dashed', linewidth = 1) + 
  geom_vline(data=filter(dates, Year=="2020-21", Dai == '3D'), aes(xintercept=pe1), colour="#00BFC4", linetype = 'dashed', linewidth = 1) + 
  geom_vline(data=filter(dates, Year=="2020-21", Dai == '15E'), aes(xintercept=ps2), colour="#F8766D", linetype = 'dashed', linewidth = 1) + 
  geom_vline(data=filter(dates, Year=="2020-21", Dai == '15E'), aes(xintercept=pe2), colour="#F8766D", linetype = 'dashed', linewidth = 1) + 
  geom_vline(data=filter(dates, Year=="2020-21", Dai == '3D'), aes(xintercept=ps2), colour="#00BFC4", linetype = 'dashed', linewidth = 1) + 
  geom_vline(data=filter(dates, Year=="2020-21", Dai == '3D'), aes(xintercept=pe2), colour="#00BFC4", linetype = 'dashed', linewidth = 1) + 
  geom_vline(data=filter(dates, Year=="2021-22"), aes(xintercept=fm1), colour="blue") + 
  geom_vline(data=filter(dates, Year=="2021-22"), aes(xintercept=fm2), colour="blue") + 
  geom_vline(data=filter(dates, Year=="2021-22"), aes(xintercept=fm3), colour="blue") + 
  geom_vline(data=filter(dates, Year=="2021-22"), aes(xintercept=fm4), colour="blue") + 
  geom_point(data = filter(dates, Year == '2021-22'), aes(y = 1, x = as.POSIXct('2020-12-01')), color = 'black') +
  geom_point(data = filter(dates, Year == '2021-22'), aes(y = 1, x = as.POSIXct('2020-12-14')), color = 'black') +
  geom_point(data = filter(dates, Year == '2021-22'), aes(y = 1, x = as.POSIXct('2020-12-29')), color = 'black') +
  geom_point(data = filter(dates, Year == '2021-22'), aes(y = 1, x = as.POSIXct('2021-01-14')), color = 'black') +
  geom_point(data = filter(dates, Year == '2021-22'), aes(y = 1, x = as.POSIXct('2021-02-04')), color = 'black') +
  geom_point(data = filter(dates, Year == '2021-22'), aes(y = 1, x = as.POSIXct('2021-02-11')), color = 'black') +
  geom_point(data = filter(dates, Year == '2021-22'), aes(y = 2, x = as.POSIXct('2020-12-01')), color = 'black') +
  geom_point(data = filter(dates, Year == '2021-22'), aes(y = 2, x = as.POSIXct('2021-01-14')), color = 'black') +
  geom_vline(data=filter(dates, Year=="2021-22", Dai == '15E'), aes(xintercept=ps1), colour="#F8766D", linetype = 'dashed', linewidth = 1) + 
  geom_vline(data=filter(dates, Year=="2021-22", Dai == '15E'), aes(xintercept=pe1), colour="#F8766D", linetype = 'dashed', linewidth = 1) + 
  geom_vline(data=filter(dates, Year=="2021-22", Dai == '3D'), aes(xintercept=ps1), colour="#00BFC4", linetype = 'dashed', linewidth = 1) + 
  geom_vline(data=filter(dates, Year=="2021-22", Dai == '3D'), aes(xintercept=pe1), colour="#00BFC4", linetype = 'dashed', linewidth = 1) + 
  geom_vline(data=filter(dates, Year=="2021-22", Dai == '15E'), aes(xintercept=ps2), colour="#F8766D", linetype = 'dashed', linewidth = 1) + 
  geom_vline(data=filter(dates, Year=="2021-22", Dai == '15E'), aes(xintercept=pe2), colour="#F8766D", linetype = 'dashed', linewidth = 1) + 
  geom_vline(data=filter(dates, Year=="2021-22", Dai == '3D'), aes(xintercept=ps2), colour="#00BFC4", linetype = 'dashed', linewidth = 1) + 
  geom_vline(data=filter(dates, Year=="2021-22", Dai == '3D'), aes(xintercept=pe2), colour="#00BFC4", linetype = 'dashed', linewidth = 1) + 
  geom_vline(data=filter(dates, Year=="2022-23"), aes(xintercept=fm1), colour="blue") + 
  geom_vline(data=filter(dates, Year=="2022-23"), aes(xintercept=fm2), colour="blue") + 
  geom_vline(data=filter(dates, Year=="2022-23"), aes(xintercept=fm3), colour="blue") + 
  geom_vline(data=filter(dates, Year=="2022-23"), aes(xintercept=fm4), colour="blue") + 
  geom_point(data = filter(dates, Year == '2022-23'), aes(y = 1, x = as.POSIXct('2020-12-03')), color = 'black') +
  geom_point(data = filter(dates, Year == '2022-23'), aes(y = 1, x = as.POSIXct('2020-12-20')), color = 'black') +
  geom_point(data = filter(dates, Year == '2022-23'), aes(y = 1, x = as.POSIXct('2021-01-02')), color = 'black') +
  geom_point(data = filter(dates, Year == '2022-23'), aes(y = 1, x = as.POSIXct('2021-01-19')), color = 'black') +
  geom_point(data = filter(dates, Year == '2022-23'), aes(y = 1, x = as.POSIXct('2021-01-31')), color = 'black') +
  geom_point(data = filter(dates, Year == '2022-23'), aes(y = 1, x = as.POSIXct('2021-02-17')), color = 'black') +
  geom_point(data = filter(dates, Year == '2022-23'), aes(y = 2, x = as.POSIXct('2020-12-03')), color = 'black') +
  geom_point(data = filter(dates, Year == '2022-23'), aes(y = 2, x = as.POSIXct('2020-12-20')), color = 'black') +
  geom_point(data = filter(dates, Year == '2022-23'), aes(y = 2, x = as.POSIXct('2021-01-02')), color = 'black') +
  geom_point(data = filter(dates, Year == '2022-23'), aes(y = 2, x = as.POSIXct('2021-01-19')), color = 'black') +
  geom_point(data = filter(dates, Year == '2022-23'), aes(y = 2, x = as.POSIXct('2021-01-31')), color = 'black') +
  geom_vline(data=filter(dates, Year=="2022-23", Dai == '15E'), aes(xintercept=ps1), colour="#F8766D", linetype = 'dashed', linewidth = 1) + 
  geom_vline(data=filter(dates, Year=="2022-23", Dai == '15E'), aes(xintercept=pe1), colour="#F8766D", linetype = 'dashed', linewidth = 1) + 
  geom_vline(data=filter(dates, Year=="2022-23", Dai == '3D'), aes(xintercept=ps1), colour="#00BFC4", linetype = 'dashed', linewidth = 1) + 
  geom_vline(data=filter(dates, Year=="2022-23", Dai == '3D'), aes(xintercept=pe1), colour="#00BFC4", linetype = 'dashed', linewidth = 1) + 
  geom_vline(data=filter(dates, Year=="2022-23", Dai == '15E'), aes(xintercept=ps2), colour="#F8766D", linetype = 'dashed', linewidth = 1) + 
  geom_vline(data=filter(dates, Year=="2022-23", Dai == '15E'), aes(xintercept=pe2), colour="#F8766D", linetype = 'dashed', linewidth = 1) + 
  geom_vline(data=filter(dates, Year=="2022-23", Dai == '3D'), aes(xintercept=ps2), colour="#00BFC4", linetype = 'dashed', linewidth = 1) + 
  geom_vline(data=filter(dates, Year=="2022-23", Dai == '3D'), aes(xintercept=pe2), colour="#00BFC4", linetype = 'dashed', linewidth = 1) + 
  #scale_y_discrete(expand = c(0, 1.9)) +
  # force_panelsizes(rows = unit(runif(3) + 3, "cm"), 
  #                 cols = unit(c(10, 10, 10), "cm"), TRUE) + 
  #scale_color_discrete(limits=c('3D', '15E'), labels=c('3D', '15E')) +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, color = 'black', size = 10), plot.margin = unit(c(1,1,1,1),"cm"), element_line(size =1), 
                     panel.grid.minor = element_blank(), 
                     legend.position = c('top'),
                     axis.text.y = element_text(color = 'black', size = 12), 
                     legend.title = element_text(size = 12), 
                     legend.text = element_text(size = 12), 
                     axis.title = element_text(size = 12, color = 'black'))
plot(gg2)

setwd('Z:/fishproj/Cambodia Dai project/Papers/Analysis Paper/Pics, Plot, Figures')
ggsave('Figure_1.png', plot = gg2, width =10, height =6, units = c("in"), dpi = 600)











####lineofbrokendreams#########################################################################################################################################################
#dates$Start<- as.numeric(as.character(dates$Start))
#dates$Start<- as.POSIXct(dates$Start)
#dates$End<- as.POSIXct(dates$End)


# gg<- ggplot(dates, aes(x=Start, xend= End, y=Dai, yend=Dai, color = Dai)) + geom_segment(size= 8) + facet_wrap(.~Year, dir = 'v', scales = 'free_y') +
#   scale_x_datetime(name = "Date/Time", breaks = c("1 week"), date_labels = "%b-%d") +
#   geom_rect(data = data.frame(Year = '2021-22'), aes(xmin= as.POSIXct("2020-12-13"), xmax= as.POSIXct("2020-12-17"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill="blue", inherit.aes = FALSE) +
#   geom_rect(data = data.frame(Year = '2021-22'), aes(xmin= as.POSIXct("2021-01-11"), xmax= as.POSIXct("2021-01-15"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill="blue", inherit.aes = FALSE) +
#   geom_rect(data = data.frame(Year = '2021-22'), aes(xmin= as.POSIXct("2021-02-10"), xmax= as.POSIXct("2021-02-14"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill="blue", inherit.aes = FALSE) +
#   geom_rect(data = data.frame(Year = '2021-22'), aes(xmin= as.POSIXct("2020-11-13"), xmax= as.POSIXct("2020-11-17"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill="blue", inherit.aes = FALSE) +
#   geom_rect(data = data.frame(Year = '2020-21'), aes(xmin= as.POSIXct("2020-11-23"), xmax= as.POSIXct("2020-11-28"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill="blue", inherit.aes = FALSE) +
#   geom_rect(data = data.frame(Year = '2020-21'), aes(xmin= as.POSIXct("2020-12-24"), xmax= as.POSIXct("2020-12-30"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill="blue", inherit.aes = FALSE) +
#   geom_rect(data = data.frame(Year = '2020-21'), aes(xmin= as.POSIXct("2021-01-23"), xmax= as.POSIXct("2021-01-29"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill="blue", inherit.aes = FALSE) +
#   geom_rect(data = data.frame(Year = '2020-21'), aes(xmin= as.POSIXct("2021-02-19"), xmax= as.POSIXct("2021-02-24"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill="blue", inherit.aes = FALSE) +
#   geom_rect(data = data.frame(Year = '2022-23'), aes(xmin= as.POSIXct("2020-12-01"), xmax= as.POSIXct("2020-12-05"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill="blue", inherit.aes = FALSE) +
#   geom_rect(data = data.frame(Year = '2022-23'), aes(xmin= as.POSIXct("2020-12-30"), xmax= as.POSIXct("2021-01-03"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill="blue", inherit.aes = FALSE) +
#   geom_rect(data = data.frame(Year = '2022-23'), aes(xmin= as.POSIXct("2021-01-29"), xmax= as.POSIXct("2021-02-02"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill="blue", inherit.aes = FALSE) +
#   geom_rect(data = data.frame(Year = '2022-23'), aes(xmin= as.POSIXct("2021-02-27"), xmax= as.POSIXct("2021-03-03"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill="blue", inherit.aes = FALSE) +
#   #scale_y_discrete(expand = c(0, 1.9)) +
#   force_panelsizes(rows = unit(runif(3) + 3, "cm"),
#                                   cols = unit(c(10, 10, 10), "cm"),
#                                    TRUE) +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5), plot.margin = unit(c(1,1,1,1),"cm"), element_line(size =1))
# plot(gg)
#force_panelsizes(rows = unit(runif(3) + 2, "cm"), 
#                 cols = unit(c(10, 10, 10), "cm"), 
#                 TRUE) +

#gridExtra::grid.arrange(gg,layout_matrix = cbind(rep(1, 3), rep(1, 3), rep(1, 3), c(NA, 2, NA))) 
#(x=as.POSIXct('2020/11/01'), xend= as.POSIXct('2021/03/01'),
# gg2<- ggplot(dates, aes(x=Start, xend= End, y=Dai, yend=Dai, color = Dai)) + geom_segment(size= 8) + facet_wrap(.~Year, dir = 'v', scales = 'free_y') +
#   scale_x_datetime(name = "Date/Time", breaks = c("1 week"), date_labels = "%b-%d") +
#   geom_rect(data = data.frame(Year = '2021-22'), aes(xmin= as.POSIXct("2020-12-16"), xmax= as.POSIXct("2020-12-17"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill="blue", inherit.aes = FALSE) +
#   geom_rect(data = data.frame(Year = '2021-22'), aes(xmin= as.POSIXct("2021-01-14"), xmax= as.POSIXct("2021-01-15"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill="blue", inherit.aes = FALSE) +
#   geom_rect(data = data.frame(Year = '2021-22'), aes(xmin= as.POSIXct("2021-02-13"), xmax= as.POSIXct("2021-02-14"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill="blue", inherit.aes = FALSE) +
#   geom_rect(data = data.frame(Year = '2021-22'), aes(xmin= as.POSIXct("2020-11-16"), xmax= as.POSIXct("2020-11-17"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill="blue", inherit.aes = FALSE) +
#   geom_rect(data = data.frame(Year = '2020-21'), aes(xmin= as.POSIXct("2020-11-27"), xmax= as.POSIXct("2020-11-28"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill="blue", inherit.aes = FALSE) +
#   geom_rect(data = data.frame(Year = '2020-21'), aes(xmin= as.POSIXct("2020-12-29"), xmax= as.POSIXct("2020-12-30"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill="blue", inherit.aes = FALSE) +
#   geom_rect(data = data.frame(Year = '2020-21'), aes(xmin= as.POSIXct("2021-01-28"), xmax= as.POSIXct("2021-01-29"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill="blue", inherit.aes = FALSE) +
#   geom_rect(data = data.frame(Year = '2020-21'), aes(xmin= as.POSIXct("2021-02-23"), xmax= as.POSIXct("2021-02-24"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill="blue", inherit.aes = FALSE) +
#   geom_rect(data = data.frame(Year = '2022-23'), aes(xmin= as.POSIXct("2020-12-04"), xmax= as.POSIXct("2020-12-05"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill="blue", inherit.aes = FALSE) +
#   geom_rect(data = data.frame(Year = '2022-23'), aes(xmin= as.POSIXct("2021-01-02"), xmax= as.POSIXct("2021-01-03"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill="blue", inherit.aes = FALSE) +
#   geom_rect(data = data.frame(Year = '2022-23'), aes(xmin= as.POSIXct("2021-02-01"), xmax= as.POSIXct("2021-02-02"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill="blue", inherit.aes = FALSE) +
#   geom_rect(data = data.frame(Year = '2022-23'), aes(xmin= as.POSIXct("2021-03-02"), xmax= as.POSIXct("2021-03-03"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill="blue", inherit.aes = FALSE) +
#   #scale_y_discrete(expand = c(0, 1.9)) +
#  # force_panelsizes(rows = unit(runif(3) + 3, "cm"), 
#   #                 cols = unit(c(10, 10, 10), "cm"), TRUE) + 
#   #scale_color_discrete(limits=c('3D', '15E'), labels=c('3D', '15E')) +
#   theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 0.5), plot.margin = unit(c(1,1,1,1),"cm"), element_line(size =1))
# plot(gg2)
# 
# setwd('Z:/fishproj/Cambodia Dai project/Papers/Analysis Paper/Pics, Plot, Figures')
# #ggsave('peaks_dates_gant_chart.pdf', plot = gg2, width =10, height =6, units = c("in"), dpi = 600)
# ggsave('peaks_dates_gant_chart.png', plot = gg2, width =10, height =6, units = c("in"), dpi = 600)




