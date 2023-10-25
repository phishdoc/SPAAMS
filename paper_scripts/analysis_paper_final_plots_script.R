setwd('Z:/fishproj/Cambodia Dai project/Papers/SPAAMS paper/Figs, pics, plots/')



Both_Dai_Flux_2020 <- grid.arrange(Dai15E_TOD_Flux_Comparison, Dai3D_TOD_Flux_Comparison, nrow = 2)
ggsave("Both_Dai_Flux_2020-21.png", plot = Both_Dai_Flux_2020, width =10, height =6, units = c("in"), dpi = 600) 



Sv_15E_DN_Split<- split(daily_avg_stn1, daily_avg_stn1$DayNight)
x<- mean(Sv_15E_DN_Split[['Night']]$Sv_Ln)
y<- mean(Sv_15E_DN_Split[['Day']]$Sv_Ln)
Percentage_greater_15 <- ((x - y / ((x + y)/2)) * 100)     
print(Percentage_greater_15) 


Sv_3C_DN_Split<- split(daily_avg_stn2, daily_avg_stn2$DayNight)
a<- mean(Sv_3C_DN_Split[['Night']]$Sv_Ln)
b<- mean(Sv_3C_DN_Split[['Day']]$Sv_Ln)
Percentage_greater_3 <- ((a - b / ((a + b)/2)) * 100)     
print(Percentage_greater_3)

left_join(daily_avg_stn1, fish_comp_small$total_catch, by = 'date_column')

attach(daily_avg_stn1)
ggplot(daily_avg_stn1, aes(x = date_column, y = Sv_log)) + geom_point(aes(color = 'Sv')) + 
  geom_point(aes(y = fish_comp_small$total_catch/scale, color = 'Total Catch (kg)')) +
  scale_x_date(name = "Date", breaks = c("1 week"), date_labels = "%b-%d") + labs('Time of Day') +
  scale_y_continuous(sec.axis = sec_axis(~.*scale, name="Total Catch (kg)")) +
  labs(x = "Date", y = "Sv (dB)", color = "") +
  scale_color_manual(values = c("orange2", "gray30"))
  

#SVDATAPLOTTEDWITHCATCHDATA------------------------------------------------------------------------
setwd(data_analysis_output)
Sv_Catch_15E_2020_21<- ggplot(aes(x=date_column, y=Sv_log, color = DayNight), data=daily_avg_stn1_2020) + geom_point( size = 3) + 
  geom_point(aes(x=date_column, y=Total_Catch/110 - 75, shape = DayNight), data=Dai15E, size = 3, color = 'black') + scale_shape_manual(values=c(1, 4)) +
  # annotate('segment', x= as.Date("2021-11-19"), xend= as.Date("2021-11-19"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  # annotate('segment', x= as.Date("2022-01-18"), xend= as.Date("2022-01-18"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  # annotate('segment', x= as.Date("2022-02-16"), xend= as.Date("2022-02-16"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  annotate('segment', x= as.Date("2020-11-30"), xend= as.Date("2020-11-30"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  annotate('segment', x= as.Date("2021-01-29"), xend= as.Date("2021-01-29"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  geom_vline(xintercept = (daily_avg_stn1_2020$date_column[daily_avg_stn1$peak_status == "Peak"]), color = "black") +
  scale_y_continuous(name="", limits = c(-75,-50, by =3), breaks = seq(-75,-50,3), sec.axis = sec_axis(~ (. + 75) * 110, breaks = seq(0,3000,400), name="")) + 
  scale_x_date(date_breaks = '14 days', date_labels = '%b-%d') + theme_bw() + xlab('') + labs(color = 'Sv Data Time of Day', shape = 'Total Catch Time of Day') +
  theme(axis.text.x = element_text(color = 'black', size = 12), 
        axis.text.y = element_text(color = 'black', size = 12), 
        legend.position = c('none'), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12), axis.line = element_line(color = 'black'),
        panel.border = element_rect(colour = "black", fill=NA)) #legend.position = c(0.25,0.80)
plot(Sv_Catch_15E_2020_21)
ggsave("Sv_Catch_plot_15E_2020-21.png", plot = Sv_Catch_15E_2020_21, width =10, height =6, units = c("in"), dpi = 600) 


Sv_Catch_3C_2020_21<- ggplot(aes(x=date_column, y=Sv_log, color = DayNight), data=daily_avg_stn2_2020) + geom_point(size = 3) + 
  geom_point(aes(x=date_column, y=Total_Catch/110 - 75, shape = DayNight), data=Dai3C, size = 3, color = 'black') + scale_shape_manual(values=c(1, 4)) +
  annotate('segment', x= as.Date("2020-11-30"), xend= as.Date("2020-11-30"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  annotate('segment', x= as.Date("2021-01-29"), xend= as.Date("2021-01-29"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  geom_vline(xintercept = (daily_avg_stn1_2020$date_column[daily_avg_stn1$peak_status == "Peak"]), color = "black") +
  scale_y_continuous(name="", limits = c(-75,-50, by =3), breaks = seq(-75,-50,3), sec.axis = sec_axis(~ (. + 75) * 110, breaks = seq(0,3000,400), name="")) + 
  scale_x_date(date_breaks = '14 days', date_labels = '%b-%d') + theme_bw() + xlab('') + labs(color = 'Sv Data Time of Day', shape = 'Total Catch Time of Day') + #0.26,0.83
  theme(axis.text.x = element_text(color = 'black', size = 12), 
        axis.text.y = element_text(color = 'black', size = 12), 
        legend.position = c('none'), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12), axis.line = element_line(color = 'black'),
        panel.border = element_rect(colour = "black", fill=NA)) 
plot(Sv_Catch_3C_2020_21)
ggsave("Sv_Catch_plot_3C_2020-21.png", plot = Sv_Catch_3C_2020_21, width =10, height =6, units = c("in"), dpi = 600) 

ggarrange(Sv_Catch_15E_2020_21 + rremove('xlab') + rremove('ylab'), Sv_Catch_3C_2020_21, Sv_Catch_15E_2021_22, Sv_Catch_3D_2021_22 + 
            rremove('x.text'), labels = c('15E', '3C'), ncol = 2, nrow = 2, common.legend = T, legend = 'top')



Sv_Catch_15E_2021_22<- ggplot(aes(x=date_column, y=Sv_log, color = DayNight), data=daily_avg_stn1_2021) + geom_point( size = 3) + 
  geom_point(aes(x=date_column, y=Total_Catch/110 - 75, shape = DayNight), data=Dai15E, size = 3, color = 'black') + scale_shape_manual(values=c(1, 4)) +
  annotate('segment', x= as.Date("2021-11-19"), xend= as.Date("2021-11-19"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  annotate('segment', x= as.Date("2021-12-19"), xend= as.Date("2021-12-19"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  annotate('segment', x= as.Date("2022-01-18"), xend= as.Date("2022-01-18"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  annotate('segment', x= as.Date("2022-02-16"), xend= as.Date("2022-02-16"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  # annotate('segment', x= as.Date("2020-11-30"), xend= as.Date("2020-11-30"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  # annotate('segment', x= as.Date("2021-01-29"), xend= as.Date("2021-01-29"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  geom_vline(xintercept = (daily_avg_stn1_2021$date_column[daily_avg_stn1$peak_status == "Peak"]), color = "black") +
  scale_y_continuous(name="", limits = c(-75,-50, by =3), breaks = seq(-75,-50,3), sec.axis = sec_axis(~ (. + 75) * 110, breaks = seq(0,3000,400), name="")) + 
  scale_x_date(date_breaks = '14 days', date_labels = '%b-%d') + theme_bw() + xlab('') + labs(color = 'Sv Data Time of Day', shape = 'Total Catch Time of Day') + #legend.position = c(0.23,0.80),
  theme(axis.text.x = element_text(color = 'black', size = 12), 
        axis.text.y = element_text(color = 'black', size = 12), 
        legend.position = c('none'), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12), axis.line = element_line(color = 'black'),
        panel.border = element_rect(colour = "black", fill=NA)) 
plot(Sv_Catch_15E_2021_22)
ggsave("Sv_Catch_plot_15E_2021-22.png", plot = Sv_Catch_15E_2021_22, width =10, height =6, units = c("in"), dpi = 600) 


Sv_Catch_3D_2021_22<- ggplot(aes(x=date_column, y=Sv_log, color = DayNight), data=daily_avg_stn2_2021) + geom_point(size = 3) + 
  geom_point(aes(x=date_column, y=Total_Catch/110 - 75, shape = DayNight), data=Dai3D, size = 3, color = 'black') + scale_shape_manual(values=c(1, 4)) +
  #annotate('segment', x= as.Date("2021-11-19"), xend= as.Date("2021-11-19"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  annotate('segment', x= as.Date("2022-01-18"), xend= as.Date("2022-01-18"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  #annotate('segment', x= as.Date("2022-02-16"), xend= as.Date("2022-02-16"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  # annotate('segment', x= as.Date("2020-11-30"), xend= as.Date("2020-11-30"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  # annotate('segment', x= as.Date("2021-01-29"), xend= as.Date("2021-01-29"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  geom_vline(xintercept = (daily_avg_stn1_2021$date_column[daily_avg_stn1$peak_status == "Peak"]), color = "black") + #legend.position = c(0.15,0.80),
  scale_y_continuous(name="", limits = c(-75,-50, by =3), breaks = seq(-75,-50,3), sec.axis = sec_axis(~ (. + 75) * 110, breaks = seq(0,3000,400), name="")) + 
  scale_x_date(date_breaks = '14 days', date_labels = '%b-%d') + theme_bw() + xlab('') + labs(color = 'Sv Data Time of Day') + labs(color = 'Sv Data Time of Day', shape = 'Total Catch Time of Day') +
  theme(axis.text.x = element_text(color = 'black', size = 12), 
        axis.text.y = element_text(color = 'black', size = 12), 
        legend.position = c('none'), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12), axis.line = element_line(color = 'black'),
        panel.border = element_rect(colour = "black", fill=NA)) 
plot(Sv_Catch_3D_2021_22)
ggsave("Sv_Catch_plot_3D_2021-22.png", plot = Sv_Catch_3D_2021_22, width =10, height =6, units = c("in"), dpi = 600) 


Sv_Catch_15E_2022_23<- ggplot(aes(x=date_column, y=Sv_log, color = DayNight), data=daily_avg_stn1_2022) + geom_point( size = 3) + 
  geom_point(aes(x=date_column, y=Total_Catch/110 - 75, shape = DayNight), data=Dai15E, size = 3, color = 'black') + scale_shape_manual(values=c(1, 4)) +
  annotate('segment', x= as.Date("2022-12-07"), xend= as.Date("2022-12-07"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  annotate('segment', x= as.Date("2023-01-06"), xend= as.Date("2023-01-06"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  annotate('segment', x= as.Date("2023-02-05"), xend= as.Date("2023-02-05"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  geom_vline(xintercept = (daily_avg_stn1_2022$date_column[daily_avg_stn1$peak_status == "Peak"]), color = "black") +
  scale_y_continuous(name="", limits = c(-75,-50, by =3), breaks = seq(-75,-50,3), sec.axis = sec_axis(~ (. + 75) * 110, breaks = seq(0,3000,400), name="")) + 
  scale_x_date(date_breaks = '14 days', date_labels = '%b-%d') + theme_bw() + xlab('') + labs(color = 'Sv Data Time of Day', shape = 'Total Catch Time of Day') + #legend.position = c(0.13,0.80),
  theme(axis.text.x = element_text(color = 'black', size=12), 
        axis.text.y = element_text(color = 'black', size = 12), 
        legend.position = c('none'), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12), axis.line = element_line(color = 'black'),
        panel.border = element_rect(colour = "black", fill=NA)) 
plot(Sv_Catch_15E_2022_23)
ggsave("Sv_Catch_plot_15E_2022-23.png", plot = Sv_Catch_15E_2022_23, width =10, height =6, units = c("in"), dpi = 600) 


Sv_Catch_3D_2022_23<- ggplot(aes(x=date_column, y=Sv_log, color = DayNight), data=daily_avg_stn2_2022) + geom_point( size = 3) + 
  geom_point(aes(x=date_column, y=Total_Catch/110 - 75, shape = DayNight), data=Dai3D, size = 3, color = 'black') + scale_shape_manual(values=c(1, 4)) +
  annotate('segment', x= as.Date("2022-12-07"), xend= as.Date("2022-12-07"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  annotate('segment', x= as.Date("2023-01-06"), xend= as.Date("2023-01-06"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  annotate('segment', x= as.Date("2023-02-05"), xend= as.Date("2023-02-05"), yend=-Inf, y=Inf, colour = "black", alpha = 1) +
  geom_vline(xintercept = (daily_avg_stn1_2022$date_column[daily_avg_stn1$peak_status == "Peak"]), color = "black") +
  scale_y_continuous(name="", limits = c(-75,-50, by =3), breaks = seq(-75,-50,3), sec.axis = sec_axis(~ (. + 75) * 110, breaks = seq(0,3000,400), name="")) + 
  scale_x_date(date_breaks = '14 days', date_labels = '%b-%d') + theme_bw() + xlab('') + labs(color = 'Sv Data Time of Day') + labs(color = 'Sv Data Time of Day', shape = 'Total Catch Time of Day') + #legend.position = c(0.2,0.80),
  theme(axis.text.x = element_text(color = 'black', size=12), 
        axis.text.y = element_text(color = 'black', size = 12), 
        legend.position = c('none'), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12), axis.line = element_line(color = 'black'),
        panel.border = element_rect(colour = "black", fill=NA)) 
plot(Sv_Catch_3D_2022_23)
ggsave("Sv_Catch_plot_3D_2022-23.png", plot = Sv_Catch_3D_2022_23, width =10, height =6, units = c("in"), dpi = 600) 

#PIECEWISEREGRESSIONFORCDF----------------------------------------------------------------
###2020-21===============================
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
        panel.border = element_rect(colour = "black", fill=NA))
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
        panel.border = element_rect(colour = "black", fill=NA))
plot(pw_reg_3C_2020_21)
as.POSIXct(p$psi[, 'Est.'])
ggsave("2020-21_Dai3C_Flux_CDF_piecewise_regression.png", plot = pw_reg_3C_2020_21, width =10, height =6, units = c("in"), dpi = 600) 

###2021-22###########################################
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
        panel.border = element_rect(colour = "black", fill=NA))
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
        panel.border = element_rect(colour = "black", fill=NA))
plot(pw_reg_3D_2021_22)
as.POSIXct(p$psi[, 'Est.'])
ggsave("2021-22_Dai3D_Flux_CDF_piecewise_regression.png", plot = pw_reg_3D_2021_22, width =10, height =6, units = c("in"), dpi = 600) 

###2022-23###################################################
library(segmented)
attach(dai15E_flux_split)
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
  scale_x_datetime(date_breaks = '14 days', date_labels = '%b-%d') +
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
        panel.border = element_rect(colour = "black", fill=NA))
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
        panel.border = element_rect(colour = "black", fill=NA))
plot(pw_reg_3D_2022_23)
as.POSIXct(p$psi[, 'Est.'])
ggsave("2022-23_Dai3D_Flux_CDF_piecewise_regression.png", plot = pw_reg_3D_2022_23, width =10, height =6, units = c("in"), dpi = 600) 

###SignificanceAnalysisDensityPeakNonPeak_DN#############################################################################
 
full_moon_dates_1<- seq.Date(as.Date('2021-12-10'), as.Date( '2021-12-14'), 'days') #seq.Date(as.Date('2020-12-20'), as.Date( '2020-12-26'), 'days') 
full_moon_dates_2<- seq.Date(as.Date('2022-01-05'), as.Date( '2022-01-09'), 'days') #seq.Date(as.Date('2021-01-14'), as.Date( '2021-01-23'), 'days')
full_moon_dates_15E<- c(full_moon_dates_1, full_moon_dates_2)
daily_avg_stn1<- daily_avg_stn1 %>%
  mutate(peak_status = ifelse(date_column %in% full_moon_dates_15E, "Peak", "Off-Peak"))

#boxplot(exports_stn1$Sv_mean ~ exports_stn1$peak_status)
model.lm<- lm(Sv_log ~ peak_status, data = daily_avg_stn1)
# hist(resid(model.lm))
# plot(model.lm, 2)
# plot(model.lm, 1)
a1 <- aov(model.lm)
summary(a1)
a1_tukey_txt<- (TukeyHSD(a1, 'peak_status', conf.level=0.95))
print(a1_tukey_txt)


full_moon_dates_3<- seq.Date(as.Date('2023-01-01'), as.Date( '2023-01-04'), 'days')# seq.Date(as.Date('2020-12-21'), as.Date( '2020-12-28'), 'days')  seq.Date(as.Date('2021-12-14'), as.Date( '2021-12-15'), 'days')
full_moon_dates_4<- seq.Date(as.Date('2023-01-25'), as.Date( '2023-01-28'), 'days')# seq.Date(as.Date('2021-01-14'), as.Date( '2021-01-27'), 'days')  seq.Date(as.Date('2022-01-05'), as.Date( '2022-01-15'), 'days')
full_moon_dates_3CD<- c(full_moon_dates_3, full_moon_dates_4)
daily_avg_stn2<- daily_avg_stn2 %>%
  mutate(peak_status = ifelse(date_column %in% full_moon_dates_3CD, "Peak", "Off-Peak"))

#boxplot(exports_stn2$Sv_mean ~ exports_stn2$peak_status)
model.lm_2<- lm(Sv_log ~ peak_status, data = daily_avg_stn2)
# hist(resid(model.lm_2))
# plot(model.lm_2, 2)
# plot(model.lm_2, 1)
a2 <- aov(model.lm_2)
summary(a2)
a2_tukey_txt<- (TukeyHSD(a2, 'peak_status', conf.level=0.95))
print(a2_tukey_txt)

#boxplot(daily_avg_stn1$Sv_log ~ daily_avg_stn1$DayNight)
model.lm = lm(Sv_log ~ DayNight, data=daily_avg_stn1)
# hist(resid(model.lm))
# plot(model.lm, 2)
# plot(model.lm, 1)
a1 <- aov(model.lm)
summary(a1)
#a1_txt<- summary(a1)
#capture.output(a1_txt, file= "stn1_DN_significance_output.txt")
a1_tukey_txt<- (TukeyHSD(a1, 'DayNight', conf.level=0.95))
print(a1_tukey_txt)
#capture.output(a1_tukey_txt, file= "stn1_DN_significance_output_tukey.txt")

#Tukey multiple comparisons of means
#95% family-wise confidence level

#boxplot(daily_avg_stn2$Sv_log ~ daily_avg_stn2$DayNight)
model.lm_2 = lm(Sv_log ~ DayNight, data=daily_avg_stn2)
# hist(resid(model.lm_2))
# plot(model.lm_2, 2)
# plot(model.lm_2, 1)
a2 <- aov(model.lm_2)
summary(a2)
#a2_txt<- summary(a2)
#capture.output(a2_txt, file= "stn2_DN_significance_output.txt")
a2_tukey_txt<- (TukeyHSD(a2, 'DayNight', conf.level=0.95))
print(a2_tukey_txt)
#capture.output(a2_tukey_txt, file= "stn2_DN_significance_output_tukey.txt")


####SignificanceAnalysisCatch####################################################################
# Dai15E<- Dai15E %>% 
#   filter(!row_number() %in% c(20485)) %>%
#   mutate(SL_mm = as.numeric(SL_mm))

catch15E<- data.frame(unique(Dai15E$Total_Catch)) %>%
  rename_at(1,~'Total_Catch')
catch15E$DayNight<- Dai15E$DayNight[match(catch15E$Total_Catch, Dai15E$Total_Catch)]

sum15<- rbind.data.frame(sum(catch15E$DayNight == 'Night'), sum(catch15E$DayNight == 'Day'))
sum15<- min(sum15)[1]

bcatch15E<- data.frame(catch15E |>
              slice_sample(n = sum15, by = c(DayNight))) |>
  semi_join(catch15E, y = _, by = c("DayNight", 'Total_Catch'))  

boxplot(bcatch15E$Total_Catch ~ bcatch15E$DayNight)
aggregate(Total_Catch ~ DayNight, bcatch15E, mean)
model.lm = lm(Total_Catch ~ DayNight, data=bcatch15E)
hist(resid(model.lm))
plot(model.lm, 2)
plot(model.lm, 1)
t.test(Total_Catch ~ DayNight, data=bcatch15E)
a1 <- aov(model.lm)
summary(a1)
a1_txt<- summary(a1)
#capture.output(a1_txt, file= "stn1fishcatch_DN_significance_output.txt")
a1_tukey_txt<- (TukeyHSD(a1, 'DayNight', conf.level=0.95))
print(a1_tukey_txt)
#capture.output(a1_tukey_txt, file= "stn1fishcatch_DN_significance_output_tukey.txt")


catch3D<- data.frame(unique(Dai3D$Total_Catch)) %>%
  rename_at(1,~'Total_Catch')
catch3D$DayNight<- Dai3D$DayNight[match(catch3D$Total_Catch, Dai3D$Total_Catch)]

sum3<- rbind.data.frame(sum(catch3D$DayNight == 'Night'), sum(catch3D$DayNight == 'Day'))
sum3<- min(sum3)[1]

bcatch3D<- data.frame(catch3D |>
                         slice_sample(n = sum3, by = c(DayNight))) |>
  semi_join(catch3D, y = _, by = c("DayNight", 'Total_Catch'))  

boxplot(bcatch3D$Total_Catch ~ bcatch3D$DayNight)
aggregate(Total_Catch ~ DayNight, bcatch3D, mean)
model.lm_2 = lm(Total_Catch ~ DayNight, data=bcatch3D)
hist(resid(model.lm_2))
plot(model.lm_2, 2)
plot(model.lm_2, 1)
t.test(Total_Catch ~ DayNight, data=bcatch3D)
a2<- aov(model.lm_2)
summary(a2)
a2_txt<- summary(a2)
print(a2_txt)
#capture.output(a2_txt, file= "stn2fishcatch_DN_significance_output.txt")
a2_tukey_txt<- (TukeyHSD(a2, 'DayNight', conf.level=0.95))
print(a2_tukey_txt)
#capture.output(a2_tukey_txt, file= "stn2fishcatch_DN_significance_output_tukey.txt")

###SignificanceAnalysisPEAKTotalLengthStandardLengthWeight#################################


boxplot(Dai15E$TL_mm ~ Dai15E$peak)
model.lm = lm(TL_mm ~ peak, data=Dai15E)
summary(model.lm)
hist(resid(model.lm))
plot(model.lm, 2)
plot(model.lm, 1)
t.test(TL_mm ~ peak, data=Dai15E)
bartlett.test(TL_mm ~ peak, data = Dai15E)
a1 <- aov(model.lm)
summary(a1)
a1_txt<- summary(a1)
#capture.output(a1_txt, file= "stn1fishcatch_DN_significance_output.txt")
a1_tukey_txt<- (TukeyHSD(a1, 'peak', conf.level=0.95))
print(a1_tukey_txt)
#capture.output(a1_tukey_txt, file= "stn1fishcatch_DN_significance_output_tukey.txt")

#kruskal.test(TL_mm ~ peak, data = Dai15E)



#Tukey multiple comparisons of means
#95% family-wise confidence level
boxplot(Dai3C$TL_mm ~ Dai3C$peak)
model.lm_2 = lm(TL_mm ~ peak, data=Dai3C)
hist(resid(model.lm_2))
plot(model.lm_2, 2)
plot(model.lm_2, 1)
t.test(TL_mm ~ peak, data=Dai3C)
bartlett.test(TL_mm ~ peak, data = Dai3C)
a2<- aov(model.lm_2)
summary(a2)
a2_txt<- summary(a2)
print(a2_txt)
#capture.output(a2_txt, file= "stn2fishcatch_DN_significance_output.txt")
a2_tukey_txt<- (TukeyHSD(a2, 'peak', conf.level=0.95))
print(a2_tukey_txt)
#capture.output(a2_tukey_txt, file= "stn2fishcatch_DN_significance_output_tukey.txt")






counts <- ddply(Dai15E, .(Dai15E$unique_id, Dai15E$DayNight), nrow)
names(counts) <- c("unique_id", "DayNight", "Freq")
#view(counts)
pp<-split(counts, with(counts, interaction(counts$DayNight)), drop = TRUE)

counts <- ddply(Dai3C, .(Dai3C$unique_id, Dai3C$DayNight), nrow)
names(counts) <- c("unique_id", "DayNight", "Freq")
#view(counts)
pp<-split(counts, with(counts, interaction(counts$DayNight)), drop = TRUE)

###TargetStrength##############################################################################################################################################################
full_moon_dates_1<- seq.Date(as.Date('2021-12-10'), as.Date( '2021-12-14'), 'days')#seq.Date(as.Date('2020-12-20'), as.Date( '2020-12-26'), 'days') 
full_moon_dates_2<- seq.Date(as.Date('2022-01-05'), as.Date( '2022-01-09'), 'days')#seq.Date(as.Date('2021-01-14'), as.Date( '2021-01-23'), 'days') 
full_moon_dates_15E<- c(full_moon_dates_1, full_moon_dates_2)
single_targets_full_stn1<- single_targets_full_stn1 %>%
  mutate(peak_status = ifelse(date_column %in% full_moon_dates_15E, "Peak", "Off-Peak"))


boxplot(single_targets_full_stn1$TS_mean ~ single_targets_full_stn1$peak_status)
model.lm = lm(TS_mean ~ peak_status, data=single_targets_full_stn1)
hist(resid(model.lm))
plot(model.lm, 2)
plot(model.lm, 1)
a1 <- aov(model.lm)
summary(a1)
a1_txt<- summary(a1)
#capture.output(a1_txt, file= "stn1fishcatch_DN_significance_output.txt")
a1_tukey_txt<- (TukeyHSD(a1, 'peak_status', conf.level=0.95))
print(a1_tukey_txt)
#capture.output(a1_tukey_txt, file= "stn1fishcatch_DN_significance_output_tukey.txt")





#boxplot(target_detections_all_stn1$TS_comp)
model.lm = lm(TS_comp ~ DayNight, data=target_detections_all_stn1)
#hist(resid(model.lm))
#plot(model.lm, 2)
#plot(model.lm, 1)
a1 <- aov(model.lm)
summary(a1)
a1_txt<- summary(a1)
#capture.output(a1_txt, file= "stn1fishcatch_DN_significance_output.txt")
a1_tukey_txt<- (TukeyHSD(a1, 'DayNight', conf.level=0.95))
print(a1_tukey_txt)
#capture.output(a1_tukey_txt, file= "stn1fishcatch_DN_significance_output_tukey.txt")

#boxplot(target_detections_all_stn1$TS_comp)
model.lm = lm(TS_comp ~ peak_status, data=target_detections_all_stn1)
#hist(resid(model.lm))
#plot(model.lm, 2)
#plot(model.lm, 1)
a1 <- aov(model.lm)
summary(a1)
a1_txt<- summary(a1)
#capture.output(a1_txt, file= "stn1fishcatch_DN_significance_output.txt")
a1_tukey_txt<- (TukeyHSD(a1, 'peak_status', conf.level=0.95))
print(a1_tukey_txt)
#capture.output(a1_tukey_txt, file= "stn1fishcatch_DN_significance_output_tukey.txt")





PDF_T<- ggplot(single_targets_full_stn1, mapping = aes(x= TS_mean)) + 
  geom_histogram(aes(y= ((..count..)/sum(..count..))), fill="blue",color="white",alpha=0.7) +
  scale_y_continuous(breaks = seq(0, .15, .01), labels = scales::percent) +
  scale_x_continuous(breaks = seq(-100,-40, by = 10)) +
  facet_wrap(.~ Week) + geom_vline(data = TS_median, aes(xintercept = x), linetype= "dashed", colour= "darkgrey") + theme_bw() +
  ylab("Relative Frequencies") + xlab("Compensated Target Strength (dB)") + ggtitle("Proportion of TS at Daistn1 by Week (Full Season)")
plot(PDF_T)

#####TS-LENGTHEQUATIONS###################################################################

# SL_med<- median(New_Dai15E$SL_mm/10)
# TL_med<- median(New_Dai15E$TL_mm/10)
# TS_med<- median(target_detections_all_stn1$TS_comp)
# ##Foote TS = 20Log(FL) + b20 ###### 20 is used as phystostomous fish usually provide an 'm' of 20
#ts_all_sub <- subset(target_detections_all_stn1, !TS_comp %in% identify_outliers(target_detections_all_stn1, "TS_comp")$TS_comp)# mx<- 20*log(TL_med)

ts_all_sub<- target_detections_all_stn1[sample(nrow(target_detections_all_stn1), nrow(Dai15E)),]
ts_all_match<- target_detections_all_stn1 %>% semi_join(Dai15E, by = 'date_column')

range(ts_all_match$TS_comp)
boxplot(ts_all_match$TS_comp)
model.lm = lm(TS_comp ~ DayNight, data=ts_all_sub)
hist(resid(model.lm))
plot(model.lm, 2)
plot(model.lm, 1)


b20<- (-69.6)
#b20<- (-75)

foote_L_est<- data.frame(10^((ts_all_sub$TS_comp-b20)/20)) %>%
  rename_at(1,~'L_cm')
foote_L_est$TS<- (ts_all_sub$TS_comp) #%>%
colnames(foote_L_est)[2]<- 'TS'
foote_L_est$L_real<- (Dai15E$TL_mm/10)
colnames(foote_L_est)[3]<- 'L_real'
model.L = lm(L_cm ~ TS, data = foote_L_est)
summary(model.L)

ggplot(foote_L_est, aes(x = L_cm, y = TS)) + geom_point() +
 # geom_point(data = foote_L_est, aes(y = TS))  + geom_point(data = foote_L_est, aes(y= L_real, color = 'blue')) + 
  scale_x_continuous(breaks = function(x){seq(round(min(foote_L_est$L_cm), -1), round(max(foote_L_est$L_cm), -1), 5)}) +
  scale_y_continuous(breaks = function(y){seq(round(min(foote_L_est$TS), -1), 10, 3)}) +
  #geom_smooth(method = loess, se = F) +
  theme_bw() + xlab("Length (cm)") + ylab(~paste('Target Strength (dB re 1 m'^'2'*')')) 


med<- data.frame(median(foote_L_est$L_cm)) %>%
  rename_at(1,~'x')

ggplot(foote_L_est, mapping = aes(x= L_cm)) + 
  geom_histogram(aes(y= ((..count..)/sum(..count..))), fill="blue",color="white",alpha=0.7) +
  geom_vline(data = med, aes(xintercept = x), linetype= "dashed", colour= "red4") 

ts_all_sub<- target_detections_all_stn1[sample(nrow(target_detections_all_stn1), nrow(Dai15E)),]
foote_L_est<- data.frame(10^((ts_all_sub$TS_comp-b20)/20)) %>%
  rename_at(1,~'L_cm')
foote_L_est$TS<- (ts_all_sub$TS_comp) #%>%
colnames(foote_L_est)[2]<- 'TS'
foote_L_est$L_real<- (Dai15E$TL_mm/10)
colnames(foote_L_est)[3]<- 'L_real'
model.L = lm(L_cm ~ TS, data = foote_L_est)
summary(model.L)



foote_TS_est<- data.frame(20*log10((Dai15E$SL_mm/10)) + b20) %>%
  rename_at(1,~'TS')
foote_TS_est$L_cm<- (Dai15E$SL_mm/10) #%>%
colnames(foote_TS_est)[2]<- 'L_cm'
foote_TS_est$TS_real<- (ts_all_sub$TS_comp)
colnames(foote_TS_est)[3]<- 'TS_real'
model.TS = lm(TS ~ L_cm, data=foote_TS_est)
summary(model.TS)

model.lm = lm(L_cm ~ TS_real, data=foote_TS_est)
summary(model.lm)
coef(model.lm)[2]

ggplot(foote_TS_est, aes(x = L_cm)) + 
  geom_point(data = foote_TS_est, aes(y=TS_real, color = 'blue')) + 
  geom_point(data = foote_TS_est, aes(y = TS)) + theme_bw() +
  #scale_x_continuous(breaks = function(x){seq(round(min(TS_est$TL_cm), -1), round(max(TS_est$TL_cm), -1), 2)}) +
  #scale_y_continuous(breaks = function(y){seq(round(min(TS_est$TS), -1), round(max(TS_est$TS), -1), 3)}) +
  #annotate('text', x = 8, y = -45, label = 'Foote 1979: TS = 20log(L) - 110.85') +
  xlab("Length (cm)") + ylab(~paste('Target Strength (dB re 1 m'^'2'*')')) +
  theme(axis.text.x = element_text(color = 'black', size = 12), 
        axis.text.y = element_text(color = 'black', size = 12),
        axis.line = element_line(color = 'black'), legend.position = 'none', 
        panel.border = element_rect(colour = "black", fill=NA))
  

##Love  TS = 19.4Log(FL) + 0.6Log(lambda) - 24.9
love_ts_const<- (target_detections_all_stn1$TS_comp*10) + 249

love_L_est<- data.frame(10^((-6*log(200)-love_ts_const)/194)) %>% #love_TS_est<- data.frame(19.4*log((New_Dai15E$TL_mm/1000)) + 0.6*log(200) - 24.9) %>%
  rename_at(1,~'L_m')
love_L_est$TS<- (target_detections_all_stn1$TS_comp) #%>%
colnames(love_L_est)[2]<- 'TS'

ggplot(love_L_est, aes(x = TS, y = L_m)) + geom_point() + geom_smooth(method = loess) + theme_bw() +
  scale_x_continuous(breaks = function(x){seq(round(min(love_L_est$TS), -1), round(max(love_L_est$TS), -1), 3)}) +
  #scale_y_continuous(breaks = function(y){seq(round(min(love_L_est$L_m), 0.01), round(max(love_L_est$L_m), 0.01), 0.03)}) +
  #annotate('text', x = 0.10, y = -46, label = 'Love 1971: TS = 19.4log(L) + 0.6log(200) - 24.9') +
  xlab("Predicted Length (m)") + ylab(~paste('Target Strength (dB re 1 m'^'2'*')')) +
  theme(axis.text.x = element_text(color = 'black', size = 12), 
        axis.text.y = element_text(color = 'black', size = 12),
        axis.line = element_line(color = 'black'),
        panel.border = element_rect(colour = "black", fill=NA))

model.lm = lm(TS ~ L_m, data=love_L_est)
summary(model.lm)
coef(model.lm)[2]
a1 <- aov(model.lm)
summary(a1)


# ggplot(pw_reg_15E, aes(x = x, y = y)) +
#   geom_point() +
#   geom_line(data = dat2, color = 'blue', linewidth = 2) +
#   scale_x_datetime(date_breaks = '5 days', date_labels = '%b-%d') +
#   scale_y_continuous(breaks = ~ seq(0, max(.x), .1)) +
#   annotate('segment', x= as.POSIXct("2021-01-29"), xend= as.POSIXct("2021-01-29"), yend=-Inf, y=Inf, colour = "black", alpha = 3, linewidth = 2) +
#   annotate('rect', xmin = c(as.POSIXct('2020-12-20 13:14:11'), as.POSIXct('2021-01-14 12:22:15')), xmax = c(as.POSIXct('2020-12-26 15:35:47'), as.POSIXct('2021-01-23 21:34:05')), 
#            ymin = c(0.19047932, 0.72899546), ymax = c(0.57782704, 0.96324831), alpha = 0.2, color = 'red', fill = 'red') +
#   annotate('rect', xmin = c(as.POSIXct('2020-12-20 13:14:11')), xmax = c(as.POSIXct('2021-01-23 21:34:05')), 
#            ymin = c(-Inf), ymax = c(Inf), alpha = 0.2, color = 'blue', fill = 'blue') +
#   geom_vline(xintercept = (dai15E_flux_split$Dates[dai15E_flux_split$peak_status == "Peak"]), color = "black", linewidth = 2) +
#   geom_vline(xintercept = o$psi[, 'Est.'], color = 'red') +
#   geom_point(size = 4) + theme_bw() + 
#   labs(x = "Date") + ylab(~paste('CDF', Phi))





#######gglock##########################################################################################################################################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
minutes <- data_frame(x = 0:60, y = 1)
hsize <- 4
clock_data <- data.frame(structure(list(Time_Between = c("0-15", "15-30", "30-33", "33-34", 
                                                         "34-35", "35-36", "36-40", "40-55", "55-56", "56-60"), Time = c(15L, 
                                                                                                                         15L, 3L, 1L, 1L, 1L, 4L, 15L, 1L, 4L), Action = c("Inactive1", 
                                                                                                                                                                           "Acoustic Sampling", "Inactive2", "SSH Tunnel", "Inactive3", 
                                                                                                                                                                           "Switch to Pi", "Data Copy", "Data Transmission", "Switch to Mini", 
                                                                                                                                                                           "Inactive4"), x = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4)), row.names = c(NA, 
                                                                                                                                                                                                                                             10L), class = "data.frame"))
clock_data <- clock_data %>%
  mutate(x = hsize) %>%
  tidyr::separate(Time_Between,
                  into = c("start", "end"),
                  convert = TRUE
  ) |>
  mutate(
    Action = if_else(
      grepl("^Inactive", Action), "Inactive", Action
    ),
    # If needed: Reorder for the legend
    Action = reorder(Action, start, FUN = min)
  )

action_clock<- ggplot(clock_data) +
  geom_rect(aes(
    xmin = hsize - 1.5, ymin = start,
    xmax = hsize + .45, ymax = end,
    fill = Action
  )) + scale_fill_manual(values = c("Inactive" = "#D55E00",
                                     "Acoustic Sampling"="#009E73",
                                     "SSH Tunnel"="#CC79A7",
                                     "Switch to Pi"="#F0E442",
                                     "Data Copy"="#0072B2",
                                     "Data Transmission"="#000000",
                                     "Switch to Mini"="#56B4E9")) + 
  coord_polar(theta = "y") + 
  xlim(c(0.2, hsize + 0.5)) + scale_y_continuous(limits = c(0,60, by = 1), breaks = seq(0,60,5), minor_breaks = seq(0, 60, 1)) +
  theme_bw() + theme_minimal() + theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 20, color = 'black'),
    axis.text.y = element_blank(),
    panel.grid.major = element_line(size = 1, color = 'white'),
    panel.grid.minor = element_line(size = 1, color = 'white')
  ) #theme(panel.grid.minor = element_line(color = 'grey', size = 0.25, linetype = 1), 
                     #axis.text.x = element_text(face="bold", color="black", size=14), 
                     #panel.grid.major = element_blank())
plot(action_clock)
ggsave("action_clock.png", plot = action_clock,  width =10, height = 8, units = c("in"), dpi = 600) #saves the graph as a pdf


library(scales)
library(ggthemes)
show_col(colorblind_pal()(8))
# minutes <- data_frame(x = 0:60, y = 1)
# minutes
# hours <- filter(minutes, x %% 5 == 0)
# hours
# 
# ggplot() +
#   geom_point(data = minutes, aes(x = x, y = y)) +
#   geom_point(data = hours, aes(x = x, y = y, 
#                                color = factor(x, levels = c(60, seq(5, 55, 5)))),
#              size = 5, show.legend = FALSE) +
#   coord_polar() +
#   expand_limits(y = c(0, 1)) +
#   scale_x_continuous(breaks = seq(15, 60, 15), labels = c(15, 30, 45, "0/60")) +
#   scale_y_continuous(breaks = seq(0, 1, 0.25)) +
#   scale_color_discrete() +
#   theme_grey() +
#   theme(
#     axis.title = element_blank(),
#     axis.ticks = element_blank(),
#     axis.text.x = element_text(size = 20),
#     axis.text.y = element_blank(),
#     panel.grid.major = element_line(size = 2),
#     panel.grid.minor = element_line(size = 2)
#   )
# 
# 
# 
# set.seed(44)
# N=500
# events <- as.POSIXct("2011-01-01", tz="GMT") + 
#   days(floor(365*runif(N))) + 
#   hours(floor(24*rnorm(N))) +  # using rnorm here
#   minutes(floor(60*runif(N))) +
#   seconds(floor(60*runif(N)))
# hour_of_event <- hour(events)
# # make a dataframe
# eventdata <- data.frame(datetime = events, eventhour = hour_of_event)
# # determine if event is in business hours
# eventdata$Workday <- eventdata$eventhour %in% seq(9, 17)
# 
# ggplot(eventdata, aes(x = eventhour, fill = Workday)) + geom_histogram(breaks = seq(0, 
#                                                                                     60), width = 2, colour = "grey") + coord_polar(start = 0) + theme_minimal() + 
#   scale_fill_brewer() + ylab("Count") + ggtitle("Events by Time of day") + 
#   scale_x_continuous("", limits = c(0, 60), breaks = seq(0, 60), labels = seq(0, 
#                                                                               60))
# 
# 
# 
# 
# 
# setwd('Z:/fishproj/Cambodia Dai project/Papers/SPAAMS paper/Figs, pics, plots')
# clock_data<- read.csv('actions_times.csv', header = T)
# colnames(clock_data)[1]<- 'Time_Between'
# colnames(clock_data)[2]<- 'Time'
# colnames(clock_data)[3]<- 'Action'
# # df <- data.frame(value = c(1, 19, 4, 1, 1, 1, 3, 15, 15),
# #                  group = paste0("G", 1:9))
# 
# 
# # Increase the value to make the hole bigger
# # Decrease the value to make the hole smaller
# hsize <- 4
# 
# clock_data <- clock_data %>%
#   mutate(x = hsize)
# 
# ggplot(clock_data, aes(x = hsize, y = Time, fill = Action)) +
#   geom_col() + #scale_color_discrete(labels=c("Inactive1", "Acoustic Sampling", "Inactive2", 'SSH Tunnel', 'Inactive3', 'Switch Pi', 'Data Copy', 'Data Transmission', 'Switch Mini', 'Inactive4'))
#   coord_polar(theta = "y") +
#   xlim(c(0.2, hsize + 0.5)) + theme_bw()
# 
# df <- df %>%
#   mutate(x = hsize)
# 
# ggplot(df, aes(x = hsize, y = value, fill = group)) +
#   geom_col() +
#   coord_polar(theta = "y") +
#   xlim(c(0.2, hsize + 0.5))
# 
# 

########TSSTACKEDGRAPHS############################################################################
{
fig<- ggarrange(hour_12_TS_2020 + rremove("ylab") + rremove("xlab"), hour12_TS_2021 + rremove("ylab") + rremove("xlab"),
                hour_12_TS_2022 + rremove("ylab") + rremove("xlab"), ncol = 1, nrow = 3, common.legend = T, legend = 'top') #labels = c('A', 'B', 'C'), 
fig<- annotate_figure(fig, left = textGrob(~paste('Target Strength (dB re 1 m'^'2'*')'), rot = 90, vjust = 1, gp = gpar(cex = 1)),
                bottom = textGrob("Date", gp = gpar(cex = 1)))
plot(fig)
}

ggsave("Z:/fishproj/Cambodia Dai project/Papers/Analysis Paper/Pics, Plot, Figures/TS_ggarrange_all_years_-80.png", plot = fig, width =12, height =8, units = c("in"), dpi = 800) 

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
}

ggsave("Z:/fishproj/Cambodia Dai project/Papers/Analysis Paper/Pics, Plot, Figures/TS_histogram_ggarrange_all_years_-80.png", plot = TS_hist, width =12, height =8, units = c("in"), dpi = 800) 

#####arrangecompgraphs##################################

comp_fig<- ggarrange(fam_prop_barchart_15E_2020 + rremove("ylab") + rremove("xlab"),
                     fam_prop_barchart_3C_2020 + rremove("ylab") + rremove("xlab"),
                     fam_prop_barchart_15E_2021 + rremove("ylab") + rremove("xlab"),
                     fam_prop_barchart_3D_2021 + rremove("ylab") + rremove("xlab"),
                     fam_prop_barchart_15E_2022 + rremove("ylab") + rremove("xlab"),
                     fam_prop_barchart_3D_2022 + rremove("ylab") + rremove("xlab"),
                     ncol = 2, nrow = 3, common.legend = T, legend = 'top')
plot(comp_fig)

########Mortalityggarrange###############################################################################
#xlab('Date') + ylab(~ paste(Delta , Phi *"(m"^"2"*"h"^"-1"*")"))
mortality_fig<- ggarrange(Mortality_Index_2020 + rremove("ylab") + rremove("xlab"),
                     Mortality_Index_2021 + rremove("ylab") + rremove("xlab"),
                     Mortality_Index_2022 + rremove("ylab") + rremove("xlab"),
                     ncol = 1, nrow = 3, common.legend = T, legend = 'top')
mortality_fig<- annotate_figure(mortality_fig, left = textGrob(~ paste(Delta , Phi *"(m"^"2"*"h"^"-1"*")"), rot = 90, vjust = 1, gp = gpar(cex = 1)),
                      bottom = textGrob("Date", gp = gpar(cex = 1)))
plot(mortality_fig)


##############fluxggarrange##########

flux_fig<- ggarrange(flux_comparison_All_facet_2020 + rremove("ylab") + rremove("xlab"),
                          flux_comparison_All_facet_2021 + rremove("ylab") + rremove("xlab"),
                          flux_comparison_All_facet_2022 + rremove("ylab") + rremove("xlab"),
                          ncol = 1, nrow = 3, common.legend = T, legend = 'top')
flux_fig<- annotate_figure(flux_fig, left = textGrob(~paste(Phi*"(m"^"2"*"h"^"-1"*")"), rot = 90, vjust = 1, gp = gpar(cex = 1)),
                                bottom = textGrob("Date", vjust = -0.60, gp = gpar(cex = 1)))
plot(flux_fig)
###########CDFPWGGARRANGE##################################################

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

######CatchSVfigarrange##########################################

catchsv_fig<- ggarrange(Sv_Catch_15E_2020_21 + rremove("ylab") + rremove("xlab"),
                        Sv_Catch_3C_2020_21 + rremove("ylab") + rremove("xlab"),
                        Sv_Catch_15E_2021_22 + rremove("ylab") + rremove("xlab"),
                        Sv_Catch_3D_2021_22 + rremove("ylab") + rremove("xlab"),
                        Sv_Catch_15E_2022_23 + rremove("ylab") + rremove("xlab"),
                        Sv_Catch_3D_2022_23 + rremove("ylab") + rremove("xlab"),
                        ncol = 2, nrow = 3, common.legend = T, legend = 'top')
catchsv_fig<- annotate_figure(catchsv_fig, left = textGrob('Sv mean 12 Hour Avg (dB)', rot = 90, vjust = 1, gp = gpar(cex = 1)),
                          bottom = textGrob("Date", gp = gpar(cex = 1)), right = textGrob('Total Catch (kg)', rot = 90, vjust = 0, gp = gpar(cex = 1)))
plot(catchsv_fig)

#########################################################
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
daily_avg_15E$sa_log = 10*log10(daily_avg_15E$sa_Ln) #
#full_moon_dates<- as.Date(c('2022-12-08', '2023-01-07'))
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
#full_moon_dates<- as.Date(c('2022-12-08', '2023-01-07'))
daily_avg_3D$date_column<- as.Date(daily_avg_3D$Date_time, format = "%Y-%m-%d")
daily_avg_3D$peak_status <- ifelse(daily_avg_3D$date_column %in% full_moon_dates, "Peak", "Off-Peak")

daily_avg_3D$Days <- as.numeric(difftime(daily_avg_3D$date_column, min(daily_avg_3D$date_column), units = "days")) + 1
#daily_avg_3D<- daily_avg_3D[-36,]
dailyavg_3D_split<- split(daily_avg_3D, daily_avg_3D$DayNight)

#renames and sets the column with dates and times to the preferred format



dailyavg_15E_split<- split(daily_avg_15E, daily_avg_15E$DayNight)

dailyavg_3D_split<- split(daily_avg_3D, daily_avg_3D$DayNight)
# common_datetimes<- intersect(dailyavg_3D_split[['Night']]$date_column, dailyavg_3D_split[['Day']]$date_column)
# for (i in seq_along(dailyavg_3D_split)) {
#   dailyavg_3D_split[[i]] <- dailyavg_3D_split[[i]][dailyavg_3D_split[[i]]$date_column %in% common_datetimes, ]
# }
dailyavg_3D_split[['Day']]<- dailyavg_3D_split[['Day']] %>% semi_join(dailyavg_3D_split[['Night']], by = 'date_column')
dailyavg_3D_split[['Night']]<- dailyavg_3D_split[['Night']] %>% semi_join(dailyavg_3D_split[['Day']], by = 'date_column')
ratio_3D_dataframe<- data.frame((dailyavg_3D_split[['Night']]$sa_Ln)/(dailyavg_3D_split[['Day']]$sa_Ln)) #%>% 
# mutate(Week = row_number())
ratio_3D_dataframe$Week<- seq_along(ratio_3D_dataframe[,1]) 
colnames(ratio_3D_dataframe)[1] <- 'ratio'
ratio_3D<- split(ratio_3D_dataframe, seq(nrow(ratio_3D_dataframe)))


jj<- mget(ls(pattern = 'Dai15_Dai3_'))
#jj <- lapply(jj, function(jj) {
#  jj$Dai <- factor(jj$Dai)
#  return(jj)
#})
rm(transposed_data)
#n = nrow(ratio_3D_dataframe)
n = length(ratio_3D)
transposed_data = list()
for (i in seq(n)){
  transposed_data[[i]] <- split(jj[[i]], jj[[i]]$Dai)
  transposed_data[[i]] <- lapply(transposed_data[[i]], head, min(sapply(transposed_data[[i]], nrow)))
  transposed_data[[i]] <- do.call(cbind, transposed_data[[i]])
  #print(transposed_data[[i]]$Dai15E.TOD)
  #print(transposed_data[[i]]$Dai3D.TOD)
  #print(ratio_3D[[i]]$ratio) # add this line to print the dataframe before modification
  transposed_data[[i]]$x <- case_when(transposed_data[[i]]$Dai15E.TOD - transposed_data[[i]]$Dai3D.TOD == 1 ~ transposed_data[[i]]$Dai3D.sa_Ln * ratio_3D[[i]]$ratio,
                                      transposed_data[[i]]$Dai15E.TOD - transposed_data[[i]]$Dai3D.TOD == -1 ~ transposed_data[[i]]$Dai3D.sa_Ln/ratio_3D[[i]]$ratio,
                                      transposed_data[[i]]$Dai15E.TOD - transposed_data[[i]]$Dai3D.TOD == 0 ~ transposed_data[[i]]$Dai3D.sa_Ln + 0)
  #print('made it this far')  # add this line to print the dataframe after modification
}

adj_data<- bind_rows(transposed_data, .id = 'Week')
colnames(adj_data)[202]<- 'Date_time'
exports_3D<- exports_3D %>% semi_join(adj_data, by = 'Date_time')
exports_3D$x<- (adj_data$x)

























































 