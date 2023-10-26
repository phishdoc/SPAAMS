#----
# RUNME_pipeline.R
# Jackson Swan, 2023-03-27
# 
# Last Modified by: Mark Yamane, 2023-10-26
#---

library(tidyverse)
library(ggpubr)  

#clear the workspace (Global Environment) of all objects
rm(list=ls())

#close any open graphics/plots
graphics.off()


#----
# User-defined variables
#----
dir_r_scripts = "./" 
input_all_data = "../data/ev_exports/"
data_analysis_output = "../output/"

full_moon_dates<- as.Date(c('2023-01-06', '2023-02-05'))

prefix = "TS2022-23"  # prefix for output from the pipeline

# only required for flux calc
input_transect_data = "../data/transects/"

#----
# Initiate SPAAMS pipeline
# DO NOT EDIT BEYOND THIS POINT!
#----
setwd(dir_r_scripts)
bottom_line = paste0(dir_r_scripts, "/TonleSap_Bottom_Line_Compilation_Universal.R")
target_strength = paste0(dir_r_scripts, "/TonleSap_Target_Strength_Universal.R")
flux_calc = paste0(dir_r_scripts, '/TonleSap_Flux_Calc_Universal.R')
source("TonleSap_OGIVE_CDF_PDF_Universal.R")
