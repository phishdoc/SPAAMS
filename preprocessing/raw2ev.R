# CREATED by: Louise P. McGarry, Ph.D. June 2021



# PURPOSE:
# Automate creation of EV files based on date/time and lag



# This script was written and tested 
# on Windows 10; R version 4.1.0 (2021-05-18); R Studio version 1.4.1717 (2021-05-24) 



# INSTRUCTIONS TO USER:
# ...0) Review the Assumptions section below.
# ...1) Carefully complete the USER-DEFINED VARIABLES section below.
#       Pay careful attention to the assumptions embedded in the User-Defined variables
#       Such as...
#          The .raw files for stn1 and for stn2 are in separate folders.
# ...2) You'll need LPMcGarry's file of USER-DEFINED FUNCTIONS.R
#       There are far more functions in that file than you need for this script.
#       We can remove those unused functions from that file once we get this script running as desired.
# ...3) Run this script
# ...4) An "End of SCRIPT" message will appear when the script completes.
# ...5) We can modify the pattern for the output EV filenames






#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#.                                                                           ####
### PREVIOUS STEP: ####
#   Place into a folder the Simrad .raw data files for site stn2 
#   Place into a folder the Simrad .raw data files for site stn1


### THIS STEP:     ####
#    ...EVFileGenerate.R


### NEXT STEP:     ####
#   


#.                                                                          ####
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# INSTRUCTIONS: Run this whole script. (See NOTES to USERS below.)



{            #by bounding the script in braces, the script will completely stop execution upon encountering "stop()" when a critical test is failed
             #if you run part of the script (i.e. not all the way to the closing brace at the bottom of the file)), simply type the closing brace '}'
             #in the Console where you can see it's awaiting the finish of the command (i.e. a '+' shows rather than '>')

  
  
### ASSUMPTIONS and NOTES TO USER: ####
#...0) Start 
#...1) Simrad .raw data files are available, in a separate folder for stn2 and a separate folder for stn1
#...2) Echoview .ecs calibration files are available
#...3) as of July 2021 - assumes one calibration file for each echosounder for any given week of .raw files
#...4) an Echoview template file is available
#...5) as of July 2021 - this script assumes that every EV file will hold 7 days worth of data
#...6) as of July 2021 - "start of Week" is 6pm (local time) on Sunday with Site stn2 as the "baseline" (0 hour offset)
#      the consequence of "baseline" is that the lag-time adjustment will be made to the stn1 files
#...7) as of July 2021 - this script assumes that all .raw files assigned to a particular week are to be included in the EV file
#      regardless of how many there are or whether there is matching data from the other site (e.g. an echosounder stops recording for a period)
#...8) this script will be run to generate the ONE EV file required for the data collected during a particular calendar week
#...9) This script was written specifically for the 2021 Cambodia Tonlesap Project.
#      ...datasets from 2 sites (stn1 and stn2) will act as two filesets within the Echoview files.
#...10) The script assumes that the .raw files associated with each fileset are
#      are in a fileset-specific folder 
#      (i.e. a folder with stn1 .raw files and a folder with stn2 .raw files.)
#...11) The string segments that we'll use for identifying date and time within the filenames
#      ("D202" and "-T") are specific to date and time and therefore not found within the filename
#      for any other purpose than finding date and time
#      IMPLICATION: Do NOT use "-Tonlesap" in the .raw filenames. You CAN use "_Tonlesap"
#...12) The folder names do not include the patterns we need to look for in the file names
#      (IMPLICATION: as of June 2021 DO NOT INCLUDE "D202" or "-T" in the folder names)
#...13) The script also assumes that there is one calibration file per week associated
#      with each fileset.
#...14) If it becomes apparent that there should be more than one calibration file
#      associated with files from one site (i.e. temperature/salinity change sufficiently
#      during data collection that sound speed should be modified), it is VITAL that the
#      user defines the appropriate calibration file (in the User-Defined Variables) associated
#      with the week's worth of .raw files that are being processed into an EV file.
#...15) Echoview .EV template file is prepped and available
#      ..."prepped" all settings within the template file
#...16) This script has been written with variable names specific to the two sites
#      used in the Tonlesap River Cambodia in 2020.
#...17) Based on a comment made in July 2021, LPM has forced the list of files to be alpha sorted
#      using gtools::mixedsort. It's not immediately evident to LPM that this is necessary
#      or whether this will cause headaches. (mixedsort sorts file1, file10, file12, file2 into file1, file2, file10, file12)
#      but given that the script currently finds the file.list index of those files starting at
#      EV.start_hour and then sucks in every file in between the indices, it'd be good if the files
#      were in appropriate order.

  
  
  
### NOTES to USERS
#...1) dates are in YYYYMMDD form - consistent with the date format in the .raw filenames
#...2) VARIABLE DESCRIPTIONS
#      "file_name"          = name of the .raw file
#      "file_date.YMD"      = date extracted from the name of the .raw file  
#      "file_time.HMS"      = time extracted from the name of the .raw file
#      "file_datetime"      = the date and time extracted from the .raw file combined with timezone
#                             useful for calculating time offsets which carry along the change of date if we go past midnight
#      "lag_offset.H"       = how much time lag was built into this data set 
#                             (i.e. stn2 = 0, stn1 = the lag between the echosounder recording at stn1 and the time that parcel of water is expected at stn2 )
#                             the purpose of this variable is to make explicit in the saved data, what lag was used for that week's data
#                             this variable is not mission critical (you can back into it from the "file_pseudodate")
#      "file_datetime.wlag" = the file datetime adjusted for the lag (i.e. "with lag" ...wlag)
#                             for site stn2 the "wlag" column will equal the "file_datetime" column (i.e. no lag because stn2 is the "baseline")
#      "file_date.wlag"     = data date inclusive of the lag (i.e. if we cross over midnight - we need the updated date that we're starting with)
#      "file_pseudodate"    = this column holds the calculated date on which we want to present the data
#                             for example: 6pm -> we want reported the next day because we've defined our "day" as 18:00 to 17:59.999999


### STEPS executed with this script (assuming "contemporary" surveys): ####
#...1) CLEAR workspace
#...2) ATTACH packages
#...3) USER-DEFINED FUNCTIONS used in this script
#...4) Capture DEFAULT SETTINGS (in case we need to revert to them - not used in this script)
#...5) SET Global Settings (optional)
#...6) CAPTURE RunDate and RunTime
#...7) USER-DEFINED VARIABLES  (YOU need to set these)
#...8) TEST for existence of INPUT DATA FILEPATH as defined in the User-Defined Variables
#      If test fails, terminate the script and alert the user to the issue
#...9) TEST for existence of OUTPUT FILEPATH as defined in the User-Defined Variables
#      If test fails, create the filepath as defined in the User-Defined Variables and alert user
#      Continue script
#..12) CAPTURE list of Simrad .raw FILENAMES
#      Verify that there is at least one file to process, if not terminate the script and alert the user


  



  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#####  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  
  
  
  
  
  
  
  
  


#                                                                           ####
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### CLEAR WORKSPACE before we begin                                          ####

#clear the workspace (Global Environment) of all objects
rm(list=ls())

#close any open graphics/plots
graphics.off()



#                                                                           ####
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### START TIMER CLOCK                                                        ####

start_time <- Sys.time()  
#                                                                           ####
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### ATTACH PACKAGES                                                          #####

requireNamespace("lubridate")                       #for dealing with date and time formatting (ymd, month, year)
requireNamespace("hms")                             #LPM may replace using lubridate with hms (for dealing with time)
requireNamespace("dplyr")                           #for working with dataframes
requireNamespace("stringr")                         #needed for manipulating or testing strings
requireNamespace("gtools")                          #used for sorting file names in (human) alpha order (i.e. file1, file2, file3 rather than file1, file10, file 11, file2, etc)
#install.packages("devtools")                       #needed for forcing install of packages RDCOMclient and EchoviewR in R versions >4.0
devtools::install_github("omegahat/RDCOMClient")   #forces install of RDCOMclient
devtools::install_github('AustralianAntarcticDivision/EchoviewR') #forces install of EchoviewR 
library(RDCOMClient)                                #R requires RDCOMClient to talk to EV
library(EchoviewR)                                  #needed for running Echoview
                                                    #LPM prefers to use requireNamespace, but for EchoviewR we'll just load it

#                                                                           ####
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### USER-DEFINED FUNCTIONS and SCRIPT CALLS used in this script
#      These should not need editing, but they are needed for the running of the script
#      So they are identified here as "User-Defined" as opposed to "Built In"
#...............................................................................
#...........................................................................####
# FUNCTION:  TEST INPUT DIRECTORY EXISTS            - and if it doesn't, catastrophic failure - quit script and alert user  ####
TEST_INPUTdirExist <- function(DIR_dataFiles) {
  DIRexist_dataFiles <- dir.exists(DIR_dataFiles)
  if (isFALSE(DIRexist_dataFiles)) {
    message (" ")
    message ("**************************************************************************************")
    message ("HEADSUP!")
    message (" ")
    message ("Can't find the directory where the data files are stored.")
    message ("PLEASE DOUBLE CHECK that the DIR_dataFiles variable has the full and correct filepath,") 
    message (DIR_dataFiles)
    message ("and is encapsulated in quotes and ending with '/'")
    message (" ")
    message ("Variables have been cleared and script will terminate.")
    message (" ")
    message ("**************************************************************************************")
    return(DIRexist_dataFiles)
  } #end if
  
} #end function


#............................................................................
# FUNCTION:  TEST INPUT FILE (NAMED) EXIST            - and if it doesn't, catastrophic failure - quit script and alert user   ####
TEST_NamedFILEexists <- function(InputFile) {
  FILESexist <- file.exists(InputFile)
  if (!FILESexist) {
    message (" ")
    message ("*************************************************************************************")
    message ("HEADSUP!")
    message (" ")
    message ("Can't find the file:")
    message (basename(InputFile))
    message ("")
    message ("as specified in the following directory:")
    message (paste(dirname(InputFile), "/", sep = ""))
    message ("")
    message ("PLEASE DOUBLE CHECK that the file exists in the directory defined - and that the filename is spelled correctly.")  
    message (" ")
    message ("Variables will be cleared and script will terminate.")
    message (" ")
    message ("**************************************************************************************")
    return(FILESexist)
  } #end if
  
} #end function  


#...............................................................................
# FUNCTION:  TEST OUTPUT DIRECTORY EXISTS           - and if it doesn't, then create it, alert user     ####                      
TEST_OUTPUTdirExist <- function(DIR_Output) {
  DIRexist_Output <- dir.exists(DIR_Output)              #populate DIRexist_OutputAppended with TRUE or FALSE
  if (isFALSE(DIRexist_Output)) {                                #if FALSE (i.e. directory doesn't exist)
    dir.create(DIR_Output)                                     #create the EXPORT DIRECTORY...at the location assigned in DIR_OutputAppended above.
    message (" ")
    message ("******************************************************") 
    message ("Created OUTPUT directory:  ")
    message (DIR_Output)   #alert user
    message (" ")
    message ("Script continues...")
    message ("******************************************************")
    message (" ")
  } #end if
  
} #end function


#...............................................................................
# FUNCTION:  TEST INPUT FILES (MANY) EXIST in DIRECTORY       - and if it doesn't, catastrophic failure - quit script and alert user   ####
TEST_DirFILESexist <- function(InputFiles) {
  if (length(InputFiles) == 0) {
    FILESexist <- FALSE
    message (" ")
    message ("*************************************************************************************")
    message ("HEADSUP!")
    message (" ")
    message ("Found no files in location specified by DIR_dataFiles.")
    message ("PLEASE DOUBLE CHECK that the DIR_dataFiles variable defines the correct path to the .csv EV-export files")  
    message (" ")
    message ("Variables will be cleared and script will terminate.")
    message (" ")
    message ("**************************************************************************************")
    return(FILESexist)
  } #end if
  
} #end function  


#...........................................................................####
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### CAPTURE GLOBAL SETTINGS that will (or will not) be modified below        ####

#CAPTURE the default number of display digits
r_digits <- options()$digits                   #This will tell you how many digits R is defaulted to show you
#                                                #You may find you need/want more digits to show.
#                                                #You can set the number of digits in the options command in the next section.

#CAPTURE the default graphics settings 
#graphics_defaults <- par(no.readonly = TRUE)   #IF we end up doing any graphing in this script
#                                                #and that graphing requires changes to the defaults
#                                                #you can uncomment this line and run the script
#                                                #This will allow you to reset to the graphing defaults if you need/want to.


#                                                                           ####
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### SET GLOBAL SETTINGS that modify R defaults --- OPTIONAL                  ####
#   LPM makes this coding available in all her scripts
#       1) uncommmenting this code is optional
#       2) LPM likes the code handy for debugging and manual confirmation of results

#  options(digits = 15)   #you can uncomment this line if you need to see more digits
#                        #and you can set that line to any number that's useful to you
#                        #R does not recommend going past 15
#                        #http://astrostatistics.psu.edu/su07/R/html/base/html/options.html
#                        #LPM found it useful when proofing that the conversion from a date column (yyyymmdd) plus a time column (hh:mm:ss.SSS)
#                        #was appropriately calculated into one Date.time column (DateTime_S and DateTime_E) used for merging.

#                                                                           ####
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### CAPTURE RunDate and RunTime -  Needed for Output Filename                ####
# These are used in the generation of the output filename
runDate <- Sys.Date()
runDate <- stringr::str_remove_all(runDate, "-")
runTime <- stringr::str_remove_all(format(as.POSIXlt(Sys.time()), format = '%T'), ":")
runDateTime <- paste("D",runDate,"_T",runTime, sep = "")
#                                                                           ####
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@







#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### START---USER DEFINED VARIABLES @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#...............................................................................
# DESIGNATE   INPUT/OUTPUT LOCATIONS                                         ####
#...............................................................................
### STATE HOW MANY FILESETS
EV.num_filesets <- 2     #tell this script how many filesets will be included in each EV file
#this must match the number of DIR_rawFiles (i.e. if you have a folder for stn1 and stn2 it'll be 2 filesets)

### DEFINE the pattern that specifies the START HOUR (as specified in the .raw file name)
#   as of June 2021, the start hour is 6pm local time (i.e. 1800)
#   and the .raw filename includes time in UTC
#   Cambodia time throughout the year is UTC + 7 (no change for daylight savings time)
#   So the start hour in the filename is "1100" (1800 local - 7 UTC conversion)
EV.start_hour <- 11    #as of June 2021, the desired start hour of data in local time is 6pm    

### DEFINE the number of hours to offset between the two filesets
EV.lag_hours.stn2  <- 0  # LPM is capturing this in the dataframe in order to be explicit
#                      that no lag was used for stn2 ...because we've assigned stn2 as our baseline
EV.lag_hours.stn1 <- 10 #as of June 2021, it is estimated that there is a 10-hour lag
#                      in transit time of waters from the northern site (stn1) to the southern site (stn2)
#     NOTE: based on July 23, 2021 conversation with JKH, LOUISE is going to make it easier to adjust
#           the stn1 lag (either put it in a config file, so the user doesn't have to modify this script
#           or some other brilliant solution....)

### PATTERN for DATE/TIME SEARCH in FILENAME  ....   *** DO NOT CHANGE THESE TWO ***
pattern.date <- "-D202"   # how will we identify date string within the .raw filenames
pattern.time <- "-T"      # how will we identify the time string within the .raw filenames




#...............................................................................
### DEFINE LOCATION to FIND the INPUT .raw SIMRAD files to be placed in EV files
DIR_rawFiles.stn2     <- ("PATH/TO/data/raw/stn2/")
DIR_rawFiles.stn1    <- ("PATH/TO/data/raw/stn1/")
# NOTE: If these paths don't exist, there is coding below to print an alert to the Console for the user
#       and then terminate the script.

#...............................................................................
### DEFINE LOCATION and NAME of the .ecs CALIBRATION FILE
# as of July 2021 - assumes one calibration file for each echosounder
# as of July 2021 - assumes that the whole path including filename is stored in FILE_ecs.stn2 or stn1
FILE_ecs.stn2  <- ("PATH/TO/data/raw/cal/FILE_stn2.ecs")
FILE_ecs.stn1 <- ("PATH/TO/data/raw/cal/FILE_stn1.ecs")
# NOTE: If these files don't exist, there is coding below to print an alert to the Console for the user
#       and then terminate the script.

#...............................................................................
### DEFINE LOCATION and NAME of the Echoview TEMPLATE FILE
# as of July 2021 - assumes that the whole path including filename is stored in FILE_EVtemplate
FILE_EVtemplate      <- ("PATH/TO/data/ev/templates/template.EV")
# NOTE: If this file doesn't exist, there is coding below to print an alert to the Console for the user
#       and then terminate the script.


#...............................................................................
### DEFINE LOCATION to SAVE the OUTPUT EV files 
DIR_SaveEVFiles <- ("PATH/TO/data/ev/")
# NOTE: If this path doesn't exist, there is coding below that will create the DIR_dataFiles path
#       and alert the user

#...............................................................................



#...............................................................................
# DEFINE     base pattern   for GENERATING OUTPUT EV FILE NAME            ####
# ...SURVEY INDICATORS...
ProjectIndicator    <- "TS2022-23"                               # used in filename of the output EV file
Location            <- "TonlesapRiver"                           # used in filename of the output EV file
SurveyType          <- "WBTMiniStationaryDownlooking"            # used in filename of the output EV file

DataTime            <- "UTC"                                  # used in filename of the output EV file what time zone is used for the date/time included in these files
                                                              # NOTE: also used in the conversion of the Thhmmss to time - so be sure to specify timezone appropriate to R
                                                              # https://stat.ethz.ch/R-manual/R-devel/library/base/html/timezones.html

# As of Oct 2021, these two are hardcoded, but we can make them calculated from the system date/time.
# Per JKH (Nov 14, 2021), leave these as hard coded (do not use computer system date/time)
# ...but may want to place these higher in the User-Defined Variables section. UW to decide.
ISOYear             <- 2023                                   # used in filename of the output EV file
ISOWeek             <- 1                                      # used in filename of the output EV file


                                                                       
# DEFINE      OUTPUT FILENAME using the SURVEY INDICATORS
# You CAN edit this...but not necessary                                      
#NOTE: output_baseFILENAMEdata doesn't need to be edited - it pulls from the SURVEY INDICATORS you define above
#      and will add the start/end date time
#LPM has left output_baseFILENAMEdata in the "USER-DEFINED" section in case the user wants to adopt a different standard for defining the output filename

#Set filenames for output (data and figures)  .. You don't need to edit these unless you need/want to change the output FILENAMES
output_baseFILENAME.EV     <- paste(ProjectIndicator, "_", Location, "_", SurveyType, "_", DataTime, "_ISOYear_", ISOYear, "_ISOWeek_",ISOWeek, sep = "")  
# no filetype defined here in case we decide below to add more info to the base filename before saving the EV file 

#WARNING: This R code will overwrite and existing file of the same name without warning.
#So please be careful when running this code such that you don't lose prior work that you want to keep 


#.                                                                          ####
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### END---USER DEFINED VARIABLES @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@












#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### BEGIN---PROCESSING  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
###  TESTS to verify that FILEPATHS and named FILES exist     ####

# TEST that INPUT DIRECTORY EXISTS (stn2) - and if it doesn't, catastrophic failure - quit script and alert user
result <- TEST_INPUTdirExist(DIR_rawFiles.stn2)
if (isFALSE(result)) {
  rm(list=ls())             #if the script can't find the DIR_dataFiles, remove all the variables from the Global Environment
  stop("SCRIPT TERMINATED")
}
rm(result)  #remove variables no longer needed


# TEST that INPUT DIRECTORY EXISTS (stn1) - and if it doesn't, catastrophic failure - quit script and alert user
result <- TEST_INPUTdirExist(DIR_rawFiles.stn1)
if (isFALSE(result)) {
  rm(list=ls())             #if the script can't find the DIR_dataFiles, remove all the variables from the Global Environment
  stop("SCRIPT TERMINATED")
}
rm(result)  #remove variables no longer needed



# TEST that INPUT .ecs EXISTS (stn2)- and if it doesn't, catastrophic failure - quit script and alert user
# assumes the whole path is included in the variable called "FILE_ecs.stn2"
result <- TEST_NamedFILEexists(FILE_ecs.stn2)
if (isFALSE(result)) {
  rm(list=ls())             #if the script can't find the DIR_dataFiles, remove all the variables from the Global Environment
  stop("SCRIPT TERMINATED")
}
rm(result)  #remove variables no longer needed



# TEST that INPUT .ecs EXISTS (stn1)- and if it doesn't, catastrophic failure - quit script and alert user
# assumes the whole path is included in the variable called "FILE_ecs.stn1"
result <- TEST_NamedFILEexists(FILE_ecs.stn1)
if (isFALSE(result)) {
  rm(list=ls())             #if the script can't find the DIR_dataFiles, remove all the variables from the Global Environment
  stop("SCRIPT TERMINATED")
}
rm(result)  #remove variables no longer needed


# TEST that INPUT EV-Template EXISTS - and if it doesn't, catastrophic failure - quit script and alert user
# assumes the whole path is included in the variable called "FILE_ecs.stn1"
result <- TEST_NamedFILEexists(FILE_EVtemplate)
if (isFALSE(result)) {
  rm(list=ls())             #if the script can't find the DIR_dataFiles, remove all the variables from the Global Environment
  stop("SCRIPT TERMINATED")
}
rm(result)  #remove variables no longer needed


# TEST that OUTPUT DIRECTORY EXISTS - and if it doesn't, then create it and alert user                     
TEST_OUTPUTdirExist(DIR_SaveEVFiles)

#                                                                           ####
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
###  TEST THAT INPUT .raw FILES EXIST and CAPTURE THEIR NAMES     

# LOUISE - ALERT!!! put in the coding to eliminate the files entered into previous EV files
# per JKH (July 2021) - this script will be run once per week (on Tuesdays) to make the EV file (ONE) for the previous week's data collection)

###  CAPTURE THE LIST OF FILENAMES WITH .raw as FINAL EXTENSION              ####
rawFiles.stn2  <- list.files(path = DIR_rawFiles.stn2,  full.names = T, pattern = '*.raw$')
# "$" forces "list.files" to include only those files that end in .raw 
#     without the "$", list.files would pick up .raw.evi files as well
#     inclusion of the "$" at the end of .raw assures that we only pick up the files we want
###  FORCE The LIST into ALPHA ORDER
rawFiles.stn2  <- gtools::mixedsort(rawFiles.stn2)
#...............................................................................
rawFiles.stn1 <- list.files(path = DIR_rawFiles.stn1, full.names = T, pattern = '*.raw$')               
# "$" forces "list.files" to include only those files that end in .raw 
#     without the "$", list.files would pick up .raw.evi files as well
#     inclusion of the "$" at the end of .raw assures that we only pick up the files we want
### FORCE the LIST into ALPHA ORDER
rawFiles.stn1  <- gtools::mixedsort(rawFiles.stn1)
#...............................................................................
#...............................................................................



###  TEST that INPUT FILES EXISTS - and if they don't, catastrophic failure - quit script and alert user  ####
# Test stn2 files exist....
result <- TEST_DirFILESexist(rawFiles.stn2)
if (isFALSE(result)) {
  rm(list=ls())             #if the script can't find the dataFiles, remove all the variables from the Global Environment
  stop("SCRIPT TERMINATED")
}
rm(result)       #remove variable no longer needed
#...............................................................................
# Test stn1 files exist...
result <- TEST_DirFILESexist(rawFiles.stn1)
if (isFALSE(result)) {
  rm(list=ls())             #if the script can't find the dataFiles, remove all the variables from the Global Environment
  stop("SCRIPT TERMINATED")
}
rm(result)       #remove variable no longer needed
#...............................................................................
#...............................................................................




###  TELL USER how many files to be imported and appended                    ####
# stn2
message (" ")
message (" ")
message ("********************************************************************")
message ("********************************************************************")
message (" ")
message (paste("There are  ", length(rawFiles.stn2), "   stn2 files from  ", ProjectIndicator, "  available to import into EV.", sep = ""))
#...............................................................................
# stn1
message (" ")
message (" ")
message (paste("There are  ", length(rawFiles.stn1), "  stn1 files from  ", ProjectIndicator, "  available to import into EV.", sep = ""))
message (" ")
message ("********************************************************************")
message ("********************************************************************")
message (" ")
message (" ")
#...............................................................................
#...............................................................................
#                                                                           ####
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# We now have the name of the files for stn2 and stn1 available to us
#        in the rawFiles.stn2 and rawFiles.stn1 variables.                    




#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# The guts of the work of the script
# Getting prepared to generate the EV file                                   ####


#...............................................................................

# Setup dataframes (populates the dataframes with NA)

colnames_data.dataframes <-  c("file_name", "file_date.YMD", "file_time.HMS", "file_datetime", 
               "lag_offset.H", "file_datetime.wlag", "file_date.wlag", "file_pseudodate", "file_ISOYear", "file_ISOWeek")

data.stn1 <- data.frame(matrix(ncol = length(colnames_data.dataframes), nrow = length(rawFiles.stn1)))
colnames(data.stn1) <- colnames_data.dataframes

data.stn2  <- data.frame(matrix(ncol = length(colnames_data.dataframes), nrow = length(rawFiles.stn2)))
colnames(data.stn2)  <- colnames_data.dataframes

#...............................................................................

# Populate the dataframes - file_name
data.stn1$file_name <- rawFiles.stn1
data.stn2$file_name  <- rawFiles.stn2

#...............................................................................

# Populate the dataframes - file_date.YMD
# inside the "as.Date" we find the date portion of the string - and then with "as.Date" convert it to a date format YYYY-MM-DD
date_position.stn1  <- stringr::str_locate(data.stn1$file_name, pattern.date)
data.stn1$file_date.YMD <- as.Date(stringr::str_sub(data.stn1$file_name, date_position.stn1[,"start"] + 2, date_position.stn1[,"start"] + 9), "%Y%m%d")


date_position.stn2  <- stringr::str_locate(data.stn2$file_name, pattern.date)
data.stn2$file_date.YMD <- as.Date(stringr::str_sub(data.stn2$file_name, date_position.stn2[,"start"] + 2, date_position.stn2[,"start"] + 9), "%Y%m%d")


#...............................................................................

# Populate the dataframes - file_time.HMS
time_position.stn1  <- stringr::str_locate(data.stn1$file_name, pattern.time)
data.stn1$file_time.HMS <- stringr::str_sub(data.stn1$file_name, time_position.stn1[,"end"] + 1, time_position.stn1[,"end"] + 6)
data.stn1$file_time.HMS <- format(lubridate::parse_date_time(data.stn1$file_time.HMS, "%H%M%S"), format = "%H:%M:%S")


time_position.stn2  <- stringr::str_locate(data.stn2$file_name, pattern.time)
data.stn2$file_time.HMS <- stringr::str_sub(data.stn2$file_name, time_position.stn2[,"end"] + 1, time_position.stn2[,"end"] + 6)
data.stn2$file_time.HMS <- format(lubridate::parse_date_time(data.stn2$file_time.HMS, "%H%M%S"), format = "%H:%M:%S")

#...............................................................................

# Create one datetime column from which to calc the time
# I tried to do this in one step (wrap the as.POSIXct around the original paste), but R would have none of it (sigh...)
data.stn1$file_datetime <- paste(data.stn1$file_date.YMD, data.stn1$file_time.HMS)
data.stn1$file_datetime <- as.POSIXct(data.stn1$file_datetime, tz = DataTime)

data.stn2$file_datetime <-  paste(data.stn2$file_date.YMD, data.stn2$file_time.HMS)
data.stn2$file_datetime <- as.POSIXct(data.stn2$file_datetime, tz = DataTime)

#...............................................................................

# Record the lag (hours) offset relative to Site stn2 
# (i.e. lag offset at stn2 = 0, lag offset from stn1 is set by the user above)

data.stn1$lag_offset.H <- EV.lag_hours.stn1
data.stn2$lag_offset.H  <- EV.lag_hours.stn2

#...............................................................................

# Calculate the file datetime inclusive of the lag

data.stn1$file_datetime.wlag <- data.stn1$file_datetime + lubridate::hours(data.stn1$lag_offset.H)
# calculting stn2 lag is redundant (we could simply copy the datetime column), but LPM is leaving it here
# for consistency and just in case things change going forward (i.e. they decide differently about assigning lag)
data.stn2$file_datetime.wlag <- data.stn2$file_datetime + lubridate::hours(data.stn2$lag_offset.H)


data.stn1$file_date.wlag <- lubridate::date(data.stn1$file_datetime.wlag)
data.stn2$file_date.wlag  <- lubridate::date(data.stn2$file_datetime.wlag)


#...........................................................................####
### Up until this point, we've simply been preparing the data 
### After this point, it matters which site we're using as the "baseline"
#   i.e. at which site, stn1 or stn2, are we starting our day at 6pm (local)



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Put the ISOWeek calculations to use                                        ####
# Assign ISOWeek - to make assigning to EV file easier
# first: we need to assign a signifier to make assigning the ISOWeek easier
#        Given that the ISOweek function in lubridate library operates on the assigned date
#        AND that the days for this project start at 6 pm local (or 1100 UTC), we'll
#        assign a pseudodate based on whether the time in the filename (UTC) is before
#        or after 1100
# We're going to do the two datasets separately in order to alleviate any differences
# in number of rows that may arise from differing start/ending hours - or missed hours
# due to echosounder malfunction (etc)





# stn1 - Assign date to use in calculating ISOweek 
class(data.stn1$file_pseudodate) <- "Date"
for (zz in 1:nrow(data.stn1)) {
  if (lubridate::hour(data.stn1$file_datetime.wlag[zz]) >= EV.start_hour){
    data.stn1$file_pseudodate[zz]  <- data.stn1$file_date.wlag[zz] + 1
  } else {
    data.stn1$file_pseudodate[zz]  <- data.stn1$file_date.wlag[zz]
  } # end if else
} # end for


# stn2 - Assign date to use in calculating ISOweek
class(data.stn2$file_pseudodate) <- "Date"
for (zz in 1:nrow(data.stn2)) {
  if (lubridate::hour(data.stn2$file_datetime.wlag[zz]) >= EV.start_hour){
    data.stn2$file_pseudodate[zz]  <- data.stn2$file_date.wlag[zz] + 1
  } else {
    data.stn2$file_pseudodate[zz]  <- data.stn2$file_date.wlag[zz]
  } # end if else
} # end for
  
  



  
# Assign a year and week number to each filename based on the calculated "pseudodate"
# The pseudodate is used to adjust those Sundays after 6pm to "Monday"
# and to get the lagged data into the right week as well.
# the isoweek function simplifies the effort to assign a .raw file to a week
# ...it really helps that the ISO 8601 standard is such that a week starts on a Monday.

data.stn1$file_ISOYear <- lubridate::isoyear(data.stn1$file_pseudodate)
data.stn2$file_ISOYear <- lubridate::isoyear(data.stn2$file_pseudodate)

data.stn1$file_ISOWeek <- lubridate::isoweek(data.stn1$file_pseudodate)
data.stn2$file_ISOWeek <- lubridate::isoweek(data.stn2$file_pseudodate)



# Now that we have a simple way of designating which .raw files to process
# (file_ISOYear, file_ISOWeek), we can use the index of the file names which 
# match the ISOYear/ISOWeek criteria defined above

index_stn1_ISOYearISOWeek <- which(data.stn1$file_ISOYear == ISOYear & data.stn1$file_ISOWeek == ISOWeek)
index_stn2_ISOYearISOWeek <- which(data.stn2$file_ISOYear == ISOYear & data.stn2$file_ISOWeek == ISOWeek)




#.                                                                          ####
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@











#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# CREATE the EV File 
# ...use the EV template
# ...each EV file contains one week's worth of data
# ...two filesets (stn2 and stn1)
# ...add one calibration file per fileset
# ...........................................................................####









# Open Echoview 
EVApp <- COMCreate('EchoviewCom.EvApplication')
EVApp$Minimize()






################################################################################

# Go through all the data files in the directory, load them into the EV
# save the EV file to the save directory


# JACKSON - if this doesn't give you what you want- let me know
# (I removed some lines of code that I belive to be no longer needed)
  
  # specify the calibration file that will get imported to EV below
  EV.ecs_stn1=FILE_ecs.stn1
  EV.ecs_stn2=FILE_ecs.stn2

  
  # specify the raw files that will get imported to EV below
  EV.raw_stn1 = rawFiles.stn1[index_stn1_ISOYearISOWeek]
  EV.raw_stn2 = rawFiles.stn2[index_stn2_ISOYearISOWeek]
  
  # Make new EV file from template
  EVFile = EVApp$NewFile(FILE_EVtemplate)
  
  #EVFile = EVCreateNew(EVApp = EVApp, templateFn = FILE_EVtemplate)  #JACKSON: I believe this line to be needed no longer
  
  # Load specified raw files to EV file
  #WAS ORIGINALLY "FILESET1", CHANGE IT BACK IF THIS FAILS.
  EVAddRawData(EVFile, 'stn1', EV.raw_stn1) # specify how many/which data files you want to load 
  EVAddRawData(EVFile, 'stn2',  EV.raw_stn2)
  
  # Load specified .ecs file to EV file
  EVAddCalibrationFile(EVFile, 'stn1', EV.ecs_stn1)
  EVAddCalibrationFile(EVFile, 'stn2', EV.ecs_stn2)
  
  # Save the EV file
  # EV file name will show the timestamp of the first and last raw files in the EV file
  EVSaveAsFile(EVFile,paste0(DIR_SaveEVFiles,output_baseFILENAME.EV, ".EV"))
  
  # Close the EV file
  EVCloseFile(EVFile)
  


# Quit Echoview
QuitEchoview(EVApp)













#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
message (" ")
message (" ")
message ("*********************************************************************")
message ("*********************************************************************")
message (" ")
message (" ")
message (" EV Files saved and ready for inspection and post-processing in")
message (paste("    ", DIR_SaveEVFiles))
message (" ")
message (" SCROLL UP to REVIEW any MESSAGES printed to the Console")
message (" ")
message (" ")
message ("*********************************************************************")
message ("*********************************************************************")
message (" ")
message (" ")
message ("  ***  END OF SCRIPT  ***")




} # end opening bracket















