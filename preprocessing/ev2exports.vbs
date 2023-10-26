'Export data, echograms, lines, settings

'Assumptions when written:
'  ...stationary (i.e. not exporting cruise track)
'  ...export data grid = 1 TimeBin
'  ...export data grid = 1-m DepthBins + FullWater Column
'  ...export data from "processed data" only  ("ANALYSIS")
'  ...export echograms and settings from BOTH "processed data" ("ANALYSIS") and from "Sv pings T1" ("RAW")

'Created by:  Louise P. McGarry, Ph.D. -  December 2020









'Strict syntax checking
option explicit



'******************************************************************************************************
'******************************************************************************************************
'**************  START - USER DEFINED VARIABLES  ******************************************************
'******************************************************************************************************


'DEFINE paths for input and output
'       ...make sure the folder paths end with a backslash \
'       ...make sure the "Output" folders exist before running the script or script will close leavin you no output files



'LOCATIONS for STATIONARY Bottom-Deployed echosounder
Const InputEVFilePath   =  "..\data\ev\"          'where the existing EV files are located
Const OutputEvFilePath  =  "..\data\ev_exports\"  'where to save a copy of the EV file that generated the export
Const OutputExportPath  =  "..\data\ev_exports\"  'where to save the exported data and the exported .EVD file


'TIME INTERVAL to EXPORT (in minutes: 0.1 to 9999)
'(0.1 minutes = 6-second)
Const TimeBin = 60


'DEPTH INTERVAL to EXPORT
Const DepthBin1 = 1         'export data in 1-meter depth bins
Const DepthBin2 = 100       '200 meter is equivalent to "full water column" (i.e. make it well deeper than the actual water column)

  
'VARIABLES for EXPORTING DATA  
Const VariableToExportstn1_BNR = "stn1 Background noise removal stn1"  	'= 'analysis data' ...The variable to integrate and export cells from, also export jpg
Const VariableToExportstn2_BNR = "stn2 Background noise removal stn2" 		'= 'raw data' ...Used to export jpg of raw data 
Const VariableToExportstn1_TS = "Mask Single targets stn1"
Const VariableToExportstn2_TS = "Mask Single targets stn2"
'NOTE: To export the cruise track jpg, the underlying acoustic variable exported needs to have a greid defined (see below)
'      ...NOTE: Grand Passage project is stationary data. So "cruise track" is irrelevant.


'DEFINE WHICH LINES FOR REFERENCE LINES (the name of the line to use as the grid reference)
Const RefLine1NAME = "Far-Field Line stn1"           'Name of Line to Use for "Top"
Const RefLine2NAME = "Far-Field Line stn2"            'Name of Line to Use for "Bottom"
'Const RefLine3NAME = "Far-Field Line stn2"
'Const RefLine4NAME = "Smoothing filter stn2"


'LINES for EXPORTING LINE DATA
'NOTE: These may need to be editable lines
Const LineToExport1 = "Far-Field Line stn1"           'The top line to export
Const LineToExport2 = "Smoothing filter stn1"   'The turbulence line to export
Const LineToExport3 = "Bottom Line stn1"                'The bottom line to export
Const LineToExport4 = "Far-Field Line stn2"           'The top line to export
Const LineToExport5 = "Smoothing filter stn2"   'The turbulence line to export
Const LineToExport6 = "Bottom Line stn2"     

'TEXT TO INCLUDE IN FILENAME
Const TimeIndicatorForFileName = "UTC"  'This text just gets added to the filename. ...EDIT THIS LINE IF NECESSARY


'THRESHOLD IN USE IN THE ANALYSIS VARIABLE (VariableToExport1) IN THESE EV FILES 
Const ColorbarSvMin1    = -78  'although we'll set the Sv minimum threshold to -66, by setting the colorbar to -76, we keep the color scale equivalent between "analysis" and "raw"
Const ColorbarRange1    =  36  'Range 36 dB for the EK500 colorbar = 3 dB increment per color    (3 dB = double/halving)
Const ColorbarSvMin2    = -110  'an order of magnitude lower than the Analysis thresold (-66 in the current project)
Const ColorbarRange2    =  36


'NAME OF THE COLORBAR
Const ColorScheme1 = "EK500"
Const ColorScheme2 = "EK500"


'MINIMUM Sv THRESHOLD TO APPLY TO THE DATA
Const MinSvThreshold1 = -68
Dim MinSvThreshold2  'this Dim line is technically optional
MinSvThreshold2 = ColorbarSvMin2
Const TsThreshold1 = -110

'IF YOU NEED TO CHANGE THE "EXPORT EMPTY CELLS SETTING"
Const EmptyCellsTF = "false" ' Enter true or false, as a string ("true" = export empty cells, "false" = don't export empty cells)


'**************************************************************************************************************************
'**************************************************************************************************************************
'*********  END - USER DEFINED VARIABLES  *********************************************************************************
'**************************************************************************************************************************




'------------------------------------------------------------------------------------------------------------
'------------------------------------------------------------------------------------------------------------
' We want these objects to be available all through the script
'------------------------------------------------------------------------------------------------------------

Dim FSO   'FSO = "Files System Object" used to gain access to computer's file system (i.e. access or create files)
Set FSO = CreateObject("Scripting.FileSystemObject")

' Open the Echoview Application
Dim EvApp
Set EvApp = CreateObject("EchoviewCOM.EvApplication")

'Per Echoview, delete scripting log file at the start of the script. The scripting log file can become very large, and slow down the processing.
'FSO.DeleteFile(EvApp.LogFileName)

' Minimize the EV application
EvApp.Minimize()


'-----------------------------------------------------------------------------
Sub QuitWithError(ErrorMessage)
	MsgBox ErrorMessage + vbCrLf + vbCrLf + "Exiting.", vbOk + vbError, "Error"
	WScript.quit 1
End Sub

'-----------------------------------------------------------------------------
' Create the source folder object from the Input path you defined above
Dim EvFileFolder
Set EvFileFolder = FSO.GetFolder(InputEVFilePath)

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'The SUBROUTINE to export various .csv files and the .EVD file. Then close the EV file
Sub SaveAndCloseFile






'**************************************************************************************************************************
'**************************************************************************************************************************
'*************  HERE WE START THE "per EV-file WORK   *********************************************************************
'**************************************************************************************************************************

	

'DEFINE FILENAME FOR EXPORTS
'  THIS FORM USES THE NAME OF .RAW FILE IN THE FILESET AS ITS BASE  (THIS CAN BE CHANGED (probably should be changed))
	Dim FirstDataFileName1
	FirstDataFileName1 = FSO.GetBaseName(EvFile.Filesets.Item(0).DataFiles.Item(0).FileName)
	Dim BaseFileName1
	BaseFileName1 = OutputExportPath & FirstDataFileName1
	Dim FirstDataFileName2
	FirstDataFileName2 = FSO.GetBaseName(EvFile.Filesets.Item(1).DataFiles.Item(1).FileName)
	Dim BaseFileName2
	BaseFileName2 = OutputExportPath & FirstDataFileName2
    Dim FirstDataFileName3
	FirstDataFileName3 = FSO.GetBaseName(EvFile.Filesets.Item(0).DataFiles.Item(0).FileName)
	Dim BaseFileName3
	BaseFileName3 = OutputExportPath & FirstDataFileName3
    Dim FirstDataFileName4
	FirstDataFileName4 = FSO.GetBaseName(EvFile.Filesets.Item(1).DataFiles.Item(1).FileName)
	Dim BaseFileName4
	BaseFileName4 = OutputExportPath & FirstDataFileName4



		
'DECLARE Global Variables
	Dim ExportObject1: 	Set ExportObject1 = EvFile.Variables.FindByName(VariableToExportstn1_BNR)   ' Object variable
	Dim ExportObject1ACOUSTIC: Set ExportObject1ACOUSTIC = ExportObject1.AsVariableAcoustic  ' Declare object as an acoustic object variable

	Dim ExportObject2: 	Set ExportObject2 = EvFile.Variables.FindByName(VariableToExportstn2_BNR)   ' Declare the object variable
	Dim ExportObject2ACOUSTIC: Set ExportObject2ACOUSTIC = ExportObject2.AsVariableAcoustic  ' Declare object variable as an acoustic object variable
	'(NOTE: The step to declare ExportObject2 as acoustic is unnecessary for "Sv pings T1" which is already defined as "acoustic" in EV
	'I'm declaring it as acoustic (redundantly) in case the choice of variable declared as ExportObject2 ever gets changed.)
	Dim ExportObject3: 	Set ExportObject3 = EvFile.Variables.FindByName(VariableToExportstn1_TS)   ' Object variable
	Dim ExportObject3ACOUSTIC: Set ExportObject3ACOUSTIC = ExportObject3.AsVariableAcoustic  ' Declare object as an acoustic object variable

	Dim ExportObject4: 	Set ExportObject4 = EvFile.Variables.FindByName(VariableToExportstn2_TS)   ' Declare the object variable
	Dim ExportObject4ACOUSTIC: Set ExportObject4ACOUSTIC = ExportObject4.AsVariableAcoustic

'DEFINE EVPingSettings SO WE CAN EXPORT SETTINGS.TXT DATA 	
	Dim EVPingSettings1: Set EVPingSettings1 = EvFile.Variables.FindByName(VariableToExportstn1_BNR)
	Dim EVPingSettings2: Set EVPingSettings1 = EvFile.Variables.FindByName(VariableToExportstn2_BNR)
	Dim EVPingSettings3: Set EVPingSettings2 = EvFile.Variables.FindByName(VariableToExportstn1_TS)
	Dim EVPingSettings4: Set EVPingSettings2 = EvFile.Variables.FindByName(VariableToExportstn2_TS)
'DEFINE the lines you'll use as your grid reference lines
	Dim RefLine1:  Set RefLine1 = EvFile.Lines.FindByName(RefLine1NAME)
	Dim RefLine2:  Set RefLine2 = EvFile.Lines.FindByName(RefLine2NAME)
    'Dim RefLine3:  Set RefLine3 = EvFile.Lines.FindByName(RefLine3NAME)
	'Dim RefLine4:  Set RefLine4 = EvFile.Lines.FindByName(RefLine4NAME)  


'DESIGNATE Export Empty Cells (true = yes, false = no)
	EvApp.Exec("EvFile | ExportOutputEmptyCells =|" & EmptyCellsTF)

'SET MINIMUM THRESHOLDS....
	ExportObject1.Properties.Data.ApplyMinimumThreshold = true
	ExportObject1.Properties.Data.LockSvMinimum = false
	ExportObject1.Properties.Data.MinimumThreshold = MinSvThreshold1

	ExportObject2.Properties.Data.ApplyMinimumThreshold = true
	ExportObject2.Properties.Data.LockSvMinimum = false
	ExportObject2.Properties.Data.MinimumThreshold = MinSvThreshold1

    ExportObject3.Properties.Data.ApplyMinimumThreshold = true
	ExportObject3.Properties.Data.LockSvMinimum = false
	ExportObject3.Properties.Data.MinimumThreshold = TsThreshold1

    ExportObject4.Properties.Data.ApplyMinimumThreshold = true
	ExportObject4.Properties.Data.LockSvMinimum = false
	ExportObject4.Properties.Data.MinimumThreshold = TsThreshold1


'SET ECHOGRAM COLORBAR... (Per Echoview: Original COM coding was set up to deal with "display" so we need to use the "Exec" commands as shown here)
    EvApp.Exec(VariableToExportstn1_BNR & " | ColorScheme =| " & ColorScheme1)
    EvApp.Exec(VariableToExportstn1_BNR & " | ColorDisplayMinimum =| " & ColorbarSvMin1)
    EvApp.Exec(VariableToExportstn1_BNR & " | ColorDisplayRange =| " & ColorbarRange1)

    EvApp.Exec(VariableToExportstn2_BNR & " | ColorScheme =| " & ColorScheme1)
    EvApp.Exec(VariableToExportstn2_BNR & " | ColorDisplayMinimum =| " & ColorbarSvMin1)
    EvApp.Exec(VariableToExportstn2_BNR & " | ColorDisplayRange =| " & ColorbarRange1)

    EvApp.Exec(VariableToExportstn1_TS & " | ColorScheme =| " & ColorScheme2)
    EvApp.Exec(VariableToExportstn1_TS & " | ColorDisplayMinimum =| " & ColorbarSvMin2)
    EvApp.Exec(VariableToExportstn1_TS & " | ColorDisplayRange =| " & ColorbarRange2)

    EvApp.Exec(VariableToExportstn2_TS & " | ColorScheme =| " & ColorScheme2)
    EvApp.Exec(VariableToExportstn2_TS & " | ColorDisplayMinimum =| " & ColorbarSvMin2)
    EvApp.Exec(VariableToExportstn2_TS & " | ColorDisplayRange =| " & ColorbarRange2)


'*********************************************************************************************************
'*********************************************************************************************************
'SET initial grid size: 1 M DEPTH BINS (DepthBin1) MEASURED FROM THE SURFACE, FOR 10-MINUTE BINS (TimeBin)...
'*********************************************************************************************************

    
'ASSIGN the DEPTH-RANGE-REFERENCE-LINE...
	ExportObject1.Properties.Grid.DepthRangeReferenceLine = RefLine1
    ExportObject2.Properties.Grid.DepthRangeReferenceLine = RefLine2
    ExportObject3.Properties.Grid.DepthRangeReferenceLine = RefLine1
    ExportObject4.Properties.Grid.DepthRangeReferenceLine = RefLine2
    
'DEFINE THE PARAMETERS OF THE EXPORT GRID...
	ExportObject1.Properties.Grid.SetDepthRangeGrid 2, DepthBin1
    ExportObject2.Properties.Grid.SetDepthRangeGrid 2, DepthBin1
    ExportObject3.Properties.Grid.SetDepthRangeGrid 2, DepthBin1
    ExportObject4.Properties.Grid.SetDepthRangeGrid 2, DepthBin1 '<--X m depth grid relative to Surface plus 1m line based on our definition here
		' first number: 
		'	0 = no depth range grid; 
		'	1 = use depth grid (reference is the surface, depth = 0); 
		'	2 = use reference line (defined above as RefLine1)
		' second number: 
		'	distance between grid lines
	ExportObject1.Properties.Grid.SetTimeDistanceGrid 1, TimeBin '<-- X minute grid using time in minutes (set in the user defined variables)
	ExportObject2.Properties.Grid.SetTimeDistanceGrid 1, TimeBin
    ExportObject3.Properties.Grid.SetTimeDistanceGrid 1, TimeBin
    ExportObject4.Properties.Grid.SetTimeDistanceGrid 1, TimeBin '                                                             Note: maximum time allowed = 9999 minutes
		' first number: 
		'	0 = no time/distance grid; 
		'	1 = time in minutes; 
		'	2 = distance determined by GPS in nmi; 
		'	3 = distance according to vessel log in nmi; 
		' 	4 = distance in pings; 
		'	5 = distance using GPS in meters; 
		'	6 = distance using vessel log in meters
		' second number: 
		'	distance/time between grid lines (units depend on which number you put in first space)
		'   Note: maximum time = 9999 minutes
        '   Note: maximum distance = 10000000 m is the maximum allowed
		

'EXPORT THE GRIDDED DATA
'Here the filename has been customized to match the grid size; Edit this to match the grid you use
	ExportObject1ACOUSTIC.ExportIntegrationByCellsAll(BaseFileName1 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "MinuteBins " & DepthBin1 & "mDepthBins_from_" & RefLine1NAME & ".sv.csv")
	ExportObject2ACOUSTIC.ExportIntegrationByCellsAll(BaseFileName2 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "MinuteBins " & DepthBin1 & "mDepthBins_from_" & RefLine2NAME & ".sv.csv")
	'ExportObject3ACOUSTIC.ExportIntegrationByCellsAll(BaseFileName3 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "TargetStrengthByCells " & DepthBin1 & RefLine1NAME & ".sv.csv")
    'ExportObject4ACOUSTIC.ExportIntegrationByCellsAll(BaseFileName4 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "TargetStrengthByCells " & DepthBin1 & RefLine2NAME & ".sv.csv")
	ExportObject3ACOUSTIC.ExportData(BaseFileName3 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "TargetStrengthAll" & DepthBin1 & RefLine1NAME & ".sv.csv")
    ExportObject4ACOUSTIC.ExportData(BaseFileName4 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "TargetStrengthAll" & DepthBin1 & RefLine2NAME & ".sv.csv")
    'EXPORT THE JPG OF THE ANALYSIS ECHOGRAM...
	'ExportObject1ACOUSTIC.ExportEchogramToImage (BaseFileName1 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "MinuteBins " & DepthBin1 & "mDepthBins_from_" & RefLine1NAME & "_ANALYSIS.sv.JPG"), 500, -1, -1
	
'EXPORT THE ANALYSIS SETTINGS...
'Here we export a text file that holds the EV file settings - including the documentation so that you'll know the colorbar settings
'associated with the exported echogram jpgs
	'EVPingSettings1.ExportSettings (BaseFileName1 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "MinuteBins " & DepthBin1 & "mDepthBins_from_" & RefLine1NAME & "_ANALYSIS SETTINGS.txt")
	'EVPingSettings2.ExportSettings (BaseFileName2 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "MinuteBins " & DepthBin1 & "mDepthBins_from_" & RefLine2NAME & "_ANALYSIS SETTINGS.txt")
	
	
'EXPORT THE  ** raw data echogram **  as JPG   ExportObject2

'Here the result is to export the echogram showing the grid definition corresponding to the gridded data export
'REMINDER: The data shown in this echogram is intended to be ALL data. Visibility is dependent on the colorbard settings set in the User-Defined section above.data (set 10 dB lower than "analysis")

	
'ASSIGN REFLINE1 AS THE DEPTH-RANGE-REFERENCE-LINE
    ExportObject1.Properties.Grid.DepthRangeReferenceLine = RefLine1
	ExportObject2.Properties.Grid.DepthRangeReferenceLine = RefLine1
'DEFINE THE PARAMETERS OF HTE EXPORT GRID... (match the export grid used in the "analysis" section above)
	ExportObject1.Properties.Grid.SetDepthRangeGrid   0,  DepthBin1 '<--1 m depth grid relative to surface line based on our definition here
	ExportObject1.Properties.Grid.SetTimeDistanceGrid 1, TimeBin
	
	ExportObject2.Properties.Grid.SetDepthRangeGrid   0,  DepthBin1 '<--1 m depth grid relative to surface line based on our definition here
	ExportObject2.Properties.Grid.SetTimeDistanceGrid 1, TimeBin '<-- X minute grid using time in minutes
    '                                                             Note: maximum time allowed = 9999 minutes

'EXPORT THE JPG OF THE ECHOGRAM...
    'ExportObject1.ExportEchogramToImage (OutputExportPath & File.Name & "RAW.JPG"), 500, -1, -1	
	'ExportObject1ACOUSTIC.ExportEchogramToImage (BaseFileName1 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "MinuteBins " & DepthBin1 & "mDepthBins_from" & RefLine1NAME & "_RAW.sv.JPG"), 500, -1, -1

	'ExportObject2.ExportEchogramToImage (OutputExportPath & File.Name & "RAW.JPG"), 500, -1, -1	
	'ExportObject2ACOUSTIC.ExportEchogramToImage (BaseFileName2 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "MinuteBins " & DepthBin1 & "mDepthBins_from" & RefLine2NAME & "_RAW.sv.JPG"), 500, -1, -1


'EXPORT THE RAW SETTINGS...
	'EVPingSettings2.ExportSettings (BaseFileName1 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "MinuteBins " & DepthBin1 & "mDepthBins_from" & RefLine1NAME & "_RAW SETTINGS.txt")




'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''



'SET GRID SIZE FOR THIS SET OF EXPORTS: 1 M DEPTH BINS (DepthBin1) MEASURED FROM THE NEARFIELD (BOTTOM), FOR 10 MINUTE BINS (TimeBin)...



   
'ASSIGN REFLINE2 as the DEPTH-RANGE-REFERENCE-LINE...
	'ExportObject1.Properties.Grid.DepthRangeReferenceLine = RefLine2
    'ExportObject2.Properties.Grid.DepthRangeReferenceLine = RefLine2
'DEFINE THE PARAMETERS OF THE EXPORT GRID...
	'ExportObject1.Properties.Grid.SetDepthRangeGrid   2,  DepthBin1
    'ExportObject2.Properties.Grid.SetDepthRangeGrid   2,  DepthBin1 '<--X m depth grid relative to the RefLine line
	'ExportObject1.Properties.Grid.SetTimeDistanceGrid 1, TimeBin
    'ExportObject2.Properties.Grid.SetTimeDistanceGrid 1, TimeBin '<--X minute grid using time in minutes
	'                                                            Note: maximum time allowed = 9999 minutes

		

'EXPORT THE GRIDDED DATA
'Here the filename has been customized to match the grid size; Edit this to match the grid you use
	'ExportObject1ACOUSTIC.ExportIntegrationByCellsAll(BaseFileName1 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "MinuteBins " & DepthBin1 & "mDepthBinsFromBottom-" & RefLine2NAME & ".sv.csv")
	'ExportObject2ACOUSTIC.ExportIntegrationByCellsAll(BaseFileName1 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "MinuteBins " & DepthBin1 & "mDepthBinsFromBottom-" & RefLine2NAME & ".sv.csv")
	'ExportObject3ACOUSTIC.ExportIntegrationByCellsAll(BaseFileName1 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "MinuteBins " & DepthBin1 & "mDepthBinsFromBottom-" & RefLine2NAME & ".sv.csv")
'EXPORT the ASSOCIATED "ANALYSIS" ECHOGRAM as JPG
'Here the result is to export the echogram showing the grid definition corresponding to the gridded data export
'REMINDER: The only data shown on the "data without turbulence" variable are those that meet the analysis criteria
'(i.e. as defined by thresholds set in the EV file such as: > -66 dB for Sv as set in the User-Defined Variables section above)   

'EXPORT THE JPG OF THE ECHOGRAM... 
	'ExportObject1ACOUSTIC.ExportEchogramToImage (BaseFileName1 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "MinuteBins " & DepthBin1 & "mDepthBinsFromBottom-" & RefLine2NAME & " ANALYSIS.sv.JPG"), 500, -1, -1
	
'EXPORT the ANALYSIS SETTINGS...
'Here we export a text file that holds the EV file settings - including the documentation so that you'll know the colorbar settings
'associated with the exported echogram jpgs
	'EVPingSettings1.ExportSettings (BaseFileName1 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "MinuteBins " & DepthBin1 & "mDepthBinsFromBottom-" & RefLine2NAME & " ANALYSIS SETTINGS.txt")
	
	
	
'EXPORT the "RAW" ECHOGRAM as JPG

'Here the result is to export the echogram showing the grid definition corresponding to the gridded data export
'The data shown is intended to be "all" data (i.e. lower colorbar minimum than "analysis")
'FIRST DEFINE THE EXPORT VARIABLE...
'Done above.
'SECOND DEFINE THE REFERENCE LINE AND THE GRID... 	
'ASSIGN RefLine2 as the DepthRangeReferenceLine...
	'ExportObject2.Properties.Grid.DepthRangeReferenceLine = RefLine2
'DEFINE the parameters of the export grid (matching the analysis grid for ExportObject1 above)
	'ExportObject2.Properties.Grid.SetDepthRangeGrid   2,  DepthBin1  '<--X m depth grid relative to nearfield line based on our definition here
	'ExportObject2.Properties.Grid.SetTimeDistanceGrid 1, TimeBin  '<--X minute grid using time in minutes
	'                                                            Note: maximum time allowed = 9999 minutes

'EXPORT THE JPG OF THE GRIDDED "RAW" ECHOGRAM...	
	'ExportObject2ACOUSTIC.ExportEchogramToImage (BaseFileName2 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "MinuteBins " & DepthBin1 & "mDepthBinsFromBottom-" & RefLine2NAME & " RAW.sv.JPG"), 500, -1, -1


'EXPORT the RAW SETTINGS...
	'EVPingSettings2.ExportSettings (BaseFileName1 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "MinuteBins " & DepthBin1 & "mDepthBinsFromBottom-" & RefLine2NAME & " RAW SETTINGS.txt")

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''	
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''



'SET GRID SIZE FOR THIS SET OF EXPORTS:  FULL WATER COLUMN (DepthBin2) in  10-MINUTE BINS (TimeBin)... 


'EXPORT VariablesToExport1 as data, echogram (whole water column in 10-minute time bins)

'ASSIGN REFLINE1 as the DEPTH-RANGE-REFERENCE-LINE...
    ExportObject1.Properties.Grid.DepthRangeReferenceLine = RefLine1
    ExportObject2.Properties.Grid.DepthRangeReferenceLine = RefLine2
    ExportObject3.Properties.Grid.DepthRangeReferenceLine = RefLine1
    ExportObject4.Properties.Grid.DepthRangeReferenceLine = RefLine2
'DEFINE THE PARAMETERS OF THE EXPORT GRID...
'See definitions above for "SetDepthRangeGrid" and "SetTimeDistanceGrid" options
	ExportObject1.Properties.Grid.SetDepthRangeGrid   2, DepthBin2 '<--200 m grid relative to RefLine (entire water column)
	ExportObject1.Properties.Grid.SetTimeDistanceGrid 1,  TimeBin '<--X minute grid using time in minutes
    ExportObject2.Properties.Grid.SetDepthRangeGrid   2, DepthBin2 '<--200 m grid relative to RefLine (entire water column)
	ExportObject2.Properties.Grid.SetTimeDistanceGrid 1,  TimeBin '                                                             Note: maximum time allowed = 9999 minutes	
	ExportObject3.Properties.Grid.SetDepthRangeGrid   2, DepthBin2 '<--200 m grid relative to RefLine (entire water column)
	ExportObject3.Properties.Grid.SetTimeDistanceGrid 1,  TimeBin
    ExportObject4.Properties.Grid.SetDepthRangeGrid   2, DepthBin2 '<--200 m grid relative to RefLine (entire water column)
	ExportObject4.Properties.Grid.SetTimeDistanceGrid 1,  TimeBin
'EXPORT THE GRIDDED DATA
'Here the filename has been customized to match the grid size; Edit this to match the grid you use
	ExportObject1ACOUSTIC.ExportIntegrationByCellsAll(BaseFileName1 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "MinuteBins FullWaterColumn_" & DepthBin2 & "m.sv.csv")
	ExportObject2ACOUSTIC.ExportIntegrationByCellsAll(BaseFileName2 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "MinuteBins FullWaterColumn_" & DepthBin2 & "m.sv.csv")
	ExportObject3ACOUSTIC.ExportSingleTargetsByCellsAll(BaseFileName3 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "TargetStrength_" & DepthBin2 & "m.sv.csv")
    ExportObject4ACOUSTIC.ExportSingleTargetsByCellsAll(BaseFileName4 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "TargetStrength_" & DepthBin2 & "m.sv.csv")
'EXPORT THE JPG OF THE ECHOGRAM...
	'ExportObject1ACOUSTIC.ExportEchogramToImage (BaseFileName1 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "MinuteBins FullWaterColumn_" & DepthBin2 & "m ANALYSIS.sv.JPG"), 500, -1, -1
	
'EXPORT the ANALYSIS SETTINGS...
'Here we export a text file that holds the EV file settings - including the documentatiaon so taht you'll know the colorbar settings
'associated with the exported echogram jpgs
'REMINDER EVPingSettings1 was declared and set above	
	'EVPingSettings1.ExportSettings (BaseFileName1 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "MinuteBins FullWaterColumn_" & DepthBin2 & "m ANALYSIS SETTINGS.txt")



'EXPORT the "RAW" ECHOGRAM as JPG

'Here the result is to export the echogram showing the grid definition corresponding to the gridded data export
'The data shown is intended to be "all" data (i.e. lower colorbar minimum than "analysis")
'FIRST DEFINE THE EXPORT VARIABLE...
'Done above

'SECOND DEFINE THE REFERENCE LINE AND THE GRID...
'ASSIGN RefLine1 as the DepthRangeReferenceLine
    'ExportObject2.Properties.Grid.DepthRangeReferenceLine = RefLine2

'DEFINE the parameters of the export grid (matching the analysis grid for ExportObject1 above)
	'ExportObject2.Properties.Grid.SetDepthRangeGrid   2, DepthBin2 '<--100 m grid relative to Surface plus 1m (entire water column)
	'ExportObject2.Properties.Grid.SetTimeDistanceGrid 1,  TimeBin '<-- X minute grid using time in minutes

'EXPORT THE JPG OF THE ECHOGRAM...
	'ExportObject2.ExportEchogramToImage (OutputExportPath & File.Name & ExportObject2 & "RAW.JPG"), 500, -1, -1	
	'ExportObject2ACOUSTIC.ExportEchogramToImage (BaseFileName1 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "MinuteBins FullWaterColumn_" & DepthBin2 & "m RAW.sv.JPG"), 500, -1, -1

'EXPORT the RAW SETTINGS...
	'EVPingSettings2.ExportSettings (BaseFileName1 & "_" & TimeIndicatorForFileName & "_" & TimeBin & "MinuteBins FullWaterColumn_" & DepthBin2 & "m RAW SETTINGS.txt")



'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''



'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'EXPORT lines
		
'EXPORT line1 (Top Line per LineToExport1 as defined above)
	 Dim EvLine1: Set EvLine1 = EvFile.Lines.FindByName(LineToExport1)
     'Dim EvLineEdit1: Set EvLineEdit1 = EvLine1.AsLineBase
	 'ExportObject1ACOUSTIC.ExportLine EvLine1,BaseFileName1 & "_" & TimeIndicatorForFileName & "_" & LineToExport1 & ".line.csv",-1,-1  
     EvLine1.Export BaseFileName1 & "_" & TimeIndicatorForFileName & "_" & LineToExport1 & ".line.csv"
	 '		-1 = start/end of line, or put specific ping range
	 
'EXPORT line2 (Turbulence Line per LineToExport2 as defined above)
	 Dim EvLine2: Set EvLine2 = EvFile.Lines.FindByName(LineToExport2)
     'Dim EvLineEdit2: Set EvLineEdit2 = EvLine2.AsLineBase
	 'ExportObject1ACOUSTIC.ExportLine EvLine2,BaseFileName1 & "_" & TimeIndicatorForFileName & "_" & LineToExport2 & ".line.csv",-1,-1 
     EvLine2.Export BaseFileName1 & "_" & TimeIndicatorForFileName & "_" & LineToExport2 & ".line.csv" 
	 '		-1 = start/end of line, or put specific ping range
	 
'EXPORT line3 (Bottom Line per LineToExport3 as defined above)
	 Dim EvLine3: Set EvLine3 = EvFile.Lines.FindByName(LineToExport3)
     'Dim EvLineEdit3: Set EvLineEdit3 = EvLine3.AsLineBase
	 'ExportObject1ACOUSTIC.ExportLine EvLine3,BaseFileName1 & "_" & TimeIndicatorForFileName & "_" & LineToExport3 & ".line.csv",-1,-1  
     EvLine3.Export BaseFileName1 & "_" & TimeIndicatorForFileName & "_" & LineToExport3 & ".line.csv"
	 '		-1 = start/end of line, or put specific ping range


'EXPORT line4 (Top Line per LineToExport1 as defined above)
	 Dim EvLine4: Set EvLine4 = EvFile.Lines.FindByName(LineToExport4)
     'Dim EvLineEdit4: Set EvLineEdit4 = EvLine4.AsLineBase
	 'ExportObject2ACOUSTIC.ExportLine EvLine4,BaseFileName2 & "_" & TimeIndicatorForFileName & "_" & LineToExport4 & ".line.csv",-1,-1  
     EvLine4.Export BaseFileName2 & "_" & TimeIndicatorForFileName & "_" & LineToExport4 & ".line.csv"
	 '		-1 = start/end of line, or put specific ping range
	 
'EXPORT line5 (Turbulence Line per LineToExport2 as defined above)
	 Dim EvLine5: Set EvLine5 = EvFile.Lines.FindByName(LineToExport5)
     'Dim EvLineEdit5: Set EvLineEdit5 = EvLine5.AsLineBase
	 'ExportObject2ACOUSTIC.ExportLine EvLine5,BaseFileName2 & "_" & TimeIndicatorForFileName & "_" & LineToExport5 & ".line.csv",-1,-1  
     EvLine5.Export BaseFileName2 & "_" & TimeIndicatorForFileName & "_" & LineToExport5 & ".line.csv"
	 '		-1 = start/end of line, or put specific ping range
	 
'EXPORT line6 (Bottom Line per LineToExport3 as defined above)
	 Dim EvLine6: Set EvLine6 = EvFile.Lines.FindByName(LineToExport6)
     'Dim EvLineEdit6: Set EvLineEdit6 = EvLine6.AsLineBase
	 'ExportObject2ACOUSTIC.ExportLine EvLine6,BaseFileName2 & "_" & TimeIndicatorForFileName & "_" & LineToExport6 & ".line.csv",-1,-1  
     EvLine6.Export BaseFileName2 & "_" & TimeIndicatorForFileName & "_" & LineToExport6 & ".line.csv"
	 '		-1 = start/end of line, or put specific ping range



'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'EXPORT theVariableToExportstn1_BNR as .EVD file for archiving

	'Dim EVDvariableCollection
	'Set EVDvariableCollection=EvFile.Variables
	Dim EVDvariable
	'Set EVDvariable=EVDvariableCollection.FindByName(VariableToExport1)
	Set EVDvariable=EvFile.Variables.FindByName(VariableToExportstn1_BNR)

'SAVE the EVD file and close it
	EVDvariable.ExportEVD(OutputExportPath & TimeIndicatorForFileName & "_" & File.Name & "D")


''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'RESET the EV file to "no grid", export echogram jpgs, and save/close the file


'RESET the grid definition to "no grid" 
'See definitions above for "SetDepthRangeGrid" and "SetTimeDistanceGrid" options
	ExportObject1.Properties.Grid.SetDepthRangeGrid   0, 0 	'<--no grid in depth/range dimension
	ExportObject1.Properties.Grid.SetTimeDistanceGrid 0, 0 	'<--no grid in the time/distance dimension
	ExportObject2.Properties.Grid.SetDepthRangeGrid   0, 0 	'<--no grid in depth/range dimension
	ExportObject2.Properties.Grid.SetTimeDistanceGrid 0, 0 	'<--no grid in the time/distance dimension
	ExportObject3.Properties.Grid.SetDepthRangeGrid   0, 0 	'<--no grid in depth/range dimension
	ExportObject3.Properties.Grid.SetTimeDistanceGrid 0, 0 
    ExportObject4.Properties.Grid.SetDepthRangeGrid   0, 0 	'<--no grid in depth/range dimension
	ExportObject4.Properties.Grid.SetTimeDistanceGrid 0, 0 
'EXPORT the echograms as JPG
	'ExportObject1ACOUSTIC.ExportEchogramToImage (BaseFileName & "_NoGrid ANALYSIS.sv.JPG"), 500, -1, -1
	'ExportObject2ACOUSTIC.ExportEchogramToImage (BaseFileName & "_NoGrid RAW.sv.JPG"), 500, -1, -1
		
'SAVE the EV file
	 EvFile.SaveAs(OutputEvFilePath & TimeIndicatorForFileName & "_" & File.Name) 'save EV file in the filepath specified above

'CLOSE the EV file.
'(If there are more .EV files in the folder, the subroutine will repeat the cycle through each of the EV files.)
EvApp.CloseFile EvFile

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	
End Sub

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


' The actual body of the script:

Dim File
Dim EvFile
Set EvFile = Nothing
For Each File In EvFileFolder.Files

	' Look for EV files in the EvFileFolder path
	If StrComp(FSO.GetExtensionName(File.Path), "EV", vbTextCompare) = 0 Then
	
		' If not already open, open an EV file
		Set EvFile = EvApp.OpenFile(File.Path)
				
		If EvFile Is Nothing Then
		   MsgBox "Failed to open EV file"
		   WScript.Quit
		Else
        	SaveAndCloseFile ' Do all the fancy stuff you want using this subfunction (defined above)
        	Set EvFile = Nothing
        End If
     
     End If
     
Next

MsgBox "Script exiting successfully.", vbOk + vbInformation