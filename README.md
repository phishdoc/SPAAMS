# SPAAMS
Scripts for active acoustic data processing and graphic products documented in

Horne JK, Swan JA, Tracy II TJ, Holtgrieve GW  
*Automated Acoustic Monitoring of Fish and Flow for Near-Real-Time Resource Management*

Data was collected from Solar Powered Autonomous Acoustic Monitoring Stations (SPAAMS) deployed along the Tonle Sap River in Cambodia. For a self-contained pipeline, users can store raw files in a subdirectory within data.

**analysis** stores R files that drive the SPAAMS pipeline.

**figures** stores figures created by SPAAMS pipeline or paper scripts.

**paper_scripts** stores code used to create figures for Horne et al. (*in progress*)

**preprocessing** stores R and VBS files used for processing .raw files into Echoview exports used in the pipeline.

## Using the SPAAMS Pipeline
NOTE: Check each file to modify input and output filepaths

0. .raw files are collected from SPAAMS devices.
1. Run ``preprocessing/raw2ev.R`` to create Echoview files from the RAW files.
2. Run ``preprocessing/ev2exports.vbs`` to generate Echoview exports and pull relevant data from Echoview files.
3. Run ``analysis/RUNME_pipeline.R`` which takes the data products from ``preprocessing`` and performs a standard suite of acoustic analyses.
``RUNME_pipeline.R`` drives the data pipeline (i.e, automatically calls subsequent steps):  
``RUNME_pipeline.R`` --> ``TonleSap_OGIVE_CDF_PDF.R`` --> ``TonleSap_Bottom_Line_Compilation.R`` --> ``TonleSap_Target_Strength.R``
4. (optional) Run ``analysis/TonleSap_Flux_Calc.R`` for flux calculations and figures documented in the Horne et al. (*in progress*)

## Dependencies
* Windows OS (for .vbs COM scripts)
* Echoview
* R (4.1.0)
    * AICcmodavg
    * broom
    * data.table
    * devtools
    * dplyr
    * ggpubr
    * ggplot2
    * gtools
    * lubridate
    * plyr
    * qpcR
    * stringr
    * tidyverse

NOTE: ``maptools`` and ``rgeos`` (dependencies of EchoviewR) were removed from CRAN as of 2023-10-16, so download archived tarballs and install in R terminal. RTools may also need to be downloaded.

Links for source downloads here: [maptools](https://cran.r-project.org/src/contrib/Archive/maptools/) - [rgeos](https://cran.r-project.org/src/contrib/Archive/rgeos/) - [RTools](https://cran.r-project.org/bin/windows/Rtools/rtools43/rtools.html)

## Directory Structure
```
.
|-- figures
|   `-- .gitignore
|-- analysis
|   |-- TonleSap_OGIVE_CDF_PDF.R
|   |-- TonleSap_Bottom_Line_Compilation.R
|   |-- TonleSap_Target_Strength.R
|   |-- TonleSap_Flux_Calc.R
|   `-- RUNME_pipeline.R
|-- paper_scripts
|   |-- analysis_paper_final_plots_script.R
|   |-- Figure_7_TS_12_Hour_Stacked.R
|   |-- Figure_10_TS_PDF_Script.R
|   |-- flux_CDF_pw_regression_script.R
|   |-- flux_sv_regression.R
|   `-- Gantt_chart_dates.R
|-- preprocessing
|   |-- ev2exports.vbs
|   |-- raw2ev.R
|   `-- README.md
`-- README.md
```

## License
This software is licensed under the open source [MIT license](LICENSE).