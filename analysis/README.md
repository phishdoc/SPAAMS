# SPAAMS/analysis
Scripts for to run the SPAAMS data processing pipeline described in:

>Horne JK, Swan JA, Tracy II TJ, Holtgrieve GW  
*Automated Acoustic Monitoring of Fish and Flow for Near-Real-Time Resource Management*

Data was collected from Solar Powered Autonomous Acoustic Monitoring Stations (SPAAMS) deployed along the Tonle Sap River in Cambodia. For a self-contained pipeline, users can store raw files in a subdirectory within data.

## Using the SPAAMS Pipeline
NOTE: Change ``PATH/TO/`` substrings in ``preprocessing/raw2ev.R``, ``preprocessing/ev2exports.vbs``, and ``RUNME_pipeline.R`` to absolute paths once SPAAMS is downloaded.

0. Place raw (.raw) files in corresponding ``data/raw/(stn1 OR stn2)/``, calibration (.ecs) files in ``data/raw/cal/``, and Echoview template (.EV) file in ``data/ev/templates/``.
1. Source ``preprocessing/raw2ev.R`` in RStudio to create Echoview files from the RAW files.
2. Run (double-click) ``preprocessing/ev2exports.vbs`` to generate Echoview exports and pull relevant data from Echoview files.
3. Run ``RUNME_pipeline.R`` which takes the data products from ``preprocessing`` and performs a standard suite of acoustic analyses.  
``RUNME_pipeline.R`` drives the data pipeline (i.e, automatically calls subsequent steps):  
``TonleSap_OGIVE_CDF_PDF.R`` --> ``TonleSap_Bottom_Line_Compilation.R`` --> ``TonleSap_Target_Strength.R``
4. (optional) Run ``TonleSap_Flux_Calc.R`` for flux calculations from Horne et al. (*in progress*).

## Dependencies
* Windows OS (for .vbs COM scripts)
* Echoview
* R (4.1.0)
    * AICcmodavg
    * broom
    * data.table
    * ggpubr
    * ggplot2
    * gtools
    * lubridate
    * plyr
    * qpcR
    * tidyverse

## License
This software is licensed under the open source [MIT license](../LICENSE).