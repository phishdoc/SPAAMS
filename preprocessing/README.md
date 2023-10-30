# SPAAMS
Scripts for preprocessing active acoustic data as described in:

>Horne JK, Swan JA, Tracy II TJ, Holtgrieve GW  
*Automated Acoustic Monitoring of Fish and Flow for Near-Real-Time Resource Management*

Data was collected from Solar Powered Autonomous Acoustic Monitoring Stations (SPAAMS) deployed along the Tonle Sap River in Cambodia. For a self-contained pipeline, users can store raw files in a subdirectory within data.

## Using Preprocessing Scripts
NOTE: Change ``PATH/TO/`` substrings in ``raw2ev.R`` and ``ev2exports.vbs`` to absolute paths once SPAAMS is downloaded.

0. Place raw (.raw) files in corresponding ``data/raw/(stn1 OR stn2)/``, calibration (.ecs) files in ``data/raw/cal/``, and Echoview template (.EV) file in ``data/ev/templates/``.
1. Source ``raw2ev.R`` in RStudio to create Echoview files from the RAW files.
2. Run (double-click) ``ev2exports.vbs`` to generate Echoview exports and pull relevant data from Echoview files.
3. Follow steps in ``SPAAMS/analysis`` to run the SPAAMS pipeline.

## Dependencies
* Windows OS (for .vbs COM scripts)
* Echoview
* R (4.1.0)
    * devtools
    * dplyr
    * gtools
    * hms
    * lubridate
    * stringr

NOTE: ``maptools`` and ``rgeos`` (dependencies of EchoviewR) were removed from CRAN as of 2023-10-16, so download archived tarballs and install in R terminal. RTools may also need to be downloaded for source installation.

Links for source downloads here: [maptools](https://cran.r-project.org/src/contrib/Archive/maptools/) - [rgeos](https://cran.r-project.org/src/contrib/Archive/rgeos/) - [RTools](https://cran.r-project.org/bin/windows/Rtools/rtools43/rtools.html)

## License
This software is licensed under the open source [MIT license](../LICENSE).