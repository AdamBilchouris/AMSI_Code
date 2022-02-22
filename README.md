# AMSI_Code
This repository contains code to recreate the models and results for the AMSI VRS report.

## CSV Files
* 1k_sumSuburbs.csv is not used.

* big_removed.csv contains information about houses.

* hist.csv contains median sales for the selected suburbs.
    * File can be found [here](https://github.com/AdamBilchouris/Domain-Scraping/blob/master/outMedian/big.csv).

* suburbs_data.csv contains census data and general suburb information.
    * This file was created by [Michal Sniatala](https://github.com/michalsn) and can be found [here](https://github.com/michalsn/australian-suburbs).

* sumSuburbs_columns.csv contains crime data, although it is not used, but still required to run the script.
    * This was obtained [here](https://www.crimestatistics.vic.gov.au/crime-statistics/latest-victorian-crime-data/download-data) (Data Tables LGA Criminal Incidents Year Ending June 2021 (XLSX, 17.33 MB)).

* 

## Files
* LoopedClean.R
    * This file contains code to produce the four models.
    * This file should be used as it contains everything needed.
    * To test the adjusted and non-adjusted prices, change the train/test sets at lines 189-192, 713-714, 1082-1085.
    * When testing against anomalies, use the same forest (don't create a new one). It only needs to be run once.
        * Once the forest is made, choose what dataframe you want (lines 713-714). This applies to testing the anomalies from line 1590 onwards.

* Plots.R
    * This file creates various plots.

* LoopedOrig.R
    * This file is the unclean version of Looped.R.

* Looped.R
    * This file is broken, do not use it.

* Cleaner.R
    * This file contains code to produce the three models.
    * This file also has some irrelevant code at the end.

* Cleaned.R
    * This file contains code to produce the above, as well as an isolation forest for anomaly detection. It also tests these.

* Test.R
    * This file was used to play around with some ideas.

## Required Libraries
```{R}
install.packages('dplyr')
install.packages('GA')
install.packages('isotree')
install.packages('knitr')
install.packages('stringr')
```
