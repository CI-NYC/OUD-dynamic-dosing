# dynamic-dose-increase-combined-data

To use the code in this repository, create another folder/directory at the same level called `Data`, with the `druguse_06062021.xlsx` and `baselinectn94.csv` files in it. Also create an empty folder inside of `Data` called `Plots`. This will ensure that all the scripts run properly, and that your raw data file & subsequent data outputs don't get uploaded to GitHub.

As you run the R scripts, your `Data` folder will get populated with new tables, in `.RData` format.

You will need to have installed the following R packages:

`install.packages(c("tidyverse", "stringr", "lubridate", "mice", "mitools", "ltmle", "xtable", "SuperLearner", "arm", "gam", "earth", "xgboost"))`

Files 0-3 include setup code for using either the provided relapse outcome definitions only, or using our own definition ("Alternative"), a hybrid taking into account the newer definition that counts a relapse at the end of the designated period of non-study opioid use, rather than the beginning. This alternative definition is used in analyses moving forward (files 4 & 6), but you can edit those to use the original definitions if needed.

File 4 sets up "cases" and all the data for LTMLE (right formatting, creating treatment rules, lists of Anodes, etc.)

File 6 includes functions for actually running LTMLE, combining imputed estimates, and creating tables for display.
