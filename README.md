# dynamic-dose-increase-combined-data

To use the code in this repository, create another folder/directory at the same level called `Data`, with the `druguse_06062021.xlsx` and `baselinectn94.csv` files in it. Also create an empty folder inside of `Data` called `Plots`. This will ensure that all the scripts run properly, and that your raw data file & subsequent data outputs don't get uploaded to GitHub.

As you run the R scripts, your `Data` folder will get populated with new tables, in `.RData` format.

You will need to have installed the following R packages:

`install.packages(c("tidyverse", "stringr", "lubridate", "mice", "mitools", "ltmle", "xtable", "SuperLearner", "arm", "gam", "earth", "xgboost", "xtable"))`

*and maybe later: ("survival", "table1"), but not yet*

*Note:* Files 0-3 include setup code for using either the provided relapse outcome definitions only, or using our own definition ("Alternative"), a hybrid taking into account the newer definition that counts a relapse at the end of the designated period of non-study opioid use, rather than the beginning. This alternative definition is used in analyses moving forward (files 4 & 6), but you can edit those to use the original definitions if needed.

## Steps:

If you just want to use data that I've already cleaned and prepared, but play around with the analysis get the `?????????` file from me (ex. via encrypted email), and do the following:

* Open up `06-ltmle-analysis.R` and run the code all at once or section-by-section to create LTMLE estimates, plots, and tables.


If you want to re-create the entire dataset and analysis from scratch, be warned that the multiple imputation step might take a long time, or try and fail repeatedly. Here are the steps to do that:

1. Run `05-one-step-data-prep.R` *(no need to run scripts 00 through 04, they won't necessarily work on their own)*
2. Take a look in your data folder, or open up R and run `load("clean_combined_imputed_data.RData")` and `load("data_ready_for_ltmle.RData")` to explore. Consider starting with the codebook.
3. Run `06-ltmle-analysis.R` to produce plots/tables. Look in your folder `Data/Plots` to find them.
