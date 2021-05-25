# dynamic-dose-increase-combined-data

To use the code in this repository, create another folder/directory at the same level called `Data`, with the `druguseupctn94.csv` file in it. This will ensure that all the scripts run properly, and that your raw data file & subsequent data outputs don't get uploaded to GitHub.

As you run the R scripts, your `Data` folder will get populated with new tables, in `.RData` format.

You will need to have installed the following R packages:

`install.packages(c("tidyverse", "stringr", "lubridate", "mice", "mitools", "ltmle"))`

*and maybe later: ("survival", "table1"), but not yet*


## Steps:

1. Run `05-one-step-data-prep.R`
  - *(no need to run scripts 00 through 04, they won't necessarily work on their own)*
2. Take a look in your data folder, or open up R and run `load("clean_combined_data.RData")` to explore. Consider starting with the codebook.
3. Run `06-ltmle-analysis.R` to produce plots. Look in your folder `Data/Plots` to find them.


## TODO later:

1. Create alternate definitions for relapse date
2. Get some descriptive summaries of the data, such as a Table 1, or anything else to help contextualize the results
3. Look into doseage changes within weeks vs. across weeks
4. Provide output datasets as CSVs in addition to RData
5. Double check that the codebook has the right values for factors (ex. 0 vs. no). Fill in missing codebook info.