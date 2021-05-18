# dynamic-dose-increase-combined-data

To use the code in this repository, create another folder/directory at the same level called `Data`, with the `druguseupctn94.csv` file in it.

As you run the R scripts, your `Data` folder will get populated with new tables, in both `.csv` and `.RData` formats.
 -->*Note: don't have CSV yet!*
 
You will need to have installed the following R packages:
`install.packages(c("tidyverse", "stringr", "lubridate", "mice", "mitools", "survival", "table1", "ltmle"))`

Steps:

1. Run `05-one-step-data-prep.R`
  - *(no need to run scripts 00 through 04, they won't necessarily work on their own.)*