# dynamic-dose-increase-combined-data

To use the code in this repository, create another folder/directory at the same level called `Data`, with the `druguseupctn94.csv` file in it.

As you run different R scripts, your `Data` folder will get populated with new tables, in both `.csv` and `.RData` formats.

You will need to have installed the following R packages: `install.packages(c("tidyverse", "stringr", "lubridate", "mice", "mitools", "survival", "table1", "ltmle"))`

Steps:

1. Run `01-initial-data-cleaning.R`
2. ?