#one step data prep: you only need to run this one R script to do all data prep and create datasets before analysis

#---------------- X -----------------#
#---------------- X -----------------#
#---------------- X -----------------#

source("00-codebook.R")
source("01-initial-data-cleaning.R")
source("02-defining-relapse-outcomes.R")
source("03-multiple-imputation.R")
#source("04-ltmle-prep.R")


#MAKE 2 COPIES, one imputed and one not. Maybe leave the un-imputed one commented out
save(
  codebook,
  initial_data_cleaning_no_outcomes_01,
  patients_with_outcomes_02, visits_with_outcomes_02, weeks_with_outcomes_02,
  patients_imputed_03,
  patients, visits, weeks,
  demog, comorbidities, treatment_info, outcomes, visit_data, weekly_indicators,
  file = "clean_combined_data.RData"
)