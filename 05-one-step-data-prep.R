#one step data prep: run this R script to prepare for doing ltmle analyses

#---------------- Source all of the helper files to run various data prep steps -----------------#

source("00-codebook.R")
source("01-initial-data-cleaning.R")
source("02-defining-relapse-outcomes.R")
source("03-multiple-imputation.R")
source("04-ltmle-prep.R")


#---------------- Save all data we might want to use later as an `RData` file -----------------#


save(
  codebook,
  initial_data_cleaning_no_outcomes_01,
  patients_with_outcomes_02, visits_with_outcomes_02,
  weeks_with_outcomes_02,
  patients_imputed_03,
  ALT_patients_with_outcomes_02, ALT_visits_with_outcomes_02,
  ALT_weeks_with_outcomes_02,
  ALT_patients_imputed_03,
  demog, comorbidities, treatment_info, outcomes, visit_data, weekly_indicators,
  file = "../Data/clean_combined_imputed_data9-8-21.RData"
)

save(
  ALT_patients_imputed_03,
  demog, comorbidities, treatment_info, outcomes, visit_data, weekly_indicators,
  #ALT_weekly_data_for_ltmle_04,
  ALT_weekly_data_for_ltmle_04_NEW,
  #cases,
  cases_NEW,
  file = "../Data/data_ready_for_ltmle9-20-21.RData"
)