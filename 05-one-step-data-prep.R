#one step data prep: you only need to run this one R script to do all data prep and create datasets before analysis

#---------------- Source all of the helper files to run various data prep steps -----------------#

source("00-codebook.R")
source("01-initial-data-cleaning.R")
source("02-defining-relapse-outcomes.R")
source("03-multiple-imputation.R")
source("04-ltmle-prep.R")


#---------------- Save all data we might want to use later as an `RData` file -----------------#

#TODO later: sort through what actually needs to be saved, get rid of the rest.
#  write explanations of what's being output here.

# TODO: put demog etc. into initial save??

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
  file = "../Data/clean_combined_imputed_data.RData"
)

save(
  weekly_data_for_ltmle_04,
  file = "../Data/data_ready_for_ltmle.RData"
)

# save(
#   results,
#   file = "../Data/ltmle_results.RData"
# )