#one step data prep: only need to run this one R script to do all data prep and create datasets

#---------------- X -----------------#
#---------------- X -----------------#
#---------------- X -----------------#

source("00-codebook.R")
source("01-initial-data-cleaning.R")
#source("02-defining-relapse-outcomes.R")
#source("03-multiple-imputation.R")
#source("04-ltmle-prep.R")

# 
# 
# save(codebook,
#      initial_data_cleaning_no_outcomes_01, patient_data_after_imputation_03
#      
#      clean_combined_data,
#      patients,
#      weeks,
#      demog, comorbidities, treatment_info, outcomes, visit_data, weekly_indicators,
#      file = "clean_combined_data.RData")