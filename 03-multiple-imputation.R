#multiple imputation

library(mice)
library(mitools)

#Note: `patients` is a data frame including all individual-level data, with one entry per patient

#set random seed for reproduceability
set.seed(123)

#how many missing values are there? --> no columns are missing for more than 8% of patients
# map(select(patients, all_of(demog), all_of(comorbidities)), ~mean(is.na(.)))

#---------------- Get set up for imputation -----------------#

setup = mice(patients, maxit = 0) 
my_methods = setup$method
my_predMatrix = setup$predictorMatrix

#alter the prediction matrix, to exclude some variables from being predictors during imputation
my_predMatrix[, c("who")] = 0
my_predMatrix[, c("project")] = 0 #exclude b/c completely determined by site
my_predMatrix[, c("end_of_detox")] = 0 #exclude b/c completely determined by rand_dt

#alter the methods matrix, to exclude any variables we don't want to impute
my_methods[all_of(outcomes)] = ""
my_methods[all_of(treatment_info)] = ""

#---------------- Impute missing data -----------------#

#imputed = mice(select(patients, all_of(demog), ivdrug), method = my_methods, predictorMatrix = my_predMatrix, m = 5)
imputed = mice(patients, method = my_methods, predictorMatrix = my_predMatrix, m = 10)

# LOOK UP:
#mi tools: MI combine function

patient_data_after_imputation_03 = complete(imputed)

#---------------- X -----------------#
#---------------- X -----------------#