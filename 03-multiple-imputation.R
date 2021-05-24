#multiple imputation

library(mice)
#library(mitools)

#set random seed for reproduceability
set.seed(123)

#Note: `patients_with_outcomes_02` is a data frame including all individual-level data, with one entry per patient
#We will be imputing values for all of the variables in `comorbidities`.
#  There is no missing data among the `demog` (demographic) variables
#  We will not be imputing outcomes, but using the relapse_date to help our imputations (the other outcomes are redundant)

#how many missing values are there? --> no columns are missing for more than 8% of patients
# map(select(patients_with_outcomes_02, all_of(comorbidities)), ~mean(is.na(.)))

patients_to_impute = patients_with_outcomes_02 %>%
  select(all_of(demog), all_of(treatment_info), all_of(comorbidities), relapse_date)


#---------------- Get set up for imputation -----------------#

setup = mice(patients_to_impute, maxit = 0) 
my_methods = setup$method
my_predMatrix = setup$predictorMatrix

#alter the prediction matrix, to exclude some variables from being predictors during imputation
my_predMatrix[, c("who")] = 0
my_predMatrix[, c("project")] = 0 #exclude b/c completely determined by site
my_predMatrix[, c("end_of_detox")] = 0 #exclude b/c completely determined by rand_dt

#alter the methods matrix, to exclude any variables we don't want to impute
#NOTE: this is just to be safe -- all of the following should have complete information  (no missing)
my_methods[all_of(demog)] = ""
my_methods[all_of(treatment_info)] = ""
my_methods[relapse_date] = ""


#---------------- Impute missing data -----------------#

patients_imputed_03 = mice(patients_to_impute, method = my_methods, predictorMatrix = my_predMatrix, m = 5)

# #NOTE: this is how to access the different versions of the imputed data later
# p1 = complete(patients_imputed_03, action = 1)
# p2 = complete(patients_imputed_03, action = 2)