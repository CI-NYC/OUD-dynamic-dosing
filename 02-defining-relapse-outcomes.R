#defining relapse outcomes
#include a couple different versions of outcome definitions

initial_data_cleaning_no_outcomes_01

#---------------- Define relapse as ????? -----------------#

# relapse_this_week = ifelse(relapse_overall,
#                            relapse_date >= rand_dt + 7*(week_of_intervention - 1) & 
#                              relapse_date < rand_dt + 7*(week_of_intervention),
#                            FALSE)) %>% 
#   

#---------------- Create outcome data sets on the patient, visit, and week level -----------------#

# clean_combined_data = relapse_calc
# 
# patients = clean_combined_data %>% 
#   distinct(who, .keep_all = TRUE) %>% 
#   select(all_of(demog), all_of(treatment_info), all_of(comorbidities), all_of(outcomes))
# 
# weeks = weekly_doseage_changes %>% 
#   select(all_of(demog), all_of(treatment_info), medicine, all_of(comorbidities), all_of(weekly_indicators))

#---------------- X -----------------#
#---------------- X -----------------#