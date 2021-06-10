#defining relapse outcomes
#TODO: include a couple different versions of outcome definitions
# --> currently only using the outcome from the provided data
# --> would like to include a version where the outcome is at the end of the non-study drug use period

library(lubridate)

all_data = initial_data_cleaning_no_outcomes_01


#---------------- Define relapse -----------------#

#Most simple version: just use provided outcomes. Only look at 24-week.
all_data = all_data %>% 
  mutate(relapse_date = fusedt24)

#update the relevant variable list
outcomes = append(outcomes, "relapse_date")


#---------------- Define use on any given day -----------------#

all_data = all_data %>% 
  # Create a variable for whether the patient used non-study opioids TODAY
  mutate(use_today = FALSE,
         use_today = ifelse((!is.na(uopioid) & uopioid == "yes") |          # a positive urine test
                              (!is.na(selfopioid) & selfopioid == "yes") |  # OR selfopioid is yes
                              any_dose == FALSE,                            # OR no study opioids (dose)
                            TRUE,
                            FALSE),
         # amend the variable such that if they self-report no and don't have a positive urine test,
         # OR they have a negative urine test, that day is no-use
         use_today = ifelse(((!is.na(selfopioid) & selfopioid == "no") &    # selfopioid is no
                               (is.na(uopioid) | uopioid != "yes")) |       # and uopioid isn't yes
                              ((is.na(selfopioid) | selfopioid != "yes") &  # OR selfopioid isn't yes
                                 (!is.na(uopioid) & uopioid == "no")),      # AND uopioid is no
                            FALSE,
                            use_today))

#update the relevant variable list
visit_data = append(visit_data, "use_today")


#---------------- Assign week-level outcome variables -----------------#

#The four variables needed to run LTMLE are: relapse_this_week, use_this_week, dose_this_week, dose_increase_this_week
#NOTE: I'm leaving out dose_increase_this_week, becasue I think that's better added in with the LTMLE prep code
# --> this is the "treatment node" and will need to be different depending on the treatment we're looking at
all_data = all_data %>% 
  group_by(who, week_of_intervention) %>% 
  mutate(relapse_this_week = (relapse_date %within% interval(rand_dt + weeks(week_of_intervention - 1),
                                                            rand_dt + weeks(week_of_intervention) - 1)),
         # NOTE: this will give FALSE if they didn't use on any days this week, but DID drop out this week.
         #    this is corrected for in the ltmle prep code, becuase we will count weeks post-drop out as use weeks
         #Below: if they never relapsed, mark all weeks as FALSE for relapse this week, regardless of relapse date
         relapse_this_week = ifelse(opioiduse24 == "no", FALSE, relapse_this_week),
         use_this_week = sum(use_today, na.rm = TRUE) > 0, 
         dose_this_week = max_dose_this_week
  ) %>% ungroup()

#update the relevant variable list
weekly_indicators = append(weekly_indicators, c("relapse_this_week", "use_this_week", "dose_this_week"))


#---------------- Create data sets on the patient, visit, and week level -----------------#

patients_with_outcomes_02 = all_data %>%
  distinct(who, .keep_all = TRUE) %>%
  select(all_of(c(demog, comorbidities, treatment_info, outcomes)))

visits_with_outcomes_02 = all_data

weeks_with_outcomes_02 = all_data %>%
  distinct(who, week_of_intervention, .keep_all = TRUE) %>%
  select(all_of(c(demog, comorbidities, treatment_info, outcomes, weekly_indicators)))

#---------------- X -----------------#
#---------------- X -----------------#