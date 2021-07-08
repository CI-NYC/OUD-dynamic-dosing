#defining relapse outcomes

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



##---------------- Alternatve version -----------------#

#use this alternate data set for calculating relapse a slightly different way.
#make their relapse date be the end of the period at which they would have qualified
#rather than the beginning.

# RULES:
# 1- opioid use today and every day for the _past_ 6 days means relapse today (on day 7 of use)
# 2- opioid use this week, and at least once a week for the _past_ 3 weeks means relapse...
#    ...at the beginning of this week (so, first day of week 4)
# 3- dropping out means relapse on that day
# all of the above are bounded by the detox period. any relapse date too early will just get...
#  ... shifted up to be the end of the detox period date.

#QUESTION: I'm currently not counting anything that happened pre-detox as relevant. is that correct?
# (ie you can use during detox and it doesn't "count against you")

alternate_data = all_data %>% 
  mutate(relapse_date = ymd("2050-12-12")) %>% 
  group_by(who) %>%
  mutate(
    # Make a helper variable: opioid use today and every day for the past 6 days
    relapsed_7day = ifelse(
      when >= end_of_detox + 7 &
        #when <= rand_dt + 168 - 7 & 
        use_today &
        lag(use_today, n = 1) &
        lag(use_today, n = 2) &
        lag(use_today, n = 3) &
        lag(use_today, n = 4) &
        lag(use_today, n = 5) &
        lag(use_today, n = 6),
        # (lag(use_today, n = 1) | is.na(lag(use_today, n = 1))) &
        # (lag(use_today, n = 2) | is.na(lag(use_today, n = 2))) &
        # (lag(use_today, n = 3) | is.na(lag(use_today, n = 3))) &
        # (lag(use_today, n = 4) | is.na(lag(use_today, n = 4))) &
        # (lag(use_today, n = 5) | is.na(lag(use_today, n = 5))) & 
        # (lag(use_today, n = 6) | is.na(lag(use_today, n = 6))),
      TRUE,
      FALSE
    ),
    # Make a helper variable: opioid use this week and every week for the next 3 weeks
    #   (or, if they stopped showing up before the end of those weeks)
    # Note: this will likely only catch the first day of the 4-week period, but that's ok
    #   (because that's the date we care about anyways)
    relapsed_4wk = ifelse(
      when >= end_of_detox + 7*4 &
        #when <= rand_dt + 168 - 7*3 & 
        use_this_week &
        lag(use_this_week, n = 7) &
        lag(use_this_week, n = 7*2) & 
        lag(use_this_week, n = 7*3),
      TRUE,
      FALSE
    )
  ) %>%
  mutate(
    relapse_overall = FALSE,
    # overall variable that is TRUE if we saw at least one relapsed_7day or relapsed_4wk, and FALSE otherwise
    relapse_overall = (sum(relapsed_7day, na.rm = TRUE) + sum(relapsed_4wk, na.rm = TRUE)) > 0
  ) %>%
  ungroup() %>%
  group_by(who, week_of_intervention) %>%
  # helper variable to find the earliest date of relapse
  mutate(relapse_date_calc = as_date(ifelse(
    (sum(relapsed_7day) > 0 | sum(relapsed_4wk) > 0) & use_today, when, ymd("2050-01-01")))
  ) %>%
  ungroup() %>%
  # mutate(relapse_date_ALT_calc = case_when(
  #   (relapse_date_calc == when) & relapsed_7day ~ when + 6,
  #   (relapse_date_calc == when) & relapsed_4wk ~ when + 7*3,
  #   TRUE ~ ymd("2050-01-01")
  # )) %>% 
  group_by(who) %>%
  # using that helper variable, switch all of the dates in 2050 to NA
  # (it was just a placeholder for no relapse)
  mutate(relapse_date = as_date(min(relapse_date_calc, na.rm = TRUE)),
         relapse_date = as_date(ifelse(
           relapse_date == ymd("2050-01-01"), NA, relapse_date
         ))#,
         # relapse_date_ALT = as_date(min(relapse_date_ALT_calc, na.rm = TRUE)),
         # relapse_date_ALT = as_date(ifelse(
         #   relapse_date_ALT == ymd("2050-01-01"), NA, relapse_date_ALT
         ) %>%
  ungroup() %>%
  select(-relapse_date_calc) %>% 
  # if someone dropped out before a week before the end of their 24-week time:
  #   (in which case the checks above might not catch them as relapsed, even though they likely did)
  # 1-set their relapse date to be EITHER the day they dropped out (as long as it's over the detox date),
  #      or the earliest relapse_date already in the system
  # 2-set relapsed_overall to TRUE
  group_by(who) %>% 
  mutate(relapse_date = as_date(ifelse(max(when) <= rand_dt + 168 - 7, # if they dropped out before a week before the end
                                       min(max(end_of_detox, max(when) + 1), relapse_date, na.rm = TRUE), # re-set their date
                                       relapse_date)),
         # relapse_date_ALT = as_date(ifelse(max(when) <= rand_dt + 168 - 7, # if they dropped out before a week before the end
         #                                   min(max(end_of_detox, max(when) + 1), relapse_date_ALT, na.rm = TRUE), # re-set their date
         #                                   relapse_date_ALT)),
         relapse_overall = ifelse(is.na(relapse_date), FALSE, TRUE)) %>% 
  ungroup() %>% 
  mutate(relapse_12wk_date = as_date(ifelse(relapse_overall == FALSE | relapse_date > rand_dt + 84,
                                            rand_dt + 84,
                                            relapse_date)),
         relapse_24wk_date = as_date(ifelse(relapse_overall == FALSE,
                                            rand_dt + 168,
                                            relapse_date))) %>% 
  arrange(who, when)

# check = alternate_data %>% 
#   distinct(who, .keep_all = TRUE) %>% 
#   select(who, rand_dt, relapse_12wk_date, relapse_24wk_date, fusedt12, fusedt24) %>% 
#   mutate(diff12 = as.factor(relapse_12wk_date - fusedt12),
#          diff24 = as.factor(relapse_24wk_date - fusedt24))


# NOTE: 1483 out of the 2199 patients are getting their relapse date updated.
# I calculated what I thought they should be, and compared against the fused dates
# If the fused date was way different, I kept what they'd given me
# If mine was only different by a couple days, or up to 4 weeks (because of the new definition)
# Then I decided to go with my date as potentially more accurate.
# It's a shame it means that some are probably technically using different definitions for relapse
# But at least preserves the original data in palces that are mysteries to me
# (like where our dates were way off, or we'd come to a different conclusion about relapse overall)
alternate_data = alternate_data %>%
  mutate(relapse_date = as_date(ifelse((relapse_24wk_date - fusedt24) <= 28 & 
                                 (relapse_24wk_date - fusedt24) > 0,
                               relapse_24wk_date,
                               fusedt24))) %>% 
  select(-relapse_12wk_date, -relapse_24wk_date, -relapsed_7day, -relapsed_4wk, -relapse_overall)


#---------------- Create data sets on the patient, visit, and week level -----------------#

patients_with_outcomes_02 = all_data %>%
  distinct(who, .keep_all = TRUE) %>%
  select(all_of(c(demog, comorbidities, treatment_info, outcomes)))

visits_with_outcomes_02 = all_data

weeks_with_outcomes_02 = all_data %>%
  distinct(who, week_of_intervention, .keep_all = TRUE) %>%
  select(all_of(c(demog, comorbidities, treatment_info, outcomes, weekly_indicators)))

#---------------- And the alternates -----------------#

ALT_patients_with_outcomes_02 = alternate_data %>%
  distinct(who, .keep_all = TRUE) %>%
  select(all_of(c(demog, comorbidities, treatment_info, outcomes)))

ALT_visits_with_outcomes_02 = alternate_data

ALT_weeks_with_outcomes_02 = alternate_data %>%
  distinct(who, week_of_intervention, .keep_all = TRUE) %>%
  select(all_of(c(demog, comorbidities, treatment_info, outcomes, weekly_indicators)))
