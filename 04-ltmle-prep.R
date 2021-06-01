# prep data for LTMLE

#QUESTIONS:
# * For our A-nodes (dose_increase_this_week), is this the treatment [any dose increase],
#    or the treatment rule [dose increase after use, above threshold]?
# * When screening out weeks without enough data, are we looking for >=3 people who had a dose increase,
#    or who fit the rule? (dose increase after use, + over some dose threshold)

#---------------- Remove patients who can't be analyzed -----------------#

#QUESTION: are all of these exclusions necessary?
#Original dataset has 2,189 patients
#Number who switched meds: 13
#Number who never initiated treatment: 118
#Number who dropped out or relapsed before the end of detox: 674
#                       (including 106 of never initiated)
#
#How many are left after removing all of the above: 1,503
#
#NOTE: there's an argument to be made that we should only start looking at outcomes
#  starting in week 5, since we need to see how week 4 went. let's discuss!

ltmle_prep1 = weeks_with_outcomes_02 %>% 
  #only keep columns we need for LTMLE. we'll add in demographics later using imputed datasets
  select(who, switched_meds, never_initiated, rand_dt, relapse_date, medicine, project,
         week_of_intervention, relapse_this_week, use_this_week, dose_this_week) %>% 
  #remove anyone who switched meds (won't be able to look at their dose / dose increase)
  #remove anyone who never initiated treatment (they won't have any weekly data to analyze)
  #remove patients who relapsed before the end of detox (their detox date represents something different)
  group_by(who) %>% 
  mutate(gone_before_end_of_detox = relapse_date - rand_dt <= 22) %>% 
  ungroup() %>% 
  filter(!switched_meds & !never_initiated & !gone_before_end_of_detox) %>% 
  #remove weekly data for weeks past 24
  filter(week_of_intervention <= 24) %>% 
  select(-switched_meds, -never_initiated, -rand_dt, -relapse_date, -gone_before_end_of_detox)


#---------------- Instructions on how LTMLE expects the data -----------------#

# LTMLE needs the data to be:
#  * in wide format, with all baseline covariates first, and all time-varying last
#  * no un-used columns (remove them at the very end before running)
#  * all weekly variables (outcomes, time-varying covariates, and treatment) as 1/0, rather than TRUE/FALSE
#  * make sure every patient's last outcome is 1 (relapsed), unless they have complete data through week 24
#    --> DECISION: (double check!) make the last week we have data for someone into a relapse if it isn't already (except 24)
#  * make sure every patient's outcome is 1 (relapsed) following any outcome of 1


#---------------- Change data according to the rules above -----------------#

ltmle_prep2 = ltmle_prep1 %>% 
  group_by(who) %>% 
  # if it's the last week we have recorded for this patient AND it's earlier than week 24,
  # mark them as relapsed this week. otherwise leave outcome as-is
  mutate(relapse_this_week = ifelse(week_of_intervention == max(week_of_intervention) &
                                      week_of_intervention < 24,
                                    TRUE,
                                    relapse_this_week),
         #if they've ever had a previous relapse, mark them as relapsed for every following week
         # (because our outcome must be monotonic for the ltmle function)
         relapse_this_week = ifelse(lag(relapse_this_week, default = FALSE) |
                                      lag(relapse_this_week, default = FALSE, n = 2) |
                                      lag(relapse_this_week, default = FALSE, n = 3) |
                                      lag(relapse_this_week, default = FALSE, n = 4) |
                                      lag(relapse_this_week, default = FALSE, n = 5) |
                                      lag(relapse_this_week, default = FALSE, n = 6) |
                                      lag(relapse_this_week, default = FALSE, n = 7) |
                                      lag(relapse_this_week, default = FALSE, n = 8) |
                                      lag(relapse_this_week, default = FALSE, n = 9) |
                                      lag(relapse_this_week, default = FALSE, n = 10) |
                                      lag(relapse_this_week, default = FALSE, n = 11) |
                                      lag(relapse_this_week, default = FALSE, n = 12) |
                                      lag(relapse_this_week, default = FALSE, n = 13) |
                                      lag(relapse_this_week, default = FALSE, n = 14) |
                                      lag(relapse_this_week, default = FALSE, n = 15) |
                                      lag(relapse_this_week, default = FALSE, n = 16) |
                                      lag(relapse_this_week, default = FALSE, n = 17) |
                                      lag(relapse_this_week, default = FALSE, n = 18) |
                                      lag(relapse_this_week, default = FALSE, n = 19) |
                                      lag(relapse_this_week, default = FALSE, n = 20) |
                                      lag(relapse_this_week, default = FALSE, n = 21) |
                                      lag(relapse_this_week, default = FALSE, n = 22) |
                                      lag(relapse_this_week, default = FALSE, n = 23) |
                                      lag(relapse_this_week, default = FALSE, n = 24),
                                    TRUE,
                                    relapse_this_week)) %>%
  ungroup() %>% 
  # Create "treatment node" - whether or not **this week's dose is higher than last week's**
  mutate(dose_increase_this_week = dose_this_week > lag(dose_this_week, default = 0),
         # BUT if they've already relapsed, count it as no dose increase (can't have treatment after outcome)
         dose_increase_this_week = ifelse(relapse_this_week, FALSE, dose_increase_this_week)
         ) %>% 
  # change TRUE/FALSE to 1/0
  mutate(relapse_this_week = as.numeric(relapse_this_week),
         use_this_week = as.numeric(use_this_week),
         dose_increase_this_week = as.numeric(dose_increase_this_week))


#---------------- Pivot to wide format -----------------#

ltmle_prep3 = ltmle_prep2 %>% 
  # pivot to wide format
  pivot_wider(names_from = week_of_intervention,
              names_glue = "wk{week_of_intervention}.{.value}",
              values_from = c(dose_this_week,
                              use_this_week,
                              relapse_this_week,
                              dose_increase_this_week),
              values_fill = NA) %>% 
  # LTMLE doesn't play nice with NAs (which are generated by pivot_wider for any weeks that weren't documented)
  # so here I'm replacing any NAs with a default value (1 for relapse, 0 for the others).
  # These won't get used in LTMLE computations anyways, since they all occur after a relapse
  mutate_at(vars(contains("relapse_this_week")), ~replace_na(., 1)) %>%
  mutate_at(vars(contains("dose_this_week")), ~replace_na(., 0)) %>%
  mutate_at(vars(contains("use_this_week")), ~replace_na(., 0)) %>% 
  mutate_at(vars(contains("dose_increase_this_week")), ~replace_na(., 0))


#---------------- Create a set of different cases to try out -----------------#

case_attributes = c("medicine", "projects", "dose_threshold", "name")

#Bupenorphine
case1 = list("bup", c(27, 30, 51), 0, "Bupenorphine (all), any dose increase")
names(case1) = case_attributes

case2 = list("bup", c(27), 0, "Bupenorphine (p27), any dose increase")
names(case2) = case_attributes

case3 = list("bup", c(30), 0, "Bupenorphine (p30), any dose increase")
names(case3) = case_attributes

case4 = list("bup", c(51), 0, "Bupenorphine (p51), any dose increase")
names(case4) = case_attributes

#Dose increases above threshold: 16, 20, 24 mg
case5 = list("bup", c(27, 30, 51), 16, "Bupenorphine (all), dose increase to >= 16")
names(case5) = case_attributes

case6 = list("bup", c(27, 30, 51), 20, "Bupenorphine (all), dose increase to >= 20")
names(case6) = case_attributes

case7 = list("bup", c(27, 30, 51), 24, "Bupenorphine (all), dose increase to >= 24")
names(case7) = case_attributes

#Methodone: only given in p27
case8 = list("met", c(27), 0, "Methodone (p27), any dose increase")
names(case8) = case_attributes

cases_easy = c(case1, case2, case3, case4, case8)
cases_hard = c(case5, case6, case7)

#---------------- Function: take in a case, output a dataset and parameters ready for LTMLE -----------------#

ltmle_case_prep = function(data, case) {
  #Step 1: filter the data down to only the medicine and project specified by the case
  filtered_data = data %>% 
    filter(project %in% case[["projects"]] & medicine == case[["medicine"]])
  
  #Step 2: check whether there are enough events in each week to include that week in the analysis

  #Are there any weeks (of 4-24) where no-one's outcome changed (no new relapses)?
  #...If so, exclude those weeks from the analysis alltogether.
  outcome_weeks = c()
  for (w in 4:24) {
    this_week_relapse_col = paste0("wk", w, ".relapse_this_week")
    last_week_relapse_col = paste0("wk", w-1, ".relapse_this_week")
    if (!all(filtered_data[[sym(this_week_relapse_col)]] == filtered_data[[sym(last_week_relapse_col)]], na.rm = TRUE)) {
      outcome_weeks = c(outcome_weeks, w)
    }
  }
  
  #Are there any weeks with under 3 dose increases?
  #...If so, exclude those weeks from making the dynamic dosing rule
  weeks_with_dose_increases = c()
  for (w in 4:24) {
    num_increases = sum(filtered_data[[sym(paste0("wk", w, ".dose_increase_this_week"))]])
    if (num_increases >= 3) {
      weeks_with_dose_increases = c(weeks_with_dose_increases, w)
    }
  }
  
  #Step 3: create the lists of Anodes, Lnodes, Ynodes, and abar
  
  #Anodes (treatment nodes) - each includes the treatment node for this and all previous weeks...
  #...restricted to weeks where a relapse outcome and a treatment were both possible (so, 4-24)
  Anodes = vector("list", length(outcome_weeks))
  for (i in 1:length(outcome_weeks)) {
    Anodes[[i]] <- paste0("wk", outcome_weeks[1:i], ".dose_increase_this_week")
  }

  #Lnodes (time-varying covariates) - each includes `dose_this_week` lagged by 1 (3-24)...
  #... and `use_this_week` lagged by 1 for this and all previous weeks (2-24).
  Lnodes = vector("list", length(outcome_weeks))
  for (i in 1:length(outcome_weeks)) {
    Lnodes[[i]] <- c(paste0("wk", c(3, outcome_weeks[1:i-1]), ".dose_this_week"),
                     paste0("wk", c(2, 3, outcome_weeks[1:i-1]), ".use_this_week"))
  }
  
  #Ynodes (outcome nodes) - `relapse_this_week` for each week...
  #...restricted to weeks where a relapse outcome was possible (so, 4-24)
  Ynodes = paste0("wk", outcome_weeks, ".relapse_this_week")
  
  
  #abar (treatment rule)
  #...for each observation at each time point, was there any opioid use the week before?
  abar1a <- filtered_data[, paste0("wk", outcome_weeks - 1, ".use_this_week")] == 1
  
  #...for each observation at each time point, were they under the max possible dose the week before?
  max_dose = ifelse(case[["medicine"]] == "bup", 32, 150)
  abar1b <- filtered_data[, paste0("wk", outcome_weeks - 1, ".dose_this_week")] < max_dose
  
  #...a matrix of what the treatment would be for every observation at every time point,
  #...in a hypothetical population who always had a dynamic dose increase when appropriate
  abar1 <- I(abar1a == TRUE & abar1b == TRUE)
  
  #...a matrix of what the treatment would be for every observation at every time point,
  #...in a hypothetical population with no dynamic dose increase (so, all FALSE)
  abar0 <- matrix(rep(FALSE, length(outcome_weeks)*nrow(filtered_data)), ncol = length(outcome_weeks))
  
  
  #Step 4: return all inputs for the LTMLE function as a list with named elements
  input_names = c("dataset_missing_baseline_covariates",
                  "Anodes",
                  "Lnodes",
                  "Ynodes",
                  "abar1",
                  "abar0",
                  "outcome_weeks",
                  "name")
  inputs = list(filtered_data,
             Anodes,
             Lnodes,
             Ynodes,
             abar1,
             abar0,
             outcome_weeks,
             case[["name"]])
  names(inputs) = input_names
  
  inputs
}

test_case_1 = ltmle_case_prep(ltmle_prep3, case1)
test_case_4 = ltmle_case_prep(ltmle_prep3, case4)

# weekly_data_for_ltmle_04 = list()
# for (case in cases_easy) {
#   weekly_data_for_ltmle_04 = append(weekly_data_for_ltmle_04, ltmle_case_prep(ltmle_prep3, case))
# }
  
