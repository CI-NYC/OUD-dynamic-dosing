# prep data for LTMLE

#load("../Data/clean_combined_imputed_data.Rdata")


#QUESTIONS:
# * When screening out weeks without enough data, are we looking for >=3 people who had a dose increase,
#    or who fit the rule? (dose increase after use, + over some dose threshold)

transform_data_for_ltmle = function (original_weekly_data) {
  weeks_with_outcomes_02 = original_weekly_data
  
  #---------------- Remove patients who can't be analyzed -----------------#
  #QUESTION: are all of these exclusions necessary?
  #Original dataset has 2,189 patients
  #Number who switched meds: 13
  #Number who never initiated treatment: 118
  #Number who dropped out or relapsed before the end of detox: 674
  #     --> No! Keep in people who relapsed on day 21/22, as we'll be looking at their doseages
  #         during detox and it's meaningful to know whether they made it through detox or not
  
  ltmle_prep1 = weeks_with_outcomes_02 %>% 
    #only keep columns we need for LTMLE. we'll add in demographics later using imputed datasets
    select(who, switched_meds, never_initiated, rand_dt, relapse_date, medicine, project,
           week_of_intervention, relapse_this_week, use_this_week, dose_this_week) %>% 
    #remove anyone who switched meds (won't be able to look at their dose / dose increase)
    #remove anyone who never initiated treatment (they won't have any weekly data to analyze)
    # Decided NOT to do this: #remove patients who relapsed before the end of detox (their detox date represents something different)
    # group_by(who) %>% 
    # mutate(gone_before_end_of_detox = relapse_date - rand_dt <= 22) %>% 
    # ungroup() %>% 
    # filter(!switched_meds & !never_initiated & !gone_before_end_of_detox) %>% 
    filter(!switched_meds & !never_initiated) %>% 
    #remove weekly data for weeks past 24
    filter(week_of_intervention <= 24) %>% 
    select(-switched_meds, -never_initiated, -rand_dt, -relapse_date)
  # select(-switched_meds, -never_initiated, -rand_dt, -relapse_date, -gone_before_end_of_detox)
  
  
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
    # Create a default "treatment node" - whether or not **this week's dose is higher than last week's**
    # Note: if our treatment definition is more complex (ex. over a dose threshold), this'll get updated later on.
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
  
  # Wide dataset to return:
  ltmle_prep3
}

#---------------- Create a set of different cases to try out -----------------#

case_attributes = c("medicine", "projects", "dose_threshold", "name", "max_weeks", "responsive")

#Bupenorphine
case1 = list("bup", c(27, 51), 32, "Buprenorphine (p27 & p51), responsive dose increase", 24, TRUE)
names(case1) = case_attributes

case2 = list("bup", c(27), 32, "Buprenorphine (p27), responsive dose increase", 24, TRUE)
names(case2) = case_attributes

case3 = list("bup", c(30), 32, "Buprenorphine (p30), responsive dose increase", 24, TRUE)
names(case3) = case_attributes

case4 = list("bup", c(51), 32, "Buprenorphine (p51), responsive dose increase", 24, TRUE)
names(case4) = case_attributes

#Dose increases above threshold: 16, 20, 24 mg
case5 = list("bup", c(27, 51), 16, "Buprenorphine (p27 & p50), responsive dose increase if <16mg", 24, TRUE)
names(case5) = case_attributes

case6 = list("bup", c(27, 51), 20, "Buprenorphine (p27 & p50), responsive dose increase if <20mg", 24, TRUE)
names(case6) = case_attributes

case7 = list("bup", c(27, 51), 24, "Buprenorphine (p27 & p50), responsive dose increase if <24mg", 24, TRUE)
names(case7) = case_attributes

#Methodone: only given in p27
case8 = list("met", c(27), 150, "Methodone (p27), responsive dose increase", 24, TRUE)
names(case8) = case_attributes

#Dose increases above threshold: 40, 50, 80, 100 mg
case9 = list("met", c(27), 40, "Methodone (p27), responsive dose increase if <40mg", 24, TRUE)
names(case9) = case_attributes

case10 = list("met", c(27), 50, "Methodone (p27), responsive dose increase if <50mg", 24, TRUE)
names(case10) = case_attributes

case11 = list("met", c(27), 80, "Methodone (p27), responsive dose increase if <80mg", 24, TRUE)
names(case11) = case_attributes

case12 = list("met", c(27), 100, "Methodone (p27), responsive dose increase if <100mg", 24, TRUE)
names(case12) = case_attributes

#All projects together, buprenorphine, only up to 12 weeks
case13 = list("bup", c(27, 30, 51), 32, "Buprenorphine (p27, p30 & p50), responsive dose increase - only till wk12", 12, TRUE)
names(case13) = case_attributes

#NON-RESPONSIVE - just dose increases if you're under a threshold
case14 = list("bup", c(27, 51), 16, "Buprenorphine (p27 & p50), any dose increase if <16mg", 24, FALSE)
names(case14) = case_attributes

case15 = list("bup", c(27, 51), 20, "Buprenorphine (p27 & p50), any dose increase if <20mg", 24, FALSE)
names(case15) = case_attributes

case16 = list("met", c(27), 40, "Methodone (p27), any dose increase if <40mg", 24, FALSE)
names(case16) = case_attributes

case17 = list("met", c(27), 50, "Methodone (p27), any dose increase if <50mg", 24, FALSE)
names(case17) = case_attributes

case18 = list("met", c(27), 80, "Methodone (p27), any dose increase if <80mg", 24, FALSE)
names(case18) = case_attributes

case19 = list("met", c(27), 100, "Methodone (p27), any dose increase if <100mg", 24, FALSE)
names(case19) = case_attributes

cases = list(case1, case2, case3, case4, case5, case6,
             case7, case8, case9, case10, case11, case12, case13,
             case14, case15, case16, case17, case18, case19)


#---------------- Function: take in a case, output a dataset and parameters ready for LTMLE -----------------#

ltmle_case_prep = function(data, case) {
  #Step 1: filter the data down to only the medicine, project, and weeks specified by the case
  filtered_data = data %>% 
    filter(project %in% case[["projects"]] & medicine == case[["medicine"]]) %>% 
    select(-starts_with(paste0("wk", (case[["max_weeks"]]+1):25)))
  
  # DELETE
  #Step 2: make "treatment node" approporiate for this case, Anodes: treatment_this_week
  #  this takes into account the dose threshold (0 if we just care about any dose increase)
  # filtered_data = filtered_data %>% 
  #   set_names(~ str_replace_all(., "dose_increase_this_week", "treatment_this_week"))
  # 
  # for (week in 1:case[["max_weeks"]]) {
  #   treatment_column_name = paste0("wk", week, ".treatment_this_week")
  #   dose_column_name = paste0("wk", week, ".dose_this_week")
  #   
  #   filtered_data = filtered_data %>% 
  #     mutate(!!treatment_column_name :=
  #              as.numeric((!!sym(treatment_column_name) == 1) & (!!sym(dose_column_name) >= case[["dose_threshold"]]))
  #     )
  # }
  
  #Step 2: check whether there are enough events in each week to include that week in the analysis

  #Are there any weeks (of 4-24) where no-one's outcome changed (no new relapses)?
  #...If so, exclude those weeks from the analysis alltogether.
  outcome_weeks = c()
  for (w in 4:case[["max_weeks"]]) {
    this_week_relapse_col = paste0("wk", w, ".relapse_this_week")
    last_week_relapse_col = paste0("wk", w-1, ".relapse_this_week")
    if (!all(filtered_data[[sym(this_week_relapse_col)]] == filtered_data[[sym(last_week_relapse_col)]], na.rm = TRUE)) {
      outcome_weeks = c(outcome_weeks, w)
    }
  }
  
  #Are there any weeks with 0 people who fit the treatment rule?
  #...If so, exclude those weeks from the Anodes, Lnodes, and Abar.
  #...also, exclude any weeks past week 12.
  weeks_with_treatments = c()
  # only go up to whichever is first, 12 weeks or max_weeks
  for (w in 4:min(case[["max_weeks"]], 12)) {
    increases_this_week = filtered_data[[sym(paste0("wk", w, ".dose_increase_this_week"))]]
    under_threshold_last_week = filtered_data[[sym(paste0("wk", w - 1, ".dose_this_week"))]] < case[["dose_threshold"]]
    use_last_week = ifelse(rep(case[["responsive"]], nrow(filtered_data)), 
                           filtered_data[[sym(paste0("wk", w - 1, ".use_this_week"))]],
                           rep(TRUE, nrow(filtered_data)))
    
    # cat(paste0("Increases this week: ", sum(increases_this_week), "\n"))
    # cat(paste0("Use last week: ", sum(use_last_week), "\n"))
    treatment = increases_this_week & under_threshold_last_week & use_last_week
    # cat(paste0("Treatments this week: ", sum(treatment), "\n"))
    if (sum(treatment) > 0) {
      weeks_with_treatments = c(weeks_with_treatments, w)
    }
  }
  
  #Step 4: create the lists of Anodes, Lnodes, Ynodes, and abar
  
  #Anodes (treatment nodes) - each includes the treatment node for this and all previous weeks...
  #...restricted to weeks where a relapse outcome and a treatment were both possible (so, 4-24)
  Anodes = vector("list", length(outcome_weeks))
  for (i in 1:length(outcome_weeks)) {
    Anodes[[i]] <- #c("wk3.dose_increase_this_week", 
                     c(paste0("wk", outcome_weeks[1:i][outcome_weeks[1:i] %in% weeks_with_treatments], ".dose_increase_this_week"))
  }

  #Lnodes (time-varying covariates) - each includes `dose_this_week` lagged by 1 (3-24)...
  #... and `use_this_week` lagged by 1 for this and all previous weeks (2-24).
  #  (for use this week, include a week if the next week was a week with a treatment)
  Lnodes = vector("list", length(outcome_weeks))
  for (i in 1:length(outcome_weeks)) {
    Lnodes[[i]] <- c(paste0("wk", c(3, outcome_weeks[1:i-1][outcome_weeks[1:i-1] %in% c(weeks_with_treatments-1)]), ".dose_this_week"),
                     paste0("wk", c(2, 3, outcome_weeks[1:i-1][outcome_weeks[1:i-1] %in% weeks_with_treatments]), ".use_this_week"))
  }
  
  #Ynodes (outcome nodes) - `relapse_this_week` for each week...
  #...restricted to weeks where a relapse outcome was possible (so, 4-24)
  Ynodes = paste0("wk", outcome_weeks, ".relapse_this_week")
  
  
  #abar (treatment rule)
  ## First part of the rule: did they have use the week before? (this is necessary for RESPONSIVE treatment rules)
  #...for each observation at each time point, was there any opioid use the week before?
  abar1a <- filtered_data[, paste0("wk", c(outcome_weeks - 1)[outcome_weeks %in% weeks_with_treatments], ".use_this_week")] == 1

  ## Second part of the rule: were they able to be increased? (i.e. under the max dose last week)
  #...for each observation at each time point, were they under the max possible dose the week before?
  max_dose = ifelse(case[["medicine"]] == "bup", 32, 150)
  abar1b <- filtered_data[, paste0("wk", c(outcome_weeks - 1)[outcome_weeks %in% weeks_with_treatments], ".dose_this_week")] < max_dose

  ## Third part of the rule: were they below the threshold we're interested in? (if no threshold, use max dose)
  #...for each observation at each time point, were they under the threshold dose the week before?
  #... (meaning: if we're interested in dose increases for anyone who is under 16mg, 
  #     we "count" them if last week they were under 16, and this week they got a dose increase)
  abar1c <- filtered_data[, paste0("wk", c(outcome_weeks - 1)[outcome_weeks %in% weeks_with_treatments], ".dose_this_week")] < case[["dose_threshold"]]
  
  #...a matrix of what the treatment would be for every observation at every time point,
  #...in a hypothetical population who always had a dose increase when appropriate
  if (case[["responsive"]]) {
    abar1 <- I(abar1a == TRUE & abar1b == TRUE & abar1c == TRUE)
  } else {
    abar1 <- I(abar1b == TRUE & abar1c == TRUE)
  }
  
  #...a matrix of what the treatment would be for every observation at every time point,
  #...in a hypothetical population with no dynamic dose increase (so, all FALSE)
  abar0 <- matrix(rep(FALSE, length(weeks_with_treatments)*nrow(filtered_data)), ncol = length(weeks_with_treatments))
  
  #Step 5: create a table showing the counts of different types of patient events at each week
  weekly_counts = tibble(week = 1:case[["max_weeks"]]) %>% 
    group_by(week) %>% 
    mutate(patients = sum(filtered_data[, paste0("wk", week, ".relapse_this_week")] == 0),
            dose_increase_this_week = sum(filtered_data[, paste0("wk", week, ".dose_increase_this_week")] == 1),
            use_last_week = sum(filtered_data[, paste0("wk", max(week - 1, 1), ".use_this_week")] == 1),
            treatment_this_week = sum(filtered_data[, paste0("wk", week, ".dose_increase_this_week")] == 1 &
                                                ifelse(rep(case[["responsive"]], nrow(filtered_data)), 
                                                       filtered_data[, paste0("wk", max(week - 1, 1), ".use_this_week")] == 1, 
                                                       rep(TRUE, nrow(filtered_data))) &
                                                filtered_data[, paste0("wk", max(week - 1, 1), ".dose_this_week")] < case[["dose_threshold"]]),
           #we want to create summary counts, but only want patients who haven't yet relapsed (the [[1,2]])
           dose_mean = aggregate(filtered_data[, paste0("wk", week, ".dose_this_week")],
                                        by = filtered_data[, paste0("wk", week, ".relapse_this_week")],
                                        FUN = mean)[[1,2]],
           dose_median = aggregate(filtered_data[, paste0("wk", week, ".dose_this_week")],
                                 by = filtered_data[, paste0("wk", week, ".relapse_this_week")],
                                 FUN = median)[[1,2]],
           dose_min = aggregate(filtered_data[, paste0("wk", week, ".dose_this_week")],
                                 by = filtered_data[, paste0("wk", week, ".relapse_this_week")],
                                 FUN = min)[[1,2]],
           dose_max = aggregate(filtered_data[, paste0("wk", week, ".dose_this_week")],
                                 by = filtered_data[, paste0("wk", week, ".relapse_this_week")],
                                 FUN = max)[[1,2]],
           dose_iqr = aggregate(filtered_data[, paste0("wk", week, ".dose_this_week")],
                                 by = filtered_data[, paste0("wk", week, ".relapse_this_week")],
                                 FUN = IQR)[[1,2]]
    ) %>% 
    ungroup() %>% 
    mutate(new_relapse_this_week = lag(patients, default = 0) - patients)
  
  #Step 6: return all inputs for the LTMLE function as a list with named elements
  input_names = c("dataset_missing_baseline_covariates",
                  "Anodes",
                  "Lnodes",
                  "Ynodes",
                  "abar1",
                  "abar0",
                  "outcome_weeks",
                  "weeks_with_treatments",
                  "weekly_counts",
                  "name",
                  "max_weeks")
  inputs = list(filtered_data,
             Anodes,
             Lnodes,
             Ynodes,
             abar1,
             abar0,
             outcome_weeks,
             weeks_with_treatments,
             weekly_counts,
             case[["name"]],
             case[["max_weeks"]])
  names(inputs) = input_names
  
  inputs
}

# ## FOR TESTING (delete later)
# for_analysis_case1_expandedcasedef = ltmle_case_prep(transform_data_for_ltmle(ALT_weeks_with_outcomes_02), case1)
# #for_analysis_case13_expandedcasedef = ltmle_case_prep(transform_data_for_ltmle(ALT_weeks_with_outcomes_02), case13)
# #for_analysis_case17_expandedcasedef = ltmle_case_prep(transform_data_for_ltmle(ALT_weeks_with_outcomes_02), case17)
# 
# est1 = run_ltmle_case(for_analysis_case1_expandedcasedef, ALT_patients_imputed_03, 1)
# #est2 = run_ltmle_case(for_analysis_case1_expandedcasedef, ALT_patients_imputed_03, 1)
# 
# #ANALYSIS
# plot_ltmle(combine_estimates_across_imputations(for_analysis_case1_expandedcasedef, 
#                                                 ALT_patients_imputed_03),
#            "7-29-TESTcase", "1")
# 
# plot_ltmle(combine_estimates_across_imputations(for_analysis_case13_expandedcasedef, 
#                                                 ALT_patients_imputed_03),
#            "7-22-TESTcase", "13")
# 
# plot_ltmle(combine_estimates_across_imputations(for_analysis_case17_expandedcasedef, 
#                                                 ALT_patients_imputed_03),
#            "7-22-TESTcase", "17")

#a = transform_data_for_ltmle(ALT_weeks_with_outcomes_02)

# for_analysis_case1_ORIGINAL = ltmle_case_prep(transform_data_for_ltmle(ALT_weeks_with_outcomes_02), case1)
# 
# for_analysis_case1_NEW = ltmle_case_prep(transform_data_for_ltmle(ALT_weeks_with_outcomes_02), case1)

## USE THIS instead of the code below, ifyou want to use the original relapse definitions
# weekly_data_for_ltmle_04 = list()
# for (case in cases) {
#   weekly_data_for_ltmle_04 = c(weekly_data_for_ltmle_04,
#                                list(ltmle_case_prep(transform_data_for_ltmle(weeks_with_outcomes_02), case)))
# }

ALT_weekly_data_for_ltmle_04 = list()
for (case in cases) {
  ALT_weekly_data_for_ltmle_04 = c(ALT_weekly_data_for_ltmle_04,
                                   list(ltmle_case_prep(transform_data_for_ltmle(ALT_weeks_with_outcomes_02), case)))
}

# for (case in ALT_weekly_data_for_ltmle_04) {
#   cat(case$weeks_with_treatments)
#   cat("\n")
# }