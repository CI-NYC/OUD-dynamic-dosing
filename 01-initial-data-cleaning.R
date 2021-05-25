#Initial data cleaning file.
#starts with raw visit-level data,
#and also ends with visit-level data, with information clarified and errors corrected

library(tidyverse)
library(stringr)
library(lubridate)

#---------------- Importing the data -----------------#

#Note: the long strings denote the type for each column (factor, character, number).
#Dates didn't convert nicely so I deal with them in the next step
druguse_data_raw = read_csv("../Data/druguseupctn94.csv", col_types = "fcnffffcfcfnfffnfnfffffffffffffffffff")

druguse_data_raw$fusedt12 <- as.Date(druguse_data_raw$fusedt12, format = "%m/%d/%y")
druguse_data_raw$fusedt24 <- as.Date(druguse_data_raw$fusedt24, format = "%m/%d/%y")
druguse_data_raw$when <- as.Date(druguse_data_raw$when, format = "%m/%d/%y", origin = "1960-01-01")
druguse_data_raw$rand_dt <- as.Date(druguse_data_raw$rand_dt, origin = "1960-01-01")

visits_raw = druguse_data_raw

#Note: the commented-out code below shows that the baseline data was all consistently copied...
#...through the druguse dataset, so no need to merge the two. We can just use the druguse data as-is.

# baseline_data_raw = read_csv("../Data/baselinectn94.csv", col_types = "fffffcfcfnfnfffffffffffffffffff")
# baseline_data_raw$fusedt12 <- as.Date(baseline_data_raw$fusedt12, format = "%m/%d/%y")
# baseline_data_raw$fusedt24 <- as.Date(baseline_data_raw$fusedt24, format = "%m/%d/%y")
# 
# visits_raw = merge(baseline_data_raw[, colnames(baseline_data_raw)],
#                    #Take all columns from the baseline data set
#                    druguse_data_raw[, c("who", setdiff(colnames(druguse_data_raw), 
#                                                        colnames(baseline_data_raw)))],
#                    #Take columns that were *not* in baseline from the druguse dataset
#                    by = "who", #Merge on patient id (who)
#                    all = TRUE)
# 
# diffdf(druguse_data_raw, visits_raw)

#---------------- Recoding missing data as NA -----------------#

#Convert values coded as -8/-9/-1 to NA
#Note: we have to do this differently for columns that are factors vs. the mg column which is doubles
visits_raw = visits_raw %>%
  mutate(across(
    where(is.factor),
    .fns = ~ recode(.x, "-9" = NA_character_, "-8" = NA_character_)
  )) %>%
  mutate(mg = recode(mg, "-9" = NA_real_)) %>% 
  mutate(uopioid = recode(uopioid, "-1" = NA_character_))

#---------------- Re-leveling factors -----------------#

#Relevel the factors to avoid annoying R bugs
#Note: I might have gotten some of these guesses wrong, see codebook.
visits_raw = visits_raw %>%
  mutate(
    project = fct_relevel(project, c("27", "30", "51")),
    isHispanic = fct_recode(isHispanic, "no" = "0", "yes" = "1"),
    race = fct_relevel(race, c("1", "2", "3", "7")),
    opioiduse12 = fct_recode(opioiduse12, "no" = "0", "yes" = "1"),
    opioiduse24 = fct_recode(opioiduse24, "no" = "0", "yes" = "1"),
    sex = fct_recode(sex, "male" = "1", "female" = "2"),
    xrace = fct_relevel(xrace, c("1", "2", "3", "4")),
    hwithdraw = fct_relevel(hwithdraw, c("1", "2", "3", "4")),
    alcdisorder = fct_recode(alcdisorder, "no" = "0", "yes" = "1"),
    cocdisorder = fct_recode(cocdisorder, "no" = "0", "yes" = "1"),
    hasBrainDamage = fct_recode(hasBrainDamage, "no" = "0", "yes" = "1"),
    hasEpilepsy = fct_recode(hasEpilepsy, "no" = "0", "yes" = "1"),
    hasSchiz = fct_recode(hasSchiz, "no" = "0", "yes" = "1"),
    hasBipolar = fct_recode(hasBipolar, "no" = "0", "yes" = "1"),
    hasAnxPan = fct_recode(hasAnxPan, "no" = "0", "yes" = "1"),
    hasMajorDep = fct_recode(hasMajorDep, "no" = "0", "yes" = "1"),
    trt = fct_relevel(trt, c("1", "2", "3", "4", "5")),
    edu = fct_relevel(edu, c("1", "2", "3")),
    mar = fct_relevel(mar, c("1", "2", "3")),
    falcohol = fct_recode(falcohol, "no" = "0", "yes" = "1"),
    fdrug = fct_recode(fdrug, "no" = "0", "yes" = "1"),
    bamphetamine30_base = fct_recode(bamphetamine30_base, "no" = "0", "yes" = "1"),
    bcannabis30_base = fct_recode(bcannabis30_base, "no" = "0", "yes" = "1"),
    bbenzo30_base = fct_recode(bbenzo30_base, "no" = "0", "yes" = "1"),
    ivdrug = fct_recode(ivdrug, "no" = "0", "yes" = "1"),
    medicine = fct_recode(medicine, "met" = "1", "bup" = "2", "nal" = "3"),
    selfopioid = fct_recode(selfopioid, "no" = "0", "yes" = "1"),
    uopioid = fct_recode(uopioid, "no" = "0", "yes" = "1"),
  )

#---------------- Creating new helper columns, fixing little data anomolies -----------------#

# Some notes and conclusions from exploring the data (code not included):
# 
# * 28 patients never initiated treatment (missing `when`, only one entry for them)
# * Everyone got randomized
# * No one got randomized multiple times.
# * No one has a visit before their randomization date.
# * Almost all patients have their first visit on their randomization date.
# * However, 3 patients have first visits one day later. (`0403-06-1059, 0403-06-1060, 0605-06-2005`).
# * **Decision:** I'm shifting all dates for these patients one day earlier, as if their first visit happened on their randomization date.
# * No one has more than one visit on the same day.
# * 64 patients only had 1 visit (0 days between first and last visit, we know there are no repeat visits on the same day)
# * 1114 patients had exactly 174 days between their first and last visit (~6 months)
# * 174 was the maximum date range, so we know there are no outliers on the high or low end
# * All visit dates are always consecutive, except for 32 patients missing a couple days right before their last day
# * **Decision:** We'll create new rows for those days copying the `medicine` and `mg` from the previous day, and putting NA in the `selfopioid` and `uopioid` columns.
# 
# * 79 patients never had `medicine` or `mg > 0` recorded
# * 11 patients had a `medicine` recorded, but never `mg > 0`
# * **Decision:** People in the two cases above were marked as `never_initiated`.
# 
# * There are 44 patients whose medicine switched at some point. They were all in project 27, and all had only medicines 1 & 2.
# * I think if they only ever got `0 mg` of their "other med", then it was probably a mistake.
# * There are 12 people who seemed to genuinely switch meds (about half then switched back after a couple visits).
# * One person was assigned to `trt=1` and got bupenorphine the first day, and methadone every subsequent day.
#      It's not quite the same as "switched meds", but it also doesn't look like a mistake?
# * **Decision:**
#   * **I made a new column `switched_meds`, TRUE for those 13 patients identified above.**
#   * **I made another new column `med_switch_date` to capture the date of the first medication switch for those patients.**
#   * **I corrected what I thoguht were errors in the `medicine` column, according to the following rule:**
#   * **If there is one medicine they had marked, but never actually received any doseage of it
#       --> recode that medicine to be whatever the "majority" medicine was that that patient received
# * Only 23+191=214 people are in the "wrong" medicine according to trt.
# * All of these are the `switched_meds` patients, so I don't think there are any remaining errors in the `medicine` or `trt` columns


#Add a new patient-level variable: never initiated treatment.
#It appears that everyone with an entry missing its date also only ever has one visit entry.
#Therefore, I'm guessing that these are participants who were randomized but never initiated treatment.
#Decision: I will fill in `when = rand_dt` for each of these 28 no-date entries, and also mark these patients as never initiated.
visits_raw = visits_raw %>% 
  mutate(never_initiated = ifelse(is.na(when), TRUE, FALSE)) %>% 
  mutate(when = as_date(ifelse(is.na(when), ymd(rand_dt), ymd(when))))

#Function for adding missing visits:
# This function takes in a tibble and outputs a new tibble with new rows/visits
# for any dates where a patient didn't have a visit recorded, but did have days on either side.
# The new rows are repeats of whatever `medicine` and `mg` were in the previous visit
# and `selfopioid` and `uopioid` are both set to NA regardless of previous values
add_missing_visits = function(old_data) {
  new_data = old_data
  
  patients_with_missing_dates =
    old_data %>%
    group_by(who) %>%
    mutate(
      day_before = lag(when),
      day_after = lead(when),
      consecutive = ifelse((when - day_before) == 1 &
                             (day_after - when) == 1, TRUE, FALSE)
    ) %>%
    mutate(num_non_consec = sum(!consecutive, na.rm = TRUE)) %>%
    filter(num_non_consec > 0) %>%
    distinct(who)
  
  for (patient in patients_with_missing_dates$who) {
    this_patient = filter(old_data, who == patient)
    
    full_date_range = seq(min(this_patient$when), max(this_patient$when), by = "day")
    existing_visit_dates = this_patient$when
    dates_without_visits = setdiff(full_date_range, existing_visit_dates)
    
    last_visit_before_missing = this_patient %>% 
      filter(when < min(dates_without_visits)) %>% 
      filter(when == max(when)) %>% 
      mutate(selfopioid = as.factor(NA),
             uopioid = as.factor(NA))
    
    for (new_date in dates_without_visits) {
      new_data = bind_rows(new_data, mutate(last_visit_before_missing, when = as_date(new_date)))
    } 
  }
  # Re-order the database so that everyone's visits are in consecutive order, and return it
  new_data %>% arrange(who, when)
}

# Add new visits for patients missing some in the middle of their sequence
added_dates = add_missing_visits(visits_raw)

# Push dates back by one for the 3 patients who seem to be shifted according to their randomization dates
added_dates = added_dates %>% 
  mutate(when = as_date(ifelse(who %in% c("0403-06-1059", "0403-06-1060", "0605-06-2005"),
                               when - 1,
                               when)))

#Add a new patient-level variable: `end_of_detox` signaling 3 weeks after their first visit, when the detoxification stage is over.
added_dates = added_dates %>% 
  group_by(who) %>% 
  mutate(end_of_detox = min(when) + 21)

#Add new visit-level variables: day and week, counting up from the patient's first visit.
added_dates = added_dates %>% 
  group_by(who) %>% 
  mutate(day_of_intervention = as.integer(when - min(when) + 1),
         week_of_intervention = ceiling(day_of_intervention / 7))

#Mark anyone who never had `mg` recorded (including `mg` always 0) as `never_initiated.`
#The majority of these patients also never had `medicine` recorded.
added_dates = added_dates %>% 
  group_by(who) %>% 
  mutate(never_initiated = ifelse(sum(mg == 0 | is.na(mg)) == n(), TRUE, never_initiated)) %>% 
  ungroup()

# Swap medicine on the entries where mg is always blank for one medicine, likely in error.
added_dates = added_dates %>% 
  group_by(who) %>% 
  mutate(double_meds = n_distinct(medicine, na.rm = TRUE) > 1,
         num_med_1 = sum(medicine == "met", na.rm = TRUE),
         num_med_2 = sum(medicine == "bup", na.rm = TRUE),
         num_med_1_blank = sum(medicine == "met" & !is.na(medicine) & (is.na(mg) | mg == 0)),
         num_med_2_blank = sum(medicine == "bup" & !is.na(medicine) & (is.na(mg) | mg == 0))) %>%
  ungroup() %>% 
  mutate(medicine = as.factor(case_when(
    medicine == "met" & double_meds & (num_med_1 == num_med_1_blank) ~ "bup",
    medicine == "bup" & double_meds & (num_med_2 == num_med_2_blank) ~ "met",
    TRUE ~ as.character(medicine)
  ))) %>% 
  select(-num_med_1, -num_med_2, -num_med_1_blank, -num_med_2_blank, -double_meds)

#Make a new column for those who switched meds at some point.
added_dates = added_dates %>% 
  group_by(who) %>% 
  mutate(switched_meds = ifelse(n_distinct(medicine, na.rm = TRUE) > 1, TRUE, FALSE)) %>%
  ungroup()

#Make a new column for the date when they switched meds
added_dates = added_dates %>% 
  group_by(who) %>% 
  mutate(med_switch_helper = ifelse((medicine != lag(medicine)) & !is.na(lag(medicine)), when, ymd("2050-01-01")),
         med_switch_date = min(med_switch_helper),
         med_switch_date = as_date(ifelse(switched_meds, med_switch_date, NA_Date_))) %>% 
  ungroup() %>% 
  select(-med_switch_helper)

#---------------- Fixing doseage issues & creating doseage-related helper columns -----------------#

# Notes on doseages:
#   
# * Bupenorphine ranged from `0 - 48`, with frequencies clustered around the multiples of 4
# * *-->ANSWER: Bupenorphine should go from 0 to 32, by 4s. Sometimes, it is also supplemented with 2mg naloxone (therefore a 6mg doseage is like 4mg bup, 2mg naloxone).*
# * Methadone ranged from `0 - 397`
# * *-->ANSWER: Methadone should go from 0 to 150, by 10s.*
# * Naltrexone was always 1 (this is an injection)
# 
# **Decisions:**
# * **I created new columns to split out all 4 different medicines: `bup_dose`, `naloxone_dose`, `met_dose`, `naltrexone_dose`.**
# * **I corrected bupenorphine doses by the following rules: (1) if > 32, set to 32; (2) round down to the nearest multiple of 2; (3) if not a multiple of 4, round down to the nearest multiple of 4 and also mark 2mg of naloxone**
# * **I corrected methodone doses by the following rules: (1) if > 150, set to 150; (2) round down to the nearest multiple of 10**
# * **I created a new column to capture whether any medicine was given: `any_dose`**
# * **I kept `mg` as is, but don't think we should use this column any more.**
# 
# * I also fill "medicine" and "mg" both up and down, to account for when this information was missing

# Create new doseage columns
split_doses = added_dates %>% 
  # set up blank columns
  mutate(bup_dose = NA_integer_,
         naloxone_dose = NA_integer_,
         met_dose = NA_integer_,
         naltrexone_dose = NA_integer_,
         any_dose = FALSE) %>% 
  # fill in bup_dose if that was their medicine
  mutate(bup_dose = ifelse(medicine == "bup", mg, NA_integer_),
         # cap at 32 mg
         bup_dose = ifelse(bup_dose > 32, 32, bup_dose),
         # round down to nearest multiple of 2
         bup_dose = floor(bup_dose/2)*2,
         # if it's still not a multiple of 4, mark 2mg of naloxone
         naloxone_dose = ifelse(bup_dose %% 4 == 2, 2, 0),
         # then round down to the nearest multiple of 4
         bup_dose = floor(bup_dose/4)*4) %>% 
  # fill in met_dose if that was their medicine
  mutate(met_dose = ifelse(medicine == "met", mg, NA_integer_),
         # cap at 150
         met_dose = ifelse(met_dose > 150, 150, met_dose),
         # round down to nearest multiple of 10
         met_dose = floor(met_dose/10)*10) %>% 
  # fill in naltrexone_dose column if that was their medicine (will be either 1 or NA)
  mutate(naltrexone_dose = ifelse(medicine == "nal", mg, NA_integer_)) %>% 
  # calculate any_dose based on any of them are over 0
  mutate(any_dose = ifelse(bup_dose > 0 | naloxone_dose > 0 | met_dose > 0 | naltrexone_dose > 0, TRUE, NA),
         # this extra code is needed because for some reason the line above can't return FALSE
         any_dose = ifelse(is.na(any_dose), FALSE, any_dose))


#For each participant, fill in their mg and medicine info according to the values from adjacent visits
filled_med = split_doses %>%
  group_by(who) %>%
  fill(c("medicine", "bup_dose", "naloxone_dose", "naltrexone_dose", "met_dose"), .direction = "downup") %>%
  ungroup() %>% 
  # need to update any_dose
  mutate(any_dose = ifelse(bup_dose > 0 | naloxone_dose > 0 | met_dose > 0 | naltrexone_dose > 0, TRUE, FALSE),
         any_dose = ifelse(is.na(any_dose), FALSE, any_dose))

#---------------- New columns for: doseage by week -----------------#

#Make new colums for: max_dose_this_week, any_dose_this_week, min_nonzero_dose_this_week, dose_change_during_week
#Moving forward, the dose perscribed this week will be understood to be max_dose_this_week
#(but we can use the other columns to look into what's happening on a weekly basis)

visits_with_weekly_dose_added = filled_med %>%
  group_by(who, week_of_intervention) %>% 
  mutate(max_dose_this_week = case_when(medicine == "bup" ~ max(bup_dose),
                                    medicine == "met" ~ max(met_dose),
                                    medicine == "nal" ~ max(naltrexone_dose),
                                    TRUE ~ 0),
         any_dose_this_week = max_dose_this_week > 0,
         #create a dummy variable to weed out the zero doses
         zero_dose_add_1000 = case_when(medicine == "bup" ~ as.numeric(ifelse(bup_dose == 0, 1000, bup_dose)),
                                        medicine == "met" ~ as.numeric(ifelse(met_dose == 0, 1000, met_dose)),
                                        medicine == "nal" ~ as.numeric(naltrexone_dose),
                                        TRUE ~ 0),
         min_nonzero_dose_this_week = ifelse(any_dose_this_week,
                                             case_when(medicine == "bup" ~ min(zero_dose_add_1000),
                                             medicine == "met" ~ min(zero_dose_add_1000),
                                             medicine == "nal" ~ min(naltrexone_dose),
                                             TRUE ~ 0),
                                             0),
         min_nonzero_dose_this_week = ifelse(min_nonzero_dose_this_week == 1000, 0, min_nonzero_dose_this_week),
         dose_change_during_week = max_dose_this_week != min_nonzero_dose_this_week
         ) %>% 
  ungroup() %>% 
  select(-zero_dose_add_1000)

#---------------- Drop redundant or incomplete columns -----------------#

#Keep the `xrace` column, which combines `race` and `isHispanic` (and drop those two)
#Drop the `hcows` column, as it is redundant with `hwithdraw`
#Drop the `edu`, `mar` (married), `falcohol`, and `fdrug` columns because they weren't included in all projects
#Drop the `mg` column, because I've split it into different columns by drug

initial_data_cleaning_no_outcomes_01 = visits_with_weekly_dose_added %>% 
  select(-race, -isHispanic, -hcows, -edu, -mar, -falcohol, -fdrug, -mg)

#---------------- Create lists of column names -----------------#

#Here, I'm creating lists of column names, that will be useful throughout the project
#They will be altered as we create relapse outcomes and add those in

demog = c("who", "sex", "age", "xrace")

treatment_info = c("project", "site", "trt", "rand_dt", "end_of_detox", "medicine",
                   "switched_meds", "med_switch_date", "never_initiated")

comorbidities = c(
  "hwithdraw",
  "alcdisorder",
  "cocdisorder",
  "hasBrainDamage",
  "hasEpilepsy",
  "hasSchiz",
  "hasBipolar",
  "hasAnxPan",
  "hasMajorDep",
  "bamphetamine30_base",
  "bcannabis30_base",
  "bbenzo30_base",
  "ivdrug"
)

outcomes = c(
  # "relapse_overall",
  # "relapse_date",
  # "relapse_12wk_date",
  # "relapse_24wk_date",
  "opioiduse12",
  "fusedt12",
  "opioiduse24",
  "fusedt24"
)

visit_data = c(
  # "use_today",
  "when",
  "day_of_intervention",
  "medicine",
  "selfopioid",
  "uopioid",
  "bup_dose",
  "naloxone_dose",
  "met_dose",
  "naltrexone_dose",
  "any_dose"
)

weekly_indicators = c(
  # "use_this_week",
  # "relapse_this_week",
  # "dose_change_since_last_week", #???
  "week_of_intervention",
  "max_dose_this_week", 
  "any_dose_this_week", 
  "min_nonzero_dose_this_week", 
  "dose_change_during_week"
)
