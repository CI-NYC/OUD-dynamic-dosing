library(tidyverse)
library(stringr)
library(lubridate)

drugUse_raw = read_csv("data/druguse_06062021.csv")
drugUse = drugUse_raw

drugUse = mutate(drugUse,
  across(
    c("fusedt12", "fusedt24", "when"),
    \(x) as.Date(x, format = "%m/%d/%y")
  ), 
  rand_dt = as.Date(rand_dt, origin = "1960-01-01")
)

old_baseline = 
  read_csv("data/baselinectn94.csv") |> 
  select(who, bamphetamine30_base, bcannabis30_base, bbenzo30_base) |> 
  mutate(across(everything(), factor))

# Defining race using racex
to_remove = c("raceg", "x2race", "x3race", "isHispanic", "i")
not_factors = c("who", "fusedt12", "fusedt24", "when", "rand_dt", "mg", "age", "hcows")

visits = 
  select(drugUse, -all_of(to_remove)) |> 
  mutate(across(!all_of(not_factors), factor)) |> 
  select(-bamphetamine30_base, -bcannabis30_base, -bbenzo30_base) |> 
  left_join(old_baseline, by = "who")

# Convert values coded as -8/-9/-1 to NA
visits = mutate(visits, 
  across(
    c(where(is.factor), "mg"), 
    \(x) na_if(na_if(x, "-9"), "-8")
  )
) |> droplevels()

# Refactor/relevel
binary = c("opioiduse12", "opioiduse24", "alcdisorder", "cocdisorder", "hasBrainDamage", 
           "hasEpilepsy", "hasSchiz", "hasBipolar", "hasAnxPan", "hasMajorDep", "falcohol", 
           "fdrug", "bamphetamine30_base", "bcannabis30_base", "bbenzo30_base", "ivdrug", 
           "selfopioid", "uopioid")

visits = mutate(visits, 
  across(all_of(binary), \(x) as.numeric(as.character(x))), 
  project = fct_relevel(project, c("27", "30", "51")), 
  sex = fct_recode(sex, "male" = "1", "female" = "2"),
  xrace = fct_relevel(xrace, c("1", "2", "3", "4")), 
  trt = fct_relevel(trt, c("1", "2", "3", "4", "5")),
  edu = fct_relevel(edu, c("1", "2", "3")),
  mar = fct_relevel(mar, c("1", "2", "3")), 
  medicine = fct_recode(medicine, "met" = "1", "bup" = "2", "nal" = "3"),
)

#---------------- Alicia note ------------------------------------#
# Some notes and conclusions from exploring the data (code not included):
# 
# * 28 patients never initiated treatment (missing `when`, only one entry for them)
# * Everyone got randomized
# * No one got randomized multiple times.
# * No one has a visit before their randomization date.
# * Almost all patients have their first visit on their randomization date.
# * However, 3 patients have first visits one day later. (`0403-06-1059, 0403-06-1060, 0605-06-2005`).
# * **Decision:** I'm shifting all dates for these patients one day earlier, 
#     as if their first visit happened on their randomization date.
# * No one has more than one visit on the same day.
# * 64 patients only had 1 visit (0 days between first and last visit, we know there are no 
#     repeat visits on the same day)
# * 1114 patients had exactly 174 days between their first and last visit (~6 months)
# * 174 was the maximum date range, so we know there are no outliers on the high or low end
# * All visit dates are always consecutive, except for 32 patients missing a couple days 
#     right before their last day
# * **Decision:** We'll create new rows for those days copying the `medicine` and `mg` 
#     from the previous day, and putting NA in the `selfopioid` and `uopioid` columns.
# 
# * 79 patients never had `medicine` or `mg > 0` recorded
# * 11 patients had a `medicine` recorded, but never `mg > 0`
# * **Decision:** People in the two cases above were marked as `never_initiated`.
# 
# * There are 44 patients whose medicine switched at some point. They were all in project 27, 
#     and all had only medicines 1 & 2.
# * I think if they only ever got `0 mg` of their "other med", then it was probably a mistake.
# * There are 12 people who seemed to genuinely switch meds (about half then switched back after a couple visits).
# * One person was assigned to `trt=1` and got bupenorphine the first day, and methadone every subsequent day.
#      It's not quite the same as "switched meds", but it also doesn't look like a mistake?
# * **Decision:**
#   * **I made a new column `switched_meds`, TRUE for those 13 patients identified above.**
#   * **I made another new column `med_switch_date` to capture the date of the first medication 
#     switch for those patients.**
#   * **I corrected what I thoguht were errors in the `medicine` column, according to the following rule:**
#   * **If there is one medicine they had marked, but never actually received any doseage of it
#       --> recode that medicine to be whatever the "majority" medicine was that that patient received
# * Only 23+191=214 people are in the "wrong" medicine according to trt.
# * All of these are the `switched_meds` patients, so I don't think there are any remaining 
#     errors in the `medicine` or `trt` columns
#-----------------------------------------------------------------#

visits = mutate(visits, 
  never_initiated = is.na(when), 
  when = as_date(if_else(never_initiated, ymd(rand_dt), ymd(when)))
)

# Add rows for missing visits
# The new rows are repeats of whatever `medicine` and `mg` were in the previous visit
# `selfopioid` and `uopioid` are both set to NA regardless of previous values
visits = left_join(
  map_dfr(split(visits, visits$who), function(x) {
    tibble(
      who = x$who[1],
      when = seq(min(x$when), max(x$when), by = "day")
    )
  }), visits
)

visits = group_by(visits, who) |> 
  mutate(across(!c("selfopioid", "uopioid"), zoo::na.locf, na.rm = FALSE)) |> 
  ungroup()

# Push dates back by one for the 3 patients who seem to be shifted 
#   according to their randomization dates
visits = 
  mutate(visits, when = case_when(
    who %in% c("0403-06-1059", "0403-06-1060", "0605-06-2005") ~ when - 1, 
    TRUE ~ when
  ))

# Add a new patient-level variable: `end_of_detox` signaling 3 weeks 
#     after their first visit, when the detoxification stage is over.
# Add new visit-level variables: day and week, counting up from the 
#     patient's first visit.
visits = 
  group_by(visits, who) |> 
  mutate(
    end_of_detox = min(when) + 21, 
    day_of_intervention = as.numeric(when - min(when) + 1), 
    week_of_intervention = ceiling(day_of_intervention / 7)
  )

# Mark anyone who never had `mg` recorded (including `mg` always 0) 
#   as `never_initiated.`
# The majority of these patients also never had `medicine` recorded.
visits = 
  group_by(visits, who) |> 
  mutate(
    never_initiated = if_else(sum(mg == 0 | is.na(mg)) == n(), TRUE, never_initiated[1])
  ) |> 
  ungroup()

# Swap medicine on the entries where mg is always blank for one 
#   medicine, likely in error.
visits = 
  group_by(visits, who) |> 
  mutate(
    double_meds = n_distinct(medicine, na.rm = TRUE) > 1,
    num_med_1 = sum(medicine == "met", na.rm = TRUE),
    num_med_2 = sum(medicine == "bup", na.rm = TRUE),
    num_med_1_blank = sum(medicine == "met" & !is.na(medicine) & (is.na(mg) | mg == 0)),
    num_med_2_blank = sum(medicine == "bup" & !is.na(medicine) & (is.na(mg) | mg == 0))
  ) |> 
  ungroup() |> 
  mutate(
    medicine = as.factor(
      case_when(
        medicine == "met" & double_meds & (num_med_1 == num_med_1_blank) ~ "bup",
        medicine == "bup" & double_meds & (num_med_2 == num_med_2_blank) ~ "met",
        TRUE ~ as.character(medicine)
      )
    )
  ) |> 
  select(-num_med_1, -num_med_2, -num_med_1_blank, -num_med_2_blank, -double_meds)

# Make a new column for those who switched meds at some point.
visits = group_by(visits, who) |> 
  mutate(switched_meds = n_distinct(medicine, na.rm = TRUE) > 1) |> 
  ungroup()

# Notes on dosages:
#   
# * Bupenorphine should go from 0 to 32, by 4s. Sometimes, it is also 
#     supplemented with 2mg naloxone 
#     (therefore a 6mg dosage is like 4mg bup, 2mg naloxone).
# * Methadone should go from 0 to max, by 10s.
# * Naltrexone was always 1 (this is an injection)
# * I also fill "medicine" and "mg" both up and down, to account for 
#     when this information was missing
visits = mutate(visits, 
  bup_dose = if_else(medicine == "bup", mg, NA_real_), 
  bup_dose = if_else(bup_dose > 32, 32, bup_dose),            # cap at 32 mg
  bup_dose = floor(bup_dose / 2) * 2,                         # round down to the nearest multiple of 2
  naloxone_dose = if_else(bup_dose %% 4 == 2, 2, 0),          # if it's still not a multiple of 4, mark 2mg of naloxone
  bup_dose = floor(bup_dose / 4) * 4,                         # then round down to the nearest multiple of 4
  met_dose = if_else(medicine == "met", mg, NA_real_),
  met_dose = floor(met_dose / 10) * 10,                       # round down to the nearest multiple of 10
  naltrexone_dose = if_else(medicine == "nal", mg, NA_real_)
)

visits = 
  group_by(visits, who) |> 
  fill(c("medicine", "bup_dose", "naloxone_dose", "naltrexone_dose", "met_dose"), 
       .direction = "downup") |> 
  ungroup() |> 
  mutate(
    any_dose = bup_dose > 0 | naloxone_dose > 0 | met_dose > 0 | naltrexone_dose > 0, 
    any_dose = replace_na(any_dose, FALSE)
  )

# Make new colums for: max_dose_this_week, any_dose_this_week, 
#   min_nonzero_dose_this_week, dose_change_during_week
visits = 
  group_by(visits, who, week_of_intervention) |> 
  mutate(
    max_dose_this_week = case_when(
      medicine == "bup" ~ max(bup_dose),
      medicine == "met" ~ max(met_dose),
      medicine == "nal" ~ max(naltrexone_dose),
      TRUE ~ 0
    ), 
    any_dose_this_week = max_dose_this_week > 0, 
    zero_dose_add_1000 = case_when(
      medicine == "bup" ~ as.numeric(ifelse(bup_dose == 0, 1000, bup_dose)),
      medicine == "met" ~ as.numeric(ifelse(met_dose == 0, 1000, met_dose)),
      medicine == "nal" ~ as.numeric(naltrexone_dose),
      TRUE ~ 0
    ), 
    min_nonzero_dose_this_week = if_else(
      any_dose_this_week,
      case_when(
        medicine == "bup" ~ min(zero_dose_add_1000),
        medicine == "met" ~ min(zero_dose_add_1000),
        medicine == "nal" ~ min(naltrexone_dose),
        TRUE ~ 0), 0
    ), 
    min_nonzero_dose_this_week = if_else(
      min_nonzero_dose_this_week == 1000, 0, 
      min_nonzero_dose_this_week
    ),
    dose_change_during_week = max_dose_this_week != min_nonzero_dose_this_week
  ) |> 
  ungroup() |> 
  select(-zero_dose_add_1000)

select(visits, -hcows, -edu, -mar, -falcohol, -fdrug, -mg) |> 
  saveRDS("data/drv/clean•visits.rds")
