suppressPackageStartupMessages(library(tidyverse))
library(stringr)
library(lubridate)

drug_use_raw <- read_csv("data/druguse_06062021.csv")
drug_use <- drug_use_raw

drug_use <- mutate(drug_use, 
                   across(c("fusedt12", "fusedt24", "when"), 
                          \(x) as.Date(x, format = "%m/%d/%y")), 
                   rand_dt = as.Date(rand_dt, origin = "1960-01-01"))

baseline <- read_csv("data/baselinectn94.csv") |> 
  select(who, bamphetamine30_base, bcannabis30_base, bbenzo30_base) |> 
  mutate(across(everything(), factor))

to_remove <- c("raceg", "x2race", "x3race", "isHispanic", "i")
not_factors <- c("who", "fusedt12", "fusedt24", "when", "rand_dt", "mg", "age", "hcows")

visits <- select(drug_use, -all_of(to_remove)) |> 
  mutate(across(!all_of(not_factors), factor)) |> 
  select(-bamphetamine30_base, -bcannabis30_base, -bbenzo30_base) |> 
  left_join(baseline, by = "who")

visits <- mutate(visits, 
                 across(c(where(is.factor), "mg"), 
                        \(x) na_if(na_if(x, "-9"), "-8"))) |> 
  droplevels()

binary <- c("opioiduse12", "opioiduse24", "alcdisorder", "cocdisorder", "hasBrainDamage", "hasEpilepsy", "hasSchiz", "hasBipolar", "hasAnxPan", "hasMajorDep", "falcohol", "fdrug", "bamphetamine30_base", "bcannabis30_base", "bbenzo30_base", "ivdrug", "selfopioid", "uopioid")

visits <- mutate(visits, 
                 across(all_of(binary), \(x) as.numeric(as.character(x))), 
                 project = fct_relevel(project, c("27", "30", "51")), 
                 sex = fct_recode(sex, "male" = "1", "female" = "2"),
                 xrace = fct_relevel(xrace, c("1", "2", "3", "4")), 
                 trt = fct_relevel(trt, c("1", "2", "3", "4", "5")),
                 edu = fct_relevel(edu, c("1", "2", "3")),
                 mar = fct_relevel(mar, c("1", "2", "3")), 
                 medicine = fct_recode(medicine, "met" = "1", "bup" = "2", "nal" = "3"))

visits <- mutate(visits, 
                 never_initiated =is.na(when), 
                 when = as_date(if_else(never_initiated, ymd(rand_dt), ymd(when))))

# Add rows for missing visits
# The new rows are repeats of whatever `medicine` and `mg` were in the previous visit
# `selfopioid` and `uopioid` are both set to NA regardless of previous values
visits <- left_join(map_dfr(split(visits, visits$who), 
                            function(x) {
                              tibble(who = x$who[1],
                                     when = seq(min(x$when), max(x$when), by = "day"))
                            }), 
                    visits)

visits <- mutate(visits, mg = na_if(mg, 0))

visits <- group_by(visits, who) |> 
  mutate(across(!c("selfopioid", "uopioid"), zoo::na.locf, na.rm = FALSE)) |> 
  ungroup()

# Push dates back by one for the 3 patients who seem to be shifted 
# according to their randomization dates
visits <- mutate(visits, 
                 when = case_when(
                   who %in% c("0403-06-1059", "0403-06-1060", "0605-06-2005") ~ when - 1, 
                   TRUE ~ when
                 ))

# Add a new patient-level variable: `end_of_detox` signaling 3 weeks 
# after their first visit, when the detoxification stage is over.
#
# Add new visit-level variables: day and week, counting up from the 
# patient's first visit.
visits <- group_by(visits, who) |> 
  mutate(end_of_detox = min(when) + 14, 
         day_of_intervention = as.numeric(when - min(when) + 1), 
         week_of_intervention = ceiling(day_of_intervention / 7))

# Mark anyone who never had `mg` recorded (including `mg` always 0) 
# as `never_initiated.`
# The majority of these patients also never had `medicine` recorded.
visits <- group_by(visits, who) |> 
  mutate(never_initiated = if_else(sum(mg == 0 | is.na(mg)) == n(), 
                                   TRUE, never_initiated[1])) |> 
  ungroup()

# Swap medicine on the entries where mg is always blank for one 
#   medicine, likely in error.
visits <- group_by(visits, who) |> 
  mutate(double_meds = n_distinct(medicine, na.rm = TRUE) > 1,
         num_med_1 = sum(medicine == "met", na.rm = TRUE),
         num_med_2 = sum(medicine == "bup", na.rm = TRUE),
         num_med_1_blank = sum(medicine == "met" & !is.na(medicine) & (is.na(mg) | mg == 0)),
         num_med_2_blank = sum(medicine == "bup" & !is.na(medicine) & (is.na(mg) | mg == 0))) |> 
  ungroup() |> 
  mutate(medicine = as.factor(
    case_when(
      medicine == "met" & double_meds & (num_med_1 == num_med_1_blank) ~ "bup",
      medicine == "bup" & double_meds & (num_med_2 == num_med_2_blank) ~ "met",
      TRUE ~ as.character(medicine)
    ))) |> 
  select(-num_med_1, -num_med_2, -num_med_1_blank, -num_med_2_blank, -double_meds)

# Making a new column for those who switched medicines at some point.
visits <- group_by(visits, who) |> 
  mutate(switched_meds = n_distinct(medicine, na.rm = TRUE) > 1) |> 
  ungroup()

# Removing those that switched medicines and never initiated treatment
visits <- filter(visits, !never_initiated, !switched_meds)

# Buprenorphine should go from 2 to 32, by 2s
# Fill "medicine" and "mg" both up and down, to account for when this information was missing
visits <- mutate(visits, 
                 bup_dose = if_else(medicine == "bup", mg, NA_real_), 
                 bup_dose = if_else(bup_dose > 32, 32, bup_dose), 
                 bup_dose = if_else(bup_dose == 1, 2, bup_dose),
                 bup_dose = floor(bup_dose / 2) * 2,               
                 met_dose = if_else(medicine == "met", mg, NA_real_),
                 naltrexone_dose = if_else(medicine == "nal", mg, NA_real_))

visits <- group_by(visits, who) |> 
  fill(c("medicine", "bup_dose", "naltrexone_dose", "met_dose"), 
       .direction = "downup") |> 
  ungroup() |> 
  group_by(who, week_of_intervention) |> 
  mutate(max_dose_this_week = case_when(medicine == "bup" ~ max(bup_dose),
                                        medicine == "met" ~ max(met_dose),
                                        medicine == "nal" ~ max(naltrexone_dose),
                                        TRUE ~ 0
  )) |> 
  ungroup()

select(visits, -hcows, -edu, -mar, -falcohol, -fdrug, -mg) |> 
  saveRDS("data/drv/clean_visits_080922.rds")
