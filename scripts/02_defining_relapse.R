suppressPackageStartupMessages(library(tidyverse))
library(lubridate)

source("scripts/covariates.R")

visits <- readRDS("data/drv/clean_visits_080922.rds")

# a positive urine test
# OR selfopioid is yes
# OR missing urine and self-report
# OR no study opioids (dose)
visits <- mutate(visits,
                 in_detox = when < end_of_detox,
                 use_today =
                   case_when((!is.na(uopioid) & uopioid == 1) ~ TRUE, 
                             (!is.na(selfopioid) & selfopioid == 1) ~ TRUE, 
                             (is.na(uopioid) & is.na(selfopioid)) ~ TRUE, 
                             ((!is.na(uopioid) & uopioid == 0) & 
                                (!is.na(selfopioid) & selfopioid == 0)) ~ FALSE,
                             (is.na(selfopioid) & uopioid == 0) ~ FALSE, 
                             (is.na(uopioid) & selfopioid == 0) ~ FALSE, 
                             TRUE ~ NA))

visits <- group_by(visits, who, week_of_intervention) |> 
  mutate(use_this_week = any(use_today)) |> 
  ungroup()

# Relapse defined as occurring on the last day of 7 days of daily use of non-study opioids
# or on the first day of the fourth consecutive week of at-least-once-weekly use
visits <- group_by(visits, who) |> 
  mutate(days_of_use = sequence(rle(use_today & !in_detox)$lengths) * use_today * !in_detox, 
         last_observed = max(when)) |> 
  ungroup() |> 
  left_join({
    group_by(visits, who, week_of_intervention) |> 
      summarize(use_this_week = any(use_this_week), 
                in_detox = all(in_detox)) |> 
      mutate(weeks_of_use = sequence(rle(use_this_week & !in_detox)$lengths) * use_this_week * !in_detox) |> 
      ungroup()
  }) |> 
  group_by(who) |> 
  mutate(relapse_overall = any(days_of_use == 7 | weeks_of_use == 4)) |> 
  ungroup()

# Last day of 7 days of use
who_7_day <- split(visits, visits$who) |> 
  map_dfr(function(data) {
    out = with(data, when[detect_index(days_of_use == 7, \(x) x)])
    if (length(out) == 0) out = NA_Date_
    tibble(relapse_date_7day = out)
  }, .id = "who")

# First day of first fourth week of use
who_4_week <- split(visits, visits$who) |> 
  map_dfr(function(data) {
    out = with(data, when[detect_index(weeks_of_use == 4 & lag(weeks_of_use, 1) == 3, \(x) x)])
    if (length(out) == 0) out = NA_Date_
    tibble(relapse_date_4week = out)
  }, .id = "who")

relapse_dates <- who_7_day |> 
  left_join(who_4_week) |> 
  left_join(unique(select(visits, who, fusedt24))) |> 
  mutate(relapse_date = case_when(is.na(relapse_date_7day) & is.na(relapse_date_4week) ~ fusedt24, 
                                  is.na(relapse_date_7day) ~ relapse_date_4week, 
                                  is.na(relapse_date_4week) ~ relapse_date_7day,
                                  TRUE ~ pmin(relapse_date_7day, relapse_date_4week)))

relapse_dates <- mutate(relapse_dates, 
                        relapse_date = if_else(
                          between(abs(as.numeric(relapse_date - fusedt24)), 1, 28), 
                          relapse_date, 
                          fusedt24
                        ))

dates <- select(visits, who, rand_dt, last_observed, end_of_detox) |> 
  distinct() |> 
  left_join(relapse_dates) |> 
  mutate(relapse_date = if_else(is.na(relapse_date) & last_observed <= rand_dt + weeks(23), 
                                pmax(end_of_detox, last_observed + 1), 
                                relapse_date))

saveRDS(dates, "data/drv/patient_dates_080922.rds")

visits <- left_join(visits, select(dates, who, relapse_date)) |> 
  group_by(who, week_of_intervention) |> 
  mutate(relapse = as.numeric(relapse_date <= max(when))) |> 
  ungroup()

saveRDS(visits, "data/drv/clean_visits_with_relapse_080922.rds")

patients <- select(visits, who, any_of(c(demog, comorbidities, treatment_info, outcomes))) |> 
  distinct(who, .keep_all = TRUE)

saveRDS(patients, "data/drv/clean_patients_with_relapse_080922.rds")

weeks <- select(visits, 
                who, any_of(c(demog, comorbidities, treatment_info, outcomes, weekly_indicators))) |> 
  distinct(who, week_of_intervention, .keep_all = TRUE)

saveRDS(weeks, "data/drv/clean_weeks_with_relapse_080922.rds")
