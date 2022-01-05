library(tidyverse)
library(lubridate)

source("R/utils.R")

visits = readRDS("data/drv/clean•visits•010422.rds")

# a positive urine test
# OR selfopioid is yes
# OR missing urine and self-report
# OR no study opioids (dose)
visits = mutate(
  visits,
  in_detox = when < end_of_detox,
  use_today =
    case_when(
      (!is.na(uopioid) & uopioid == 1) ~ TRUE, 
      (!is.na(selfopioid) & selfopioid == 1) ~ TRUE, 
      (is.na(uopioid) & is.na(selfopioid)) ~ TRUE, 
      ((!is.na(uopioid) & uopioid == 0) & 
         (!is.na(selfopioid) & selfopioid == 0)) ~ FALSE,
      (is.na(selfopioid) & uopioid == 0) ~ FALSE, 
      (is.na(uopioid) & selfopioid == 0) ~ FALSE, 
      TRUE ~ NA
    )
)

visits = 
  group_by(visits, who, week_of_intervention) |> 
  mutate(use_this_week = any(use_today)) |> 
  ungroup()

# Relapse was operationalized as occurring on the last day of 7 days of daily use
# of non-study opioids, or on the first day of the fourth consecutive week of at-least-once-weekly use
visits = 
  group_by(visits, who) |> 
  mutate(days_of_use = sequence(rle(use_today & !in_detox)$lengths) * 
           use_today * !in_detox, 
         last_observed = max(when)) |> 
  ungroup() |> 
  left_join(
    group_by(visits, who, week_of_intervention) |> 
      summarize(use_this_week = any(use_this_week), 
                in_detox = all(in_detox)) |> 
      mutate(weeks_of_use = sequence(rle(use_this_week & !in_detox)$lengths) * 
               use_this_week * !in_detox) |> 
      ungroup()
  ) |> 
  group_by(who) |> 
  mutate(relapse_overall = any(days_of_use == 7 | weeks_of_use == 4)) |> 
  ungroup()

relapse_dates = 
  split(visits, visits$who) |> 
  map_dfr(function(data) {
    # First 7th day of consecutive use
    out = with(data, when[detect_index(days_of_use == 7, \(x) x)])
    if (length(out) == 0) out = NA_Date_
    tibble(relapse_date_7day = out)
  }, .id = "who") |> 
  left_join({
    split(visits, visits$who) |> 
      map_dfr(function(data) {
        # First day of first fourth week of use
        out = with(data, when[detect_index(weeks_of_use == 4 & lag(weeks_of_use, 1) == 3, \(x) x)])
        if (length(out) == 0) out = NA_Date_
        tibble(relapse_date_4week = out)
      }, .id = "who")
  }) |> 
  mutate(relapse_date = case_when(
    is.na(relapse_date_7day) & is.na(relapse_date_4week) ~ NA_Date_, 
    is.na(relapse_date_7day) ~ relapse_date_4week, 
    is.na(relapse_date_4week) ~ relapse_date_7day,
    TRUE ~ pmin(relapse_date_7day, relapse_date_4week)
  ))

dates =
  left_join(
    select(distinct(visits, who, .keep_all = TRUE), 
      who, rand_dt, last_observed, end_of_detox, fusedt24),
    relapse_dates
  ) |> 
  mutate(
    relapse_date = if_else(
      between(as.numeric(relapse_date - fusedt24), 1, 28), 
      relapse_date, 
      fusedt24
    ), 
    relapse_date = if_else(
      is.na(relapse_date) & last_observed <= rand_dt + weeks(23), 
      pmax(end_of_detox, last_observed + 1), 
      relapse_date
    )
  )

saveRDS(dates, "data/drv/patient•dates•010422.rds")

visits = 
  left_join(visits, select(dates, who, relapse_date)) |> 
  group_by(who, week_of_intervention) |> 
  mutate(relapsed = as.numeric(!is.na(relapse_date) & relapse_date <= max(when)), 
         relapse_overall = !is.na(relapse_date)) |> 
  ungroup()

saveRDS(visits, "data/drv/clean•visits•with•relapse•010422.rds")

patients = 
  distinct(visits, who, .keep_all = TRUE) |> 
  select(who, any_of(c(demog, comorbidities, treatment_info, outcomes)))

saveRDS(patients, "data/drv/clean•patients•with•relapse•010422.rds")

weeks = 
  distinct(visits, who, week_of_intervention, .keep_all = TRUE) |> 
  select(who, any_of(c(demog, comorbidities, treatment_info, outcomes, weekly_indicators)))

saveRDS(weeks, "data/drv/clean•weeks•with•relapse•010422.rds")
