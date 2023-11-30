# READ ME ----------------------------------------------------------------------
#
#       Author: Amy Pitts
# Last updated: 23 Oct 2023
#
#         Note: Author: Sarah Forrest 
#               Modified code originally written here:
#               https://github.com/CI-NYC/harmonize_0055_0067/scripts/relapse_harmonize.qmd
# 
# ------------------------------------------------------------------------------

# Set up -----------------------------------------------------------------------

# Load packages
library(tidyverse)
library(lubridate)


# Read in data files -----------------------------------------------------------

# Load opioid usage data created in: https://github.com/CI-NYC/relapse_0055_0067/tree/main/scripts
prim <- readRDS(here::here("ctn_0094_0055_0067/data/ctn0067/drv/randomization_and_induction.rds")) 
uds <- readRDS(here::here("ctn_0094_0055_0067/data/ctn0067/drv/urine_opioids.rds")) 
tl <- readRDS(here::here("ctn_0094_0055_0067/data/ctn0067/drv/timeline_followback_opioids.rds"))


# Create end_of_detox_variable
tl <- group_by(tl, patid) |>
  mutate(end_of_detox = randomization_date + 14) |>
  ungroup()

# Fill in missing dates in long format dataset
use <-
  left_join(prim, select(tl, -randomization_date), multiple = "all") |>
  group_by(patid) |>
  mutate(date = if_else(!is.na(date), date, randomization_date + days(days_since_randomization))) |>
  fill(end_of_detox, .direction = "downup") |>
  mutate(in_detox = date <= end_of_detox) |>
  ungroup()


prim2 <- prim %>% mutate(
  prescribed_bup = ifelse(trt == "TAU", 1,0),
  prescribed_methadone = 0
)
# If the urine visit is missing, consider a positive
# If any detectable opioid other than buprenorphine or methadone, consider a positive
# If no prior methadone prescription and detectable methadone, consider a positive
# If no prior buprenorphine prescription and detectable buprenorphine, consider a positive
# If prior methadone prescription and detectable methadone, consider a negative
# If prior buprenorphine prescription and detectable buprenorphine, consider a negative
urine <- select(prim2, patid, prescribed_methadone, prescribed_bup) |>
  left_join(select(uds,
                   patid,
                   visit,
                   urine_date,
                   urine_methadone,
                   urine_buprenorphine,
                   urine_opiates_2000_300_or_oxycodone_or_fentanyl),
            multiple = "all") |>
  mutate(urine_opioid = case_when(
    is.na(urine_date) ~ TRUE,
    urine_opiates_2000_300_or_oxycodone_or_fentanyl == 1 ~ TRUE,
    !prescribed_methadone & urine_methadone == 1 ~ TRUE,
    !prescribed_bup & urine_buprenorphine == 1 ~ TRUE,
    prescribed_methadone & urine_methadone == 1 ~ FALSE,
    prescribed_bup & urine_buprenorphine == 1 ~ FALSE,
    urine_methadone == 0 & urine_buprenorphine == 0 &
      urine_opiates_2000_300_or_oxycodone_or_fentanyl == 0 ~ FALSE
  )) |>
  select(patid, date = urine_date, urine_opioid) |>
  filter(!is.na(date))


## find who missed a visit 
urine2 <- select(prim2, randomization_date, patid, prescribed_methadone) |>
  left_join(select(uds,
                   patid,
                   visit,
                   urine_date,
                   urine_methadone,
                   urine_buprenorphine,
                   urine_opiates_2000_300_or_oxycodone_or_fentanyl),
            multiple = "all") %>%
  mutate(
    urine_missed = case_when(
    !is.na(urine_date) ~ TRUE
  ),
  date_since_random = urine_date - randomization_date 
  ) %>% 
  mutate(
    visit_missed = ifelse(is.na(urine_missed), visit, NA)
  )


# use_today = 1 if positive or refused urine test OR self reported opioid use is yes
use <- left_join(use, urine) |>
  mutate(use_today = case_when(
    !is.na(urine_opioid) & urine_opioid ~ TRUE,
    !is.na(selfreport_opioid_today) & selfreport_opioid_today ~ TRUE,
    (!is.na(urine_opioid) & !urine_opioid) &
      (!is.na(selfreport_opioid_today) & !selfreport_opioid_today) ~ FALSE,
    is.na(selfreport_opioid_today) & !urine_opioid ~ FALSE,
    is.na(urine_opioid) & !selfreport_opioid_today ~ FALSE,
    TRUE ~ NA
  ))

use <- use |> 
  group_by(patid, week) |>
  mutate(use_this_week = any(use_today))


# Create dropout variable for if a participant missed their 4-week visit or more
use <- use |>
  mutate(
    was_observed = case_when(
      #days_since_randomization %in% c(1:168) & 
        is.na(urine_opioid) & is.na(selfreport_opioid_today) ~ FALSE,
      TRUE ~ TRUE),
    obs_period = case_when(
      days_since_randomization %in% c(1:14) ~ "00I",
      days_since_randomization %in% c(22:42) ~ "01",
      days_since_randomization %in% c(50:70) ~ "04",
      days_since_randomization %in% c(78:98) ~ "08",
      days_since_randomization %in% c(106:129) ~ "12",
      days_since_randomization %in% c(134:154) ~ "16",
      TRUE ~ NA
    )
  )

id_missing_obs <- urine2 %>% group_by(patid) %>%
  filter(visit != "00I") %>% 
  summarize(
    missing_obs = sum(is.na(urine_date))
  ) %>% filter(missing_obs > 0) %>% pull(patid)




use <- use |> 
  group_by(patid, week) |>
  mutate( was_observed_this_week = any(was_observed)) |>
  ungroup() |>
  group_by(patid, obs_period) |>
  mutate(was_observed_obs_period = any(was_observed)) |>
  ungroup() |>
  mutate(
    dropout_date = case_when(
      was_observed_obs_period == "00I" & was_observed_obs_period == F ~ NA,
      patid %in% id_missing_obs & !is.na(was_observed_obs_period) & was_observed_obs_period == F ~ date + 22,
      TRUE ~ NA
    )
  )

use <- group_by(use, patid) |>
  mutate(days_of_use = sequence(rle(use_today & !in_detox)$lengths) * use_today * !in_detox) |>
  ungroup() |>
  left_join({
    group_by(use, patid, week) |>
      summarize(use_this_week = any(use_this_week),
                in_detox = all(in_detox)) |>
      mutate(weeks_of_use = sequence(rle(use_this_week & !in_detox)$lengths) * use_this_week * !in_detox) |>
      ungroup()
  }) |>
  select(patid, trt, induction, induction_date, randomization_date,
         date, days_since_randomization, week, end_of_detox, in_detox,
         selfreport_opioid_today, urine_opioid, use_today, days_of_use, obs_period,
         was_observed, dropout_date)

use <- use %>% 
  mutate(days_of_use = ifelse(is.na(days_of_use), 0, days_of_use))

# Last day of 7 days of use
patid_7_day <- split(use, use$patid) |>
  map_dfr(function(data) {
    out = with(data, date[detect_index(days_of_use == 7, \(x) x)])
    if (length(out) == 0) out = NA_Date_
    tibble(relapse_date_7day = out)
  }, .id = "patid")


# First day of the fourth week visit missed (dropout)
patid_4_week <- use %>%
  group_by(patid) %>%
  summarize(relapse_date_4week = if (any(!is.na(dropout_date))) max(dropout_date, na.rm = TRUE) else NA_Date_) %>%
  ungroup()

relapse_dates <- left_join(patid_7_day, patid_4_week) |>
  mutate(relapse_date = case_when(
    is.na(relapse_date_7day) & is.na(relapse_date_4week) ~ NA_Date_,
    is.na(relapse_date_7day) ~ relapse_date_4week,
    is.na(relapse_date_4week) ~ relapse_date_7day,
    TRUE ~ pmin(relapse_date_7day, relapse_date_4week)
  ))

relapse <- left_join(use, select(relapse_dates, patid, relapse_date)) |>
  group_by(patid, week) |>
  mutate(relapse = as.numeric(relapse_date <= max(date))) |>
  ungroup() |>
  select(patid, trt, randomization_date, week, relapse, relapse_date) |>
  unique() |>
  mutate(relapse = replace_na(relapse, 0), .by = "patid")

# Save
#saveRDS(relapse, "ctn_0094_0055_0067/data/ctn0067/drv/relapse_nov13.rds")

