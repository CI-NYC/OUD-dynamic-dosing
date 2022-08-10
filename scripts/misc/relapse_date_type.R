suppressPackageStartupMessages(library(tidyverse))

dates <- readRDS("data/drv/patient_dates_080922.rds")

source("scripts/04_lmtp_formatting.R")

dates <- left_join(select(visits_wide, who), dates)
rm(visits_wide)

dates <- split(visits, visits$who) |> 
  map_dfr(function(data) {
    out <- with(data, week_of_intervention[detect_index(relapse_this_week == 1, \(x) x)])
    tibble(relapse_week = out)
  }, .id = "who") |> 
  left_join(dates)

dates <- mutate(dates, 
                relapse_class = case_when(relapse_date == relapse_date_7day ~ "7 day", 
                                          relapse_date == relapse_date_4week ~ "4 week", 
                                          relapse_date == fusedt24 ~ "fused"))

table(dates$relapse_class)
