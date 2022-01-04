library(tidyverse)

source("R/utils.R")
source("scripts/04•lmtp•formatting.R")

by_day = readRDS("data/drv/clean•visits•with•relapse•010322.rds") |> 
  select(who, day_of_intervention, week_of_intervention, medicine, bup_dose, met_dose)

before_relapse = filter(visits, 
  relapse_this_week == 0, 
  week_of_intervention >= 2, 
  week_of_intervention < 12
)

by_day = inner_join(
  by_day, 
  select(before_relapse, who, week_of_intervention), 
  by = c("who", "week_of_intervention")
)

filter(by_day, medicine == "bup") |> 
  group_by(who) |> 
  summarise(max_dose = max(bup_dose)) |> 
  summarise(median(max_dose), quantile(max_dose, 0.25), quantile(max_dose, 0.75))
  
filter(by_day, medicine == "met") |> 
  group_by(who) |> 
  summarise(max_dose = max(met_dose)) |> 
  (\(x) summary(x$max_dose))()
