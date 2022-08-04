library(tidyverse)

source("R/utils.R")
source("scripts/04•lmtp•formatting.R")

by_day = readRDS("data/drv/clean•visits•with•relapse•010422.rds") |>
  select(who, project, day_of_intervention, week_of_intervention, medicine, bup_dose, met_dose)

before_relapse = filter(visits, 
  relapse_this_week == 0, 
  week_of_intervention >= 3, 
  week_of_intervention < 12
)

by_day = inner_join(
  by_day, 
  select(before_relapse, who, week_of_intervention), 
  by = c("who", "week_of_intervention")
)

group_by(by_day, who, project)

filter(by_day, medicine == "bup") |> 
  (\(x) summary(x$bup_dose))()

filter(by_day, medicine == "bup") |> 
  (\(x) split(x, x$project))() |> 
  map(\(x) summarize(group_by(x, who), max_dose = max(bup_dose))) |>
  map(\(x) summary(x$max_dose))
  
filter(by_day, medicine == "met") |> 
  group_by(who) |> 
  summarize(max_dose = max(met_dose)) |> 
  (\(x) summary(x$max_dose))()
