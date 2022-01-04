library(tidyverse)

visits = 
  readRDS("data/drv/clean•visits•with•relapse•010322.rds") |> 
  distinct(who, week_of_intervention, .keep_all = TRUE)

visits = 
  filter(
    visits, 
    !switched_meds & !never_initiated, 
    week_of_intervention <= 24
  ) |> 
  group_by(who, week_of_intervention) |> 
  mutate(dose_this_week = max_dose_this_week) |> 
  ungroup() |> 
  select(who, medicine, project, week_of_intervention, 
         use_this_week, dose_this_week, relapse_this_week = relapsed)

visits = 
  right_join(visits, expand(visits, who, week_of_intervention)) |> 
  arrange(who, week_of_intervention) |> 
  mutate(
    medicine = zoo::na.locf(medicine), 
    project = zoo::na.locf(project), 
    use_this_week = if_else(
      is.na(use_this_week) & (week_of_intervention %in% c(2, 3, 4)), 
      TRUE, use_this_week
    ), 
    dose_this_week = if_else(
      is.na(dose_this_week) & (week_of_intervention %in% c(2, 3, 4)), 
      0, dose_this_week
    ), 
    relapse_this_week = if_else(
      is.na(relapse_this_week) & (week_of_intervention %in% c(2, 3)), 
      0, relapse_this_week
    ), 
    relapse_this_week = if_else(
      is.na(relapse_this_week) & (week_of_intervention == 4), 
      1, relapse_this_week
    ), 
    relapse_this_week = replace_na(relapse_this_week, 1)
  )

visits = 
  mutate(
    visits, 
    dose_increase_this_week = dose_this_week > lag(dose_this_week, default = 0), 
    dose_increase_this_week = ifelse(relapse_this_week, FALSE, dose_increase_this_week),
    across(c("relapse_this_week", "use_this_week", "dose_increase_this_week"), as.numeric)
  )

visits_wide = pivot_wider(visits, 
  names_from = week_of_intervention, 
  names_glue = "wk{week_of_intervention}.{.value}", 
  values_from = c(
    dose_this_week, 
    dose_increase_this_week, 
    use_this_week, 
    relapse_this_week
  ), values_fill = NA
)

saveRDS(visits_wide, "data/drv/clean•weeks•with•relapse•wide•010322.rds")
