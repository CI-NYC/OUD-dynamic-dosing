#defining relapse outcomes
#include a couple different versions of outcome definitions


# relapse_this_week = ifelse(relapse_overall,
#                            relapse_date >= rand_dt + 7*(week_of_intervention - 1) & 
#                              relapse_date < rand_dt + 7*(week_of_intervention),
#                            FALSE)) %>% 
#   