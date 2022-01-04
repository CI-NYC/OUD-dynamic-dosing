library(mice)
library(tidyverse)

source("R/utils.R")

patients = 
  readRDS("data/drv/clean•patients•with•relapse•010322.rds") |> 
  select(who, any_of(c(demog, treatment_info, comorbidities)), relapse_date)

setup = mice(patients, m = 1, maxit = 0)
methods = setup$method
predMatrix = setup$predictorMatrix

predMatrix[, c("who", "site", "trt", "end_of_detox")] = 
  apply(predMatrix[, c("who", "site", "trt", "end_of_detox")], 2, \(x) 0)

set.seed(46243)
imputed = mice(patients, method = methods, predictorMatrix = predMatrix, m = 5, maxit = 20)

saveRDS(imputed, "data/drv/clean•patients•imputed•010322.rds")
