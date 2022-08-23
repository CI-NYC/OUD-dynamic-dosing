library(mice)
suppressPackageStartupMessages(library(tidyverse))

source("scripts/covariates.R")

patients <- readRDS("data/drv/clean_patients_with_relapse_080922.rds") |> 
  select(who, 
         any_of(c(demog, treatment_info, comorbidities)), 
         -switched_meds, -never_initiated)

setup <- mice(patients, m = 1, maxit = 0)
methods <- setup$method
predMatrix <- setup$predictorMatrix

predMatrix[, c("who", "site", "trt", "rand_dt", "end_of_detox")] <- 
  apply(predMatrix[, c("who", "site", "trt", "rand_dt", "end_of_detox")], 2, \(x) 0)

set.seed(46243)
imputed <- mice(patients, method = methods, predictorMatrix = predMatrix, m = 5, maxit = 20)

saveRDS(imputed, "data/drv/clean_patients_imputed_080922.rds")
