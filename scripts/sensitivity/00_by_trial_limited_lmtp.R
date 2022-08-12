library(lmtp)
library(glue)
suppressPackageStartupMessages(library(tidyverse))

source("scripts/covariates.R")
source("R/SL.lightgbm.R")

visits_wide <- readRDS("data/drv/clean_weeks_with_relapse_wide_080922.rds")
imputed <- readRDS("data/drv/clean_patients_imputed_080922.rds")

# Create datasets under hypothetical strategies ---------------------------

# Was there previous time opioid use? 
condA <- visits_wide[, glue("wk{2:11}.use_this_week")] == 1

# Was dose under the allowable maximum? 
condB <- matrix(FALSE, dim(condA)[1], dim(condA)[2])
condB[visits_wide$medicine == "bup", ] <- 
  visits_wide[visits_wide$medicine == "bup", glue("wk{2:11}.dose_this_week")] < 32
condB[visits_wide$medicine == "met", ] <- 
  visits_wide[visits_wide$medicine == "met", glue("wk{2:11}.dose_this_week")] < 397

# Was dose under the dose threshold? 
condC <- matrix(FALSE, dim(condA)[1], dim(condA)[2])
condC[visits_wide$medicine == "bup", ] <- 
  visits_wide[visits_wide$medicine == "bup", glue("wk{2:11}.dose_this_week")] < 16
condC[visits_wide$medicine == "met", ] <- 
  visits_wide[visits_wide$medicine == "met", glue("wk{2:11}.dose_this_week")] < 100

A <- glue("wk{2:11}.dose_increase_this_week")

colnames(condA) <- A
colnames(condB) <- A
colnames(condC) <- A

dynamic <- visits_wide
dynamic[, A] <- apply(condA & condB, 2, function(x) as.numeric(x))

threshold <- visits_wide
threshold[, A] <- apply(condC, 2, function(x) as.numeric(x))

hybrid <- visits_wide
hybrid[, A] <- apply(condC | (condB & condA), 2, function(x) as.numeric(x))

constant <- lmtp:::shift_trt(visits_wide, A, static_binary_off)

# LMTP --------------------------------------------------------------------

node <- Sys.getenv("SGE_TASK_ID")
task_list <- expand.grid(imp = 1:5, 
                         med = c("bup"), 
                         project = c("27", "30", "51"),
                         tau = 2:11, 
                         strat = c("constant", "dynamic", "threshold", "hybrid"), 
                         stringsAsFactors = FALSE)

task <- task_list[node, ]
observed <- left_join(visits_wide, mice::complete(imputed, task$imp))
observed <- observed[as.character(observed$medicine) == task$med, ]
observed <- observed[as.character(observed$project) == task$project, ]

shifted <- list("dynamic" = dynamic, 
                "threshold" = threshold, 
                "hybrid" = hybrid, 
                "constant" = constant)[[task$strat]]
shifted <- left_join(shifted, mice::complete(imputed, task$imp))
shifted <- shifted[as.character(shifted$medicine) == task$med, ]
shifted <- shifted[as.character(shifted$project) == task$project, ]

if (task$tau <= 4) {
  A <- glue("wk{2:task$tau}.dose_increase_this_week")
  L <- lapply(2:task$tau, function(x) c(glue("wk{x-1}.dose_this_week"), glue("wk{x}.use_this_week")))
  Y <- glue("wk{3:(task$tau + 1)}.relapse_this_week")
} else {
  A <- glue("wk{2:4}.dose_increase_this_week")
  L <- lapply(2:4, function(x) c(glue("wk{x-1}.dose_this_week"), glue("wk{x}.use_this_week")))
  Y <- glue("wk{c(3:4, task$tau + 1)}.relapse_this_week")
}

W <- c(demog, comorbidities)

stack <- c("SL.glm", "SL.mean", "SL.lightgbm", "SL.earth")

seed <- round(runif(1, 1000, 1e5))
set.seed(seed)

fit <- lmtp_sdr(observed, 
                A, Y, W, L, 
                shifted = shifted, 
                outcome_type = ifelse(task$tau == 2, "binomial", "survival"), 
                folds = 1, 
                learners_outcome = stack, 
                learners_trt = stack)

saveRDS(list(seed = seed, fit = fit), 
        paste0("data/fits/by_trial_limited/", do.call(paste, c(task, sep = "_")), ".rds"))
