library(lmtp)
library(glue)
library(tidyverse)

source("R/utils.R")
source("R/rubin.R")

visits_wide = readRDS("data/drv/clean•weeks•with•relapse•wide•010422.rds")

# Create datasets under hypothetical strategies ---------------------------

# Was there previous time opioid use? 
condA <- visits_wide[, glue("wk{3:11}.use_this_week")] == 1

# Was dose under the allowable maximum? 
condB <- matrix(FALSE, dim(condA)[1], dim(condA)[2])
condB[visits_wide$medicine == "bup", ] <- 
  visits_wide[visits_wide$medicine == "bup", glue("wk{3:11}.dose_this_week")] < 32
condB[visits_wide$medicine == "met", ] <- 
  visits_wide[visits_wide$medicine == "met", glue("wk{3:11}.dose_this_week")] < 397

# Was dose under the dose threshold? 
condC <- matrix(FALSE, dim(condA)[1], dim(condA)[2])
condC[visits_wide$medicine == "bup", ] <- 
  visits_wide[visits_wide$medicine == "bup", glue("wk{3:11}.dose_this_week")] < 16
condC[visits_wide$medicine == "met", ] <- 
  visits_wide[visits_wide$medicine == "met", glue("wk{3:11}.dose_this_week")] < 100

A = glue("wk{3:11}.dose_increase_this_week")

colnames(condA) <- A
colnames(condB) <- A
colnames(condC) <- A

dynamic = visits_wide
dynamic[, A] <- apply(condA & condB, 2, \(x) as.numeric(x), simplify = FALSE)

threshold = visits_wide
threshold[, A] = apply(condC, 2, \(x) as.numeric(x), simplify = FALSE)

hybrid = visits_wide
hybrid[, A] = apply(condC | (condB & condA), 2, \(x) as.numeric(x), simplify = FALSE)

constant = lmtp:::shift_trt(visits_wide, A, static_binary_off)

imputed = readRDS("data/drv/clean•patients•imputed•010422.rds")

observed  = map(1:5, \(x) left_join(visits_wide, mice::complete(imputed, x)))
dynamic   = map(1:5, \(x) left_join(dynamic, mice::complete(imputed, x)))
threshold = map(1:5, \(x) left_join(threshold, mice::complete(imputed, x)))
hybrid    = map(1:5, \(x) left_join(hybrid, mice::complete(imputed, x)))
constant  = map(1:5, \(x) left_join(constant, mice::complete(imputed, x)))

# LMTP --------------------------------------------------------------------

lmtp = function(data, tau, shifted, folds) {
  A = glue("wk{3:tau}.dose_increase_this_week")
  W = c(demog, comorbidities, "site")
  L = lapply(3:tau, \(x) c(glue("wk{x-1}.dose_this_week"), glue("wk{x}.use_this_week")))
  Y = glue("wk{4:(tau + 1)}.relapse_this_week")
  
  stack = sl3::make_learner_stack(
    sl3::Lrnr_glm, 
    sl3::Lrnr_mean, 
    sl3::Lrnr_xgboost, 
    sl3::Lrnr_earth
  )
  
  lmtp_sdr(
    data, 
    A, Y, W, L, 
    shifted = shifted, 
    outcome_type = "survival", 
    folds = folds, 
    learners_outcome = stack, 
    learners_trt = stack, 
    .SL_folds = 10
  )
}

iterate_fits = function(obs, shifted, subset) {
  lapply(1:5, function(i) {
    x = subset(obs[[i]], medicine == subset)
    y = subset(shifted[[i]], medicine == subset)
    lapply(3:11, \(tau) lmtp(x, tau, y, 1))
  })
}

progressr::handlers(global = TRUE)

set.seed(43634)

fits = list(
  bup = list(
    dynamic = iterate_fits(observed, dynamic, "bup"), 
    threshold = iterate_fits(observed, threshold, "bup"), 
    hybrid = iterate_fits(observed, hybrid, "bup"), 
    constant = iterate_fits(observed, constant, "bup")
  ), 
  met = list(
    dynamic = iterate_fits(observed, dynamic, "met"), 
    threshold = iterate_fits(observed, threshold, "met"), 
    hybrid = iterate_fits(observed, hybrid, "met"), 
    constant = iterate_fits(observed, constant, "met")
  )
)

saveRDS(fits, "data/drv/lmtp•fits•sdr•010422.rds")
