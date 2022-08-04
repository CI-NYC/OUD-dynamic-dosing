library(lmtp)
library(tidyverse)
library(glue)
library(kableExtra)

source("R/utils.R")

visits_wide = readRDS("data/drv/clean•weeks•with•relapse•wide•010422.rds")

A = glue("wk{3:11}.dose_increase_this_week")
Y = glue("wk{4:12}.relapse_this_week")

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

colnames(condA) <- A
colnames(condB) <- A
colnames(condC) <- A

dynamic = visits_wide
dynamic[, A] <- apply(condA & condB, 2, \(x) as.numeric(x), simplify = FALSE)

threshold = visits_wide
threshold[, A] = apply(condC, 2, \(x) as.numeric(x), simplify = FALSE)

hybrid = visits_wide
hybrid[, A] = apply(condC | (condB & condA), 2, \(x) as.numeric(x), simplify = FALSE)

constant = lmtp:::shift_trt(visits_wide, glue("wk{3:11}.dose_increase_this_week"), static_binary_off)

imputed = readRDS("data/drv/clean•patients•imputed•010422.rds")

observed = map(1:5, \(x) left_join(visits_wide, mice::complete(imputed, x)))
dynamic = map(1:5, \(x) left_join(dynamic, mice::complete(imputed, x)))
threshold = map(1:5, \(x) left_join(threshold, mice::complete(imputed, x)))
hybrid = map(1:5, \(x) left_join(hybrid, mice::complete(imputed, x)))
constant = map(1:5, \(x) left_join(constant, mice::complete(imputed, x)))

lmtp = function(data, tau, shifted, folds) {
  A = glue("wk{3:tau}.dose_increase_this_week")
  W = c(demog, comorbidities, "site")
  L = lapply(3:tau, \(x) c(glue("wk{x-1}.dose_this_week"), glue("wk{x}.use_this_week")))
  Y = glue("wk{4:(tau + 1)}.relapse_this_week")
  
  stack = sl3::make_learner_stack(sl3::Lrnr_mean, sl3::Lrnr_glm)
  
  lmtp_tmle(
    data, 
    A, Y, W, L, 
    shifted = shifted, 
    outcome_type = "survival", 
    folds = folds, 
    learners_outcome = stack, 
    learners_trt = stack, 
    .SL_folds = 2
  )
}

iterate_fits = function(obs, shifted, subset) {
    x = subset(obs[[1]], medicine == subset)
    y = subset(shifted[[1]], medicine == subset)
    lmtp(x, 11, y, 1)
}

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

# Counts for following a strategy
strategy_n = function(x) {
  colnames(x) = c("Wk. 3", 4:11)
  colSums(x, na.rm = TRUE) |>
    as_tibble(rownames = "time") |>
    pivot_wider(
      names_from = "time",
      values_from = "value"
    )
}

bup = visits_wide$medicine == "bup"
met = visits_wide$medicine == "met"
increased = visits_wide[, glue("wk{3:11}.dose_increase_this_week")] == 1

# prior week use decreases as time goes on
map(1:9, function(x) {
  visits_wide[bup, 
              ][(fits$bup$dynamic$density_ratios != 0)[, x], 
                ][[glue("wk{(3:11)[x]}.use_this_week")]] |> mean()
})

# patients under threshold decreases as time goes on
map(1:9, function(x) {
  (visits_wide[bup, 
              ][(fits$bup$threshold$density_ratios != 0)[, x], 
                ][[glue("wk{(2:10)[x]}.dose_this_week")]] < 16) |> mean()
})

map(1:9, function(x) {
  visits_wide[met, 
  ][(fits$met$dynamic$density_ratios != 0)[, x], 
  ][[glue("wk{(3:11)[x]}.use_this_week")]] |> mean()
})

map(1:9, function(x) {
  (visits_wide[met, 
  ][(fits$met$threshold$density_ratios != 0)[, x], 
  ][[glue("wk{(2:10)[x]}.dose_this_week")]] < 100) |> mean()
})

# produces LaTeX for counts of patients that would have increased with BUP-NX
map_dfr(
  list(
    `\\hspace{1em}$\\d1^a$` = 
      visits_wide[bup, glue("wk{4:12}.relapse_this_week")] == 0 & 
      dynamic[[1]][bup, glue("wk{3:11}.dose_increase_this_week")] == 1 & 
      fits$bup$constant$density_ratios != 0, 
    `\\hspace{1em}$\\d2^b$` = 
      visits_wide[bup, glue("wk{4:12}.relapse_this_week")] == 0 & 
      threshold[[1]][bup, glue("wk{3:11}.dose_increase_this_week")] == 1 & 
      fits$bup$constant$density_ratios != 0, 
    `\\hspace{1em}$\\d3^c$` = 
      visits_wide[bup, glue("wk{4:12}.relapse_this_week")] == 0 & 
      hybrid[[1]][bup, glue("wk{3:11}.dose_increase_this_week")] == 1 & 
      fits$bup$constant$density_ratios != 0
  ), strategy_n, .id = "strategy"
) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)

# produces LaTeX for counts of patients that would have increased with Met.
map_dfr(
  list(
    `\\hspace{1em}$\\d1^a$` = 
      visits_wide[met, glue("wk{4:12}.relapse_this_week")] == 0 & 
      dynamic[[1]][met, glue("wk{3:11}.dose_increase_this_week")] == 1 & 
      fits$met$constant$density_ratios != 0, 
    `\\hspace{1em}$\\d2^b$` = 
      visits_wide[met, glue("wk{4:12}.relapse_this_week")] == 0 & 
      threshold[[1]][met, glue("wk{3:11}.dose_increase_this_week")] == 1 & 
      fits$met$constant$density_ratios != 0, 
    `\\hspace{1em}$\\d3^c$` = 
      visits_wide[met, glue("wk{4:12}.relapse_this_week")] == 0 & 
      hybrid[[1]][met, glue("wk{3:11}.dose_increase_this_week")] == 1 & 
      fits$met$constant$density_ratios != 0
  ), strategy_n, .id = "strategy"
) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)

map_dfr(
  list(
    `\\emph{Total}` = fits$bup$constant$density_ratios != 0,
    `Increase`= fits$bup$dynamic$density_ratios != 0 & increased[bup, ],
    `Constant`= fits$bup$dynamic$density_ratios != 0 & !increased[bup, ],
    `\\emph{Total}` = fits$bup$dynamic$density_ratios != 0,
    `Increase`= fits$bup$threshold$density_ratios != 0 & increased[bup, ],
    `Constant`= fits$bup$threshold$density_ratios != 0 & !increased[bup, ],
    `\\emph{Total}` = fits$bup$threshold$density_ratios != 0,
    `Increase`= fits$bup$hybrid$density_ratios != 0 & increased[bup, ],
    `Constant`= fits$bup$hybrid$density_ratios != 0 & !increased[bup, ],
    `\\emph{Total}` = fits$bup$hybrid$density_ratios != 0
  ), strategy_n, .id = "subset"
) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE) |>
  pack_rows("Constant", 1, 1) |>
  pack_rows("Dynamic", 2, 4) |>
  pack_rows("Threshold", 5, 7) |>
  pack_rows("Hybrid", 8, 10)
   
map_dfr(
  list(
    `\\emph{Total}` = fits$met$constant$density_ratios != 0,
    `Increase`= fits$met$dynamic$density_ratios != 0 & increased[met, ],
    `Constant`= fits$met$dynamic$density_ratios != 0 & !increased[met, ],
    `\\emph{Total}` = fits$met$dynamic$density_ratios != 0,
    `Increase`= fits$met$threshold$density_ratios != 0 & increased[met, ],
    `Constant`= fits$met$threshold$density_ratios != 0 & !increased[met, ],
    `\\emph{Total}` = fits$met$threshold$density_ratios != 0,
    `Increase`= fits$met$hybrid$density_ratios != 0 & increased[met, ],
    `Constant`= fits$met$hybrid$density_ratios != 0 & !increased[met, ],
    `\\emph{Total}` = fits$met$hybrid$density_ratios != 0
  ), strategy_n, .id = "subset"
) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE) |>
  pack_rows("Constant", 1, 1) |>
  pack_rows("Dynamic", 2, 4) |>
  pack_rows("Threshold", 5, 7) |>
  pack_rows("Hybrid", 8, 10)

# were observed as increasing, but didn't increase under the strategy...
map_dfr(
  list(
    Dynamic = fits$bup$dynamic$density_ratios == 0 & 
      increased[bup, ] & 
      visits_wide[bup, glue("wk{4:12}.relapse_this_week")] == 0,
    Threshold = fits$bup$threshold$density_ratios == 0 & 
      increased[bup, ] & 
      visits_wide[bup, glue("wk{4:12}.relapse_this_week")] == 0,
    Hybrid = fits$bup$hybrid$density_ratios == 0 & 
      increased[bup, ] & 
      visits_wide[bup, glue("wk{4:12}.relapse_this_week")] == 0
  ), strategy_n, .id = "subset"
) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)

map_dfr(
  list(
    Dynamic = fits$met$dynamic$density_ratios == 0 & 
      increased[met, ] & 
      visits_wide[met, glue("wk{4:12}.relapse_this_week")] == 0,
    Threshold = fits$met$threshold$density_ratios == 0 & 
      increased[met, ] & 
      visits_wide[met, glue("wk{4:12}.relapse_this_week")] == 0,
    Hybrid = fits$met$hybrid$density_ratios == 0 & 
      increased[met, ] & 
      visits_wide[met, glue("wk{4:12}.relapse_this_week")] == 0
  ), strategy_n, .id = "subset"
) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)

strategy_mean = function(x) {
  colnames(x) = c("Wk. 3", 4:11)
  colMeans(x, na.rm = TRUE) |>
    as_tibble(rownames = "time") |>
    pivot_wider(
      names_from = "time",
      values_from = "value"
    )
}

map_dfr(
  list(
    `Prior week use` = visits_wide[bup, glue("wk{4:12}.relapse_this_week")] == 0 & 
      condA[bup, ],
    `Dose under threshold` = visits_wide[bup, glue("wk{4:12}.relapse_this_week")] == 0 & 
      condB[bup, ]
  ), strategy_mean, .id = "subset"
) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE, digits = 2)

map_dfr(
  list(
    `Prior week use` = (visits_wide[, glue("wk{4:12}.relapse_this_week")] == 0 & condA)[bup & proj27, ],
    `Dose under threshold` = (visits_wide[, glue("wk{4:12}.relapse_this_week")] == 0 & condB)[bup & proj27, ]
  ), strategy_mean, .id = "subset"
) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE, digits = 2)

proj27 = visits_wide$project == "27"
proj30 = visits_wide$project == "30"
proj51 = visits_wide$project == "51"

map_dfr(
  list(
    `Prior week use` = (visits_wide[, glue("wk{4:12}.relapse_this_week")] == 0 & condA)[bup & proj30, ],
    `Dose under threshold` = (visits_wide[, glue("wk{4:12}.relapse_this_week")] == 0 & condB)[bup & proj30, ]
  ), strategy_mean, .id = "subset"
) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE, digits = 2)

map_dfr(
  list(
    `Prior week use` = (visits_wide[, glue("wk{4:12}.relapse_this_week")] == 0 & condA)[bup & proj51, ],
    `Dose under threshold` = (visits_wide[, glue("wk{4:12}.relapse_this_week")] == 0 & condB)[bup & proj51, ]
  ), strategy_mean, .id = "subset"
) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE, digits = 2)

# metformin only comes from the one trial
map_dfr(
  list(
    `Prior week use` = visits_wide[met, glue("wk{4:12}.relapse_this_week")] == 0 & 
      condA[met, ],
    `Dose under threshold` = visits_wide[met, glue("wk{4:12}.relapse_this_week")] == 0 & 
      condB[met, ]
  ), strategy_mean, .id = "subset"
) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE, digits = 2)
