library(lmtp)
library(glue)
library(kableExtra)

source("R/utils.R")
source("scripts/CopyOfalicia/04-lmtp-prep.R")

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

imputed = readRDS("data/fme/patients_imputed.rds")

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
  
  stack = sl3::make_learner(sl3::Lrnr_mean)
  
  lmtp_tmle(
    data, 
    A, Y, W, L, 
    shifted = shifted, 
    outcome_type = "survival", 
    folds = folds, 
    learners_outcome = stack, 
    learners_trt = stack
  )
}

iterate_fits = function(obs, shifted, subset) {
    x = subset(obs[[1]], medicine == subset)
    y = subset(shifted[[1]], medicine == subset)
    lmtp(x, 11, y, 1)
}

progressr::handlers(global = TRUE)

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

map_dfr(
  list(
     `BUP-NX` = fits$bup$constant$density_ratios != 0,
     `Methadone`= fits$met$constant$density_ratios != 0,
     `BUP-NX`= fits$bup$dynamic$density_ratios != 0 & increased[bup, ],
     `Methadone`= fits$met$dynamic$density_ratios != 0 & increased[met, ],
     `BUP-NX`= fits$bup$dynamic$density_ratios != 0,
     `Methadone`= fits$met$dynamic$density_ratios != 0,
     `BUP-NX`= fits$bup$threshold$density_ratios != 0 & increased[bup, ],
     `Methadone`= fits$met$threshold$density_ratios != 0 & increased[met, ],
     `BUP-NX`= fits$bup$threshold$density_ratios != 0,
     `Methadone`= fits$met$threshold$density_ratios != 0,
     `BUP-NX`= fits$bup$hybrid$density_ratios != 0 & increased[bup, ],
     `Methadone`= fits$met$hybrid$density_ratios != 0 & increased[met, ],
     `BUP-NX`= fits$bup$hybrid$density_ratios != 0,
     `Methadone`= fits$met$hybrid$density_ratios != 0
  ), strategy_n, .id = "Strategy"
) |>
  mutate(s = c(rep("A = a", 2), rep(c("A = 1", "A = 1", "A = a", "A = a"), 3))) |> 
  select(s, everything()) |> 
  kbl(
    # format = "latex", booktabs = TRUE,
    # caption = "Number of observed patients that followed a given strategy."
  ) |>
  kable_paper("striped", full_width = F) |> 
  pack_rows("Constant", 1, 2) |>
  pack_rows("Dynamic", 3, 6) |>
  pack_rows("Threshold", 7, 10) |>
  pack_rows("Hybrid", 11, 14)

h = c("", rep(2, times = 9))
names(h) = c("", "Wk. 3", 4:11)

map_dfr(
  list(
    `A = a` = fits$bup$constant$density_ratios != 0,
    `A = 1`= fits$bup$dynamic$density_ratios != 0 & increased[bup, ],
    `A = a`= fits$bup$dynamic$density_ratios != 0,
    `A = 1`= fits$bup$threshold$density_ratios != 0 & increased[bup, ],
    `A = a`= fits$bup$threshold$density_ratios != 0,
    `A = 1`= fits$bup$hybrid$density_ratios != 0 & increased[bup, ],
    `A = a`= fits$bup$hybrid$density_ratios != 0
  ), strategy_n, .id = "Strategy"
) |>
  bind_cols(
    map_dfr(
      list(
        `A = a` = fits$met$constant$density_ratios != 0,
        `A = 1`= fits$met$dynamic$density_ratios != 0 & increased[met, ],
        `A = a`= fits$met$dynamic$density_ratios != 0,
        `A = 1`= fits$met$threshold$density_ratios != 0 & increased[met, ],
        `A = a`= fits$met$threshold$density_ratios != 0,
        `A = 1`= fits$met$hybrid$density_ratios != 0 & increased[met, ],
        `A = a`= fits$met$hybrid$density_ratios != 0
      ), strategy_n
    )
  ) |>
  select(Strategy, 2, 11, 3, 12, 4, 13, 5, 14, 6, 15, 7, 16, 8, 17, 9, 18, 10, 19) |> 
  kbl(
    col.names = c("Strategy", rep(c("B.", "M."), times = 9)),
    format = "latex", booktabs = TRUE,
    caption = "Number of observed patients that followed a given strategy."
  ) |>
  kable_paper("striped", full_width = F) %>%
  add_header_above(h) %>%
  pack_rows("Constant", 1, 1) |>
  pack_rows("Dynamic", 2, 3) |>
  pack_rows("Threshold", 4, 5) |>
  pack_rows("Hybrid", 6, 7)

map_dfr(
  list(
    `Total` = fits$bup$constant$density_ratios != 0,
    `1`= fits$bup$dynamic$density_ratios != 0 & increased[bup, ],
    `0`= fits$bup$dynamic$density_ratios != 0  & !increased[bup, ],
    `Total` = fits$bup$dynamic$density_ratios != 0,
    `1`= fits$bup$threshold$density_ratios != 0 & increased[bup, ],
    `0`= fits$bup$threshold$density_ratios != 0  & !increased[bup, ],
    `Total` = fits$bup$threshold$density_ratios != 0,
    `1`= fits$bup$hybrid$density_ratios != 0 & increased[bup, ],
    `0`= fits$bup$hybrid$density_ratios != 0  & !increased[bup, ],
    `Total` = fits$bup$hybrid$density_ratios != 0
  ), strategy_n, .id = "subset"
) |> 
  kbl(
    format = "latex", booktabs = TRUE,
    caption = "Number of observed patients that were randomized to receive BUP-NX that followed a given strategy."
  ) |>
  kable_paper("striped", full_width = FALSE) |> 
  pack_rows("Constant", 1, 1) |>
  pack_rows("Dynamic", 2, 4) |>
  pack_rows("Threshold", 5, 7) |>
  pack_rows("Hybrid", 8, 10)
  
map_dfr(
  list(
    `Total` = fits$met$constant$density_ratios != 0,
    `1`= fits$met$dynamic$density_ratios != 0 & increased[met, ],
    `0`= fits$met$dynamic$density_ratios != 0  & !increased[met, ],
    `Total` = fits$met$dynamic$density_ratios != 0,
    `1`= fits$met$threshold$density_ratios != 0 & increased[met, ],
    `0`= fits$met$threshold$density_ratios != 0  & !increased[met, ],
    `Total` = fits$met$threshold$density_ratios != 0,
    `1`= fits$met$hybrid$density_ratios != 0 & increased[met, ],
    `0`= fits$met$hybrid$density_ratios != 0  & !increased[met, ],
    `Total` = fits$met$hybrid$density_ratios != 0
  ), strategy_n, .id = "subset"
) |> 
  kbl(
    format = "latex", booktabs = TRUE,
    caption = "Number of observed patients that were randomized to receive Methadone that followed a given strategy."
  ) |>
  kable_paper("striped", full_width = FALSE) |> 
  pack_rows("Constant", 1, 1) |>
  pack_rows("Dynamic", 2, 4) |>
  pack_rows("Threshold", 5, 7) |>
  pack_rows("Hybrid", 8, 10)

