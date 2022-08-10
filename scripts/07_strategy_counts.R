library(lmtp)
suppressPackageStartupMessages(library(tidyverse))
library(glue)
library(kableExtra)

source("scripts/covariates.R")

visits_wide <- readRDS("data/drv/clean_weeks_with_relapse_wide_080922.rds")

fits <- list()
for (med in c("met", "bup")) {
  fits[[med]] <- list()
  for (strat in c("constant", "dynamic", "threshold", "hybrid")) {
    fits[[med]][[strat]] <- readRDS(glue("data/fits/1_{med}_11_{strat}.rds"))$fit
  }
}

# Counts for following a strategy
strategy_n = function(x) {
  colnames(x) = c("Wk. 2", 3:11)
  colSums(x, na.rm = TRUE) |>
    as_tibble(rownames = "time") |>
    pivot_wider(
      names_from = "time",
      values_from = "value"
    )
}

bup <- visits_wide$medicine == "bup"
met <- visits_wide$medicine == "met"
increased <- visits_wide[, glue("wk{2:11}.dose_increase_this_week")] == 1

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

# Table 2 -----------------------------------------------------------------

# produces LaTeX for counts of patients that would have increased with BUP-NX
map_dfr(
  list(
    `\\hspace{1em}$\\d1^a$` = 
      visits_wide[bup, glue("wk{3:12}.relapse_this_week")] == 0 & 
      dynamic[bup, glue("wk{2:11}.dose_increase_this_week")] == 1 & 
      fits$bup$constant$density_ratios != 0, 
    `\\hspace{1em}$\\d2^b$` = 
      visits_wide[bup, glue("wk{3:12}.relapse_this_week")] == 0 & 
      threshold[bup, glue("wk{2:11}.dose_increase_this_week")] == 1 & 
      fits$bup$constant$density_ratios != 0, 
    `\\hspace{1em}$\\d3^c$` = 
      visits_wide[bup, glue("wk{3:12}.relapse_this_week")] == 0 & 
      hybrid[bup, glue("wk{2:11}.dose_increase_this_week")] == 1 & 
      fits$bup$constant$density_ratios != 0
  ), strategy_n, .id = "strategy"
) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)

# produces LaTeX for counts of patients that would have increased with Met.
map_dfr(
  list(
    `\\hspace{1em}$\\d1^a$` = 
      visits_wide[met, glue("wk{3:12}.relapse_this_week")] == 0 & 
      dynamic[met, glue("wk{2:11}.dose_increase_this_week")] == 1 & 
      fits$met$constant$density_ratios != 0, 
    `\\hspace{1em}$\\d2^b$` = 
      visits_wide[met, glue("wk{3:12}.relapse_this_week")] == 0 & 
      threshold[met, glue("wk{2:11}.dose_increase_this_week")] == 1 & 
      fits$met$constant$density_ratios != 0, 
    `\\hspace{1em}$\\d3^c$` = 
      visits_wide[met, glue("wk{3:12}.relapse_this_week")] == 0 & 
      hybrid[met, glue("wk{2:11}.dose_increase_this_week")] == 1 & 
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

# Appendix tables ---------------------------------------------------------

# table A3
# were observed as increasing, but didn't increase under the strategy...
map_dfr(
  list(
    Dynamic = fits$bup$dynamic$density_ratios == 0 & 
      increased[bup, ] & 
      visits_wide[bup, glue("wk{3:12}.relapse_this_week")] == 0,
    Threshold = fits$bup$threshold$density_ratios == 0 & 
      increased[bup, ] & 
      visits_wide[bup, glue("wk{3:12}.relapse_this_week")] == 0,
    Hybrid = fits$bup$hybrid$density_ratios == 0 & 
      increased[bup, ] & 
      visits_wide[bup, glue("wk{3:12}.relapse_this_week")] == 0
  ), strategy_n, .id = "subset"
) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)

map_dfr(
  list(
    Dynamic = fits$met$dynamic$density_ratios == 0 & 
      increased[met, ] & 
      visits_wide[met, glue("wk{3:12}.relapse_this_week")] == 0,
    Threshold = fits$met$threshold$density_ratios == 0 & 
      increased[met, ] & 
      visits_wide[met, glue("wk{3:12}.relapse_this_week")] == 0,
    Hybrid = fits$met$hybrid$density_ratios == 0 & 
      increased[met, ] & 
      visits_wide[met, glue("wk{3:12}.relapse_this_week")] == 0
  ), strategy_n, .id = "subset"
) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)

# table A4
strategy_mean = function(x) {
  colnames(x) = c("Wk. 2", 3:11)
  colMeans(x, na.rm = TRUE) |>
    as_tibble(rownames = "time") |>
    pivot_wider(
      names_from = "time",
      values_from = "value"
    )
}

proj27 <- visits_wide$project == "27"
proj30 <- visits_wide$project == "30"
proj51 <- visits_wide$project == "51"

map_dfr(
  list(
    `Prior week use` = visits_wide[bup, glue("wk{3:12}.relapse_this_week")] == 0 & 
      condA[bup, ],
    `Dose under threshold` = visits_wide[bup, glue("wk{3:12}.relapse_this_week")] == 0 & 
      condB[bup, ]
  ), strategy_mean, .id = "subset"
) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE, digits = 2)

map_dfr(
  list(
    `Prior week use` = (visits_wide[, glue("wk{3:12}.relapse_this_week")] == 0 & condA)[bup & proj27, ],
    `Dose under threshold` = (visits_wide[, glue("wk{3:12}.relapse_this_week")] == 0 & condB)[bup & proj27, ]
  ), strategy_mean, .id = "subset"
) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE, digits = 2)

map_dfr(
  list(
    `Prior week use` = (visits_wide[, glue("wk{3:12}.relapse_this_week")] == 0 & condA)[bup & proj30, ],
    `Dose under threshold` = (visits_wide[, glue("wk{3:12}.relapse_this_week")] == 0 & condB)[bup & proj30, ]
  ), strategy_mean, .id = "subset"
) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE, digits = 2)

map_dfr(
  list(
    `Prior week use` = (visits_wide[, glue("wk{3:12}.relapse_this_week")] == 0 & condA)[bup & proj51, ],
    `Dose under threshold` = (visits_wide[, glue("wk{3:12}.relapse_this_week")] == 0 & condB)[bup & proj51, ]
  ), strategy_mean, .id = "subset"
) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE, digits = 2)

# methadone only comes from the one trial
map_dfr(
  list(
    `Prior week use` = visits_wide[met, glue("wk{3:12}.relapse_this_week")] == 0 & 
      condA[met, ],
    `Dose under threshold` = visits_wide[met, glue("wk{3:12}.relapse_this_week")] == 0 & 
      condB[met, ]
  ), strategy_mean, .id = "subset"
) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE, digits = 2)

# table A5, density ratios
map(fits$bup, \(x) x$density_ratios) |> 
  map(\(x) apply(x, 2, max)) |> 
  map(\(x) setNames(x, c("Wk. 2", 3:11))) |> 
  map_dfr(\(x) as_tibble_row(x), .id = "strategy") |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE, digits = 2)

map(fits$met, \(x) x$density_ratios) |> 
  map(\(x) apply(x, 2, max)) |> 
  map(\(x) setNames(x, c("Wk. 2", 3:11))) |> 
  map_dfr(\(x) as_tibble_row(x), .id = "strategy") |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE, digits = 2)

# Table A6, table 2 stratified by project
# produces LaTeX for counts of patients that would have increased with BUP-NX

# ctn 27
map_dfr(
  list(
    `\\hspace{1em}$\\d1^a$` = 
      (visits_wide[, glue("wk{3:12}.relapse_this_week")] == 0 & 
         dynamic[, glue("wk{2:11}.dose_increase_this_week")] == 1)[bup & proj27, ] & 
      (fits$bup$constant$density_ratios != 0)[proj27[bup], ], 
    `\\hspace{1em}$\\d2^b$` = 
      (visits_wide[, glue("wk{3:12}.relapse_this_week")] == 0 & 
         threshold[, glue("wk{2:11}.dose_increase_this_week")] == 1)[bup & proj27, ] & 
      (fits$bup$constant$density_ratios != 0)[proj27[bup], ], 
    `\\hspace{1em}$\\d3^c$` = 
      (visits_wide[, glue("wk{3:12}.relapse_this_week")] == 0 & 
         hybrid[, glue("wk{2:11}.dose_increase_this_week")] == 1)[bup & proj27, ] & 
      (fits$bup$constant$density_ratios != 0)[proj27[bup], ]
  ), strategy_n, .id = "strategy"
) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)

# ctn 30
map_dfr(
  list(
    `\\hspace{1em}$\\d1^a$` = 
      (visits_wide[, glue("wk{3:12}.relapse_this_week")] == 0 & 
         dynamic[, glue("wk{2:11}.dose_increase_this_week")] == 1)[bup & proj30, ] & 
      (fits$bup$constant$density_ratios != 0)[proj30[bup], ], 
    `\\hspace{1em}$\\d2^b$` = 
      (visits_wide[, glue("wk{3:12}.relapse_this_week")] == 0 & 
         threshold[, glue("wk{2:11}.dose_increase_this_week")] == 1)[bup & proj30, ] & 
      (fits$bup$constant$density_ratios != 0)[proj30[bup], ], 
    `\\hspace{1em}$\\d3^c$` = 
      (visits_wide[, glue("wk{3:12}.relapse_this_week")] == 0 & 
         hybrid[, glue("wk{2:11}.dose_increase_this_week")] == 1)[bup & proj30, ] & 
      (fits$bup$constant$density_ratios != 0)[proj30[bup], ]
  ), strategy_n, .id = "strategy"
) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)

# ctn 51
map_dfr(
  list(
    `\\hspace{1em}$\\d1^a$` = 
      (visits_wide[, glue("wk{3:12}.relapse_this_week")] == 0 & 
         dynamic[, glue("wk{2:11}.dose_increase_this_week")] == 1)[bup & proj51, ] & 
      (fits$bup$constant$density_ratios != 0)[proj51[bup], ], 
    `\\hspace{1em}$\\d2^b$` = 
      (visits_wide[, glue("wk{3:12}.relapse_this_week")] == 0 & 
         threshold[, glue("wk{2:11}.dose_increase_this_week")] == 1)[bup & proj51, ] & 
      (fits$bup$constant$density_ratios != 0)[proj51[bup], ], 
    `\\hspace{1em}$\\d3^c$` = 
      (visits_wide[, glue("wk{3:12}.relapse_this_week")] == 0 & 
         hybrid[, glue("wk{2:11}.dose_increase_this_week")] == 1)[bup & proj51, ] & 
      (fits$bup$constant$density_ratios != 0)[proj51[bup], ]
  ), strategy_n, .id = "strategy"
) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)

# ctn 27
map_dfr(
  list(
    `\\emph{Total}` = (fits$bup$constant$density_ratios != 0)[proj27[bup], ],
    `Increase`= (fits$bup$dynamic$density_ratios != 0)[proj27[bup], ] & increased[bup & proj27, ],
    `Constant`= (fits$bup$dynamic$density_ratios != 0)[proj27[bup], ] & !increased[bup & proj27, ],
    `\\emph{Total}` = (fits$bup$dynamic$density_ratios != 0)[proj27[bup], ],
    `Increase`= (fits$bup$threshold$density_ratios != 0)[proj27[bup], ] & increased[bup & proj27, ],
    `Constant`= (fits$bup$threshold$density_ratios != 0)[proj27[bup], ] & !increased[bup & proj27, ],
    `\\emph{Total}` = (fits$bup$threshold$density_ratios != 0)[proj27[bup], ],
    `Increase`= (fits$bup$hybrid$density_ratios != 0)[proj27[bup], ] & increased[bup & proj27, ],
    `Constant`= (fits$bup$hybrid$density_ratios != 0)[proj27[bup], ] & !increased[bup & proj27, ],
    `\\emph{Total}` = (fits$bup$hybrid$density_ratios != 0)[proj27[bup], ]
  ), strategy_n, .id = "subset"
) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE) |>
  pack_rows("Constant", 1, 1) |>
  pack_rows("Dynamic", 2, 4) |>
  pack_rows("Threshold", 5, 7) |>
  pack_rows("Hybrid", 8, 10)

map_dfr(
  list(
    `\\emph{Total}` = (fits$bup$constant$density_ratios != 0)[proj30[bup], ],
    `Increase`= (fits$bup$dynamic$density_ratios != 0)[proj30[bup], ] & increased[bup & proj30, ],
    `Constant`= (fits$bup$dynamic$density_ratios != 0)[proj30[bup], ] & !increased[bup & proj30, ],
    `\\emph{Total}` = (fits$bup$dynamic$density_ratios != 0)[proj30[bup], ],
    `Increase`= (fits$bup$threshold$density_ratios != 0)[proj30[bup], ] & increased[bup & proj30, ],
    `Constant`= (fits$bup$threshold$density_ratios != 0)[proj30[bup], ] & !increased[bup & proj30, ],
    `\\emph{Total}` = (fits$bup$threshold$density_ratios != 0)[proj30[bup], ],
    `Increase`= (fits$bup$hybrid$density_ratios != 0)[proj30[bup], ] & increased[bup & proj30, ],
    `Constant`= (fits$bup$hybrid$density_ratios != 0)[proj30[bup], ] & !increased[bup & proj30, ],
    `\\emph{Total}` = (fits$bup$hybrid$density_ratios != 0)[proj30[bup], ]
  ), strategy_n, .id = "subset"
) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE) |>
  pack_rows("Constant", 1, 1) |>
  pack_rows("Dynamic", 2, 4) |>
  pack_rows("Threshold", 5, 7) |>
  pack_rows("Hybrid", 8, 10)

map_dfr(
  list(
    `\\emph{Total}` = (fits$bup$constant$density_ratios != 0)[proj51[bup], ],
    `Increase`= (fits$bup$dynamic$density_ratios != 0)[proj51[bup], ] & increased[bup & proj51, ],
    `Constant`= (fits$bup$dynamic$density_ratios != 0)[proj51[bup], ] & !increased[bup & proj51, ],
    `\\emph{Total}` = (fits$bup$dynamic$density_ratios != 0)[proj51[bup], ],
    `Increase`= (fits$bup$threshold$density_ratios != 0)[proj51[bup], ] & increased[bup & proj51, ],
    `Constant`= (fits$bup$threshold$density_ratios != 0)[proj51[bup], ] & !increased[bup & proj51, ],
    `\\emph{Total}` = (fits$bup$threshold$density_ratios != 0)[proj51[bup], ],
    `Increase`= (fits$bup$hybrid$density_ratios != 0)[proj51[bup], ] & increased[bup & proj51, ],
    `Constant`= (fits$bup$hybrid$density_ratios != 0)[proj51[bup], ] & !increased[bup & proj51, ],
    `\\emph{Total}` = (fits$bup$hybrid$density_ratios != 0)[proj51[bup], ]
  ), strategy_n, .id = "subset"
) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE) |>
  pack_rows("Constant", 1, 1) |>
  pack_rows("Dynamic", 2, 4) |>
  pack_rows("Threshold", 5, 7) |>
  pack_rows("Hybrid", 8, 10)
