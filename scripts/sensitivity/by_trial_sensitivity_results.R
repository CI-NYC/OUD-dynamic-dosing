library(lmtp)
library(tidyverse)
library(patchwork)
library(kableExtra)

source("R/rubin.R")

fits <- readRDS("data/drv/lmtp•fits•bytrial•sdr•010422.rds")

combine = \(fits) map_dfr(1:9, \(t) rubins_rules(map(fits, \(x) x[[t]]), t + 3))

risk_diff = function(x, ref, trial) {
  map2(x, ref, function(x, y) {
    x$theta = 1 - x$theta
    y$theta = 1 - y$theta
    lmtp_contrast(x, ref = y)
  }) |> rubins_rules(trial)
}

risk_ratio = function(x, ref, trial) {
  map2(x, ref, function(x, y) {
    x$theta = 1 - x$theta
    y$theta = 1 - y$theta
    lmtp_contrast(x, ref = y, type = "rr")
  }) |> rubins_rules(trial)
}

# Produces basic LaTeX code for Table A1, results are added to the clipboard
map(c(dynamic = "dynamic_27", threshold = "threshold_27", hybrid = "hybrid_27"), 
    \(x) fits[[x]]) |>
  map_dfr(\(x) risk_diff(x, ref = fits$constant_27, "CTN0027"), .id = "strategy") |> 
  mutate(
    strategy = case_when(
      strategy == "dynamic" ~ "d1",
      strategy == "threshold" ~ "d2",
      strategy == "hybrid" ~ "d3"
    )
  ) |>
  select(-alpha, -se) |> 
  mutate(across(c("theta", "conf.low", "conf.high"), \(x) sprintf("%.2f", x))) |> 
  unite(ci, conf.low, conf.high, sep = ", ") |> 
  left_join({
    map(c(dynamic = "dynamic_27", threshold = "threshold_27", hybrid = "hybrid_27"), 
        \(x) fits[[x]]) |>
      map_dfr(\(x) risk_ratio(x, ref = fits$constant_27, "CTN0027"), .id = "strategy") |> 
      mutate(
        strategy = case_when(
          strategy == "dynamic" ~ "d1",
          strategy == "threshold" ~ "d2",
          strategy == "hybrid" ~ "d3"
        )
      ) |>
      select(-alpha, -se) |> 
      mutate(across(c("theta", "conf.low", "conf.high"), \(x) sprintf("%.2f", x))) |> 
      unite(ci, conf.low, conf.high, sep = ", ") 
  }, by = c("strategy", "label")) |> 
  select(strategy, label, theta.x, ci.x, theta.y, ci.y) |> 
  bind_rows({
    map(c(dynamic = "dynamic_30", threshold = "threshold_30", hybrid = "hybrid_30"), 
        \(x) fits[[x]]) |>
      map_dfr(\(x) risk_diff(x, ref = fits$constant_30, "CTN0030"), .id = "strategy") |> 
      mutate(
        strategy = case_when(
          strategy == "dynamic" ~ "d1",
          strategy == "threshold" ~ "d2",
          strategy == "hybrid" ~ "d3"
        )
      ) |>
      select(-alpha, -se) |> 
      mutate(across(c("theta", "conf.low", "conf.high"), \(x) sprintf("%.2f", x))) |> 
      unite(ci, conf.low, conf.high, sep = ", ") |> 
      left_join({
        map(c(dynamic = "dynamic_30", threshold = "threshold_30", hybrid = "hybrid_30"), 
            \(x) fits[[x]]) |>
          map_dfr(\(x) risk_ratio(x, ref = fits$constant_30, "CTN0030"), .id = "strategy") |> 
          mutate(
            strategy = case_when(
              strategy == "dynamic" ~ "d1",
              strategy == "threshold" ~ "d2",
              strategy == "hybrid" ~ "d3"
            )
          ) |>
          select(-alpha, -se) |> 
          mutate(across(c("theta", "conf.low", "conf.high"), \(x) sprintf("%.2f", x))) |> 
          unite(ci, conf.low, conf.high, sep = ", ") 
      }, by = c("strategy", "label")) |> 
      select(strategy, label, theta.x, ci.x, theta.y, ci.y) |> 
      bind_rows({
        map(c(dynamic = "dynamic_51", threshold = "threshold_51", hybrid = "hybrid_51"), 
            \(x) fits[[x]]) |>
          map_dfr(\(x) risk_diff(x, ref = fits$constant_51, "CTN0051"), .id = "strategy") |> 
          mutate(
            strategy = case_when(
              strategy == "dynamic" ~ "d1",
              strategy == "threshold" ~ "d2",
              strategy == "hybrid" ~ "d3"
            )
          ) |>
          select(-alpha, -se) |> 
          mutate(across(c("theta", "conf.low", "conf.high"), \(x) sprintf("%.2f", x))) |> 
          unite(ci, conf.low, conf.high, sep = ", ") |> 
          left_join({
            map(c(dynamic = "dynamic_51", threshold = "threshold_51", hybrid = "hybrid_51"), 
                \(x) fits[[x]]) |>
              map_dfr(\(x) risk_ratio(x, ref = fits$constant_51, "CTN0051"), .id = "strategy") |> 
              mutate(
                strategy = case_when(
                  strategy == "dynamic" ~ "d1",
                  strategy == "threshold" ~ "d2",
                  strategy == "hybrid" ~ "d3"
                )
              ) |>
              select(-alpha, -se) |> 
              mutate(across(c("theta", "conf.low", "conf.high"), \(x) sprintf("%.2f", x))) |> 
              unite(ci, conf.low, conf.high, sep = ", ") 
          }, by = c("strategy", "label")) |> 
          select(strategy, label, theta.x, ci.x, theta.y, ci.y)
      })
  }) |> 
  select(-label) |> 
  kbl(format = "latex", booktabs = TRUE) |> 
  pack_rows("CTN0027", 1, 3) |>
  pack_rows("CTN0030", 4, 6) |>
  pack_rows("CTN0051", 7, 9) |>
  add_header_above(c(" " = 1, "ATE" = 2, "RR" = 2)) |> 
  clipr::write_clip()
