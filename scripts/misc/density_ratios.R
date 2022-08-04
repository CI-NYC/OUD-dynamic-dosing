library(lmtp)
library(tidyverse)
library(kableExtra)

source("R/rubin.R")

fits <- readRDS("data/drv/lmtp•fits•sdr•010422.rds")

map(fits$bup, \(x) x[[1]][[9]]) |> 
  map(\(x) x$density_ratios) |> 
  map(\(x) apply(x, 2, max)) |> 
  map(\(x) setNames(x, c("Wk. 3", 4:11))) |> 
  map_dfr(\(x) as_tibble_row(x), .id = "strategy") |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE, digits = 2)

map(fits$met, \(x) x[[1]][[9]]) |> 
  map(\(x) x$density_ratios) |> 
  map(\(x) apply(x, 2, max)) |> 
  map(\(x) setNames(x, c("Wk. 3", 4:11))) |> 
  map_dfr(\(x) as_tibble_row(x), .id = "strategy") |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE, digits = 2)
