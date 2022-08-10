library(lmtp)
suppressPackageStartupMessages(library(tidyverse))
library(patchwork)
library(kableExtra)
library(glue)

source("R/rubin.R")

fits <- list()
for (med in c("met", "bup")) {
  fits[[med]] <- list()
  for (strat in c("constant", "dynamic", "threshold", "hybrid")) {
    fits[[med]][[strat]] <- list()
    for (imp in 1:5) {
      fits[[med]][[strat]][[imp]] <- list()
      for (t in 1:10) {
        fits[[med]][[strat]][[imp]][[t]] <- 
          readRDS(glue("data/fits/{imp}_{med}_{t+1}_{strat}.rds"))
      }
    }
  }
}

combine <- \(fits) map_dfr(1:10, \(t) rubins_rules(map(fits, \(x) x[[t]]$fit), t + 2))

contrast <- function(x, ref, type = c("additive", "rr")) {
  ans <- list()
  for (imp in 1:5) {
    ans[[imp]] <- list()
    for (t in 1:10) {
      fit <- pluck(x, imp, t, "fit")
      y <- pluck(ref, imp, t, "fit")
      
      if (t == 1) {
        ans[[imp]][[t]] <- lmtp_contrast(fit, ref = y, type = match.arg(type))
        next
      }
      
      fit$theta = 1 - fit$theta
      y$theta = 1 - y$theta
      ans[[imp]][[t]] <- lmtp_contrast(fit, ref = y, type = match.arg(type))
    }
  }
  
  map(1:10, \(i) map(ans, \(x) x[[i]])) |> 
    map2_dfr(3:12, rubins_rules)
}

# Tables ------------------------------------------------------------------

# Produces basic LaTeX code for Table A1, results are added to the clipboard
map(c(dynamic = "dynamic", threshold = "threshold", hybrid = "hybrid"), 
    \(x) fits$bup[[x]]) |>
  map_dfr(\(x) contrast(x, ref = fits$bup$constant), .id = "strategy") |> 
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
    map(c(dynamic = "dynamic", threshold = "threshold", hybrid = "hybrid"), 
        \(x) fits$met[[x]]) |>
      map_dfr(\(x) contrast(x, ref = fits$met$constant), .id = "strategy") |> 
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
  left_join({
    map(c(dynamic = "dynamic", threshold = "threshold", hybrid = "hybrid"), 
        \(x) fits$bup[[x]]) |>
      map_dfr(\(x) contrast(x, ref = fits$bup$constant, "rr"), .id = "strategy") |> 
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
        map(c(dynamic = "dynamic", threshold = "threshold", hybrid = "hybrid"), 
            \(x) fits$met[[x]]) |>
          map_dfr(\(x) contrast(x, ref = fits$met$constant, "rr"), .id = "strategy") |> 
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
      }, by = c("strategy", "label")
      )}, by = c("strategy", "label")
  ) |> 
  select(label, theta.x.x, ci.x.x, theta.x.y, ci.x.y, theta.y.x, ci.y.x, theta.y.y, ci.y.y) |> 
  kbl(format = "latex", booktabs = TRUE) |> 
  pack_rows("d1", 1, 10) |> 
  pack_rows("d2", 11, 20) |> 
  pack_rows("d3", 21, 30) |> 
  add_header_above(c(" " = 1, "ATE" = 2, "RR" = 2, "ATE" = 2, "RR" = 2)) |> 
  add_header_above(c(" " = 1, "BUP-NX" = 4, "Met." = 4)) |> 
  cat(file = "_research/tables.txt", append = TRUE)

# Produces basic LaTeX code for Table A2, results are added to the clipboard
map(c(dynamic = "dynamic", threshold = "threshold"), 
    \(x) fits$bup[[x]]) |>
  map_dfr(\(x) contrast(x, ref = fits$bup$hybrid), .id = "strategy") |> 
  mutate(
    strategy = case_when(
      strategy == "dynamic" ~ "d1", 
      strategy == "threshold" ~ "d2"
    )
  ) |> 
  select(-alpha, -se) |> 
  mutate(across(c("theta", "conf.low", "conf.high"), \(x) sprintf("%.2f", x))) |> 
  unite(ci, conf.low, conf.high, sep = ", ") |> 
  left_join({
    map(c(dynamic = "dynamic", threshold = "threshold"), 
        \(x) fits$met[[x]]) |>
      map_dfr(\(x) contrast(x, ref = fits$met$hybrid), .id = "strategy") |> 
      mutate(
        strategy = case_when(
          strategy == "dynamic" ~ "d1", 
          strategy == "threshold" ~ "d2"
        )
      ) |> 
      select(-alpha, -se) |> 
      mutate(across(c("theta", "conf.low", "conf.high"), \(x) sprintf("%.2f", x))) |> 
      unite(ci, conf.low, conf.high, sep = ", ")
  }, by = c("strategy", "label")) |> 
  left_join({
    map(c(dynamic = "dynamic", threshold = "threshold"), 
        \(x) fits$bup[[x]]) |>
      map_dfr(\(x) contrast(x, ref = fits$bup$hybrid, "rr"), .id = "strategy") |> 
      mutate(
        strategy = case_when(
          strategy == "dynamic" ~ "d1", 
          strategy == "threshold" ~ "d2"
        )
      ) |> 
      select(-alpha, -se) |> 
      mutate(across(c("theta", "conf.low", "conf.high"), \(x) sprintf("%.2f", x))) |> 
      unite(ci, conf.low, conf.high, sep = ", ") |> 
      left_join({
        map(c(dynamic = "dynamic", threshold = "threshold"), 
            \(x) fits$met[[x]]) |>
          map_dfr(\(x) contrast(x, ref = fits$met$hybrid, "rr"), .id = "strategy") |> 
          mutate(
            strategy = case_when(
              strategy == "dynamic" ~ "d1", 
              strategy == "threshold" ~ "d2"
            )
          ) |> 
          select(-alpha, -se) |> 
          mutate(across(c("theta", "conf.low", "conf.high"), \(x) sprintf("%.2f", x))) |> 
          unite(ci, conf.low, conf.high, sep = ", ")
      }, by = c("strategy", "label")
      )}, by = c("strategy", "label")
  ) |> 
  select(label, theta.x.x, ci.x.x, theta.x.y, ci.x.y, theta.y.x, ci.y.x, theta.y.y, ci.y.y) |> 
  kbl(format = "latex", booktabs = TRUE) |> 
  pack_rows("d1", 1, 10) |> 
  pack_rows("d2", 11, 20) |> 
  add_header_above(c(" " = 1, "ATE" = 2, "RR" = 2, "ATE" = 2, "RR" = 2)) |> 
  add_header_above(c(" " = 1, "BUP-NX" = 4, "Met." = 4)) |> 
  cat(file = "_research/tables.txt", append = TRUE, sep = "\n")

# Figures -----------------------------------------------------------------

# Produce Figure 1a
ragg::agg_png("figures/combined_sdr_bup_080922.png", width = 8, height = 4.5, units = "cm", res = 400)

wrap_plots(
  {
    map_dfr(fits$bup, combine, .id = "strategy") |>
      mutate(strategy = factor(case_when(
        strategy == "constant" ~ "d4", 
        strategy == "dynamic" ~ "d1", 
        strategy == "threshold" ~ "d2", 
        strategy == "hybrid" ~ "d3"
      ), levels = c("d1", "d2", "d3", "d4")), 
      theta = if_else(label == 3, 1 - theta, theta)) |> 
      ggplot(aes(x = label, y = 1 - theta, color = strategy)) +
      geom_step(size = 0.2) + 
      geom_point(size = 0.2, aes(shape = strategy, color = strategy)) + 
      scale_x_continuous(breaks = 3:12, labels = c("Wk. 3", 4:12), 
                         limits = c(2.75, 12.25), expand = c(0, .2)) + 
      # scale_linetype_manual(
      #   values = c("solid", "dashed", "dotted", "dotdash")
      # ) + 
      scale_color_manual(
        values = c('#004488', '#DDAA33', '#BB5566', '#000000')
      ) + 
      labs(
        x = "", 
        y = "Relapse risk",
        linetype = "", 
        shape = "", 
        color = ""
      ) + 
      theme_light(base_size = 4, 
                  base_line_size = 0.2,
                  base_rect_size = 0.2) + 
      theme(axis.text.x = element_blank(), 
            axis.ticks.x = element_blank(), 
            legend.text = element_text(size = 3), 
            legend.key.size = unit(0.25, "cm"))  + 
      guides(guide_legend(override.aes = list(size = 0.5)))
  }, {
    map(c(dynamic = "dynamic", threshold = "threshold", hybrid = "hybrid"), 
        \(x) fits$bup[[x]]) |>
      map_dfr(\(x) contrast(x, ref = fits$bup$constant), .id = "strategy") |> 
      mutate(
        strategy = factor(case_when(
          strategy == "constant" ~ "d4", 
          strategy == "dynamic" ~ "d1", 
          strategy == "threshold" ~ "d2", 
          strategy == "hybrid" ~ "d3"
        ), levels = c("d1", "d2", "d3", "d4"))
      ) |> 
      ggplot(aes(x = label, y = theta)) + 
      geom_point(position = position_dodge(.5), size = 0.2, 
                 aes(shape = strategy, color = strategy)) + 
      geom_errorbar(
        aes(
          ymin = conf.low,
          ymax = conf.high, 
          #linetype = strategy, 
          color = strategy
        ),
        width = 0.2,
        position = position_dodge(.5), 
        size = 0.2
      ) + 
      geom_hline(yintercept = 0, color = "grey", size = 0.2) + 
      scale_y_continuous(limits = c(-0.18, 0.05)) + 
      scale_x_continuous(breaks = 3:12, labels = c("Wk. 3", 4:12), 
                         limits = c(2.75, 12.25), expand = c(0, 0.2)) + 
      # scale_linetype_manual(
      #   drop = FALSE, 
      #   values = c("solid", "dashed", "dotted", "dotdash")
      # ) + 
      scale_color_manual(
        drop = FALSE,
        values = c('#004488', '#DDAA33', '#BB5566', '#000000')
      ) + 
      scale_shape_discrete(drop = FALSE) + 
      labs(
        x = "", 
        y = "ATE",
        linetype = "", 
        shape = "", 
        color = ""
      ) + 
      theme_light(base_size = 4, 
                  base_line_size = 0.2,
                  base_rect_size = 0.2) + 
      theme(legend.text = element_text(size = 3), 
            legend.key.size = unit(0.25, "cm"), 
            legend.position = "none") + 
      guides(guide_legend(override.aes = list(size = 0.5)))
  },
  nrow = 2, heights = c(0.75, .25), guides = "collect") & 
  theme(plot.margin = grid::unit(c(1, 1, 0, 1), units = "mm"), 
        legend.margin = margin(l = -5))

dev.off()

# Produce Figure 1b
ragg::agg_png("figures/combined_sdr_methadone_080922.png", width = 8, height = 4.5, units = "cm", res = 400)

wrap_plots(
  {
    map_dfr(fits$met, combine, .id = "strategy") |>
      mutate(strategy = factor(case_when(
        strategy == "constant" ~ "d4", 
        strategy == "dynamic" ~ "d1", 
        strategy == "threshold" ~ "d2", 
        strategy == "hybrid" ~ "d3"
      ), levels = c("d1", "d2", "d3", "d4")), 
      theta = if_else(label == 3, 1 - theta, theta)) |> 
      ggplot(aes(x = label, y = 1 - theta, color = strategy)) +
      geom_step(size = 0.2) + 
      geom_point(size = 0.2, aes(shape = strategy, color = strategy)) + 
      scale_x_continuous(breaks = 3:12, labels = c("Wk. 3", 4:12), 
                         limits = c(2.75, 12.25), expand = c(0, .2)) + 
      # scale_linetype_manual(
      #   values = c("solid", "dashed", "dotted", "dotdash")
      # ) + 
      scale_color_manual(
        values = c('#004488', '#DDAA33', '#BB5566', '#000000')
      ) + 
      labs(
        x = "", 
        y = "Relapse risk",
        linetype = "", 
        shape = "", 
        color = ""
      ) + 
      theme_light(base_size = 4, 
                  base_line_size = 0.2,
                  base_rect_size = 0.2) + 
      theme(axis.text.x = element_blank(), 
            axis.ticks.x = element_blank(), 
            legend.text = element_text(size = 3), 
            legend.key.size = unit(0.25, "cm"))  + 
      guides(guide_legend(override.aes = list(size = 0.5)))
  }, {
    map(c(dynamic = "dynamic", threshold = "threshold", hybrid = "hybrid"), 
        \(x) fits$met[[x]]) |>
      map_dfr(\(x) contrast(x, ref = fits$met$constant), .id = "strategy") |> 
      mutate(
        strategy = factor(case_when(
          strategy == "constant" ~ "d4", 
          strategy == "dynamic" ~ "d1", 
          strategy == "threshold" ~ "d2", 
          strategy == "hybrid" ~ "d3"
        ), levels = c("d1", "d2", "d3", "d4"))
      ) |> 
      ggplot(aes(x = label, y = theta)) + 
      geom_point(position = position_dodge(.5), size = 0.2, 
                 aes(shape = strategy, color = strategy)) + 
      geom_errorbar(
        aes(
          ymin = conf.low,
          ymax = conf.high, 
          #linetype = strategy, 
          color = strategy
        ),
        width = 0.2,
        position = position_dodge(.5), 
        size = 0.2
      ) + 
      geom_hline(yintercept = 0, color = "grey", size = 0.2) + 
      scale_y_continuous(limits = c(-0.22, 0.075)) + 
      scale_x_continuous(breaks = 3:12, labels = c("Wk. 3", 4:12), 
                         limits = c(2.75, 12.25), expand = c(0, 0.2)) + 
      # scale_linetype_manual(
      #   drop = FALSE, 
      #   values = c("solid", "dashed", "dotted", "dotdash")
      # ) + 
      scale_color_manual(
        drop = FALSE,
        values = c('#004488', '#DDAA33', '#BB5566', '#000000')
      ) + 
      scale_shape_discrete(drop = FALSE) + 
      labs(
        x = "", 
        y = "ATE",
        linetype = "", 
        shape = "", 
        color = ""
      ) + 
      theme_light(base_size = 4, 
                  base_line_size = 0.2,
                  base_rect_size = 0.2) + 
      theme(legend.text = element_text(size = 3), 
            legend.key.size = unit(0.25, "cm"), 
            legend.position = "none") + 
      guides(guide_legend(override.aes = list(size = 0.5)))
  },
  nrow = 2, heights = c(0.75, .25), guides = "collect") & 
  theme(plot.margin = grid::unit(c(1, 1, 0, 1), units = "mm"), 
        legend.margin = margin(l = -5))

dev.off()

# Produce Figure A1a
ragg::agg_png("figures/A1_combined_sdr_bup_080922.png", width = 8, height = 4.5, units = "cm", res = 400)

map(c(dynamic = "dynamic", threshold = "threshold"), 
    \(x) fits$bup[[x]]) |>
  map_dfr(\(x) contrast(x, ref = fits$bup$hybrid), .id = "strategy") |> 
  mutate(
    strategy = factor(case_when(
      strategy == "constant" ~ "d4", 
      strategy == "dynamic" ~ "d1", 
      strategy == "threshold" ~ "d2", 
      strategy == "hybrid" ~ "d3"
    ), levels = c("d1", "d2", "d3", "d4"))
  ) |> 
  ggplot(aes(x = label, y = theta, 
             # linetype = strategy, 
             color = strategy)) + 
  geom_point(position = position_dodge(.5), size = 0.2) + 
  geom_errorbar(
    aes(
      ymin = conf.low,
      ymax = conf.high, 
      # linetype = strategy, 
      color = strategy
    ),
    width = 0.2,
    position = position_dodge(.5), 
    size = 0.2
  ) + 
  scale_color_manual(
    values = c('#004488', '#DDAA33', '#BB5566', '#000000')
  ) + 
  geom_hline(yintercept = 0, color = "grey", size = 0.2) + 
  scale_x_continuous(breaks = 3:12, labels = c("Wk. 3", 4:12), 
                     limits = c(2.75, 12.25), expand = c(0, 0.2)) + 
  labs(
    x = "", 
    y = "ATE",
    linetype = "", 
    color = ""
  ) + 
  theme_light(base_size = 4, 
              base_line_size = 0.2,
              base_rect_size = 0.2) + 
  theme(legend.text = element_text(size = 3), 
        legend.key.size = unit(0.25, "cm")) + 
  guides(shape = guide_legend(override.aes = list(size = 0.5)))

dev.off()

# Produce Figure A1b
ragg::agg_png("figures/A1_combined_sdr_methadone_080922.png", width = 8, height = 4.5, units = "cm", res = 400)

map(c(dynamic = "dynamic", threshold = "threshold"), 
    \(x) fits$met[[x]]) |>
  map_dfr(\(x) contrast(x, ref = fits$met$hybrid), .id = "strategy") |> 
  mutate(
    strategy = factor(case_when(
      strategy == "constant" ~ "d4", 
      strategy == "dynamic" ~ "d1", 
      strategy == "threshold" ~ "d2", 
      strategy == "hybrid" ~ "d3"
    ), levels = c("d1", "d2", "d3", "d4"))
  ) |> 
  ggplot(aes(x = label, y = theta, 
             # linetype = strategy, 
             color = strategy)) + 
  geom_point(position = position_dodge(.5), size = 0.2) + 
  geom_errorbar(
    aes(
      ymin = conf.low,
      ymax = conf.high, 
      # linetype = strategy, 
      color = strategy
    ),
    width = 0.2,
    position = position_dodge(.5), 
    size = 0.2
  ) + 
  scale_color_manual(
    values = c('#004488', '#DDAA33', '#BB5566', '#000000')
  ) + 
  geom_hline(yintercept = 0, color = "grey", size = 0.2) + 
  scale_x_continuous(breaks = 3:12, labels = c("Wk. 3", 4:12), 
                     limits = c(2.75, 12.25), expand = c(0, 0.2)) + 
  labs(
    x = "", 
    y = "ATE",
    linetype = "", 
    color = ""
  ) + 
  theme_light(base_size = 4, 
              base_line_size = 0.2,
              base_rect_size = 0.2) + 
  theme(legend.text = element_text(size = 3), 
        legend.key.size = unit(0.25, "cm")) + 
  guides(shape = guide_legend(override.aes = list(size = 0.5)))

dev.off()
