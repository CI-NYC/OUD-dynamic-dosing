library(lmtp)
suppressPackageStartupMessages(library(tidyverse))
library(patchwork)
library(kableExtra)
library(glue)

source("R/rubin.R")

fits <- list()
for (proj in c("27", "30", "51")) {
  fits[[proj]] <- list()
  for (strat in c("constant", "dynamic", "threshold", "hybrid")) {
    fits[[proj]][[strat]] <- list()
    for (imp in 1:5) {
      fits[[proj]][[strat]][[imp]] <- list()
      for (t in 1:10) {
        fits[[proj]][[strat]][[imp]][[t]] <- 
          readRDS(glue("data/fits/by_trial/{imp}_bup_{proj}_{t+1}_{strat}.rds"))
      }
    }
  }
}

bad <- c()
for (proj in c("27", "30", "51")) {
  for (strat in c("constant", "dynamic", "threshold", "hybrid")) {
    for (imp in 1:5) {
      for (t in 1:10) {
        file <- glue("{imp}_bup_{proj}_{t+1}_{strat}.rds")
        if (!(file %in% list.files("data/fits/by_trial"))) {
          bad <- c(bad, file)
        }
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

# Figures -----------------------------------------------------------------

ragg::agg_png("figures/27_sdr_bup_080922.png", width = 8, height = 4.5, units = "cm", res = 400)

wrap_plots(
  {
    map_dfr(fits$`27`, combine, .id = "strategy") |>
      filter(label <= 6) |> 
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
      scale_x_continuous(breaks = 3:6, labels = c("Wk. 3", 4:6), 
                         limits = c(2.75, 6.25), expand = c(0, .2)) + 
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
        \(x) fits$`27`[[x]]) |>
      map_dfr(\(x) contrast(x, ref = fits$`27`$constant), .id = "strategy") |> 
      filter(label <= 6) |> 
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
      scale_y_continuous(limits = c(-0.25, 0.05)) + 
      scale_x_continuous(breaks = 3:6, labels = c("Wk. 3", 4:6), 
                         limits = c(2.75, 6.25), expand = c(0, 0.2)) + 
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

ragg::agg_png("figures/30_sdr_bup_080922.png", width = 8, height = 4.5, units = "cm", res = 400)

wrap_plots(
  {
    map_dfr(fits$`30`, combine, .id = "strategy") |>
      filter(label <= 6) |> 
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
      scale_x_continuous(breaks = 3:6, labels = c("Wk. 3", 4:6), 
                         limits = c(2.75, 6.25), expand = c(0, .2)) + 
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
        \(x) fits$`30`[[x]]) |>
      map_dfr(\(x) contrast(x, ref = fits$`30`$constant), .id = "strategy") |> 
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
      scale_y_continuous(limits = c(-0.13, 0.2)) + 
      scale_x_continuous(breaks = 3:6, labels = c("Wk. 3", 4:6), 
                         limits = c(2.75, 6.25), expand = c(0, 0.2)) + 
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

ragg::agg_png("figures/51_sdr_bup_080922.png", width = 8, height = 4.5, units = "cm", res = 400)

wrap_plots(
  {
    map_dfr(fits$`51`, combine, .id = "strategy") |>
      filter(label <= 6) |> 
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
      scale_x_continuous(breaks = 3:6, labels = c("Wk. 3", 4:6), 
                         limits = c(2.75, 6.25), expand = c(0, .2)) + 
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
        \(x) fits$`51`[[x]]) |>
      map_dfr(\(x) contrast(x, ref = fits$`51`$constant), .id = "strategy") |> 
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
      scale_y_continuous(limits = c(-0.18, 0.23)) + 
      scale_x_continuous(breaks = 3:6, labels = c("Wk. 3", 4:6), 
                         limits = c(2.75, 6.25), expand = c(0, 0.2)) + 
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
