#ltmle analysis

library(ltmle)
library(mitools)
library(mice)
library(SuperLearner)
library(arm)
library(gam)
library(tidyverse)
library(xtable)

#set random seed for reproduceability
set.seed(100)

load("../Data/clean_combined_imputed_data.RData")
load("~/Documents/Columbia MPH (to back up)/DSI project/ForGitHub/Data/data_ready_for_ltmle.RData")


#---------------- Function: run LTMLE on each eligible outcome week -----------------#

# test_results_MEAN = run_SMALLEST_ltmle_case(ALT_weekly_data_for_ltmle_04[[1]], ALT_patients_imputed_03, 1, c("SL.mean"))
# test_results_GLM = run_SMALLEST_ltmle_case(ALT_weekly_data_for_ltmle_04[[1]], ALT_patients_imputed_03, 1, c("SL.glm"))
# test_results_GLMNET = run_SMALLEST_ltmle_case(ALT_weekly_data_for_ltmle_04[[1]], ALT_patients_imputed_03, 1, c("SL.glmnet"))
# test_results_EARTH = run_SMALLEST_ltmle_case(ALT_weekly_data_for_ltmle_04[[1]], ALT_patients_imputed_03, 1, c("SL.earth"))
# test_results_xgboost = run_SMALLEST_ltmle_case(ALT_weekly_data_for_ltmle_04[[1]], ALT_patients_imputed_03, 1, c("SL.xgboost"))
# 
# case13_all_proj_up_to_12wk = run_SMALLEST_ltmle_case(ALT_weekly_data_for_ltmle_04[[1]], ALT_patients_imputed_03, 1, c("SL.glm", "SL.mean"))

run_SMALLEST_ltmle_case = function(prepared_info, mi_baseline_covariates, imputation_num, SL_library) {
  #Step 1: general prep
  #...1a: "unpack" the prepared info
  dataset_missing_baseline_covariates = prepared_info$dataset_missing_baseline_covariates %>% select(., -medicine, -project)
  Anodes = prepared_info$Anodes
  Lnodes = prepared_info$Lnodes
  Ynodes = prepared_info$Ynodes
  abar1 = prepared_info$abar1
  abar0 = prepared_info$abar0
  outcome_weeks = prepared_info$outcome_weeks
  name = prepared_info$name

  #...1b: "unpack" the imputed baseline data
  #TODO: turn this into a loop?
  baseline_covariates = complete(mi_baseline_covariates, action = imputation_num) %>%
    select(all_of(c(demog, comorbidities)), site)

  #...1c: add baseline covariates into the dataset
  all_data = inner_join(baseline_covariates,
                        dataset_missing_baseline_covariates,
                        by = "who")

  #...1d: create helper variables
  all_cols = colnames(all_data)
  baseline_cols = colnames(baseline_covariates)

  # #...1e: create an empty vector where we'll store the output from each week's LTMLE estimation
  txest = vector("list", length(outcome_weeks))
  txvar = vector("list", length(outcome_weeks))
  cntest = vector("list", length(outcome_weeks))
  cntvar = vector("list", length(outcome_weeks))
  est = vector("list", length(outcome_weeks))
  var = vector("list", length(outcome_weeks))
  lci = vector("list", length(outcome_weeks))
  uci = vector("list", length(outcome_weeks))


  #Step 2: iterate through all outcome weeks to get estimations
  for (i in 1:length(outcome_weeks)) {
    #...2a: prepare the data for use in the LTMLE function:
    #remove any columns that weren't used as A-, L-, or Y- nodes
    unused = setdiff(all_cols, c(baseline_cols, Anodes[[i]], Lnodes[[i]], Ynodes[1:i]))

    ltmle_data = all_data %>%
      select(-all_of(unused)) %>%
      relocate(baseline_cols, Lnodes[[i]], Anodes[[i]], Ynodes[1:i]) %>%
      select(-who)

    ltmle_data = as.data.frame(ltmle_data)

    abar_this_time = list(abar1[,1:length(Anodes[[i]]), drop = FALSE], abar0[,1:length(Anodes[[i]]), drop = FALSE])

    #...2b: run the ltmle function
    output <- ltmle(
      data = ltmle_data,
      Anodes = Anodes[[i]],
      Lnodes = Lnodes[[i]],
      Ynodes = Ynodes[1:i],
      survivalOutcome = TRUE, #TRUE means the outcome is an event that can only occur once.
      abar = abar_this_time,
      estimate.time = FALSE, #if TRUE, this would just compute a rough estimate of runtime based on first 50 obs
      SL.library = SL_library,
      variance.method = "ic"
    )

    #...2c: keep the output
    sumout = summary(output)

    txest[i] <- sumout$effect.measures$treatment$estimate
    txvar[i] <- sumout$effect.measures$treatment$std.dev ^ 2

    cntest[i] <- sumout$effect.measures$control$estimate
    cntvar[i] <- sumout$effect.measures$control$std.dev ^ 2

    est[i] <- sumout$effect.measures$ATE$estimate
    var[i] <- sumout$effect.measures$ATE$std.dev ^ 2
  }

  #Step 3: package output together to return
  estimates = tibble(
    Outcome_week = outcome_weeks,
    Treatment_estimate = txest,
    Treatment_variance = txvar,
    Control_estimate = cntest,
    Control_variance = cntvar,
    ATE_estimate = est,
    ATE_variance = var
  )

  estimates
}


run_ltmle_case = function(prepared_info, mi_baseline_covariates, imputation_num, week_cutoff) {
  #Step 1: general prep
  #...1a: "unpack" the prepared info
  dataset_missing_baseline_covariates = prepared_info$dataset_missing_baseline_covariates %>% select(., -medicine, -project)
  Anodes = prepared_info$Anodes
  Lnodes = prepared_info$Lnodes
  Ynodes = prepared_info$Ynodes
  abar1 = prepared_info$abar1
  abar0 = prepared_info$abar0
  outcome_weeks = prepared_info$outcome_weeks
  outcome_weeks = outcome_weeks[outcome_weeks <= week_cutoff]
  name = prepared_info$name

  #...1b: "unpack" the imputed baseline data
  #TODO: turn this into a loop?
  baseline_covariates = complete(mi_baseline_covariates, action = imputation_num) %>%
    select(all_of(c(demog, comorbidities)), site)

  #...1c: add baseline covariates into the dataset
  all_data = inner_join(baseline_covariates,
                        dataset_missing_baseline_covariates,
                        by = "who")

  #...1d: create helper variables
  all_cols = colnames(all_data)
  baseline_cols = colnames(baseline_covariates)

  # #...1e: create an empty vector where we'll store the output from each week's LTMLE estimation
  txest = vector("list", length(outcome_weeks))
  txvar = vector("list", length(outcome_weeks))
  # txlci = vector("list", length(outcome_weeks))
  # txuci = vector("list", length(outcome_weeks))
  cntest = vector("list", length(outcome_weeks))
  cntvar = vector("list", length(outcome_weeks))
  # cntlci = vector("list", length(outcome_weeks))
  # cntuci = vector("list", length(outcome_weeks))
  est = vector("list", length(outcome_weeks))
  var = vector("list", length(outcome_weeks))
  lci = vector("list", length(outcome_weeks))
  uci = vector("list", length(outcome_weeks))
  # rrest = vector("list", length(outcome_weeks))
  # rrlci = vector("list", length(outcome_weeks))
  # rruci = vector("list", length(outcome_weeks))
  

  #Step 2: iterate through all outcome weeks to get estimations
 for (i in 1:length(outcome_weeks)) {
    #...2a: prepare the data for use in the LTMLE function:
    #remove any columns that weren't used as A-, L-, or Y- nodes
    unused = setdiff(all_cols, c(baseline_cols, Anodes[[i]], Lnodes[[i]], Ynodes[1:i]))
    
    ltmle_data = all_data %>%
      select(-all_of(unused)) %>%
      relocate(baseline_cols, Lnodes[[i]], Anodes[[i]], Ynodes[1:i]) %>% 
      select(-who)

    ltmle_data = as.data.frame(ltmle_data)
    
    abar_this_time = list(abar1[,1:length(Anodes[[i]]), drop = FALSE], abar0[,1:length(Anodes[[i]]), drop = FALSE])
      
    #...2b: run the ltmle function
    output <- ltmle(
      data = ltmle_data,
      Anodes = Anodes[[i]],
      Lnodes = Lnodes[[i]],
      Ynodes = Ynodes[1:i],
      survivalOutcome = TRUE, #TRUE means the outcome is an event that can only occur once.
      abar = abar_this_time,
      #abar = list(abar1[,1:i, drop = FALSE], abar0[,1:i, drop = FALSE]),
      estimate.time = FALSE, #if TRUE, this would just compute a rough estimate of runtime based on first 50 obs
      #SL.library = NULL #if NULL, glm will be used. later, switch to super learner later
      #SL.library = c("SL.glm", "SL.mean", "SL.earth", "SL.xgboost"),
      SL.library = c("SL.glm", "SL.mean"),
      variance.method = "ic"
    )

    #...2c: keep the output
    sumout = summary(output)

    txest[i] <- sumout$effect.measures$treatment$estimate
    txvar[i] <- sumout$effect.measures$treatment$std.dev ^ 2
    # txlci[i] <- sumout$effect.measures$treatment$CI[1]
    # txuci[i] <- sumout$effect.measures$treatment$CI[2]

    cntest[i] <- sumout$effect.measures$control$estimate
    cntvar[i] <- sumout$effect.measures$control$std.dev ^ 2
    # cntlci[i] <- sumout$effect.measures$control$CI[1]
    # cntuci[i] <- sumout$effect.measures$control$CI[2]
    
    est[i] <- sumout$effect.measures$ATE$estimate
    var[i] <- sumout$effect.measures$ATE$std.dev ^ 2
    # lci[i] <- sumout$effect.measures$ATE$CI[1]
    # uci[i] <- sumout$effect.measures$ATE$CI[2]
    
    # rrest[i] <- sumout$effect.measures$RR$estimate
    # rrlci[i] <- sumout$effect.measures$RR$CI[1]
    # rruci[i] <- sumout$effect.measures$RR$CI[2]
  }

  #Step 3: package output together to return
  estimates = tibble(
    Outcome_week = outcome_weeks,
    Treatment_estimate = txest, 
    Treatment_variance = txvar, 
    # Treatment_lowerCI = txlci, 
    # Treatment_upperCI = txuci, 
    Control_estimate = cntest,
    Control_variance = cntvar, 
    # Control_lowerCI = cntlci,
    # Control_upperCI = cntuci,
    ATE_estimate = est,
    ATE_variance = var #,
    # ATE_lowerCI = lci,
    # ATE_upperCI = uci,
    # RR_estimate = rrest,
    # RR_lowerCI = rrlci,
    # RR_upper_CI = rruci
  )

  estimates
}

# case1_results1 = run_ltmle_case(for_analysis_case1_ORIGINAL, ALT_patients_imputed_03, 1)
# case1_results2 = run_ltmle_case(for_analysis_case1_ORIGINAL, ALT_patients_imputed_03, 2)
# case1_results3 = run_ltmle_case(for_analysis_case1_ORIGINAL, ALT_patients_imputed_03, 3)
# case1_results4 = run_ltmle_case(for_analysis_case1_ORIGINAL, ALT_patients_imputed_03, 4)


#---------------- Function: combine results from different imputed datasets -----------------#

# run the `run_ltmle_case` function m times (where m is the number of imputations)
# pool the results
# create confidence intervals from the provided variances

combine_estimates_across_imputations = function(prepared_info, mi_baseline_covariates, week_cutoff = 24, only_first_iter = FALSE) {
  num_imputations = mi_baseline_covariates$m #4
  
  prepared_info = ALT_weekly_data_for_ltmle_04[[1]]
  mi_baseline_covariates = patients_imputed_03
  week_cutoff = 24
  only_first_iter = TRUE
  
  if (only_first_iter) {
    num_imputations = 1
  }
  
  all_estimates = vector("list", length(num_imputations))
  for (m in 1:num_imputations) {
    all_estimates[[m]] = run_ltmle_case(prepared_info, mi_baseline_covariates, m, week_cutoff)
  }
  
  if (only_first_iter) {
    merged_all = all_estimates[[1]]
  } else{
    merged_all = full_join(all_estimates[[1]],
                           all_estimates[[2]],
                           by = "Outcome_week",
                           suffix = c("1", "2"))
    
    for (m in 3:num_imputations) {
      merged_all = full_join(all_estimates[[m]],
                             merged_all,
                             by = "Outcome_week",
                             suffix = c(as.character(m), "3")) #for some reason this is what works for labeling!
    }
  }
  merged_all = merged_all %>% 
    select("Outcome_week", sort(colnames(.)))
  
  pooled_results = tibble(
    Outcome_week = numeric(),
    Treatment_estimate = numeric(),
    Treatment_variance = numeric(),
    Treatment_lowerCI = numeric(),
    Treatment_upperCI = numeric(),
    Control_estimate = numeric(),
    Control_variance = numeric(),
    Control_lowerCI = numeric(),
    Control_upperCI = numeric(),
    ATE_estimate = numeric(),
    ATE_variance = numeric(),
    ATE_lowerCI = numeric(),
    ATE_upperCI = numeric(),
  )
  
  # I'm sure this could all be done much more efficiently! But I can't quite figure it out.
  if (only_first_iter) {
      pooled_results = merged_all %>% 
        mutate(
          Treatment_lowerCI = unlist(Treatment_estimate) - 1.96 * (unlist(Treatment_variance) ^ .5),
          Treatment_upperCI = unlist(Treatment_estimate) + 1.96 * (unlist(Treatment_variance) ^ .5),
          Control_lowerCI = unlist(Control_estimate) - 1.96 * (unlist(Control_variance) ^ .5),
          Control_upperCI = unlist(Control_estimate) + 1.96 * (unlist(Control_variance) ^ .5),
          ATE_lowerCI = unlist(ATE_estimate) - 1.96 * (unlist(ATE_variance) ^ .5),
          ATE_upperCI = unlist(ATE_estimate) + 1.96 * (unlist(ATE_variance) ^ .5)
        )
  } else {
    for (week in merged_all$Outcome_week) {
      this_week_only = merged_all %>% filter(Outcome_week == week)
      
      treatment_pool = MIcombine(results = as.list(unlist(
        select(this_week_only, starts_with("Treatment_estimate")),
        use.names = FALSE
      )),
      variances = as.list(unlist(
        select(this_week_only, starts_with("Treatment_variance")),
        use.names = FALSE
      )))
      
      control_pool = MIcombine(results = as.list(unlist(
        select(this_week_only, starts_with("Control_estimate")),
        use.names = FALSE
      )),
      variances = as.list(unlist(
        select(this_week_only, starts_with("Control_variance")),
        use.names = FALSE
      )))
      
      ATE_pool = MIcombine(results = as.list(unlist(
        select(this_week_only, starts_with("ATE_estimate")),
        use.names = FALSE
      )),
      variances = as.list(unlist(
        select(this_week_only, starts_with("ATE_variance")),
        use.names = FALSE
      )))
      
      
      pooled_results = pooled_results %>% 
        add_row(
          Outcome_week = week,
          Treatment_estimate = treatment_pool$coefficients,
          Treatment_variance = treatment_pool$variance,
          Treatment_lowerCI = treatment_pool$coefficients - 1.96 * (treatment_pool$variance ^ .5),
          Treatment_upperCI = treatment_pool$coefficients + 1.96 * (treatment_pool$variance ^ .5),
          Control_estimate = control_pool$coefficients,
          Control_variance = control_pool$variance,
          Control_lowerCI = control_pool$coefficients - 1.96 * (control_pool$variance ^ .5),
          Control_upperCI = control_pool$coefficients + 1.96 * (control_pool$variance ^ .5),
          ATE_estimate = ATE_pool$coefficients,
          ATE_variance = ATE_pool$variance,
          ATE_lowerCI = ATE_pool$coefficients - 1.96 * (ATE_pool$variance ^ .5),
          ATE_upperCI = ATE_pool$coefficients + 1.96 * (ATE_pool$variance ^ .5),
        )
    }
  }
  
  pooled_results
}

#z = combine_estimates_across_imputations(x, y)

#---------------- Function: make a plot of the results -----------------#

#TODO: deal with the list/unlist problem in the first function, instead of this one
#TODO: check if the order of columns in the first function is working as expected (by looking into summary(output))
#TODO: re-do Anodes, Lnodes, Ynodes based on last week's conversation
#TODO: incorporate checks about which weeks dont have enough data
#TODO: make a table (with # people at each week)

#---DONE:
#TODO: take out people with no relapse, don't count them as relapsed on last date
#TODO: re-do who is excluded
#TODO: prep for dynamic dosing rules with cutoffs



plot_ltmle = function(estimates, title, id_number) {

  #Create dataframe
  datgraph <- tibble(
    est = unlist(c(estimates$Treatment_estimate, estimates$Control_estimate)), 
    lci = unlist(c(estimates$Treatment_lowerCI, estimates$Control_lowerCI)), 
    uci = unlist(c(estimates$Treatment_upperCI, estimates$Control_upperCI)), 
    week = unlist(rep(estimates$Outcome_week, 2)), 
    tx = unlist(c(rep("dynamic dose increase", length(estimates$Outcome_week)), 
                  rep("constant dose", length(estimates$Outcome_week))))
    )
  
  datgraph = as.data.frame(datgraph)
  
  #Make the dynamic dose curves
  pd <- position_dodge(0.5)
  #pdf("dynamicdosecurves.pdf")
  ggplot(datgraph, aes(x=week, y=est, linetype=tx, shape=tx, group=tx)) + 
    #facet_wrap(~estimand, scales="free", labeller=label_parsed) 
    #geom_errorbar(width=.1, aes(ymin=lci, ymax=uci), position=pd)  + 
    geom_point(position=pd, size=3) +  ylab("Risk of Relapse to OUD") + xlab("Week")   + geom_hline(yintercept=0,linetype = 2)  + geom_smooth(method="loess", se=FALSE) + theme_classic(base_size = 12, base_family = "Helvetica") +  ylim(0,1) +  theme(
      axis.line.x = element_line(colour = "black"),
      axis.line.y = element_line(colour = "black")
    )+theme(axis.text = element_text(size=12, colour="black"), axis.title=element_text(size=12, colour="black")) + theme(legend.position = c(.2, .9), legend.title=element_blank(), legend.text=element_text(size=12)) +
    ggtitle(title)
  
  ggsave(paste0(id_number, "dynamicdosecurves.pdf"), path = "../Data/Plots")
  #dev.off()
  
  #Make the average treatment effect graph
  ategraph <- tibble(est = unlist(estimates$ATE_estimate), 
                     lci = unlist(estimates$ATE_lowerCI), 
                     uci = unlist(estimates$ATE_upperCI), 
                     week = unlist(estimates$Outcome_week))
  #pdf("../Data/Plots/dynamicdoseate.pdf")
  ggplot(ategraph, aes(x=week, y=est)) + 
    #facet_wrap(~estimand, scales="free", labeller=label_parsed) 
    geom_errorbar(width=.1, aes(ymin=lci, ymax=uci))  + 
    geom_point(size=3) +  ylab("Estimated difference in risk of relapse comparing dynamic to static dose" ) + xlab("Week")   + geom_hline(yintercept=0,linetype = 2)  + theme_classic(base_size = 12, base_family = "Helvetica") +    theme(
      axis.line.x = element_line(colour = "black"),
      axis.line.y = element_line(colour = "black")
    )+theme(axis.text = element_text(size=12, colour="black"), axis.title=element_text(size=12, colour="black")) +
    ggtitle(title)
  
  ggsave(paste0(id_number, "dynamicdoseate.pdf"), path = "../Data/Plots")
  #dev.off()
  while (!is.null(dev.list()))  dev.off()
}

#---------------- Function: make two tables -----------------#

make_table1 = function(prepared_info, mi_baseline_covariates, case) {
  dataset_missing_baseline_covariates = prepared_info$dataset_missing_baseline_covariates
  name = prepared_info$name
  baseline_data = mi_baseline_covariates$data
  max_weeks = prepared_info$max_weeks
  max_weeks = 24
  combined = inner_join(dataset_missing_baseline_covariates, baseline_data, by = c("who", "project", "medicine"))
  
  combined$ever_responsive_increase <- FALSE
  for (week in 4:24) {
    for (who in 1:nrow(combined)) {
      combined$ever_responsive_increase[who] <- 
        ifelse(combined[who, paste0("wk", week, ".dose_increase_this_week")] & 
                 combined[who, paste0("wk", week - 1, ".use_this_week")] &
                 combined[who, paste0("wk", week - 1, ".dose_this_week")] < cases[[case]][["dose_threshold"]], 
               TRUE, 
               combined$ever_responsive_increase[who])
    }
  }
  
  combined = combined %>% 
    mutate(ever_use = (select(., ends_with(".use_this_week")) %>% rowSums()) > 0,
           num_dose_increases = select(., ends_with(".dose_increase_this_week")) %>% rowSums(),
           ever_increase = num_dose_increases > 0,
          #ever_responsive_increase = dose_increase_this_week & use_this_week -1,
           max_dose = pmax(!!!rlang::syms(paste0("wk", 1:max_weeks, ".dose_this_week"))), #select(., ends_with(".dose_this_week")) %>% max(),
           week_of_relapse = floor(as.numeric(relapse_date - rand_dt) / 7),
           ever_relapse = !!sym(paste0("wk", max_weeks, ".relapse_this_week")) == 1,
          Never_use_Never_increase = !ever_use & !ever_increase,
          Never_use_Ever_increase = !ever_use & ever_increase,
          Use_Never_increase = ever_use & !ever_increase,
          Use_nonresponsive_increase = ever_use & ever_increase,
          Use_Responsive_increase = ever_use & ever_responsive_increase
    ) %>% 
    select(-all_of(starts_with("wk")))
  
  allT = combined
  Never_use_Never_increaseT = combined %>% filter(Never_use_Never_increase)
  Never_use_Ever_increaseT = combined %>% filter(Never_use_Ever_increase)
  Use_Never_increaseT = combined %>% filter(Use_Never_increase)
  Use_nonresponsive_increaseT = combined %>% filter(Use_nonresponsive_increase)
  Use_Responsive_increaseT = combined %>% filter(Use_Responsive_increase)
  
  categories = list(allT, Never_use_Never_increaseT, Never_use_Ever_increaseT,
              Use_Never_increaseT, Use_nonresponsive_increaseT, Use_Responsive_increaseT)
  
  table1 = data.frame(nrow = 21, ncol = 6)
  table1 = tibble(
    name = c("All", "Never use, never increase", "Never use, ever increase",
             "Use, never increase", "Use, non-responsive increase", "Use, responsive increase"),
    n = rep(0, 6),
    Women = rep(0, 6),
    `Age (years)` = rep("0", 6),
    `Race/ethnicity: Non-Hispanic White` = rep(0, 6),
    `Race/ethnicity: Non-Hispanic African American` = rep(0, 6),
    `Race/ethnicity: Hispanic` = rep(0, 6),
    `Race/ethnicity: Other (including multiracial)` = rep(0, 6),
    `Current IV drug use` = rep(0, 6),
    `Alcohol use disorder` = rep(0, 6),
    `Cocaine use disorder` = rep(0, 6),
    `Brain damage` = rep(0, 6),
    `History of psychiatric disorder` = rep(0, 6), #combine 5 cols
    `Cannabis use at baseline` = rep(0, 6),
    `Amphetamine use at baseline` = rep(0, 6),
    `Benzodiazepine use at baseline` = rep(0, 6),
    `Past opioid withdrawl discomfort (1-4)` = rep(0, 6),
    `Maximum dose (mg)` = rep("0", 6),
    `Number of dose increases` = rep("0", 6),
    `Week of relapse` = rep("0", 6),
    `Relapse by week 24` = rep(0, 6)
  )
  
  for (i in 1:length(categories)) {
    data <- categories[[i]]
    total <- nrow(data)
    
    table1$n[i] <- total
    table1$Women[i] = round(sum(data$sex == "female") / total * 100, 1)
    table1$`Age (years)`[i] = paste0(round(mean(data$age), 1), " (", round(sd(data$age), 1), ")")
    table1$`Race/ethnicity: Non-Hispanic White`[i] = round(sum(data$xrace == 1) / total * 100, 1)
    table1$`Race/ethnicity: Non-Hispanic African American`[i] = round(sum(data$xrace == 2) / total * 100, 1)
    table1$`Race/ethnicity: Hispanic`[i] = round(sum(data$xrace == 3) / total * 100, 1)
    table1$`Race/ethnicity: Other (including multiracial)`[i] = round(sum(data$xrace == 4) / total * 100, 1)
    table1$`Current IV drug use`[i] = round(sum(data$ivdrug == "yes", na.rm = TRUE) /
                                              (total - sum(is.na(data$ivdrug))) * 100, 1)
    table1$`Alcohol use disorder`[i] =  round(sum(data$alcdisorder == "yes", na.rm = TRUE) /
                                                (total - sum(is.na(data$alcdisorder))) * 100, 1)
    table1$`Cocaine use disorder`[i] =  round(sum(data$cocdisorder == "yes", na.rm = TRUE) /
                                                (total - sum(is.na(data$cocdisorder))) * 100, 1)
    table1$`Brain damage`[i] =  round(sum(data$hasBrainDamage == "yes", na.rm = TRUE) /
                                        (total - sum(is.na(data$hasBrainDamage))) * 100, 1)
    
    table1$`History of psychiatric disorder`[i] = round(sum(data$hasSchiz == "yes" |
                                                        data$hasAnxPan  == "yes" |
                                                        data$hasBipolar == "yes" |
                                                        data$hasEpilepsy == "yes" |
                                                        data$hasMajorDep == "yes",
                                                      na.rm = TRUE) / total * 100, 1) #combine 5 cols
    
    table1$`Cannabis use at baseline`[i] = round(sum(data$bcannabis30_base == "yes", na.rm = TRUE) /
                                                    (total - sum(is.na(data$bcannabis30_base))) * 100, 1)
    table1$`Amphetamine use at baseline`[i] = round(sum(data$bamphetamine30_base == "yes", na.rm = TRUE) /
                                                       (total - sum(is.na(data$bamphetamine30_base))) * 100, 1)
    table1$`Benzodiazepine use at baseline`[i] = round(sum(data$bbenzo30_base == "yes", na.rm = TRUE) /
                                                          (total - sum(is.na(data$bbenzo30_base))) * 100, 1)
    table1$`Past opioid withdrawl discomfort (1-4)`[i] = paste0(round(mean(as.numeric(data$hwithdraw), na.rm = TRUE), 1), 
                                                                " (", round(sd(as.numeric(data$hwithdraw), na.rm = TRUE), 1), ")")
    table1$`Maximum dose (mg)`[i] = paste0(round(mean(data$max_dose), 1), " (", round(sd(data$max_dose), 1), ")")
    table1$`Number of dose increases`[i] = paste0(round(mean(data$num_dose_increases), 1), " (", round(sd(data$num_dose_increases), 1), ")")
    table1$`Week of relapse`[i] = paste0(round(mean(data$week_of_relapse), 1), " (", round(sd(data$week_of_relapse), 1), ")")
    table1$`Relapse by week 24`[i] = round(sum(data$ever_relapse) / total * 100, 1)
  }
  
  table1 = as.data.frame(t(table1))
  
  table1
}

# ----------------------------------------------------------
# JULY 8 TESTING -------------------------------------------
# ----------------------------------------------------------

# Compare alt and regular outcome definitions

# case = weekly_data_for_ltmle_04[[1]]
# ALT_case = ALT_weekly_data_for_ltmle_04[[1]]
# 
# plot_ltmle(run_ltmle_case(case, patients_imputed_03), case$name, paste0("regular_case"))
# plot_ltmle(run_ltmle_case(ALT_case, ALT_patients_imputed_03), case$name, paste0("ALT_case"))
# 
# plot_ltmle(z, case1$name, paste0("thurs7-15"))

case_names = list()
for (case in cases) {
  case_names = c(case_names, case[["name"]])
}

############## Just to find best set 8-5 ##############

all_proj_wk12_results_table = combine_estimates_across_imputations(ALT_weekly_data_for_ltmle_04[[13]], 
                                                               ALT_patients_imputed_03, 
                                                               week_cutoff = 24,
                                                               only_first_iter = TRUE)

plot_ltmle(all_proj_wk12_results_table, cases[[13]]$name, paste0("c", i, "-8-5"))

results = list()
for (i in 20:20) {
#for (i in 1:length(ALT_weekly_data_for_ltmle_04)) {
  #i = 13
  case = cases[[1]] #cases[[i]]
  data_for_analysis = ALT_weekly_data_for_ltmle_04[[1]] #[[i]]
 # plot_ltmle(combine_estimates_across_imputations(data_for_analysis, ALT_patients_imputed_03), case$name, paste0("c", i, "-8-4"))

  this_case_results_table = combine_estimates_across_imputations(data_for_analysis, 
                                                                 ALT_patients_imputed_03, 
                                                                 week_cutoff = 20, #24,
                                                                 only_first_iter = TRUE)
  #result13 = this_case_results_table
  
  results = c(results, this_case_results_table)
  
  plot_ltmle(this_case_results_table, case$name, paste0("c", i, "-8-24"))
  #table = case$weekly_counts %>% mutate(dose_mean = round(dose_mean, 2))
  #transposed = as_tibble(cbind(nms = names(table), t(table)))
 # write_csv(make_tables(data_for_analysis, ALT_patients_imputed_03), paste0("../Data/Plots/c", i, "-7-29counts.csv"))
}
names(results) <- case_names[[1]]

### MAKE TABLE1s, convert to latex
# bup_table1 = make_table1(ALT_weekly_data_for_ltmle_04[[1]], ALT_patients_imputed_03, 1)
# xtable(bup_table1)
# met_table1 = make_table1(ALT_weekly_data_for_ltmle_04[[8]], ALT_patients_imputed_03, 8)
# xtable(met_table1)

### MAKE weekly tables, convert to latex
weekly_tables = vector(mode = "list", length = length(cases))
for (case in 1:length(cases)) {
  weekly_table <- ALT_weekly_data_for_ltmle_04[[i]]$weekly_counts %>% mutate(dose_mean = round(dose_mean, 2))
  transposed <- as_tibble(cbind(nms = names(weekly_table), t(weekly_table)))
  transposed[11,2] <- "0" #new relapses this week should be zero in the first week
  transposed = transposed[c(1:5,11,6:10),]
  names(transposed) <- NULL
  
  nice_names = c("Week",                 
  "Patients (n)",          
  "Dose increase",
  "Use last week",         
  "Treatment",    
  "New relapse",  
  "Dose mean",              
  "Dose median",            
  "Dose min",               
  "Dose max",               
  "Dose iqr") 
  
  transposed[1] <- nice_names
  
  weekly_tables[[case]] <- transposed
  
  print(xtable(transposed), include.rownames = FALSE)
}


### SAVE TABLES
result_tables = list(
  bup_table1,
  met_table1,
  weekly_tables
)

save(
  weekly_tables, #result_tables,
  file = "../Data/result_tables_small_set_8-24.RData"
)

save(
  results,
  file = "../Data/results_small_set_8-24.RData"
)

###
save(
  weekly_tables, #result_tables,
  file = "../Data/result_tables-GOODWEEKLY.RData"
)

save(
  results,
  file = "../Data/RESULTSthru7-glmOnly.RData"
)

