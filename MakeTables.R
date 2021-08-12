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

load("~/Documents/Columbia MPH (to back up)/DSI project/ForGitHub/Data/clean_combined_imputed_data.RData")
load("~/Documents/Columbia MPH (to back up)/DSI project/ForGitHub/Data/data_ready_for_ltmle.RData")

#---------------- Function: make two tables -----------------#

make_tables = function(prepared_info, mi_baseline_covariates) {
  # prepared_info = ALT_weekly_data_for_ltmle_04[[1]]
  # mi_baseline_covariates = ALT_patients_imputed_03

# #   dataset_missing_baseline_covariates = prepared_info$dataset_missing_baseline_covariates
  # name = prepared_info$name
  # baseline_data = mi_baseline_covariates$data
  # max_weeks = prepared_info$max_weeks
  # max_weeks = 24
  # combined = full_join(dataset_missing_baseline_covariates, baseline_data, by = c("who", "project", "medicine"))
  
  # #NOTE: ever_increase is based on the treatment, so if we run this for a case with a doseage threshold
  # # it'll be wrong / not necessarily clear what we're referring to
  # combined$ever_responsive_increase <- FALSE
  # for (week in 4:24) {
    # for (who in 1:nrow(combined)) {
      # combined$ever_responsive_increase[who] <- 
        # ifelse(combined[who, paste0("wk", week, ".dose_increase_this_week")] & combined[who, paste0("wk", week - 1, ".use_this_week")], 
               # TRUE, 
               # combined$ever_responsive_increase[who])
    # }
  # }
  
  # combined = combined %>% 
    # mutate(ever_use = (select(., ends_with(".use_this_week")) %>% rowSums()) > 0,
           # num_dose_increases = select(., ends_with(".dose_increase_this_week")) %>% rowSums(),
           # ever_increase = num_dose_increases > 0,
          # #ever_responsive_increase = dose_increase_this_week & use_this_week -1,
           # max_dose = pmax(!!!rlang::syms(paste0("wk", 1:max_weeks, ".dose_this_week"))), #select(., ends_with(".dose_this_week")) %>% max(),
           # week_of_relapse = floor(as.numeric(relapse_date - rand_dt) / 7),
           # ever_relapse = !!sym(paste0("wk", max_weeks, ".relapse_this_week")) == 1,
          # Never_use_Never_increase = !ever_use & !ever_increase,
          # Never_use_Ever_increase = !ever_use & ever_increase,
          # Use_Never_increase = ever_use & !ever_increase,
          # Use_nonresponsive_increase = ever_use & ever_increase,
          # Use_Responsive_increase = ever_use & ever_responsive_increase
    # )
  
  # table1 = combined %>% group_by(Never_use_Never_increase) %>% 
    # summarise(max_dose = mean(max_dose),
              # age = mean(age),
              # hasMajorDep = sum(hasMajorDep == "yes") / n())
  ###
  
  table1 = tibble()
  
  weekly_table = prepared_info$weekly_counts %>% mutate(dose_mean = round(dose_mean, 2))
  transposed = as_tibble(cbind(nms = names(weekly_table), t(weekly_table)))
  #write_csv(transposed, paste0("../Data/Plots/c", i, "counts.csv"))
  
  resulting_tables = list(table1, transposed)
  names(resulting_tables) = c("Table1", "WeeklyCounts")
  
  resulting_tables
}

case_names = list()
for (case in cases) {
	case_names = c(case_names, case[["name"]])
}
cat(case_names)

results_tables = list()
for (i in 1:1) {
	this_case_tables = make_tables(ALT_weekly_data_for_ltmle_04[[i]], ALT_patients_imputed_03)
	results_tables = c(results_tables, this_case_tables)
}
names(results_tables) <- case_names

# # #---------------- Function: run LTMLE on each eligible outcome week -----------------#

# run_ltmle_case = function(prepared_info, mi_baseline_covariates, imputation_num) {
  # #Step 1: general prep
  # #...1a: "unpack" the prepared info
  # dataset_missing_baseline_covariates = prepared_info$dataset_missing_baseline_covariates %>% select(., -medicine, -project)
  # Anodes = prepared_info$Anodes
  # Lnodes = prepared_info$Lnodes
  # Ynodes = prepared_info$Ynodes
  # abar1 = prepared_info$abar1
  # abar0 = prepared_info$abar0
  # outcome_weeks = prepared_info$outcome_weeks
  # name = prepared_info$name

  # #...1b: "unpack" the imputed baseline data
  # #TODO: turn this into a loop?
  # baseline_covariates = complete(mi_baseline_covariates, action = imputation_num) %>%
    # select(all_of(c(demog, comorbidities)), site)

  # #...1c: add baseline covariates into the dataset
  # all_data = inner_join(baseline_covariates,
                        # dataset_missing_baseline_covariates,
                        # by = "who")

  # #...1d: create helper variables
  # all_cols = colnames(all_data)
  # baseline_cols = colnames(baseline_covariates)

  # # #...1e: create an empty vector where we'll store the output from each week's LTMLE estimation
  # txest = vector("list", length(outcome_weeks))
  # txvar = vector("list", length(outcome_weeks))
  # # txlci = vector("list", length(outcome_weeks))
  # # txuci = vector("list", length(outcome_weeks))
  # cntest = vector("list", length(outcome_weeks))
  # cntvar = vector("list", length(outcome_weeks))
  # # cntlci = vector("list", length(outcome_weeks))
  # # cntuci = vector("list", length(outcome_weeks))
  # est = vector("list", length(outcome_weeks))
  # var = vector("list", length(outcome_weeks))
  # lci = vector("list", length(outcome_weeks))
  # uci = vector("list", length(outcome_weeks))
  # # rrest = vector("list", length(outcome_weeks))
  # # rrlci = vector("list", length(outcome_weeks))
  # # rruci = vector("list", length(outcome_weeks))
  

  # #Step 2: iterate through all outcome weeks to get estimations
 # for (i in 1:length(outcome_weeks)) {
    # #...2a: prepare the data for use in the LTMLE function:
    # #remove any columns that weren't used as A-, L-, or Y- nodes
    # unused = setdiff(all_cols, c(baseline_cols, Anodes[[i]], Lnodes[[i]], Ynodes[1:i]))
    
    # ltmle_data = all_data %>%
      # select(-all_of(unused)) %>%
      # relocate(baseline_cols, Lnodes[[i]], Anodes[[i]], Ynodes[1:i]) %>% 
      # select(-who)

    # ltmle_data = as.data.frame(ltmle_data)
    
    # abar_this_time = list(abar1[,1:length(Anodes[[i]]), drop = FALSE], abar0[,1:length(Anodes[[i]]), drop = FALSE])
      
    # #...2b: run the ltmle function
    # output <- ltmle(
      # data = ltmle_data,
      # Anodes = Anodes[[i]],
      # Lnodes = Lnodes[[i]],
      # Ynodes = Ynodes[1:i],
      # survivalOutcome = TRUE, #TRUE means the outcome is an event that can only occur once.
      # abar = abar_this_time,
      # #abar = list(abar1[,1:i, drop = FALSE], abar0[,1:i, drop = FALSE]),
      # estimate.time = FALSE, #if TRUE, this would just compute a rough estimate of runtime based on first 50 obs
      # #SL.library = NULL #if NULL, glm will be used. later, switch to super learner later
      # SL.library = c("SL.glm", "SL.mean", "SL.glmnet", "SL.earth", "SL.xgboost"),
      # #SL.library = SL.library
      # #SL.library = "default"
      # variance.method = "ic"
    # )

    # #...2c: keep the output
    # sumout = summary(output)

    # txest[i] <- sumout$effect.measures$treatment$estimate
    # txvar[i] <- sumout$effect.measures$treatment$std.dev ^ 2
    # # txlci[i] <- sumout$effect.measures$treatment$CI[1]
    # # txuci[i] <- sumout$effect.measures$treatment$CI[2]

    # cntest[i] <- sumout$effect.measures$control$estimate
    # cntvar[i] <- sumout$effect.measures$control$std.dev ^ 2
    # # cntlci[i] <- sumout$effect.measures$control$CI[1]
    # # cntuci[i] <- sumout$effect.measures$control$CI[2]
    
    # est[i] <- sumout$effect.measures$ATE$estimate
    # var[i] <- sumout$effect.measures$ATE$std.dev ^ 2
    # # lci[i] <- sumout$effect.measures$ATE$CI[1]
    # # uci[i] <- sumout$effect.measures$ATE$CI[2]
    
    # # rrest[i] <- sumout$effect.measures$RR$estimate
    # # rrlci[i] <- sumout$effect.measures$RR$CI[1]
    # # rruci[i] <- sumout$effect.measures$RR$CI[2]
  # }

  # #Step 3: package output together to return
  # estimates = tibble(
    # Outcome_week = outcome_weeks,
    # Treatment_estimate = txest, 
    # Treatment_variance = txvar, 
    # # Treatment_lowerCI = txlci, 
    # # Treatment_upperCI = txuci, 
    # Control_estimate = cntest,
    # Control_variance = cntvar, 
    # # Control_lowerCI = cntlci,
    # # Control_upperCI = cntuci,
    # ATE_estimate = est,
    # ATE_variance = var #,
    # # ATE_lowerCI = lci,
    # # ATE_upperCI = uci,
    # # RR_estimate = rrest,
    # # RR_lowerCI = rrlci,
    # # RR_upper_CI = rruci
  # )

  # estimates
# }

# # case1_results1 = run_ltmle_case(for_analysis_case1_ORIGINAL, ALT_patients_imputed_03, 1)
# # case1_results2 = run_ltmle_case(for_analysis_case1_ORIGINAL, ALT_patients_imputed_03, 2)
# # case1_results3 = run_ltmle_case(for_analysis_case1_ORIGINAL, ALT_patients_imputed_03, 3)
# # case1_results4 = run_ltmle_case(for_analysis_case1_ORIGINAL, ALT_patients_imputed_03, 4)


# #---------------- Function: combine results from different imputed datasets -----------------#

# # run the `run_ltmle_case` function m times (where m is the number of imputations)
# # pool the results
# # create confidence intervals from the provided variances

# combine_estimates_across_imputations = function(prepared_info, mi_baseline_covariates) {
  # num_imputations = mi_baseline_covariates$m #4
  
  # all_estimates = vector("list", length(num_imputations))
  # for (m in 1:num_imputations) {
    # all_estimates[[m]] = run_ltmle_case(prepared_info, mi_baseline_covariates, m)
  # }

  # # all_estimates[[1]] <- case1_results1
  # # all_estimates[[2]] <- case1_results2
  # # all_estimates[[3]] <- case1_results3
  # # all_estimates[[4]] <- case1_results3
  # # 
  
  # merged_all = full_join(all_estimates[[1]],
                         # all_estimates[[2]],
                         # by = "Outcome_week",
                         # suffix = c("1", "2"))
  
  # for (m in 3:num_imputations) {
    # merged_all = full_join(all_estimates[[m]],
                           # merged_all,
                           # by = "Outcome_week",
                           # suffix = c(as.character(m), "3")) #for some reason this is what works for labeling!
  # }
  
  # merged_all = merged_all %>% 
    # select("Outcome_week", sort(colnames(.)))
  
  # pooled_results = tibble(
    # Outcome_week = numeric(),
    # Treatment_estimate = numeric(),
    # Treatment_variance = numeric(),
    # Treatment_lowerCI = numeric(),
    # Treatment_upperCI = numeric(),
    # Control_estimate = numeric(),
    # Control_variance = numeric(),
    # Control_lowerCI = numeric(),
    # Control_upperCI = numeric(),
    # ATE_estimate = numeric(),
    # ATE_variance = numeric(),
    # ATE_lowerCI = numeric(),
    # ATE_upperCI = numeric(),
  # )
  
  # # I'm sure this could all be done much more efficiently! But I can't quite figure it out.
  # for (week in merged_all$Outcome_week) {
    # this_week_only = merged_all %>% filter(Outcome_week == week)
    
    # treatment_pool = MIcombine(results = as.list(unlist(
      # select(this_week_only, starts_with("Treatment_estimate")),
      # use.names = FALSE
    # )),
    # variances = as.list(unlist(
      # select(this_week_only, starts_with("Treatment_variance")),
      # use.names = FALSE
    # )))
    
    # control_pool = MIcombine(results = as.list(unlist(
      # select(this_week_only, starts_with("Control_estimate")),
      # use.names = FALSE
    # )),
    # variances = as.list(unlist(
      # select(this_week_only, starts_with("Control_variance")),
      # use.names = FALSE
    # )))
    
    # ATE_pool = MIcombine(results = as.list(unlist(
      # select(this_week_only, starts_with("ATE_estimate")),
      # use.names = FALSE
    # )),
    # variances = as.list(unlist(
      # select(this_week_only, starts_with("ATE_variance")),
      # use.names = FALSE
    # )))
    
    
    # pooled_results = pooled_results %>% 
      # add_row(
        # Outcome_week = week,
        # Treatment_estimate = treatment_pool$coefficients,
        # Treatment_variance = treatment_pool$variance,
        # Treatment_lowerCI = treatment_pool$coefficients - 1.96 * (treatment_pool$variance ^ .5),
        # Treatment_upperCI = treatment_pool$coefficients + 1.96 * (treatment_pool$variance ^ .5),
        # Control_estimate = control_pool$coefficients,
        # Control_variance = control_pool$variance,
        # Control_lowerCI = control_pool$coefficients - 1.96 * (control_pool$variance ^ .5),
        # Control_upperCI = control_pool$coefficients + 1.96 * (control_pool$variance ^ .5),
        # ATE_estimate = ATE_pool$coefficients,
        # ATE_variance = ATE_pool$variance,
        # ATE_lowerCI = ATE_pool$coefficients - 1.96 * (ATE_pool$variance ^ .5),
        # ATE_upperCI = ATE_pool$coefficients + 1.96 * (ATE_pool$variance ^ .5),
      # )
  # }
  
  # pooled_results
# }

# #z = combine_estimates_across_imputations(x, y)

# #---------------- Function: make a plot of the results -----------------#

# #TODO: deal with the list/unlist problem in the first function, instead of this one
# #TODO: check if the order of columns in the first function is working as expected (by looking into summary(output))
# #TODO: re-do Anodes, Lnodes, Ynodes based on last week's conversation
# #TODO: incorporate checks about which weeks dont have enough data
# #TODO: make a table (with # people at each week)

# #---DONE:
# #TODO: take out people with no relapse, don't count them as relapsed on last date
# #TODO: re-do who is excluded
# #TODO: prep for dynamic dosing rules with cutoffs



# plot_ltmle = function(estimates, title, id_number) {

  # #Create dataframe
  # datgraph <- tibble(
    # est = unlist(c(estimates$Treatment_estimate, estimates$Control_estimate)), 
    # lci = unlist(c(estimates$Treatment_lowerCI, estimates$Control_lowerCI)), 
    # uci = unlist(c(estimates$Treatment_upperCI, estimates$Control_upperCI)), 
    # week = unlist(rep(estimates$Outcome_week, 2)), 
    # tx = unlist(c(rep("dynamic dose increase", length(estimates$Outcome_week)), 
                  # rep("constant dose", length(estimates$Outcome_week))))
    # )
  
  # datgraph = as.data.frame(datgraph)
  
  # #Make the dynamic dose curves
  # pd <- position_dodge(0.5)
  # #pdf("dynamicdosecurves.pdf")
  # ggplot(datgraph, aes(x=week, y=est, linetype=tx, shape=tx, group=tx)) + 
    # #facet_wrap(~estimand, scales="free", labeller=label_parsed) 
    # #geom_errorbar(width=.1, aes(ymin=lci, ymax=uci), position=pd)  + 
    # geom_point(position=pd, size=3) +  ylab("Risk of Relapse to OUD") + xlab("Week")   + geom_hline(yintercept=0,linetype = 2)  + geom_smooth(method="loess", se=FALSE) + theme_classic(base_size = 12, base_family = "Helvetica") +  ylim(0,1) +  theme(
      # axis.line.x = element_line(colour = "black"),
      # axis.line.y = element_line(colour = "black")
    # )+theme(axis.text = element_text(size=12, colour="black"), axis.title=element_text(size=12, colour="black")) + theme(legend.position = c(.2, .9), legend.title=element_blank(), legend.text=element_text(size=12)) +
    # ggtitle(title)
  
  # ggsave(paste0(id_number, "dynamicdosecurves.pdf"), path = "../Data/Plots")
  # #dev.off()
  
  # #Make the average treatment effect graph
  # ategraph <- tibble(est = unlist(estimates$ATE_estimate), 
                     # lci = unlist(estimates$ATE_lowerCI), 
                     # uci = unlist(estimates$ATE_upperCI), 
                     # week = unlist(estimates$Outcome_week))
  # #pdf("../Data/Plots/dynamicdoseate.pdf")
  # ggplot(ategraph, aes(x=week, y=est)) + 
    # #facet_wrap(~estimand, scales="free", labeller=label_parsed) 
    # geom_errorbar(width=.1, aes(ymin=lci, ymax=uci))  + 
    # geom_point(size=3) +  ylab("Estimated difference in risk of relapse comparing dynamic to static dose" ) + xlab("Week")   + geom_hline(yintercept=0,linetype = 2)  + theme_classic(base_size = 12, base_family = "Helvetica") +    theme(
      # axis.line.x = element_line(colour = "black"),
      # axis.line.y = element_line(colour = "black")
    # )+theme(axis.text = element_text(size=12, colour="black"), axis.title=element_text(size=12, colour="black")) +
    # ggtitle(title)
  
  # ggsave(paste0(id_number, "dynamicdoseate.pdf"), path = "../Data/Plots")
  # #dev.off()
  # while (!is.null(dev.list()))  dev.off()
# }

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

# results = list()
# for (i in 1:1) {
# #for (i in 1:length(ALT_weekly_data_for_ltmle_04)) {
  # i = 13
  # case = cases[[i]]
  # data_for_analysis = ALT_weekly_data_for_ltmle_04[[i]]
  # plot_ltmle(combine_estimates_across_imputations(data_for_analysis, ALT_patients_imputed_03), case$name, paste0("c", i, "-8-4"))

  # this_case_results_table = combine_estimates_across_imputations(data_for_analysis, ALT_patients_imputed_03)
  # result13 = this_case_results_table
  
  # results = c(results, this_case_results_table)
  
  # plot_ltmle(this_case_results_table, case$name, paste0("c", i, "-8-4"))
  # #table = case$weekly_counts %>% mutate(dose_mean = round(dose_mean, 2))
  # #transposed = as_tibble(cbind(nms = names(table), t(table)))
 # # write_csv(make_tables(data_for_analysis, ALT_patients_imputed_03), paste0("../Data/Plots/c", i, "-7-29counts.csv"))
# }
# names(results) <- cases$names

# for (i in 9:12) {
#   case = weekly_data_for_ltmle_04[[i]]
#   plot_ltmle(run_ltmle_case(case, patients_imputed_03), case$name, paste0("c", i, "-"))
# 
#   table = case$weekly_counts %>% mutate(dose_mean = round(dose_mean, 2))
#   transposed = as_tibble(cbind(nms = names(table), t(table)))
#   write_csv(transposed, paste0("../Data/Plots/c", i, "counts.csv"))
# }

# plot_ltmle(run_ltmle_case(test_case_1, patients_imputed_03), test_case_1$name, "01b-")
# plot_ltmle(run_ltmle_case(test_case_2, patients_imputed_03), test_case_1$name, "02b-")
# plot_ltmle(run_ltmle_case(test_case_3, patients_imputed_03), test_case_1$name, "03b-")
# plot_ltmle(run_ltmle_case(test_case_4, patients_imputed_03), test_case_1$name, "04b-")
# plot_ltmle(run_ltmle_case(test_case_5, patients_imputed_03), test_case_5$name, "05b-")
# plot_ltmle(run_ltmle_case(test_case_6, patients_imputed_03), test_case_5$name, "06b-")
# plot_ltmle(run_ltmle_case(test_case_7, patients_imputed_03), test_case_5$name, "07b-")
# plot_ltmle(run_ltmle_case(test_case_8, patients_imputed_03), test_case_8$name, "08b-")

# define a set of rule / filter combos
# for each of them:
#   * function: run prep functions on weekly data, then run the ltmle analysis, looking weekly, and saving data somewhere (output?)
#   * function: call the function above 5 times for the 5 imputed datasets, then merge them with mitools
#   * function: make a plot of the merged results