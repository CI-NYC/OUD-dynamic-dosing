#ltmle analysis

library(ltmle)
library(mitools)

#set random seed for reproduceability
set.seed(100)

load("../Data/clean_combined_data.RData")


#---------------- Function: run LTMLE on each eligible outcome week -----------------#

run_ltmle_case = function(prepared_info, mi_baseline_covariates) {
  #Step 1: general prep
  #...1a: "unpack" the prepared info
  dataset_missing_baseline_covariates = prepared_info$dataset_missing_baseline_covariates %>% select(-medicine, -project)
  Anodes = prepared_info$Anodes
  Lnodes = prepared_info$Lnodes
  Ynodes = prepared_info$Ynodes
  abar1 = prepared_info$abar1
  abar0 = prepared_info$abar0
  outcome_weeks = prepared_info$outcome_weeks
  name = prepared_info$name

  #...1b: "unpack" the imputed baseline data
  #TODO: turn this into a loop
  baseline_covariates = complete(mi_baseline_covariates, action = 1) %>%
    select(all_of(c(demog, comorbidities)), site)

  #...1c: add baseline covariates into the dataset
  all_data = inner_join(baseline_covariates,
                        dataset_missing_baseline_covariates,
                        by = "who")

 # all_data = dataset_missing_baseline_covariates
  
  #...1d: create helper variables
  all_cols = colnames(all_data)
  baseline_cols = colnames(baseline_covariates)

  # #...1e: create an empty vector where we'll store the output from each week's LTMLE estimation
  txest = vector("list", length(outcome_weeks))
  txlci = vector("list", length(outcome_weeks))
  txuci = vector("list", length(outcome_weeks))
  cntest = vector("list", length(outcome_weeks))
  cntlci = vector("list", length(outcome_weeks))
  cntuci = vector("list", length(outcome_weeks))
  est = vector("list", length(outcome_weeks))
  lci = vector("list", length(outcome_weeks))
  uci = vector("list", length(outcome_weeks))
  rrest = vector("list", length(outcome_weeks))
  rrlci = vector("list", length(outcome_weeks))
  rruci = vector("list", length(outcome_weeks))
  

  #Step 2: iterate through all outcome weeks to get estimations
 for (i in 1:length(outcome_weeks)) {

   # for (i in 1:5) {
 #  i = 5
   
    #...2a: prepare the data for use in the LTMLE function:
    #remove any columns that weren't used as A-, L-, or Y- nodes
    unused = setdiff(all_cols, c(baseline_cols, Anodes[[i]], Lnodes[[i]], Ynodes[1:i]))
    
    ltmle_data = all_data %>%
      select(-all_of(unused)) %>%
      relocate(baseline_cols, Lnodes[[i]], Anodes[[i]], Ynodes[1:i]) %>% 
      select(-who)

    ltmle_data = as.data.frame(ltmle_data)
    
    #...2b: run the ltmle function
    output <- ltmle(
      data = ltmle_data,
      Anodes = Anodes[[i]],
      Lnodes = Lnodes[[i]],
      Ynodes = Ynodes[1:i],
      survivalOutcome = TRUE, #TRUE means the outcome is an event that can only occur once.
      abar = list(abar1[,1:i, drop = FALSE], abar0[,1:i, drop = FALSE]),
      estimate.time = FALSE, #if TRUE, this would just compute a rough estimate of runtime based on first 50 obs
      SL.library = NULL #if NULL, glm will be used. later, switch to super learner later
      #SL.library = c(SL.glm, SL.mean),
      #SL.library = SL.library,
      #variance.method = "ic"
    )

    #...2c: keep the output
    sumout = summary(output)

    txest[i] <- sumout$effect.measures$treatment$estimate
    txlci[i] <- sumout$effect.measures$treatment$CI[1]
    txuci[i] <- sumout$effect.measures$treatment$CI[2]

    cntest[i] <- sumout$effect.measures$control$estimate
    cntlci[i] <- sumout$effect.measures$control$CI[1]
    cntuci[i] <- sumout$effect.measures$control$CI[2]
    
    est[i] <- sumout$effect.measures$ATE$estimate
    lci[i] <- sumout$effect.measures$ATE$CI[1]
    uci[i] <- sumout$effect.measures$ATE$CI[2]
    
    rrest[i] <- sumout$effect.measures$RR$estimate
    rrlci[i] <- sumout$effect.measures$RR$CI[1]
    rruci[i] <- sumout$effect.measures$RR$CI[2]
  }

  #Step 3: package output together to return
  estimates = tibble(
    Outcome_week = outcome_weeks,
    Treatment_estimate = txest, 
    Treatment_lowerCI = txlci, 
    Treatment_upperCI = txuci, 
    Control_estimate = cntest, 
    Control_lowerCI = cntlci,
    Control_upperCI = cntuci,
    ATE_estimate = est,
    ATE_lowerCI = lci,
    ATE_upperCI = uci,
    RR_estimate = rrest,
    RR_lowerCI = rrlci,
    RR_upper_CI = rruci
  )

  estimates
}


#---------------- Function: add imputed data & covariates list -----------------#

# do this all on a dataset missing demographics, treatment, etc.
# then at the end: add in 5x from imputed data


#---------------- Function: make a plot of the results -----------------#

#TODO: deal with the list/unlist problem in the first function, instead of this one
#TODO: check if the order of columns in the first function is working as expected (by looking into summary(output))
#TODO: re-do Anodes, Lnodes, Ynodes based on last week's conversation
#TODO: incorporate checks about which weeks dont have enough data
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

plot_ltmle(run_ltmle_case(test_case_1, patients_imputed_03), test_case_1$name, 01)
plot_ltmle(run_ltmle_case(test_case_4, patients_imputed_03), test_case_4$name, 04)

# define a set of rule / filter combos
# for each of them:
#   * function: run prep functions on weekly data, then run the ltmle analysis, looking weekly, and saving data somewhere (output?)
#   * function: call the function above 5 times for the 5 imputed datasets, then merge them with mitools
#   * function: make a plot of the merged results