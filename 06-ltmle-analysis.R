#ltmle analysis

library(ltmle)
library(mitools)

#set random seed for reproduceability
set.seed(100)


#NOTE: in a future function, we'll need to:
# * add baseline covariates into the dataset
# * iterate through the weeks 1 by 1 to run LTMLE, using the right parts of Anodes etc. and filtering data columns



#---------------- Function: run LTMLE on each eligible outcome week -----------------#

run_ltmle_case = function(prepared_info, mi_baseline_covariates) {
  #########DELETE########
  prepared_info = test_case_1
  mi_baseline_covariates = patients_imputed_03
  
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
 #   select(-relapse_date, -trt, -project, -medicine, -rand_dt)
  
  #...1c: add baseline covariates into the dataset
  all_data = inner_join(baseline_covariates,
                        dataset_missing_baseline_covariates,
                        by = "who")
  
  #...1d: create helper variables
  all_cols = colnames(all_data)
  baseline_cols = colnames(baseline_covariates)
  
  # #...1e: create an empty vector where we'll store the output from each week's LTMLE estimation
  # ltmle_OUT = vector("list", length(outcome_weeks))
  
  
  #Step 2: iterate through all outcome weeks to get estimations
  for (i in 2:2) {
 # for (i in 1:length(outcome_weeks)) {
    
    #...2a: prepare the data for use in the LTMLE function:
    # (1) remove any columns that weren't used as A-, L-, or Y- nodes
    unused = setdiff(all_cols, c(baseline_cols, Anodes[[i]], Lnodes[[i]], Ynodes[1:i]))
    
    ltmle_data = all_data %>% 
      select(-all_of(unused)) %>% 
      #TODO: remove baseline cols we don't want
      select(-who)

    #...2b: run the ltmle function
    output <- ltmle(
      data = ltmle_data,
      Anodes = Anodes[[i]],
      Lnodes = Lnodes[[i]],
      Ynodes = Ynodes[1:i],
      survivalOutcome = TRUE, #TRUE means the outcome is an event that can only occur once.
      #abar = list(abar1[,1:i, drop=FALSE], abar0[,1:i, drop=FALSE]),
      abar = list(abar1[,1:i, drop=FALSE], abar0[,1:i, drop=FALSE]),
      estimate.time = FALSE, #if TRUE, this would just compute a rough estimate of runtime based on first 50 obs
      SL.library = NULL #if NULL, glm will be used. later, switch to super learner later
      #SL.library = c(SL.glm, SL.mean),
      #SL.library = SL.library,
      #variance.method = "ic"
    )
  }
    #...2c: keep the output
    #ltmle_OUT[i]<-output

    sumout = summary(output)

    txest[i]<-sumout$effect.measures$treatment$estimate
    txlci[i]<-sumout$effect.measures$treatment$CI[1]
    txuci[i]<-sumout$effect.measures$treatment$CI[2]

    cntest[i]<-sumout$effect.measures$control$estimate
    cntlci[i]<-sumout$effect.measures$control$CI[1]
    cntuci[i]<-sumout$effect.measures$control$CI[2]
    
    # 
    # est[i]<-sumout$effect.measures$ATE$estimate
    # lci[i]<-sumout$effect.measures$ATE$CI[1]
    # uci[i]<-sumout$effect.measures$ATE$CI[2]
    # 
    # rrest[i]<-sumout$effect.measures$RR$estimate
    # rrlci[i]<-sumout$effect.measures$RR$CI[1]
    # rruci[i]<-sumout$effect.measures$RR$CI[2]
    # 
  }
  
  #Step 3: package output together to return
  estimate_names = c("txest", "txlci", "txuci", "cntest", "cntlci", "cntuci")
  estimates = c(txest, txlci, txuci, cntest, cntlci, cntuci)
  names(estimates) = estimate_names
  
  estimates
}

aaaa = run_ltmle_case(test_case_1, patients_imputed_03)


#---------------- Function: add imputed data & covariates list -----------------#

# do this all on a dataset missing demographics, treatment, etc.
# then at the end: add in 5x from imputed data


#---------------- Function: make a plot of the results -----------------#



# define a set of rule / filter combos
# for each of them:
#   * function: run prep functions on weekly data, then run the ltmle analysis, looking weekly, and saving data somewhere (output?)
#   * function: call the function above 5 times for the 5 imputed datasets, then merge them with mitools
#   * function: make a plot of the merged results