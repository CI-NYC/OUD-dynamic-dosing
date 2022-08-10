suppressPackageStartupMessages(library(tidyverse))

source("scripts/covariates.R")
source("scripts/04_lmtp_formatting.R")

before_relapse <- filter(visits, relapse_this_week == 0, week_of_intervention >= 2)

strata <- group_by(before_relapse, who) |> 
  summarise(relapse_week = min(max(week_of_intervention) + 1, 24), 
            no_increases = sum(dose_increase_this_week),
            max_dose = max(dose_this_week),
            any_use = any(as.logical(use_this_week)), 
            any_increase = any(as.logical(dose_increase_this_week))) |> 
  mutate(relapse_week12 = relapse_week <= 12)

baseline <- readRDS("data/drv/clean_visits_with_relapse_080922.rds") |> 
  filter(week_of_intervention <= 12, 
         !switched_meds & !never_initiated, 
         medicine %in% c("bup", "met")) |> 
  select(who, any_of(c(demog, treatment_info, comorbidities))) |> 
  distinct(who, .keep_all = TRUE) |> 
  left_join(strata) |> 
  select(-switched_meds, -never_initiated, -rand_dt, -end_of_detox, -who, -site) 

pmean = \(x, ...) sprintf("%0.1f\\%%", mean(x, na.rm = TRUE) * 100)
cmean = \(x, ...) sprintf("%.2f", mean(x, na.rm = TRUE))
csd = \(x, ...) sprintf("%.2f", sd(x, na.rm = TRUE))

stats = function(data, ...) {
  summarise(data,
    n = n(), 
    trial_27 = pmean(project == "27"), 
    trial_30 = pmean(project == "30"), 
    trial_51 = pmean(project == "51"),
    women = pmean(sex == "female"), 
    age_sd = csd(age), 
    age = cmean(age), 
    race_1 = pmean(xrace == "1"), 
    race_2 = pmean(xrace == "2"), 
    race_3 = pmean(xrace == "3"), 
    race_4 = pmean(xrace == "4"), 
    iv = pmean(ivdrug), 
    alc = pmean(alcdisorder), 
    coc = pmean(cocdisorder), 
    brain_damage = pmean(hasBrainDamage), 
    epilepsy = pmean(hasEpilepsy), 
    schiz = pmean(hasSchiz), 
    bipolar = pmean(hasBipolar), 
    anxiety = pmean(hasAnxPan), 
    cannabis = pmean(bcannabis30_base), 
    meth = pmean(bamphetamine30_base), 
    benzo = pmean(bbenzo30_base), 
    hwithdraw_sd = csd(as.numeric(hwithdraw)),
    hwithdraw = cmean(as.numeric(hwithdraw)), 
    rweek_sd = csd(relapse_week),
    rweek = cmean(relapse_week), 
    no_increases_sd = csd(no_increases),
    no_increases = cmean(no_increases), 
    dose_sd = csd(max_dose),
    dose = cmean(max_dose), 
    w12 = pmean(relapse_week12)
  )
}

bup = stats(filter(baseline, medicine == "bup"))
met = stats(filter(baseline, medicine == "met"))
bnu = stats(filter(baseline, medicine == "bup", !any_use))
bu = stats(filter(baseline, medicine == "bup", any_use))
bni = stats(filter(baseline, medicine == "bup", !any_increase))
bi = stats(filter(baseline, medicine == "bup", any_increase))
mnu = stats(filter(baseline, medicine == "met", !any_use))
mu = stats(filter(baseline, medicine == "met", any_use))
mni = stats(filter(baseline, medicine == "met", !any_increase))
mi = stats(filter(baseline, medicine == "met", any_increase))

# Produces LaTeX for table 1, written to the clipboard
glue::glue(
  "\\begin{table}
  \\caption{Descriptive statistics for those initiating treatment with BUP-NX or methadone.}
  \\centering\\footnotesize
  \\begin{tabular}[t]{lccccc}
  \\toprule
  & All & Never used & Used & Never increased & Increased \\\\ 
  \\midrule
  \\addlinespace[0.3em]
  \\multicolumn{6}{l}{\\textbf{BUP-NX}} \\\\ 
  \\midrule
  N & <bup$n> & <bnu$n> & <bu$n> & <bni$n> & <bi$n> \\\\ 
  \\multicolumn{6}{l}{Trial} \\\\ 
  \\hspace{1em} CTN0027 & <bup$trial_27> & <bnu$trial_27> & <bu$trial_27> & <bni$trial_27> & <bi$trial_27> \\\\ 
  \\hspace{1em} CTN0030 & <bup$trial_30> & <bnu$trial_30> & <bu$trial_30> & <bni$trial_30> & <bi$trial_30> \\\\ 
  \\hspace{1em} CTN0051 & <bup$trial_51> & <bnu$trial_51> & <bu$trial_51> & <bni$trial_51> & <bi$trial_51> \\\\ 
  Age & <bup$age> (<bup$age_sd>) & <bnu$age> (<bnu$age_sd>) & <bu$age> (<bu$age_sd>) & <bni$age> (<bni$age_sd>) & <bi$age> (<bi$age_sd>) \\\\ 
  Women & <bup$women> & <bnu$women> & <bu$women> & <bni$women> & <bi$women> \\\\ 
  \\multicolumn{6}{l}{Race/ethnicity} \\\\ 
  \\hspace{1em} Non-Hispanic white & <bup$race_1> & <bnu$race_1> & <bu$race_1> & <bni$race_1> & <bi$race_1> \\\\ 
  \\hspace{1em} Non-Hispanic Black & <bup$race_2> & <bnu$race_2> & <bu$race_2> & <bni$race_2> & <bi$race_2> \\\\ 
  \\hspace{1em} Hispanic & <bup$race_3> & <bnu$race_3> & <bu$race_3> & <bni$race_3> & <bi$race_3> \\\\ 
  \\hspace{1em} Other (including multiracial) & <bup$race_4> & <bnu$race_4> & <bu$race_4> & <bni$race_4> & <bi$race_4> \\\\ 
  Current IV drug use & <bup$iv> & <bnu$iv> & <bu$iv> & <bni$iv> & <bi$iv> \\\\ 
  Current cannabis use & <bup$cannabis> & <bnu$cannabis> & <bu$cannabis> & <bni$cannabis> & <bi$cannabis> \\\\ 
  Current amphetamine use & <bup$meth> & <bnu$meth> & <bu$meth> & <bni$meth> & <bi$meth> \\\\ 
  Current benzodiazepine drug use & <bup$benzo> & <bnu$benzo> & <bu$benzo> & <bni$benzo> & <bi$meth> \\\\ 
  Alcohol use disorder & <bup$alc> & <bnu$alc> & <bu$alc> & <bni$alc> & <bi$alc> \\\\ 
  Cocaine use disorder & <bup$coc> & <bnu$coc> & <bu$coc> & <bni$coc> & <bi$coc> \\\\ 
  Neurological injury & <bup$brain_damage> & <bnu$brain_damage> & <bu$brain_damage> & <bni$brain_damage> & <bi$brain_damage> \\\\ 
  History of epilepsy & <bup$epilepsy> & <bnu$epilepsy> & <bu$epilepsy> & <bni$epilepsy> & <bi$epilepsy> \\\\ 
  History of schizophrenia & <bup$schiz> & <bnu$schiz> & <bu$schiz> & <bni$schiz> & <bi$schiz> \\\\ 
  History of bipolar disorder & <bup$bipolar> & <bnu$bipolar> & <bu$bipolar> & <bni$bipolar> & <bi$bipolar> \\\\ 
  History of anxiety disorder & <bup$anxiety> & <bnu$anxiety> & <bu$anxiety> & <bni$anxiety> & <bi$anxiety> \\\\ 
  Opioid withdrawl discomfort (1-4) & <bup$hwithdraw> (<bup$hwithdraw_sd>) & <bnu$hwithdraw> (<bnu$hwithdraw_sd>) & <bu$hwithdraw> (<bu$hwithdraw_sd>) & <bni$hwithdraw> (<bni$hwithdraw_sd>) & <bi$hwithdraw> (<bi$hwithdraw_sd>) \\\\ 
  Max dose (mg) & <bup$dose> (<bup$dose_sd>) & <bnu$dose> (<bnu$dose_sd>) & <bu$dose> (<bu$dose_sd>) & <bni$dose> (<bni$dose_sd>) & <bi$dose> (<bi$dose_sd>) \\\\ 
  No. dose increases & <bup$no_increases> (<bup$no_increases_sd>) & <bnu$no_increases> (<bnu$no_increases_sd>) & <bu$no_increases> (<bu$no_increases_sd>) & <bni$no_increases> (<bni$no_increases_sd>) & <bi$no_increases> (<bi$no_increases_sd>) \\\\ 
  Week of relapse & <bup$rweek> (<bup$rweek_sd>) & <bnu$rweek> (<bnu$rweek_sd>) & <bu$rweek> (<bu$rweek_sd>) & <bni$rweek> (<bni$rweek_sd>) & <bi$rweek> (<bi$rweek_sd>) \\\\ 
  Relapse by week 12 & <bup$w12> & <bnu$w12> & <bu$w12> & <bni$w12> & <bi$w12> \\\\ 
  \\addlinespace[0.3em]
  \\midrule
  \\multicolumn{6}{l}{\\textbf{Methadone}} \\\\ 
  \\midrule
  N & <met$n> & <mnu$n> & <mu$n> & <mni$n> & <mi$n> \\\\ 
  Age & <met$age> (<met$age_sd>) & <mnu$age> (<mnu$age_sd>) & <mu$age> (<mu$age_sd>) & <mni$age> (<mni$age_sd>) & <mi$age> (<mi$age_sd>) \\\\ 
  Women & <met$women> & <mnu$women> & <mu$women> & <mni$women> & <mi$women> \\\\ 
  \\multicolumn{6}{l}{Race/ethnicity} \\\\ 
  \\hspace{1em} Non-Hispanic white & <met$race_1> & <mnu$race_1> & <mu$race_1> & <mni$race_1> & <mi$race_1> \\\\ 
  \\hspace{1em} Non-Hispanic Black & <met$race_2> & <mnu$race_2> & <mu$race_2> & <mni$race_2> & <mi$race_2> \\\\ 
  \\hspace{1em} Hispanic & <met$race_3> & <mnu$race_3> & <mu$race_3> & <mni$race_3> & <mi$race_3> \\\\ 
  \\hspace{1em} Other (including multiracial) & <met$race_4> & <mnu$race_4> & <mu$race_4> & <mni$race_4> & <mi$race_4> \\\\ 
  Current IV drug use & <met$iv> & <mnu$iv> & <mu$iv> & <mni$iv> & <mi$iv> \\\\ 
  Current cannabis use & <met$cannabis> & <mnu$cannabis> & <mu$cannabis> & <mni$cannabis> & <mi$cannabis> \\\\ 
  Current amphetamine use & <met$meth> & <mnu$meth> & <mu$meth> & <mni$meth> & <mi$meth> \\\\ 
  Current benzodiazepine drug use & <met$benzo> & <mnu$benzo> & <mu$benzo> & <mni$benzo> & <mi$meth> \\\\ 
  Alcohol use disorder & <met$alc> & <mnu$alc> & <mu$alc> & <mni$alc> & <mi$alc> \\\\ 
  Cocaine use disorder & <met$coc> & <mnu$coc> & <mu$coc> & <mni$coc> & <mi$coc> \\\\ 
  Neurological injury & <met$brain_damage> & <mnu$brain_damage> & <mu$brain_damage> & <mni$brain_damage> & <mi$brain_damage> \\\\ 
  History of epilepsy & <met$epilepsy> & <mnu$epilepsy> & <mu$epilepsy> & <mni$epilepsy> & <mi$epilepsy> \\\\ 
  History of schizophrenia & <met$schiz> & <mnu$schiz> & <mu$schiz> & <mni$schiz> & <mi$schiz> \\\\ 
  History of bipolar disorder & <met$bipolar> & <mnu$bipolar> & <mu$bipolar> & <mni$bipolar> & <mi$bipolar> \\\\ 
  History of anxiety disorder & <met$anxiety> & <mnu$anxiety> & <mu$anxiety> & <mni$anxiety> & <mi$anxiety> \\\\ 
  Opioid withdrawl discomfort (1-4) & <met$hwithdraw> (<met$hwithdraw_sd>) & <mnu$hwithdraw> (<mnu$hwithdraw_sd>) & <mu$hwithdraw> (<mu$hwithdraw_sd>) & <mni$hwithdraw> (<mni$hwithdraw_sd>) & <mi$hwithdraw> (<mi$hwithdraw_sd>) \\\\ 
  Max dose (mg) & <met$dose> (<met$dose_sd>) & <mnu$dose> (<mnu$dose_sd>) & <mu$dose> (<mu$dose_sd>) & <mni$dose> (<mni$dose_sd>) & <mi$dose> (<mi$dose_sd>) \\\\ 
  No. dose increases & <met$no_increases> (<met$no_increases_sd>) & <mnu$no_increases> (<mnu$no_increases_sd>) & <mu$no_increases> (<mu$no_increases_sd>) & <mni$no_increases> (<mni$no_increases_sd>) & <mi$no_increases> (<mi$no_increases_sd>) \\\\ 
  Week of relapse & <met$rweek> (<met$rweek_sd>) & <mnu$rweek> (<mnu$rweek_sd>) & <mu$rweek> (<mu$rweek_sd>) & <mni$rweek> (<mni$rweek_sd>) & <mi$rweek> (<mi$rweek_sd>) \\\\ 
  Relapse by week 12 & <met$w12> & <mnu$w12> & <mu$w12> & <mni$w12> & <mi$w12> \\\\ 
  \\bottomrule
  \\label{tab:t1}
  \\end{tabular}
  \\end{table}",
  .open =  "<", .close = ">"
) |> 
  clipr::write_clip()
