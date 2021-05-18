#Codebook

# # FOR ADDING NEW VARIABLES:
# codebook = add_row(
#   codebook,
#   variable = "$$$VAR$$$",
#   description = "$$$DESC$$$",
#   values = "$$$VALS$$$",
#   provenance = "added_by_us",
#   calculated = TRUE,
#   level = "patient" / "visit",
#   in_project_27 = TRUE, #or FALSE
#   in_project_30 = TRUE, #or FALSE
#   in_project_51 = TRUE #or FALSE
# )

#---------------- Initial setup -----------------#

codebook = tibble(
  variable = character(),
  description = character(),
  values = character(),
  provenance = factor(), #baseline, druguse, added_by_us
  calculated = logical(), #TRUE=calculated, FALSE=measured
  level = factor(), #patient vs. visit
  in_project_27 = logical(),
  in_project_30 = logical(),
  in_project_51 = logical()
)

desc = data.frame(
  row.names = c(
    "who",
    "project",
    "isHispanic",
    "race",
    "opioiduse12",
    "fusedt12",
    "opioiduse24" ,
    "fusedt24" ,
    "sex" ,
    "age",
    "xrace",
    "hcows",
    "hwithdraw",
    "alcdisorder",
    "cocdisorder",
    "hasBrainDamage",
    "hasEpilepsy",
    "hasSchiz",
    "hasBipolar",
    "hasAnxPan",
    "hasMajorDep"   ,
    "site" ,
    "trt",
    "edu",
    "mar",
    "falcohol",
    "fdrug",
    "bamphetamine30_base",
    "bcannabis30_base",
    "bbenzo30_base",
    "ivdrug",
    "when",
    "mg",
    "medicine",
    "selfopioid",
    "uopioid",
    "rand_dt"
  ),
  description = c(
    "patient unique ID number",
    "project",
    "isHispanic",
    "race",
    "12-week survival outcome: relapse with non-study opioid use by week 12",
    "fused 12-week survival outcome: date of first non-study opioid use [OR ?? if no relapse]",
    "24-week survival outcome: relapse with non-study opioid use by week 24",
    "fused 24-week survival outcome: date of first non-study opioid use [OR ?? if no relapse]",
    "sex" ,
    "age",
    "xrace -- combined race & isHispanic",
    "hcows ?? -- a baseline measure of past experience with heroin withdrawl",
    "hwithdraw ?? -- a baseline measure of past experience with heroin withdrawl",
    "history of alcohol use disorder (past year)",
    "history of cocaine use disorder (past year)",
    "has brain damage",
    "has epilepsy",
    "has schizophrenia",
    "has bipolar disorder",
    "has anxiety or panic disorder",
    "has major depression",
    "site",
    "assigned treatment group (type of medicine)",
    "education",
    "marital status",
    "lives with someone with problematic alcohol use",
    "lives with someone with problematic drug use",
    "amphetamines in 30 days before baseline",
    "cannabis in 30 days before baseline",
    "benzodiazepines in 30 days before baseline",
    "IV drug use in 30 days before baseline",
    "visit date",
    "(DEPRECATED: use med-specific dose columns instead): mg of medication given at visit",
    "medicine type given at visit (usually matches with trt, but sometimes is different)",
    "self-reported opioid use",
    "urine opioid test result",
    "randomization date"
  ),
  values = c(
    "formatted as dddd-dd-dddd in projects 27 & 30, formatted as ddddddddddddd in project 51",
    "there are 3 projects: 27, 30, 51",
    "guess: 0=not hispanic, 1=hispanic",
    "race values: 1=white only, 2=african american only, 3=other only, 7=multiracial",
    "guess: 1=failure by 12 weeks, 0=no failure by 12 weeks",
    "fusedt12: date of 12-week relapse",
    "guess: 1=failure by 24 weeks, 0=no failure by 24 weeks" ,
    "fusedt24: date of 24-week relapse" ,
    "guess: 1=male, 2=female" ,
    "age in years",
    "xrace values: 1=non-hispanic white, 2=non-hispanic african american, 3=hispanic, 4=other",
    "hcows: a continuous value ranging from 0-58 ???",
    "hwithdraw values: 1=?, 2=?, 3=?, 4=?",
    "guess: 0=no alcohol use disorder, 1=yes alcohol use disorder",
    "guess: 0=no cocaine use disorder, 1=yes cocaine use disorder",
    "guess: 0=no brain damage, 1=yes brain damage",
    "guess: 0=no epilepsy, 1=yes epilepsy",
    "guess: 0=no schizophrenia, 1=yes schizophrenia",
    "guess: 0=no bipolar disorder, 1=yes bipolar disorder",
    "guess: 0=no anxiety disorder, 1=yes anxiety disorder",
    "guess: 0=no major depression, 1=yes major depression"   ,
    "site: 27 different sites",
    "treatment values: 1=methadone (ctn27), 2=bupenorphine (outpatient ctn27), 3=bupenorphine (outpatient ctn30), 4=bupenorphine (inpatient ctn51), 5=naltrexone (ctn51)",
    "education values: 1=<12, 2=12, 3=>12",
    "marriage values: 1=married, 2=div/sep/widow, 3=single",
    "guess: 0=no falcohol, 1=yes falcohol",
    "guess: 0=no fdrug, 1=yes fdrug",
    "guess: 0=no bamphetamine30_base, 1=yes bamphetamine30_base",
    "guess: 0=no bcannabis30_base, 1=yes bcannabis30_base",
    "guess: 0=no bbenzo30_base, 1=yes bbenzo30_base",
    "guess: 0=no ivdrug, 1=yes ivdrug",
    "when: date of visit",
    "mg: medicine doseage perscribed, in mg. bupenorphine range: 0-32mg (by 4s), methadone range: 0-150mg (by 10s). naltrexone is a shot, and always dose=1",
    "medicine: 1=methadone, 2=bupenorphine, 3=naltrexone",
    "selfopioid: 0=no self-reported opioid use since last visit, 1=yes self-reported opioid use since last visit",
    "uopioid: 0=negative urine opioid test, 1=positive urine opioid test",
    "date of randomization into one of the arms of the trial"
  )
)

for (var in colnames(baseline_data_raw)) {
  codebook = add_row(
    codebook,
    variable = var,
    description = desc[var,1],
    values = desc[var,2],
    provenance = "baseline",
    calculated = ifelse(
      var %in% c("opioiduse12", "opioiduse24", "fusedt12", "fusedt24"),
      TRUE,
      FALSE
    ),
    level = "patient",
    in_project_27 = ifelse(
      var %in% c("edu", "mar", "falcohol","fdrug"),
      FALSE,
      TRUE),
    in_project_30 = TRUE,
    in_project_51 = TRUE
  )
}

for (var in colnames(druguse_data_raw)) {
  if (!(var %in% codebook$variable)) {
    codebook = add_row(
      codebook,
      variable = var,
      description = desc[var,1],
      values = desc[var,2],
      provenance = "druguse",
      calculated = FALSE,
      level = ifelse(var == "rand_dt", "patient", "visit"),
      in_project_27 = TRUE,
      in_project_30 = TRUE,
      in_project_51 = TRUE
    )
  }
}

demog = c(
  "isHispanic",
  "race",
  "sex" ,
  "age",
  "xrace",
  "hcows",
  "hwithdraw",
  "alcdisorder",
  "cocdisorder",
  "hasBrainDamage",
  "hasEpilepsy",
  "hasSchiz",
  "hasBipolar",
  "hasAnxPan",
  "hasMajorDep"   ,
  "edu",
  "mar",
  "falcohol",
  "fdrug",
  "bamphetamine30_base",
  "bcannabis30_base",
  "bbenzo30_base",
  "ivdrug"
)

#---------------- New variables from 01-initial-data-cleaning -----------------#

codebook = add_row(
  codebook,
  variable = "never_initiated",
  description = "whether a patient never initiated treatment",
  values = "TRUE if the patient was randomized but never initiated treatment (never received medicine), FALSE otherwise",
  provenance = "added_by_us",
  calculated = TRUE,
  level = "patient",
  in_project_27 = TRUE,
  in_project_30 = TRUE,
  in_project_51 = TRUE
)

codebook = add_row(
  codebook,
  variable = "end_of_detox",
  description = "the date when treatment would start (whether or not the patient is still in the trial), after 3 weeks of medication-assisted detoxification",
  values = "date: 3 weeks after the patient's first visit. Note: this is the first date when a patient could be marked as relapsed.",
  provenance = "added_by_us",
  calculated = TRUE,
  level = "patient",
  in_project_27 = TRUE,
  in_project_30 = TRUE,
  in_project_51 = TRUE
)

codebook = add_row(
  codebook,
  variable = "day_of_intervention",
  description = "how many days the patient has been in the intervention (even if dropped out, etc)",
  values = "starts counting at 1. day 1 is randomization date",
  provenance = "added_by_us",
  calculated = TRUE,
  level = "visit",
  in_project_27 = TRUE,
  in_project_30 = TRUE,
  in_project_51 = TRUE
)

codebook = add_row(
  codebook,
  variable = "week_of_intervention",
  description = "how many weeks the patient has been in the intervention (even if dropped out, etc)",
  values = "starts counting at week 1",
  provenance = "added_by_us",
  calculated = TRUE,
  level = "visit",
  in_project_27 = TRUE,
  in_project_30 = TRUE,
  in_project_51 = TRUE
)

codebook = add_row(
  codebook,
  variable = "switched_meds",
  description = "whether a patient switched medicine during treatment. This only happend at project 27, between bupenorphine & methadone",
  values = "TRUE if the patient switched medicine at least once (inclusing eventually switching back), FALSE otherwise",
  provenance = "added_by_us",
  calculated = TRUE,
  level = "patient",
  in_project_27 = TRUE,
  in_project_30 = TRUE,
  in_project_51 = TRUE
)

codebook = add_row(
  codebook,
  variable = "med_switch_date",
  description = "the date on which a patient first switched medications (always between methadone & bupenorphine).",
  values = "a date if the patient switched medicine at least once (inclusing eventually switching back), NA otherwise",
  provenance = "added_by_us",
  calculated = TRUE,
  level = "patient",
  in_project_27 = TRUE,
  in_project_30 = TRUE,
  in_project_51 = TRUE
)

codebook = add_row(
  codebook,
  variable = "bup_dose",
  description = "mg doseage of bupenorphine given",
  values = "0-32, in multiples of 4 (or NA if on a different medicine)",
  provenance = "added_by_us",
  calculated = TRUE,
  level = "visit",
  in_project_27 = TRUE,
  in_project_30 = TRUE,
  in_project_51 = TRUE
)

codebook = add_row(
  codebook,
  variable = "naloxone_dose",
  description = "mg doseage of naloxone given (as a supplement to bupenorphine)",
  values = "0 or 2 if the patient is also getting bupenorphine, NA otherwise",
  provenance = "added_by_us",
  calculated = TRUE,
  level = "visit",
  in_project_27 = TRUE,
  in_project_30 = TRUE,
  in_project_51 = TRUE
)

codebook = add_row(
  codebook,
  variable = "met_dose",
  description = "mg doseage of methadone given",
  values = "0-150, in multiples of 10 (or NA if on a different medicine)",
  provenance = "added_by_us",
  calculated = TRUE,
  level = "visit",
  in_project_27 = TRUE,
  in_project_30 = TRUE,
  in_project_51 = TRUE
)

codebook = add_row(
  codebook,
  variable = "naltrexone_dose",
  description = "doseage of naltrexone injection given",
  values = "1 if injection given, NA otherwise",
  provenance = "added_by_us",
  calculated = TRUE,
  level = "visit",
  in_project_27 = TRUE,
  in_project_30 = TRUE,
  in_project_51 = TRUE
)

codebook = add_row(
  codebook,
  variable = "any_dose",
  description = "if any medicine doseage was given at this visit or not",
  values = "TRUE if something was given, FALSE otherwise",
  provenance = "added_by_us",
  calculated = TRUE,
  level = "visit",
  in_project_27 = TRUE,
  in_project_30 = TRUE,
  in_project_51 = TRUE
)

# NOTE: TODO - change this later once I've made a "better" dose-this-week function
codebook = add_row(
  codebook,
  variable = "dose_this_week",
  description = "the dose of medicine given on the first day of this week",
  values = "number of mg",
  provenance = "added_by_us",
  calculated = TRUE,
  level = "week",
  in_project_27 = TRUE,
  in_project_30 = TRUE,
  in_project_51 = TRUE
)

codebook = add_row(
  codebook,
  variable = "dose_increase_this_week",
  description = "TRUE if this week's dose is higher than last week's",
  values = "TRUE / FALSE",
  provenance = "added_by_us",
  calculated = TRUE,
  level = "week",
  in_project_27 = TRUE,
  in_project_30 = TRUE,
  in_project_51 = TRUE
)

#---------------- New variables from 02-defining-relapse-outcomes -----------------#

codebook = add_row(
  codebook,
  variable = "relapse_this_week",
  description = "TRUE if our calculated relapse date falls within this week",
  values = "TRUE / FALSE",
  provenance = "added_by_us",
  calculated = TRUE,
  level = "week",
  in_project_27 = TRUE,
  in_project_30 = TRUE,
  in_project_51 = TRUE
)

#---------------- New variables from 03-multiple-imputation -----------------#

#---------------- New variables from 04-ltmle-prep -----------------#
