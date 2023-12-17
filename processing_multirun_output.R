################################################

# File is organised as below:

# 1. Set-up section specifies functions and lists used later in code
# 2. Data combining section reads in data from individual-level and benefit unit-level SimPaths output files to create a single data.table
# 3. Data cleaning section creates ordered factor variables and integer variables from inputted data as required for analysis
# 4. Final section generates summary statistics for whole population and each required stratified subgroup

# It is unlikely that, if running SimPaths 500+ times, all analyses will be able to be combined at once with standard computing power
# Therefore, the code in section 2 can be easily modified to include only a specific subset of the analyses/scenarios

################################################

# 1. Set-up #
#############

# import libraries, it is assumed they've been installed already
library(data.table) # parallel data manipulation
library(gsubfn) # string templates
library(readstata13) # to read from .dta files
library(magrittr)
library(tibble)
library(dplyr)
library(tidyverse)
library(R.utils)
library(readr)
library(LaF)

# set the number of threads to decrease the time of file reading/writing
data.table::getDTthreads()
data.table::setDTthreads(8)

# a list of variables in the main table
main_out_colnames =  c('les_c4', 'out_ghqcase', 'dhm',
                       'equivalisedDisposableIncomeYearly', 'atRiskOfPoverty', 
                       'labourSupplyWeekly')

ids = c("scenario", "run", "time")

main_loop <- function(sid_, dft) {
  per_run <- get_summary_statistics(ids, dft, sid_)
  per_run <- rename_final_columns(per_run, sid_)
  return(per_run)
}

subgroup_loop <- function(sid_, dft) {
  per_run <- get_subgroup_summary_statistics(ids, dft, sid_)
  per_run <- rename_subgroup_final_columns(per_run, sid_)
  return(per_run)
}

rename_final_columns <- function(data_table_name, postfix) {
  names(data_table_name)[names(data_table_name) == 'out_ghqcase'] <- paste("out_ghqcase", postfix, sep="_")
  names(data_table_name)[names(data_table_name) == 'dhm'] <- paste("dhm", postfix, sep="_")
  names(data_table_name)[names(data_table_name) == 'les_c4'] <- paste("out_emp", postfix, sep="_")
  names(data_table_name)[names(data_table_name) == 'labourSupplyWeekly'] <- paste("out_emphrs", postfix, sep="_")
  names(data_table_name)[names(data_table_name) == 'equivalisedDisposableIncomeYearly'] <- paste("out_income", postfix, sep="_")
  names(data_table_name)[names(data_table_name) == 'atRiskOfPoverty'] <- paste("out_poverty", postfix, sep="_")
  names(data_table_name)[names(data_table_name) == 'N'] <- paste("out_N", postfix, sep="_")
  names(data_table_name)[names(data_table_name) == 'RII'] <- paste("out_RII", postfix, sep="_")
  names(data_table_name)[names(data_table_name) == 'SII'] <- paste("out_SII", postfix, sep="_")
  names(data_table_name)[names(data_table_name) == 'medianinc'] <- paste("out_medianinc", postfix, sep="_")
  data_table_name
}

rename_subgroup_final_columns <- function(data_table_name, postfix) {
  names(data_table_name)[names(data_table_name) == 'out_ghqcase'] <- paste("out_ghqcase", postfix, sep="_")
  names(data_table_name)[names(data_table_name) == 'dhm'] <- paste("dhm", postfix, sep="_")
  names(data_table_name)[names(data_table_name) == 'les_c4'] <- paste("out_emp", postfix, sep="_")
  names(data_table_name)[names(data_table_name) == 'labourSupplyWeekly'] <- paste("out_emphrs", postfix, sep="_")
  names(data_table_name)[names(data_table_name) == 'equivalisedDisposableIncomeYearly'] <- paste("out_income", postfix, sep="_")
  names(data_table_name)[names(data_table_name) == 'atRiskOfPoverty'] <- paste("out_poverty", postfix, sep="_")
  names(data_table_name)[names(data_table_name) == 'N'] <- paste("out_N", postfix, sep="_")
  names(data_table_name)[names(data_table_name) == 'medianinc'] <- paste("out_medianinc", postfix, sep="_")
  data_table_name
}

get_summary_statistics <- function(main_column_names, # common columns for all minor tables, run id is missing when summarise over all runs
                                   data_table_name, experiment_stage) {
  action_columns <- list(averaging = main_out_colnames)
  action_list <- list(averaging = mean) # list of functions to evaluate
  todo <- tibble(action_columns, action_list) # combinations of functions applied to corresponding columns
  
  # need to exclude gender as predictor for sex-stratified models
  if(length(unique(data_table_name$dgn))<2) { 
    pmap_dfc(todo, ~data_table_name[experiment==experiment_stage] %>%
               group_by(across(all_of(main_column_names))) %>%
               mutate(rank = rank(fct_rev(deh_c3)) / n()) %>%
               summarise(
                 N = n(),
                 RII = possibly(exp, otherwise = NA)(coef(
                   glm(out_ghqcase ~ rank + dag, family = "poisson")
                 )[["rank"]]),
                 SII = coef(lm(out_ghqcase ~ rank + dag))[["rank"]],
                 medianinc = median(equivalisedDisposableIncomeYearly),
                 across(all_of(.x), .y),
                 .groups = 'keep'
               )
    )
  } else {
    pmap_dfc(todo, ~data_table_name[experiment==experiment_stage] %>%
               group_by(across(all_of(main_column_names))) %>%
               mutate(rank = rank(fct_rev(deh_c3)) / n()) %>%
               summarise(
                 N = n(),
                 RII = possibly(exp, otherwise = NA)(coef(
                   glm(out_ghqcase ~ rank + dag + dgn, family = "poisson")
                 )[["rank"]]),
                 SII = coef(lm(out_ghqcase ~ rank + dag + dgn))[["rank"]],
                 medianinc = median(equivalisedDisposableIncomeYearly),
                 across(all_of(.x), .y),
                 .groups = 'keep'
               )
    )
  }
}

get_subgroup_summary_statistics <- function(main_column_names, # common columns for all minor tables, run id is missing when summarize over all runs
                                   data_table_name, experiment_stage) {
  action_columns <- list(averaging = main_out_colnames)
  action_list <- list(averaging = mean) # list of functions to evaluate
  todo <- tibble(action_columns, action_list) # combinations of functions applied to corresponding columns
  pmap_dfc(todo, ~data_table_name[experiment==experiment_stage] %>%
             group_by(across(all_of(main_column_names))) %>%
             summarise(
               across(all_of(.x), .y),
               N = n(),
               medianinc = median(equivalisedDisposableIncomeYearly),
               .groups = 'keep'
             )
  )
}

# 2. Combining person-level data #
##################################

df <- data.table()
analysis_id <- c("S1", "S2", "S3", "S4") # can add/remove analyses here - for the paper, s1 = primary analysis, s2-s4 = sensitivity analyses
scenario_id <- c("baseline", "partial", "full no bens", "full w bens") # can add/remove policy scenarios here

# to facilitate combining, each Person.csv and BenefitUnit.csv file from SimPaths should be stored in the corresponding folder:
# 'data/analysis_id/scenario_id'
        
for (aid in analysis_id) {
  for (sid in scenario_id) {
    path <- paste0("data", "/", aid, "/", sid, "/", "Person.csv")
    cols <- c(
      'id_Person', 'household_status',
      'id_benefitUnit', 'run', 'dag', 'dgn', 'deh_c3', 'time',
      'dhm_ghq', 'dhm', 'les_c4', 'labourSupplyWeekly')
        file <- laf_open(detect_dm_csv(path, header = TRUE))
    person <- as.data.table(file[, cols])
    gc()
    person <- filter(person, dag>=25 & dag<65) # dropping those not in our analytical sample
    pathbu <- paste0("data", "/", aid, "/", sid, "/", "BenefitUnit.csv")
    colsbu <- c(
      'atRiskOfPoverty', 'id_BenefitUnit',
      'equivalisedDisposableIncomeYearly',
      'run', 'time',
      'n_children_0', 'n_children_1',
      'n_children_2', 'n_children_3',
      'n_children_4', 'n_children_5',
      'n_children_6', 'n_children_7',
      'n_children_8', 'n_children_9',
      'n_children_10', 'n_children_11',
      'n_children_12','n_children_13',
      'n_children_14', 'n_children_15',
      'n_children_16', 'n_children_17'
      )
    filebu <- laf_open(detect_dm_csv(pathbu, header = TRUE))
    bu <- as.data.table(filebu[, colsbu])

    gc()
    
    bu$grp_hchild <- select(bu, contains("n_children")) %>% rowSums %>% as.logical
    # drop number of children columns
    bu <- bu %>% select(-starts_with("n_children"))
    bu$grp_nchild <- !bu$grp_hchild
    
    gc()
    
    df_scratch <- merge(
      person,
      bu,
      by.x = c("id_benefitUnit", "run", "time"),
      by.y = c("id_BenefitUnit", "run", "time"),
    )
    rm(bu)
    rm(person)
    
    # add scenario/arm id now at the cost of memory
    df_scratch[, scenario:=aid]
    df_scratch[, experiment:=sid]

    # glue together
    df <- rbind(df, df_scratch)
  }
}
# free memory
# rm(df_scratch)
gc()

# 3. Organising person-level data #
###################################

educ_levels <- c("Low",
                 "Medium",
                 "High") # to make these are correctly ranked

# convert certain columns to categorical
df$scenario <- as.factor(df$scenario)
df$experiment <- as.factor(df$experiment)
df$dgn <- as.factor(df$dgn) # gender
df$deh_c3 <- factor(df$deh_c3, 
                     levels = educ_levels) # education
df$out_ghqcase <- if_else(df$dhm_ghq == "true", 1, 0)

df <- select(df, -dhm_ghq)
gc()

# convert initial raw data into something to work with
unique_years <- unique(df$time) # all available year values
unique_runs <- unique(df$run) # all values of run ids

unique_weekly_hours <- unique(df$labourSupplyWeekly) # string values of working hours/week
unique_weekly_hours_numeric <- c(30, 10, 40, 0, 20) # integer values of unique_weekly_hours, used to convert chars to ints properly
# not automatically correct - must be manually checked on first run

unique_labour_status <- unique(df$les_c4) # employment status
unique_labour_status_unemp <- c(0, 1, 0, 0) # identifying unemployed as integer/boolean
unique_labour_status_emp <- c(1, 0, 0, 0) # identifying only those in employment
unique_labour_status_ret <- c(0, 0, 0, 1) # identifying retired
unique_labour_status_stud <- c(0, 0, 1, 0) # identifying student
# not automatically correct - must be manually checked on first run

#convert labour category to ints
df$labourSupplyWeekly <- as.integer(format(factor(df$labourSupplyWeekly,
                                                  levels = unique_weekly_hours,
                                                  labels = unique_weekly_hours_numeric)))

# convert employment category to ints

df$les_c4_unemp <- as.integer(format(factor(df$les_c4,
                                            levels = unique_labour_status,
                                            labels = unique_labour_status_unemp)))

df$les_c4 <- as.integer(format(factor(df$les_c4,
                                      levels = unique_labour_status,
                                      labels = unique_labour_status_emp)))
# needs to be last since this recodes the original variable

gc()

# 4. Generating summary results #
#################################

result <- lapply(scenario_id, main_loop, dft=df) %>% reduce(left_join, by = ids)
result$grp_all <- TRUE

# stratified by gender

subpop_result <- lapply(scenario_id, main_loop, dft=df[dgn=="Male"]) %>% reduce(left_join, by = ids)
subpop_result$grp_sxmale <- TRUE
result <- do.call(rbind, list(result, subpop_result))

subpop_result <- lapply(scenario_id, main_loop, dft=df[dgn=="Female"]) %>% reduce(left_join, by = ids)
subpop_result$grp_sxfemale <- TRUE
result <- do.call(rbind, list(result, subpop_result))

# stratified by age

subpop_result <- lapply(scenario_id, subgroup_loop, dft=df[dag >= 25 & dag < 45]) %>% reduce(left_join, by = ids)
subpop_result$grp_age25 <- TRUE
result <- do.call(rbind, list(result, subpop_result))

subpop_result <- lapply(scenario_id, subgroup_loop, dft=df[dag >= 45 & dag < 65]) %>% reduce(left_join, by = ids)
subpop_result$grp_age45 <- TRUE
result <- do.call(rbind, list(result, subpop_result))

# stratified by household status

subpop_result <- lapply(scenario_id, subgroup_loop, dft=df[grp_hchild == TRUE]) %>% reduce(left_join, by = ids)
subpop_result$grp_FShchild <- TRUE
result <- do.call(rbind, list(result, subpop_result))

subpop_result <- lapply(scenario_id, subgroup_loop, dft=df[grp_nchild == TRUE]) %>% reduce(left_join, by = ids)
subpop_result$grp_FSnchild <- TRUE
result <- do.call(rbind, list(result, subpop_result))

subpop_result <- lapply(scenario_id, subgroup_loop, dft=df[household_status=="Single" & grp_hchild == TRUE]) %>% reduce(left_join, by = ids)
subpop_result$grp_FSloneparents <- TRUE
result <- do.call(rbind, list(result, subpop_result))

# stratified by education

subpop_result <- lapply(scenario_id, subgroup_loop, dft=df[deh_c3 == "Low"]) %>% reduce(left_join, by = ids)
subpop_result$grp_edlow <- TRUE
result <- do.call(rbind, list(result, subpop_result))

subpop_result <- lapply(scenario_id, subgroup_loop, dft=df[deh_c3 == "Medium"]) %>% reduce(left_join, by = ids)
subpop_result$grp_edmed <- TRUE
result <- do.call(rbind, list(result, subpop_result))

subpop_result <- lapply(scenario_id, subgroup_loop, dft=df[deh_c3 == "High"]) %>% reduce(left_join, by = ids)
subpop_result$grp_edhi <- TRUE
result <- do.call(rbind, list(result, subpop_result))

# Saving files #
################

arrow::write_feather(result, 'data/summary/results.feather')
write.csv(result, file = "data/summary/results.csv")