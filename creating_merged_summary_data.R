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
library(openxlsx)

# This process was required because the individual-level output files from 1000 runs of SimPaths (across four policy scenarios  
# and several sensitivity analyses) were too large to be combined/summarised using a single run of processing_multirun_output.R

# SimPaths was first run 250 times for the primary analysis (s1) and all three sensitivity analyses (s2-s4)
# SimPaths was then run a further 750 times, with these files summarised separately as 'baseline & partial' and 'full & full+' due to size

# This file combines all 1000 iterations into a single summary file, which includes all four scenarios run for s1-s4

s1_250 <- read_csv(file.path(getwd(), "data/summary/results_S1_250.csv"),
               show_col_types = FALSE)

s1_750_basepart <- read_csv(file.path(getwd(), "data/summary/results_S1_baselinepartial_750.csv"),
                            show_col_types = FALSE)

s1_750_bothfull <- read_csv(file.path(getwd(), "data/summary/results_S1_bothfull_750.csv"),
                            show_col_types = FALSE)

s1_750 <- left_join(s1_750_basepart,
                    s1_750_bothfull,
                    by = c("...1", "scenario", "run", "time", "grp_all",
                           "grp_sxmale",
                           "grp_sxfemale",
                           "grp_age25",
                           "grp_age45",
                           "grp_FShchild",
                           "grp_FSnchild",
                           "grp_FSloneparents",
                           "grp_edlow",
                           "grp_edmed",
                           "grp_edhi"))

s1_750$run <- s1_750$run + 250 # this ensures that the model run number begins at 251 once the data are combined

s1 <- bind_rows(s1_250, s1_750)

s2_250 <- read_csv(file.path(getwd(), "data/summary/results_S2_250.csv"),
               show_col_types = FALSE)

s2_750_basepart <- s1_750_basepart %>%
  mutate(scenario = "S2") # as sensitivity analyses s2 and s3 are only run on the Full UBI scenarios, s1 data on baseline/partial scenarios are called

s2_750_bothfull <- read_csv(file.path(getwd(), "data/summary/results_S2_bothfull_750.csv"),
                            show_col_types = FALSE)

s2_750 <- left_join(s2_750_basepart,
                    s2_750_bothfull,
                    by = c("...1", "scenario", "run", "time", "grp_all",
                           "grp_sxmale",
                           "grp_sxfemale",
                           "grp_age25",
                           "grp_age45",
                           "grp_FShchild",
                           "grp_FSnchild",
                           "grp_FSloneparents",
                           "grp_edlow",
                           "grp_edmed",
                           "grp_edhi"))

s2_750$run <- s2_750$run + 250

s2 <- bind_rows(s2_250, s2_750)

s3_250 <- read_csv(file.path(getwd(), "data/summary/results_S3_250.csv"),
               show_col_types = FALSE)

s3_750_basepart <- s1_750_basepart %>%
  mutate(scenario = "S3") # as sensitivity analyses s2 and s3 are only run on the Full UBI scenarios, s1 data on baseline/partial scenarios are called

s3_750_bothfull <- read_csv(file.path(getwd(), "data/summary/results_S3_bothfull_750.csv"),
                            show_col_types = FALSE)

s3_750 <- left_join(s3_750_basepart,
                    s3_750_bothfull,
                    by = c("...1", "scenario", "run", "time", "grp_all",
                           "grp_sxmale",
                           "grp_sxfemale",
                           "grp_age25",
                           "grp_age45",
                           "grp_FShchild",
                           "grp_FSnchild",
                           "grp_FSloneparents",
                           "grp_edlow",
                           "grp_edmed",
                           "grp_edhi"))

s3_750$run <- s3_750$run + 250

s3 <- bind_rows(s3_250, s3_750)

s4_250 <- read_csv(file.path(getwd(), "data/summary/results_S4_250.csv"),
                   show_col_types = FALSE)

s4_750_basepart <- read_csv(file.path(getwd(), "data/summary/results_S4_baselinepartial_750.csv"),
                            show_col_types = FALSE)

s4_750_bothfull <- read_csv(file.path(getwd(), "data/summary/results_S4_bothfull_750.csv"),
                            show_col_types = FALSE)

s4_750 <- left_join(s4_750_basepart,
                    s4_750_bothfull,
                    by = c("...1", "scenario", "run", "time", "grp_all",
                           "grp_sxmale",
                           "grp_sxfemale",
                           "grp_age25",
                           "grp_age45",
                           "grp_FShchild",
                           "grp_FSnchild",
                           "grp_FSloneparents",
                           "grp_edlow",
                           "grp_edmed",
                           "grp_edhi"))

s4_750$run <- s4_750$run + 250

s4 <- bind_rows(s4_250, s4_750)

combined <- bind_rows(s1, s2) %>%
  bind_rows(., s3) %>%
  bind_rows(., s4)

arrow::write_feather(combined, 'data/results.feather')
write.csv(combined, file = "data/results.csv")