# import libraries, it is assumed they've been installed already
library(data.table) # parallel data manipulation
library(gsubfn) # string templates
library(magrittr)
library(tibble)
library(dplyr)
library(tidyverse)
library(R.utils)
library(openxlsx)

# read in data from statistics file associated with the baseline policy scenario from 2022-2026

df <- fread(
  file = "data/Statistics1.csv",
  select = c(
    'run', 'time', 'medianEquivalisedHouseholdDisposableIncome'
  ))

# delete every second row (as these were duplicated in SimPaths output)

toDelete <- seq(1, nrow(df), 2)
df <- df[ toDelete ,]

# generating poverty line
df$povline <- df$medianEquivalisedHouseholdDisposableIncome * 0.6

# generating seed variable - this allows SimPaths to select the correct fixed poverty line for subsequent runs of intervention scenarios
df$seed <- df$run + 600 - 1 # replace first number with random seed used for multi-run

df %>%
  select(seed, time, povline) %>%
  write.xlsx(file = "fixed_poverty_lines.xlsx")
