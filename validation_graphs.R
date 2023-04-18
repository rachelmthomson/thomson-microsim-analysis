library(data.table) # parallel data manipulation
library(gsubfn) # string templates
library(readstata13) # to read from .dta files
library(magrittr)
library(tibble)
library(dplyr)
library(tidyverse)
library(R.utils)
library(openxlsx)
library(SPHSUgraphs)

data.table::getDTthreads()
data.table::setDTthreads(8)

# File uses data from single run of SimPaths to generate validation graphs

person <- fread(
  file = 'data/validation/Person.csv',
  select = c(
    'id_benefitUnit', 'run', 'dag', 'dgn', 'time', 'dhm_ghq')
)

bu <- fread(
  file = 'data/validation/BenefitUnit.csv',
  select = c(
    'id_BenefitUnit', 'run', 'time', 'region')
)

merged <- 
  merge(
    person,
    bu,
    by.x = c("id_benefitUnit", "run", "time"),
    by.y = c("id_BenefitUnit", "run", "time"),
  ) %>%
  filter(region %in% c("UKC", "UKD", "UKE", "UKF", "UKG", "UKH", "UKI", "UKJ", "UKK")) 
# filtering out non-English regions for better comparability with Health Survey for England

# GHQ caseness

expPD <- merged %>%
  mutate(Year = time) %>%
  select(-time) %>%
  filter(dag > 24 & dag < 65 & Year > 2011) %>%
  group_by(Year, dgn) %>%
  summarise(psych_distress=mean(dhm_ghq)) %>%
  mutate(Data = "Simulations",
         Group = dgn,
         Year = round(Year)) %>%
  select(-dgn)

expPDall <- merged %>%
  mutate(Year = time) %>%
  select(-time) %>%
  filter(dag > 24 & dag < 65 & Year > 2011) %>%
  group_by(Year) %>%
  summarise(psych_distress=mean(dhm_ghq)) %>%
  mutate(Data = "Simulations",
         Group = "All",
         Year = round(Year))

expPD <- bind_rows(expPD, expPDall)

validationpsych <- read.xlsx('validation_statistics_UK.xlsx',
                             sheet = 'UK_psychDistressByGender')
# inputs data from HSE

validationpsych <- validationpsych %>%
  mutate(Data = "Validation") %>%
  pivot_longer(starts_with("psych_distress_"),
               names_prefix = "psych_distress_",
               names_to = "Group",
               values_to = "psych_distress")

allPD <- expPD %>%
  bind_rows(validationpsych) %>%
  mutate(psych_distress = psych_distress * 100)
  
# GRAPHS #
##########

psych <- allPD %>%
  filter(Year < 2019) %>%
  ggplot(aes(Year, psych_distress, colour = Group, linetype = Data)) +
  geom_line(linewidth=0.75) +
  ylab("Prevalence of CMD") +
  ylim(0,25) +
  theme_sphsu_light() +
  scale_colour_manual(values = sphsu_cols("Thistle", "Leaf", "Rust", "University Blue", "Rose", names = FALSE))

psych

write.xlsx(allPD, "Validation data.xlsx")
