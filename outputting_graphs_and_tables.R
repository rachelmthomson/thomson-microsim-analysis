library(readr)
library(tidyverse)
library(openxlsx)
source("graphing_functions.R")

out_data <-
  read_csv(file.path(getwd(), "data/results.csv"),
           show_col_types = FALSE)

scenario_id <- c("S1", "S2", "S3", "S4")
# identifies which analyses graphs will be generated for (s1 = primary analysis; s2-s4 = sensitivity analyses)

pclist <- c("out_ghqcase_baseline",
            "out_ghqcase_partial",
            "out_ghqcase_full no bens",
            "out_ghqcase_full w bens",
            "out_poverty_baseline",
            "out_poverty_partial",
            "out_poverty_full no bens",
            "out_poverty_full w bens",
            "out_emp_baseline",
            "out_emp_partial",
            "out_emp_full no bens",
            "out_emp_full w bens")

for (var in pclist) {
  out_data[var] <- out_data[var]*100
} # make sure only to run once - converts required variables to percentages

# generate required difference measures (CMD prevalence, RII, SII, hours worked)
out_data <- out_data %>%
  mutate(out_diff_partial = (out_ghqcase_partial - out_ghqcase_baseline),
         `out_diff_full no bens` = (`out_ghqcase_full no bens` - out_ghqcase_baseline),
         `out_diff_full w bens` = (`out_ghqcase_full w bens` - out_ghqcase_baseline),
         out_diffRII_partial = (out_RII_partial - out_RII_baseline),
         `out_diffRII_full no bens` = (`out_RII_full no bens` - out_RII_baseline),
         `out_diffRII_full w bens` = (`out_RII_full w bens` - out_RII_baseline),
         out_diffSII_partial = (out_SII_partial - out_SII_baseline),
         `out_diffSII_full no bens` = (`out_SII_full no bens` - out_SII_baseline),
         `out_diffSII_full w bens` = (`out_SII_full w bens` - out_SII_baseline),
         out_diffemphrs_partial = (out_emphrs_partial - out_emphrs_baseline),
         `out_diffemphrs_full no bens` = (`out_emphrs_full no bens` - out_emphrs_baseline),
         `out_diffemphrs_full w bens` = (`out_emphrs_full w bens` - out_emphrs_baseline))

grp_data <- out_data %>%
  mutate(
    grp_sx = case_when(grp_sxmale ~ "Male", grp_sxfemale ~ "Female"),
    grp_age = case_when(grp_age25 ~ "25-44y", grp_age45 ~ "45-64y"),
    grp_FS = case_when(grp_FSnchild ~ "No kids", grp_FShchild ~ "Has kids", grp_FSloneparents ~ "Lone parents"),
    grp_ed = case_when(grp_edlow ~ "Low", grp_edmed ~ "Medium", grp_edhi ~ "High")
  )

#           Results table

summary <- out_data |>
  filter(time > 2020, !is.na(run)) |>
  # Turn the grp_ variables into a single "group" variable
  pivot_longer(starts_with("grp_"), names_to = "group", values_to = "this_group") |>
  filter(this_group == TRUE) |>
  select(-this_group) |>
  # Transform from wide to long
  pivot_longer(
    starts_with("out_"),
    names_to = c("variable", "policy"),
    values_to = "value",
    names_pattern = "out_(.*)_(.*)"
  ) |>
  mutate(
    policy = factor(
      policy,
      levels = c("baseline", "partial", "full no bens", "full w bens"),
      labels = c("Baseline", "Partial UBI", "Full UBI", "Full UBI + Bens")
    ),
    group = stringr::str_remove(group, "grp_"),
    group = factor(
      group,
      levels = c("all", "sxmale", "sxfemale", "edlow", "edmed", "edhi",
                 "age25", "age45", "FShchild", "FSnchild",
                 "FSloneparents"),
      labels = c("All", "Men", "Women", "Low education", "Medium education",
                 "High education", "Younger 25-44y",
                 "Older 45-64y", "Have children", "No children", "Lone parents")
    ),
    variable = factor(
      variable,
      levels = c("ghqcase", "diff", "RII", "diffRII", "SII", "diffSII", "medianinc", "poverty", "emp", "N", "emphrs", "diffemphrs"),
      labels = c("GHQ caseness", "Effect", "RII", "RII effect", "SII", "SII effect", "Median income", "Poverty", "Employment", "N", "Hours worked", "Diff. hours worked")
    )
  ) |>
  filter(!is.na(group)) |>
  filter(variable %in% c("GHQ caseness", "Effect", "RII", "RII effect", "SII", "SII effect", "Median income", "Poverty", "Employment", "N", "Hours worked", "Diff. hours worked")) |>
  filter(group == "All" | variable == "GHQ caseness" | variable == "Effect" | variable == "Employment" | variable == "N" | variable == "Hours worked" | variable == "Diff. hours worked") |>
  # Generate the summary data
  group_by(scenario, group, variable, policy, time) |>
  summarise(
    median = median(value, na.rm = TRUE),
    pct05 = quantile(value, 0.025, na.rm = TRUE),
    pct95 = quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    median_ci = case_when(
      variable != "N" ~ sprintf("%.2f (%.2f, %.2f)", median, pct05, pct95),
      variable == "N" ~ sprintf("%.0f (%.0f, %.0f)", median, pct05, pct95)
    )
  ) %>%
  select(scenario, group, policy, time, variable, median_ci) %>%
  pivot_wider(names_from = time, values_from = median_ci) %>%
  filter(!(variable == "N" & policy == "Baseline")) %>% 
  filter(!(variable == "N" & policy == "Partial UBI")) %>% 
  filter(!(variable == "N" & policy == "Full UBI"))

bind_rows(
  summary %>% filter(group == "All") %>% filter(variable == "Median income" | variable == "Poverty" | variable == "Employment" | variable == "N") %>% arrange(scenario, group, variable, policy),
  summary %>% filter(group == "All") %>% filter(variable == "GHQ caseness" | variable == "Effect") %>% arrange(scenario, policy, variable),
  summary %>% filter(group == "All") %>% filter(variable == "RII" | variable == "RII effect") %>% arrange(scenario, policy, variable),
  summary %>% filter(group == "All") %>% filter(variable == "SII" | variable == "SII effect") %>% arrange(scenario, policy, variable),
  summary %>% filter(group == "All") %>% filter(variable == "Hours worked" | variable == "Diff. hours worked") %>% arrange(scenario, policy, variable),
  summary %>% filter( group == "Men" |
                       group == "Women" |
                       group == "Low education" |
                       group == "Medium education" |
                       group == "High education" |
                       group == "Unemployed" |
                       group == "Employed" |
                       group == "Younger 25-44y" |
                       group == "Older 45-64y" |
                       group == "Have children" |
                       group == "No children" |
                       group == "Lone parents") %>% arrange(scenario, group, policy)
) %>% 
  write.xlsx(file = "all_results.xlsx")

#           Graphs for whole population

# CMD

for (sid in scenario_id) {
{out_data %>%
    filter(time>2020 & scenario == sid) %>%
    graph_policy_comparisons(
      out_ghqcase_baseline,
      out_ghqcase_partial,
      `out_ghqcase_full no bens`,
      `out_ghqcase_full w bens`,
      grp_all,
      y_lab = "Prevalence of CMD (%)",
      ci_geom = "errorbar") + ylim(15,22.5) # add this code if want to set upper/lower limit on y axis
} %>%
  ggplot_build()
  ggsave(file = paste0("plots/svgs/", sid, "/All.svg"),
         width = 8.5,
         height = 7,
         device = svg)
}

for (sid in scenario_id) {
  {out_data %>%
      filter(time>2020 & scenario == sid) %>%
      graph_policy_comparisons(
        out_ghqcase_baseline,
        out_ghqcase_partial,
        `out_ghqcase_full no bens`,
        `out_ghqcase_full w bens`,
        grp_all,
        y_lab = "Prevalence of CMD (%)",
        ci_geom = "errorbar") + ylim(15,22.5) # add this code if want to set upper/lower limit on y axis
  } %>%
    ggplot_build()
  ggsave(file = paste0("plots/pdfs/", sid, "/All.pdf"),
         width = 8.5,
         height = 7)
}

# RII

for (sid in scenario_id) {
  {out_data %>%
      filter(time>2020 & scenario == sid) %>%
      graph_policy_comparisons(
        out_RII_baseline,
        out_RII_partial,
        `out_RII_full no bens`,
        `out_RII_full w bens`,
        grp_all,
        y_lab = "Relative index of inequality for CMD",
        ci_geom = "errorbar") + ylim(1,1.7) # add this code if want to set upper/lower limit on y axis
  } %>%
    ggplot_build()
  ggsave(file = paste0("plots/svgs/", sid, "/All - RII.svg"),
         width = 7,
         height = 7,
         device = svg)
}
  
for (sid in scenario_id) {
  {out_data %>%
      filter(time>2020 & scenario == sid) %>%
      graph_policy_comparisons(
        out_RII_baseline,
        out_RII_partial,
        `out_RII_full no bens`,
        `out_RII_full w bens`,
        grp_all,
        y_lab = "Relative index of inequality for CMD",
        ci_geom = "errorbar")  + ylim(1,1.7) # add this code if want to set upper/lower limit on y axis
   } %>%
    ggplot_build()
  ggsave(file = paste0("plots/pdfs/", sid, "/All - RII.pdf"),
         width = 7,
         height = 7)
}

# SII

for (sid in scenario_id) {
  {out_data %>%
      filter(time>2020 & scenario == sid) %>%
      graph_policy_comparisons(
        out_SII_baseline,
        out_SII_partial,
        `out_SII_full no bens`,
        `out_SII_full w bens`,
        grp_all,
        y_lab = "Slope index of inequality for CMD",
        ci_geom = "errorbar") + ylim(0,0.12) # add this code if want to set upper/lower limit on y axis
    } %>%
    ggplot_build()
  ggsave(file = paste0("plots/svgs/", sid, "/All - SII.svg"),
         width = 7,
         height = 7,
         device = svg)
}

for (sid in scenario_id) {
  {out_data %>%
      filter(time>2020 & scenario == sid) %>%
      graph_policy_comparisons(
        out_SII_baseline,
        out_SII_partial,
        `out_SII_full no bens`,
        `out_SII_full w bens`,
        grp_all,
        y_lab = "Slope index of inequality for CMD",
        ci_geom = "errorbar") + ylim(0,0.12) # add this code if want to set upper/lower limit on y axis
  } %>%
    ggplot_build()
  ggsave(file = paste0("plots/pdfs/", sid, "/All - SII.pdf"),
         width = 7,
         height = 7)
}

# Poverty

for (sid in scenario_id) {
{out_data %>%
    filter(time>2020 & scenario == sid) %>%
    graph_policy_comparisons(
      out_poverty_baseline,
      out_poverty_partial,
      `out_poverty_full no bens`,
      `out_poverty_full w bens`,
      grp_all,
      y_lab = "Prevalence of Poverty (%)",
      agg_method = mean,
      ci_geom = "errorbar") + ylim(0,15) # add this code if want to set upper/lower limit on y axis
  } %>%
  ggsave(file = paste0("plots/svgs/", sid, "/All - poverty.svg"),
         width = 7,
         height = 7,
         device = svg)
}

for (sid in scenario_id) {
  {out_data %>%
      filter(time>2020 & scenario == sid) %>%
      graph_policy_comparisons(
        out_poverty_baseline,
        out_poverty_partial,
        `out_poverty_full no bens`,
        `out_poverty_full w bens`,
        grp_all,
        y_lab = "Prevalence of Poverty (%)",
        agg_method = mean,
        ci_geom = "errorbar") + ylim(0,15) # add this code if want to set upper/lower limit on y axis
  } %>%
    ggsave(file = paste0("plots/pdfs/", sid, "/All - poverty.pdf"),
           width = 7,
           height = 7)
}

# Employment

for (sid in scenario_id) {
{out_data %>%
    filter(time>2020 & scenario == sid) %>%
    graph_policy_comparisons(
      out_emp_baseline,
      out_emp_partial,
      `out_emp_full no bens`,
      `out_emp_full w bens`,
      grp_all,
      y_lab = "Prevalence of Employment (%)",
      agg_method = mean,
      ci_geom = "errorbar") + ylim(70,85) # add this code if want to set upper/lower limit on y axis
  } %>%
  ggsave(file = paste0("plots/svgs/", sid, "/All - emp.svg"),
         width = 7,
         height = 7,
         device = svg)
}

for (sid in scenario_id) {
  {out_data %>%
      filter(time>2020 & scenario == sid) %>%
      graph_policy_comparisons(
        out_emp_baseline,
        out_emp_partial,
        `out_emp_full no bens`,
        `out_emp_full w bens`,
        grp_all,
        y_lab = "Prevalence of Employment (%)",
        agg_method = mean,
        ci_geom = "errorbar") + ylim(70,85) # add this code if want to set upper/lower limit on y axis
  } %>%
    ggsave(file = paste0("plots/pdfs/", sid, "/All - emp.pdf"),
           width = 7,
           height = 7)
}
  
for (sid in scenario_id) {
  {grp_data %>%
      filter(time>2020 & scenario == sid) %>%
      graph_policy_comparisons(
        out_emp_baseline,
        out_emp_partial,
        `out_emp_full no bens`,
        `out_emp_full w bens`,
        grp_sxmale,
        y_lab = "Prevalence of Employment in Men (%)",
        agg_method = mean,
        ci_geom = "errorbar") + ylim(75,90) # add this code if want to set upper/lower limit on y axis
  } %>%
    ggsave(file = paste0("plots/svgs/", sid, "/All - empmen.svg"),
           width = 7,
           height = 7,
           device = svg)
}
  
for (sid in scenario_id) {
  {grp_data %>%
      filter(time>2020 & scenario == sid) %>%
      graph_policy_comparisons(
        out_emp_baseline,
        out_emp_partial,
        `out_emp_full no bens`,
        `out_emp_full w bens`,
        grp_sxmale,
        y_lab = "Prevalence of Employment in Men (%)",
        agg_method = mean,
        ci_geom = "errorbar") + ylim(75,90) # add this code if want to set upper/lower limit on y axis
  } %>%
    ggsave(file = paste0("plots/pdfs/", sid, "/All - empmen.pdf"),
           width = 7,
           height = 7)
}

for (sid in scenario_id) {
  {grp_data %>%
      filter(time>2020 & scenario == sid) %>%
      graph_policy_comparisons(
        out_emp_baseline,
        out_emp_partial,
        `out_emp_full no bens`,
        `out_emp_full w bens`,
        grp_sxfemale,
        y_lab = "Prevalence of Employment in Women (%)",
        agg_method = mean,
        ci_geom = "errorbar") + ylim(65,80) # add this code if want to set upper/lower limit on y axis
    } %>%
    ggsave(file = paste0("plots/svgs/", sid, "/All - empwomen.svg"),
           width = 7,
           height = 7,
           device = svg)
}
  
for (sid in scenario_id) {
  {grp_data %>%
      filter(time>2020 & scenario == sid) %>%
      graph_policy_comparisons(
        out_emp_baseline,
        out_emp_partial,
        `out_emp_full no bens`,
        `out_emp_full w bens`,
        grp_sxfemale,
        y_lab = "Prevalence of Employment in Women (%)",
        agg_method = mean,
        ci_geom = "errorbar") + ylim(65,80) # add this code if want to set upper/lower limit on y axis
  } %>%
    ggsave(file = paste0("plots/pdfs/", sid, "/All - empwomen.pdf"),
           width = 7,
           height = 7)
}

#             Stratified graphs for CMD

grplist <- c("grp_sx",
             "grp_ed",
             "grp_age",
             "grp_FS")

for (sid in scenario_id) {
  for (grp in grplist) {
    {grp_data %>%
        filter(time>2020 & scenario == sid) %>%
        graph_policy_comparisons_2(
          out_ghqcase_baseline,
          `out_ghqcase_full w bens`,
          .data[[grp]],
          y_lab = "Prevalence of CMD (%)",
          agg_method = mean,
          ci_geom = "ribbon") 
    } %>%
      ggsave(file = paste0("plots/svgs/", sid, "/", grp, ".svg"),
             width = 7,
             height = 7,
             device = svg)
  }
}

for (sid in scenario_id) {
  for (grp in grplist) {
    {grp_data %>%
        filter(time>2020 & scenario == sid) %>%
        graph_policy_comparisons_2(
          out_ghqcase_baseline,
          `out_ghqcase_full w bens`,
          .data[[grp]],
          y_lab = "Prevalence of CMD (%)",
          agg_method = mean,
          ci_geom = "ribbon") 
    } %>%
      ggsave(file = paste0("plots/pdfs/", sid, "/", grp, ".pdf"),
             width = 7,
             height = 7)
  }
}
