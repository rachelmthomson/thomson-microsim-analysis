library(data.table)
library(ggplot2)
library(ggh4x)

data <- fread("data/summary/s1only.csv") # inputs data from primary analysis only (designated s1)
names(data)

numeric_cols <- c(
  "out_N_baseline", "out_RII_baseline", "out_SII_baseline",
  "out_medianinc_baseline", "out_emp_baseline", "out_ghqcase_baseline",
  "out_income_baseline", "out_poverty_baseline", "Common mental disorder", "Poverty"
)
group_cols <- c(
  "grp_all", "grp_sxmale", "grp_sxfemale", "grp_age25",
  "grp_age45", "grp_FShchild", "grp_FSnchild", "grp_FSloneparents",
  "grp_edlow", "grp_edmed", "grp_edhi"
)

# Turn grp_ columns into a single "group" column
for (group in group_cols) {
  set(data, i = which(data[[group]]), j = "group", value = group)
}

data$group <- stringr::str_remove(data$group, "grp_")
data$group <- factor(
  data$group,
  levels = c("all", "sxmale", "sxfemale", "edlow", "edmed", "edhi",
             "age25", "age45", "FShchild", "FSnchild",
             "FSloneparents"),
  labels = c("All", "Men", "Women", "Low education", "Medium education",
             "High education", "Younger 25-44y",
             "Older 45-64y", "Have children", "No children", "Lone parents")
)

data$`Common mental disorder` <- data$out_ghqcase_baseline*100
data$`Poverty` <- data$out_poverty_baseline*100

# Calculate cumulative means, per group, per year
cummeans <- data[, lapply(.SD, dplyr::cummean), .SDcols = numeric_cols,
                 by = .(time, group)]
# Add row number by group/year
cummeans[, iterations := seq_len(.N), by = .(time, group)]

cummeans <- melt(cummeans, id.vars = c("time", "group", "iterations"))

unique(cummeans$group)
unique(cummeans$variable)

# Select which groups to graph
groups <- c("All", "Men", "Women")
variables <- c("Common mental disorder", "Poverty")

# Graph runs for single year
ggplot(cummeans[time == 2022 & group %in% groups & variable %in% variables]) +
  aes(x = iterations, y = value) +
  xlab("Number of iterations") +
  ylab("Cumulative mean prevalence") + 
  geom_line() +
  ggh4x::facet_grid2(group ~ variable, scales = "free_y", independent = "y")
