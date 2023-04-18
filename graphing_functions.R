#' Outputting line graphs comparing policy scenarios over time
#'
#' @param baseline_var Column to graph baseline
#' @param comparison_var Column to graph comparison
#' @param group_var Column with `TRUE` values corresponding to group
#' @param y_lab Title for y axis
#' @param agg_method Aggregating method for combining lines/points (defaults to median; does not affect confidence intervals)
#' @param ci_geom `geom` to display confidence intervals (defaults to "ribbon")
#' @return A ggplot object
#' @examples
#' out_data |>
#'   graph_policy_comparisons(out_ghq_baseline, out_ghq_reform,  y_lab = "GQH score")

# Standard graphs

graph_policy_comparisons <-
  function(.data,
           baseline_var,
           comparison_var,
           comparison_var2,
           comparison_var3,
           group_var,
           y_lab = "",
           agg_method = median,
           ci_geom = c("ribbon", "errorbar", "linerange", "crossbar")) {

    ci_geom <- match.arg(ci_geom)

    require(ggplot2)
    require(SPHSUgraphs)
    require(dplyr)
    require(tidyr)

    # agg_method <- match.arg(agg_method)

    baseline_var <- enquo(baseline_var)
    comparison_var <- enquo(comparison_var)
    comparison_var2 <- enquo(comparison_var2)
    comparison_var3 <- enquo(comparison_var3)
    group_var <- enquo(group_var)

    .data |>
      filter(!!group_var, !is.na(run)) |>
      select(run, time, !!baseline_var, !!comparison_var, !!comparison_var2, !!comparison_var3) |>
      pivot_longer(
        -c(run, time),
        names_to = "case",
        values_to = "val",
        names_prefix = ".*_.*_"
      ) |>
      mutate(
        case = factor(
          case,
          levels = c("baseline", "partial", "full no bens", "full w bens"),
          labels = c("Baseline", "Partial UBI", "Full UBI", "Full+ UBI")
        )
      ) |>
      ggplot(aes(time, val, colour = case, fill = case)) +
      geom_vline(xintercept = 2023, linetype = "dashed",
                 colour = "red") +
      {if (ci_geom == "ribbon") {
        stat_summary(
        fun.data = median_hilow,
        geom = "ribbon",
        alpha = 0.5,
        colour = NA)
      } else {
        stat_summary(
        fun.data = median_hilow,
        geom = ci_geom,
        width = 0.2,
        size = 1)
      }} +
      stat_summary(fun = agg_method, geom = "line") +
      stat_summary(fun = agg_method, geom = "point") +
      scale_fill_manual(
        "Policy:",
        aesthetics = c("fill", "colour"),
        values = sphsu_cols("Thistle", "University Blue", "Rust", "Leaf", names = FALSE)
      ) +
      scale_linetype("") +
      xlab("") +
      ylab(y_lab) +
      labs(
        caption = paste(
          "Notes:",
          "1000 simulation runs in each condition.",
          "Red line denotes reform implementation point",
          sep = "\n"
        )
      ) +
      theme_sphsu_light() +
      theme(legend.position = "bottom",
            plot.caption = element_text(hjust = 0),
            legend.text=element_text(size=12),
            axis.text.y=element_text(size=12),
            axis.text.x=element_text(size=13),
            axis.title=element_text(size=14),
            legend.box.margin=margin(-25,-10,-10,-10))

  }

# two-group graphs

graph_policy_comparisons_2 <-
  function(.data,
           baseline_var,
           comparison_var3,
           group_var,
           y_lab = "",
           agg_method = median,
           ci_geom = c("ribbon", "errorbar", "linerange", "crossbar")) {
    
    ci_geom <- match.arg(ci_geom)
    
    require(ggplot2)
    require(SPHSUgraphs)
    require(dplyr)
    require(tidyr)
    
    # agg_method <- match.arg(agg_method)
    
    baseline_var <- enquo(baseline_var)
    comparison_var3 <- enquo(comparison_var3)
    group_var <- enquo(group_var)
    
    .data |>
      filter(!is.na(!!group_var), !is.na(run)) |>
      select(run, time, !!baseline_var, !!comparison_var3, !!group_var) |>
      pivot_longer(
        -c(run, time, !!group_var),
        names_to = "case",
        values_to = "val",
        names_prefix = ".*_.*_"
      ) |>
      mutate(
        case = factor(
          case,
          levels = c("baseline", "partial", "full no bens", "full w bens"),
          labels = c("Baseline", "Partial UBI", "Full UBI", "Full+ UBI")
        )
      ) |>
      ggplot(aes(time, val, colour = case, fill = case, linetype = !!group_var)) +
      geom_vline(xintercept = 2023, linetype = "dashed",
                 colour = "red") +
      {if (ci_geom == "ribbon") {
        stat_summary(
          fun.data = median_hilow,
          geom = "ribbon",
          alpha = 0.12,
          colour = NA)
      } else {
        stat_summary(
          fun.data = median_hilow,
          geom = ci_geom,
          width = 0.2,
          size = 1)
      }} +
      stat_summary(fun = agg_method, geom = "line") +
      stat_summary(fun = agg_method, geom = "point") +
      scale_fill_manual(
        "Policy:",
        aesthetics = c("fill", "colour"),
        values = sphsu_cols("Thistle", "Leaf", names = FALSE)
      ) +
      scale_linetype("") +
      xlab("") +
      ylab(y_lab) +
      labs(
        caption = paste(
          "Notes:",
          "1000 simulation runs in each condition.",
          "Red line denotes reform implementation point",
          sep = "\n"
        )
      ) +
      theme_sphsu_light() +
      theme(legend.position = "bottom",
            plot.caption = element_text(hjust = 0),
            legend.text=element_text(size=12),
            axis.text.y=element_text(size=12),
            axis.text.x=element_text(size=13),
            axis.title=element_text(size=14),
            legend.box.margin=margin(-25,-10,-10,-10))
    
  }
