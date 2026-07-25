################################################################################
##
## [ PROJ ] < Teacher Retention >
## [ FILE ] < 03-plot-retention-cbs-trends.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 7/13/26 >
##
################################################################################

#Goal: Plot retention rate AND shifting rate trends (2020-21 through
#      2024-25) alongside the five CBS strategy scores, at three levels:
#      per school, per cohort (CCSPP cohort and SDUSD cohort, separately),
#      and district-wide overall (all schools combined). For each level,
#      build three views: a retention-only plot, a shifting-only plot,
#      and a combined plot with both rates on the same primary axis
#      (retention = solid black, shifting = dashed grey) plus CBS scores
#      on the secondary axis. Output is a set of named lists of ggplot
#      objects, saved together into a single .RData file rather than
#      exported as image files.

################################################################################

## ---------------------------
## libraries
## ---------------------------
library(tidyverse)
library(data.table)
library(openxlsx)
library(readxl)
library(janitor)
library(scales)

## ---------------------------
## directory paths
## ---------------------------

#see current directory
getwd()

data_dir<-file.path("C:/Users/jyo/Box/TR Data/SDUSD/original","data")

## ---------------------------
## helper functions & strings
## ---------------------------

#the five CBS strategy columns and human-readable labels for the legend
cbs_cols<-c("cbs_community_based_learning", "cbs_collaborative_leadership",
            "cbs_shared_commitment", "cbs_strategic_partnerships",
            "cbs_sustaining_staff")

cbs_labels<-c(cbs_community_based_learning = "Community-Based Learning",
              cbs_collaborative_leadership = "Collaborative Leadership",
              cbs_shared_commitment        = "Shared Commitment",
              cbs_strategic_partnerships   = "Strategic Partnerships",
              cbs_sustaining_staff         = "Sustaining Staff")

#CBS scores are on a 1-3 maturity scale, retention/shifting rates are
#0-1 - rescale CBS scores by dividing by cbs_max so all series share a
#0-1 plotting axis, then relabel the secondary axis back to the
#original 1-3 scale
cbs_max<-3

#human-readable labels for the two rate metrics
metric_labels<-c(retention_rate = "Retention Rate", shifting_rate = "Shifting Rate")

#sanitizes a school/cohort name for use as a list element name
safe_list_name<-function(x) {
  make_clean_names(x)
}

#adds the rescaled CBS layers (secondary axis) shared by both plot
#builders below, so the rescaling logic only lives in one place
cbs_layers<-function(cbs_df) {
  
  cbs_df<-cbs_df %>%
    mutate(cbs_score_rescaled = cbs_score / cbs_max)
  
  list(
    geom_line(data = cbs_df,
              aes(x = academic_year, y = cbs_score_rescaled, color = cbs_strategy_label,
                  group = cbs_strategy_label),
              linewidth = 0.6, alpha = 0.85, na.rm = TRUE),
    geom_point(data = cbs_df,
               aes(x = academic_year, y = cbs_score_rescaled, color = cbs_strategy_label),
               size = 1.8, alpha = 0.85, na.rm = TRUE)
  )
}

#shared axis/theme layer for both plot builders below
metric_theme<-function(y_name) {
  list(
    scale_y_continuous(name = y_name,
                       labels = label_percent(),
                       limits = c(0, 1),
                       sec.axis = sec_axis(transform = ~ . * cbs_max,
                                           name = "CBS Strategy Score (1-3 scale)",
                                           breaks = 1:3)),
    theme_minimal(base_size = 12),
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(face = "bold"),
          legend.position = "bottom"),
    guides(color = guide_legend(nrow = 2))
  )
}

#builds a dual-axis line plot for ONE rate metric (left axis, 0-1) plus
#the five CBS strategy scores (right axis, 1-3) - against academic year
#on the x-axis
#  metric_df  : one row per academic_year, with a column named metric_col
#  cbs_df     : long-format CBS scores for the same grouping
#               (columns: academic_year, cbs_strategy_label, cbs_score)
#  metric_col : "retention_rate" or "shifting_rate"
#  plot_title : title text for the chart
build_metric_plot<-function(metric_df, cbs_df, metric_col, plot_title) {
  
  metric_name<-metric_labels[[metric_col]]
  
  ggplot() +
    cbs_layers(cbs_df) +
    #rate line (primary axis, 0-1), drawn last so it's on top
    geom_line(data = metric_df,
              aes(x = academic_year, y = .data[[metric_col]], group = 1),
              color = "black", linewidth = 1.1, na.rm = TRUE) +
    geom_point(data = metric_df,
               aes(x = academic_year, y = .data[[metric_col]]),
               color = "black", size = 2.4, na.rm = TRUE) +
    metric_theme(metric_name) +
    labs(title = plot_title,
         subtitle = paste0("Black line = ", metric_name, " (left axis) | Colored lines = CBS strategy scores (right axis)"),
         x = "Academic Year",
         color = "CBS Strategy")
}

#builds a dual-axis line plot with BOTH rate metrics on the primary axis
#(retention = solid black, shifting = dashed grey) plus the five CBS
#strategy scores on the secondary axis
#  rates_df : one row per academic_year, with retention_rate and
#             shifting_rate columns
#  cbs_df   : long-format CBS scores for the same grouping
#  plot_title : title text for the chart
build_combined_plot<-function(rates_df, cbs_df, plot_title) {
  
  ggplot() +
    cbs_layers(cbs_df) +
    #retention rate line (primary axis, solid black), drawn on top
    geom_line(data = rates_df,
              aes(x = academic_year, y = retention_rate, group = 1),
              color = "black", linetype = "solid", linewidth = 1.1, na.rm = TRUE) +
    geom_point(data = rates_df,
               aes(x = academic_year, y = retention_rate),
               color = "black", shape = 16, size = 2.4, na.rm = TRUE) +
    #shifting rate line (primary axis, dashed grey), drawn on top
    geom_line(data = rates_df,
              aes(x = academic_year, y = shifting_rate, group = 1),
              color = "grey40", linetype = "dashed", linewidth = 1.1, na.rm = TRUE) +
    geom_point(data = rates_df,
               aes(x = academic_year, y = shifting_rate),
               color = "grey40", shape = 17, size = 2.4, na.rm = TRUE) +
    metric_theme("Rate") +
    labs(title = plot_title,
         subtitle = "Solid black = retention rate | Dashed grey = shifting rate (left axis) | Colored lines = CBS strategy scores (right axis)",
         x = "Academic Year",
         color = "CBS Strategy")
}

## -----------------------------------------------------------------------------
## Part 1 - Load & Clean Data
## -----------------------------------------------------------------------------

retention_apr<-read_csv(file.path(data_dir, "school_retention_apr_long_2020_2024.csv")) %>%
  clean_names()

## -----------------------------------------------------------------------------
## Part 2 - Fix Marshall Elementary Duplicate Rows
## -----------------------------------------------------------------------------

#KNOWN DATA ISSUE: every academic year has two rows for "Marshall
#Elementary" with different retention_rate values - same 35-vs-34 school
#discrepancy flagged in 01-clean-reshape-retention-data.R. Per project
#decision, treating these as two distinct schools ("Marshall Elementary A"
#/ "Marshall Elementary B") rather than averaging or dropping one.
#
#NOTE: the A/B assignment is based on row order within each school-year
#pair in the source file, since there's no other identifier to split on -
#this is a best-effort split. Worth re-pulling the raw data with a site
#code if the source system distinguishes these two schools by name.

retention_apr<-retention_apr %>%
  arrange(school_name, academic_year) %>%
  group_by(school_name, academic_year) %>%
  mutate(row_in_group = row_number()) %>%
  ungroup() %>%
  mutate(school_name = if_else(school_name == "Marshall Elementary",
                               paste("Marshall Elementary", if_else(row_in_group == 1, "A", "B")),
                               school_name)) %>%
  select(-row_in_group)

#sanity check - every school should now have exactly one row per year
dup_check<-retention_apr %>%
  count(school_name, academic_year) %>%
  filter(n > 1)

if (nrow(dup_check) > 0) {
  stop("Duplicate school-year rows remain after the Marshall Elementary fix:\n",
       paste(capture.output(print(dup_check)), collapse = "\n"))
}

#order academic years chronologically for plotting (x-axis as ordered factor)
year_levels<-retention_apr %>%
  distinct(academic_year) %>%
  arrange(academic_year) %>%
  pull(academic_year)

retention_apr<-retention_apr %>%
  mutate(academic_year = factor(academic_year, levels = year_levels, ordered = TRUE))

## -----------------------------------------------------------------------------
## Part 3 - Reshape CBS Columns to Long Format
## -----------------------------------------------------------------------------

cbs_long<-retention_apr %>%
  select(school_name, ccspp_cohort, sdusd_cohort, academic_year, all_of(cbs_cols)) %>%
  pivot_longer(cols = all_of(cbs_cols),
               names_to = "cbs_strategy",
               values_to = "cbs_score") %>%
  mutate(cbs_strategy_label = recode(cbs_strategy, !!!cbs_labels))

## -----------------------------------------------------------------------------
## Part 4 - Per-School Plots
## -----------------------------------------------------------------------------

schools<-sort(unique(retention_apr$school_name))

#builds all 3 views (retention-only, shifting-only, combined) for one school
build_school_plot_set<-function(sch) {
  
  rates_df<-retention_apr %>%
    filter(school_name == sch) %>%
    select(academic_year, retention_rate, shifting_rate)
  
  cbs_df<-cbs_long %>%
    filter(school_name == sch) %>%
    select(academic_year, cbs_strategy_label, cbs_score)
  
  list(
    retention_rate = build_metric_plot(rates_df, cbs_df, "retention_rate", plot_title = sch),
    shifting_rate  = build_metric_plot(rates_df, cbs_df, "shifting_rate", plot_title = sch),
    combined       = build_combined_plot(rates_df, cbs_df, plot_title = sch)
  )
}

#named list of 3-element lists (one per school) -> access e.g.
#school_plots_by_school$baker_elementary$retention_rate
school_plots_by_school<-map(schools, build_school_plot_set) %>%
  set_names(safe_list_name(schools))

#re-key so the top level is the view (retention_rate / shifting_rate /
#combined) and the second level is the school - easier to browse when
#comparing the same view across schools. map(x, "name") preserves the
#names of x, so the school names carry through correctly (more reliable
#here than purrr::transpose(), whose name-handling can vary by version)
school_plots<-list(
  retention_rate = map(school_plots_by_school, "retention_rate"),
  shifting_rate  = map(school_plots_by_school, "shifting_rate"),
  combined       = map(school_plots_by_school, "combined")
)

message(sprintf("Built plots for %d schools (retention_rate, shifting_rate, combined)", length(schools)))

## -----------------------------------------------------------------------------
## Part 5 - Per-Cohort Plots (CCSPP & SDUSD)
## -----------------------------------------------------------------------------

#aggregates retention + shifting + CBS scores by a grouping column (mean
#across schools within the group, for each academic year), then returns
#a list with 3 views (retention_rate / shifting_rate / combined), each a
#named list of ggplot objects, one per group value
plot_by_cohort<-function(cohort_col) {
  
  cohort_sym<-sym(cohort_col)
  
  cohort_rates<-retention_apr %>%
    filter(!is.na(!!cohort_sym)) %>%
    group_by(!!cohort_sym, academic_year) %>%
    summarise(retention_rate = mean(retention_rate, na.rm = TRUE),
              shifting_rate  = mean(shifting_rate, na.rm = TRUE), .groups = "drop")
  
  cohort_cbs<-cbs_long %>%
    filter(!is.na(!!cohort_sym)) %>%
    group_by(!!cohort_sym, academic_year, cbs_strategy_label) %>%
    summarise(cbs_score = mean(cbs_score, na.rm = TRUE), .groups = "drop")
  
  cohorts<-sort(unique(pull(cohort_rates, !!cohort_sym)))
  
  cohort_plot_sets<-map(cohorts, function(coh) {
    
    rates_df<-cohort_rates %>%
      filter(!!cohort_sym == coh) %>%
      select(academic_year, retention_rate, shifting_rate)
    
    cbs_df<-cohort_cbs %>%
      filter(!!cohort_sym == coh) %>%
      select(academic_year, cbs_strategy_label, cbs_score)
    
    coh_title<-paste0(coh, " (average across schools)")
    
    list(
      retention_rate = build_metric_plot(rates_df, cbs_df, "retention_rate", plot_title = coh_title),
      shifting_rate  = build_metric_plot(rates_df, cbs_df, "shifting_rate", plot_title = coh_title),
      combined       = build_combined_plot(rates_df, cbs_df, plot_title = coh_title)
    )
  }) %>%
    set_names(safe_list_name(cohorts))
  
  #re-key so the top level is the view, second level is the cohort - see
  #the school_plots comment above for why map(x, "name") is used instead
  #of purrr::transpose()
  list(
    retention_rate = map(cohort_plot_sets, "retention_rate"),
    shifting_rate  = map(cohort_plot_sets, "shifting_rate"),
    combined       = map(cohort_plot_sets, "combined")
  )
}

#list with 3 views (retention_rate / shifting_rate / combined), each a
#named list of ggplot objects, one per CCSPP cohort
ccspp_cohort_plots<-plot_by_cohort("ccspp_cohort")

#same structure, one per SDUSD cohort
sdusd_cohort_plots<-plot_by_cohort("sdusd_cohort")

message(sprintf("Built cohort plots for %d CCSPP cohorts and %d SDUSD cohorts (retention_rate, shifting_rate, combined)",
                length(ccspp_cohort_plots$retention_rate), length(sdusd_cohort_plots$retention_rate)))

## -----------------------------------------------------------------------------
## Part 6 - District-Wide Overall Plots (All Schools Combined)
## -----------------------------------------------------------------------------

overall_rates<-retention_apr %>%
  group_by(academic_year) %>%
  summarise(retention_rate = mean(retention_rate, na.rm = TRUE),
            shifting_rate  = mean(shifting_rate, na.rm = TRUE), .groups = "drop")

overall_cbs<-cbs_long %>%
  group_by(academic_year, cbs_strategy_label) %>%
  summarise(cbs_score = mean(cbs_score, na.rm = TRUE), .groups = "drop")

overall_title<-"All Schools Overall (average across all schools)"

#list with 3 single ggplot objects (no per-item list needed - only one
#"overall" grouping) -> overall_plots$retention_rate, $shifting_rate, $combined
overall_plots<-list(
  retention_rate = build_metric_plot(overall_rates, overall_cbs, "retention_rate", plot_title = overall_title),
  shifting_rate  = build_metric_plot(overall_rates, overall_cbs, "shifting_rate", plot_title = overall_title),
  combined       = build_combined_plot(overall_rates, overall_cbs, plot_title = overall_title)
)

message("Built overall district-wide plots (retention_rate, shifting_rate, combined)")

## -----------------------------------------------------------------------------
## Part 7 - Export Data
## -----------------------------------------------------------------------------

#save all four plot objects together into one .RData file - load with
#load(file.path(data_dir, "retention_cbs_plots.RData")) to get
#school_plots, ccspp_cohort_plots, sdusd_cohort_plots, and overall_plots
#back into the environment. Each is a 3-item list keyed by view
#(retention_rate / shifting_rate / combined); school_plots and the
#cohort lists nest a named list of ggplot objects under each view, e.g.
#school_plots$retention_rate$baker_elementary or
#ccspp_cohort_plots$combined$cohort_1

#save(school_plots, ccspp_cohort_plots, sdusd_cohort_plots, overall_plots,
#     file = file.path(data_dir, "retention_cbs_plots.RData"))

#message("Saved plot lists to ", file.path(data_dir, "retention_cbs_plots.RData"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------