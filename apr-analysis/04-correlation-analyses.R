################################################################################
##
## [ PROJ ] < Teacher Retention >
## [ FILE ] < 04-correlation-analyses.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 7/13/26 >
##
################################################################################

#Goal: Explore how retention_rate relates to the CBS strategy scores and
#      other numeric APR/participation variables, via five analyses:
#      (1) correlation matrix/heatmap (significant cells starred),
#      (2) faceted scatterplots with trend lines (each panel labeled with
#      its r value), (3) one-way ANOVA + boxplot of retention_rate across
#      the 3 CBS maturity stages (1-Visioning, 2-Engaging, 3-Transforming)
#      for each CBS strategy, (4) cohort comparisons (CCSPP & SDUSD), and
#      (5) lagged correlation (CBS score this year vs outcome next year).
#      Numeric results (correlations, test stats) are exported as CSVs;
#      plots are saved as named lists of ggplot objects in one .RData file.
#
#      shifting_rate was dropped as an outcome (near-zero variation made
#      it uninformative) - retention_rate is the sole outcome throughout.
#
#      Every analysis below is written once as a generic function that
#      takes an `outcome_var` argument, then looped over `outcome_vars`
#      so it's a one-line change to add an outcome back later.

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
output_dir<-file.path("C:/Users/jyo/Box/TR Data/SDUSD/original","output")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

## ---------------------------
## helper functions & strings
## ---------------------------

outcome_vars<-c("retention_rate")

#runs cor.test() but never errors out - returns NA row instead of
#stopping the script when a group has too few complete pairs (this
#happens for some CBS strategies where score = 3 is rare)
safe_cor_test<-function(x, y, method = "pearson", min_n = 4) {
  
  ok<-complete.cases(x, y)
  n<-sum(ok)
  
  if (n < min_n) {
    return(tibble(estimate = NA_real_, p_value = NA_real_, n = n))
  }
  
  ct<-cor.test(x[ok], y[ok], method = method)
  tibble(estimate = unname(ct$estimate), p_value = ct$p.value, n = n)
}

#runs safe_cor_test() within each level of group_var - reused for the
#predictor-vs-outcome correlations (Part 4) and the lagged correlations
#(Part 8)
compute_cor_by_group<-function(df, group_var, x_var, y_var, method = "pearson") {
  df %>%
    group_by(.data[[group_var]]) %>%
    reframe(safe_cor_test(.data[[x_var]], .data[[y_var]], method = method)) %>%
    mutate(method = method)
}

#converts a snake_case variable name into a readable title-case label,
#rendering a leading "cbs_"/"ccspp_"/"sdusd_" as their initialisms
#rather than "Cbs"/"Ccspp"/"Sdusd" (e.g. cbs_community_based_learning
#-> "CBS Community Based Learning", ccspp_cohort -> "CCSPP Cohort")
format_var_label<-function(x) {
  x %>%
    str_replace_all("_", " ") %>%
    str_to_title() %>%
    str_replace("^Cbs\\b", "CBS") %>%
    str_replace("^Ccspp\\b", "CCSPP") %>%
    str_replace("^Sdusd\\b", "SDUSD")
}

#single heatmap combining both correlation methods - predictor on the
#y-axis, method (Pearson/Spearman) on the x-axis, tile fill = estimate.
#(Outcome is dropped from the x-axis here since retention_rate is now
#the only outcome - method is the more useful thing to compare.)
#Predictors with zero valid observations across every method are
#dropped entirely, and sample size is shown once as a caption rather
#than repeated on every tile, since it's constant once those zero-n
#predictors are gone
build_corr_heatmap<-function(cor_df, title) {
  
  zero_n_predictors<-cor_df %>%
    group_by(predictor) %>%
    summarise(max_n = max(n, na.rm = TRUE), .groups = "drop") %>%
    filter(max_n == 0) %>%
    pull(predictor)
  
  plot_df<-cor_df %>%
    filter(!predictor %in% zero_n_predictors) %>%
    mutate(method_label = str_to_title(method))
  
  n_vals<-unique(plot_df$n[plot_df$n > 0])
  n_caption<-if (length(n_vals) == 1) {
    paste0("n = ", n_vals)
  } else {
    paste0("n ranges from ", min(n_vals), " to ", max(n_vals))
  }
  n_caption<-paste0(n_caption, "  |  * p < 0.05")
  
  #tile label = the correlation, with a trailing "*" when p < 0.05
  plot_df<-plot_df %>%
    mutate(tile_label = ifelse(is.na(estimate), "",
                               paste0(sprintf("%.2f", estimate),
                                      ifelse(!is.na(p_value) & p_value < 0.05, "*", ""))))
  
  ggplot(plot_df, aes(x = method_label, y = predictor_label, fill = estimate)) +
    geom_tile(color = "white") +
    geom_text(aes(label = tile_label), size = 3) +
    scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC",
                         midpoint = 0, limits = c(-1, 1), na.value = "grey90",
                         name = "Correlation") +
    labs(title = title, x = NULL, y = NULL, caption = n_caption) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(hjust = 0.5),
          plot.title = element_text(face = "bold"),
          plot.caption = element_text(face = "italic"))
}

#faceted scatterplot - one panel per predictor, with a linear trend line.
#Expects df_long to already carry a predictor_label column (title-case,
#used for facet strip text) alongside the raw predictor column
build_scatter_facets<-function(df_long, outcome_var, title) {
  ggplot(df_long, aes(x = predictor_value, y = .data[[outcome_var]])) +
    geom_point(alpha = 0.4, size = 1.2, na.rm = TRUE) +
    geom_smooth(method = "lm", se = FALSE, color = "#B2182B", linewidth = 0.7, na.rm = TRUE) +
    facet_wrap(~predictor_label, scales = "free_x") +
    labs(title = title, x = "Predictor Value", y = str_to_title(str_replace_all(outcome_var, "_", " ")),
         caption = "r = Pearson correlation  |  * p < 0.05") +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold"),
          plot.caption = element_text(face = "italic"))
}

#boxplot of outcome across the 3 CBS maturity stages, faceted by CBS
#strategy. Expects df_group to carry a "stage" factor (see
#cbs_stage_labels below) and a predictor_label column for facet strips
build_stage_boxplot<-function(df_group, outcome_var, title) {
  ggplot(df_group, aes(x = stage, y = .data[[outcome_var]], fill = stage)) +
    geom_boxplot(na.rm = TRUE, outlier.alpha = 0.5) +
    facet_wrap(~predictor_label) +
    scale_fill_manual(values = c("1 - Visioning" = "#B2182B",
                                 "2 - Engaging" = "#F4A582",
                                 "3 - Transforming" = "#2166AC")) +
    labs(title = title, x = NULL, y = str_to_title(str_replace_all(outcome_var, "_", " "))) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1),
          legend.position = "none", plot.title = element_text(face = "bold"))
}

#boxplot of outcome by cohort
build_cohort_boxplot<-function(df, cohort_col, outcome_var, title) {
  ggplot(df, aes(x = .data[[cohort_col]], y = .data[[outcome_var]])) +
    geom_boxplot(fill = "#4393C3", na.rm = TRUE, outlier.alpha = 0.5) +
    labs(title = title, x = NULL, y = str_to_title(str_replace_all(outcome_var, "_", " "))) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          plot.title = element_text(face = "bold"))
}

#dodged bar chart comparing lag-0 (same year) vs lag-1 (next year)
#correlation strength, one bar pair per CBS strategy. Expects lag_df to
#carry a predictor_label column (title-case) for the axis text
build_lag_barplot<-function(lag_df, title) {
  ggplot(lag_df, aes(x = predictor_label, y = estimate, fill = lag)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    coord_flip() +
    scale_fill_manual(values = c(`Same Year (lag 0)` = "#2166AC", `Next Year (lag 1)` = "#B2182B")) +
    labs(title = title, x = NULL, y = "Spearman Correlation", fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
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
#Elementary" with different retention/shifting values - same 35-vs-34
#school discrepancy flagged in 01-clean-reshape-retention-data.R. Per
#project decision, treating these as two distinct schools ("Marshall
#Elementary A" / "Marshall Elementary B") rather than averaging or
#dropping one - see 03-plot-retention-cbs-trends.R for the same fix.

retention_apr<-retention_apr %>%
  arrange(school_name, academic_year) %>%
  group_by(school_name, academic_year) %>%
  mutate(row_in_group = row_number()) %>%
  ungroup() %>%
  mutate(school_name = if_else(school_name == "Marshall Elementary",
                               paste("Marshall Elementary", if_else(row_in_group == 1, "A", "B")),
                               school_name)) %>%
  select(-row_in_group)

dup_check<-retention_apr %>%
  count(school_name, academic_year) %>%
  filter(n > 1)

if (nrow(dup_check) > 0) {
  stop("Duplicate school-year rows remain after the Marshall Elementary fix:\n",
       paste(capture.output(print(dup_check)), collapse = "\n"))
}

#order academic years chronologically (needed for the lag/lead in Part 8)
year_levels<-retention_apr %>%
  distinct(academic_year) %>%
  arrange(academic_year) %>%
  pull(academic_year)

retention_apr<-retention_apr %>%
  mutate(academic_year = factor(academic_year, levels = year_levels, ordered = TRUE))

## -----------------------------------------------------------------------------
## Part 3 - Define Predictor Variables
## -----------------------------------------------------------------------------

#every numeric column except the two outcomes themselves - picked up
#programmatically so this still works if columns are added/renamed later
numeric_vars<-retention_apr %>%
  select(where(is.numeric)) %>%
  names()

predictor_vars<-setdiff(numeric_vars, outcome_vars)

#the 5 CBS strategy scores specifically - used in Parts 6 and 8
cbs_vars<-predictor_vars[str_starts(predictor_vars, "cbs_")]

#predictors shown in the Part 5 scatterplots: the 5 CBS strategy scores
#plus one non-CBS predictor kept in for its relevance to CBS strategy
#work specifically
scatter_vars<-c(cbs_vars)

#long-format table used by Part 4 (correlations) - CBS strategy scores
#only, one row per school x year x predictor
predictors_long<-retention_apr %>%
  select(school_name, academic_year, all_of(cbs_vars), all_of(outcome_vars)) %>%
  pivot_longer(cols = all_of(cbs_vars), names_to = "predictor", values_to = "predictor_value")

## -----------------------------------------------------------------------------
## Part 4 - Analysis 1: Correlation Matrix & Heatmap
## -----------------------------------------------------------------------------

#pearson + spearman correlation of every predictor against every outcome
corr_table<-map_dfr(outcome_vars, function(outcome_var) {
  bind_rows(compute_cor_by_group(predictors_long, "predictor", "predictor_value", outcome_var, method = "pearson"),
            compute_cor_by_group(predictors_long, "predictor", "predictor_value", outcome_var, method = "spearman")) %>%
    mutate(outcome = outcome_var)
}) %>%
  select(outcome, predictor, method, estimate, p_value, n) %>%
  mutate(predictor_label = format_var_label(predictor),
         outcome_label   = format_var_label(outcome)) %>%
  arrange(outcome, method, desc(abs(estimate)))

corr_heatmap<-build_corr_heatmap(corr_table, "Correlation Between Retention Rate and CBS Strategy Scores")

message(sprintf("Part 4 - built correlation table (%d rows) and 1 combined heatmap", nrow(corr_table)))

## -----------------------------------------------------------------------------
## Part 5 - Analysis 2: Faceted Scatterplots
## -----------------------------------------------------------------------------

#dedicated long-format table for the scatterplots, built straight from
#retention_apr rather than reusing predictors_long (which only carries
#the CBS variables) since scatter_vars also includes one non-CBS predictor
scatter_predictors_long<-retention_apr %>%
  select(school_name, academic_year, all_of(scatter_vars), all_of(outcome_vars)) %>%
  pivot_longer(cols = all_of(scatter_vars), names_to = "predictor", values_to = "predictor_value")

#pearson r per predictor x outcome (matches the lm trend line drawn on
#each panel) - used to label each facet with its r value, starred when
#p < 0.05
scatter_corr<-map_dfr(outcome_vars, function(outcome_var) {
  compute_cor_by_group(scatter_predictors_long, "predictor", "predictor_value", outcome_var, method = "pearson") %>%
    mutate(outcome = outcome_var)
})

#builds "CBS Collaborative Leadership (r = 0.43*)" style facet labels for
#one outcome, then joins them onto that outcome's scatter data
build_scatter_label_df<-function(outcome_var) {
  scatter_corr %>%
    filter(outcome == outcome_var) %>%
    mutate(predictor_label = paste0(format_var_label(predictor),
                                    "\n(r = ", ifelse(is.na(estimate), "NA", sprintf("%.2f", estimate)),
                                    ifelse(!is.na(p_value) & p_value < 0.05, "*", ""), ")")) %>%
    select(predictor, predictor_label)
}

scatter_plots<-map(outcome_vars, function(outcome_var) {
  plot_df<-scatter_predictors_long %>%
    left_join(build_scatter_label_df(outcome_var), by = "predictor")
  
  build_scatter_facets(plot_df, outcome_var,
                       title = paste0(str_to_title(str_replace_all(outcome_var, "_", " ")),
                                      " vs. CBS Strategy Scores"))
}) %>%
  set_names(outcome_vars)

message("Part 5 - built faceted scatterplots for ", length(scatter_plots), " outcomes")

## -----------------------------------------------------------------------------
## Part 6 - Analysis 3: CBS Maturity Stage Comparison (Visioning / Engaging /
## Transforming), one-way ANOVA per CBS strategy
## -----------------------------------------------------------------------------

#CBS scores map onto 3 named maturity stages on the 1-3 scale
cbs_stage_labels<-c(`1` = "1 - Visioning", `2` = "2 - Engaging", `3` = "3 - Transforming")

cbs_group_long<-predictors_long %>%
  filter(predictor %in% cbs_vars, !is.na(predictor_value)) %>%
  mutate(stage = factor(cbs_stage_labels[as.character(predictor_value)],
                        levels = cbs_stage_labels),
         predictor_label = format_var_label(predictor))

#one-way ANOVA of outcome ~ stage for every CBS strategy x outcome -
#tryCatch guards against a strategy having fewer than 2 stages
#represented (aov() errors in that case)
cbs_stage_anova_results<-map_dfr(outcome_vars, function(outcome_var) {
  cbs_group_long %>%
    group_by(predictor) %>%
    group_modify(function(d, key) {
      
      fit<-tryCatch(aov(as.formula(paste0(outcome_var, " ~ stage")), data = d),
                    error = function(e) NULL)
      fit_summary<-if (is.null(fit)) NULL else summary(fit)[[1]]
      
      tibble(
        n       = sum(!is.na(d[[outcome_var]])),
        f_stat  = if (is.null(fit_summary)) NA_real_ else fit_summary[1, "F value"],
        p_value = if (is.null(fit_summary)) NA_real_ else fit_summary[1, "Pr(>F)"]
      )
    }) %>%
    mutate(outcome = outcome_var)
}) %>%
  select(outcome, predictor, n, f_stat, p_value)

#group means/n per stage, for context alongside the ANOVA p-values
cbs_stage_group_summary<-map_dfr(outcome_vars, function(outcome_var) {
  cbs_group_long %>%
    group_by(predictor, stage) %>%
    summarise(mean_value = mean(.data[[outcome_var]], na.rm = TRUE),
              n = sum(!is.na(.data[[outcome_var]])), .groups = "drop") %>%
    mutate(outcome = outcome_var)
}) %>%
  select(outcome, predictor, stage, mean_value, n)

cbs_stage_boxplots<-map(outcome_vars, function(outcome_var) {
  build_stage_boxplot(cbs_group_long, outcome_var,
                      title = paste0(str_to_title(str_replace_all(outcome_var, "_", " ")),
                                     " by CBS Maturity Stage"))
}) %>%
  set_names(outcome_vars)

message(sprintf("Part 6 - built CBS stage ANOVA table (%d rows) and %d boxplots",
                nrow(cbs_stage_anova_results), length(cbs_stage_boxplots)))

## -----------------------------------------------------------------------------
## Part 7 - Analysis 4: Cohort Comparisons
## -----------------------------------------------------------------------------

cohort_vars<-c("ccspp_cohort", "sdusd_cohort")

#one-way ANOVA of outcome ~ cohort, for every cohort_var x outcome combo
cohort_anova_results<-map_dfr(cohort_vars, function(cohort_var) {
  map_dfr(outcome_vars, function(outcome_var) {
    
    d<-retention_apr %>% filter(!is.na(.data[[cohort_var]]), !is.na(.data[[outcome_var]]))
    
    fit<-aov(as.formula(paste0(outcome_var, " ~ ", cohort_var)), data = d)
    fit_summary<-summary(fit)[[1]]
    
    tibble(cohort_var = cohort_var, outcome = outcome_var,
           f_stat = fit_summary[1, "F value"],
           p_value = fit_summary[1, "Pr(>F)"],
           n = nrow(d))
  })
}) %>%
  arrange(outcome, cohort_var)

#group means/n per cohort level, for context alongside the ANOVA p-values
cohort_group_summary<-map_dfr(cohort_vars, function(cohort_var) {
  map_dfr(outcome_vars, function(outcome_var) {
    retention_apr %>%
      filter(!is.na(.data[[cohort_var]])) %>%
      group_by(cohort_level = .data[[cohort_var]]) %>%
      summarise(mean_value = mean(.data[[outcome_var]], na.rm = TRUE),
                n = sum(!is.na(.data[[outcome_var]])), .groups = "drop") %>%
      mutate(cohort_var = cohort_var, outcome = outcome_var)
  })
}) %>%
  select(cohort_var, outcome, cohort_level, mean_value, n)

cohort_plots<-map(cohort_vars, function(cohort_var) {
  map(outcome_vars, function(outcome_var) {
    build_cohort_boxplot(retention_apr %>% filter(!is.na(.data[[cohort_var]])), cohort_var, outcome_var,
                         title = paste0(str_to_title(str_replace_all(outcome_var, "_", " ")),
                                        " by ", str_to_title(str_replace_all(cohort_var, "_", " "))))
  }) %>%
    set_names(outcome_vars)
}) %>%
  set_names(cohort_vars)

message(sprintf("Part 7 - built cohort ANOVA table (%d rows) and %d boxplots",
                nrow(cohort_anova_results), length(cohort_vars) * length(outcome_vars)))

## -----------------------------------------------------------------------------
## Part 8 - Analysis 5: Lagged Correlation (CBS Score vs. Next-Year Outcome)
## -----------------------------------------------------------------------------

#within each school, ordered by year, lead(outcome, 1) pulls next year's
#value alongside this year's CBS scores - lets us compare "same year"
#correlation against "does this year's strategy score predict NEXT
#year's outcome" correlation
retention_apr_leads<-retention_apr %>%
  arrange(school_name, academic_year) %>%
  group_by(school_name) %>%
  mutate(retention_rate_lead1 = lead(retention_rate)) %>%
  ungroup()

cbs_lag_long<-retention_apr_leads %>%
  select(school_name, academic_year, all_of(cbs_vars),
         retention_rate, retention_rate_lead1) %>%
  pivot_longer(cols = all_of(cbs_vars), names_to = "predictor", values_to = "predictor_value")

lagged_results<-map_dfr(outcome_vars, function(outcome_var) {
  lead_var<-paste0(outcome_var, "_lead1")
  
  bind_rows(
    compute_cor_by_group(cbs_lag_long, "predictor", "predictor_value", outcome_var, method = "spearman") %>%
      mutate(lag = "Same Year (lag 0)"),
    compute_cor_by_group(cbs_lag_long, "predictor", "predictor_value", lead_var, method = "spearman") %>%
      mutate(lag = "Next Year (lag 1)")
  ) %>%
    mutate(outcome = outcome_var)
}) %>%
  select(outcome, predictor, lag, estimate, p_value, n) %>%
  mutate(predictor_label = format_var_label(predictor),
         outcome_label   = format_var_label(outcome))

lagged_plots<-map(outcome_vars, function(outcome_var) {
  build_lag_barplot(lagged_results %>% filter(outcome == outcome_var),
                    title = paste0("Lag-0 vs. Lag-1 Correlation: CBS Scores vs. ",
                                   str_to_title(str_replace_all(outcome_var, "_", " "))))
}) %>%
  set_names(outcome_vars)

message(sprintf("Part 8 - built lagged correlation table (%d rows) and %d bar charts",
                nrow(lagged_results), length(lagged_plots)))

## -----------------------------------------------------------------------------
## Part 9 - Export Data
## -----------------------------------------------------------------------------

#capitalizes just the first letter of a column name and swaps
#underscores for spaces (e.g. p_value -> "P value", f_stat -> "F stat")
capitalize_header<-function(x) {
  x<-str_replace_all(x, "_", " ")
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

#rounding/label-formatting is done here, at export time, rather than on
#the working tables above - build_corr_heatmap's significance stars and
#scatter_corr's facet labels need the FULL-PRECISION p_value (rounding
#first could flip a borderline p=0.049 to a non-significant-looking 0.05)

corr_table_export<-corr_table %>%
  transmute(outcome    = outcome_label,
            predictor   = predictor_label,
            method,
            estimate    = round(estimate, 2),
            p_value     = round(p_value, 2),
            n) %>%
  rename_with(capitalize_header)

cbs_stage_anova_export<-cbs_stage_anova_results %>%
  mutate(outcome   = format_var_label(outcome),
         predictor  = format_var_label(predictor),
         f_stat     = round(f_stat, 2),
         p_value    = round(p_value, 2)) %>%
  rename_with(capitalize_header)

cbs_stage_group_summary_export<-cbs_stage_group_summary %>%
  mutate(outcome   = format_var_label(outcome),
         predictor  = format_var_label(predictor),
         mean_value = round(mean_value, 2)) %>%
  rename_with(capitalize_header)

cohort_anova_export<-cohort_anova_results %>%
  mutate(cohort_var = format_var_label(cohort_var),
         outcome     = format_var_label(outcome),
         f_stat      = round(f_stat, 2),
         p_value     = round(p_value, 2)) %>%
  rename_with(capitalize_header)

cohort_group_summary_export<-cohort_group_summary %>%
  mutate(cohort_var = format_var_label(cohort_var),
         outcome     = format_var_label(outcome),
         mean_value = round(mean_value,2)) %>%
  rename_with(capitalize_header)

lagged_results_export<-lagged_results %>%
  transmute(outcome    = outcome_label,
            predictor   = predictor_label,
            lag,
            estimate    = round(estimate, 2),
            p_value     = round(p_value, 2),
            n) %>%
  rename_with(capitalize_header)

#numeric results -> CSV
fwrite(corr_table_export, file.path(output_dir, "correlation_table.csv"))
fwrite(cbs_stage_anova_export, file.path(output_dir, "cbs_stage_anova_results.csv"))
fwrite(cbs_stage_group_summary_export, file.path(output_dir, "cbs_stage_group_summary.csv"))
fwrite(cohort_anova_export, file.path(output_dir, "cohort_anova_results.csv"))
fwrite(cohort_group_summary_export, file.path(output_dir, "cohort_group_summary.csv"))
fwrite(lagged_results_export, file.path(output_dir, "lagged_correlation_results.csv"))

#plots -> one .RData file, load() restores all 5 objects into the environment
save(corr_heatmap, scatter_plots, cbs_stage_boxplots, cohort_plots, lagged_plots,
     file = file.path(data_dir, "correlation_analysis_plots.RData"))

message("Saved CSV tables to ", output_dir)
message("Saved plot lists to ", file.path(data_dir, "correlation_analysis_plots.RData"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------