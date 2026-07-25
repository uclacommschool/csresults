################################################################################
##
## [ PROJ ] < Teacher Retention >
## [ FILE ] < 01-clean-reshape-retention-data.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 7/10/26 >
##
################################################################################

#Goal: Clean the merged APR/retention data - keep only schools with a
#      ccspp_cohort assignment, then reshape the retention metrics from
#      long (one row per school x academic_year) to wide (one row per
#      school, one column set per academic_year) for analysis.

################################################################################

## ---------------------------
## libraries
## ---------------------------
library(tidyverse)
library(data.table)
library(openxlsx)
library(readxl)
library(janitor)

## ---------------------------
## directory paths
## ---------------------------

#see current directory
getwd()

data_dir<-file.path("C:/Users/jyo/Box/TR Data/SDUSD/original","data")

## ---------------------------
## helper functions & strings
## ---------------------------

#the retention metrics below are the only columns that vary by
#academic_year_id within a school - everything else (demographics,
#supports, cbs, engagement, etc.) is already static per school, so
#these are the only columns we pivot wide
retention_metrics<-c("total_teachers", "retained", "stayers",
                     "shifters", "leavers", "retention_rate",
                     "shifting_rate")

## ---------------------------
## load & inspect data
## ---------------------------

retention_apr<-read_excel(file.path(data_dir, "retention_apr_merged_all.xlsx"),
                          sheet = "Sheet1")

## -----------------------------------------------------------------------------
## Part 1 - Clean Data
## -----------------------------------------------------------------------------

#clean columns
retention_apr<-clean_names(retention_apr)

#remove duplicates
retention_apr<-distinct(retention_apr)

#keep only records with a ccspp_cohort value
#(ccspp_cohort is fixed per school - it's either assigned for all of a
#school's rows or missing for all of them, so this drops schools
#that were never part of a CCSPP cohort)
retention_apr<-retention_apr %>% 
  filter(!is.na(ccspp_cohort))

#quick check on how many schools/cohorts remain
check_cohort<-retention_apr %>% count(ccspp_cohort, school_id)
count(retention_apr, ccspp_cohort)

## -----------------------------------------------------------------------------
## Part 2 - Split Into Two Datasets
## -----------------------------------------------------------------------------

#the merge isn't clean as one flat table - the retention metrics are
#truly long (one row per school x academic_year), while everything
#else is really a wide, one-row-per-school table. Splitting them apart
#so each can be handled on its own terms.

#dataset 1: the long retention panel (school_type through cds_code)
retention_long<-retention_apr %>% 
  select(school_type:cds_code)

#dataset 2: everything else, keyed back to school_match_name so it
#can be joined back to retention_long later
#(school_match_name sits inside the school_type:cds_code range, so we
#OR it back in rather than listing it separately - listing it
#separately alongside a negated range that contains it would cancel
#it back out)
school_apr<-retention_apr %>% 
  select(!(school_type:sdusd_cohort) | school_match_name)

school_apr<-distinct(school_apr)
#Note: Marshall Middle and Marshall High are here

## -----------------------------------------------------------------------------
## Part 2.1 - Reshape Long to Wide by Academic Year
## -----------------------------------------------------------------------------

#clean up academic_year_id for use as a column-name suffix
#(pivot_wider will paste this onto the metric name, and a bare hyphen
#like "2017-2018" is awkward in a column name, so swap it for an underscore)
retention_long<-retention_long %>% 
  mutate(academic_year_id = str_replace(academic_year_id, "-", "_"))

#everything except the year and the retention metrics is static per
#school, so it becomes the id_cols for the pivot
id_cols<-retention_long %>% 
  select(-academic_year_id, -all_of(retention_metrics)) %>% 
  names()

retention_wide<-retention_long %>% 
  pivot_wider(id_cols = all_of(id_cols),
             names_from = academic_year_id,
             values_from = all_of(retention_metrics),
             names_sep = "_")

retention_wide<-distinct(retention_wide)

## -----------------------------------------------------------------------------
## Part 2.2 - Merge Data together
## -----------------------------------------------------------------------------

#merge data 
retention_update<-left_join(retention_wide,
                            school_apr, by = c("cds_code", "school_match_name"))

#rearrange so the id columns lead
retention_update<-retention_update %>% 
  select(school_id, school_type, ccspp_cohort, everything())

## -----------------------------------------------------------------------------
## Part 3 - Export Data
## -----------------------------------------------------------------------------

fwrite(retention_update,
      file.path(data_dir, "retention_apr_wide_updated.csv"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
