################################################################################
##
## [ PROJ ] < Teacher Retention >
## [ FILE ] < 02-build-school-time-series-tables.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 7/10/26 >
##
################################################################################

#Goal: For each school, build a descriptive table that tracks retention_rate,
#      shifting_rate, and all other time-varying APR variables across years.
#      Output is a named list of 35 tables, one per school, with one row
#      per academic year. APR variables only exist for 2022-2025, so those
#      cells are NA for years outside that range (and vice versa for any
#      variable that doesn't cover the full 2017-2024 retention span).

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

#id columns - these describe the school itself, not a specific year
id_vars<-c("school_id", "school_type", "ccspp_cohort", "school_match_name",
           "sdusd_cohort", "cds_code", "lea_name", "school_name")

#some APR variables are already suffixed with a full academic year
#(e.g. "_2022_2023"), but others use a condensed 4-digit code
#(e.g. "_2223"). This standardizes every column to the full
#"_YYYY_YYYY" suffix so they can all be pivoted with one regex.
#Non-year columns are returned unchanged.
normalize_year_suffix<-function(col_names) {
  
  full_pat<-"^(.+)_([0-9]{4})_([0-9]{4})$"
  short_pat<-"^(.+)_([0-9]{2})([0-9]{2})$"
  
  map_chr(col_names, function(nm) {
    
    #already a full "base_YYYY_YYYY" name - leave as is
    if (str_detect(nm, full_pat)) return(nm)
    
    #condensed "base_YYZZ" name - expand only if the two 2-digit
    #chunks are consecutive years (e.g. 22/23), which avoids
    #mistakenly treating an unrelated trailing number as a year
    if (str_detect(nm, short_pat)) {
      parts<-str_match(nm, short_pat)
      base<-parts[2]
      yr1<-as.numeric(parts[3])
      yr2<-as.numeric(parts[4])
      if (yr2 == yr1 + 1) {
        return(str_glue("{base}_20{parts[3]}_20{parts[4]}"))
      }
    }
    
    #no recognizable year suffix - static column, leave as is
    nm
  })
}

## ---------------------------
## load & inspect data
## ---------------------------

retention_wide<-read_csv(file.path(data_dir, "retention_apr_wide_updated.csv"))

## -----------------------------------------------------------------------------
## Part 1 - Clean Data
## -----------------------------------------------------------------------------

#clean columns
retention_wide<-clean_names(retention_wide)

#remove duplicates
retention_wide<-distinct(retention_wide)

#standardize every year suffix to "_YYYY_YYYY"
names(retention_wide)<-normalize_year_suffix(names(retention_wide))

#check for schools that appear more than once in the wide data - if
#this fires, a school is duplicated upstream and will silently double
#its row count once reshaped long (this is almost certainly why
#"marshall" ended up with 10 rows instead of 5 - it's in retention_wide
#twice)
dup_schools<-retention_wide %>% 
  count(school_match_name) %>% 
  filter(n > 1)

# if (nrow(dup_schools) > 0) {
#   message("Duplicate school_match_name found in retention_wide:")
#   print(dup_schools)
#   stop("Resolve the duplicate row(s) above before reshaping - ",
#        "check whether it's an exact duplicate (safe to drop with ",
#        "distinct()) or two genuinely different rows sharing a name.")
# }

## -----------------------------------------------------------------------------
## Part 2 - Reshape to Long (One Row per School x Academic Year)
## -----------------------------------------------------------------------------

year_pat<-"^(.+)_([0-9]{4}_[0-9]{4})$"

#columns that carry a year suffix vs. columns that are static per school
year_cols<-names(retention_wide)[str_detect(names(retention_wide), year_pat) &
                                   !names(retention_wide) %in% id_vars]
static_cols<-setdiff(names(retention_wide), c(id_vars, year_cols))

#static, school-level table (kept aside - not "over time" data)
school_static<-retention_wide %>% 
  select(all_of(id_vars), all_of(static_cols))

#long panel: one row per school x academic_year, one column per
#time-varying variable (retention_rate, shifting_rate, and every
#time-varying APR variable). pivot_longer's ".value" fills any
#school x year x variable combination that didn't exist in the wide
#data with NA, which is exactly the "blank for non-overlapping years"
#behavior we want.
school_year_long<-retention_wide %>% 
  select(all_of(id_vars), all_of(year_cols)) %>% 
  pivot_longer(cols = all_of(year_cols),
               names_to = c(".value", "academic_year"),
               names_pattern = year_pat) %>% 
  arrange(school_match_name, academic_year)

#keep 2020-21 onward only - drop the earlier academic years
#(academic_year's leading 4 digits are the start year, e.g.
#"2020_2021" starts in 2020)
school_year_long<-school_year_long %>% 
  filter(as.numeric(str_sub(academic_year, 1, 4)) >= 2020)

## -----------------------------------------------------------------------------
## Part 3 - Split Into One Descriptive Table per School
## -----------------------------------------------------------------------------

#named list of 35 tables, one per school - each table is that
#school's retention_rate, shifting_rate, and APR variables by
#academic_year
school_tables<-school_year_long %>% 
  group_split(school_match_name) %>% 
  set_names(map_chr(., ~unique(.x$school_match_name)))

#make them all data frames
school_tables<-map(school_tables, function(x) data.frame(x))

#example - pull up a single school's table
school_tables[[1]]

## -----------------------------------------------------------------------------
## Part 4 - Export Data
## -----------------------------------------------------------------------------

#stack the per-school tables back into one long file (school_tables is
#just school_year_long split apart, so bind_rows reassembles it in the
#same row order/shape). IMPORTANT: fwrite() must be given this bound
#tibble, never school_tables itself - passing the raw list to fwrite()
#(or data.frame()/tibble()) treats each school's table as one column,
#which is what threw the "Can't recycle `baker`...` to match
#`marshall`..." error, since schools don't all have the same row count.
school_year_export<-bind_rows(school_tables)

stopifnot(is.data.frame(school_year_export))

#select only related variables
school_year_export<-school_year_export |> 
  select(school_name, ccspp_cohort, sdusd_cohort,
         academic_year, retention_rate, shifting_rate,
         cbs_community_based_learning, cbs_collaborative_leadership,
         cbs_shared_commitment, cbs_shared_commitment,
         cbs_strategic_partnerships, cbs_sustaining_staff,
         supports_importance_teacher_leadership_development_and_opportunities,
         mean_activities_collaborative_leadership,
         mean_activities_community_based_learning,
         total_collaborative_leadership,
         total_community_based_learning,
         mean_served_teacher_leadership,
         mean_served_collaborative_leadership,
         mean_served_community_based_learning,
         mean_served_students_collaborative_leadership,
         mean_served_students_community_based_learning)

#what are the codes for(1-3)

fwrite(school_year_export,
       file.path(data_dir, "school_retention_apr_long_2020_2024.csv"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------