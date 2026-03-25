# CHL5222 Final Project
# Data Cleaning + Missing Value Analysis + EDA
library(tidyverse)
library(naniar)
library(knitr)
library(kableExtra)
library(here)


# 1 Import Data
df_raw <- read_csv("vaccine.csv")
glimpse(df_raw)
summary(df_raw)

# 2 Data Cleaning
df <- df_raw %>%
  mutate(
    across(c(year, city, county, type), ~na_if(.x, "null")),
    across(c(year, city, county, type), ~na_if(.x, "")),
    mmr = na_if(mmr, -1),
    overall = na_if(overall, -1)
  )

# 3 Duplicate Check (BEFORE)
# remove exact duplicate rows first
df_nodup_exact <- df %>%
  distinct()

df_summary_data <- df_nodup_exact %>%
  mutate(
    mmr = ifelse(mmr == -1, NA, mmr),
    overall = ifelse(overall == -1, NA, overall),
    
    # Reporting Groups as Factors
    reporting_group = case_when(
      !is.na(mmr) & !is.na(overall) ~ "Overall and MMR",
      !is.na(mmr) & is.na(overall) ~ "MMR Only",
      is.na(mmr) & !is.na(overall) ~ "Overall Only",
      TRUE ~ "Exclude" 
    ),
    reporting_group = factor(reporting_group, 
                             levels = c("Overall and MMR", "MMR Only", "Overall Only")),
    
    school_cat = case_when(
      type == "Public" ~ "Public",
      type == "Private" ~ "Private",
      type %in% c("BOCES", "Charter", "Kindergarten", "Nonpublic") ~ "Other",
      TRUE ~ "Other"
    )
  ) %>%
  filter(!is.na(reporting_group))

# helper function
get_stats <- function(df) {
  u_states <- n_distinct(df$state)
  sch_per_state <- df %>% group_by(state) %>% summarise(n = n()) %>% pull(n)
  
  fmt_mean_sd <- function(x) {
    if(length(na.omit(x)) < 1) return("0 (0)")
    paste0(round(mean(x, na.rm = TRUE), 1), " (", round(sd(x, na.rm = TRUE), 1), ")")
  }
  
  tibble(
    Variable = c(
      "Unique States", 
      "Avg Schools per State", 
      "State Spending in 1000s (2016)",
      "Total Number of Schools (n)", # NEW ROW
      "Enrollment per School", 
      "Public Schools (n)", 
      "Private Schools (n)", 
      "Other Schools (n)"
    ),
    Value = c(
      as.character(u_states),
      fmt_mean_sd(sch_per_state),
      fmt_mean_sd(df$statespending2016),
      as.character(nrow(df)), # Total N
      fmt_mean_sd(df$enroll),
      as.character(sum(df$school_cat == "Public")),
      as.character(sum(df$school_cat == "Private")),
      as.character(sum(df$school_cat == "Other"))
    )
  )
}

total_col <- get_stats(df_summary_data) %>% rename(Overall = Value)

group_cols <- df_summary_data %>%
  group_split(reporting_group) %>%
  map(~{
    g_name <- as.character(unique(.x$reporting_group))
    get_stats(.x) %>% rename(!!g_name := Value)
  }) %>%
  reduce(left_join, by = "Variable")

total_col %>%
  left_join(group_cols, by = "Variable") %>%
  kbl(caption = "Table 1: Descriptive Statistics by Vaccination Reporting Status",
      col.names = c("Variable", "Full Sample", "Overall and MMR", "MMR Only", "Overall Only"),
      align = "lrrrr",
      booktabs = TRUE) %>%
  kable_classic(full_width = F, html_font = "Arial") %>%
  add_header_above(c(" " = 1, "Total Sample" = 1, "Reporting Group Subsets" = 3)) %>%
  pack_rows("Geographic & Fiscal Metrics", 1, 3) %>%
  pack_rows("School Counts & Enrollment", 4, 5) %>%
  pack_rows("School Type Distribution", 6, 8) %>%
  column_spec(1, bold = T)

cat("\nRows before removing exact duplicate rows:", nrow(df))
cat("\nRows after removing exact duplicate rows:", nrow(df_nodup_exact))

# check duplicates using a stricter school identifier
duplicate_before <- df_nodup_exact %>%
  count(state, name, year, type, city, county, name = "dup_n") %>%
  filter(dup_n > 1)

cat("\nNumber of duplicated school-year-location groups:\n")
print(nrow(duplicate_before))

cat("\nDuplicated school-year-location groups BEFORE cleaning:\n")
print(duplicate_before, n = 50)

# inspect duplicated rows
duplicate_records <- df_nodup_exact %>%
  semi_join(
    duplicate_before,
    by = c("state", "name", "year", "type", "city", "county")
  ) %>%
  arrange(state, name, year, type, city, county)

cat("\nDuplicated records:\n")
print(duplicate_records, n = 50)

# check conflicts within duplicate groups
duplicate_conflict <- duplicate_records %>%
  group_by(state, name, year, type, city, county) %>%
  summarise(
    n_rows = n(),
    n_mmr_values = n_distinct(mmr, na.rm = TRUE),
    n_overall_values = n_distinct(overall, na.rm = TRUE),
    n_enroll_values = n_distinct(enroll, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mmr_conflict = n_mmr_values > 1,
    overall_conflict = n_overall_values > 1,
    enroll_conflict = n_enroll_values > 1
  )

cat("\nDuplicate groups with possible conflicts:\n")
print(duplicate_conflict, n = 50)

# 4 Deduplicate Records
# keep the row with the largest enrollment within each duplicated school-year-location
df_dedup <- df_nodup_exact %>%
  mutate(
    outcome_nonmissing = rowSums(!is.na(select(., mmr, overall)))
  ) %>%
  group_by(state, name, year, type, city, county) %>%
  arrange(desc(enroll), desc(outcome_nonmissing), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(-outcome_nonmissing)

cat("\nRows before deduplication:", nrow(df_nodup_exact))
cat("\nRows after deduplication:", nrow(df_dedup))

# 5 Duplicate Check (AFTER)
duplicate_after <- df_dedup %>%
  count(state, name, year, type, city, county, name = "dup_n") %>%
  filter(dup_n > 1)

cat("\nDuplicate school-year-location records AFTER cleaning:\n")
print(duplicate_after)

# use cleaned dataset
df <- df_dedup

# 6 Convert variable types
df <- df %>%
  mutate(
    state = as.factor(state),
    year = as.factor(year),
    type = as.factor(type),
    city = as.factor(city),
    county = as.factor(county)
  )

# 7 Missing Value Analysis
missing_table <- tibble(
  variable = names(df),
  missing_n = colSums(is.na(df)),
  missing_pct = round(colMeans(is.na(df)) * 100, 2)
) %>%
  arrange(desc(missing_n))

cat("\nMissing value summary:\n")
print(missing_table)

# Plot missing summary
ggplot(missing_table,
       aes(x = missing_n, y = reorder(variable, missing_n))) +
  geom_point(size = 3) +
  geom_segment(aes(x = 0, xend = missing_n,
                   y = variable, yend = variable)) +
  labs(
    title = "Number of Missing Values by Variable",
    x = "Missing Count",
    y = "Variable"
  ) +
  theme_minimal()

# 8 Missing Pattern Analysis

# missing MMR by state
mmr_missing_state <- df %>%
  group_by(state) %>%
  summarise(
    schools = n(),
    mmr_missing = sum(is.na(mmr)),
    mmr_missing_pct = mean(is.na(mmr)) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(mmr_missing_pct))

print(mmr_missing_state)

# missing MMR by school type
mmr_missing_type <- df %>%
  group_by(type) %>%
  summarise(
    schools = n(),
    mmr_missing = sum(is.na(mmr)),
    mmr_missing_pct = mean(is.na(mmr)) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(mmr_missing_pct))

print(mmr_missing_type)

# missing overall by state
overall_missing_state <- df %>%
  group_by(state) %>%
  summarise(
    schools = n(),
    overall_missing = sum(is.na(overall)),
    overall_missing_pct = mean(is.na(overall)) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(overall_missing_pct))

print(overall_missing_state)

# missing overall by school type
overall_missing_type <- df %>%
  group_by(type) %>%
  summarise(
    schools = n(),
    overall_missing = sum(is.na(overall)),
    overall_missing_pct = mean(is.na(overall)) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(overall_missing_pct))

print(overall_missing_type)

# 9 Missing Value Handling
# complete-case datasets for outcome-specific analyses
df_mmr <- df %>% filter(!is.na(mmr))
df_overall <- df %>% filter(!is.na(overall))
df_mmr_type <- df %>% filter(!is.na(mmr), !is.na(type))

# 10 Exploratory Data Analysis
# distribution of MMR
ggplot(df_mmr,aes(x=mmr))+
  geom_histogram(bins=30,fill="steelblue",color="white")+
  labs(
    title="Distribution of MMR Vaccination Rates",
    x="MMR Vaccination Rate",
    y="Number of Schools"
  )+
  theme_minimal()

# MMR by school type
ggplot(df_mmr_type,aes(x=type,y=mmr))+
  geom_boxplot()+
  labs(
    title="MMR Vaccination Rate by School Type",
    x="School Type",
    y="MMR Vaccination Rate"
  )+
  theme_minimal()

# state vaccination rate
state_summary <- df_mmr %>%
  group_by(state) %>%
  summarise(mean_mmr=mean(mmr,na.rm=TRUE)) %>%
  arrange(desc(mean_mmr))

ggplot(state_summary,
       aes(x=reorder(state,mean_mmr),y=mean_mmr))+
  geom_col(fill="darkblue")+
  coord_flip()+
  labs(
    title="Average MMR Vaccination Rate by State",
    x="State",
    y="Average MMR Vaccination Rate"
  )+
  theme_minimal()

# 11 Export Final Dataset

write_csv(df, here("data", "analytical", "vaccine_clean_final.csv"))

cat("\nFinal dataset rows:",nrow(df))
cat("\nFinal dataset columns:",ncol(df))


# missingness by state for MMR
mmr_missing_state <- df %>%
  group_by(state) %>%
  summarise(
    schools = n(),
    mmr_missing = sum(is.na(mmr)),
    mmr_missing_pct = mean(is.na(mmr)) * 100,
    .groups = "drop"
  ) %>%
  arrange(mmr_missing_pct)

print(mmr_missing_state)

ggplot(mmr_missing_state,
       aes(x = reorder(state, mmr_missing_pct), y = mmr_missing_pct)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Percentage of Missing MMR Vaccination Rates by State",
    x = "State",
    y = "Missing MMR Rate (%)"
  ) +
  theme_minimal()

overall_missing_state <- df %>%
  group_by(state) %>%
  summarise(
    schools = n(),
    overall_missing = sum(is.na(overall)),
    overall_missing_pct = mean(is.na(overall)) * 100,
    .groups = "drop"
  ) %>%
  arrange(overall_missing_pct)

print(overall_missing_state)

ggplot(overall_missing_state,
       aes(x = reorder(state, overall_missing_pct), y = overall_missing_pct)) +
  geom_col(fill = "gray40") +
  coord_flip() +
  labs(
    title = "Percentage of Missing Overall Vaccination Rates by State",
    x = "State",
    y = "Missing Overall Rate (%)"
  ) +
  theme_minimal()
