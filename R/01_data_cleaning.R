# 1. Load Packages
library(tidyverse)
library(naniar)
library(mice)
library(forcats)
library(ggrepel)


# 2. Import Data
df_raw <- read_csv("data/original/vaccine.csv")

glimpse(df_raw)
summary(df_raw)

# 3. Data Cleaning
df <- df_raw %>%
  mutate(
    across(c(year, city, county, type), ~ na_if(.x, "null")),
    across(c(year, city, county, type), ~ na_if(.x, "")),
    mmr = na_if(mmr, -1),
    overall = na_if(overall, -1)
  )

# 4. Duplicate Check Before Deduplication
# 4.1 Remove Exact Duplicate Rows
df_nodup_exact <- df %>%
  distinct()

tibble(
  stage = c("Before removing exact duplicates", "After removing exact duplicates"),
  rows = c(nrow(df), nrow(df_nodup_exact))
) %>%
  print(caption = "Row Counts Before and After Removing Exact Duplicate Rows") 

# 4.2 Check Duplicated School-Year-Location Groups
duplicate_before <- df_nodup_exact %>%
  count(state, name, year, type, city, county, name = "dup_n") %>%
  filter(dup_n > 1)

duplicate_before %>%
  print(caption = "Duplicated School-Year-Location Groups Before Cleaning") 

# 4.3 Inspect Duplicated Records
duplicate_records <- df_nodup_exact %>%
  semi_join(
    duplicate_before,
    by = c("state", "name", "year", "type", "city", "county")
  ) %>%
  arrange(state, name, year, type, city, county)

duplicate_records %>%
  print(caption = "Duplicated Records")  

# 4.4 Check Conflicts Within Duplicate Groups
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

duplicate_conflict %>%
  print(caption = "Duplicate Groups With Possible Conflicts")  

# 5. Deduplicate Records
df_dedup <- df_nodup_exact %>%
  mutate(
    outcome_nonmissing = rowSums(!is.na(select(., mmr, overall)))
  ) %>%
  group_by(state, name, year, type, city, county) %>%
  arrange(desc(enroll), desc(outcome_nonmissing), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(-outcome_nonmissing)

tibble(
  stage = c("Before deduplication", "After deduplication"),
  rows = c(nrow(df_nodup_exact), nrow(df_dedup))
) %>%
  print(caption = "Row Counts Before and After Deduplication") 

# 6. Duplicate Check After Deduplication
duplicate_after <- df_dedup %>%
  count(state, name, year, type, city, county, name = "dup_n") %>%
  filter(dup_n > 1)

duplicate_after %>%
  print(caption = "Duplicate School-Year-Location Records After Cleaning") 

df <- df_dedup
df_missing <- df

# 7. Table 1: Descriptive Statistics by Vaccination Reporting Status
df_summary_data <- df %>%
  mutate(
    reporting_group = case_when(
      !is.na(mmr) & !is.na(overall) ~ "Overall and MMR",
      !is.na(mmr) & is.na(overall) ~ "MMR Only",
      is.na(mmr) & !is.na(overall) ~ "Overall Only",
      TRUE ~ "Exclude"
    ),
    reporting_group = factor(
      reporting_group,
      levels = c("Overall and MMR", "MMR Only", "Overall Only", "Exclude")
    ),
    school_cat = case_when(
      type == "Public" ~ "Public",
      type == "Private" ~ "Private",
      type %in% c("BOCES", "Charter", "Kindergarten", "Nonpublic") ~ "Other",
      TRUE ~ "Other"
    )
  ) %>%
  filter(reporting_group != "Exclude")

get_stats <- function(df_in) {
  u_states <- n_distinct(df_in$state)
  sch_per_state <- df_in %>%
    group_by(state) %>%
    summarise(n = n(), .groups = "drop") %>%
    pull(n)

  fmt_mean_sd <- function(x) {
    if (length(na.omit(x)) < 1) return("0 (0)")
    paste0(round(mean(x, na.rm = TRUE), 1), " (", round(sd(x, na.rm = TRUE), 1), ")")
  }

  tibble(
    Variable = c(
      "Unique States",
      "Avg Schools per State",
      "State Spending in 1000s (2016)",
      "Total Number of Schools (n)",
      "Enrollment per School",
      "Public Schools (n)",
      "Private Schools (n)",
      "Other Schools (n)"
    ),
    Value = c(
      as.character(u_states),
      fmt_mean_sd(sch_per_state),
      fmt_mean_sd(df_in$statespending2016),
      as.character(nrow(df_in)),
      fmt_mean_sd(df_in$enroll),
      as.character(sum(df_in$school_cat == "Public", na.rm = TRUE)),
      as.character(sum(df_in$school_cat == "Private", na.rm = TRUE)),
      as.character(sum(df_in$school_cat == "Other", na.rm = TRUE))
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

table1 <- total_col %>%
  left_join(group_cols, by = "Variable")

colnames(table1) <- c("Variable", "Full Sample", "Overall and MMR", "MMR Only", "Overall Only")

print(table1)

# 8. Missing Value Analysis
# 8.1 Convert Variable Types for Missingness Assessment
df_missing <- df_missing %>%
  mutate(
    state = as.factor(state),
    year = as.factor(year),
    type = as.factor(type),
    city = as.factor(city),
    county = as.factor(county)
  )

# 8.2 Missing Value Summary
missing_table <- tibble(
  variable = names(df_missing),
  missing_n = colSums(is.na(df_missing)),
  missing_pct = round(colMeans(is.na(df_missing)) * 100, 2)
) %>%
  arrange(desc(missing_n))

missing_table %>%
  print(caption = "Missing Value Summary") 

# 8.3 Missing Value Plot
ggplot(missing_table,
       aes(x = missing_n, y = reorder(variable, missing_n))) +
  geom_point(size = 3) +
  geom_segment(aes(x = 0, xend = missing_n,
                   y = variable, yend = variable)) +
  geom_text(
    aes(label = paste0(missing_n, " (", missing_pct, "%)")),
    hjust = -0.1,
    size = 3
  ) +
  labs(
    title = "Number of Missing Values by Variable",
    x = "Missing Count",
    y = "Variable"
  ) +
  theme_minimal() +
  expand_limits(x = max(missing_table$missing_n) * 1.15)

# 9. Missing Pattern Analysis Using Original Missing Data
# 9.1 Missing MMR by State
mmr_missing_state <- df_missing %>%
  group_by(state) %>%
  summarise(
    schools = n(),
    mmr_missing = sum(is.na(mmr)),
    mmr_missing_pct = mean(is.na(mmr)) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(mmr_missing_pct))

mmr_missing_state %>%
  print(caption = "Missing MMR by State") 

# 9.2 Missing MMR by School Type
mmr_missing_type <- df_missing %>%
  group_by(type) %>%
  summarise(
    schools = n(),
    mmr_missing = sum(is.na(mmr)),
    mmr_missing_pct = mean(is.na(mmr)) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(mmr_missing_pct))

mmr_missing_type %>%
  print(caption = "Missing MMR by School Type") 

# 9.3 Missing Overall by State
overall_missing_state <- df_missing %>%
  group_by(state) %>%
  summarise(
    schools = n(),
    overall_missing = sum(is.na(overall)),
    overall_missing_pct = mean(is.na(overall)) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(overall_missing_pct))

overall_missing_state %>%
  print(caption = "Missing Overall Vaccination Rate by State") 

# 9.4 Missing Overall by School Type
overall_missing_type <- df_missing %>%
  group_by(type) %>%
  summarise(
    schools = n(),
    overall_missing = sum(is.na(overall)),
    overall_missing_pct = mean(is.na(overall)) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(overall_missing_pct))

overall_missing_type %>%
  print(caption = "Missing Overall Vaccination Rate by School Type") 

# 10. Create Analysis-Ready Dataset
df <- df_missing %>%
  mutate(
    state = as.factor(state),
    year = fct_explicit_na(as.factor(year), na_level = "Unknown"),
    type = fct_explicit_na(as.factor(type), na_level = "Unknown"),
    city = fct_explicit_na(as.factor(city), na_level = "Unknown"),
    county = fct_explicit_na(as.factor(county), na_level = "Unknown")
  )

# 11. Missingness Visualization
# 11.1 Missing MMR by State
ggplot(mmr_missing_state,
       aes(x = reorder(state, mmr_missing_pct), y = mmr_missing_pct)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  geom_text(
    aes(label = paste0(mmr_missing, " (", round(mmr_missing_pct, 1), "%)")),
    hjust = -0.1,
    size = 3
  ) +
  labs(
    title = "Percentage of Missing MMR by State",
    subtitle = paste("n =", sum(mmr_missing_state$schools), "schools"),
    x = "State",
    y = "Missing MMR (%)"
  ) +
  theme_minimal() +
  expand_limits(y = max(mmr_missing_state$mmr_missing_pct) * 1.1)

# 11.2 Missing Overall by State
ggplot(overall_missing_state,
       aes(x = reorder(state, overall_missing_pct), y = overall_missing_pct)) +
  geom_col(fill = "gray40") +
  coord_flip() +
  geom_text(
    aes(label = paste0(overall_missing, " (", round(overall_missing_pct, 1), "%)")),
    hjust = -0.1,
    size = 3
  ) +
  labs(
    title = "Percentage of Missing Overall Vaccination Rate by State",
    subtitle = paste("n =", sum(overall_missing_state$schools), "schools"),
    x = "State",
    y = "Missing Overall (%)"
  ) +
  theme_minimal() +
  expand_limits(y = max(overall_missing_state$overall_missing_pct) * 1.1)

# 11.3 Missing MMR by School Type
ggplot(mmr_missing_type,
       aes(x = reorder(type, mmr_missing_pct), y = mmr_missing_pct)) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = paste0(mmr_missing, " (", round(mmr_missing_pct, 1), "%)")),
    vjust = -0.3,
    size = 3
  ) +
  labs(
    title = "Percentage of Missing MMR by School Type",
    subtitle = paste("n =", sum(mmr_missing_type$schools), "schools"),
    x = "School Type",
    y = "Missing MMR (%)"
  ) +
  theme_minimal()

# 11.4 Missing Overall by School Type
ggplot(overall_missing_type,
       aes(x = reorder(type, overall_missing_pct), y = overall_missing_pct)) +
  geom_col(fill = "gray40") +
  geom_text(
    aes(label = paste0(overall_missing, " (", round(overall_missing_pct, 1), "%)")),
    vjust = -0.3,
    size = 3
  ) +
  labs(
    title = "Percentage of Missing Overall Vaccination Rate by School Type",
    subtitle = paste("n =", sum(overall_missing_type$schools), "schools"),
    x = "School Type",
    y = "Missing Overall (%)"
  ) +
  theme_minimal()

# 12. Reporting Pattern Summary
report_type_summary <- df %>%
  summarise(
    both = sum(!is.na(mmr) & !is.na(overall)),
    mmr_only = sum(!is.na(mmr) & is.na(overall)),
    overall_only = sum(is.na(mmr) & !is.na(overall)),
    none = sum(is.na(mmr) & is.na(overall))
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "report_type",
    values_to = "schools"
  ) %>%
  mutate(
    pct = round(schools / sum(schools) * 100, 2)
  )

report_type_summary %>%
  print(caption = "Reporting Pattern Summary") 


ggplot(report_type_summary, aes(x = report_type, y = schools)) +
  geom_col(fill = "tomato") +
  geom_text(
    aes(label = paste0(schools, " (", pct, "%)")),
    vjust = -0.3,
    size = 4
  ) +
  labs(
    title = "Distribution of Vaccination Reporting Pattern",
    subtitle = paste("n =", sum(report_type_summary$schools), "schools"),
    x = "Reporting Pattern",
    y = "Number of Schools"
  ) +
  theme_minimal()

# 12.1 Reporting Pattern by State
report_type_state <- df %>%
  group_by(state) %>%
  summarise(
    both = sum(!is.na(mmr) & !is.na(overall)),
    mmr_only = sum(!is.na(mmr) & is.na(overall)),
    overall_only = sum(is.na(mmr) & !is.na(overall)),
    none = sum(is.na(mmr) & is.na(overall)),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(both, mmr_only, overall_only, none),
    names_to = "report_type",
    values_to = "schools"
  ) %>%
  group_by(state) %>%
  mutate(
    pct = schools / sum(schools) * 100,
    label = ifelse(pct > 10, paste0(schools, " (", round(pct, 1), "%)"), "")
  ) %>%
  ungroup()

ggplot(report_type_state,
       aes(x = state, y = schools, fill = report_type)) +
  geom_col(position = "fill") +
  coord_flip() +
  geom_text(
    aes(label = label),
    position = position_fill(vjust = 0.5),
    size = 2.4
  ) +
  labs(
    title = "Vaccination Reporting Pattern by State",
    subtitle = paste("n =", n_distinct(df$state), "states"),
    x = "State",
    y = "Proportion",
    fill = "Reporting Pattern"
  ) +
  theme_minimal()

# 13. Missing Value Handling for EDA
mice_data <- df %>%
  select(type, enroll, mmr, overall, statespending2016)

mice_data$type <- as.factor(mice_data$type)

meth <- make.method(mice_data)
meth["type"] <- ""
meth["enroll"] <- "cart"
meth["mmr"] <- "cart"
meth["overall"] <- "cart"
meth["statespending2016"] <- ""

pred <- matrix(0, nrow = ncol(mice_data), ncol = ncol(mice_data))
colnames(pred) <- names(mice_data)
rownames(pred) <- names(mice_data)

pred["enroll", "statespending2016"] <- 1
pred["mmr", c("enroll", "statespending2016")] <- 1
pred["overall", c("enroll", "statespending2016")] <- 1

mice_fit <- mice(
  mice_data,
  m = 5,
  method = meth,
  predictorMatrix = pred,
  seed = 123,
  printFlag = TRUE
)

df_mice <- complete(mice_fit, 1)


df_imp <- df %>%
  mutate(
    enroll_imp = df_mice$enroll,
    mmr_imp = df_mice$mmr,
    overall_imp = df_mice$overall
  )

summary(df_mice)
colSums(is.na(df_mice))


tibble(
  variable = c("enroll", "mmr", "overall"),
  original_missing = c(sum(is.na(df$enroll)), sum(is.na(df$mmr)), sum(is.na(df$overall))),
  imputed_missing = c(sum(is.na(df_imp$enroll_imp)), sum(is.na(df_imp$mmr_imp)), sum(is.na(df_imp$overall_imp)))
) %>%
  print(caption = "Missingness Before and After Imputation") 

# 14. Create Subsets for EDA
df_mmr_obs <- df %>% filter(!is.na(mmr))
df_overall_obs <- df %>% filter(!is.na(overall))
df_mmr_type_obs <- df_mmr_obs
df_overall_type_obs <- df_overall_obs

df_mmr_imp <- df_imp
df_overall_imp <- df_imp
df_mmr_type_imp <- df_imp
df_overall_type_imp <- df_imp

# 15. Exploratory Data Analysis: Imputed Data
# 15.1 Distribution of Imputed MMR
ggplot(df_mmr_imp, aes(x = mmr_imp)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Imputed MMR Vaccination Rates",
    subtitle = paste("n =", nrow(df_mmr_imp)),
    x = "MMR Vaccination Rate",
    y = "Number of Schools"
  ) +
  theme_minimal()

# 15.2 Distribution of Imputed Overall Vaccination Rate
ggplot(df_overall_imp, aes(x = overall_imp)) +
  geom_histogram(bins = 30, fill = "orange", color = "white") +
  labs(
    title = "Distribution of Imputed Overall Vaccination Rates",
    subtitle = paste("n =", nrow(df_overall_imp)),
    x = "Overall Vaccination Rate",
    y = "Number of Schools"
  ) +
  theme_minimal()

# 15.3 Imputed MMR by School Type
type_count_mmr_imp <- df_mmr_type_imp %>%
  count(type) %>%
  mutate(
    pct = round(n / sum(n) * 100, 1),
    type_lab = paste0(type, "\n", n, " (", pct, "%)")
  )

ggplot(
  df_mmr_type_imp %>% left_join(type_count_mmr_imp, by = "type"),
  aes(x = type_lab, y = mmr_imp)
) +
  geom_boxplot() +
  labs(
    title = "Imputed MMR Vaccination Rate by School Type",
    subtitle = paste("n =", nrow(df_mmr_type_imp)),
    x = "School Type",
    y = "MMR Vaccination Rate"
  ) +
  theme_minimal()

# 15.4 Imputed Overall by School Type
type_count_overall_imp <- df_overall_type_imp %>%
  count(type) %>%
  mutate(
    pct = round(n / sum(n) * 100, 1),
    type_lab = paste0(type, "\n", n, " (", pct, "%)")
  )

ggplot(
  df_overall_type_imp %>% left_join(type_count_overall_imp, by = "type"),
  aes(x = type_lab, y = overall_imp)
) +
  geom_boxplot() +
  labs(
    title = "Imputed Overall Vaccination Rate by School Type",
    subtitle = paste("n =", nrow(df_overall_type_imp)),
    x = "School Type",
    y = "Overall Vaccination Rate"
  ) +
  theme_minimal()

# 15.5 Average Imputed MMR by State
state_summary_mmr_imp <- df_mmr_imp %>%
  group_by(state) %>%
  summarise(
    schools = n(),
    mean_mmr = mean(mmr_imp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_of_total = round(schools / sum(schools) * 100, 1)
  ) %>%
  arrange(desc(mean_mmr))

ggplot(state_summary_mmr_imp,
       aes(x = reorder(state, mean_mmr), y = mean_mmr)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  geom_text(
    aes(label = paste0(schools, " (", pct_of_total, "%)")),
    hjust = -0.1,
    size = 2.8
  ) +
  labs(
    title = "Average Imputed MMR Vaccination Rate by State",
    subtitle = paste("n =", sum(state_summary_mmr_imp$schools), "schools"),
    x = "State",
    y = "Average MMR Vaccination Rate"
  ) +
  theme_minimal() +
  expand_limits(y = max(state_summary_mmr_imp$mean_mmr) * 1.08)

# 15.6 Average Imputed Overall by State
state_summary_overall_imp <- df_overall_imp %>%
  group_by(state) %>%
  summarise(
    schools = n(),
    mean_overall = mean(overall_imp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_of_total = round(schools / sum(schools) * 100, 1)
  ) %>%
  arrange(desc(mean_overall))

ggplot(state_summary_overall_imp,
       aes(x = reorder(state, mean_overall), y = mean_overall)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  geom_text(
    aes(label = paste0(schools, " (", pct_of_total, "%)")),
    hjust = -0.1,
    size = 2.8
  ) +
  labs(
    title = "Average Imputed Overall Vaccination Rate by State",
    subtitle = paste("n =", sum(state_summary_overall_imp$schools), "schools"),
    x = "State",
    y = "Average Overall Vaccination Rate"
  ) +
  theme_minimal() +
  expand_limits(y = max(state_summary_overall_imp$mean_overall) * 1.08)

# 16. State-Level EDA for Spending
# 16.1 Distribution of State Education Spending
state_level <- df %>%
  distinct(state, statespending2016)

ggplot(state_level, aes(x = statespending2016)) +
  geom_histogram(bins = 15, fill = "darkgreen", color = "white") +
  scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Distribution of State Education Spending",
    subtitle = paste("n =", nrow(state_level), "states"),
    x = "State Education Spending (2016)",
    y = "Number of States"
  ) +
  theme_minimal()

# 16.2 Observed Average MMR vs Spending
state_spending_mmr_obs <- df_mmr_obs %>%
  group_by(state, statespending2016) %>%
  summarise(
    schools = n(),
    mean_mmr = mean(mmr, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(label = paste0(schools, " schools"))

ggplot(state_spending_mmr_obs,
       aes(x = statespending2016, y = mean_mmr)) +
  geom_point(size = 3, color = "black") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_text_repel(
    aes(label = label),
    size = 3,
    max.overlaps = 20
  ) +
  scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Association Between State Education Spending and Average MMR Vaccination Rate",
    subtitle = paste("n =", nrow(state_spending_mmr_obs), "states"),
    x = "State Education Spending (2016)",
    y = "Average MMR Vaccination Rate"
  ) +
  theme_minimal()

# 16.3 Observed Average Overall vs Spending
state_spending_overall_obs <- df_overall_obs %>%
  group_by(state, statespending2016) %>%
  summarise(
    schools = n(),
    mean_overall = mean(overall, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(label = paste0(schools, " schools"))

ggplot(state_spending_overall_obs,
       aes(x = statespending2016, y = mean_overall)) +
  geom_point(size = 3, color = "black") +
  geom_text_repel(
    aes(label = label),
    size = 3,
    max.overlaps = 20
  ) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Association Between State Education Spending and Average Overall Vaccination Rate",
    subtitle = paste("n =", nrow(state_spending_overall_obs), "states"),
    x = "State Education Spending (2016)",
    y = "Average Overall Vaccination Rate"
  ) +
  theme_minimal()

# 16.4 Imputed Average MMR vs Spending
state_spending_mmr_imp <- df_mmr_imp %>%
  group_by(state, statespending2016) %>%
  summarise(
    schools = n(),
    mean_mmr = mean(mmr_imp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(label = paste0(schools, " schools"))

ggplot(state_spending_mmr_imp,
       aes(x = statespending2016, y = mean_mmr)) +
  geom_point(size = 3, color = "black") +
  geom_text_repel(
    aes(label = label),
    size = 3,
    max.overlaps = 20
  ) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Association Between State Education Spending and Average Imputed MMR Vaccination Rate",
    subtitle = paste("n =", nrow(state_spending_mmr_imp), "states"),
    x = "State Education Spending (2016)",
    y = "Average MMR Vaccination Rate"
  ) +
  theme_minimal()

# 16.5 Imputed Average Overall vs Spending
state_spending_overall_imp <- df_overall_imp %>%
  group_by(state, statespending2016) %>%
  summarise(
    schools = n(),
    mean_overall = mean(overall_imp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(label = paste0(schools, " schools"))

ggplot(state_spending_overall_imp,
       aes(x = statespending2016, y = mean_overall)) +
  geom_point(size = 3, color = "black") +
  geom_text_repel(
    aes(label = label),
    size = 3,
    max.overlaps = 20
  ) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Association Between State Education Spending and Average Imputed Overall Vaccination Rate",
    subtitle = paste("n =", nrow(state_spending_overall_imp), "states"),
    x = "State Education Spending (2016)",
    y = "Average Overall Vaccination Rate"
  ) +
  theme_minimal()

# 17. Compare Observed vs Imputed Distributions
# 17.1 MMR Distribution
ggplot() +
  geom_density(data = df, aes(x = mmr), fill = "red", alpha = 0.35) +
  geom_density(data = df_imp, aes(x = mmr_imp), fill = "blue", alpha = 0.35) +
  labs(
    title = "Observed vs Imputed MMR Distribution",
    subtitle = paste("Observed n =", sum(!is.na(df$mmr)), "| Imputed n =", sum(!is.na(df_imp$mmr_imp))),
    x = "MMR Vaccination Rate",
    y = "Density"
  ) +
  theme_minimal()

# 17.2 Overall Distribution
ggplot() +
  geom_density(data = df, aes(x = overall), fill = "red", alpha = 0.35) +
  geom_density(data = df_imp, aes(x = overall_imp), fill = "blue", alpha = 0.35) +
  labs(
    title = "Observed vs Imputed Overall Distribution",
    subtitle = paste("Observed n =", sum(!is.na(df$overall)), "| Imputed n =", sum(!is.na(df_imp$overall_imp))),
    x = "Overall Vaccination Rate",
    y = "Density"
  ) +
  theme_minimal()

# 17.3 Enrollment Distribution
ggplot() +
  geom_density(data = df, aes(x = enroll), fill = "red", alpha = 0.35) +
  geom_density(data = df_imp, aes(x = enroll_imp), fill = "blue", alpha = 0.35) +
  labs(
    title = "Observed vs Imputed Enrollment Distribution",
    subtitle = paste("Observed n =", sum(!is.na(df$enroll)), "| Imputed n =", sum(!is.na(df_imp$enroll_imp))),
    x = "Enrollment",
    y = "Density"
  ) +
  theme_minimal()

# 18. Export Final Clean Datasets
#deduplicated dataset without impute
write_csv(df, here("data/analytical", "vaccine_clean_final.csv"))
#final imputed dataset (used for analysis)
write_csv(df_imp, here("data/analytical", "vaccine_clean_imputed.csv"))


tibble(
  dataset = c( "Final imputed dataset"),
  rows = nrow(df_imp),
  columns = ncol(df_imp)
) %>%
  print(caption = "Dimensions of Final Exported Datasets") 

