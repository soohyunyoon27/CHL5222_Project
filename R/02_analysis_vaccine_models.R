# CHL5222 Final Project
# Feature Engineering + Primary Analysis + Secondary Analysis + Sensitivity 
# Analysis + Missing Data Analysis (Representativeness Check) + Model Validation
library(tidyverse)
library(here)
library(lme4)
library(ggplot2)
library(lmerTest)
library(performance)


# =========== 1 Data Import ==============

vaccine <- read_csv(here("data", "vaccine_clean_final.csv"))
glimpse(vaccine)
summary(vaccine)

# =========== 2 Feature Engineering ==============

# Calculate spending_per_student (spending / school aged pop.)
vaccine$spending_per_student <- vaccine$statespending2016 / vaccine$schagepop2016

# distribution of Spending per Student
ggplot(vaccine,aes(x=spending_per_student))+
  geom_histogram(bins=30,fill="steelblue",color="white")+
  labs(
    title="Distribution of Education Spending per Student",
    x="Spending",
    y="Number of Schools"
  )+
  theme_minimal()

# Standardize the spending_per_student to have a mean of 0 and SD of 1
vaccine$spending_scaled <- as.numeric(scale(vaccine$spending_per_student))

# Check how many distinct states are in each type
vaccine %>% group_by(type) %>% 
  summarise(n_state = n_distinct(state),n_school = n())

# Combine rare school types (BOCES, Charter, Kindergarten, Nonpublic) into "Other" 
# to prevent confounding with state random intercepts
rare_type <- c("BOCES", "Charter", "Kindergarten", "Nonpublic")
vaccine <- vaccine %>%
  mutate(combined_type = ifelse(type %in% rare_type, "Other", type))%>%
  # Set Public as the reference category
  mutate(combined_type = fct_relevel(combined_type, "Public"))


# =========== 3 Primary Analysis: MMR Vaccination Rate ==============

# Fit Random Intercepts Linear Mixed Model
# Primary outcome: School-level MMR vaccination rate
# Predictor: Standardized education spending per student
# Covariate: School types
# Random Effect: State

# Create a clean dataset for primary analysis (Listwise deletion)
vaccine_primary <- vaccine %>%
  drop_na(mmr, spending_scaled, combined_type, state)
cat("\n Number of observations included in the primary analysis:\n")
nrow(vaccine_primary)
cat("\n Number of states included in the primary analysis:\n")
cat(n_distinct(vaccine_primary$state), "-", unique(vaccine_primary$state))

# Fit the null model for calculate baseline of ICC 
# to justify that state-level clustering exists
model_mmr_null <- lmer(mmr ~ 1 + (1 | state), data = vaccine_primary)
cat("\n Primary Analysis - Null Model (Baseline Variance) Summary:\n")
summary(model_mmr_null)

# Fit the model with covariates
model_mmr <- lmer(mmr ~ spending_scaled + combined_type + (1 | state),
                  data = vaccine_primary)
cat("\n Primary Analysis - MMR Vaccination Rate Summary:\n")
summary(model_mmr)


# =========== 4 (OPTIONAL) Secondary Analysis: Overall Vaccination Rate ==============

# Fit the Random Intercepts Linear Mixed Model
# Secondary Outcome: school-level overall vaccination rate
# Predictor: Standardized education spending per student
# Covariate: School types
# Random Effect: State

# Create a clean dataset for secondary analysis (Listwise deletion)
vaccine_secondary <- vaccine %>%
  drop_na(overall, spending_scaled, combined_type, state)
cat("\n Number of observations included in the secondary analysis:\n")
nrow(vaccine_secondary)
cat("\n Number of states included in the secondary analysis:\n")
cat(n_distinct(vaccine_secondary$state), "-", unique(vaccine_secondary$state))

# Fit the null model for calculate baseline of ICC 
# to justify that state-level clustering exists
model_overall_null <- lmer(overall ~ 1 + (1 | state), data = vaccine_secondary)
cat("\n Secondary Analysis - Null Model (Baseline Variance) Summary:\n")
summary(model_overall_null)

# Fit the model with covariates
model_overall <- lmer(overall ~ spending_scaled + combined_type + (1 | state),
                      data = vaccine_secondary)
cat("\n Secondary Analysis - Overall Vaccination Rate Summary:\n")
summary(model_overall)


# =========== 5 Sensitivity Analysis: Stratified by School Type ==============

# Stratifying the models by school type to assess if the association 
# differs across these groups

# -------------- 5A. Stratified MMR Models --------------

# Public Schools Only (MMR)
model_mmr_public <- lmer(mmr ~ spending_scaled + (1 | state), 
                     data = filter(vaccine_primary, combined_type == "Public"))
cat("\n Public Schools Only (MMR) Summary:\n")
summary(model_mmr_public)

# Private Schools Only (MMR)
model_mmr_private <- lmer(mmr ~ spending_scaled + (1 | state), 
                      data = filter(vaccine_primary, combined_type == "Private"))
cat("\n Private Schools Only (MMR) Summary:\n")
summary(model_mmr_private)

# 'Other' Schools (MMR)
model_mmr_other <- lmer(mmr ~ spending_scaled + (1 | state), 
                           data = filter(vaccine_primary, combined_type == "Other"))
cat("\n MMR Vaccination Model (Other - BOCES, Charter, Kindergarten, Nonpublic) Summary:\n")
summary(model_mmr_other)

# -------------- 5B. Stratified Overall Models --------------

# Public Schools Only (Overall)
model_overall_public <- lmer(overall ~ spending_scaled + (1 | state), 
                             data = filter(vaccine_secondary, combined_type == "Public"))
cat("\n Public Schools Only (Overall) Summary:\n")
summary(model_overall_public)

# Private Schools Only (Overall)
model_overall_private <- lmer(overall ~ spending_scaled + (1 | state), 
                              data = filter(vaccine_secondary, combined_type == "Private"))
cat("\n Private Schools Only (Overall) Summary:\n")
summary(model_overall_private)


# =========== 6 Sensitivity Analysis: Stratified by Cohort ==============

# Stratifying the models by cohort (2017-2018 vs. 2018-2019) to assess if the 
# association differs across cohorts

# -------------- 6A. Stratified MMR Models --------------

# 2017-2018 Cohort Only (MMR)
model_mmr_1718 <- lmer(mmr ~ spending_scaled + combined_type + (1 | state),
                   data = filter(vaccine_primary, year == "2017" | year == "2017-18"))
cat("\n 2017-2018 Cohort Only (MMR) Summary:\n")
summary(model_mmr_1718)

# 2018-2019 Cohort Only (MMR)
model_mmr_1819 <- lmer(mmr ~ spending_scaled + combined_type + (1 | state),
                   data = filter(vaccine_primary, year == "2018-19"))
cat("\n 2018-2019 Cohort Only (MMR) Summary:\n")
summary(model_mmr_1819)

# -------------- 6B. Stratified Overall Models --------------

# 2017-2018 Cohort Only (Overall)
vaccine_secondary %>%
  group_by(state, combined_type) %>%
  filter(year == "2017" | year == "2017-18") %>%
  summarise(n = n(),
            n_state = n_distinct(state))
# Note: Since only one state reported the overall vaccination rate for the 
# 2017-2018 cohort, it is impossible to run a mixed-effects model (lmer) with 
# state as a random effect.
#model_overall_1718 <- lmer(overall ~ spending_scaled + combined_type + (1 | state),
#data = filter(vaccine_secondary, year == "2017" | year == "2017-18"))
#cat("\n 2017-2018 Cohort Only (Overall) Summary:\n")
#summary(model_overall_1718)


# 2018-2019 Cohort Only (Overall)
model_overall_1819 <- lmer(overall ~ spending_scaled + combined_type + (1 | state),
                       data = filter(vaccine_secondary, year == "2018-19"))
cat("\n 2018-2019 Cohort Only (Overall) Summary:\n")
summary(model_overall_1819)


# =========== 7 Missing data analysis: Representativeness Check ==============

# Check the potential bias from missing data

# Create indicator for missing outcome
vaccine <- vaccine %>%
  mutate(
    missing_mmr = is.na(mmr),
    missing_overall = is.na(overall))

# -------------- 7A. Missing MMR --------------

# Compare spending_scaled between complete and missing MMR groups
vaccine %>%
  group_by(missing_mmr) %>%
  summarise(
    n = n(),
    mean_spending = mean(spending_scaled, na.rm = TRUE),
    sd_spending = sd(spending_scaled, na.rm = TRUE),
    n_state = n_distinct(state),
    n_type = n_distinct(combined_type))

# t-test for spending difference
t.test(spending_scaled ~ missing_mmr, data = vaccine)

# School type distribution by missing MMR
table(vaccine$combined_type, vaccine$missing_mmr, useNA = "ifany")
chisq.test(table(vaccine$combined_type, vaccine$missing_mmr))

# Mixed-effects logistic regression to predict missing MMR
missing_mmr_glmer <- glmer(missing_mmr ~ spending_scaled + combined_type + 
                             (1|state),
                       data = vaccine, family = binomial)
summary(missing_mmr_glmer)

# -------------- 7B. Missing Overall --------------

# Compare spending_scaled between complete and missing overall groups
vaccine %>%
  group_by(missing_overall) %>%
  summarise(
    n = n(),
    mean_spending = mean(spending_scaled, na.rm = TRUE),
    sd_spending = sd(spending_scaled, na.rm = TRUE),
    n_state = n_distinct(state),
    n_type = n_distinct(combined_type))

# t-test for spending difference
t.test(spending_scaled ~ missing_overall, data = vaccine)

# Combined School type distribution by missing Overall
table(vaccine$combined_type, vaccine$missing_overall, useNA = "ifany")
chisq.test(table(vaccine$combined_type, vaccine$missing_overall))

# Mixed-effects logistic regression to predict missing Overall
missing_overall_glmer <- glmer(missing_overall ~ spending_scaled + combined_type 
                               + (1|state),
                       data = vaccine, family = binomial)
summary(missing_overall_glmer)

# =========== 8 Model Validation ==============

# calculate Intraclass Correlation Coefficient (ICC)
cat("\n Baseline ICC for MMR Model:\n")
icc(model_mmr_null)
cat("\n ICC for MMR Model:\n")
icc(model_mmr)
cat("\n Baseline ICC for Overall Model:\n")
icc(model_overall_null)
cat("\n ICC for Overall Model:\n")
icc(model_overall)

# check residual normality and homoscedasticity
plot(model_mmr, which = 2) # plot fitted values vs. residuals for homoscedasticity
qqnorm(resid(model_mmr), main = "Normal Q-Q Plot") # Q-Q plot for normality of residuals
qqline(resid(model_mmr), col = "red")

# check residual normality and homoscedasticity
plot(model_overall, which = 2) # plot fitted values vs. residuals for homoscedasticity
qqnorm(resid(model_overall), main = "Normal Q-Q Plot") # Q-Q plot for normality of residuals
qqline(resid(model_overall), col = "red")

# check for multicollinearity
check_collinearity(model_mmr)
check_collinearity(model_overall)

