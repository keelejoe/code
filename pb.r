#-------------------------------------------------------------------------------
# TITLE: Youth and Drug Use (Weighted Analysis)
# DATE: 2025-10-19
# AUTHOR: [Your Name Here]
#-------------------------------------------------------------------------------

# --- 1. SETUP: LOAD PACKAGES ---
# This section loads all required packages for the analysis.

# Tidyverse for data manipulation (dplyr, etc.)
library(tidyverse) 

# This is the main package for survey analysis
library(survey)         

# For creating weighted summary tables (gtsummary)
library(gtsummary)      

# For the freq() function to check unweighted Ns
library(summarytools)   


# --- 2. LOAD DATA ---
# Load the NSDUH .Rdata file and assign it to a working data frame.

# Make sure the .Rdata file loads an object named 'data'
load("C:/Users/fr8412/OneDrive - Wayne State University/Teaching/Family Medicine/FPH 7440 Practicum/2025, Keele Thomas/nsduh-2023-ds0001-bndl-data-r_v2/NSDUH_2023.Rdata")

# Create a copy of the dataset
d <- data


# --- 3. SUBSET MAIN DATA ---
# Create a new data frame `s` containing only the variables of interest.

s <- d %>%
  select(
    # Core IDs & Weights
    QUESTID2, VEREP, VESTR_C, ANALWT2_C,
    
    # Demographics / Confounders
    AGE3, CATAG2, IRSEX, NEWRACE2, EDUHIGHCAT, POVERTY3,
    
    # Drug Variables (Exposures)
    CIGFLAG, CGRFLAG, PIPFLAG, SMKLSSFLAG, NICVAPFLAG, ALCFLAG, CBDHMPFLAG, 
    MRJFLAG, COCFLAG, CRKFLAG, HERFLAG, HALLUCFLAG, INHALFLAG, METHAMFLAG, 
    PNRANYFLAG, TRQANYFLAG, STMANYFLAG, SEDANYFLAG,
    
    # Outcome Variable
    YMDELT, YMDEAUD5YR, YMIUD5YANY, YMSUD5YANY, 
    
    # Other Covariates
    AVGGRADE, PARLMTSN
  )


# --- 4. DATA WRANGLING & FEATURE ENGINEERING ---
# Create composite exposure variables for "any drug", "soft drugs", and "hard drugs".

# Define the variable lists
drug_vars <- c("CIGFLAG", "CGRFLAG", "PIPFLAG", "SMKLSSFLAG", "NICVAPFLAG", 
               "ALCFLAG", "CBDHMPFLAG", "MRJFLAG", "COCFLAG", "CRKFLAG", 
               "HERFLAG", "HALLUCFLAG", "INHALFLAG", "METHAMFLAG", 
               "PNRANYFLAG", "TRQANYFLAG", "STMANYFLAG", "SEDANYFLAG")

soft_drug_vars <- c("CIGFLAG", "CGRFLAG", "PIPFLAG", "SMKLSSFLAG", 
                    "NICVAPFLAG", "ALCFLAG", "CBDHMPFLAG", "MRJFLAG")

hard_drug_vars <- c("COCFLAG", "CRKFLAG", "HERFLAG", "HALLUCFLAG", 
                    "INHALFLAG", "METHAMFLAG", "PNRANYFLAG", "TRQANYFLAG", 
                    "STMANYFLAG", "SEDANYFLAG")

# Create composite variables
s_clean <- s %>%
  mutate(
    # Main Exposure: 1 if ANY drug flag is 1, 0 if ALL are 0
    any_drug_ever_used = case_when(
      if_any(all_of(drug_vars), ~ . == 1) ~ 1,
      if_all(all_of(drug_vars), ~ . == 0) ~ 0,
      TRUE ~ NA_real_ # All other cases (e.g., all NA)
    ),
    
    # Secondary: Soft Drugs
    any_soft_drug_ever_used = case_when(
      if_any(all_of(soft_drug_vars), ~ . == 1) ~ 1,
      if_all(all_of(soft_drug_vars), ~ . == 0) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Secondary: Hard Drugs
    any_hard_drug_ever_used = case_when(
      if_any(all_of(hard_drug_vars), ~ . == 1) ~ 1,
      if_all(all_of(hard_drug_vars), ~ . == 0) ~ 0,
      TRUE ~ NA_real_
    )
  )

cat("--- Glimpse of new composite drug variables ---\n")
glimpse(s_clean %>% select(any_drug_ever_used, any_soft_drug_ever_used, any_hard_drug_ever_used))


# --- 5. CREATE ANALYTIC SAMPLE ---
# Filter for youth and apply list-wise deletion for all model variables.

# Define all confounders for list-wise deletion
confounder_vars <- c("AGE3", "IRSEX", "NEWRACE2", "EDUHIGHCAT", "POVERTY3", "AVGGRADE", "PARLMTSN")

youth_analytic <- s_clean %>%
  # 1. Filter for youth population
  filter(AGE3 %in% c(1, 2, 3)) %>%
  
  # 2. Recode outcome and key variables for clarity and modeling
  mutate(
    # Recode YMDELT (Outcome): 1=Yes, 2=No --> 1=Yes, 0=No
    YMDELT_factor = factor(YMDELT, 
                           levels = c(1, 2), 
                           labels = c("Yes (MDE)", "No (MDE)")),
    
    # Recode key confounders to be factors
    AGE3 = factor(AGE3, 
                  levels = c(1, 2, 3), 
                  labels = c("12-13", "14-15", "16-17")),
    
    IRSEX = factor(IRSEX, 
                   levels = c(1, 2), 
                   labels = c("Male", "Female")),
    
    any_drug_ever_used_factor = factor(any_drug_ever_used, 
                                       levels = c(0, 1), 
                                       labels = c("Never Used", "Ever Used"))
  ) %>%
  
  # 3. List-wise deletion: remove rows with NA in outcome, main exposure, or confounders
  drop_na(YMDELT_factor, any_drug_ever_used_factor, all_of(confounder_vars))


# Check the new analytic sample (unweighted)
cat("\n--- Unweighted Age distribution in final sample ---\n")
freq(youth_analytic$AGE3)

cat("\n--- Unweighted Drug Use in final sample ---\n")
freq(youth_analytic$any_drug_ever_used_factor)

cat("\n--- Unweighted Depression Outcome in final sample ---\n")
freq(youth_analytic$YMDELT_factor)


# --- 6. APPLY SURVEY DESIGN ---
# Create the `svydesign` object. ALL analysis from this point forward
# must use this `svy_analytic` object.

svy_analytic <- svydesign(
  id      = ~VEREP,       # Primary Sampling Unit
  strata  = ~VESTR_C,     # Strata
  weights = ~ANALWT2_C,   # Survey weights
  data    = youth_analytic, # Use the final analytic dataset
  nest    = TRUE
)

cat("\n--- Survey Design Object Summary ---\n")
print(svy_analytic)


# --- 7. TABLE 1: WEIGHTED DESCRIPTIVE CHARACTERISTICS ---
# This creates a publication-ready table, stratified by the outcome,
# with all statistics correctly weighted.

# Define variable labels for the table
var_labels <- list(
  AGE3 ~ "Age Group",
  IRSEX ~ "Sex",
  NEWRACE2 ~ "Race/Ethnicity",
  EDUHIGHCAT ~ "Parent's Education",
  POVERTY3 ~ "Poverty Level",
  AVGGRADE ~ "Average Grades",
  PARLMTSN ~ "Parental Monitoring",
  any_drug_ever_used_factor ~ "Any Drug Use (Ever)"
)

# Generate the weighted, stratified Table 1
table1 <- svy_analytic %>%
  tbl_svysummary(
    # Stratify by our outcome
    by = YMDELT_factor,
    
    # Include all confounders and the main exposure
    include = c(AGE3, IRSEX, NEWRACE2, EDUHIGHCAT, POVERTY3, AVGGRADE, 
                PARLMTSN, any_drug_ever_used_factor),
    
    label = var_labels,
    
    # Show unweighted N and weighted %
    statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)")
  ) %>%
  add_overall(
    # Add an "Overall" column
    col_name = "**Overall (N = {N_unweighted})**",
    statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)")
  ) %>%
  add_p() %>% # Add weighted p-values (survey-weighted Chi-square test)
  modify_header(label = "**Characteristic**") %>%
  bold_labels()

# Explicitly print the table to the R viewer
cat("\n--- Weighted Table 1 ---\n")
print(table1)


# --- 8. BIVARIATE ANALYSIS (WEIGHTED CONFOUNDER ASSESSMENT) ---
# Programmatically run weighted chi-square tests for each potential confounder
# against the Outcome and the Exposure.

# Define our lists
outcome_var <- "YMDELT_factor"
exposure_var <- "any_drug_ever_used_factor"
confounders_to_test <- c("AGE3", "IRSEX", "NEWRACE2", "EDUHIGHCAT", 
                         "POVERTY3", "AVGGRADE", "PARLMTSN")

# --- 1. Test Confounder <-> Outcome ---
cat("\n\n--- Confounder vs. Outcome (Depression) Tests ---\n")
conf_outcome_tests <- lapply(confounders_to_test, function(conf) {
  formula <- as.formula(paste("~", conf, "+", outcome_var))
  test_result <- svychisq(formula, design = svy_analytic)
  return(test_result)
})
names(conf_outcome_tests) <- confounders_to_test
print(conf_outcome_tests)


# --- 2. Test Confounder <-> Exposure ---
cat("\n\n--- Confounder vs. Exposure (Drug Use) Tests ---\n")
conf_exposure_tests <- lapply(confounders_to_test, function(conf) {
  formula <- as.formula(paste("~", conf, "+", exposure_var))
  test_result <- svychisq(formula, design = svy_analytic)
  return(test_result)
})
names(conf_exposure_tests) <- confounders_to_test
print(conf_exposure_tests)


# --- 9. MODELING: WEIGHTED LOGISTIC REGRESSION ---
# Run the final survey-weighted logistic regression model (`svyglm`).
# We adjust for variables associated with both exposure and outcome.

# NOTE: Based on the tests, you must decide which variables to include.
# For this example, we assume AGE3, IRSEX, NEWRACE2, EDUHIGHCAT, and AVGGRADE
# were confounders and POVERTY3 and PARLMTSN were not.

final_model <- svyglm(
  # Formula: outcome ~ exposure + confounder1 + confounder2 ...
  YMDELT_factor ~ any_drug_ever_used_factor + AGE3 + IRSEX + NEWRACE2 + EDUHIGHCAT + AVGGRADE,
  
  design = svy_analytic,
  family = quasibinomial() # Use for logistic regression with binary outcomes
)

# Print the standard model summary
cat("\n\Date: 2025-10-19--- Final Model Summary (Coefficients) ---\n")
print(summary(final_model))

# Create a beautiful, publication-ready table of Odds Ratios
model_table <- tbl_regression(final_model, exponentiate = TRUE) %>%
  bold_p()

# Print the formatted table
cat("\n\n--- Final Model Table (Odds Ratios) ---\n")
print(model_table)

# --- END OF SCRIPT ---
