# Load necessary libraries
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(janitor)
library(emmeans)
library(boot)

# Load processed QALY and Cost file
QC <- read_excel("../data/QALYs and Costs 3.26.26.xlsx", sheet = "Costs and QALYs")

# Load Smitha SNOT22s sheet for MRN matching
smitha <- read_excel("../data/QALYs and Costs 3.26.26.xlsx", sheet = "Smitha SNOT22s")

# Create name-based MRN lookup
smitha_lookup <- smitha %>%
  mutate(
    first_name_match = str_to_upper(str_squish(as.character(PAT_FIRST_NAME))),
    last_name_match  = str_to_upper(str_squish(as.character(PAT_LAST_NAME))),
    MRN = as.character(MRN)
  ) %>%
  distinct(first_name_match, last_name_match, MRN)

# Optional: check whether any first-name/last-name combination maps to multiple MRNs
duplicate_name_check <- smitha_lookup %>%
  count(first_name_match, last_name_match, name = "n_mrn") %>%
  filter(n_mrn > 1)

if (nrow(duplicate_name_check) > 0) {
  warning("Some first-name/last-name combinations match multiple MRNs. Please review duplicate_name_check.")
}

# Join MRN into QC by first name and last name
QC <- QC %>%
  mutate(
    first_name_match = str_to_upper(str_squish(as.character(`Patient First name`))),
    last_name_match  = str_to_upper(str_squish(as.character(`Patient Last Name`)))
  ) %>%
  left_join(
    smitha_lookup,
    by = c("first_name_match", "last_name_match")
  ) %>%
  select(
    `ASC OR HOPD`,
    MRN,
    `Age at time of surgery`,
    `Total Costs`,
    `Preop QALY (=Preop HUV * 0.5 years)`,
    `Postop QALY (=Postop HUV * 0.5 years)`
  )

# Rename columns
names(QC) <- c(
  "ASC or HOPD",
  "mrn",
  "Age at time of surgery",
  "Total Costs",
  "Preop QALY",
  "Postop QALY"
)

# load covariates file
load("DRcovariate.rds")
DR4048 <- df_select

## ---- 1) Link tables ----
# Clean column names so joins are easy
QC1 <- QC %>%
  janitor::clean_names() %>%
  mutate(
    mrn = str_trim(as.character(mrn))
  )

DRcov <- DR4048 %>%
  janitor::clean_names() %>%
  mutate(
    mrn = str_trim(as.character(mrn))
  ) %>%
  select(mrn, sex, bmi, smoke5, cciscore, hosp_gap_days)

# Join QC with DRcov by MRN
dat <- QC1 %>%
  left_join(DRcov, by = "mrn")

## =========================
## 1b) Prep variables
## =========================
dat <- dat %>%
  mutate(
    # Environment parsing; set ASC as reference
    env_raw = tolower(`asc_or_hopd`),
    env = case_when(
      str_detect(env_raw, "hopd|hospital") ~ "HOPD",
      str_detect(env_raw, "asc")           ~ "ASC",
      TRUE ~ NA_character_
    ),
    env = factor(env, levels = c("ASC","HOPD")),
    
    # Outcomes (if these are HUVs, replace with 0.5*preop_huv etc.)
    pre_qaly  = suppressWarnings(as.numeric(preop_qaly)),
    post_qaly = suppressWarnings(as.numeric(postop_qaly)),
    dqaly     = post_qaly - pre_qaly,
    cost      = suppressWarnings(as.numeric(total_costs)),
    
    # Covariates
    age      = suppressWarnings(as.numeric(age_at_time_of_surgery)),
    bmi      = suppressWarnings(as.numeric(bmi)),
    cci      = suppressWarnings(as.numeric(cciscore)),
    sex      = factor(sex),
    smoke    = factor(smoke5),
    los_days = suppressWarnings(as.numeric(hosp_gap_days))
  ) %>%
  # Keep rows we can analyze
  filter(!is.na(env), !is.na(dqaly), !is.na(cost))

dat_costpos <- dat %>% filter(cost > 0) %>% mutate(q_per_cost = dqaly / cost)

# Welch t-test
wilcox.test(q_per_cost ~ env, data = dat_costpos)

# OLS
lm_qpc <- lm(q_per_cost ~ env + age + sex + bmi + smoke + cci + los_days, data = dat_costpos)
summary(lm_qpc)
confint(lm_qpc)["envHOPD", ]

