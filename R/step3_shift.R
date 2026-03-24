# Load necessary libraries
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(janitor)
library(emmeans)
library(boot)

# load processed QALY and Cost file
QC <- read_excel("../data/QALYs and Costs 3.17.25.xlsx", sheet = "Cost and QALYs")
QC <- QC[,c("ASC or HOPD", "Patient Number (coded)", "Age at time of surgery",
            "Total Costs", "Preop QALY", "Postop QALY")]

# load patient coding
SNOT2QALY <- read_excel("../data/QALYs and Costs 3.17.25.xlsx", sheet = "SNOT22s to QALYs")
MRN2PC <- SNOT2QALY[,c('Patient Number (coded)', 'mrn')]
names(MRN2PC) <- c('PC', 'MRN')
MRN2PC <- unique(MRN2PC)

# load covariates file
load("DRcovariate.rds")
DR2380 <- df_select

## ---- 1) Link tables ----
# Clean column names so joins are easy
QC1     <- QC     %>% janitor::clean_names()
MRN2PC1 <- MRN2PC %>% janitor::clean_names()    # expects: pc, mrn
DRcov   <- DR2380 %>%
  janitor::clean_names() %>%
  select(mrn, sex, bmi, smoke5, cciscore, hosp_gap_days)

# Join QC (has QALY & cost) -> MRN2PC (pc->mrn) -> DRcov (covariates)
dat <- QC1 %>%
  left_join(MRN2PC1, by = c("patient_number_coded" = "pc")) %>%
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


library(dplyr)
library(gtsummary)

# ensure variable types
dat_costpos2 <- dat_costpos %>%
  mutate(
    env      = factor(env),
    sex      = factor(sex),
    smoke    = factor(smoke),
    los_days = factor(los_days)   # categorical per your note
  )

two_level_env <- nlevels(dat_costpos2$env) == 2

table1 <- dat_costpos2 %>%
  select(env, age, sex, bmi, smoke, cci, los_days) %>%
  tbl_summary(
    by = env,
    type = list(
      age ~ "continuous", bmi ~ "continuous", cci ~ "continuous",
      sex ~ "categorical", smoke ~ "categorical", los_days ~ "categorical"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",      # or "{median} [{p25}, {p75}]"
      all_categorical() ~ "{n} ({p}%)"
    ),
    # digits: continuous -> 2; categorical n -> 0, % -> 2
    digits = list(
      all_continuous()  ~ 2,
      all_categorical() ~ c(0, 2)
    ),
    missing = "ifany"
  ) %>%
  add_overall() %>%
  add_p(
    test = if (two_level_env) {
      list(
        all_continuous()  ~ "t.test",
        all_categorical() ~ "chisq.test.no.correct"
      )
    } else {
      list(
        all_continuous()  ~ "anova",              # or "kruskal.test"
        all_categorical() ~ "chisq.test.no.correct"
      )
    },
    pvalue_fun = ~ style_pvalue(.x, digits = 2)    # p-values with 2 decimals
  ) %>%
  bold_labels()

table1