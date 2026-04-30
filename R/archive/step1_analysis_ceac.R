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

# Simple BMI median fill (optional; keeps rows)
if (any(is.na(dat$bmi))) {
  med_bmi <- median(dat$bmi, na.rm = TRUE)
  dat$bmi[is.na(dat$bmi)] <- med_bmi
}

## =========================
## 2) Adjusted models (ΔQALY & Cost)
## =========================
mod_dqaly <- lm(dqaly ~ env + age + sex + bmi + smoke + cci + los_days, data = dat)
mod_cost  <- lm(cost  ~ env + age + sex + bmi + smoke + cci + los_days, data = dat)

# Marginal means by environment and adjusted increments (HOPD - ASC)
emm_dq <- emmeans(mod_dqaly, ~ env)
emm_co <- emmeans(mod_cost,  ~ env)

inc_dqaly <- contrast(emm_dq, method = "revpairwise")  # HOPD - ASC
inc_cost  <- contrast(emm_co, method = "revpairwise")

adj_dqaly_est <- as.data.frame(inc_dqaly)$estimate[1]
adj_cost_est  <- as.data.frame(inc_cost)$estimate[1]
adj_dqaly_ci  <- confint(inc_dqaly)[1, c("lower.CL","upper.CL")]
adj_cost_ci   <- confint(inc_cost)[1,  c("lower.CL","upper.CL")]

ICER_est <- if (is.finite(adj_dqaly_est) && adj_dqaly_est > 0) adj_cost_est / adj_dqaly_est else NA_real_

## =========================
## 3) Net Monetary Benefit (primary)
## =========================
nmb_fit <- function(lambda) {
  d <- dat %>% mutate(nmb = lambda * dqaly - cost)
  lm(nmb ~ env + age + sex + bmi + smoke + cci + los_days, data = d)
}

lambda <- 100000  # main WTP; change as needed
mod_nmb <- nmb_fit(lambda)
inc_nmb_est <- coef(summary(mod_nmb))["envHOPD", "Estimate"]
inc_nmb_se  <- coef(summary(mod_nmb))["envHOPD", "Std. Error"]
inc_nmb_ci  <- inc_nmb_est + c(-1, 1) * 1.96 * inc_nmb_se

## =========================
## 4) Bootstrap for CIs & CEAC (with named columns)
## =========================
set.seed(1)
B <- 2000
lambda_grid <- seq(0, 200000, by = 5000)
boot_colnames <- c("edq","eco","icer", paste0("nmb_", lambda_grid))

boot_fn <- function(data, indices) {
  d <- data[indices, , drop = FALSE]
  mdq <- lm(dqaly ~ env + age + sex + bmi + smoke + cci + los_days, data = d)
  mco <- lm(cost  ~ env + age + sex + bmi + smoke + cci + los_days, data = d)
  
  edq <- as.data.frame(contrast(emmeans(mdq, ~ env), method = "revpairwise"))$estimate[1]
  eco <- as.data.frame(contrast(emmeans(mco, ~ env), method = "revpairwise"))$estimate[1]
  icer_b <- if (is.finite(edq) && edq > 0) eco / edq else NA_real_
  
  inc_nmb_vec <- sapply(lambda_grid, function(lam) {
    mn <- lm(I(lam * dqaly - cost) ~ env + age + sex + bmi + smoke + cci + los_days, data = d)
    unname(coef(mn)["envHOPD"])
  })
  
  out <- c(edq, eco, icer_b, inc_nmb_vec)
  names(out) <- boot_colnames  # << ensure names
  out
}

boot_out <- boot(dat, statistic = boot_fn, R = B)

# Ensure column names are present
if (is.null(colnames(boot_out$t))) colnames(boot_out$t) <- boot_colnames
boot_df <- as.data.frame(boot_out$t)

# Extract boot distributions
edq_boot  <- boot_df$edq
eco_boot  <- boot_df$eco
icer_boot <- boot_df$icer

dq_ci_boot   <- quantile(edq_boot, c(0.025, 0.975), na.rm = TRUE)
cost_ci_boot <- quantile(eco_boot, c(0.025, 0.975), na.rm = TRUE)
icer_ci_boot <- if (any(is.finite(icer_boot))) quantile(icer_boot[is.finite(icer_boot)], c(0.025, 0.975)) else c(NA, NA)

# CEAC: probability(HOPD cost-effective vs ASC) across λ
nmb_cols <- grep("^nmb_", names(boot_df), value = TRUE)
prob_ce  <- colMeans(boot_df[, nmb_cols] > 0, na.rm = TRUE)
ceac     <- data.frame(lambda = as.numeric(sub("^nmb_", "", nmb_cols)), prob_ce = prob_ce)

## =========================
## 5) Results object
## =========================
results <- list(
  N = nrow(dat),
  setting_counts = table(dat$env),
  adjusted_delta_qaly = list(estimate = adj_dqaly_est, CI = setNames(as.numeric(adj_dqaly_ci), c("lwr","upr"))),
  adjusted_delta_cost = list(estimate = adj_cost_est,  CI = setNames(as.numeric(adj_cost_ci),  c("lwr","upr"))),
  ICER = ICER_est,
  boot_CIs = list(
    delta_qaly_95CI = dq_ci_boot,
    delta_cost_95CI = cost_ci_boot,
    ICER_95CI = icer_ci_boot
  ),
  inc_NMB_at_lambda = list(lambda = lambda, estimate = inc_nmb_est, lwr = inc_nmb_ci[1], upr = inc_nmb_ci[2]),
  CEAC = ceac
)

# Optional quick prints
print(results$setting_counts)
print(results$adjusted_delta_qaly)
print(results$adjusted_delta_cost)
cat("ICER (HOPD vs ASC):", results$ICER, "\n")
head(results$CEAC)

save(results, file = 'results.rds')
