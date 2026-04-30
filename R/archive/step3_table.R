# ============================================================
# FULL UPDATED CODE:
# Cost-effectiveness client table with corrected patient-level joins
# and corrected NMB summary order
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(janitor)
  library(stringr)
  library(boot)
})

# ----------------------------
# User settings
# ----------------------------
QALY_COST_FILE  <- "../data/QALYs and Costs 3.17.25.xlsx"
QALY_COST_SHEET <- "Cost and QALYs"
SNOT_SHEET      <- "SNOT22s to QALYs"
COVARIATE_FILE  <- "DRcovariate.rds"

BOOT_R      <- 2000
SEED        <- 1
LAMBDA_100K <- 100000
LAMBDA_200K <- 200000

OUT_CSV <- "client_cost_effectiveness_table.csv"
OUT_RDS <- "client_cost_effectiveness_objects.rds"

# ----------------------------
# Helper functions
# ----------------------------
first_non_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  x[1]
}

load_saved_object <- function(path) {
  e <- new.env(parent = emptyenv())
  nm <- load(path, envir = e)
  e[[nm[1]]]
}

find_cols <- function(df, patterns) {
  nms <- names(df)
  out <- unique(unlist(lapply(patterns, function(p) {
    grep(p, nms, ignore.case = TRUE, value = TRUE)
  })))
  out
}

coalesce_numeric_from_cols <- function(df, cols) {
  if (length(cols) == 0) return(rep(NA_real_, nrow(df)))
  vals <- lapply(cols, function(cc) suppressWarnings(as.numeric(df[[cc]])))
  Reduce(dplyr::coalesce, vals)
}

safe_quantile <- function(x, probs = c(0.025, 0.975)) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(rep(NA_real_, length(probs)))
  as.numeric(quantile(x, probs = probs, na.rm = TRUE))
}

fmt_num <- function(x, digits = 2) {
  if (is.na(x)) return("")
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

fmt_money <- function(x, digits = 0) {
  if (is.na(x)) return("")
  if (x < 0) {
    paste0("-$", formatC(abs(x), format = "f", digits = digits, big.mark = ","))
  } else {
    paste0("$", formatC(x, format = "f", digits = digits, big.mark = ","))
  }
}

fmt_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("<0.001")
  sprintf("%.3f", p)
}

fmt_pct <- function(x, digits = 1) {
  if (is.na(x)) return("")
  paste0(formatC(100 * x, format = "f", digits = digits), "%")
}

fmt_ci_num <- function(lwr, upr, digits = 2) {
  if (is.na(lwr) || is.na(upr)) return("")
  paste0("(", fmt_num(lwr, digits), ", ", fmt_num(upr, digits), ")")
}

fmt_ci_money <- function(lwr, upr, digits = 0) {
  if (is.na(lwr) || is.na(upr)) return("")
  paste0("(", fmt_money(lwr, digits), ", ", fmt_money(upr, digits), ")")
}

mean_sd_str <- function(x, type = c("num", "money"), digits = 2) {
  type <- match.arg(type)
  x <- x[!is.na(x)]
  if (length(x) == 0) return("")
  m <- mean(x)
  s <- sd(x)
  if (type == "money") {
    paste0(fmt_money(m, digits), " (", fmt_money(s, digits), ")")
  } else {
    paste0(fmt_num(m, digits), " (", fmt_num(s, digits), ")")
  }
}

get_group_summary <- function(data, outcome, type = c("num", "money"), digits = 2) {
  type <- match.arg(type)
  x_all  <- data[[outcome]]
  x_hopd <- data[data$env == "HOPD", outcome, drop = TRUE]
  x_asc  <- data[data$env == "ASC",  outcome, drop = TRUE]
  
  list(
    all  = mean_sd_str(x_all,  type = type, digits = digits),
    hopd = mean_sd_str(x_hopd, type = type, digits = digits),
    asc  = mean_sd_str(x_asc,  type = type, digits = digits)
  )
}

make_model_formula <- function(data, outcome) {
  covars <- c("age", "sex", "bmi", "smoke", "cci", "los_days")
  keep <- c()
  
  for (v in covars) {
    if (!v %in% names(data)) next
    x <- data[[v]]
    x_nonmiss <- x[!is.na(x)]
    if (length(x_nonmiss) == 0) next
    
    if (is.numeric(x_nonmiss)) {
      if (sd(x_nonmiss) > 0) keep <- c(keep, v)
    } else {
      if (length(unique(x_nonmiss)) > 1) keep <- c(keep, v)
    }
  }
  
  rhs <- c("env", keep)
  as.formula(paste(outcome, "~", paste(rhs, collapse = " + ")))
}

fit_adjusted_env_effect <- function(data, outcome) {
  needed <- c("env", outcome, "age", "sex", "bmi", "smoke", "cci", "los_days")
  needed <- intersect(needed, names(data))
  
  d <- data %>%
    select(all_of(needed)) %>%
    filter(!is.na(env), !is.na(.data[[outcome]])) %>%
    droplevels()
  
  if (nrow(d) == 0) {
    return(list(
      n = 0, estimate = NA_real_, lwr = NA_real_, upr = NA_real_, p = NA_real_,
      model = NULL
    ))
  }
  
  d$env <- factor(d$env, levels = c("ASC", "HOPD"))
  
  if (length(unique(d$env[!is.na(d$env)])) < 2) {
    return(list(
      n = nrow(d), estimate = NA_real_, lwr = NA_real_, upr = NA_real_, p = NA_real_,
      model = NULL
    ))
  }
  
  fml <- make_model_formula(d, outcome)
  mod <- tryCatch(lm(fml, data = d), error = function(e) NULL)
  
  if (is.null(mod)) {
    return(list(
      n = nrow(d), estimate = NA_real_, lwr = NA_real_, upr = NA_real_, p = NA_real_,
      model = NULL
    ))
  }
  
  coef_tab <- summary(mod)$coefficients
  rn <- rownames(coef_tab)
  
  if (!"envHOPD" %in% rn) {
    return(list(
      n = nrow(d), estimate = NA_real_, lwr = NA_real_, upr = NA_real_, p = NA_real_,
      model = mod
    ))
  }
  
  ci <- tryCatch(confint(mod, "envHOPD"), error = function(e) c(NA_real_, NA_real_))
  
  list(
    n = nrow(d),
    estimate = unname(coef_tab["envHOPD", "Estimate"]),
    lwr = unname(ci[1]),
    upr = unname(ci[2]),
    p = unname(coef_tab["envHOPD", "Pr(>|t|)"]),
    model = mod
  )
}

make_row <- function(label, all = "", hopd = "", asc = "", inc = "", ci = "", p = "") {
  data.frame(
    Outcome = label,
    All = all,
    HOPD = hopd,
    ASC = asc,
    Increment = inc,
    CI95 = ci,
    p_value = p,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# ----------------------------
# Load source data
# ----------------------------
QC_raw <- read_excel(QALY_COST_FILE, sheet = QALY_COST_SHEET) %>%
  clean_names()

SNOT_raw <- read_excel(QALY_COST_FILE, sheet = SNOT_SHEET) %>%
  clean_names()

DRcov_raw <- load_saved_object(COVARIATE_FILE) %>%
  clean_names()

# ----------------------------
# Basic checks
# ----------------------------
if (!"patient_number_coded" %in% names(QC_raw)) {
  stop("QC sheet does not contain 'patient_number_coded' after clean_names().")
}
if (!"asc_or_hopd" %in% names(QC_raw)) {
  stop("QC sheet does not contain 'asc_or_hopd' after clean_names().")
}
if (!"patient_number_coded" %in% names(SNOT_raw) || !"mrn" %in% names(SNOT_raw)) {
  stop("SNOT sheet must contain both 'patient_number_coded' and 'mrn' after clean_names().")
}
if (!"mrn" %in% names(DRcov_raw)) {
  stop("DRcovariate.rds does not contain 'mrn' after clean_names().")
}

# ----------------------------
# Create unique patient code -> MRN map
# ----------------------------
MRN2PC_unique <- SNOT_raw %>%
  select(patient_number_coded, mrn) %>%
  filter(!is.na(patient_number_coded), !is.na(mrn)) %>%
  group_by(patient_number_coded) %>%
  summarise(mrn = first_non_na(mrn), .groups = "drop")

# ----------------------------
# Collapse covariates to one row per MRN
# ----------------------------
cci_col <- NULL
if ("cciscore" %in% names(DRcov_raw)) cci_col <- "cciscore"
if (is.null(cci_col) && "cci_score" %in% names(DRcov_raw)) cci_col <- "cci_score"

if (is.null(cci_col)) {
  stop("Cannot find CCI column in DRcovariate.rds ('cciscore' or 'cci_score').")
}

cov_keep <- c("mrn", "sex", "bmi", "smoke5", cci_col, "hosp_gap_days")
cov_keep <- intersect(cov_keep, names(DRcov_raw))

DRcov1 <- DRcov_raw %>%
  select(all_of(cov_keep))

if (cci_col != "cciscore") {
  DRcov1 <- DRcov1 %>% rename(cciscore = all_of(cci_col))
}

DRcov1 <- DRcov1 %>%
  group_by(mrn) %>%
  summarise(
    sex = first_non_na(sex),
    bmi = first_non_na(bmi),
    smoke5 = first_non_na(smoke5),
    cciscore = first_non_na(cciscore),
    hosp_gap_days = first_non_na(hosp_gap_days),
    .groups = "drop"
  )

# ----------------------------
# Collapse SNOT sheet to one row per patient
# ----------------------------
pre_snot_patterns <- c(
  "^pre(op)?_?snot.*22$",
  "^pre(op)?_?snot.*$",
  "^snot.*22.*pre",
  "^baseline.*snot.*22",
  "^pre.*sino.*nasal",
  "^pre.*snot"
)

post_snot_patterns <- c(
  "^post(op)?_?snot.*22$",
  "^post(op)?_?snot.*$",
  "^snot.*22.*post",
  "^follow.*snot.*22",
  "^post.*sino.*nasal",
  "^post.*snot"
)

pre_snot_cols <- setdiff(find_cols(SNOT_raw, pre_snot_patterns), c("preop_qaly", "postop_qaly"))
post_snot_cols <- setdiff(find_cols(SNOT_raw, post_snot_patterns), c("preop_qaly", "postop_qaly"))

message("Detected pre-op SNOT columns:  ", ifelse(length(pre_snot_cols) == 0, "NONE", paste(pre_snot_cols, collapse = ", ")))
message("Detected post-op SNOT columns: ", ifelse(length(post_snot_cols) == 0, "NONE", paste(post_snot_cols, collapse = ", ")))

SNOT_one_row <- SNOT_raw %>%
  mutate(
    pre_snot22_tmp  = coalesce_numeric_from_cols(., pre_snot_cols),
    post_snot22_tmp = coalesce_numeric_from_cols(., post_snot_cols)
  ) %>%
  group_by(patient_number_coded) %>%
  summarise(
    pre_snot22  = first_non_na(pre_snot22_tmp),
    post_snot22 = first_non_na(post_snot22_tmp),
    .groups = "drop"
  )

# ----------------------------
# Correct patient-level join
# ----------------------------
cohort0 <- QC_raw %>%
  left_join(MRN2PC_unique, by = "patient_number_coded") %>%
  left_join(DRcov1, by = "mrn") %>%
  left_join(SNOT_one_row, by = "patient_number_coded")

cat("\n==================== COHORT SIZE CHECK ====================\n")
cat("QC rows:              ", nrow(QC_raw), "\n")
cat("Joined cohort rows:   ", nrow(cohort0), "\n")
cat("Unique patient codes: ", n_distinct(cohort0$patient_number_coded), "\n")

# ----------------------------
# Build analysis variables
# ----------------------------
dat <- cohort0 %>%
  mutate(
    env_raw = tolower(as.character(asc_or_hopd)),
    env = case_when(
      str_detect(env_raw, "hopd|hospital") ~ "HOPD",
      str_detect(env_raw, "asc") ~ "ASC",
      TRUE ~ NA_character_
    ),
    env = factor(env, levels = c("ASC", "HOPD")),
    
    cost = suppressWarnings(as.numeric(total_costs)),
    pre_qaly = suppressWarnings(as.numeric(preop_qaly)),
    post_qaly = suppressWarnings(as.numeric(postop_qaly)),
    dqaly = post_qaly - pre_qaly,
    
    age = suppressWarnings(as.numeric(age_at_time_of_surgery)),
    bmi = suppressWarnings(as.numeric(bmi)),
    cci = suppressWarnings(as.numeric(cciscore)),
    los_days = suppressWarnings(as.numeric(hosp_gap_days)),
    
    sex = as.character(sex),
    smoke = as.character(smoke5),
    
    pre_snot22 = suppressWarnings(as.numeric(pre_snot22)),
    post_snot22 = suppressWarnings(as.numeric(post_snot22))
  )

# fill covariates to stabilize models
if (all(is.na(dat$bmi))) stop("BMI is all missing after join.")
dat$bmi[is.na(dat$bmi)] <- median(dat$bmi, na.rm = TRUE)
dat$cci[is.na(dat$cci)] <- 0
dat$los_days[is.na(dat$los_days)] <- 0
dat$sex[is.na(dat$sex) | dat$sex == ""] <- "Unknown"
dat$smoke[is.na(dat$smoke) | dat$smoke == ""] <- "Unknown"

dat$sex <- factor(dat$sex)
dat$smoke <- factor(dat$smoke)

# ----------------------------
# IMPORTANT FIX:
# Create NMB variables BEFORE creating cohort_env
# ----------------------------
dat <- dat %>%
  mutate(
    nmb_100k = LAMBDA_100K * dqaly - cost,
    nmb_200k = LAMBDA_200K * dqaly - cost
  )

# cohort used for header counts and raw summaries
cohort_env <- dat %>% filter(!is.na(env))

N_all  <- nrow(cohort_env)
N_hopd <- sum(cohort_env$env == "HOPD", na.rm = TRUE)
N_asc  <- sum(cohort_env$env == "ASC",  na.rm = TRUE)

cat("\n==================== FINAL HEADER COUNTS ====================\n")
cat("All patients: ", N_all, "\n")
cat("HOPD:         ", N_hopd, "\n")
cat("ASC:          ", N_asc, "\n")

# ----------------------------
# Raw summaries (All / HOPD / ASC)
# ----------------------------
sum_cost      <- get_group_summary(cohort_env, "cost",       type = "money", digits = 0)
sum_pre_snot  <- get_group_summary(cohort_env, "preop_snot22", type = "num",   digits = 2)
sum_post_snot <- get_group_summary(cohort_env, "postop_snot22",type = "num",   digits = 2)
sum_pre_qaly  <- get_group_summary(cohort_env, "pre_qaly",   type = "num",   digits = 3)
sum_post_qaly <- get_group_summary(cohort_env, "post_qaly",  type = "num",   digits = 3)
sum_dqaly     <- get_group_summary(cohort_env, "dqaly",      type = "num",   digits = 3)
sum_nmb100k   <- get_group_summary(cohort_env, "nmb_100k",   type = "money", digits = 0)

# ----------------------------
# Adjusted HOPD - ASC models
# ----------------------------
fit_cost      <- fit_adjusted_env_effect(dat, "cost")
fit_pre_snot  <- fit_adjusted_env_effect(dat, "preop_snot22")
fit_post_snot <- fit_adjusted_env_effect(dat, "postop_snot22")
fit_pre_qaly  <- fit_adjusted_env_effect(dat, "pre_qaly")
fit_post_qaly <- fit_adjusted_env_effect(dat, "post_qaly")
fit_dqaly     <- fit_adjusted_env_effect(dat, "dqaly")
fit_nmb100k   <- fit_adjusted_env_effect(dat, "nmb_100k")

# ----------------------------
# Common subset for ICER / CEAC
# ----------------------------
ce_dat <- dat %>%
  filter(
    !is.na(env),
    !is.na(cost),
    !is.na(dqaly)
  ) %>%
  droplevels()

fit_cost_ce  <- fit_adjusted_env_effect(ce_dat, "cost")
fit_dqaly_ce <- fit_adjusted_env_effect(ce_dat, "dqaly")

ICER_est <- if (!is.na(fit_dqaly_ce$estimate) && abs(fit_dqaly_ce$estimate) > 1e-12) {
  fit_cost_ce$estimate / fit_dqaly_ce$estimate
} else {
  NA_real_
}

QALY_per_cost_diff_est <- if (!is.na(fit_cost_ce$estimate) && abs(fit_cost_ce$estimate) > 1e-12) {
  fit_dqaly_ce$estimate / fit_cost_ce$estimate
} else {
  NA_real_
}

# ----------------------------
# Bootstrap for ICER / QALY-per-cost / CE probabilities
# ----------------------------
boot_names <- c("edq", "eco", "icer", "qpc", "nmb100k", "nmb200k")

boot_fn <- function(data, indices) {
  d <- data[indices, , drop = FALSE]
  out <- rep(NA_real_, length(boot_names))
  names(out) <- boot_names
  
  fit_dq <- fit_adjusted_env_effect(d, "dqaly")
  fit_co <- fit_adjusted_env_effect(d, "cost")
  
  edq <- fit_dq$estimate
  eco <- fit_co$estimate
  
  out["edq"] <- edq
  out["eco"] <- eco
  out["icer"] <- if (!is.na(edq) && abs(edq) > 1e-12) eco / edq else NA_real_
  out["qpc"]  <- if (!is.na(eco) && abs(eco) > 1e-12) edq / eco else NA_real_
  
  d$nmb_100k <- LAMBDA_100K * d$dqaly - d$cost
  d$nmb_200k <- LAMBDA_200K * d$dqaly - d$cost
  
  fit_nmb100 <- fit_adjusted_env_effect(d, "nmb_100k")
  fit_nmb200 <- fit_adjusted_env_effect(d, "nmb_200k")
  
  out["nmb100k"] <- fit_nmb100$estimate
  out["nmb200k"] <- fit_nmb200$estimate
  
  out
}

set.seed(SEED)
boot_out <- boot(
  data = ce_dat,
  statistic = boot_fn,
  R = BOOT_R,
  strata = ce_dat$env
)

boot_df <- as.data.frame(boot_out$t)
names(boot_df) <- boot_names

ICER_ci <- safe_quantile(boot_df$icer)
QPC_ci  <- safe_quantile(boot_df$qpc)

prob_ce_100k <- mean(boot_df$nmb100k > 0, na.rm = TRUE)
prob_ce_200k <- mean(boot_df$nmb200k > 0, na.rm = TRUE)

# ----------------------------
# Build final client table
# ----------------------------
final_table <- bind_rows(
  make_row("Cost Outcomes, Patient Reported Outcomes, Utility and QALY Measures"),
  make_row(
    "Total Costs",
    all  = sum_cost$all,
    hopd = sum_cost$hopd,
    asc  = sum_cost$asc,
    inc  = fmt_money(fit_cost$estimate, 0),
    ci   = fmt_ci_money(fit_cost$lwr, fit_cost$upr, 0),
    p    = fmt_p(fit_cost$p)
  ),
  make_row(
    "Preop SNOT22",
    all  = sum_pre_snot$all,
    hopd = sum_pre_snot$hopd,
    asc  = sum_pre_snot$asc,
    inc  = fmt_num(fit_pre_snot$estimate, 2),
    ci   = fmt_ci_num(fit_pre_snot$lwr, fit_pre_snot$upr, 2),
    p    = fmt_p(fit_pre_snot$p)
  ),
  make_row(
    "Postop SNOT22",
    all  = sum_post_snot$all,
    hopd = sum_post_snot$hopd,
    asc  = sum_post_snot$asc,
    inc  = fmt_num(fit_post_snot$estimate, 2),
    ci   = fmt_ci_num(fit_post_snot$lwr, fit_post_snot$upr, 2),
    p    = fmt_p(fit_post_snot$p)
  ),
  make_row(
    "Preop QALY",
    all  = sum_pre_qaly$all,
    hopd = sum_pre_qaly$hopd,
    asc  = sum_pre_qaly$asc,
    inc  = fmt_num(fit_pre_qaly$estimate, 3),
    ci   = fmt_ci_num(fit_pre_qaly$lwr, fit_pre_qaly$upr, 3),
    p    = fmt_p(fit_pre_qaly$p)
  ),
  make_row(
    "Postop QALY",
    all  = sum_post_qaly$all,
    hopd = sum_post_qaly$hopd,
    asc  = sum_post_qaly$asc,
    inc  = fmt_num(fit_post_qaly$estimate, 3),
    ci   = fmt_ci_num(fit_post_qaly$lwr, fit_post_qaly$upr, 3),
    p    = fmt_p(fit_post_qaly$p)
  ),
  make_row(
    "Incremental QALY (ΔQALY)",
    all  = sum_dqaly$all,
    hopd = sum_dqaly$hopd,
    asc  = sum_dqaly$asc,
    inc  = fmt_num(fit_dqaly$estimate, 3),
    ci   = fmt_ci_num(fit_dqaly$lwr, fit_dqaly$upr, 3),
    p    = fmt_p(fit_dqaly$p)
  ),
  make_row("Cost-Effectiveness and Efficiency Analysis"),
  make_row(
    "ICER ($/QALY)",
    all  = "",
    hopd = "",
    asc  = "",
    inc  = fmt_money(ICER_est, 0),
    ci   = fmt_ci_money(ICER_ci[1], ICER_ci[2], 0),
    p    = ""
  ),
  make_row(
    "Net Monetary Benefit (WTP = 100k/QALY)",
    all  = sum_nmb100k$all,
    hopd = sum_nmb100k$hopd,
    asc  = sum_nmb100k$asc,
    inc  = fmt_money(fit_nmb100k$estimate, 0),
    ci   = fmt_ci_money(fit_nmb100k$lwr, fit_nmb100k$upr, 0),
    p    = fmt_p(fit_nmb100k$p)
  ),
  make_row(
    "Probability Cost-Effective at 100k",
    all  = "",
    hopd = "",
    asc  = "",
    inc  = fmt_pct(prob_ce_100k, 1),
    ci   = "",
    p    = ""
  ),
  make_row(
    "Probability Cost-Effective at 200k",
    all  = "",
    hopd = "",
    asc  = "",
    inc  = fmt_pct(prob_ce_200k, 1),
    ci   = "",
    p    = ""
  ),
  make_row(
    "QALY per Cost Difference",
    all  = "",
    hopd = "",
    asc  = "",
    inc  = fmt_num(QALY_per_cost_diff_est, 6),
    ci   = fmt_ci_num(QPC_ci[1], QPC_ci[2], 6),
    p    = ""
  ),
  make_row("HOPD - Hospital Outpatient Departments"),
  make_row("ASC - Ambulatory Surgical Center"),
  make_row("SNOT22 - Sino-nasal Outcome Test"),
  make_row("QALY - Quality-Adjusted Life Year"),
  make_row("ICER - Incremental Cost Effectiveness Ratio"),
  make_row("*P value < 0.05")
)

names(final_table) <- c(
  "",
  paste0("All Patients (N = ", N_all, ")"),
  paste0("HOPD (N = ", N_hopd, ")"),
  paste0("ASC (N = ", N_asc, ")"),
  "Increment (HOPD-ASC)",
  "95% CI",
  "p-value"
)

cat("\n==================== FINAL TABLE ====================\n")
print(final_table, row.names = FALSE, right = FALSE)

write.csv(final_table, OUT_CSV, row.names = FALSE)

saveRDS(
  list(
    final_table = final_table,
    cohort_joined = cohort0,
    analysis_data = dat,
    header_counts = c(All = N_all, HOPD = N_hopd, ASC = N_asc),
    model_results = list(
      cost = fit_cost,
      pre_snot22 = fit_pre_snot,
      post_snot22 = fit_post_snot,
      pre_qaly = fit_pre_qaly,
      post_qaly = fit_post_qaly,
      dqaly = fit_dqaly,
      nmb100k = fit_nmb100k
    ),
    ce_results = list(
      ICER_est = ICER_est,
      ICER_ci = ICER_ci,
      qaly_per_cost_difference_est = QALY_per_cost_diff_est,
      qaly_per_cost_difference_ci = QPC_ci,
      prob_ce_100k = prob_ce_100k,
      prob_ce_200k = prob_ce_200k
    ),
    detected_snot_columns = list(
      pre = pre_snot_cols,
      post = post_snot_cols
    )
  ),
  OUT_RDS
)

cat("\nSaved final table to: ", OUT_CSV, "\n", sep = "")
cat("Saved objects to:     ", OUT_RDS, "\n", sep = "")