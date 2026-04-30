# ============================================================
# Revised table-generation code
# Based on updated MRN-based data preparation and analysis files
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(janitor)
  library(stringr)
  library(forcats)
  library(emmeans)
  library(boot)
})

# ------------------------------------------------------------
# User settings
# ------------------------------------------------------------

QALY_COST_FILE  <- "../data/QALYs and Costs 3.26.26.xlsx"
QALY_COST_SHEET <- "Costs and QALYs"
SMITHA_SHEET    <- "Smitha SNOT22s"
COVARIATE_FILE  <- "DRcovariate.rds"

BOOT_R      <- 2000
SEED        <- 1
LAMBDA_100K <- 100000
LAMBDA_200K <- 200000

OUT_CSV <- "client_cost_effectiveness_table_revised.csv"
OUT_RDS <- "client_cost_effectiveness_objects_revised.rds"

# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------

clean_mrn <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_replace("\\.0$", "")
}

load_saved_object <- function(path) {
  e <- new.env(parent = emptyenv())
  nm <- load(path, envir = e)
  e[[nm[1]]]
}

first_non_na <- function(x) {
  if (is.character(x)) {
    x <- x[!is.na(x) & str_squish(x) != ""]
  } else {
    x <- x[!is.na(x)]
  }
  
  if (length(x) == 0) {
    return(NA)
  }
  
  x[1]
}

find_cols <- function(df, patterns) {
  nms <- names(df)
  
  out <- unique(unlist(lapply(patterns, function(p) {
    grep(p, nms, ignore.case = TRUE, value = TRUE)
  })))
  
  out
}

find_first_col <- function(df, patterns, required = TRUE, label = NULL) {
  hits <- find_cols(df, patterns)
  
  if (length(hits) == 0) {
    if (required) {
      if (is.null(label)) label <- paste(patterns, collapse = " / ")
      stop("Cannot find required column: ", label)
    } else {
      return(NA_character_)
    }
  }
  
  hits[1]
}

coalesce_numeric_from_cols <- function(df, cols) {
  if (length(cols) == 0 || all(is.na(cols))) {
    return(rep(NA_real_, nrow(df)))
  }
  
  cols <- cols[!is.na(cols)]
  
  vals <- lapply(cols, function(cc) {
    suppressWarnings(as.numeric(df[[cc]]))
  })
  
  Reduce(dplyr::coalesce, vals)
}

coalesce_character_from_cols <- function(df, cols) {
  if (length(cols) == 0 || all(is.na(cols))) {
    return(rep(NA_character_, nrow(df)))
  }
  
  cols <- cols[!is.na(cols)]
  
  vals <- lapply(cols, function(cc) {
    as.character(df[[cc]])
  })
  
  Reduce(dplyr::coalesce, vals)
}

safe_quantile <- function(x, probs = c(0.025, 0.975)) {
  x <- x[is.finite(x)]
  
  if (length(x) == 0) {
    return(rep(NA_real_, length(probs)))
  }
  
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
  
  if (length(x) == 0) {
    return("")
  }
  
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
  
  if (!outcome %in% names(data)) {
    return(list(all = "", hopd = "", asc = ""))
  }
  
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
      if (length(unique(x_nonmiss)) > 1) {
        keep <- c(keep, v)
      }
    } else {
      if (length(unique(as.character(x_nonmiss))) > 1) {
        keep <- c(keep, v)
      }
    }
  }
  
  rhs <- c("env", keep)
  as.formula(paste(outcome, "~", paste(rhs, collapse = " + ")))
}

fit_adjusted_increment <- function(data, outcome) {
  needed <- c("env", outcome, "age", "sex", "bmi", "smoke", "cci", "los_days")
  needed <- intersect(needed, names(data))
  
  if (!outcome %in% names(data)) {
    return(list(
      n = 0,
      estimate = NA_real_,
      lwr = NA_real_,
      upr = NA_real_,
      p = NA_real_,
      model = NULL,
      contrast = NULL
    ))
  }
  
  d <- data %>%
    select(all_of(needed)) %>%
    filter(!is.na(env), !is.na(.data[[outcome]])) %>%
    droplevels()
  
  d$env <- factor(d$env, levels = c("ASC", "HOPD"))
  
  if (nrow(d) == 0 || length(unique(d$env[!is.na(d$env)])) < 2) {
    return(list(
      n = nrow(d),
      estimate = NA_real_,
      lwr = NA_real_,
      upr = NA_real_,
      p = NA_real_,
      model = NULL,
      contrast = NULL
    ))
  }
  
  fml <- make_model_formula(d, outcome)
  
  mod <- tryCatch(
    lm(fml, data = d),
    error = function(e) NULL
  )
  
  if (is.null(mod)) {
    return(list(
      n = nrow(d),
      estimate = NA_real_,
      lwr = NA_real_,
      upr = NA_real_,
      p = NA_real_,
      model = NULL,
      contrast = NULL
    ))
  }
  
  ct <- tryCatch(
    contrast(emmeans(mod, ~ env), method = "revpairwise"),
    error = function(e) NULL
  )
  
  if (is.null(ct)) {
    return(list(
      n = nrow(d),
      estimate = NA_real_,
      lwr = NA_real_,
      upr = NA_real_,
      p = NA_real_,
      model = mod,
      contrast = NULL
    ))
  }
  
  ct_df <- tryCatch(
    as.data.frame(summary(ct, infer = c(TRUE, TRUE))),
    error = function(e) NULL
  )
  
  if (is.null(ct_df) || nrow(ct_df) == 0) {
    return(list(
      n = nrow(d),
      estimate = NA_real_,
      lwr = NA_real_,
      upr = NA_real_,
      p = NA_real_,
      model = mod,
      contrast = ct
    ))
  }
  
  lwr_col <- if ("lower.CL" %in% names(ct_df)) "lower.CL" else NA_character_
  upr_col <- if ("upper.CL" %in% names(ct_df)) "upper.CL" else NA_character_
  p_col   <- if ("p.value"  %in% names(ct_df)) "p.value"  else NA_character_
  
  list(
    n = nrow(d),
    estimate = unname(ct_df$estimate[1]),
    lwr = if (!is.na(lwr_col)) unname(ct_df[[lwr_col]][1]) else NA_real_,
    upr = if (!is.na(upr_col)) unname(ct_df[[upr_col]][1]) else NA_real_,
    p = if (!is.na(p_col)) unname(ct_df[[p_col]][1]) else NA_real_,
    model = mod,
    contrast = ct
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

# ------------------------------------------------------------
# Load QC and match MRN using Smitha SNOT22s
# ------------------------------------------------------------

QC_raw <- read_excel(QALY_COST_FILE, sheet = QALY_COST_SHEET)

smitha <- read_excel(QALY_COST_FILE, sheet = SMITHA_SHEET)

smitha_lookup_all <- smitha %>%
  mutate(
    first_name_match = str_to_upper(str_squish(as.character(PAT_FIRST_NAME))),
    last_name_match  = str_to_upper(str_squish(as.character(PAT_LAST_NAME))),
    MRN = clean_mrn(MRN)
  ) %>%
  filter(
    !is.na(first_name_match), first_name_match != "",
    !is.na(last_name_match), last_name_match != "",
    !is.na(MRN), MRN != ""
  ) %>%
  distinct(first_name_match, last_name_match, MRN)

duplicate_name_check <- smitha_lookup_all %>%
  count(first_name_match, last_name_match, name = "n_mrn") %>%
  filter(n_mrn > 1)

if (nrow(duplicate_name_check) > 0) {
  warning("Some first-name/last-name combinations match multiple MRNs. Please review duplicate_name_check.")
}

smitha_lookup <- smitha_lookup_all %>%
  group_by(first_name_match, last_name_match) %>%
  summarise(
    MRN = first_non_na(MRN),
    .groups = "drop"
  )

QC_joined <- QC_raw %>%
  mutate(
    first_name_match = str_to_upper(str_squish(as.character(`Patient First name`))),
    last_name_match  = str_to_upper(str_squish(as.character(`Patient Last Name`)))
  ) %>%
  left_join(
    smitha_lookup,
    by = c("first_name_match", "last_name_match")
  ) %>%
  clean_names()

# ------------------------------------------------------------
# Standardize QC variables
# ------------------------------------------------------------

asc_col <- find_first_col(
  QC_joined,
  patterns = c("^asc_or_hopd$", "asc.*hopd"),
  required = TRUE,
  label = "ASC OR HOPD"
)

mrn_col <- find_first_col(
  QC_joined,
  patterns = c("^mrn$", "medical_record_number"),
  required = TRUE,
  label = "MRN"
)

age_col <- find_first_col(
  QC_joined,
  patterns = c("^age_at_time_of_surgery$", "age.*time.*surgery"),
  required = TRUE,
  label = "Age at time of surgery"
)

cost_col <- find_first_col(
  QC_joined,
  patterns = c("^total_costs$", "^total_cost$"),
  required = TRUE,
  label = "Total Costs"
)

pre_qaly_col <- find_first_col(
  QC_joined,
  patterns = c("^preop_qaly", "pre.*qaly"),
  required = TRUE,
  label = "Preop QALY"
)

post_qaly_col <- find_first_col(
  QC_joined,
  patterns = c("^postop_qaly", "post.*qaly"),
  required = TRUE,
  label = "Postop QALY"
)

pre_snot_cols <- find_cols(
  QC_joined,
  patterns = c("^preop_snot22.*total$", "^pre.*snot.*22.*total$", "^pre.*snot")
)

post_snot_cols <- find_cols(
  QC_joined,
  patterns = c("^postop_snot22.*total$", "^post.*snot.*22.*total$", "^post.*snot")
)

pre_snot_cols <- setdiff(pre_snot_cols, c(pre_qaly_col, post_qaly_col))
post_snot_cols <- setdiff(post_snot_cols, c(pre_qaly_col, post_qaly_col))

message("Detected pre-op SNOT columns:  ",
        ifelse(length(pre_snot_cols) == 0, "NONE", paste(pre_snot_cols, collapse = ", ")))

message("Detected post-op SNOT columns: ",
        ifelse(length(post_snot_cols) == 0, "NONE", paste(post_snot_cols, collapse = ", ")))

QC1 <- tibble(
  asc_or_hopd = as.character(QC_joined[[asc_col]]),
  mrn = clean_mrn(QC_joined[[mrn_col]]),
  age_at_time_of_surgery = suppressWarnings(as.numeric(QC_joined[[age_col]])),
  total_costs = suppressWarnings(as.numeric(QC_joined[[cost_col]])),
  preop_qaly = suppressWarnings(as.numeric(QC_joined[[pre_qaly_col]])),
  postop_qaly = suppressWarnings(as.numeric(QC_joined[[post_qaly_col]])),
  preop_snot22 = coalesce_numeric_from_cols(QC_joined, pre_snot_cols),
  postop_snot22 = coalesce_numeric_from_cols(QC_joined, post_snot_cols)
)

# ------------------------------------------------------------
# Load one-record-per-patient covariates from updated step0_datapre.R
# ------------------------------------------------------------

DRcov_raw <- load_saved_object(COVARIATE_FILE) %>%
  clean_names()

if (!"mrn" %in% names(DRcov_raw)) {
  stop("DRcovariate.rds does not contain 'mrn' after clean_names().")
}

cci_col <- NA_character_

if ("cciscore" %in% names(DRcov_raw)) {
  cci_col <- "cciscore"
} else if ("cci_score" %in% names(DRcov_raw)) {
  cci_col <- "cci_score"
} else {
  cci_candidates <- grep("^cci", names(DRcov_raw), value = TRUE)
  if (length(cci_candidates) > 0) {
    cci_col <- cci_candidates[1]
  }
}

if (is.na(cci_col)) {
  stop("Cannot find CCI column in DRcovariate.rds.")
}

cov_keep <- c(
  "mrn",
  "sex",
  "bmi",
  "smoke5",
  cci_col,
  "hosp_gap_days"
)

cov_keep <- intersect(cov_keep, names(DRcov_raw))

DRcov1 <- DRcov_raw %>%
  select(all_of(cov_keep)) %>%
  mutate(
    mrn = clean_mrn(mrn)
  )

if (cci_col != "cciscore") {
  DRcov1 <- DRcov1 %>%
    rename(cciscore = all_of(cci_col))
}

DRcov1 <- DRcov1 %>%
  group_by(mrn) %>%
  summarise(
    sex = if ("sex" %in% names(.)) first_non_na(sex) else NA,
    bmi = if ("bmi" %in% names(.)) first_non_na(bmi) else NA_real_,
    smoke5 = if ("smoke5" %in% names(.)) first_non_na(smoke5) else NA,
    cciscore = if ("cciscore" %in% names(.)) first_non_na(cciscore) else NA_real_,
    hosp_gap_days = if ("hosp_gap_days" %in% names(.)) first_non_na(hosp_gap_days) else NA_real_,
    .groups = "drop"
  )

# ------------------------------------------------------------
# Join QC and covariates by MRN
# ------------------------------------------------------------

cohort0 <- QC1 %>%
  left_join(DRcov1, by = "mrn")

cat("\n==================== COHORT SIZE CHECK ====================\n")
cat("QC rows:             ", nrow(QC1), "\n")
cat("Joined cohort rows:  ", nrow(cohort0), "\n")
cat("Unique MRNs in QC:   ", n_distinct(QC1$mrn), "\n")
cat("Unique MRNs joined:  ", n_distinct(cohort0$mrn), "\n")

# ------------------------------------------------------------
# Build analysis variables
# ------------------------------------------------------------

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
    
    pre_snot22 = suppressWarnings(as.numeric(preop_snot22)),
    post_snot22 = suppressWarnings(as.numeric(postop_snot22)),
    
    age = suppressWarnings(as.numeric(age_at_time_of_surgery)),
    bmi = suppressWarnings(as.numeric(bmi)),
    cci = suppressWarnings(as.numeric(cciscore)),
    los_days = suppressWarnings(as.numeric(hosp_gap_days)),
    
    sex = as.character(sex),
    smoke = as.character(smoke5)
  )

# Fill covariates to stabilize adjusted models
if (all(is.na(dat$bmi))) {
  warning("BMI is all missing after join. BMI will not be useful in adjusted models.")
} else {
  dat$bmi[is.na(dat$bmi)] <- median(dat$bmi, na.rm = TRUE)
}

dat$cci[is.na(dat$cci)] <- 0
dat$los_days[is.na(dat$los_days)] <- 0

dat$sex[is.na(dat$sex) | dat$sex == "" | dat$sex == "0"] <- "Unknown"
dat$smoke[is.na(dat$smoke) | dat$smoke == "" | dat$smoke == "0"] <- "Unknown"

dat$sex <- factor(dat$sex)
dat$smoke <- factor(dat$smoke)

dat <- dat %>%
  mutate(
    nmb_100k = LAMBDA_100K * dqaly - cost,
    nmb_200k = LAMBDA_200K * dqaly - cost,
    q_per_cost = if_else(
      !is.na(cost) & cost > 0 & !is.na(dqaly),
      dqaly / cost,
      NA_real_
    )
  )

# Main analytic cohort follows updated analysis code:
# non-missing environment, cost, and QALY difference
analysis_dat <- dat %>%
  filter(
    !is.na(env),
    !is.na(cost),
    !is.na(dqaly)
  ) %>%
  droplevels()

N_all  <- nrow(analysis_dat)
N_hopd <- sum(analysis_dat$env == "HOPD", na.rm = TRUE)
N_asc  <- sum(analysis_dat$env == "ASC",  na.rm = TRUE)

cat("\n==================== FINAL HEADER COUNTS ====================\n")
cat("All patients: ", N_all, "\n")
cat("HOPD:         ", N_hopd, "\n")
cat("ASC:          ", N_asc, "\n")

# ------------------------------------------------------------
# Raw summaries
# ------------------------------------------------------------

sum_cost      <- get_group_summary(analysis_dat, "cost",         type = "money", digits = 0)
sum_pre_snot  <- get_group_summary(analysis_dat, "pre_snot22",   type = "num",   digits = 2)
sum_post_snot <- get_group_summary(analysis_dat, "post_snot22",  type = "num",   digits = 2)
sum_pre_qaly  <- get_group_summary(analysis_dat, "pre_qaly",     type = "num",   digits = 3)
sum_post_qaly <- get_group_summary(analysis_dat, "post_qaly",    type = "num",   digits = 3)
sum_dqaly     <- get_group_summary(analysis_dat, "dqaly",        type = "num",   digits = 3)
sum_nmb100k   <- get_group_summary(analysis_dat, "nmb_100k",     type = "money", digits = 0)
sum_qpc       <- get_group_summary(analysis_dat, "q_per_cost",   type = "num",   digits = 8)

# ------------------------------------------------------------
# Adjusted HOPD - ASC estimates
# ------------------------------------------------------------

fit_cost      <- fit_adjusted_increment(analysis_dat, "cost")
fit_pre_snot  <- fit_adjusted_increment(analysis_dat, "pre_snot22")
fit_post_snot <- fit_adjusted_increment(analysis_dat, "post_snot22")
fit_pre_qaly  <- fit_adjusted_increment(analysis_dat, "pre_qaly")
fit_post_qaly <- fit_adjusted_increment(analysis_dat, "post_qaly")
fit_dqaly     <- fit_adjusted_increment(analysis_dat, "dqaly")
fit_nmb100k   <- fit_adjusted_increment(analysis_dat, "nmb_100k")

dat_costpos <- analysis_dat %>%
  filter(!is.na(q_per_cost), cost > 0) %>%
  droplevels()

fit_qpc <- fit_adjusted_increment(dat_costpos, "q_per_cost")

# ------------------------------------------------------------
# ICER based on adjusted HOPD - ASC increments
# ------------------------------------------------------------

ICER_est <- if (
  is.finite(fit_dqaly$estimate) &&
  !is.na(fit_dqaly$estimate) &&
  abs(fit_dqaly$estimate) > 1e-12
) {
  fit_cost$estimate / fit_dqaly$estimate
} else {
  NA_real_
}

# ------------------------------------------------------------
# Bootstrap for ICER, QALY-per-cost, and CEAC probabilities
# ------------------------------------------------------------

lambda_grid <- seq(0, 200000, by = 5000)

boot_names <- c(
  "edq",
  "eco",
  "icer",
  "qpc",
  paste0("nmb_", lambda_grid)
)

boot_fn <- function(data, indices) {
  d <- data[indices, , drop = FALSE]
  d <- d %>% droplevels()
  
  out <- rep(NA_real_, length(boot_names))
  names(out) <- boot_names
  
  fit_dq <- fit_adjusted_increment(d, "dqaly")
  fit_co <- fit_adjusted_increment(d, "cost")
  
  edq <- fit_dq$estimate
  eco <- fit_co$estimate
  
  out["edq"] <- edq
  out["eco"] <- eco
  
  out["icer"] <- if (
    is.finite(edq) &&
    !is.na(edq) &&
    abs(edq) > 1e-12
  ) {
    eco / edq
  } else {
    NA_real_
  }
  
  d_qpc <- d %>%
    filter(!is.na(q_per_cost), cost > 0) %>%
    droplevels()
  
  fit_qpc_b <- fit_adjusted_increment(d_qpc, "q_per_cost")
  out["qpc"] <- fit_qpc_b$estimate
  
  for (lam in lambda_grid) {
    nmb_var <- paste0("nmb_", lam)
    d[[nmb_var]] <- lam * d$dqaly - d$cost
    
    fit_nmb <- fit_adjusted_increment(d, nmb_var)
    out[nmb_var] <- fit_nmb$estimate
  }
  
  out
}

set.seed(SEED)

boot_out <- boot(
  data = analysis_dat,
  statistic = boot_fn,
  R = BOOT_R,
  strata = analysis_dat$env
)

boot_df <- as.data.frame(boot_out$t)
names(boot_df) <- boot_names

ICER_ci <- safe_quantile(boot_df$icer)
QPC_ci  <- safe_quantile(boot_df$qpc)

nmb_cols <- grep("^nmb_", names(boot_df), value = TRUE)

ceac <- data.frame(
  lambda = as.numeric(sub("^nmb_", "", nmb_cols)),
  prob_ce = colMeans(boot_df[, nmb_cols, drop = FALSE] > 0, na.rm = TRUE)
)

prob_ce_100k <- ceac %>%
  filter(lambda == LAMBDA_100K) %>%
  pull(prob_ce)

prob_ce_200k <- ceac %>%
  filter(lambda == LAMBDA_200K) %>%
  pull(prob_ce)

if (length(prob_ce_100k) == 0) prob_ce_100k <- NA_real_
if (length(prob_ce_200k) == 0) prob_ce_200k <- NA_real_

# ------------------------------------------------------------
# Optional nonparametric comparison for QALY per cost
# ------------------------------------------------------------

qpc_wilcox <- tryCatch(
  wilcox.test(q_per_cost ~ env, data = dat_costpos),
  error = function(e) NULL
)

qpc_wilcox_p <- if (!is.null(qpc_wilcox)) {
  qpc_wilcox$p.value
} else {
  NA_real_
}

# ------------------------------------------------------------
# Build final client table
# ------------------------------------------------------------

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
    "Net Monetary Benefit (WTP = $100k/QALY)",
    all  = sum_nmb100k$all,
    hopd = sum_nmb100k$hopd,
    asc  = sum_nmb100k$asc,
    inc  = fmt_money(fit_nmb100k$estimate, 0),
    ci   = fmt_ci_money(fit_nmb100k$lwr, fit_nmb100k$upr, 0),
    p    = fmt_p(fit_nmb100k$p)
  ),
  
  make_row(
    "Probability Cost-Effective at $100k/QALY",
    all  = "",
    hopd = "",
    asc  = "",
    inc  = fmt_pct(prob_ce_100k, 1),
    ci   = "",
    p    = ""
  ),
  
  make_row(
    "Probability Cost-Effective at $200k/QALY",
    all  = "",
    hopd = "",
    asc  = "",
    inc  = fmt_pct(prob_ce_200k, 1),
    ci   = "",
    p    = ""
  ),
  
  make_row(
    "QALY per Cost (ΔQALY / Cost)",
    all  = sum_qpc$all,
    hopd = sum_qpc$hopd,
    asc  = sum_qpc$asc,
    inc  = fmt_num(fit_qpc$estimate, 8),
    ci   = fmt_ci_num(QPC_ci[1], QPC_ci[2], 8),
    p    = fmt_p(fit_qpc$p)
  ),
  
  make_row("Definitions / Notes"),
  make_row("HOPD - Hospital Outpatient Departments"),
  make_row("ASC - Ambulatory Surgical Center"),
  make_row("SNOT22 - Sino-nasal Outcome Test"),
  make_row("QALY - Quality-Adjusted Life Year"),
  make_row("ICER - Incremental Cost Effectiveness Ratio"),
  make_row("NMB - Net Monetary Benefit"),
  make_row("*P value < 0.05")
)

names(final_table) <- c(
  "",
  paste0("All Patients (N = ", N_all, ")"),
  paste0("HOPD (N = ", N_hopd, ")"),
  paste0("ASC (N = ", N_asc, ")"),
  "Increment (HOPD - ASC)",
  "95% CI",
  "p-value"
)

cat("\n==================== FINAL TABLE ====================\n")
print(final_table, row.names = FALSE, right = FALSE)

# ------------------------------------------------------------
# Save outputs
# ------------------------------------------------------------

write.csv(final_table, OUT_CSV, row.names = FALSE)

saveRDS(
  list(
    final_table = final_table,
    cohort_joined = cohort0,
    analysis_data = analysis_dat,
    header_counts = c(All = N_all, HOPD = N_hopd, ASC = N_asc),
    duplicate_name_check = duplicate_name_check,
    detected_snot_columns = list(
      pre = pre_snot_cols,
      post = post_snot_cols
    ),
    model_results = list(
      cost = fit_cost,
      pre_snot22 = fit_pre_snot,
      post_snot22 = fit_post_snot,
      pre_qaly = fit_pre_qaly,
      post_qaly = fit_post_qaly,
      dqaly = fit_dqaly,
      nmb100k = fit_nmb100k,
      q_per_cost = fit_qpc
    ),
    ce_results = list(
      ICER_est = ICER_est,
      ICER_ci = ICER_ci,
      prob_ce_100k = prob_ce_100k,
      prob_ce_200k = prob_ce_200k,
      ceac = ceac,
      qpc_wilcox_p = qpc_wilcox_p,
      qpc_boot_ci = QPC_ci
    ),
    bootstrap = list(
      boot_out = boot_out,
      boot_df = boot_df
    )
  ),
  OUT_RDS
)

cat("\nSaved final table to: ", OUT_CSV, "\n", sep = "")
cat("Saved objects to:     ", OUT_RDS, "\n", sep = "")