# Load necessary libraries
library(dplyr)
library(forcats)
library(tidyr)
library(readxl)
library(janitor)
library(stringr)
library(lubridate)
library(emmeans)
library(boot)

# ------------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------------

clean_mrn <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_replace("\\.0$", "")
}

last_non_missing <- function(x) {
  if (is.character(x)) {
    keep <- !is.na(x) & str_squish(x) != ""
  } else {
    keep <- !is.na(x)
  }
  
  x2 <- x[keep]
  
  if (length(x2) == 0) {
    return(x[NA_integer_][1])
  }
  
  dplyr::last(x2)
}

# ------------------------------------------------------------------
# Load processed QALY and Cost file
# ------------------------------------------------------------------

QC <- read_excel("../data/QALYs and Costs 3.26.26.xlsx", sheet = "Costs and QALYs")

# Load Smitha SNOT22s sheet for MRN matching
smitha <- read_excel("../data/QALYs and Costs 3.26.26.xlsx", sheet = "Smitha SNOT22s")

# Create name-based MRN lookup
smitha_lookup <- smitha %>%
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
  "MRN",
  "Age at time of surgery",
  "Total Costs",
  "Preop QALY",
  "Postop QALY"
)

# ------------------------------------------------------------------
# Load covariates file
# ------------------------------------------------------------------

DR4048 <- read.csv("../data/DR4048_onerecord.csv")

# Drop completely empty columns
DR4048 <- DR4048[, colSums(!(is.na(DR4048) | DR4048 == "")) > 0]

# ------------------------------------------------------------------
# Step 1: work on ICD codes
# ------------------------------------------------------------------

df <- DR4048 %>%
  mutate(row_id = row_number())

icd_cols <- c("icd10_code1", paste0("ICD10_Code", 2:8))
icd_cols <- icd_cols[icd_cols %in% names(df)]

icd_long <- df %>%
  pivot_longer(
    cols = all_of(icd_cols),
    names_to = "icd_position",
    values_to = "icd10_code"
  ) %>%
  mutate(icd10_code = str_trim(icd10_code)) %>%
  filter(!is.na(icd10_code), icd10_code != "")

icd_wide <- icd_long %>%
  mutate(flag = 1L) %>%
  distinct(row_id, icd10_code, .keep_all = TRUE) %>%
  pivot_wider(
    id_cols = row_id,
    names_from = icd10_code,
    names_prefix = "icd:",
    values_from = flag,
    values_fill = 0
  )

drop_cols <- grep(
  "^(icd10_code1|ICD10_Code[2-8]|ICD_Dx_Name\\d+)$",
  names(df),
  value = TRUE
)

DR4048 <- df %>%
  select(-all_of(drop_cols)) %>%
  left_join(icd_wide, by = "row_id") %>%
  select(-row_id)

# ------------------------------------------------------------------
# Step 2: work on CPT codes
# ------------------------------------------------------------------

df <- DR4048 %>%
  mutate(row_id = row_number())

cpt_cols <- grep("^CPT_Code_\\d+$", names(df), value = TRUE)

df <- df %>%
  mutate(across(all_of(cpt_cols), ~ as.character(.)))

cpt_long <- df %>%
  pivot_longer(
    cols = all_of(cpt_cols),
    names_to = "cpt_position",
    values_to = "cpt_code"
  ) %>%
  mutate(cpt_code = str_trim(cpt_code)) %>%
  filter(!is.na(cpt_code), cpt_code != "")

cpt_long <- cpt_long %>%
  mutate(
    cpt_code = toupper(cpt_code),
    cpt_code = gsub("[^A-Z0-9]", "", cpt_code)
  ) %>%
  filter(str_detect(cpt_code, "^[A-Z0-9]{5}$"))

cpt_wide <- cpt_long %>%
  mutate(flag = 1L) %>%
  distinct(row_id, cpt_code, .keep_all = TRUE) %>%
  pivot_wider(
    id_cols = row_id,
    names_from = cpt_code,
    names_prefix = "cpt:",
    values_from = flag,
    values_fill = 0
  )

drop_cols_cpt <- grep(
  "^CPT_Code_\\d+$|^CPT_Name_\\d+$",
  names(df),
  value = TRUE
)

DR4048 <- df %>%
  select(-all_of(drop_cols_cpt)) %>%
  left_join(cpt_wide, by = "row_id") %>%
  select(-row_id)

# ------------------------------------------------------------------
# Step 3: work on antibiotics
# ------------------------------------------------------------------

df <- DR4048 %>%
  mutate(row_id = row_number())

abx_name_cols <- grep("^AntiBioT_Name_\\d+$", names(df), value = TRUE)
abx_date_cols <- grep("^AntiBioT_Strt_DT_\\d+$", names(df), value = TRUE)

df <- df %>%
  mutate(across(all_of(abx_name_cols), ~ as.character(.)))

abx_long <- df %>%
  pivot_longer(
    cols = all_of(abx_name_cols),
    names_to = "abx_position",
    values_to = "abx_name"
  ) %>%
  mutate(
    abx_name = str_squish(str_to_upper(abx_name)),
    abx_root = str_extract(abx_name, "^[A-Z]+")
  ) %>%
  filter(!is.na(abx_root), abx_root != "")

abx_wide <- abx_long %>%
  mutate(flag = 1L) %>%
  distinct(row_id, abx_root, .keep_all = TRUE) %>%
  pivot_wider(
    id_cols = row_id,
    names_from = abx_root,
    names_prefix = "antibios:",
    values_from = flag,
    values_fill = 0
  ) %>%
  clean_names()

drop_cols <- c(abx_name_cols, abx_date_cols)

DR4048 <- df %>%
  select(-all_of(drop_cols)) %>%
  left_join(abx_wide, by = "row_id") %>%
  select(-row_id)

# ------------------------------------------------------------------
# Step 4: work on bioagents
# ------------------------------------------------------------------

df <- DR4048 %>%
  mutate(row_id = row_number())

bio_name_cols <- grep("^BioAgents_Name_\\d+$", names(df), value = TRUE)
bio_date_cols <- grep("^BioAgents_Strt_DT_\\d+$", names(df), value = TRUE)

df <- df %>%
  mutate(across(all_of(bio_name_cols), ~ as.character(.)))

bio_long <- df %>%
  pivot_longer(
    cols = all_of(bio_name_cols),
    names_to = "bio_position",
    values_to = "bio_name"
  ) %>%
  mutate(
    bio_name = str_squish(str_to_upper(bio_name)),
    bio_root = str_extract(bio_name, "^[A-Z]+")
  ) %>%
  filter(!is.na(bio_root), bio_root != "")

bio_wide <- bio_long %>%
  mutate(flag = 1L) %>%
  distinct(row_id, bio_root, .keep_all = TRUE) %>%
  pivot_wider(
    id_cols = row_id,
    names_from = bio_root,
    names_prefix = "bioagents:",
    values_from = flag,
    values_fill = 0
  ) %>%
  clean_names()

drop_cols <- c(bio_name_cols, bio_date_cols)

DR4048 <- df %>%
  select(-all_of(drop_cols)) %>%
  left_join(bio_wide, by = "row_id") %>%
  select(-row_id)

# ------------------------------------------------------------------
# Step 5: work on steroids
# ------------------------------------------------------------------

df <- DR4048 %>%
  mutate(row_id = row_number())

st_name_cols <- grep("^Steroids_Name_\\d+$", names(df), value = TRUE)
st_date_cols <- grep("^Steroids_Strt_DT_\\d+$", names(df), value = TRUE)

df <- df %>%
  mutate(across(all_of(st_name_cols), ~ as.character(.)))

st_long <- df %>%
  pivot_longer(
    cols = all_of(st_name_cols),
    names_to = "st_position",
    values_to = "st_name"
  ) %>%
  mutate(
    st_name = str_squish(str_to_upper(st_name)),
    st_root = str_extract(st_name, "^[A-Z]+")
  ) %>%
  filter(!is.na(st_root), st_root != "")

st_wide <- st_long %>%
  mutate(flag = 1L) %>%
  distinct(row_id, st_root, .keep_all = TRUE) %>%
  pivot_wider(
    id_cols = row_id,
    names_from = st_root,
    names_prefix = "steroids:",
    values_from = flag,
    values_fill = 0
  )

drop_cols <- c(st_name_cols, st_date_cols)

DR4048 <- df %>%
  select(-all_of(drop_cols)) %>%
  left_join(st_wide, by = "row_id") %>%
  select(-row_id)

# ------------------------------------------------------------------
# Step 6: work on anticoag
# ------------------------------------------------------------------

df <- DR4048 %>%
  mutate(row_id = row_number())

ac_name_cols <- grep("^Anticoag_Name_\\d+$", names(df), value = TRUE)
ac_date_cols <- grep("^Anticoag_(Strt|End)_DT_\\d+$", names(df), value = TRUE)

df <- df %>%
  mutate(across(all_of(ac_name_cols), ~ as.character(.)))

ac_long <- df %>%
  pivot_longer(
    cols = all_of(ac_name_cols),
    names_to = "ac_pos",
    values_to = "ac_name"
  ) %>%
  mutate(
    ac_name = str_squish(str_to_upper(ac_name)),
    ac_root = str_extract(ac_name, "^[A-Z]+")
  ) %>%
  filter(!is.na(ac_root), ac_root != "")

ac_wide <- ac_long %>%
  mutate(flag = 1L) %>%
  distinct(row_id, ac_root, .keep_all = TRUE) %>%
  pivot_wider(
    id_cols = row_id,
    names_from = ac_root,
    names_prefix = "anticoag:",
    values_from = flag,
    values_fill = 0
  )

drop_cols <- c(ac_name_cols, ac_date_cols)

DR4048 <- df %>%
  select(-all_of(drop_cols)) %>%
  left_join(ac_wide, by = "row_id") %>%
  select(-row_id)

# ------------------------------------------------------------------
# Standardize column names
# ------------------------------------------------------------------

names(DR4048) <- tolower(names(DR4048))

names(DR4048) <- sub("^antibios_",  "antibios:",  names(DR4048))
names(DR4048) <- sub("^bioagents_", "bioagents:", names(DR4048))
names(DR4048) <- sub("^cpt_",       "cpt:",       names(DR4048))
names(DR4048) <- sub("^icd_",       "icd:",       names(DR4048))

# ------------------------------------------------------------------
# Aggregate ICD codes to first 3 characters
# Example: icd:j32xxx -> icd:j32
# ------------------------------------------------------------------

icd_cols <- grep("^icd:[a-z][0-9]{2}", names(DR4048), value = TRUE)

if (length(icd_cols) > 0) {
  mat <- as.matrix(DR4048[, icd_cols, drop = FALSE])
  mat[is.na(mat)] <- 0
  storage.mode(mat) <- "numeric"
  
  icd_roots <- sub("^icd:([a-z][0-9]{2}).*$", "icd:\\1", icd_cols, perl = TRUE)
  
  agg_mat <- t(rowsum(t(mat), group = icd_roots))
  agg_mat <- ifelse(agg_mat > 0, 1L, 0L)
  
  agg_df <- as.data.frame(agg_mat, check.names = FALSE)
  
  DR4048 <- DR4048 %>%
    select(-all_of(icd_cols)) %>%
    bind_cols(agg_df)
}

# ------------------------------------------------------------------
# Parse surgery date, hospital admit/discharge dates, and calculate gap
# ------------------------------------------------------------------

DR4048 <- DR4048 %>%
  mutate(
    mrn = clean_mrn(mrn),
    
    surgery_date_parsed = parse_date_time(
      surgery_date,
      orders = c("d-b-y", "d-b-Y", "mdy", "ymd"),
      locale = "C"
    ) %>%
      as.Date(),
    
    hosp_adm_date = parse_date_time(
      hosp_adm_dt,
      orders = c("d-b-y", "d-b-Y", "mdy", "ymd"),
      locale = "C"
    ) %>%
      as.Date(),
    
    hosp_disch_date = parse_date_time(
      hosp_disch_dt,
      orders = c("d-b-y", "d-b-Y", "mdy", "ymd"),
      locale = "C"
    ) %>%
      as.Date(),
    
    hosp_gap_days = if_else(
      !is.na(hosp_adm_date) &
        !is.na(hosp_disch_date) &
        hosp_disch_date >= hosp_adm_date,
      as.integer(hosp_disch_date - hosp_adm_date),
      NA_integer_
    )
  )

# ------------------------------------------------------------------
# Collapse DR4048 to one row per MRN
# ------------------------------------------------------------------
# For cpt:, icd:, antibios:, bioagents:, steroids:, anticoag:
#   use max(), meaning ever had this feature.
#
# For non-aggregated predictors such as bmi, cci, sex, smoking status,
# hosp_gap_days, etc.:
#   take the latest non-missing value based on surgery date.
# ------------------------------------------------------------------

indicator_prefixes <- c(
  "cpt:",
  "icd:",
  "antibios:",
  "bioagents:",
  "steroids:",
  "anticoag:"
)

indicator_cols <- names(DR4048)[
  str_detect(
    names(DR4048),
    paste0("^(", paste(indicator_prefixes, collapse = "|"), ")")
  )
]

non_indicator_cols <- setdiff(names(DR4048), c("mrn", indicator_cols))

DR4048 <- DR4048 %>%
  mutate(
    surgery_date_sort = coalesce(surgery_date_parsed, as.Date("1900-01-01"))
  ) %>%
  arrange(mrn, surgery_date_sort) %>%
  group_by(mrn) %>%
  summarise(
    across(
      all_of(non_indicator_cols),
      last_non_missing
    ),
    across(
      all_of(indicator_cols),
      ~ {
        x_num <- suppressWarnings(as.numeric(.x))
        x_num[is.na(x_num)] <- 0
        as.integer(max(x_num, na.rm = TRUE))
      }
    ),
    .groups = "drop"
  )

# Optional check: should now be one row per MRN
mrn_duplicate_check <- DR4048 %>%
  count(mrn) %>%
  filter(n > 1)

if (nrow(mrn_duplicate_check) > 0) {
  warning("Some MRNs still have multiple rows. Please review mrn_duplicate_check.")
}

# ------------------------------------------------------------------
# Select covariates
# ------------------------------------------------------------------

prefixes <- c(
  "cci",
  "cpt:",
  "icd:",
  "antibios:",
  "bioagents:",
  "steroids:",
  "anticoag:"
)

df_select <- DR4048 %>%
  select(
    any_of(c(
      "mrn",
      "sex",
      "bmi",
      "smokingstatus_around_surg",
      "hosp_gap_days"
    )),
    tidyselect::starts_with(prefixes)
  )

# Impute missing BMI using median BMI
med_bmi <- median(df_select$bmi, na.rm = TRUE)

df_select <- df_select %>%
  mutate(
    bmi = coalesce(bmi, med_bmi)
  )

# Replace remaining NA values with 0
df_select[is.na(df_select)] <- 0

# Remove cpt:new if present
df_select <- df_select %>%
  select(-any_of("cpt:new"))

# Remove numeric/logical columns that are all 0
df_select <- df_select %>%
  select(
    where(
      ~ !(is.numeric(.) || is.logical(.)) || any(. != 0, na.rm = TRUE)
    )
  )

# Create smoking category
df_select <- df_select %>%
  mutate(
    smoke_raw = smokingstatus_around_surg %>%
      as.character() %>%
      str_to_lower() %>%
      str_squish() %>%
      str_replace_all("[^a-z ]", ""),
    
    smoke5 = case_when(
      smoke_raw %in% c("passive smoke exposure  never smoker") ~ "Passive",
      smoke_raw %in% c("never", "never smoker") ~ "Never",
      smoke_raw %in% c("former", "former smoker") ~ "Former",
      smoke_raw %in% c(
        "some days",
        "current some day smoker",
        "light tobacco smoker"
      ) ~ "Current (nondaily/light)",
      smoke_raw %in% c(
        "every day",
        "current every day smoker",
        "heavy tobacco smoker"
      ) ~ "Current (daily/heavy)",
      smoke_raw %in% c("unknown", "never assessed") ~ "Unknown",
      is.na(smoke_raw) | smoke_raw == "" ~ "Unknown",
      TRUE ~ "Unknown"
    ),
    
    smoke5 = fct_relevel(
      factor(smoke5),
      "Never",
      "Passive",
      "Former",
      "Current (nondaily/light)",
      "Current (daily/heavy)",
      "Unknown"
    )
  ) %>%
  select(-smoke_raw)

# Save one-record-per-patient covariate file
save(df_select, file = "DRcovariate.rds")

