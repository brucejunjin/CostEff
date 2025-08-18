# Load necessary libraries
library(dplyr)
library(tidyr)
library(readxl)
library(janitor)
library(stringr)
library(lubridate)

# load processed QALY and Cost file
QC <- read_excel("../data/QALYs and Costs 3.17.25.xlsx", sheet = "Cost and QALYs")
QC <- QC[,c("ASC or HOPD", "Patient Number (coded)", "Age at time of surgery",
            "Total Costs", "Preop QALY", "Postop QALY")]

# load covariates file
DR2380 <- read.csv('../data/DR2380_onerecord.csv')
DR2380 <- DR2380[, colSums(!(is.na(DR2380) | DR2380 == "")) > 0]

# load patient coding
SNOT2QALY <- read_excel("../data/QALYs and Costs 3.17.25.xlsx", sheet = "SNOT22s to QALYs")
MRN2PC <- SNOT2QALY[,c('Patient Number (coded)', 'mrn')]
names(MRN2PC) <- c('PC', 'MRN')
MRN2PC <- unique(MRN2PC)

# covariate wide to specific
# Step 1: work on icd
df <- DR2380 %>% mutate(row_id = row_number())
icd_cols <- c("icd10_code1", paste0("ICD10_Code", 2:8))
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
drop_cols <- grep("^(icd10_code1|ICD10_Code[2-8]|ICD_Dx_Name\\d+)$", names(df), value = TRUE)
DR2380 <- df %>%
  select(-all_of(drop_cols)) %>%
  left_join(icd_wide, by = "row_id") %>%
  select(-row_id)
# Step 2: work on cpt
df <- DR2380 %>% mutate(row_id = row_number())
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
drop_cols_cpt <- grep("^CPT_Code_\\d+$|^CPT_Name_\\d+$", names(df), value = TRUE)
DR2380 <- df %>%
  select(-all_of(drop_cols_cpt)) %>%
  left_join(cpt_wide, by = "row_id") %>%
  select(-row_id)
# Step 3: work on antibiot
df <- DR2380 %>% mutate(row_id = row_number())
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
DR2380 <- df %>%
  select(-all_of(drop_cols)) %>%
  left_join(abx_wide, by = "row_id") %>%
  select(-row_id)
# Step 4: work on bioagents
df <- DR2380 %>% mutate(row_id = row_number())
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
DR2380 <- df %>%
  select(-all_of(drop_cols)) %>%
  left_join(bio_wide, by = "row_id") %>%
  select(-row_id)
# Step 5: work on steroids
df <- DR2380 %>% mutate(row_id = row_number())
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
DR2380 <- df %>%
  select(-all_of(drop_cols)) %>%
  left_join(st_wide, by = "row_id") %>%
  select(-row_id)
# Step 6: work on anticoag
df <- DR2380 %>% mutate(row_id = row_number())
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
DR2380 <- df %>%
  select(-all_of(drop_cols)) %>%
  left_join(ac_wide, by = "row_id") %>%
  select(-row_id)

names(DR2380) <- tolower(names(DR2380))
names(DR2380) <- sub("^antibios_",  "antibios:",  names(DR2380))
names(DR2380) <- sub("^bioagents_", "bioagents:", names(DR2380))
names(DR2380) <- sub("^cpt_", "cpt:", names(DR2380))
names(DR2380) <- sub("^icd_", "icd:", names(DR2380))

# aggregate icd to first 3 letters
icd_cols <- grep("^icd:[a-z][0-9]{2}", names(DR2380), value = TRUE)
mat <- as.matrix(DR2380[, icd_cols, drop = FALSE])
mat[is.na(mat)] <- 0
storage.mode(mat) <- "numeric"
icd_roots <- sub("^icd:([a-z][0-9]{2}).*$", "icd:\\1", icd_cols, perl = TRUE)
agg_mat <- t(rowsum(t(mat), group = icd_roots))   # rows: patients, cols: roots
agg_mat <- ifelse(agg_mat > 0, 1L, 0L)
agg_df <- as.data.frame(agg_mat, check.names = FALSE)
DR2380 <- DR2380 %>%
  select(-all_of(icd_cols)) %>%
  bind_cols(agg_df)

DR2380 <- DR2380 %>%
  mutate(
    # parse like 10JAN2024 (day + 3-letter month + year)
    hosp_adm_date   = parse_date_time(hosp_adm_dt,   orders = "d%bY", locale = "C") %>% as.Date(),
    hosp_disch_date = parse_date_time(hosp_disch_dt, orders = "d%bY", locale = "C") %>% as.Date(),
    
    # gap in days (typical LOS = discharge - admit; same-day stay -> 0)
    hosp_gap_days = if_else(
      !is.na(hosp_adm_date) & !is.na(hosp_disch_date) & hosp_disch_date >= hosp_adm_date,
      as.integer(hosp_disch_date - hosp_adm_date),
      NA_integer_
    )
  )

# select covariates
prefixes <- c("cci", "cpt:", "icd:", "antibios:", "bioagents:", "steroids:", "anticoag:")
df_select <- DR2380 %>%
  select("mrn", "sex", "bmi", "smokingstatus_around_surg", "hosp_gap_days", 
         tidyselect::starts_with(prefixes))
med_bmi <- median(df_select$bmi, na.rm = TRUE)
df_select <- df_select %>% mutate(bmi = coalesce(bmi, med_bmi))
df_select[is.na(df_select)] <- 0
df_select <- df_select %>% select(-c('cpt:new'))
df_select <- df_select %>%
  select(where(~ !(is.numeric(.) || is.logical(.)) || any(. != 0, na.rm = TRUE)))
save(df_select, file = "DRcovariate.rds")

