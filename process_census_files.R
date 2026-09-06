# Research Inclusion Barrier Domains constructed from INCLUDE characteristics
# ═══════════════════════════════════════════════════════════════════════════

# 1. Communication
# ────────────────

#  INCLUDE characteristics 
#    • language barriers
#    • people not fluent in the majority language
#    • educational disadvantage
#    • inaccessible information

#  Proxy census variables
#    • age
#    • main language
#    • proficiency in English
#    • qualifications

# 2. Socioeconomic position
# ─────────────────────────

#  INCLUDE characteristics 
#    • socioeconomic disadvantage
#    • unemployment/low income
#    • educational disadvantage
#    • employment

#  Proxy census variables
#    • NS-SeC
#    • occupation
#    • qualifications
#    • tenure

# 3. Physical and practical access
# ────────────────────────────────

#  INCLUDE characteristics 
#    • physical disability
#    • logistical barriers
#    • transport

#  Proxy census variables
#    • age
#    • car availability
#    • disability
#    • general health

# 4. Social, cultural and geographical access
# ───────────────────────────────────────────

#  INCLUDE characteristics 
#    • ethnicity
#    • religion
#    • migrants/new residents
#    • community/cultural factors
#    • intersectionality

#  Proxy census variables
#    • car availability
#    • ethnicity
#    • household composition
#    • length of residence
#    • religion

# 5. Demographic and health inequality
# ────────────────────────────────────

#  INCLUDE characteristics 
#    • systematic under-representation
#    • unequal treatment
#    • differential healthcare/research experience

#  Proxy census variables
#    • age
#    • disability
#    • ethnicity
#    • general Health
#    • sex

# 6. Timing and caring commitments
# ────────────────────────────────

#  INCLUDE characteristics 
#    • carers 
#    • time lost
#    • lost earnings
#    • childcare

#  Proxy census variables
#    • economic activity
#    • household composition
#    • household type
#    • unpaid care



# 0. Load Libraries and Declare Functions ----
# ════════════════════════════════════════════
library(tidyverse)

fnProcessMarginal <- function(filename, var){
  df <- read.csv(filename)
  
  names(df) <- c(paste0(c(rep("AREA", 2), rep(var, 2)), rep(c("_CODE", "_DESC"), 2)), "OBS")
  
  df <- df %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% arrange(AREA_CODE) %>% mutate(across(.cols = c(1:2, 4), .fns = as.factor))

  df[,paste0(var, "_DESC")] <- forcats::fct_reorder(df[,paste0(var, "_DESC")], df[,paste0(var, "_CODE")])

  return(df)
}

fnProcessConstraint <- function(filename, vars){
  df <- read.csv(filename)
  
  names(df) <- c(paste0(c(rep("AREA", 2), rep(vars[1], 2), rep(vars[2], 2)), rep(c("_CODE", "_DESC"), 3)), "OBS")
  
  df <- df %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% arrange(AREA_CODE) %>% mutate(across(.cols = c(1:2, 4, 6), .fns = as.factor))
  
  df[,paste0(vars[1], "_DESC")] <- forcats::fct_reorder(df[,paste0(vars[1], "_DESC")], df[,paste0(vars[1], "_CODE")])
  df[,paste0(vars[2], "_DESC")] <- forcats::fct_reorder(df[,paste0(vars[2], "_DESC")], df[,paste0(vars[2], "_CODE")])
  
  return(df)
}

# 1. Load Data ----
# ════════════════════════════════════════════

# • 1.0. Low Level to High Level Geography Lookups and Units Translations ----
# ────────────────────────────────────────────────────────────────────────────

df_area_lu <- read.csv("data/OA21_LSOA21_MSOA21_LAD22_LU.csv") %>% 
  # English output areas only
  filter(grepl("^E", OA21CD)) %>%
  select(-c(LSOA21NMW, MSOA21NMW, LAD22NMW, ObjectId)) %>% 
  left_join(read.csv("data/OA21_RGN22_LU.csv") %>% 
              select(1:3) %>% 
              rename_with(.fn = ~c("OA21CD", "RGN22CD", "RGN22NM")),
            by = "OA21CD")

# Load translation files
df_car_avail_tx <- fnProcessMarginal(filename = "data/marginals/unit_tx/car_availability_3_msoa.csv", var = "CAR_AVAIL")
df_hhold_comp_tx <- fnProcessMarginal(filename = "data/marginals/unit_tx/hhold_comp_6_msoa.csv", var = "HHOLD_COMP")
df_hhold_type_tx <- fnProcessMarginal(filename = "data/marginals/unit_tx/hhold_type_6_msoa.csv", var = "HHOLD_TYPE")
df_tenure_tx <- fnProcessMarginal(filename = "data/marginals/unit_tx/tenure_5_msoa.csv", var = "TENURE")

# • 1.1. Low Level Geography Marginals ----
# ─────────────────────────────────────────

# • • 1.1.1. Age ----
df_age <- fnProcessMarginal(filename = "data/marginals/age_6_oa.csv", var = "AGE")
# • • 1.1.2. Car availability ----
df_car_avail <- fnProcessMarginal(filename = "data/marginals/hhold_car_availability_3_oa.csv", var = "CAR_AVAIL")
# • • 1.1.3. Disability ----
df_disability <- fnProcessMarginal(filename = "data/marginals/disability_3_oa.csv", var = "DISABILITY")
# • • 1.1.4. Economic activity ----
df_econ_act <- fnProcessMarginal(filename = "data/marginals/economic_activity_10_oa.csv", var = "ECON_ACT")
# • • 1.1.5. Ethnicity ----
df_ethnicity <- fnProcessMarginal(filename = "data/marginals/ethnicity_6_oa.csv", var = "ETHNICITY")
# • • 1.1.6. General health ----
df_health <- fnProcessMarginal(filename = "data/marginals/health_4_oa.csv", var = "HEALTH")
# • • 1.1.7. Household composition ----
df_hhold_comp <- fnProcessMarginal(filename = "data/marginals/hhold_comp_6_oa.csv", var = "HHOLD_COMP")
# • • 1.1.8. Household type ----
df_hhold_type <- fnProcessMarginal(filename = "data/marginals/hhold_type_6_oa.csv", var = "HHOLD_TYPE")
# • • 1.1.9. Length of residence ----
df_resid_length <- fnProcessMarginal(filename = "data/marginals/residence_length_6_oa.csv", var = "RESID_LENGTH")
# • • 1.1.10. Main language ----
df_main_lang <- fnProcessMarginal(filename = "data/marginals/main_lang_11_oa.csv", var = "MAIN_LANG")
# • • 1.1.11. NS-SeC ----
df_nssec <- fnProcessMarginal(filename = "data/marginals/nssec_10_oa.csv", var = "NSSEC")
# • • 1.1.12. Occupation ----
df_occupation <- fnProcessMarginal(filename = "data/marginals/occupation_10_oa.csv", var = "OCCUPATION")
# • • 1.1.13. Proficiency in English ----
df_english_prof <- fnProcessMarginal(filename = "data/marginals/english_prof_4_oa.csv", var = "ENGLISH_PROF")
# • • 1.1.14. Qualifications ----
df_quals <- fnProcessMarginal(filename = "data/marginals/quals_7_oa.csv", var = "QUALS")
# • • 1.1.15. Religion ----
df_religion <- fnProcessMarginal(filename = "data/marginals/religion_10_oa.csv", var = "RELIGION")
# • • 1.1.16. Sex ----
df_sex <- fnProcessMarginal(filename = "data/marginals/sex_2_oa.csv", var = "SEX")
# • • 1.1.17. Tenure ----
df_tenure <- fnProcessMarginal(filename = "data/marginals/hhold_tenure_5_oa.csv", var = "TENURE")
# • • 1.1.18. Unpaid care ----
df_unpaid_care <- fnProcessMarginal(filename = "data/marginals/unpaid_care_5_oa.csv", var = "UNPAID_CARE")


# • 1.2. High Level Geography Constraints by Research Inclusion Barrier Domain ----
# ─────────────────────────────────────────────────────────────────────────────────

# • • 1.2.1. Communication ----
# age x main language
df_age_main_lang_rgn <- fnProcessConstraint(filename = "data/constraints/age_6_main_lang_11_rgn.csv", vars = c("AGE", "MAIN_LANG"))
#    • age x proficiency in English
df_age_english_prof_msoa <- fnProcessConstraint(filename = "data/constraints/age_6_english_prof_4_msoa.csv", vars = c("AGE", "ENGLISH_PROF"))
#    • age x qualifications
df_age_quals_msoa <- fnProcessConstraint(filename = "data/constraints/age_6_qual_7_msoa.csv", vars = c("AGE", "QUALS"))
#    • main language x proficiency in English
df_main_lang_english_prof_rgn <- fnProcessConstraint(filename = "data/constraints/main_lang_11_english_prof_4_rgn.csv", vars = c("MAIN_LANG", "ENGLISH_PROF"))
#    • main language x qualifications
df_main_lang_quals_rgn <- fnProcessConstraint(filename = "data/constraints/main_lang_11_quals_7_rgn.csv", vars = c("MAIN_LANG", "QUALS"))
#    • proficiency in English x qualifications
df_english_prof_quals_msoa <- fnProcessConstraint(filename = "data/constraints/english_prof_4_quals_7_msoa.csv", vars = c("ENGLISH_PROF", "QUALS"))

# • • 1.2.2. Socioeconomic position ----
#    • NS-SeC x occupation
df_nssec_occupation_msoa <- fnProcessConstraint(filename = "data/constraints/nssec_10_occupation_10_msoa.csv", vars = c("NSSEC", "OCCUPATION"))
#    • NS-SeC x qualifications
df_nssec_quals_msoa <- fnProcessConstraint(filename = "data/constraints/nssec_10_quals_7_msoa.csv", vars = c("NSSEC", "TENURE"))
#    • NS-SeC x tenure
df_nnsec_tenure_msoa <- fnProcessConstraint(filename = "data/constraints/nssec_10_tenure_5_msoa.csv", vars = c("NSSEC", "QUALS"))
#    • occupation x qualifications
df_occupation_quals_msoa <- fnProcessConstraint(filename = "data/constraints/occupation_10_quals_7_msoa.csv", vars = c("OCCUPATION", "TENURE"))
#    • occupation x tenure (HHOLD)
df_occupation_tenure_msoa <- fnProcessConstraint(filename = "data/constraints/occupation_10_tenure_5_msoa.csv", vars = c("OCCUPATION", "QUALS"))
#    • qualifications x tenure (HHOLD)
df_quals_tenure_msoa <- fnProcessConstraint(filename = "data/constraints/quals_7_tenure_5_msoa.csv", vars = c("QUALS", "TENURE"))

# • • 1.2.3. Physical and practical access ----
#    • age x car availability (HHOLD)
df_age_car_avail_msoa <- fnProcessConstraint(filename = "data/constraints/age_6_car_availability_3_msoa.csv", vars = c("AGE", "CAR_AVAIL"))
#    • age x disability
df_age_disability_msoa <- fnProcessConstraint(filename = "data/constraints/age_6_disability_3_msoa.csv", vars = c("AGE", "DISABILITY"))
#    • age x general health
df_age_health_msoa <- fnProcessConstraint(filename = "data/constraints/age_6_health_4_msoa.csv", vars = c("AGE", "HEALTH"))
#    • car availability x disability (HHOLD)
df_car_avail_disability_msoa <- fnProcessConstraint(filename = "data/constraints/car_availability_3_disability_3_msoa.csv", vars = c("CAR_AVAIL", "DISABILITY"))
#    • car availability x general health (HHOLD)
df_car_avail_health_msoa <- fnProcessConstraint(filename = "data/constraints/car_availability_3_health_4_msoa.csv", vars = c("CAR_AVAIL", "HEALTH"))
#    • disability x general health
df_disability_health_msoa <- fnProcessConstraint(filename = "data/constraints/disability_3_health_4_msoa.csv", vars = c("DISABILITY", "HEALTH"))

# • • 1.2.4. Social, cultural and geographical access ----
#    • car availability x ethnicity (HHOLD)
df_car_avail_ethnicity_msoa <- fnProcessConstraint(filename = "data/constraints/car_availability_3_ethnicity_6_msoa.csv", vars = c("CAR_AVAIL", "ETHNICITY"))
#    • car availability x household composition (HHOLD)
df_car_avail_hhold_comp_msoa <- fnProcessConstraint(filename = "data/constraints/car_availability_3_hhold_comp_6_msoa.csv", vars = c("CAR_AVAIL", "HHOLD_COMP"))
#    • car availability x length of residence (HHOLD)
df_car_avail_resid_length_msoa <- fnProcessConstraint(filename = "data/constraints/car_availability_3_residence_length_6_msoa.csv", vars = c("CAR_AVAIL", "RESID_LENGTH"))
#    • car availability x religion (HHOLD)
df_car_avail_religion_lad <- fnProcessConstraint(filename = "data/constraints/car_availability_3_religion_10_lad.csv", vars = c("CAR_AVAIL", "RELIGION"))
#    • ethnicity x household composition (HHOLD)
df_ethnicity_hhold_comp_msoa <- fnProcessConstraint(filename = "data/constraints/ethnicity_6_hhold_comp_6_msoa.csv", vars = c("ETHNICITY", "HHOLD_COMP"))
#    • ethnicity x length of residence
df_ethnicity_resid_length_msoa <- fnProcessConstraint(filename = "data/constraints/ethnicity_6_residence_length_6_msoa_INCOMPLETE.csv", vars = c("ETHNICITY", "RESID_LENGTH"))
#    • ethnicity x religion
df_ethnicity_religion_msoa <- fnProcessConstraint(filename = "data/constraints/ethnicity_6_religion_10_msoa_INCOMPLETE.csv", vars = c("ETHNICITY", "RELIGION"))
#    • household composition x length of residence (HHOLD)
df_hhold_comp_resid_length_msoa <- fnProcessConstraint(filename = "data/constraints/hhold_comp_6_residence_length_6_msoa.csv", vars = c("HHOLD_COMP", "RESID_LENGTH"))
#    • household composition x religion (HHOLD)
df_hhold_comp_religion_lad <- fnProcessConstraint(filename = "data/constraints/hhold_comp_6_religion_10_lad.csv", vars = c("HHOLD_COMP", "RELIGION"))
#    • length of residence x religion
df_resid_length_religion_msoa<- fnProcessConstraint(filename = "data/constraints/residence_length_6_religion_10_msoa_INCOMPLETE.csv", vars = c("RESID_LENGTH", "RELIGION"))

# • • 1.2.5. Demographic and health inequality ----
#    • age x disability (ALREADY LOADED)
#    • age x ethnicity
df_age_ethnicity_msoa <- fnProcessConstraint(filename = "data/constraints/age_6_ethnicity_6_msoa.csv", vars = c("AGE", "ETHNICITY"))
#    • age x general health (ALREADY LOADED)
#    • age x sex
df_age_sex_msoa <- fnProcessConstraint(filename = "data/constraints/age_6_sex_2_msoa.csv", vars = c("AGE", "SEX"))
#    • disability x ethnicity
df_disability_ethnicity_msoa <- fnProcessConstraint(filename = "data/constraints/disability_3_ethnicity_6_msoa.csv", vars = c("DISABILITY", "ETHNICITY"))
#    • disability x general health (ALREADY LOADED)
#    • disability x sex
df_disability_sex_msoa <- fnProcessConstraint(filename = "data/constraints/disability_3_sex_2_msoa.csv", vars = c("DISABILITY", "SEX"))
#    • ethnicity x general health
df_ethnicity_health_msoa <- fnProcessConstraint(filename = "data/constraints/ethnicity_6_health_4_msoa.csv", vars = c("ETHNICITY", "HEALTH"))
#    • ethnicity x sex
df_ethnicity_sex_msoa <- fnProcessConstraint(filename = "data/constraints/ethnicity_6_sex_2_msoa.csv", vars = c("ETHNICITY", "SEX"))
#    • general health x sex
df_health_sex_msoa <- fnProcessConstraint(filename = "data/constraints/health_4_sex_2_msoa.csv", vars = c("HEALTH", "SEX"))

# • • 1.2.6. Timing and caring commitments ----
#    • economic activity x household composition (HHOLD)
df_econ_act_hhold_comp_msoa <- fnProcessConstraint(filename = "data/constraints/economic_activity_10_hhold_comp_6_msoa.csv", vars = c("ECON_ACT", "HHOLD_COMP"))
#    • economic activity x household type (HHOLD)
df_econ_act_hhold_type_msoa <- fnProcessConstraint(filename = "data/constraints/economic_activity_10_hhold_type_6_msoa.csv", vars = c("ECON_ACT", "HHOLD_TYPE"))
#    • economic activity x unpaid care
df_econ_act_unpaid_care_msoa <- fnProcessConstraint(filename = "data/constraints/economic_activity_10_unpaid_care_5_msoa.csv", vars = c("ECON_ACT", "UNPAID_CARE"))
#    • household composition x household type (HHOLD)
df_hhold_comp_hhold_type_msoa <- fnProcessConstraint(filename = "data/constraints/hhold_comp_6_hhold_type_6_msoa.csv", vars = c("HHOLD_COMP", "HHOLD_TYPE"))
#    • household composition x unpaid care (HHOLD)
df_hhold_comp_unpaid_care_msoa<- fnProcessConstraint(filename = "data/constraints/hhold_comp_6_unpaid_care_5_msoa.csv", vars = c("HHOLD_COMP", "UNPAID_CARE"))
#    • household type x unpaid care (HHOLD)
df_hhold_type_unpaid_care_msoa <- fnProcessConstraint(filename = "data/constraints/hhold_type_6_unpaid_care_5_msoa.csv", vars = c("HHOLD_TYPE", "UNPAID_CARE"))

# 2. Process Data ----
# ════════════════════

# • 2.1. Convert household marginals to persons ----
# ──────────────────────────────────────────────────

# Calculate the persons per household data at MSOA level
df_car_avail_tx <- df_car_avail_tx %>% 
  left_join(
    df_car_avail %>% 
      left_join(df_area_lu, by = c("AREA_CODE" = "OA21CD")) %>% 
      group_by(MSOA21CD, CAR_AVAIL_DESC) %>%
      summarise(HHOLDS = sum(OBS), .groups = "keep") %>%
      ungroup(),
    by = c("AREA_CODE" = "MSOA21CD", "CAR_AVAIL_DESC")) %>%
  mutate(PERSONS_PER_HHOLD = if_else(HHOLDS==0, 0, OBS / HHOLDS),
         MSOA21CD = AREA_CODE) %>%
  left_join(df_area_lu %>% select(OA21CD, MSOA21CD), by = "MSOA21CD", relationship = "many-to-many") %>%
  select(OA21CD, MSOA21CD, CAR_AVAIL_DESC, PERSONS_PER_HHOLD)

df_hhold_comp_tx <- df_hhold_comp_tx %>% 
  left_join(
    df_hhold_comp %>% 
      left_join(df_area_lu, by = c("AREA_CODE" = "OA21CD")) %>% 
      group_by(MSOA21CD, HHOLD_COMP_DESC) %>%
      summarise(HHOLDS = sum(OBS), .groups = "keep") %>%
      ungroup(),
    by = c("AREA_CODE" = "MSOA21CD", "HHOLD_COMP_DESC")) %>%
  mutate(PERSONS_PER_HHOLD = if_else(HHOLDS==0, 0, OBS / HHOLDS),
         MSOA21CD = AREA_CODE) %>%
  left_join(df_area_lu %>% select(OA21CD, MSOA21CD), by = "MSOA21CD", relationship = "many-to-many") %>%
  select(OA21CD, MSOA21CD, HHOLD_COMP_DESC, PERSONS_PER_HHOLD)

df_hhold_type_tx <- df_hhold_type_tx %>% 
  left_join(
    df_hhold_type %>% 
      left_join(df_area_lu, by = c("AREA_CODE" = "OA21CD")) %>% 
      group_by(MSOA21CD, HHOLD_TYPE_DESC) %>%
      summarise(HHOLDS = sum(OBS), .groups = "keep") %>%
      ungroup(),
    by = c("AREA_CODE" = "MSOA21CD", "HHOLD_TYPE_DESC")) %>%
  mutate(PERSONS_PER_HHOLD = if_else(HHOLDS==0, 0, OBS / HHOLDS),
         MSOA21CD = AREA_CODE) %>%
  left_join(df_area_lu %>% select(OA21CD, MSOA21CD), by = "MSOA21CD", relationship = "many-to-many") %>%
  select(OA21CD, HHOLD_TYPE_DESC, PERSONS_PER_HHOLD)

df_tenure_tx <- df_tenure_tx %>% 
  left_join(
    df_tenure %>% 
      left_join(df_area_lu, by = c("AREA_CODE" = "OA21CD")) %>% 
      group_by(MSOA21CD, TENURE_DESC) %>%
      summarise(HHOLDS = sum(OBS), .groups = "keep") %>%
      ungroup(),
    by = c("AREA_CODE" = "MSOA21CD", "TENURE_DESC")) %>%
  mutate(PERSONS_PER_HHOLD = if_else(HHOLDS==0, 0, OBS / HHOLDS),
         MSOA21CD = AREA_CODE) %>%
  left_join(df_area_lu %>% select(OA21CD, MSOA21CD), by = "MSOA21CD", relationship = "many-to-many") %>%
  select(OA21CD, MSOA21CD, TENURE_DESC, PERSONS_PER_HHOLD)

# Apply persons per household to OA level data
df_car_avail <- df_car_avail %>% 
  left_join(df_car_avail_tx, by = c("AREA_CODE" = "OA21CD", "CAR_AVAIL_DESC")) %>%
  mutate(AREA_CODE, AREA_DESC, CAR_AVAIL_CODE, CAR_AVAIL_DESC, OBS = round(OBS*PERSONS_PER_HHOLD, 0), .keep = "none")

df_hhold_comp <- df_hhold_comp %>% 
  left_join(df_hhold_comp_tx, by = c("AREA_CODE" = "OA21CD", "HHOLD_COMP_DESC")) %>%
  mutate(AREA_CODE, AREA_DESC, HHOLD_COMP_CODE, HHOLD_COMP_DESC, OBS = round(OBS*PERSONS_PER_HHOLD, 0), .keep = "none")

df_hhold_type <- df_hhold_type %>% 
  left_join(df_hhold_type_tx, by = c("AREA_CODE" = "OA21CD", "HHOLD_TYPE_DESC")) %>%
  mutate(AREA_CODE, AREA_DESC, HHOLD_TYPE_CODE, HHOLD_TYPE_DESC, OBS = round(OBS*PERSONS_PER_HHOLD, 0), .keep = "none")

df_tenure <- df_tenure %>% 
  left_join(df_tenure_tx, by = c("AREA_CODE" = "OA21CD", "TENURE_DESC")) %>%
  mutate(AREA_CODE, AREA_DESC, TENURE_CODE, TENURE_DESC, OBS = round(OBS*PERSONS_PER_HHOLD, 0), .keep = "none")

# • 2.2. Convert constraints into MSOA level and deal with missing MSOAs ----
# ───────────────────────────────────────────────────────────────────────────

df_area_lu %>% dplyr::filter(grepl("^E", OA21CD)) %>% distinct(MSOA21CD) %>% NROW()
df_area_lu %>% dplyr::filter(grepl("^E", OA21CD)) %>% distinct(RGN22CD) %>% NROW()

df_age_main_lang_rgn %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
df_main_lang_english_prof_rgn %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
df_main_lang_quals_rgn %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()

df_age_english_prof_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
df_age_quals_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()

df_missing <- df_english_prof_quals_msoa %>% 
  left_join(df_area_lu %>% distinct(MSOA21CD, LAD22CD), by = c("AREA_CODE" = "MSOA21CD")) %>% 
  group_by(LAD22CD, ENGLISH_PROF_CODE, ENGLISH_PROF_DESC, QUALS_CODE, QUALS_DESC) %>%
  summarise(OBS = sum(OBS), .groups = "keep") %>% 
  ungroup() %>%
  group_by(LAD22CD) %>%
  mutate(PCT = OBS / sum(OBS)) %>%
  ungroup() %>% 
  select(-OBS) %>%
  inner_join(
    df_area_lu %>% distinct(MSOA21CD, LAD22CD) %>% dplyr::filter(grepl("^E", MSOA21CD)) %>%
      anti_join(df_english_prof_quals_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE), by = c("MSOA21CD" = "AREA_CODE")),
    by = "LAD22CD", relationship = "many-to-many") %>%
  left_join(df_age %>% 
              left_join(df_area_lu, by = c("AREA_CODE" = "OA21CD")) %>% 
              group_by(MSOA21CD) %>%
              summarise(OBS = sum(OBS)) %>% 
              ungroup(),
            by = "MSOA21CD")

names(df_missing)

df_english_prof_quals_msoa <- df_english_prof_quals_msoa %>% 
  bind_rows()

df_age_english_prof_msoa %>% dplyr::filter(AREA_CODE=="E02003169") %>% group_by(AREA_CODE) %>% summarise(OBS = sum(OBS))
df_english_prof_quals_msoa %>% dplyr::filter(AREA_CODE=="E02003169")

  

# 2. Iterative Proportional Fitting ----
# ══════════════════════════════════════

# 2.1. Domain 1: Communication ----
# ─────────────────────────────────

# Constraints
# age x main language
df_age_main_lang_rgn <- fnProcessConstraint(filename = "data/constraints/age_6_main_lang_11_rgn.csv", vars = c("AGE", "MAIN_LANG"))
#    • age x proficiency in English
df_age_english_prof_msoa <- fnProcessConstraint(filename = "data/constraints/age_6_english_prof_4_msoa.csv", vars = c("AGE", "ENGLISH_PROF"))
#    • age x qualifications
df_age_quals_msoa <- fnProcessConstraint(filename = "data/constraints/age_6_qual_7_msoa.csv", vars = c("AGE", "QUALS"))
#    • main language x proficiency in English
df_main_lang_english_prof_rgn <- fnProcessConstraint(filename = "data/constraints/main_lang_11_english_prof_4_rgn.csv", vars = c("MAIN_LANG", "ENGLISH_PROF"))
#    • main language x qualifications
df_main_lang_quals_rgn <- fnProcessConstraint(filename = "data/constraints/main_lang_11_quals_7_rgn.csv", vars = c("MAIN_LANG", "QUALS"))
#    • proficiency in English x qualifications
df_english_prof_quals_msoa <- fnProcessConstraint(filename = "data/constraints/english_prof_4_quals_7_msoa.csv", vars = c("ENGLISH_PROF", "QUALS"))

#  Proxy census variables
#    • age
#    • main language
#    • proficiency in English
#    • qualifications


