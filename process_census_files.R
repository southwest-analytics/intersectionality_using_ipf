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
library(mipfp)

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

fnD1_CreateSeed <- function(){
  df_seed <- expand_grid(AGE_CODE = df_code_lookup %>% dplyr::filter(VAR == "AGE") %>% .$CODE,
                         MAIN_LANG_CODE = df_code_lookup %>% dplyr::filter(VAR == "MAIN_LANG") %>% .$CODE,
                         ENGLISH_PROF_CODE = df_code_lookup %>% dplyr::filter(VAR == "ENGLISH_PROF") %>% .$CODE,
                         QUALS_CODE = df_code_lookup %>% dplyr::filter(VAR == "QUALS") %>% .$CODE) %>%
    left_join(df_code_lookup %>% dplyr::filter(VAR == "AGE") %>% mutate(AGE_CODE = CODE, P_AGE = P, .keep = "none"), by = c("AGE_CODE")) %>%
    left_join(df_code_lookup %>% dplyr::filter(VAR == "MAIN_LANG") %>% mutate(MAIN_LANG_CODE = CODE, P_MAIN_LANG = P, .keep = "none"), by = c("MAIN_LANG_CODE")) %>%
    left_join(df_code_lookup %>% dplyr::filter(VAR == "ENGLISH_PROF") %>% mutate(ENGLISH_PROF_CODE = CODE, P_ENGLISH_PROF = P, .keep = "none"), by = c("ENGLISH_PROF_CODE")) %>%
    left_join(df_code_lookup %>% dplyr::filter(VAR == "QUALS") %>% mutate(QUALS_CODE = CODE, P_QUALS = P, .keep = "none"), by = c("QUALS_CODE")) %>%
    mutate(P_SEED = P_AGE * P_MAIN_LANG * P_ENGLISH_PROF * P_QUALS)
  
  seed <- xtabs(
    P_SEED ~ AGE_CODE + MAIN_LANG_CODE + ENGLISH_PROF_CODE + QUALS_CODE,
    data = df_seed
  )
  
  return(seed)
}


msoa21cd <- "E02004152"
fnD1_BalanceHighLevel(msoa21cd, seed)

fnD1_BalanceHighLevel <- function(msoa21cd, seed){
  tgt_age_main_lang_msoa <- df_age_main_lang_msoa %>%
    dplyr::filter(AREA_CODE == msoa21cd) %>%
    select(AGE_CODE, MAIN_LANG_CODE, P) %>%
    tidyr::pivot_wider(names_from = MAIN_LANG_CODE, values_from = P) %>%
    column_to_rownames(var = "AGE_CODE") %>%
    as.matrix()
  
  tgt_age_english_prof_msoa <- df_age_english_prof_msoa %>%
    dplyr::filter(AREA_CODE == msoa21cd) %>%
    select(AGE_CODE, ENGLISH_PROF_CODE, P) %>%
    tidyr::pivot_wider(names_from = ENGLISH_PROF_CODE, values_from = P) %>%
    column_to_rownames(var = "AGE_CODE") %>%
    as.matrix()
  
  tgt_age_quals_msoa <- df_age_quals_msoa %>%
    dplyr::filter(AREA_CODE == msoa21cd) %>%
    select(AGE_CODE, QUALS_CODE, P) %>%
    tidyr::pivot_wider(names_from = QUALS_CODE, values_from = P) %>%
    column_to_rownames(var = "AGE_CODE") %>%
    as.matrix()
  
  tgt_main_lang_english_prof_msoa <- df_main_lang_english_prof_msoa %>%
    dplyr::filter(AREA_CODE == msoa21cd) %>%
    select(MAIN_LANG_CODE, ENGLISH_PROF_CODE, P) %>%
    tidyr::pivot_wider(names_from = ENGLISH_PROF_CODE, values_from = P) %>%
    column_to_rownames(var = "MAIN_LANG_CODE") %>%
    as.matrix()
  
  tgt_main_lang_quals_msoa <- df_main_lang_quals_msoa %>%
    dplyr::filter(AREA_CODE == msoa21cd) %>%
    select(MAIN_LANG_CODE, QUALS_CODE, P) %>%
    tidyr::pivot_wider(names_from = QUALS_CODE, values_from = P) %>%
    column_to_rownames(var = "MAIN_LANG_CODE") %>%
    as.matrix()
  
  tgt_english_prof_quals_msoa <- df_english_prof_quals_msoa %>%
    dplyr::filter(AREA_CODE == msoa21cd) %>%
    select(ENGLISH_PROF_CODE, QUALS_CODE, P) %>%
    tidyr::pivot_wider(names_from = QUALS_CODE, values_from = P) %>%
    column_to_rownames(var = "ENGLISH_PROF_CODE") %>%
    as.matrix()
  
  target_list <- list(
    c(1, 2), # Age x Main language
    c(1, 3), # Age x English proficiency
    c(1, 4), # Age x Qualifications
    c(2, 3), # Main language x English proficiency
    c(2, 4), # Main language x Qualifications
    c(3, 4) # English proficiency x Qualifications
  )
  
  target_data <- list(
    tgt_age_main_lang_msoa,
    tgt_age_english_prof_msoa,
    tgt_age_quals_msoa,
    tgt_main_lang_english_prof_msoa,
    tgt_main_lang_quals_msoa,
    tgt_english_prof_quals_msoa
  )
  
  # Run the ipf for the high level geography
  ipf_high_level <- mipfp::Ipfp(
    seed = seed,
    target.list = target_list,
    target.data = target_data,
    print = TRUE,
    iter = 1000,
    tol = 1e-10,
    tol.margins = 1e-10
  )

  browser()
  
  oa_list <- df_area_lu %>% dplyr::filter(MSOA21CD == msoa21cd) %>% distinct(OA21CD) %>% .$OA21CD
  res <- do.call("rbind", lapply(oa_list, fnD1_BalanceLowLevel, ipf_high_level, df_popn))
  
  df_ipf_high_level <- ipf_high_level$p.hat %>% 
    as.data.frame() %>%
    mutate(AREA_CODE = msoa21cd, .before = 1) %>%
    rename(c(P = "Freq")) %>%
    left_join(df_popn %>% select(AREA_CODE, OBS) %>%
                left_join(df_area_lu %>% select(OA21CD, MSOA21CD, MSOA21NM), by = c("AREA_CODE" = "OA21CD")) %>%
                group_by(MSOA21CD, MSOA21NM) %>%
                summarise(OBS = sum(OBS), .groups = "keep") %>% 
                ungroup() %>% 
                rename_with(.fn = ~c("AREA_CODE", "AREA_DESC", "OBS")),
              by = "AREA_CODE") %>%
    left_join(df_code_lookup %>% dplyr::filter(VAR=="AGE") %>% mutate(AGE_CODE = as.factor(CODE), AGE_DESC = DESC, .keep = "none"), by = "AGE_CODE") %>%
    left_join(df_code_lookup %>% dplyr::filter(VAR=="MAIN_LANG") %>% mutate(MAIN_LANG_CODE = as.factor(CODE), MAIN_LANG_DESC = DESC, .keep = "none"), by = "MAIN_LANG_CODE") %>%
    left_join(df_code_lookup %>% dplyr::filter(VAR=="ENGLISH_PROF") %>% mutate(ENGLISH_PROF_CODE = as.factor(CODE), ENGLISH_PROF_DESC = DESC, .keep = "none"), by = "ENGLISH_PROF_CODE") %>%
    left_join(df_code_lookup %>% dplyr::filter(VAR=="QUALS") %>% mutate(QUALS_CODE = as.factor(CODE), QUALS_DESC = DESC, .keep = "none"), by = "QUALS_CODE") %>%
    select(AREA_CODE, AGE_CODE, AGE_DESC, MAIN_LANG_CODE, MAIN_LANG_DESC, ENGLISH_PROF_CODE, ENGLISH_PROF_DESC, QUALS_CODE, QUALS_DESC, OBS, P)

  return(list(hi = df_ipf_high_level, lo = res))  
}

oa21cd <- oa_list[2]

# PROBLEM E00101203 ----
fnD1_BalanceLowLevel <- function(oa21cd, ipf_high_level, df_popn){
  age <- df_age %>% dplyr::filter(AREA_CODE == oa21cd) %>% .$P
  main_lang <- df_main_lang %>% dplyr::filter(AREA_CODE == oa21cd) %>% .$P
  english_prof <- df_english_prof %>% dplyr::filter(AREA_CODE == oa21cd) %>% .$P
  quals <- df_quals %>% dplyr::filter(AREA_CODE == oa21cd) %>% .$P
  
  ipf_low_level <- mipfp::Ipfp(
    seed = ipf_high_level$p.hat,
    target.list = list(1, 2, 3, 4),
    target.data = list(age, main_lang, english_prof, quals),
    iter = 1000,
    tol = 1e-10,
    tol.margins = 1e-6
  )
  
  df_ipf_low_level <- ipf_low_level$p.hat %>%
    as.data.frame() %>% 
    mutate(AREA_CODE = oa21cd, P = Freq) %>%
    select(AREA_CODE, AGE_CODE, MAIN_LANG_CODE, ENGLISH_PROF_CODE, QUALS_CODE, P) %>%
    left_join(df_popn, by = "AREA_CODE") %>%
    mutate(OBS = P * OBS) %>%
    left_join(df_code_lookup %>% dplyr::filter(VAR=="AGE") %>% mutate(AGE_CODE = as.factor(CODE), AGE_DESC = DESC, .keep = "none"), by = "AGE_CODE") %>%
    left_join(df_code_lookup %>% dplyr::filter(VAR=="MAIN_LANG") %>% mutate(MAIN_LANG_CODE = as.factor(CODE), MAIN_LANG_DESC = DESC, .keep = "none"), by = "MAIN_LANG_CODE") %>%
    left_join(df_code_lookup %>% dplyr::filter(VAR=="ENGLISH_PROF") %>% mutate(ENGLISH_PROF_CODE = as.factor(CODE), ENGLISH_PROF_DESC = DESC, .keep = "none"), by = "ENGLISH_PROF_CODE") %>%
    left_join(df_code_lookup %>% dplyr::filter(VAR=="QUALS") %>% mutate(QUALS_CODE = as.factor(CODE), QUALS_DESC = DESC, .keep = "none"), by = "QUALS_CODE") %>%
    select(AREA_CODE, AGE_CODE, AGE_DESC, MAIN_LANG_CODE, MAIN_LANG_DESC, ENGLISH_PROF_CODE, ENGLISH_PROF_DESC, QUALS_CODE, QUALS_DESC, OBS, P)
    
    return(df_ipf_low_level)
}

# 1. Load Data ----
# ════════════════════════════════════════════

# • 1.0. Low Level to High Level Geography Lookups and Units Translations ----
# ────────────────────────────────────────────────────────────────────────────

df_area_lu <- read.csv("data/lookups/OA21_LSOA21_MSOA21_LAD22_LU.csv") %>% 
  # English output areas only
  filter(grepl("^E", OA21CD)) %>%
  select(-c(LSOA21NMW, MSOA21NMW, LAD22NMW, ObjectId)) %>% 
  left_join(read.csv("data/lookups/OA21_RGN22_LU.csv") %>% 
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

# • • 1.1.0. Population (from Age) ----
df_popn <- fnProcessMarginal(filename = "data/marginals/age_6_oa.csv", var = "AGE") %>%
  group_by(AREA_CODE, AREA_DESC) %>% summarise(OBS = sum(OBS)) %>% ungroup()
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
df_english_prof_quals_msoa <- fnProcessConstraint(filename = "data/constraints/english_prof_4_quals_7_msoa.csv", vars = c("QUALS", "ENGLISH_PROF")) %>% 
  # Wrong order from census download (OCD kicking in)
  select(AREA_CODE, AREA_DESC, ENGLISH_PROF_CODE, ENGLISH_PROF_DESC, QUALS_CODE, QUALS_DESC, OBS)

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

# • 2.0. Create Code Lookup with National Proportions ----
# ────────────────────────────────────────────────────────

df_code_lookup <- df_age %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% group_by(AGE_CODE, AGE_DESC) %>% summarise(OBS = sum(OBS), .groups = "keep") %>% ungroup() %>% rename_with(.fn = ~c("CODE", "DESC", "OBS")) %>% mutate(VAR = "AGE", .before = "CODE") %>% mutate(P = OBS/sum(OBS)) %>%
  bind_rows(df_car_avail %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% group_by(CAR_AVAIL_CODE, CAR_AVAIL_DESC) %>% summarise(OBS = sum(OBS), .groups = "keep") %>% ungroup() %>% rename_with(.fn = ~c("CODE", "DESC", "OBS")) %>% mutate(VAR = "CAR_AVAIL", .before = "CODE") %>% mutate(P = OBS/sum(OBS))) %>%
  bind_rows(df_disability %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% group_by(DISABILITY_CODE, DISABILITY_DESC) %>% summarise(OBS = sum(OBS), .groups = "keep") %>% ungroup() %>% rename_with(.fn = ~c("CODE", "DESC", "OBS")) %>% mutate(VAR = "DISABILITY", .before = "CODE") %>% mutate(P = OBS/sum(OBS))) %>%
  bind_rows(df_econ_act %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% group_by(ECON_ACT_CODE, ECON_ACT_DESC) %>% summarise(OBS = sum(OBS), .groups = "keep") %>% ungroup() %>% rename_with(.fn = ~c("CODE", "DESC", "OBS")) %>% mutate(VAR = "ECON_ACT", .before = "CODE") %>% mutate(P = OBS/sum(OBS))) %>%
  bind_rows(df_ethnicity %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% group_by(ETHNICITY_CODE, ETHNICITY_DESC) %>% summarise(OBS = sum(OBS), .groups = "keep") %>% ungroup() %>% rename_with(.fn = ~c("CODE", "DESC", "OBS")) %>% mutate(VAR = "ETHNICITY", .before = "CODE") %>% mutate(P = OBS/sum(OBS))) %>%
  bind_rows(df_health %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% group_by(HEALTH_CODE, HEALTH_DESC) %>% summarise(OBS = sum(OBS), .groups = "keep") %>% ungroup() %>% rename_with(.fn = ~c("CODE", "DESC", "OBS")) %>% mutate(VAR = "HEALTH", .before = "CODE") %>% mutate(P = OBS/sum(OBS))) %>%
  bind_rows(df_hhold_comp %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% group_by(HHOLD_COMP_CODE, HHOLD_COMP_DESC) %>% summarise(OBS = sum(OBS), .groups = "keep") %>% ungroup() %>% rename_with(.fn = ~c("CODE", "DESC", "OBS")) %>% mutate(VAR = "HHOLD_COMP", .before = "CODE") %>% mutate(P = OBS/sum(OBS))) %>%
  bind_rows(df_hhold_type %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% group_by(HHOLD_TYPE_CODE, HHOLD_TYPE_DESC) %>% summarise(OBS = sum(OBS), .groups = "keep") %>% ungroup() %>% rename_with(.fn = ~c("CODE", "DESC", "OBS")) %>% mutate(VAR = "HHOLD_TYPE", .before = "CODE") %>% mutate(P = OBS/sum(OBS))) %>%
  bind_rows(df_resid_length %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% group_by(RESID_LENGTH_CODE, RESID_LENGTH_DESC) %>% summarise(OBS = sum(OBS), .groups = "keep") %>% ungroup() %>% rename_with(.fn = ~c("CODE", "DESC", "OBS")) %>% mutate(VAR = "RESID_LENGTH", .before = "CODE") %>% mutate(P = OBS/sum(OBS))) %>%
  bind_rows(df_main_lang %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% group_by(MAIN_LANG_CODE, MAIN_LANG_DESC) %>% summarise(OBS = sum(OBS), .groups = "keep") %>% ungroup() %>% rename_with(.fn = ~c("CODE", "DESC", "OBS")) %>% mutate(VAR = "MAIN_LANG", .before = "CODE") %>% mutate(P = OBS/sum(OBS))) %>%
  bind_rows(df_nssec %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% group_by(NSSEC_CODE, NSSEC_DESC) %>% summarise(OBS = sum(OBS), .groups = "keep") %>% ungroup() %>% rename_with(.fn = ~c("CODE", "DESC", "OBS")) %>% mutate(VAR = "NSSEC", .before = "CODE") %>% mutate(P = OBS/sum(OBS))) %>%
  bind_rows(df_occupation %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% group_by(OCCUPATION_CODE, OCCUPATION_DESC) %>% summarise(OBS = sum(OBS), .groups = "keep") %>% ungroup() %>% rename_with(.fn = ~c("CODE", "DESC", "OBS")) %>% mutate(VAR = "OCCUPATION", .before = "CODE") %>% mutate(P = OBS/sum(OBS))) %>%
  bind_rows(df_english_prof %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% group_by(ENGLISH_PROF_CODE, ENGLISH_PROF_DESC) %>% summarise(OBS = sum(OBS), .groups = "keep") %>% ungroup() %>% rename_with(.fn = ~c("CODE", "DESC", "OBS")) %>% mutate(VAR = "ENGLISH_PROF", .before = "CODE") %>% mutate(P = OBS/sum(OBS))) %>%
  bind_rows(df_quals %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% group_by(QUALS_CODE, QUALS_DESC) %>% summarise(OBS = sum(OBS), .groups = "keep") %>% ungroup() %>% rename_with(.fn = ~c("CODE", "DESC", "OBS")) %>% mutate(VAR = "QUALS", .before = "CODE") %>% mutate(P = OBS/sum(OBS))) %>%
  bind_rows(df_religion %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% group_by(RELIGION_CODE, RELIGION_DESC) %>% summarise(OBS = sum(OBS), .groups = "keep") %>% ungroup() %>% rename_with(.fn = ~c("CODE", "DESC", "OBS")) %>% mutate(VAR = "RELIGION", .before = "CODE") %>% mutate(P = OBS/sum(OBS))) %>%
  bind_rows(df_sex %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% group_by(SEX_CODE, SEX_DESC) %>% summarise(OBS = sum(OBS), .groups = "keep") %>% ungroup() %>% rename_with(.fn = ~c("CODE", "DESC", "OBS")) %>% mutate(VAR = "SEX", .before = "CODE") %>% mutate(P = OBS/sum(OBS))) %>%
  bind_rows(df_tenure %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% group_by(TENURE_CODE, TENURE_DESC) %>% summarise(OBS = sum(OBS), .groups = "keep") %>% ungroup() %>% rename_with(.fn = ~c("CODE", "DESC", "OBS")) %>% mutate(VAR = "TENURE", .before = "CODE") %>% mutate(P = OBS/sum(OBS))) %>%
  bind_rows(df_unpaid_care %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% group_by(UNPAID_CARE_CODE, UNPAID_CARE_DESC) %>% summarise(OBS = sum(OBS), .groups = "keep") %>% ungroup() %>% rename_with(.fn = ~c("CODE", "DESC", "OBS")) %>% mutate(VAR = "UNPAID_CARE", .before = "CODE") %>% mutate(P = OBS/sum(OBS)))

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

# • 2.2. Add proportion to  marginals ----
# ────────────────────────────────────────
df_age <- df_age %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_car_avail <- df_car_avail %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_disability <- df_disability %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_econ_act <- df_econ_act %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_ethnicity <- df_ethnicity %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_health <- df_health %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_hhold_comp <- df_hhold_comp %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_hhold_type <- df_hhold_type %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_resid_length <- df_resid_length %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_main_lang <- df_main_lang %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_nssec <- df_nssec %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_occupation <- df_occupation %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_english_prof <- df_english_prof %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_quals <- df_quals %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_religion <- df_religion %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_sex <- df_sex %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_tenure <- df_tenure %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_unpaid_care <- df_unpaid_care %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()

# • 2.3. Convert constraints into MSOA level and deal with missing MSOAs ----
# ───────────────────────────────────────────────────────────────────────────

# • • 2.3.1. Domain 1: Communication ----

# ROW COUNTS
# df_area_lu %>% dplyr::filter(grepl("^E", OA21CD)) %>% distinct(MSOA21CD) %>% NROW()
# df_area_lu %>% dplyr::filter(grepl("^E", OA21CD)) %>% distinct(RGN22CD) %>% NROW()
# df_age_english_prof_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_age_quals_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_age_main_lang_rgn %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_main_lang_english_prof_rgn %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_main_lang_quals_rgn %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_english_prof_quals_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()

# These constraints are already at MSOA level and cover all areas without any 
# data suppression so only need to convert into proportions
df_age_english_prof_msoa <- df_age_english_prof_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_age_quals_msoa <- df_age_quals_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()

# Convert regions into MSOAs
df_age_main_lang_msoa <- df_age_main_lang_rgn %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup() %>%
  left_join(df_area_lu %>% distinct(MSOA21CD, MSOA21NM, RGN22CD), by = c("AREA_CODE" = "RGN22CD"), relationship = "many-to-many") %>%
  mutate(AREA_CODE = MSOA21CD, AREA_DESC = MSOA21NM) %>%
  select(AREA_CODE, AREA_DESC, AGE_CODE, AGE_DESC, MAIN_LANG_CODE, MAIN_LANG_DESC, OBS, P)

df_main_lang_english_prof_msoa <- df_main_lang_english_prof_rgn %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup() %>%
  left_join(df_area_lu %>% distinct(MSOA21CD, MSOA21NM, RGN22CD), by = c("AREA_CODE" = "RGN22CD"), relationship = "many-to-many") %>%
  mutate(AREA_CODE = MSOA21CD, AREA_DESC = MSOA21NM) %>% 
  select(AREA_CODE, AREA_DESC, MAIN_LANG_CODE, MAIN_LANG_DESC, ENGLISH_PROF_CODE, ENGLISH_PROF_DESC, OBS, P)

df_main_lang_quals_msoa <- df_main_lang_quals_rgn %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup() %>%
  left_join(df_area_lu %>% distinct(MSOA21CD, MSOA21NM, RGN22CD), by = c("AREA_CODE" = "RGN22CD"), relationship = "many-to-many") %>%
  mutate(AREA_CODE = MSOA21CD, AREA_DESC = MSOA21NM) %>% 
  select(AREA_CODE, AREA_DESC, MAIN_LANG_CODE, MAIN_LANG_DESC, QUALS_CODE, QUALS_DESC, OBS, P)

# Data suppression rules have resulted in 46 MSOAs having been removed, we will replace with the next level (LAD) value
df_english_prof_quals_msoa <- df_english_prof_quals_msoa %>%
  # Add in LAD code
  left_join(df_area_lu %>% distinct(MSOA21CD, LAD22CD), by = c("AREA_CODE" = "MSOA21CD")) %>% 
  # Group by LAD and summarise
  group_by(LAD22CD, ENGLISH_PROF_CODE, ENGLISH_PROF_DESC, QUALS_CODE, QUALS_DESC) %>%
  summarise(OBS = sum(OBS), .groups = "keep") %>%
  ungroup() %>%
  # Calculate LAD proportions
  group_by(LAD22CD) %>%
  mutate(P = OBS/sum(OBS)) %>%
  ungroup() %>%
  inner_join(df_area_lu %>% distinct(MSOA21CD, MSOA21NM, LAD22CD) %>%
               anti_join(df_english_prof_quals_msoa %>% distinct(AREA_CODE, AREA_DESC), 
                         by = c("MSOA21CD" = "AREA_CODE", "MSOA21NM" = "AREA_DESC")),
             by = "LAD22CD", relationship = "many-to-many") %>%
  mutate(AREA_CODE = MSOA21CD, AREA_DESC = MSOA21NM) %>%
  select(AREA_CODE, AREA_DESC, ENGLISH_PROF_CODE, ENGLISH_PROF_DESC, QUALS_CODE, QUALS_DESC, OBS, P) %>%
  bind_rows(df_english_prof_quals_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup())

# • • 2.3.2. Domain 2: Socioeconomic position ----

# ROW COUNTS
# df_area_lu %>% dplyr::filter(grepl("^E", OA21CD)) %>% distinct(MSOA21CD) %>% NROW()
# df_area_lu %>% dplyr::filter(grepl("^E", OA21CD)) %>% distinct(RGN22CD) %>% NROW()
# df_nssec_occupation_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_nssec_quals_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_nnsec_tenure_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_occupation_quals_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_occupation_tenure_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_quals_tenure_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()

# These constraints are already at MSOA level and cover all areas without any 
# data suppression so only need to convert into proportions
df_nssec_occupation_msoa <- df_nssec_occupation_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_nssec_quals_msoa <- df_nssec_quals_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_nnsec_tenure_msoa <- df_nnsec_tenure_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_occupation_quals_msoa <- df_occupation_quals_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_occupation_tenure_msoa <- df_occupation_tenure_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_quals_tenure_msoa <- df_quals_tenure_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()

# • • 2.3.3. Domain 3: Physical and practical access ----

# ROW COUNTS
# df_area_lu %>% dplyr::filter(grepl("^E", OA21CD)) %>% distinct(MSOA21CD) %>% NROW()
# df_area_lu %>% dplyr::filter(grepl("^E", OA21CD)) %>% distinct(RGN22CD) %>% NROW()
# df_age_car_avail_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_age_disability_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_age_health_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_car_avail_disability_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_car_avail_health_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_disability_health_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()

# These constraints are already at MSOA level and cover all areas without any 
# data suppression so only need to convert into proportions
df_age_car_avail_msoa <- df_age_car_avail_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_age_disability_msoa <- df_age_disability_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_age_health_msoa <- df_age_health_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_car_avail_disability_msoa <- df_car_avail_disability_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_car_avail_health_msoa <- df_car_avail_health_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_disability_health_msoa <- df_disability_health_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()

# • • 2.3.4. Domain 4: Social, cultural and geographical access ----

# ROW COUNTS
# df_area_lu %>% dplyr::filter(grepl("^E", OA21CD)) %>% distinct(MSOA21CD) %>% NROW()
# df_area_lu %>% dplyr::filter(grepl("^E", OA21CD)) %>% distinct(RGN22CD) %>% NROW()
# df_car_avail_hhold_comp_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_car_avail_ethnicity_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_car_avail_resid_length_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_ethnicity_hhold_comp_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_ethnicity_resid_length_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_ethnicity_religion_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_hhold_comp_resid_length_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_resid_length_religion_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_car_avail_religion_lad %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_hhold_comp_religion_lad %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()

# This constraints is already at MSOA level and covers all areas without any 
# data suppression so only need to convert into proportions
df_car_avail_hhold_comp_msoa <- df_car_avail_hhold_comp_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()

# Convert regions into MSOAs
df_car_avail_religion_msoa <- df_car_avail_religion_lad %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup() %>%
  left_join(df_area_lu %>% distinct(MSOA21CD, MSOA21NM, LAD22CD), by = c("AREA_CODE" = "LAD22CD"), relationship = "many-to-many") %>%
  mutate(AREA_CODE = MSOA21CD, AREA_DESC = MSOA21NM) %>% 
  select(AREA_CODE, AREA_DESC, CAR_AVAIL_CODE, CAR_AVAIL_DESC, RELIGION_CODE, RELIGION_DESC, OBS, P)

df_hhold_comp_religion_msoa <- df_hhold_comp_religion_lad %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup() %>%
  left_join(df_area_lu %>% distinct(MSOA21CD, MSOA21NM, LAD22CD), by = c("AREA_CODE" = "LAD22CD"), relationship = "many-to-many") %>%
  mutate(AREA_CODE = MSOA21CD, AREA_DESC = MSOA21NM) %>%
  select(AREA_CODE, AREA_DESC, HHOLD_COMP_CODE, HHOLD_COMP_DESC, RELIGION_CODE, RELIGION_DESC, OBS, P)

# Data suppression rules have resulted in a number of MSOAs having been removed from the following constraints, 
# we will replace with the next level (LAD) value
# Suppressed MSOAs 12
df_car_avail_ethnicity_msoa <- df_car_avail_ethnicity_msoa %>% 
  # Add in LAD code
  left_join(df_area_lu %>% distinct(MSOA21CD, LAD22CD), by = c("AREA_CODE" = "MSOA21CD")) %>% 
  # Group by LAD and summarise
  group_by(LAD22CD, CAR_AVAIL_CODE, CAR_AVAIL_DESC, ETHNICITY_CODE, ETHNICITY_DESC) %>%
  summarise(OBS = sum(OBS), .groups = "keep") %>%
  ungroup() %>%
  # Calculate LAD proportions
  group_by(LAD22CD) %>%
  mutate(P = OBS/sum(OBS)) %>%
  ungroup() %>%
  inner_join(df_area_lu %>% distinct(MSOA21CD, MSOA21NM, LAD22CD) %>%
               anti_join(df_car_avail_ethnicity_msoa %>% distinct(AREA_CODE, AREA_DESC), 
                         by = c("MSOA21CD" = "AREA_CODE", "MSOA21NM" = "AREA_DESC")),
             by = "LAD22CD", relationship = "many-to-many") %>%
  mutate(AREA_CODE = MSOA21CD, AREA_DESC = MSOA21NM) %>%
  select(AREA_CODE, AREA_DESC, CAR_AVAIL_CODE, CAR_AVAIL_DESC, ETHNICITY_CODE, ETHNICITY_DESC, OBS, P) %>%
  bind_rows(df_car_avail_ethnicity_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup())

# Suppressed MSOAs 14
df_car_avail_resid_length_msoa <- df_car_avail_resid_length_msoa %>%
  # Add in LAD code
  left_join(df_area_lu %>% distinct(MSOA21CD, LAD22CD), by = c("AREA_CODE" = "MSOA21CD")) %>% 
  # Group by LAD and summarise
  group_by(LAD22CD, CAR_AVAIL_CODE, CAR_AVAIL_DESC, RESID_LENGTH_CODE, RESID_LENGTH_DESC) %>%
  summarise(OBS = sum(OBS), .groups = "keep") %>%
  ungroup() %>%
  # Calculate LAD proportions
  group_by(LAD22CD) %>%
  mutate(P = OBS/sum(OBS)) %>%
  ungroup() %>%
  inner_join(df_area_lu %>% distinct(MSOA21CD, MSOA21NM, LAD22CD) %>%
               anti_join(df_car_avail_resid_length_msoa %>% distinct(AREA_CODE, AREA_DESC), 
                         by = c("MSOA21CD" = "AREA_CODE", "MSOA21NM" = "AREA_DESC")),
             by = "LAD22CD", relationship = "many-to-many") %>%
  mutate(AREA_CODE = MSOA21CD, AREA_DESC = MSOA21NM) %>%
  select(AREA_CODE, AREA_DESC, CAR_AVAIL_CODE, CAR_AVAIL_DESC, RESID_LENGTH_CODE, RESID_LENGTH_DESC, OBS, P) %>%
  bind_rows(df_car_avail_resid_length_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup())

# Suppressed MSOAs 12
df_ethnicity_hhold_comp_msoa <- df_ethnicity_hhold_comp_msoa %>% 
  # Add in LAD code
  left_join(df_area_lu %>% distinct(MSOA21CD, LAD22CD), by = c("AREA_CODE" = "MSOA21CD")) %>% 
  # Group by LAD and summarise
  group_by(LAD22CD, ETHNICITY_CODE, ETHNICITY_DESC, HHOLD_COMP_CODE, HHOLD_COMP_DESC) %>%
  summarise(OBS = sum(OBS), .groups = "keep") %>%
  ungroup() %>%
  # Calculate LAD proportions
  group_by(LAD22CD) %>%
  mutate(P = OBS/sum(OBS)) %>%
  ungroup() %>%
  inner_join(df_area_lu %>% distinct(MSOA21CD, MSOA21NM, LAD22CD) %>%
               anti_join(df_ethnicity_hhold_comp_msoa %>% distinct(AREA_CODE, AREA_DESC), 
                         by = c("MSOA21CD" = "AREA_CODE", "MSOA21NM" = "AREA_DESC")),
             by = "LAD22CD", relationship = "many-to-many") %>%
  mutate(AREA_CODE = MSOA21CD, AREA_DESC = MSOA21NM) %>%
  select(AREA_CODE, AREA_DESC, ETHNICITY_CODE, ETHNICITY_DESC, HHOLD_COMP_CODE, HHOLD_COMP_DESC, OBS, P) %>%
  bind_rows(df_ethnicity_hhold_comp_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup())

# Suppressed MSOAs 11
df_ethnicity_resid_length_msoa <- df_ethnicity_resid_length_msoa %>% 
  # Add in LAD code
  left_join(df_area_lu %>% distinct(MSOA21CD, LAD22CD), by = c("AREA_CODE" = "MSOA21CD")) %>% 
  # Group by LAD and summarise
  group_by(LAD22CD, ETHNICITY_CODE, ETHNICITY_DESC, RESID_LENGTH_CODE, RESID_LENGTH_DESC) %>%
  summarise(OBS = sum(OBS), .groups = "keep") %>%
  ungroup() %>%
  # Calculate LAD proportions
  group_by(LAD22CD) %>%
  mutate(P = OBS/sum(OBS)) %>%
  ungroup() %>%
  inner_join(df_area_lu %>% distinct(MSOA21CD, MSOA21NM, LAD22CD) %>%
               anti_join(df_ethnicity_resid_length_msoa %>% distinct(AREA_CODE, AREA_DESC), 
                         by = c("MSOA21CD" = "AREA_CODE", "MSOA21NM" = "AREA_DESC")),
             by = "LAD22CD", relationship = "many-to-many") %>%
  mutate(AREA_CODE = MSOA21CD, AREA_DESC = MSOA21NM) %>%
  select(AREA_CODE, AREA_DESC, ETHNICITY_CODE, ETHNICITY_DESC, RESID_LENGTH_CODE, RESID_LENGTH_DESC, OBS, P) %>%
  bind_rows(df_ethnicity_resid_length_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup())

# Suppressed MSOAs 2
# There is an additional problem with the data supression here as the LAD for Isles of Scilly is the MSOA so there is nothing 
# available at the LAD level, as such we will replace it with the Cornwall LAD proportions which whilst not perfect it is 
# better than using the national or regional position which are the other options.
df_ethnicity_religion_msoa <- df_ethnicity_religion_msoa %>% 
  # Add in LAD code
  left_join(df_area_lu %>% distinct(MSOA21CD, LAD22CD), by = c("AREA_CODE" = "MSOA21CD")) %>% 
  # Group by LAD and summarise
  group_by(LAD22CD, ETHNICITY_CODE, ETHNICITY_DESC, RELIGION_CODE, RELIGION_DESC) %>%
  summarise(OBS = sum(OBS), .groups = "keep") %>%
  ungroup() %>%
  # Calculate LAD proportions
  group_by(LAD22CD) %>%
  mutate(P = OBS/sum(OBS)) %>%
  ungroup() %>%
  inner_join(df_area_lu %>% distinct(MSOA21CD, MSOA21NM, LAD22CD) %>%
               anti_join(df_ethnicity_religion_msoa %>% distinct(AREA_CODE, AREA_DESC), 
                         by = c("MSOA21CD" = "AREA_CODE", "MSOA21NM" = "AREA_DESC")) %>%
               # Manual tweak to deal with Isles of Scilly issue
               mutate(LAD22CD = if_else(LAD22CD=="E06000053", "E06000052", LAD22CD)),
             by = "LAD22CD", relationship = "many-to-many") %>%
  mutate(AREA_CODE = MSOA21CD, AREA_DESC = MSOA21NM) %>%
  select(AREA_CODE, AREA_DESC, ETHNICITY_CODE, ETHNICITY_DESC, RELIGION_CODE, RELIGION_DESC, OBS, P) %>%
  bind_rows(df_ethnicity_religion_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup())

# Suppressed MSOAs 13
df_hhold_comp_resid_length_msoa <- df_hhold_comp_resid_length_msoa %>% 
  # Add in LAD code
  left_join(df_area_lu %>% distinct(MSOA21CD, LAD22CD), by = c("AREA_CODE" = "MSOA21CD")) %>% 
  # Group by LAD and summarise
  group_by(LAD22CD, HHOLD_COMP_CODE, HHOLD_COMP_DESC, RESID_LENGTH_CODE, RESID_LENGTH_DESC) %>%
  summarise(OBS = sum(OBS), .groups = "keep") %>%
  ungroup() %>%
  # Calculate LAD proportions
  group_by(LAD22CD) %>%
  mutate(P = OBS/sum(OBS)) %>%
  ungroup() %>%
  inner_join(df_area_lu %>% distinct(MSOA21CD, MSOA21NM, LAD22CD) %>%
               anti_join(df_hhold_comp_resid_length_msoa %>% distinct(AREA_CODE, AREA_DESC), 
                         by = c("MSOA21CD" = "AREA_CODE", "MSOA21NM" = "AREA_DESC")),
             by = "LAD22CD", relationship = "many-to-many") %>%
  mutate(AREA_CODE = MSOA21CD, AREA_DESC = MSOA21NM) %>%
  select(AREA_CODE, AREA_DESC, HHOLD_COMP_CODE, HHOLD_COMP_DESC, RESID_LENGTH_CODE, RESID_LENGTH_DESC, OBS, P) %>%
  bind_rows(df_hhold_comp_resid_length_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup())

# Suppressed MSOAs 11
df_resid_length_religion_msoa <- df_resid_length_religion_msoa %>% 
  # Add in LAD code
  left_join(df_area_lu %>% distinct(MSOA21CD, LAD22CD), by = c("AREA_CODE" = "MSOA21CD")) %>% 
  # Group by LAD and summarise
  group_by(LAD22CD, RESID_LENGTH_CODE, RESID_LENGTH_DESC, RELIGION_CODE, RELIGION_DESC) %>%
  summarise(OBS = sum(OBS), .groups = "keep") %>%
  ungroup() %>%
  # Calculate LAD proportions
  group_by(LAD22CD) %>%
  mutate(P = OBS/sum(OBS)) %>%
  ungroup() %>%
  inner_join(df_area_lu %>% distinct(MSOA21CD, MSOA21NM, LAD22CD) %>%
               anti_join(df_resid_length_religion_msoa %>% distinct(AREA_CODE, AREA_DESC), 
                         by = c("MSOA21CD" = "AREA_CODE", "MSOA21NM" = "AREA_DESC")),
             by = "LAD22CD", relationship = "many-to-many") %>%
  mutate(AREA_CODE = MSOA21CD, AREA_DESC = MSOA21NM) %>%
  select(AREA_CODE, AREA_DESC, RESID_LENGTH_CODE, RESID_LENGTH_DESC, RELIGION_CODE, RELIGION_DESC, OBS, P) %>%
  bind_rows(df_resid_length_religion_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup())

# • • 2.3.5. Domain 5: Demographic and health inequality ----

# ROW COUNTS
# df_area_lu %>% dplyr::filter(grepl("^E", OA21CD)) %>% distinct(MSOA21CD) %>% NROW()
# df_area_lu %>% dplyr::filter(grepl("^E", OA21CD)) %>% distinct(RGN22CD) %>% NROW()
# * ALREADY LOADED * df_age_disability_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW() 
# df_age_ethnicity_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# * ALREADY LOADED * df_age_health_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_age_sex_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_disability_ethnicity_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# * ALREADY LOADED * df_disability_health_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW() 
# df_disability_sex_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_ethnicity_health_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_ethnicity_sex_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()
# df_health_sex_msoa %>% dplyr::filter(grepl("^E", AREA_CODE)) %>% distinct(AREA_CODE) %>% NROW()

# These constraints are already at MSOA level and cover all areas without any 
# data suppression so only need to convert into proportions
df_age_ethnicity_msoa <- df_age_ethnicity_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_age_sex_msoa <- df_age_sex_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_disability_ethnicity_msoa <- df_disability_ethnicity_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_disability_sex_msoa <- df_disability_sex_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_ethnicity_health_msoa <- df_ethnicity_health_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_ethnicity_sex_msoa <- df_ethnicity_sex_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_health_sex_msoa <-df_health_sex_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()

# • • 2.3.6. Domain 6: Timing and caring commitments ----

# ROW COUNTS
# df_area_lu %>% dplyr::filter(grepl("^E", OA21CD)) %>% distinct(MSOA21CD) %>% NROW()
# df_area_lu %>% dplyr::filter(grepl("^E", OA21CD)) %>% distinct(RGN22CD) %>% NROW()
# 

# This constraints is already at MSOA level and covers all areas without any 
# data suppression so only need to convert into proportions
df_econ_act_unpaid_care_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_hhold_comp_unpaid_care_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()
df_hhold_type_unpaid_care_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup()

# There is an additional problem with the data suppression for the next towo constraints here as the LAD for Isles of Scilly 
# is the MSOA so there is nothing available at the LAD level, as such we will replace it with the Cornwall LAD proportions 
# which whilst not perfect it is better than using the national or regional position which are the other options.
# Suppressed MSOAs 2
df_econ_act_hhold_comp_msoa <- df_econ_act_hhold_comp_msoa %>% 
  # Add in LAD code
  left_join(df_area_lu %>% distinct(MSOA21CD, LAD22CD), by = c("AREA_CODE" = "MSOA21CD")) %>% 
  # Group by LAD and summarise
  group_by(LAD22CD, ECON_ACT_CODE, ECON_ACT_DESC, HHOLD_COMP_CODE, HHOLD_COMP_DESC) %>%
  summarise(OBS = sum(OBS), .groups = "keep") %>%
  ungroup() %>%
  # Calculate LAD proportions
  group_by(LAD22CD) %>%
  mutate(P = OBS/sum(OBS)) %>%
  ungroup() %>%
  inner_join(df_area_lu %>% distinct(MSOA21CD, MSOA21NM, LAD22CD) %>%
               anti_join(df_econ_act_hhold_comp_msoa %>% distinct(AREA_CODE, AREA_DESC), 
                         by = c("MSOA21CD" = "AREA_CODE", "MSOA21NM" = "AREA_DESC")) %>%
               # Manual tweak to deal with Isles of Scilly issue
               mutate(LAD22CD = if_else(LAD22CD=="E06000053", "E06000052", LAD22CD)),
             by = "LAD22CD", relationship = "many-to-many") %>%
  mutate(AREA_CODE = MSOA21CD, AREA_DESC = MSOA21NM) %>%
  select(AREA_CODE, AREA_DESC, ECON_ACT_CODE, ECON_ACT_DESC, HHOLD_COMP_CODE, HHOLD_COMP_DESC, OBS, P) %>%
  bind_rows(df_econ_act_hhold_comp_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup())

# Suppressed MSOAs 2
df_econ_act_hhold_type_msoa <- df_econ_act_hhold_type_msoa %>% 
  # Add in LAD code
  left_join(df_area_lu %>% distinct(MSOA21CD, LAD22CD), by = c("AREA_CODE" = "MSOA21CD")) %>% 
  # Group by LAD and summarise
  group_by(LAD22CD, ECON_ACT_CODE, ECON_ACT_DESC, HHOLD_TYPE_CODE, HHOLD_TYPE_DESC) %>%
  summarise(OBS = sum(OBS), .groups = "keep") %>%
  ungroup() %>%
  # Calculate LAD proportions
  group_by(LAD22CD) %>%
  mutate(P = OBS/sum(OBS)) %>%
  ungroup() %>%
  inner_join(df_area_lu %>% distinct(MSOA21CD, MSOA21NM, LAD22CD) %>%
               anti_join(df_econ_act_hhold_type_msoa %>% distinct(AREA_CODE, AREA_DESC), 
                         by = c("MSOA21CD" = "AREA_CODE", "MSOA21NM" = "AREA_DESC")) %>%
               # Manual tweak to deal with Isles of Scilly issue
               mutate(LAD22CD = if_else(LAD22CD=="E06000053", "E06000052", LAD22CD)),
             by = "LAD22CD", relationship = "many-to-many") %>%
  mutate(AREA_CODE = MSOA21CD, AREA_DESC = MSOA21NM) %>%
  select(AREA_CODE, AREA_DESC, ECON_ACT_CODE, ECON_ACT_DESC, HHOLD_TYPE_CODE, HHOLD_TYPE_DESC, OBS, P) %>%
  bind_rows(df_econ_act_hhold_type_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup())

# Suppressed MSOAs 3
df_hhold_comp_hhold_type_msoa <- df_hhold_comp_hhold_type_msoa %>% 
  # Add in LAD code
  left_join(df_area_lu %>% distinct(MSOA21CD, LAD22CD), by = c("AREA_CODE" = "MSOA21CD")) %>% 
  # Group by LAD and summarise
  group_by(LAD22CD, HHOLD_COMP_CODE, HHOLD_COMP_DESC, HHOLD_TYPE_CODE, HHOLD_TYPE_DESC) %>%
  summarise(OBS = sum(OBS), .groups = "keep") %>%
  ungroup() %>%
  # Calculate LAD proportions
  group_by(LAD22CD) %>%
  mutate(P = OBS/sum(OBS)) %>%
  ungroup() %>%
  inner_join(df_area_lu %>% distinct(MSOA21CD, MSOA21NM, LAD22CD) %>%
               anti_join(df_hhold_comp_hhold_type_msoa %>% distinct(AREA_CODE, AREA_DESC), 
                         by = c("MSOA21CD" = "AREA_CODE", "MSOA21NM" = "AREA_DESC")),
             by = "LAD22CD", relationship = "many-to-many") %>%
  mutate(AREA_CODE = MSOA21CD, AREA_DESC = MSOA21NM) %>%
  select(AREA_CODE, AREA_DESC, HHOLD_COMP_CODE, HHOLD_COMP_DESC, HHOLD_TYPE_CODE, HHOLD_TYPE_DESC, OBS, P) %>%
  bind_rows(df_hhold_comp_hhold_type_msoa %>% group_by(AREA_CODE) %>% mutate(P = OBS/sum(OBS)) %>% ungroup())

# 3. Iterative Proportional Fitting ----
# ══════════════════════════════════════

# • 3.1 High Level Balancing (MSOA Level) ----

df_seed <- expand_grid(AGE_CODE = df_code_lookup %>% dplyr::filter(VAR == "AGE") %>% .$CODE,
                       MAIN_LANG_CODE = df_code_lookup %>% dplyr::filter(VAR == "MAIN_LANG") %>% .$CODE,
                       ENGLISH_PROF_CODE = df_code_lookup %>% dplyr::filter(VAR == "ENGLISH_PROF") %>% .$CODE,
                       QUALS_CODE = df_code_lookup %>% dplyr::filter(VAR == "QUALS") %>% .$CODE) %>%
  left_join(df_code_lookup %>% dplyr::filter(VAR == "AGE") %>% mutate(AGE_CODE = CODE, P_AGE = P, .keep = "none"), by = c("AGE_CODE")) %>%
  left_join(df_code_lookup %>% dplyr::filter(VAR == "MAIN_LANG") %>% mutate(MAIN_LANG_CODE = CODE, P_MAIN_LANG = P, .keep = "none"), by = c("MAIN_LANG_CODE")) %>%
  left_join(df_code_lookup %>% dplyr::filter(VAR == "ENGLISH_PROF") %>% mutate(ENGLISH_PROF_CODE = CODE, P_ENGLISH_PROF = P, .keep = "none"), by = c("ENGLISH_PROF_CODE")) %>%
  left_join(df_code_lookup %>% dplyr::filter(VAR == "QUALS") %>% mutate(QUALS_CODE = CODE, P_QUALS = P, .keep = "none"), by = c("QUALS_CODE")) %>%
  mutate(P_SEED = P_AGE * P_MAIN_LANG * P_ENGLISH_PROF * P_QUALS)

seed <- xtabs(
  P_SEED ~ AGE_CODE + MAIN_LANG_CODE + ENGLISH_PROF_CODE + QUALS_CODE,
  data = df_seed
)

fnProcessDomain1 <- function(){
  # Domain One: Communication
  # • age
  # • main language
  # • proficiency in English
  # • qualifications
  
  # Create high level geography seed
  seed <- fnD1_CreateSeed()
  
  # Balance high level geography
  msoa_list <- df_area_lu %>% dplyr::filter(grepl("^E", MSOA21CD)) %>% distinct(MSOA21CD) %>% .$MSOA21CD
  res <- do.call("rbind", lapply(msoa_list, fnD1_BalanceHighLevel, seed))
  
  fnD1_BalanceHighLevel(msoa_list[1], seed)
  # Process low level geography
  oa_list <- 
}



# Create target matrices for the constraints

area <- "E02004152"
fnD1_BalanceHighLevel(area)


oa_list <- df_area_lu %>% dplyr::filter(MSOA21CD == area) %>% .$OA21CD


age
  
ipf$p.hat %>% 
  as.data.frame() %>% 
  mutate(AREA_CODE = oa_list[1], P = Freq) %>%
  select(AREA_CODE, AGE_CODE, MAIN_LANG_CODE, ENGLISH_PROF_CODE, QUALS_CODE, P) %>%
  left_join(df_popn, by = "AREA_CODE") %>%
  mutate(OBS = P * OBS) %>%
  left_join(df_code_lookup %>% dplyr::filter(VAR=="AGE") %>% mutate(AGE_CODE = as.factor(CODE), AGE_DESC = DESC, .keep = "none"), by = "AGE_CODE") %>%
  left_join(df_code_lookup %>% dplyr::filter(VAR=="MAIN_LANG") %>% mutate(MAIN_LANG_CODE = as.factor(CODE), MAIN_LANG_DESC = DESC, .keep = "none"), by = "MAIN_LANG_CODE") %>%
  left_join(df_code_lookup %>% dplyr::filter(VAR=="ENGLISH_PROF") %>% mutate(ENGLISH_PROF_CODE = as.factor(CODE), ENGLISH_PROF_DESC = DESC, .keep = "none"), by = "ENGLISH_PROF_CODE") %>%
  left_join(df_code_lookup %>% dplyr::filter(VAR=="QUALS") %>% mutate(QUALS_CODE = as.factor(CODE), QUALS_DESC = DESC, .keep = "none"), by = "QUALS_CODE") %>%
  select(AREA_CODE, AGE_CODE, AGE_DESC, MAIN_LANG_CODE, MAIN_LANG_DESC, ENGLISH_PROF_CODE, ENGLISH_PROF_DESC, QUALS_CODE, QUALS_DESC, OBS, P) %>% 
  write.csv("temp.csv")



  df <- ipf$p.hat %>% 
    as.data.frame() %>% 
    mutate(AREA = area, AGE, GENDER, HEALTH, P = Freq, .keep = "none") %>%
    select(AREA, AGE, GENDER, HEALTH, P)
  
  return(df)
}



target_age_gender <- df_age_gender %>%
  select(AGE, GENDER, PCT) %>%
  tidyr::pivot_wider(
    names_from = GENDER,
    values_from = PCT
  ) %>%
  tibble::column_to_rownames("AGE") %>%
  as.matrix()


mutate(P_AGE = df_code_lookup$P[df_code_lookup$VAR=="AGE" & df_code_lookup$CODE==AGE_CODE])





df_code_lookup$P[df_code_lookup$VAR=="AGE" & df_code_lookup$CODE==AGE_CODE]



# Communication
df_age_english_prof_msoa
df_age_quals_msoa
df_age_main_lang_msoa
df_main_lang_english_prof_msoa
df_main_lang_quals_msoa
df_english_prof_quals_msoa

df_age_english_prof_msoa %>% 
  select(AREA_CODE, AGE_DESC, ENGLISH_PROF_DESC, P)


#    • age
#    • main language
#    • proficiency in English
#    • qualifications

expand_grid(
  
)

expand_grid(
  df_age_english_prof_msoa[df_age_english_prof_msoa$AREA_CODE=="E02000001", c("AREA_CODE", "AGE_DESC", "ENGLISH_PROF_DESC", "P")],
  df_age_quals_msoa[df_age_quals_msoa$AREA_CODE=="E02000001", c("AREA_CODE", "AGE_DESC", "QUALS_DESC", "P")])

%>%
  expand_()
