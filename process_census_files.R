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


fnProcessConstraint <- function(filename, vars){
  df <- read.csv(filename)
  
  names(df) <- c(paste0(c(rep("AREA", 2), rep(vars[1], 2), rep(vars[2], 2)), rep(c("_CODE", "_DESC"), 3)), "OBS")
  
  df <- df %>% arrange(AREA_CODE) %>% mutate(across(.cols = c(1:2, 4, 6), .fns = as.factor))
  
  df[,paste0(vars[1], "_DESC")] <- forcats::fct_reorder(df[,paste0(vars[1], "_DESC")], df[,paste0(vars[1], "_CODE")])
  df[,paste0(vars[2], "_DESC")] <- forcats::fct_reorder(df[,paste0(vars[2], "_DESC")], df[,paste0(vars[2], "_CODE")])
  
  return(df)
}

# 1. Load Data ----
# ════════════════════════════════════════════

# • 1.0. Low Level to High Level Geography Lookups ----
# ─────────────────────────────────────────────────────

# • 1.1. Low Level Geography Marginals ----
# ─────────────────────────────────────────

# • • 1.1.1. Age ----
# • • 1.1.2. Car availability ----
# • • 1.1.3. Disability ----
# • • 1.1.4. Economic activity ----
# • • 1.1.5. Ethnicity ----
# • • 1.1.6. General health ----
# • • 1.1.7. Household composition ----
# • • 1.1.8. Household type ----
# • • 1.1.9. Length of residence ----
# • • 1.1.10. Main language ----
# • • 1.1.11. NS-SeC ----
# • • 1.1.12. Occupation ----
# • • 1.1.13. Proficiency in English ----
# • • 1.1.14. Qualifications ----
# • • 1.1.15. Religion ----
# • • 1.1.16. Sex ----
# • • 1.1.17. Tenure ----
# • • 1.1.18. Unpaid care ----

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


