library(tidyverse)
library(readxl)
library(mipfp)

levels_age <- c("YOUNG", "ADULT", "ELDERLY")
levels_gender <- c("FEMALE", "MALE")
levels_health <- c("GOOD", "FAIR", "POOR")
levels_tenure <- c("OWNED", "MORTAGE", "SOCIAL", "PRIVATE")

# Load high level geography constraints
df_age_gender <- readxl::read_xlsx(path = "worked_example/data/constraints_with_tenure.xlsx", sheet = "AGE_GENDER") %>%
  mutate(AGE = factor(AGE, levels_age), GENDER = factor(GENDER, levels_gender))
df_age_health <- readxl::read_xlsx(path = "worked_example/data/constraints_with_tenure.xlsx", sheet = "AGE_HEALTH") %>%
  mutate(AGE = factor(AGE, levels_age), HEALTH = factor(HEALTH, levels_health))
df_gender_health <- readxl::read_xlsx(path = "worked_example/data/constraints_with_tenure.xlsx", sheet = "GENDER_HEALTH") %>%
  mutate(GENDER = factor(GENDER, levels_gender), HEALTH = factor(HEALTH, levels_health))
df_age_tenure <- readxl::read_xlsx(path = "worked_example/data/constraints_with_tenure.xlsx", sheet = "AGE_TENURE") %>%
  mutate(AGE = factor(AGE, levels_age), TENURE = factor(TENURE, levels_tenure))
df_gender_tenure <- readxl::read_xlsx(path = "worked_example/data/constraints_with_tenure.xlsx", sheet = "GENDER_TENURE") %>%
  mutate(GENDER = factor(GENDER, levels_gender), TENURE = factor(TENURE, levels_tenure))
df_health_tenure <- readxl::read_xlsx(path = "worked_example/data/constraints_with_tenure.xlsx", sheet = "HEALTH_TENURE") %>%
  mutate(HEALTH = factor(HEALTH, levels_health), TENURE = factor(TENURE, levels_tenure))

# Load low level geography marginals
df_age <- readxl::read_xlsx(path = "worked_example/data/marginals_with_tenure.xlsx", sheet = "AGE") %>% 
  select(-POPN) %>% pivot_longer(cols = 3:5, names_to = "AGE", values_to = "POPN") %>%
  mutate(AGE = factor(AGE, levels_age))
df_gender <- readxl::read_xlsx(path = "worked_example/data/marginals_with_tenure.xlsx", sheet = "GENDER") %>% 
  select(-POPN) %>% pivot_longer(cols = 3:4, names_to = "GENDER", values_to = "POPN") %>%
  mutate(GENDER = factor(GENDER, levels_gender))
df_health <- readxl::read_xlsx(path = "worked_example/data/marginals_with_tenure.xlsx", sheet = "HEALTH") %>% 
  select(-POPN) %>% pivot_longer(cols = 3:5, names_to = "HEALTH", values_to = "POPN") %>%
  mutate(HEALTH = factor(HEALTH, levels_health))
df_tenure <- readxl::read_xlsx(path = "worked_example/data/marginals_with_tenure.xlsx", sheet = "TENURE") %>% 
  select(-HOUSEHOLDS) %>% pivot_longer(cols = 3:6, names_to = "TENURE", values_to = "HOUSEHOLDS") %>%
  mutate(TENURE = factor(TENURE, levels_tenure))


# Transform to proportions
df_age_gender_prop <- df_age_gender %>% group_by(LAD24CD) %>% mutate(P = POPN/sum(POPN)) %>% ungroup() %>% select(AGE, GENDER, P)
df_age_health_prop <- df_age_health %>% group_by(LAD24CD) %>% mutate(P = POPN/sum(POPN)) %>% ungroup() %>% select(AGE, HEALTH, P)
df_gender_health_prop <- df_gender_health %>% group_by(LAD24CD) %>% mutate(P = POPN/sum(POPN)) %>% ungroup() %>% select(HEALTH, GENDER, P)
df_age_tenure_prop <- df_age_tenure %>% group_by(LAD24CD) %>% mutate(P = POPN/sum(POPN)) %>% ungroup() %>% select(AGE, TENURE, P)
df_gender_tenure_prop <- df_gender_tenure %>% group_by(LAD24CD) %>% mutate(P = POPN/sum(POPN)) %>% ungroup() %>% select(GENDER, TENURE, P)
df_health_tenure_prop <- df_health_tenure %>% group_by(LAD24CD) %>% mutate(P = POPN/sum(POPN)) %>% ungroup() %>% select(HEALTH, TENURE, P)

df_age_prop <- df_age %>% group_by(OA21CD) %>% mutate(P = POPN/sum(POPN)) %>% ungroup() %>% select(OA21CD, OA21NM, AGE, P)
df_gender_prop <- df_gender %>% group_by(OA21CD) %>% mutate(P = POPN/sum(POPN)) %>% ungroup() %>% select(OA21CD, OA21NM, GENDER, P)
df_health_prop <- df_health %>% group_by(OA21CD) %>% mutate(P = POPN/sum(POPN)) %>% ungroup() %>% select(OA21CD, OA21NM, HEALTH, P)
df_tenure_prop <- df_tenure %>% group_by(OA21CD) %>% mutate(P = HOUSEHOLDS/sum(HOUSEHOLDS)) %>% ungroup() %>% select(OA21CD, OA21NM, TENURE, P)

df_seed_hi <- expand_grid(df_age_health_prop %>% group_by(AGE) %>% summarise(P_AGE = sum(P)) %>% ungroup(),
                          df_age_gender_prop %>% group_by(GENDER) %>% summarise(P_GENDER = sum(P)) %>% ungroup(),
                          df_gender_health_prop %>% group_by(HEALTH) %>% summarise(P_HEALTH = sum(P)) %>% ungroup(),
                          df_health_tenure_prop %>% group_by(TENURE) %>% summarise(P_TENURE = sum(P)) %>% ungroup()) %>%
  mutate(AGE, GENDER, HEALTH, TENURE, P_SEED = P_AGE * P_GENDER * P_HEALTH * P_TENURE, .keep = "none")


seed <- xtabs(
  P_SEED ~ AGE + GENDER + HEALTH + TENURE,
  data = df_seed_hi
)

# Create target matrices for the constraints
target_age_gender <- df_age_gender_prop %>%
  select(AGE, GENDER, P) %>%
  tidyr::pivot_wider(
    names_from = GENDER,
    values_from = P
  ) %>%
  tibble::column_to_rownames("AGE") %>%
  as.matrix()

target_age_health <- df_age_health_prop %>%
  select(AGE, HEALTH, P) %>%
  tidyr::pivot_wider(
    names_from = HEALTH,
    values_from = P
  ) %>%
  tibble::column_to_rownames("AGE") %>%
  as.matrix()

target_gender_health <- df_gender_health_prop %>%
  select(GENDER, HEALTH, P) %>%
  tidyr::pivot_wider(
    names_from = HEALTH,
    values_from = P
  ) %>%
  tibble::column_to_rownames("GENDER") %>%
  as.matrix()

target_age_tenure <- df_age_tenure_prop %>%
  select(AGE, TENURE, P) %>%
  tidyr::pivot_wider(
    names_from = TENURE,
    values_from = P
  ) %>%
  tibble::column_to_rownames("AGE") %>%
  as.matrix()

target_gender_tenure <- df_gender_tenure_prop %>%
  select(GENDER, TENURE, P) %>%
  tidyr::pivot_wider(
    names_from = TENURE,
    values_from = P
  ) %>%
  tibble::column_to_rownames("GENDER") %>%
  as.matrix()

target_health_tenure <- df_health_tenure_prop %>%
  select(HEALTH, TENURE, P) %>%
  tidyr::pivot_wider(
    names_from = TENURE,
    values_from = P
  ) %>%
  tibble::column_to_rownames("HEALTH") %>%
  as.matrix()

target_list <- list(
  c(1, 2),  # Age × Gender
  c(1, 3),  # Age × Health
  c(2, 3),  # Gender × Health
  c(1, 4),  # Age × Tenure
  c(2, 4),  # Gender × Tenure
  c(3, 4)   # Health × Tenure
)

target_data <- list(
  target_age_gender,
  target_age_health,
  target_gender_health,
  target_age_tenure,
  target_gender_tenure,
  target_health_tenure
)

# Run the ipf for the high level geogrpahy
ipf_high <- mipfp::Ipfp(
  seed = seed,
  target.list = target_list,
  target.data = target_data,
  print = TRUE,
  iter = 1000,
  tol = 1e-10,
  tol.margins = 1e-10
)

write.csv(ipf_high$p.hat %>% data.frame() %>% left_join(df_seed_hi, by = c("AGE","GENDER","HEALTH","TENURE")), "ipf_high.csv")
