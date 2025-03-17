

rm(list = ls())

# ============================================================================#
# ============================================================================#
# ==== # ========================= Uploading =========================== # ====
# ============================================================================#
# ============================================================================#

library("nanoparquet")
library("arrow")
library("furrr")

list_year_ads <- map(
  2018:2024,
  ~ open_dataset(paste0("Data/Intermediate/Ads/Size/ads_", .x, ".parquet")) %>%
    collect() %>% 
    mutate(
      across(c("low_carbon", "green"),
      ~ if_else(is.na(.), 0, .))
    )
) %>%
  setNames(as.character(2018:2024))

# Turn into a dataframe:

df_ads_full <- bind_rows(list_year_ads)

# Upload classifications and occupation info:

df_ads_classification <- read_csv("Data/Raw/Ads/Skills Classification.csv")
df_soc_weights <- read_csv("Data/Cleaned/Occupations/SOC Flags and Weights.csv")

# ============================================================================#
# ============================================================================#
# ==== # ======== Add Skill Classifications & Weights & Flags ========== # ====
# ============================================================================#
# ============================================================================#

# Add Classifications

df_ads <- df_ads_full %>% 
  left_join(
    df_ads_classification %>%
      select(skill, llm_classification),
    by = "skill"
  ) %>% 
  mutate(
    classification = if_else(is.na(llm_classification), "Non-low-carbon", llm_classification)
  ) %>% 
  select(-llm_classification)

# Count by classification:

low_carbon_classes_inc_industry <- c("Decarbonization-buildings", "Decarbonization-transport",
                        "Decarbonization-energy", "Decarbonization-industry")

low_carbon_classes <- c("Decarbonization-buildings", "Decarbonization-transport",
                         "Decarbonization-energy")

df_ads %<>%
  mutate(
    classification = if_else(classification %in% low_carbon_classes, classification, "Non-low-carbon")
  ) %>% 
  group_by(
    date,
    ttwa11cd, ttwa11nm,
    soc4d_code, soc4d_desc,
    low_carbon, classification
  ) %>% 
  summarise(
    n_ads_class = sum(n_ads_skill, na.rm = TRUE),
    .groups = "drop"
  )

# Add weights & flags

df_ads %<>%
  left_join(
    df_soc_weights,
    by = c("soc4d_code", "soc4d_desc")
  ) %>% 
  mutate(
    weighted_n_ads_class = n_ads_class * weight
  )

# ============================================================================#
# ============================================================================#
# ==== # ======= Subsample Ads Data & Aggregate Away Occupations ======= # ====
# ============================================================================#
# ============================================================================#

df_ads_all_socs <- df_ads %>%
  group_by(
    date, 
    ttwa11cd, ttwa11nm,
    classification
  ) %>% 
  summarise(
    n_ads_class = sum(n_ads_class, na.rm = TRUE),
    weighted_n_ads_class = sum(weighted_n_ads_class, na.rm = TRUE),
    .groups = "drop"
  )

df_ads_as_aligned <- df_ads %>% 
  filter(flag_educ) %>% 
  group_by(
    date, 
    ttwa11cd, ttwa11nm,
    classification
  ) %>% 
  summarise(
    n_ads_class = sum(n_ads_class, na.rm = TRUE),
    weighted_n_ads_class = sum(weighted_n_ads_class, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================================#
# ============================================================================#
# ==== # ==== Create a Version at Skill Level for Descriptive Stats ==== # ====
# ============================================================================#
# ============================================================================#

df_ads <- df_ads_full %>% 
  left_join(
    df_ads_classification %>%
      select(skill, llm_classification),
    by = "skill"
  ) %>% 
  mutate(
    classification = if_else(is.na(llm_classification), "Non-low-carbon", llm_classification)
  ) %>% 
  select(-llm_classification)

df_ads_by_skill <- df_ads %>% 
  left_join(
    df_soc_weights %>% select(-n_ads_occ),
    by = c("soc4d_code", "soc4d_desc")
  )
  

# ============================================================================#
# ============================================================================#
# ==== # ========================== Export ============================== # ===
# ============================================================================#
# ============================================================================#

exporting_csv <- function(df, name, dir) {
  base_dir <- dir
  df_exp <- df
  exp_path <- file.path(base_dir, paste0(name, ".csv"))
  write.csv(df_exp, file = exp_path, row.names = FALSE)
}

exporting_csv(df_ads_by_skill, "Ads By Skill", "Data/Cleaned/Ads")

exporting_csv(df_ads_all_socs, "Ads Full Sample", "Data/Cleaned/Ads")
exporting_csv(df_ads_as_aligned, "Ads AS-Aligned", "Data/Cleaned/Ads")


