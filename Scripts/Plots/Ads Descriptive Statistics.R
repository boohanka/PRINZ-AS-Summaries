

rm(list = ls())

# ============================================================================#
# ============================================================================#
# ==== # ========================= Uploading =========================== # ====
# ============================================================================#
# ============================================================================#

df_ads <- read_csv("Data/Cleaned/Ads/Ads By Skill.csv")
df_ads_classification <- read_csv("Data/Raw/Ads/Skills Classification.csv")

# ============================================================================#
# ============================================================================#
# ==== # ===================== Most Popular Skills ===================== # ====
# ============================================================================#
# ============================================================================#

low_carbon_classes <- c("Decarbonization-buildings", "Decarbonization-energy",
                        "Decarbonization-transport")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#        Full Sample         #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~ #

full_sample <- list()

full_sample$'Top Skills' <- df_ads %>% 
  filter(classification %in% low_carbon_classes) %>% 
  group_by(skill) %>% 
  summarise(
    n_ads_skill = sum(n_ads_skill, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  arrange(desc(n_ads_skill)) %>% 
  left_join(
    df_ads_classification,
    by = "skill"
  )

full_sample$'Top 15 Skills' <- full_sample$'Top Skills'[1:15,]


full_sample$'Top Skills Weighted' <- df_ads %>% 
  filter(classification %in% low_carbon_classes) %>% 
  mutate(weighted_n_ads_skill = n_ads_skill * weight) %>% 
  group_by(skill) %>% 
  summarise(
    weighted_n_ads_skill = sum(weighted_n_ads_skill, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  arrange(desc(weighted_n_ads_skill)) %>% 
  left_join(
    df_ads_classification,
    by = "skill"
  )

full_sample$'Top Classes' <- df_ads %>% 
  filter(classification %in% low_carbon_classes) %>% 
  group_by(classification) %>% 
  summarise(
    n_ads_class = sum(n_ads_skill, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  arrange(desc(n_ads_class)) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#      AS Aligned Sample     #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~ #

df_ads_as_aligned <- df_ads %>% 
  filter((flag_1 | flag_3) & flag_2)

as_aligned_sample <- list()

as_aligned_sample$'Top Skills' <- df_ads_as_aligned %>% 
  filter(classification %in% low_carbon_classes) %>% 
  group_by(skill) %>% 
  summarise(
    n_ads_skill = sum(n_ads_skill, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  arrange(desc(n_ads_skill)) %>% 
  left_join(
    df_ads_classification,
    by = "skill"
  )

as_aligned_sample$'Top 15 Skills' <- as_aligned_sample$'Top Skills'[1:15,]


as_aligned_sample$'Top Skills Weighted' <- df_ads_as_aligned %>% 
  filter(classification %in% low_carbon_classes) %>% 
  mutate(weighted_n_ads_skill = n_ads_skill * weight) %>% 
  group_by(skill) %>% 
  summarise(
    weighted_n_ads_skill = sum(weighted_n_ads_skill, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  arrange(desc(weighted_n_ads_skill)) %>% 
  left_join(
    df_ads_classification,
    by = "skill"
  )

as_aligned_sample$'Top Classes' <- df_ads_as_aligned %>% 
  filter(classification %in% low_carbon_classes) %>% 
  group_by(classification) %>% 
  summarise(
    n_ads_class = sum(n_ads_skill, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  arrange(desc(n_ads_class)) 

# ============================================================================#
# ============================================================================#
# ==== # ========================== Export ============================== # ===
# ============================================================================#
# ============================================================================#

library(writexl)

exporting_table <- function(list, name, dir) {
  exp_path <- file.path(dir, paste0(name, ".xlsx"))
  write_xlsx(list, path = exp_path)
}

exporting_table(full_sample, "Full Sample", "Outputs/Ads/Summary Stats")
exporting_table(as_aligned_sample, "AS Aligned Sample", "Outputs/Ads/Summary Stats")
