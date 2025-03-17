

rm(list = ls())

# ============================================================================#
# ============================================================================#
# ==== # ========================= Uploading =========================== # ====
# ============================================================================#
# ============================================================================#

df_lfs <- read_csv("Data/Intermediate/Occupations/LFS SOC3D Distribution.csv")
df_lightcast <- read_csv("Data/Intermediate/Occupations/Lightcast SOC4D Distribution.csv")

# ============================================================================#
# ============================================================================#
# ==== # ================= Align, Merge, Create Weights ================ # ====
# ============================================================================#
# ============================================================================#

# Change Lightcast distribution to 3 digit level:

df_lightcast %<>%
  mutate(soc3d_code = substr(soc4d_code, 1, 3)) %>% 
  group_by(soc3d_code) %>% 
  summarise(
    n_ads_occ_1820 = sum(n_ads_occ_1820, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(lightcast_soc_share = (n_ads_occ_1820 / sum(n_ads_occ_1820)))

# Merge the two datasets:

df_soc <- df_lfs %>%
  rename(soc3d_code = sc10mmn) %>% 
  mutate(soc3d_code = as.character(soc3d_code)) %>% 
  left_join(df_lightcast, by = "soc3d_code")

# Tidy

df_soc %<>%
  mutate(soc3d_weight = lfs_soc_share / lightcast_soc_share) %>% 
  select(
    soc3d_code,
    n_ads_occ_1820, lfs_weight_1820,
    lfs_soc_share, lightcast_soc_share,
    soc3d_weight
  )

# ============================================================================#
# ============================================================================#
# ==== # ========================== Export ============================== # ===
# ============================================================================#
# ============================================================================#

exporting_csv <- function(df, name, dir) {
  exp_path <- file.path(dir, paste0(name, ".csv"))
  write.csv(df, file = exp_path, row.names = FALSE)
}

exporting_csv(df_soc, "SOC Weights", "Data/Intermediate/Occupations")
