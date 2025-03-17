

rm(list = ls())

# ============================================================================#
# ============================================================================#
# ==== # ========================= Uploading =========================== # ====
# ============================================================================#
# ============================================================================#

library("nanoparquet")
library("arrow")

ds_ads <- open_dataset("Data/Raw/Ads/uk_ads_skills_soc_ttwa.parquet")
#pq_ads <- read_parquet("Data/Raw/Ads/uk_ads_skills_soc_ttwa.parquet")
  

ds_ads$schema

unique_years <- ds_ads %>%
  select(year) %>%
  distinct() %>%
  collect() %>%
  arrange(year) %>% 
  pull(year) 

# ============================================================================#
# ============================================================================#
# ==== # ================== Obtain Distribution of SOCs ============== # ====
# ============================================================================#
# ============================================================================#

# ~~~~~~~~~~~~~~~~~~~ #
#         2018        #
# ~~~~~~~~~~~~~~~~~~~ #


df_ads18 <- ds_ads %>% filter(year == 2018) %>% collect()

df_soc18 <- df_ads18 %>% 
  select(-c(skill, low_carbon, green, n_ads_skill)) %>% 
  distinct() %>% 
  group_by(year, soc4d_code, soc4d_desc) %>% 
  summarise(
    n_ads_occ = sum(n_ads_ttwa_occ, na.rm = TRUE),
    .groups = "drop"
  )

rm(df_ads18)

# ~~~~~~~~~~~~~~~~~~~ #
#         2019        #
# ~~~~~~~~~~~~~~~~~~~ #

df_ads19 <- ds_ads %>% filter(year == 2019) %>% collect()

df_soc19 <- df_ads19 %>% 
  select(-c(skill, low_carbon, green, n_ads_skill)) %>% 
  distinct() %>% 
  group_by(year, soc4d_code, soc4d_desc) %>% 
  summarise(
    n_ads_occ = sum(n_ads_ttwa_occ, na.rm = TRUE),
    .groups = "drop"
  )

rm(df_ads19)

# ~~~~~~~~~~~~~~~~~~~ #
#         2020        #
# ~~~~~~~~~~~~~~~~~~~ #

df_ads20 <- ds_ads %>% filter(year == 2020) %>% collect()

df_soc20 <- df_ads20 %>% 
  select(-c(skill, low_carbon, green, n_ads_skill)) %>% 
  distinct() %>% 
  group_by(year, soc4d_code, soc4d_desc) %>% 
  summarise(
    n_ads_occ = sum(n_ads_ttwa_occ, na.rm = TRUE),
    .groups = "drop"
  )

rm(df_ads20)


# ~~~~~~~~~~~~~~~~~~~ #
#       Combine       #
# ~~~~~~~~~~~~~~~~~~~ #

df_soc1820 <- bind_rows(df_soc18, df_soc19, df_soc20)

df_soc1820 %<>%
  group_by(soc4d_code, soc4d_desc) %>% 
  summarise(
    n_ads_occ_1820 = sum(n_ads_occ, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  arrange(desc(n_ads_occ_1820)) %>% 
  mutate(lightcast_soc_share = (n_ads_occ_1820 / sum(n_ads_occ_1820)))


rm(df_soc18, df_soc19, df_soc20)




# ============================================================================#
# ============================================================================#
# ==== # ========================== Export ============================== # ===
# ============================================================================#
# ============================================================================#

exporting_csv <- function(df, name, dir) {
  exp_path <- file.path(dir, paste0(name, ".csv"))
  write.csv(df, file = exp_path, row.names = FALSE)
}

exporting_csv(df_soc1820, "Lightcast SOC4D Distribution", "Data/Intermediate/Occupations")

gc()
