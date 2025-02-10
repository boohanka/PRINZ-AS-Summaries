

rm(list = ls())

# ============================================================================#
# ============================================================================#
# ==== # ========================= Uploading =========================== # ====
# ============================================================================#
# ============================================================================#

df_raw_as_skills <- read_xlsx("Data 2025/Raw/AS/Raw List AS Skills.xlsx")
df_green_skills <- read_xlsx("Data 2025/Classification Check/Ilya Check V2/AS LC Skills List.xlsx")

# ============================================================================#
# ============================================================================#
# ==== # ================ Tidy Datasets and Assign Tags ================ # ====
# ============================================================================#
# ============================================================================#


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#           LC Skills List            #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

df_lc_skills <- df_green_skills %>% 
  filter(low_carbon & climate_related != "Sustainability") %>% 
  select(standard_reference, skill_id, climate_related, low_carbon)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#             Raw AS List             #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

df_as_skills <- df_raw_as_skills %>% 
  select(-climate_related)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#             Full AS List            #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

df_full <- df_as_skills %>% 
  left_join(
    df_lc_skills,
    by = c("standard_reference", "skill_id")
  ) %>% 
  mutate(
    climate_related = if_else(is.na(climate_related), "Not Low Carbon", climate_related),
    low_carbon = if_else(is.na(low_carbon), FALSE, low_carbon)
  ) %>% 
  select(
    standard_reference, standard_name, skill_id, low_carbon, climate_related,
    everything()
  )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#         Classify Standards          #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

df_full %<>% 
  group_by(standard_reference) %>% 
  mutate(
    lc_standard = if_else(any(low_carbon), TRUE, FALSE),
    st_transport = sum(climate_related == "Decarbonization-transport"),
    st_buildings = sum(climate_related == "Decarbonization-buildings"),
    st_energy = sum(climate_related == "Decarbonization-energy"),
    st_lc_total = sum(low_carbon),
    st_sk_total = n()
  ) %>% 
  ungroup()
  

# ============================================================================#
# ============================================================================#
# ==== # ======================== Export Files ========================= # ====
# ============================================================================#
# ============================================================================#

exporting_csv <- function(df, name, dir) {
  base_dir <- dir
  df_exp <- df
  exp_path <- file.path(base_dir, paste0(name, ".csv"))
  write.csv(df_exp, file = exp_path, row.names = FALSE)
}

exporting_csv(df_full, "List AS Skills", "Data/Cleaned/AS")
