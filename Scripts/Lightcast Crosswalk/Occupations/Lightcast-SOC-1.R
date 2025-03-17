
rm(list = ls())

# ============================================================================#
# ============================================================================#
# ==== # ========================= Uploading =========================== # ====
# ============================================================================#
# ============================================================================#

df_lightcast_soc <- read_xlsx("Data/Raw/Occupations/Lightcast Occupations Crosswalk.xlsx")


colnames(df_lightcast_soc) <- c("lightcast_code", "lightcast_desc",
                                "soc4d_code", "soc4d_desc",
                                "relationship",
                                "best_fit_lightcast", "best_fit_soc",
                                "notes")


# ============================================================================#
# ============================================================================#
# ==== # ==================== Cleaning Crosswalk ======================= # ====
# ============================================================================#
# ============================================================================#

df_crosswalk <- df_lightcast_soc %>% 
  filter(best_fit_soc == 1)

df_crosswalk_weights <- df_lightcast_soc %>% 
  mutate(
    weight_binary = if_else(as.character(best_fit_soc) == "1", 1, 0, missing = 0)
  ) %>% 
  group_by(lightcast_code, lightcast_desc) %>% 
  mutate(
    weight_proportional = 1 / n()
  ) %>% 
  ungroup()

df_crosswalk_weights %<>%
  select(-c(relationship, best_fit_lightcast, best_fit_soc, notes))


# Final Version

df_xwalk <- df_crosswalk_weights %>% 
  rename(weight = weight_proportional) %>% 
  select(-weight_binary)

# Remove the NA values:

df_xwalk %<>%
  filter(lightcast_code != "na" & soc4d_code != "na")

df_xwalk %<>% arrange(lightcast_desc)

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

exporting_csv(df_crosswalk, "Lightcast-SOC Crosswalk", "Data/Cleaned/Lightcast/Occupations")
exporting_csv(df_xwalk, "Lightcast-SOC Weighted Crosswalk", "Data/Cleaned/Lightcast/Occupations")

