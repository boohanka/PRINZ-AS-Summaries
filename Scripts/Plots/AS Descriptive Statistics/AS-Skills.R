

rm(list = ls())

# ============================================================================#
# ============================================================================#
# ==== # ========================= Uploading =========================== # ====
# ============================================================================#
# ============================================================================#

df_as_skills <- read_csv("Data 2025/Cleaned/AS/List AS Skills.csv")
df_lc_skills_starts <- read_csv("Data 2025/Cleaned/AS/LC-Skills-Starts-skill_q_g.csv")
df_skills_starts <- read_csv("Data 2025/Cleaned/AS/Skills-Starts-skill_q_g.csv")

# ============================================================================#
# ============================================================================#
# ==== # ====================== Skills Content ========================= # ====
# ============================================================================#
# ============================================================================#

fraction_lc_skills <- nrow(df_as_skills %>% filter(low_carbon == TRUE)) / nrow(df_as_skills) * 100

dist_classification <- df_as_skills %>% filter(low_carbon) %>% 
  group_by(climate_related) %>% 
  summarise(
    num = n(),
    .groups = "drop"
  ) %>% 
  mutate(
    total = sum(num),
    pct_class = (num / total) * 100
  )

dist_standards <- df_as_skills %>% 
  filter(lc_standard & climate_related != "Not Low Carbon") %>% 
  select(standard_reference, standard_name, climate_related) %>% 
  distinct()

# ============================================================================#
# ============================================================================#
# ==== # =========================== Starts ============================ # ====
# ============================================================================#
# ============================================================================#

top_15_lc_skills <- df_lc_skills_starts %>% 
  group_by(st_code, skill_id) %>% 
  summarise(
    starts = sum(starts, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  group_by(st_code) %>% 
  slice_max(starts, n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  slice_max(starts, n = 15) %>% 
  left_join(
    df_as_skills, 
    join_by(st_code == standard_reference, skill_id)
  )

top_lc_classes <- df_lc_skills_starts %>% 
  group_by(climate_related) %>% 
  summarise(
    starts = sum(starts, na.rm = TRUE),
    .groups = "drop"
  )
  

exporting_csv <- function(df, name, dir) {
  base_dir <- dir
  df_exp <- df
  exp_path <- file.path(base_dir, paste0(name, ".csv"))
  write.csv(df_exp, file = exp_path, row.names = FALSE)
}

exporting_csv(top_15_lc_skills, "Top 15", "Data 2025")

