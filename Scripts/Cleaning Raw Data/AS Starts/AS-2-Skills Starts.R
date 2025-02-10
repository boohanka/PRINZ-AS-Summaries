

rm(list = ls())

# ============================================================================#
# ============================================================================#
# ==== # ========================= Uploading =========================== # ====
# ============================================================================#
# ============================================================================#

df_as_starts <- read_csv("Data 2025/Cleaned/AS/AS-Starts-st_q_g.csv")
df_as_class <- read_csv("Data 2025/Cleaned/AS/List AS Skills.csv")

# Check how many are actually mapped

st_code_starts <- df_as_starts %>% pull(st_code) %>% unique()
st_code_class <- df_as_class %>% pull(standard_reference) %>% unique()

st_code_mismatch <- symdiff(st_code_starts, st_code_class)
st_code_not_in_starts <- setdiff(st_code_class, st_code_starts)
st_code_not_in_class <- setdiff(st_code_starts, st_code_class)





# ============================================================================#
# ============================================================================#
# ==== # ================ Transform into Skills Starts ================= # ====
# ============================================================================#
# ============================================================================#

df_sk_starts <- df_as_starts %>% 
  drop_na(ttwa11cd, ttwa11nm) %>% 
  left_join(
    df_as_class,
    join_by(st_code == standard_reference)
  ) %>% 
  drop_na(standard_name) %>% 
  group_by(date, ttwa11cd, ttwa11nm, climate_related) %>% 
  summarise(
    starts = sum(starts, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  group_by(date, ttwa11cd, ttwa11nm) %>% 
  mutate(
    total = sum(starts, na.rm = TRUE),
    frac = (starts / total) * 100
  ) %>% 
  ungroup()

df_sk_starts_full <- df_as_starts %>% 
  left_join(
    df_as_class,
    join_by(st_code == standard_reference)
  ) %>% 
  drop_na(standard_name)

df_lc_sk_starts_full <- df_sk_starts_full %>% 
  filter(low_carbon)
  

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

exporting_csv(df_sk_starts, "Skills-Starts-class_q_g", "Data 2025/Cleaned/AS")
exporting_csv(df_sk_starts_full, "Skills-Starts-skill_q_g", "Data 2025/Cleaned/AS")
exporting_csv(df_lc_sk_starts_full, "LC-Skills-Starts-skill_q_g", "Data 2025/Cleaned/AS")

