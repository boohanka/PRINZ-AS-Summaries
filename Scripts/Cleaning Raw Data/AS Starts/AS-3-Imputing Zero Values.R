

rm(list = ls())

# ============================================================================#
# ============================================================================#
# ==== # ========================= Uploading =========================== # ====
# ============================================================================#
# ============================================================================#

df_skills_starts <- read_csv("Data 2025/Cleaned/AS/Skills-Starts-class_q_g.csv")

# ============================================================================#
# ============================================================================#
# ==== # ============== Determine the Combinations and Impute ========== # ====
# ============================================================================#
# ============================================================================#

combinations <- expand_grid(
  date = unique(df_skills_starts$date),
  ttwa11cd = unique(df_skills_starts$ttwa11cd),
  climate_related = unique(df_skills_starts$climate_related)
) %>% 
  left_join(
    df_skills_starts %>% 
      select(ttwa11cd, ttwa11nm) %>% 
      distinct(),
    by = "ttwa11cd"
  )


df_complete <- combinations %>%
  left_join(df_skills_starts, by = c("date", "ttwa11cd", "ttwa11nm", "climate_related")) %>%
  group_by(date, ttwa11cd, ttwa11nm) %>%  # Group by date and ttwa11cd to keep "total" consistent
  mutate(total = first(na.omit(total))) %>%  # Retain the existing "total" value
  ungroup()


df_complete %<>%
  mutate(
    starts = ifelse(is.na(starts), 0, starts),  
    frac = ifelse(is.na(frac), 0, frac)    
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

exporting_csv(df_complete, "Skills-Full-Starts-class_q_g", "Data 2025/Cleaned/AS")
