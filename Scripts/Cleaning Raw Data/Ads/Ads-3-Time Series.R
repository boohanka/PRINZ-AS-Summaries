

rm(list = ls())

# ============================================================================#
# ============================================================================#
# ==== # ========================= Uploading =========================== # ====
# ============================================================================#
# ============================================================================#

df_ads_full <- read_csv("Data/Cleaned/Ads/Ads Full Sample.csv")
df_ads_aligned <- read_csv("Data/Cleaned/Ads/Ads AS-Aligned.csv")

# ============================================================================#
# ============================================================================#
# ==== # =================== Aggregate to Time Series ================== # ====
# ============================================================================#
# ============================================================================#

f_ts_aggregation <- function(df) {
  
  df %<>%
    mutate(date = as.yearqtr(date)) %>% 
    group_by(date, classification) %>% 
    summarise(
      across(contains("n_ads"), function(x) sum(x, na.rm = TRUE)),
      .groups = "drop_last"
    ) %>% 
    mutate(
      across(contains("n_ads"), ~ . / sum(.), .names = "{str_replace(.col, 'n_', 'frac_')}")
    ) %>% 
    ungroup()
  
  return(df)
  
}

df_ads_ts_full <- f_ts_aggregation(df_ads_full)
df_ads_ts_aligned <- f_ts_aggregation(df_ads_aligned)

# Impute the zeroes:

all_dates <- unique(c(df_ads_ts_full$date, df_ads_ts_aligned$date))
all_classes <- unique(c(df_ads_ts_full$classification, df_ads_ts_aligned$classification))
full_grid <- expand.grid(date = all_dates, classification = all_classes) %>%
  mutate(date = as.yearqtr(date)) %>% arrange(date)

df_ads_ts_full <- full_grid %>%
  full_join(df_ads_ts_full, by = c("date", "classification")) %>%
  replace_na(list(n_ads_class = 0, weighted_n_ads_class = 0, frac_ads_class = 0, weighted_frac_ads_class = 0))

df_ads_ts_aligned <- full_grid %>%
  full_join(df_ads_ts_aligned, by = c("date", "classification")) %>%
  replace_na(list(n_ads_class = 0, weighted_n_ads_class = 0, frac_ads_class = 0, weighted_frac_ads_class = 0))

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

exporting_csv(df_ads_ts_full, "Ads TS All SOCs", "Data/Intermediate/Ads/Time Series")
exporting_csv(df_ads_ts_aligned, "Ads TS AS SOCs", "Data/Intermediate/Ads/Time Series")

